---
title: "Let zero-size `layout_stride::mapping` accept zero strides"
document: PXXXXR0
date: 2025-10-20
audience: LEWG
author:
  - name: Jacob Faibussowitsch
  - name: Mark Hoemmen
    email: <mhoemmen@nvidia.com>
  - name: Christian Trott
    email: <crtrott@sandia.gov>
toc: true
---

# Author

* Jacob Faibussowitsch (NVIDIA)

* Mark Hoemmen (mhoemmen@nvidia.com) (NVIDIA)

* Christian Trott (crtrott@sandia.gov) (Sandia National Laboratories)

# Revision history

* Revision 0 to be submitted 2025-??-??

# Introduction

We propose to change `layout_stride::mapping`'s constructors
to permit strides to be zero if one or more extents are zero.
For example, for extents (3, 5, 0, 11),
this change would permit strides (1, 3, 0, 105).
Currently, users would need to set the stride(s) corresponding
to zero extents to an arbitrary positive value, such as 1.
That would make the strides (1, 3, 1, 105) in this example.

This change has two benefits.
First, it conforms with NumPy's `ndarray` convention for specifying strides.
This would let users construct a `layout_stride::mapping`
directly from a NumPy array, or from any library that follows this convention.
Second, it would let users convert `layout_left::mapping`
or `layout_right::mapping` with size-zero extents to `layout_stride::mapping`.

This change would relax preconditions of two existing
`layout_stride::mapping` constructors.
It would not change what C++ code is currently well-formed or ill-formed.
The only way in which it could affect backwards compatibility is by
introducing `layout_stride::mapping` instances that have zero strides.
This may affect use of some libraries like the BLAS and LAPACK
that forbid a zero stride,
even if the corresponding matrix dimension is zero.
On the other hand, `layout_left` and `layout_right` mappings
can already have zero strides,
so generic code that operates on the elements of an `mdspan`
by calling the BLAS or LAPACK must already account for this.

# Motivation

## Interoperability between NumPy arrays and `mdspan`

[NumPy](https://numpy.org/) is a Python package for scientific computing.
Its [`ndarray` protocol](https://numpy.org/doc/2.3/reference/arrays.ndarray.html)
defines an application binary interface for sharing multidimensional array views
across libraries, programming languages, and hardware interfaces.

Since `ndarray` objects represent multidimensional strided array views,
it would be natural to translate them to C++ by using `mdspan` with `layout_stride`.
Translation from C++ `mdspan` with `layout_stride` to Python `ndarray`
works just fine, because Python permits arbitrary strides
if the multidimensional index space has size zero.
We show how to do this conversion from C++ to Python below
by using the `pybind11` library.

```c++
namespace py = pybind11;

template<class ElementType, class Extents, class Accessor>
py::dict make_array_interface_dict(
  std::mdspan<ElementType, Extents, std::layout_stride, Accessor> md)
{
  py::dict array_interface;
  array_interface["strides"] = md.mapping().strides();
  array_interface["shape"] = md.extents();
  // ... fill in other ndarray fields ...
  return array_interface;
}
```

However, when converting from Python to C++,
the strides need an extra conversion step.
This is because `layout_stride::mapping`'s constructor
has a precondition that all strides are positive,
even for zero extents.

```c++
template<std::size_t Rank>
std::layout_stride::mapping<std::dims<Rank>>
python_to_cpp_mapping(const py::dict& array_interface)
{
  auto cpp_strides = std::array<std::size_t, Rank>{};
  auto py_strides  = array_interface["strides"];
  for (std::size_t i = 0; i < Rank; ++i) {
    if (py_strides[i] == 0) {
      assert(array_interface["shape"][i] == 0);
      cpp_strides[i] = 1; // to satisfy layout_stride
    } else {
      cpp_strides[i] = py_strides[i];
    }
  }

  auto py_extents = array_interface["shape"];
  auto cpp_extents =
    [&] <std::size_t... Inds> (std::index_sequence<Inds...>) {
      return std::dims<Rank>{py_extents[Inds]...};
    } (std::make_index_sequence<Rank>());

  return std::layout_stride::mapping<std::dims<Rank>>{
    cpp_extents, cpp_strides};
}
```

## Permit conversion of empty `layout_{left,right}::mapping` to `layout_stride::mapping`

The `layout_stride::mapping` class can represent the layout mapping
of any `submdspan` of an `mdspan` with `layout_left` or `layout_right`.
In other words, `layout_stride` is the "fixed point"
of applying `submdspan` to any valid `layout_left` or `layout_right` mdspan.
The `layout_stride::mapping(const StridedLayoutMapping&)` constructor
accepts any strided mapping,
including but not limited to the four defined in C++26
(the above two, plus `layout_left_padded` and `layout_right_padded`).
This makes it tempting to treat `layout_stride::mapping`
as a "maximally type-erased" mapping,
for example when defining stable application binary interfaces.
This was part of the design intent for `layout_stride`.

That works fine, except when the conversion's input mapping has a zero extent.
Creating a `layout_right` or `layout_left` `mdspan`
with one or more extents of zero can result in
an `mdspan` with one or more strides of zero.
For example, a `layout_right` `mdspan` with extents (1, 0)
will have strides (0, 1),
and a `layout_left` `mdspan` with extents (0, 1)
will have strides (1, 0).
This is expected behavior, and it matches implementations.
For example, [this Compiler Explorer link](https://godbolt.org/z/G37WGKa4G)
builds and runs the following example with Clang 21.1.0 and libc++,
using build options `-std=c++26 -stdlib=libc++ -Wall`.
[This Compiler Explorer link](https://godbolt.org/z/aec58hsTd)
builds and runs the same example (with just a namespace change)
with the [reference `mdspan` implementation](https::github.com/kokkos/mdspan).

```c++
#include <cassert>
#include <mdspan>
#include <print>

template<class Layout>
using mdspan_2d = std::mdspan<float, std::dims<2>, Layout>;

int main() {
  mdspan_2d<std::layout_right> mr(nullptr, 1, 0);
  std::print("{} {}\n", mr.stride(0), mr.stride(1));
  assert(mr.stride(0) == 0);
  assert(mr.stride(1) == 1);

  mdspan_2d<std::layout_left> ml(nullptr, 0, 1);
  std::print("{} {}\n", ml.stride(0), ml.stride(1));
  assert(ml.stride(0) == 1);
  assert(ml.stride(1) == 0);

  return 0;
}
```

Conversion from these `layout_right` or `layout_left` `mdspan`
to `layout_stride` `mdspan` violates the preconditions
of `layout_stride::mapping`'s converting constructor,
specifically [mdspan.layout.stride.cons] 7.2,
that requires all strides to be positive.

We propose to relax this precondition.
The conversion is otherwise well-formed.
Neither the reference implementation nor libc++ enforces the precondition,
and the conversion works as expected.
[This Compiler Explorer link](https://godbolt.org/z/3Thfrb1W9)
demonstrates that with both implementations.

```c++
#define USE_REFERENCE_MDSPAN 1

#if defined(USE_REFERENCE_MDSPAN)
#  include <https://raw.githubusercontent.com/kokkos/mdspan/single-header/mdspan.hpp>
#else
#  define _LIBCPP_DEBUG 1
#  include <mdspan>
#endif

#include <cassert>
#include <print>

#if defined(USE_REFERENCE_MDSPAN)
namespace md = std::experimental;
#else
namespace md = std;
#endif

template<class Layout>
using mdspan_2d = md::mdspan<float, md::dims<2>, Layout>;

int main(int, char* argv[]) {
  mdspan_2d<md::layout_right> mr(nullptr, 1, 0);
  mdspan_2d<md::layout_stride> mrs(mr);
  assert(mrs.stride(0) == 0u);
  assert(mrs.stride(1) == 1u);

  mdspan_2d<md::layout_left> ml(nullptr, 0, 1);
  mdspan_2d<md::layout_stride> mls(ml);
  assert(mls.stride(0) == 1u);
  assert(mls.stride(1) == 0u);
  return 0;
}
```

## Work-around: Create a new layout

One could imagine working around these issues by creating a new layout
whose mapping's constructor accepts zero strides.
One could provisionally call it "`layout_any_stride`."

We reject this approach because the design intent of `layout_stride`
is to represent any `submdspan` of a `layout_left` or `layout_right` mapping,
even if the mapping has one or more extents that are zero.
The precondition hinders complete expression of the design intent.

Adding a new layout that explicitly permits zero strides
would encourage its use as a "broadcast" layout --
a nonunique, nonempty mapping where a single index
may be "broadcasted" to an entire extent of indices.
While `mdspan` generally permits custom nonunique layouts,
the `mdspan` authors did not want a commonly used layout
such as `layout_stride` to have this behavior.
This is because it can be difficult to understand
how to write generic algorithms for nonunique layouts,
especially for algorithms that need to write to the `mdspan`'s elements.

# Relaxing the precondition does not violate current requirements

In this section, we prove that relaxing the precondition
to allow a stride of 0 for empty extents
does not violate `layout_stride::mapping`'s requirements,
and therefore does not necessitate a new mapping type.

A `layout_stride::mapping` must must satisfy two properties.

1. It is always unique (`is_always_unique()` is `true`)

2. It is always exhaustive (`is_always_exhaustive()` is `true`)

Uniqueness means that every multidimensional index in the mapping's extents
must map to a distinct offset in $[0$, `required_span_size()`$)$.
That is, the mapping must be *injective*.

Exhaustiveness means that for each offset in $[0$, `required_span_size()`$)$,
there must exist a multidimensional index in the mapping's extents
that maps to that offset.  That is, the mapping must be *surjective*.

Relaxing the constraints on strides to allow a value of zero
if and only if there is at least one extent of zero
preserves both uniqueness and exhaustiveness.
In the case where an extent is zero,
the multidimensional index set is empty,
and the mapping becomes a function from the empty set to the empty set.
That makes the mapping both injective and surjective vacuously.

# Implementation

As shown above, both the reference `mdspan` implementation
and libc++'s implementation actually implement this proposal
by not checking the preconditions
and by producing a valid `layout_stride::mapping`.
In a hypothetical implementation that does check the preconditions,
the only required changes would be removing these checks
from three `layout_stride::mapping` constructors.

# Proposed wording

> Text in blockquotes is not proposed wording,
> but rather instructions for generating proposed wording.

## Increment `__cpp_lib_mdspan` feature test macro

In [version.syn], increase the value of the `__cpp_lib_mdspan` macro
by replacing YYYMML below with the integer literal
encoding the appropriate year (YYYY) and month (MM).

```c++
#define __cpp_lib_submdspan YYYYMML // also in <mdspan>
```

## Change [mdspan.layout.stride.cons] 4.1

> Change [mdspan.layout.stride.cons] as follows.

```
template<class OtherIndexType>
  constexpr mapping(const extents_type& e, span<OtherIndexType, @_rank__@> s) noexcept;
template<class OtherIndexType>
  constexpr mapping(const extents_type& e, const array<OtherIndexType, @_rank__@>& s) noexcept;
```

[3]{.pnum} *Constraints*:

* [3.1]{.pnum} `is_convertible_v<const OtherIndexType&, index_type>` is `true`, and

* [3.2]{.pnum} `is_nothrow_constructible_v<index_type, const OtherIndexType&>` is `true`.

[4]{.pnum} *Preconditions*:

::: rm
* [4.1]{.pnum} The result of converting `s[`$i$`]` to `index_type`
    is greater than `0` for all $i$ in the range $[0$, _`rank_`_$)$.
:::

::: add
* [4.1]{.pnum} Let $\sigma_i$ be the result of converting `s[`$i$`]` to `index_type`.
    Then, for all $i$ in the range $[0$, _`rank_`_$)$,
    if the multidimensional index space `e` is empty,
    $\sigma_i$ is greater than or equal to zero,
    otherwise $\sigma_i$ is greater than zero.
:::

* [4.2]{.pnum} _`REQUIRED-SPAN-SIZE`_`(e, s)` is representable
    as a value of type `index_type` ([basic.fundamental]).

* [4.3]{.pnum} If _`rank_`_ is greater than 0,
    then there exists a permutation $P$ of the integers
    in the range $[0$, _`rank_`_$)$, such that
    `s[`$p_i$`] >= s[`$p_{i-1}$`] * e.extent(`$p_{i-1}$`)` is `true`
    for all $i$ in the range $[1$, _`rank_`_$)$,
    where $p_i$ is the $i^{th}$ element of $P$.

::: add
[*Editorial Note*: This definition also permits zero strides,
because the permutation can be selected to move them
to the front of the list of strides.
For example, suppose that the extents are (2, 3, 0, 7, 0, 13)
and the strides are (1, 2, 0, 30, 0, 2310).
If the permutation is (2, 3, 0, 4, 1, 5),
then the permuted extents are (0, 0, 2, 3, 7, 13)
and the permuted strides are (0, 0, 1, 2, 30, 2310).
This even works if the extents corresponding to the zero strides are nonzero.
— *end note*]
:::

[*Note 1*:
For `layout_stride`, this condition is necessary and sufficient
for `is_unique()` to be true.
— *end note*]

## Change [mdspan.layout.stride.cons] 7.2

> Change [mdspan.layout.stride.cons] as follows,
> to permit conversion from `layout_{left,right}::mapping` with some
> zero extents (or any strided layout) to `layout_stride::mapping`.

```
template<class StridedLayoutMapping>
  constexpr explicit(@_see below_@)
    mapping(const StridedLayoutMapping& other) noexcept;
```

[6]{.pnum} *Constraints*:

* [6.1]{.pnum} _`layout-mapping-alike`_`<StridedLayoutMapping>` is satisfied.

* [6.2]{.pnum} `is_constructible_v<extents_type, typename StridedLayoutMapping​::​extents_type>` is `true`.

* [6.3]{.pnum} `StridedLayoutMapping​::​is_always_unique()` is `true`.

* [6.4]{.pnum} `StridedLayoutMapping​::​is_always_strided()` is `true`.

[7]{.pnum} *Constraints*:

* [7.1]{.pnum} `StridedLayoutMapping` meets the
    layout mapping requirements ([mdspan.layout.reqmts])[,]{.rm}[;]{.add}

::: rm
* [7.2]{.pnum} `other.stride(`$r$`) > 0` is `true` for every rank index $r$ of `extents()`,
:::

::: add
* [7.2]{.pnum} for every rank index $r$ of `extents()`,
    if the multidimensional index space `other.extents()` is empty,
    `other.stride(`$r$`)` is greater than or equal to zero,
    otherwise `other.stride(`$r$`)` is greater than zero;
:::

* [7.3]{.pnum} `other.required_span_size()` is representable as a value
    of type `index_type` ([basic.fundamental])[,]{.rm}[;]{.add} and

* [7.4]{.pnum} _`OFFSET`_`(other) == 0` is `true`.
