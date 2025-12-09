---
title: "Define trivial copy-constructibility to improve parallelizability of ranges algorithms"
document: DXXXXR0
date: today
audience:
  - SG1, Evolution, Library Evolution
author:
  - name: Mark Hoemmen
    email: <mhoemmen@nvidia.com>
toc: true
toc-depth: 2
---

# Abstract

A `transform_view` or `zip_transform_view`
whose function object is a lambda that captures an `int` by value
is not trivially copyable, even though the lambda itself is.
This is because the two views are specified using _`movable-box`_,
which fills in its function object's missing copy assignment operator
with a nontrivial copy assignment operator.
These views not being trivially copyable
hinders parallelization of ranges algorithms
on accelerators with separate memories.
The nontrivial copy assignment operator of _`movable-box`_
is nevertheless worth preserving
because it simplifies Ranges' specification and implementation.

What we want to do is copy the underlying bytes of an object
of type `T` containing a non-trivially-copyable _`movable-box`_,
then start the lifetime of a new `T` object using those bytes
as the object's representation.
We propose making that well-defined behavior
by introducing a narrowing of trivial copyability
called *trivial copy-constructibility*,
that requires only that the type have a trivial copy constructor.
Our definition makes all trivial copy-constructible types
also implicit-lifetime types, so we can use `start_lifetime_as`
to start the lifetime of a trivially copy-constructible object
using an existing object representation.

# Motivation

## Accelerators launch parallel algorithms by byte-copying their arguments

Standard Library users would like parallel algorithms
to execute on accelerators (such as GPUs) if possible.
The parallel algorithm generally would be invoked
in ordinary C++ code that is executed on a "host" CPU thread,
but the algorithm would perform its computations on the accelerator.

Many kinds of parallel accelerators have their own memories.
Directly accessing "host" (ordinary C++) memory
from the accelerator, or accelerator memory from a host thread,
may be either impossible or slow.
"Impossible" is not Standard C++, but "slow" is.
For example, NUMA (non-uniform memory access)
may result in different memory pages
having different access latencies to different threads,
depending on the physical hardware on which the thread runs.
To forestall these performance or correctness issues,
C++ code can first copy data from host memory
to accelerator memory before executing the algorithm.
Depending on the accelerator, copying may be required
or just a possible optimization.

Copying from host to accelerator memory happens bytewise,
using `memcpy` or a non-Standard function that behaves like it.
If the objects to copy can't be correctly copied bytewise,
then the only other option is serialization and deserialization.
That is, the accelerator needs to run extra code
(generally written by the user)
to "deserialize" raw data into objects.
Accelerator users generally don't want to incur this overhead.
There is also no Standard serialization interface,
so implementers cannot ask users to implement serialization
for their types in portable code.

Algorithms accesses memory both when accessing the elements
of their input and output ranges,
and when accessing the views or iterators
that represent these ranges.
Even if the elements live in accelerator memory,
the views themselves might not.
Implementations generally address that by copying
the views or iterators into accelerator memory
before "launching" the parallel operation there.
This copying again works bytewise, as if by `memcpy`.
Thus, the same concerns about bytewise copying
apply to views or iterators as to the actual data.

As a result, if an input view,
such as a `transform_view` or `zip_transform_view`,
cannot be copied bytewise to the accelerator,
then the algorithm's implementation
generally would not attempt to run on the accelerator.
It would instead fall back to a possibly nonparallel
and likely slower host implementation.
Implementations thus need some type trait
to test all the parallel algorithm arguments' types.

## Type trait that models launching a parallel algorithm on an accelerator?

Implementations need a type trait to know whether a parallel
algorithms' arguments can be copied bytewise to the accelerator.
It seems like trivial copyability (`is_trivially_copyable`)
is the right trait, but before we accept this, we need to know
how launching a parallel algorithm on an accelerator works.
A parallel algorithms' arguments include ranges or iterators,
function objects, and possibly "scalars"
(e.g., the input value in `ranges::fill`).
The ranges or iterators are "metadata" representing actual data
-- generally collections such as arrays,
over which we want to iterate.
For correctness and performance, we would like all arguments,
and the data to which any metadata refer,
in accelerator memory when we launch the parallel algorithm.
Suppose that the data are already there, but the metadata are not.

1. Allocate (or have allocated) space for the algorithm's arguments

2. Bytewise copy the arguments to accelerator memory

3. Launch the parallel algorithm's "kernel `main`" function
    on the accelerator.  The body of this function finds
    its arguments with the same values as they had on host,
    and with their lifetimes started as usual.

"Kernel `main`" refers to an entry point function for the accelerator.
In CUDA C++, the dialect of C++ for running on NVIDIA's GPUs,
these are functions marked with the special keyword `__global__`
that return `void`.

Step (3) refers to the function's arguments with two phrases:

a. "with the same values as they had on host," and

b. "with their lifetimes started."

The type trait best matching the first phrase is trivial copyability.
However, adding the second phrase complicates this.
Trivial copyability lets us copy bytes between two *existing* objects,
but the objects in accelerator memory haven't started their lifetimes yet.
We could first default-construct the objects in accelerator memory,
but that would require that the objects be both
default-constructible and trivially copyable.
Those are excessive requirements.
We don't *need* to default-construct the objects;
we just need them to exist in accelerator memory
with the valid object representations that we provide for them.
If the objects have nontrivial default constructors,
then we have to do extra work.

The phrase "with their lifetimes started"
suggests that we should require the arguments
to be *implicit-lifetime types*.
Such types can have their lifetimes started by one of the following:

* allocation without explicit construction,

* the `memcpy` that brought over their bytes, or

* `start_lifetime_as` or an implementation-specific analog.

## `transform_view` may not be trivially copyable, even if its components are

The `transform_view` class may not be trivially copyable,
even if their underlying view(s) and function object are.
In particular, non-`mutable` lambdas that capture
trivially copyable variables by value are trivially copyable,
but `transform_view` with such a lambda is not.
An example follows
(available on [Compiler Explorer](https://godbolt.org/z/vYnzGd3js)).
The `zip_transform_view` class has the same issue.

```c++
#include <ranges>
#include <type_traits>
#include <vector>
// Function object type that acts just like f2 below.
struct F3 {
  int operator() (int x) const {
    return x + y;
  }
  int y = 1;
};
int main() {
  std::vector v{1, 2, 3};
  // operator= is defaulted; lambda type is trivially copyable
  auto f1 = [] (auto x) {
    return x + 1;
  };
  static_assert(std::is_trivially_copyable_v<decltype(f1)>);
  // Capture means that lambda's operator= is deleted,
  // but lambda type is still trivially copyable
  auto f2 = [y = 1] (auto x) {
    return x + y;
  };
  static_assert(std::is_trivially_copyable_v<decltype(f2)>);
  // decltype(view1) is trivially copyable
  auto view1 = v | std::views::transform(f1);
  static_assert(std::is_trivially_copyable_v<decltype(view1)>);
  // decltype(view2) is NOT trivially copyable, even though f2 is
  auto view2 = v | std::views::transform(f2);
  static_assert(!std::is_trivially_copyable_v<decltype(view2)>);
  // view3 is trivally copyable, though it behaves just like view2.
  F3 f3{};
  auto view3 = v | std::views::transform(f3);
  static_assert(std::is_trivially_copyable_v<decltype(view3)>);
  return 0;
}
```

Both lambdas `f1` and `f2` are trivially copyable,
but `std::views::transform(f2)` is *not* trivally copyable.
The specification of `transform_view` and `zip_transform_view` permits this.
The wording expresses the input function object of type `F`
as stored in an exposition-only _`movable-box`_`<F>` member.
The `f2` lambda has a capture
that gives it a `=delete`d copy assignment operator.
Nevertheless, `f2` is still trivially copyable,
because each of its default copy and move operations
is either trivial or deleted,
and its destructor is trivial and not deleted.

The problem is _`movable-box`_.
As [range.move.wrap]{- .sref} 1.3 explains,
since `copyable<decltype(f2)>` is not modeled,
_`movable-box`_`<decltype(f2)>` provides a nontrivial,
not deleted copy assignment operator.
This makes _`movable-box`_`<decltype(f2)>`,
and therefore `transform_view` and `zip_transform_view`,
not trivially copyable.

This doesn't feel like an essential limitation.
The `f2` lambda is effectively a struct with one `int` member.
Why can't I `memcpy` `views::transform(f2)` wherever I need it to go?
Even worse, `f3` is a struct just like `f2`,
yet `views::transform(f3)` *is* trivially copyable.

## Work-around couples algorithm and view implementations

As we've shown above, `transform_view` or `zip_transform_view`
may not be trivially copyable, even if their components are.
One way to work around this is to take the view apart,
copy the components separately to accelerator memory,
and then reassemble a new view of the same type on the accelerator.
This would let parallel algorithms work with a `transform_view`,
as long as it has a trivially copyable underlying view and function object.
This would require specializing the parallel algorithm implementation
on different view or iterator types.

This approach has two disadvantages.
First, it adds implementation complexity, especially with deeply nested views.
Second, the Standard views generally do not have a public interface
that exposes all their components.
While `transform_view` exposes the underlying view via `base()`,
it does not offer a Standard public interface to its function object.
As a result, this approach couples the parallel algorithm implementation
to the view implementation.

Implementations such as libc++ already accept this coupling
in order to optimize some algorithms for some iterator or view types.
The Standard presumes that implementers have complete control
over the entire implementation and can give any algorithm
access to any nonpublic component of a view or iterator.
In the extreme case, this leads to a "fully coupled" implementation,
where many parallel algorithms have specializations
on many different view and iterator types.

We do not advocate a fully coupled approach for three reasons.
First, software engineering best practice
generally prefers minimizing coupling between components.
Users of accelerators consider it a software defect
for an accelerator-based implementation
not to run some operations on the accelerator.
Thus, we would prefer not to require so much coupling
just to make acceleration work for something
so common in ranges as a `views::transform`.
Second, [P2500](https://wg21.link/p2500)
proposes opening up the Standard algorithms to customization.
In order to be portable, customizations would still
need to work with Standard view and iterator types.
Third, even without [P2500](https://wg21.link/p2500),
accelerator vendors would still like to provide parallel algorithms
that live in a different namespace than `std`,
but otherwise conform to the Standard.
The only way for vendors to do that now would be
to provide a reimplementation of much of the Standard Library
in a vendor-specific namespace.
The reimplementation would need to include
Standard containers that are also ranges, such as `std::vector`.
Even with this heavyweight vendor reimplementation,
users would still like vendor-provided algorithms
to work with Standard view and iterator types.
For example, users might want to define a library
that accepts Standard views or iterators,
but dispatches to different vendor-provided implementations underneath.
[Kokkos](https://github.com/kokkos/kokkos) is an example of such a library.

Minimizing coupling between algorithms and views
calls for making it possible for algorithms to iterate over views
without specializing on the view or iterator type.
This includes making views bytewise copyable to accelerators if possible.
Doing so would get more parallel algorithms
running on accelerators right away.

## Using iterators instead of views doesn't help

The exposition-only members of `transform_view::`_`iterator`_
and `transform_view::`_`sentinel`_
suggest that we could specify them to be trivially copyable
as long as the underlying view's iterator and sentinel are trivially copyable.
In particular, `transform_view::`_`iterator`_
has an exposition-only pointer to the `transform_view`,
and pointers are trivially copyable.
However, that doesn't solve the problem for us,
because the pointer still points to a `transform_view`
that may live in non-accelerator memory.
The algorithm was invoked on host,
and thus the `transform_view` and its function were created on host.
We need a copy of the function,
and access to the underlying view's elements,
on the accelerator.
The easiest way to do that would be to copy the whole `transform_view`
from host memory to accelerator memory.

## We want to retain _`movable-box`_'s copyability

A _`movable-box`_ of a function object with a copy constructor
but a deleted copy assignment operator is not trivally copyable
because the _`movable-box`_ has a nontrivial copy assignment operator.
The _`movable-box`_'s copy assignment operator ensures that
`transform_view` and `zip_transform_view` both model `copyable`
in as many cases as possible.
This simplifies both the specification and the implementation
of ranges algorithms and views.
For example, it avoids special cases for situations like
"copy constructible but not copy assignable."
It also makes creation and use of views more forgiving for users.
These are useful properties that we want to retain.

# Approaches to a fix

## Decree that _`movable-box`_ is trivially copyable if its component is?

Launching a parallel algorithm with a `transform_view`
on an accelerator does not actually require
invoking `transform_view`'s copy assignment operator.
It's a purely syntactic requirement for trivial copyability.
We could simply *decree* that _`movable-box`_`<F>`
is trivially copyable if `F` is.
However, this would require adding a special case
to the core language for a type with no Standard name.
Lack of a Standard name might hinder use of different
Standard Library implementations with the same compiler.
Thus, we do not favor this approach.

## What about a special case for lambdas?

We could add a special case to the Standard
that makes lambdas trivially copyable
if they only capture values
and if all those values are of trivially copyable types.
This would solve the immediate problem at hand.
However, we would prefer not to add a special case for lambdas.
It's easier to reason about lambdas
if they behave like ordinary classes.
Thus, we do not favor this approach.

## Trivial move (relocation) is more than we need

## What about trivial move, or trivial relocation?

Suppose that we could "trivially move" the view
from host to accelerator memory, instead of trivially copying it.
By "trivially move," we mean

1. ending the lifetime of the view object in host memory
    while retaining its storage;

2. copying the storage bytes from host to accelerator memory,
    after which point the host storage is no longer needed
    and can be freed; and

3. starting the lifetime of a new view object in accelerator memory
    using the bytes copied in (2).

If the view is an implicit-lifetime type,
then we could use `start_lifetime_as` to accomplish (3).
(Please see the section above.)

Another way to say "trivial move" is "trivial relocation."
The Standard doesn't currently have a way to express this.
C++26's trivial relocation proposal
[P2786](https://wg21.link/p2786)
did not survive National Body comment challenges
and was removed at the Kona meeting in November 2025.

A reasonable definition of "trivial movability"
might work like trivial copyability, in that
it would require a trivial or deleted move constructor
and move assignment operator.
This would admit a lambda
with value captures of trivially copyable types.
Such a lambda would have a trivial move constructor,
nondeclared (and therefore not user-provided)
move assignment operator, and trivial destructor.
However, the lambda's nondeclared move assignment operator
would make _`movable-box`_
define a nontrivial move assignment operator
per [range.move.wrap] 1.4.
One way to work around this would be to add syntax
for asserting that a class is trivially movable
despite having a nontrivial move assignment operator.
This would work like the special identifier
`trivially_relocatable_if_eligible`
in C++26's trivial relocation proposal
[P2786](https://wg21.link/p2786).

We do not favor this approach because
we want to copy objects, not move them.
We don't need or necessarily even want
to end the lifetime of the source objects.
However, if the language gives us trivial relocation
as the only tool to use, we would use it.

## What about letting types declare their trivial copyability?

What if we could assert that a class is trivially copyable,
even if it has a nontrivial copy assignment operator?
This might work like the special identifier
`trivially_relocatable_if_eligible`
that was part of C++26's trivial relocation proposal
[P2786](https://wg21.link/p2786).
We could then specify lambdas to have this declaration,
conditionally on that they only capture values
and that all those values are of trivially copyable types.

The problem with this approach is that trivial copyability
also includes copying bytes between existing objects.
That bypasses the destination object's copy assignment operator.
A nontrivial copy assignment operator might have arbitrary effects
that do not correspond to copying bytes.
Giving users a way to demand trivial copyability
could open up a field of potential bugs,
while hindering compilers from helping users do the right thing.
Also, we don't *need* to copy bytes between existing objects.
Thus, we do not favor this approach.

## Remaining option: "Trivial copy-constructibility"

### Define trivial copy-constructibility

The problem with the previous options is that
they fight the solution naturally suggested by the implementation.
The implementation wants to "copy-construct" the arguments from bytes.
A _`movable-box`_ of the lambda in question
is still copy constructible and its copy constructor is trivial.
Thus, what we actually need here is not trivial copyability,
but "trivial copy-constructibility."

We define a *trivially copy-constructible class*
(analogously to [class.prop] 1) to be either an aggregate
whose destructor is not user-provided, or to be any class

* that has at least one eligible copy constructor,
    move constructor,
    copy assignment operator,
    or move assignment operator,

* where each eligible copy constructor and move constructor
    is trivial, and

* that has a trivial, non-deleted destructor.

We define a *trivially copy-constructible type*
to be a scalar type, trivially copy-copyable class type,
arrays of such types, or cv-qualified versions of these types.
Trivially constructible types are also trivially copy-constructible,
but not necessarily the other way around.
Our lambda example that captures a single `int` by value
would be trivially copy-constructible,
even though it is not trivally copyable.

### Trivially copy-constructible types are implicit-lifetime types

It turns out that trivially copy-constructible types
are also implicit-lifetime types.
This means that we can use them with `start_lifetime_as`.

### Define trivial copy construction

We then say that trivially copy-constructible types can be
*trivially copy-constructed* or "copy-constructed from bytes."

```c++
void* dst_mem = std::aligned_alloc(alignof(T), sizeof(T));
std::memcpy(dst_mem, &src, sizeof(T));
T* dst_ptr = std::start_lifetime_as(dst_mem);
```

Analogously to [basic.types.general] 3, we say that,
given an object `src` of trivially copy-constructible type `T`,
and given correctly aligned storage
for an object of type `T` at address `dst_mem`,
copying the bytes of `src` into `dst_mem` and then somehow
starting the lifetime of a `T` object at address `dst_mem`
would ensure that `dst_mem` points to a valid `T` object
that holds the same value as `src`.

The existing `start_lifetime_as` function expresses
"starting the lifetime of a `T` object
using these bytes as its value representation,
without invoking an existing constructor of `T`."
Other existing syntax options would not express this idea.
Default initialization with placement `new`
would invoke a possibly nontrivial default constructor.
We don't want to use the default constructor
or even require that types be default constructible.
None of the `uninitialized_*` or `ranges::uninitialized_*`
algorithms in `<memory>` would accomplish this.

### _`movable-box`_ already has a trivial destructor

We depend on both the ability to copy algorithm arguments bytewise,
and on the ability to start the lifetime of these arguments
from their value representations.
The latter requires that the arguments be implicit-lifetime types.
Implicit-lifetime classes that are not aggregates
must have a trivial destructor ([class.prop]).

The Standard specifies _`movable-box`_ as an enumerated
list of differences from `optional<T>` ([range.move.wrap]).
None of these differences include the destructor.
The specification of `optional<T>` says that its destructor is trivial
if `is_trivially_destructible_v<T>` is `true` ([optional.dtor]).
Thus, the same applies to _`movable-box`_`<F>`.

Lambdas that capture values have a trivial destructor
as long as all their members are trivially destructible.
This means that we don't need to change the specification
of _`movable-box`_ itself in order for our example to work.  

### This approach treats lambdas like any other class type

This approach would treat lambdas like any other class type.
In fact, it would work for types other than lambdas,
such as a struct with a `const T` member,
where `T` is trivially copyable.

# Acknowledgments

Thanks to my NVIDIA colleague Ilya Burylov for reviewing this paper and suggesting wording changes!

# Proposed wording

> Text in blockquotes is not proposed wording, but rather instructions for generating proposed wording.

## Increment `__cpp_lib_trivially_copy_constructible` feature test macro

In [version.syn], add the `__cpp_lib_trivally_copy_constructible` macro by replacing YYYMML below with the integer literal encoding the appropriate year (YYYY) and month (MM).

```
#define __cpp_lib_transparent_operators 201510L // @_freestanding, also in_@ <memory>, <functional>
```
::: add
```
#define __cpp_lib_trivially_copy_constructible YYYYMML // @_freestanding, also in_@ <memory>, <type_traits>
```
:::
```
#define __cpp_lib_tuple_element_t 201402L // @_freestanding, also in_2 <tuple>
```

## Change beginning of [class.prop]

> Change the beginning of [class.prop] ("Properties of classes") as follows.

[This assumes removal of trivial relocatability (P2786R13).]{.ednote}

::: add
[1]{.pnum} A *trivially copy-constructible class* is a class:

* [1.1]{.pnum} that has at least one eligible copy constructor ([special], [class.copy.ctor]),

* [1.2]{.pnum} where each eligible copy constructor is trivial,

* [1.3]{.pnum} where each eligible move constructor, if present, is trivial, and

* [1.4]{.pnum} that has a trivial, non-deleted destructor ([class.dtor]).
:::

[We say "at least one eligible copy constructor" because a class `X` could have multiple copy constructors `X(const X&)` and `X(X&, int=1)`, for example, per [class.copy.ctor] 1.  These are not "of the same kind" per [special] 5, and thus both of them could be eligible, if invoked with a nonconst `X` object.]{.ednote}

[2]{.pnum} A *trivially copyable class* is a class:

* [2.1]{.pnum} that has at least one eligible copy constructor, move constructor, copy assignment operator, or move assignment operator ([special], [class.copy.ctor], [class.copy.assign]),

* [2.2]{.pnum} where each eligible copy constructor, move constructor, copy assignment operator, and move assignment operator is trivial, and

* [2.3]{.pnum} that has a trivial, non-deleted destructor ([class.dtor]).

[3]{.pnum} A class S is a *standard-layout class* if it:

## Add example to end of [class.prop]

> Add an example to the end of [class.prop] ("Properties of classes") as follows.

[16]{.pnum} A class `S` is an *implicit-lifetime class* if

* [16.1]{.pnum} it is an aggregate whose destructor is not user-provided or

* [16.2]{.pnum} it has at least one trivial eligible constructor and a trivial, non-deleted destructor.

::: add
[*Note 7*:
A trivially copy-constructible class `S` is also an implicit-lifetime class.
— *end note*]

[*Example 3*:
If `S` is a trivially copy-constructible class and `src` is an object of type `S`,
then the following starts the lifetime of an `S` object at `dst_ptr`
with the same object representation as that of `src`.

```c++
void* dst_mem = std::aligned_alloc(alignof(S), sizeof(S));
std::memcpy(dst_mem, &src, sizeof(T));
S* dst_ptr = std::start_lifetime_as(dst_mem);
```
— *end example*]
:::

## Change beginning of [basic.types.general]

> Change the beginning of [basic.types.general] as follows.

[1]{.pnum} [*Note 1*: [basic.types] and the subclauses thereof impose requirements on implementations regarding the representation of types.  There are two kinds of types: fundamental types and compound types.  Types describe objects, references, or functions. — *end note*]

::: add
[2]{.pnum} For any object `src`
(other than a potentially-overlapping subobject)
of trivially copy-constructible type `T`,
the underlying bytes ([intro.memory]) making up `src`
can be copied into an array of
`char`, `unsigned char`, or `std::byte` ([cstddef.syn]).
If the start of the array has alignment at least to `alignof(T)`
and an object `dst` of type `T`
is implicitly created ([intro.object]) in the array,
then `dst` shall subsequently hold the same value as `src`.

[*Example 1*:
At the end of this example, `dst_ptr` points to
a valid `T` object that holds the same value as `src`.

```
T src; // default constructibility is not necessary
char dst_mem[sizeof(T)];
std::memcpy(&src, dst_mem, sizeof(T));
T* dst_ptr = std::start_lifetime_as<T>(dst_mem);
```
— *end example*]
:::

[3]{.pnum} For any object (other than a potentially-overlapping subobject) of trivially copyable type T, whether or not the object holds a valid value of type `T`, the underlying bytes ([intro.memory]) making up the object can be copied into an array of `char`, `unsigned char`, or `std​::​byte` ([cstddef.syn]).  [We omit the existing footnote link "[26]" because we do not know how to format it.]{.ednote}  If the content of that array is copied back into the object, the object shall subsequently hold its original value.

[*Example 2*:
```
constexpr std::size_t N = sizeof(T);
char buf[N];
T obj;                          // obj initialized to its original value
std::memcpy(buf, &obj, N);      // between these two calls to std​::​memcpy, obj might be modified
std::memcpy(&obj, buf, N);      // at this point, each subobject of obj of scalar type holds its original value
```
— *end example*]

## Change [meta.unary.prop]

> Change [meta.unary.prop] ("Type properties") as follows.

| **Template** | **Condition** | **Preconditions** |
| === | === | === |
| ... | ... | ... |
| `template<class T> struct is_const;` | `T` is const-qualified ([basic.type.qualifier]) | |
| `template<class T> struct is_volatile;` | `T` is volatile-qualified ([basic.type.qualifier]) | |
::: add
| `template<class T> struct is_trivially_copy_constructible;` | `T` is a trivially copy-copyable type ([basic.types.general]) | `remove_all_extents_t<T>` shall be a complete type or *cv* `void`. |
:::
| `template<class T> struct is_trivially_copyable;` | `T` is a trivially copyable type ([basic.types.general]) | `remove_all_extents_t<T>` shall be a complete type or *cv* `void`. |
| `template<class T> struct is_standard_layout;` | `T` is a standard-layout type ([basic.types.general]) | `remove_all_extents_t<T>` shall be a complete type or *cv* `void`. |

## Change [meta.reflection.traits]

> Change [meta.reflection.traits] ("Reflection type traits") as follows.

```
// associated with [meta.unary.prop], type properties
consteval bool is_const_type(info type);
consteval bool is_volatile_type(info type);
```
::: add
```
consteval bool is_trivially_copy_constructible_type(info type);
```
:::
```
consteval bool is_trivially_copyable_type(info type);
consteval bool is_standard_layout_type(info type);
```

## Change [meta.type.synop]

> Change [meta.type.synop] ("Header `<type_traits>` synopsis") as follows.

```
  // [meta.unary.prop], type properties
  template<class T> struct is_const;
  template<class T> struct is_volatile;
```
::: add
```
  template<class T> struct is_trivially_copy_constructible;
```
:::
```
  template<class T> struct is_trivially_copyable;
  template<class T> struct is_standard_layout;
```

[Please skip down to the `*_v` declarations.]{.ednote}

```
  // [meta.unary.prop], type properties
  template<class T>
    constexpr bool is_const_v = is_const<T>::value;
  template<class T>
    constexpr bool is_volatile_v = is_volatile<T>::value;
```
::: add
  template<class T>
    constexpr bool is_trivially_copy_constructible_v = is_trivially_copy_constructible<T>::value;
:::
```
  template<class T>
    constexpr bool is_trivially_copyable_v = is_trivially_copyable<T>::value;
  template<class T>
    constexpr bool is_standard_layout_v = is_standard_layout<T>::value;
```