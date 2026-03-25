---
title: "Breaking change in `std::span`'s new `initializer_list` constructor"
document: P4144R0
date: today
audience: LEWG
author:
  - name: Mark Hoemmen
    email: <mhoemmen@nvidia.com>
toc: true
---

# Author

* Mark Hoemmen (mhoemmen@nvidia.com) (NVIDIA)

# Revision history

* Revision 0 submitted 2026-03-25

# Problem: "Silent" change in `span`'s behavior

## Summary

This paper expresses [LWG4520](https://cplusplus.github.io/LWG/issue4520) and proposes a fix.

LWG 4520 came from [an issue](https://github.com/NVIDIA/cccl/issues/7619) filed on NVIDIA's Standard Library's (libcu++) implementation of `span`.

We've written a demo (also with partial implementation of proposed fix) [here](https://godbolt.org/z/7T9Eof9ba).  Here is a short illustration of the change in behavior between C++23 and C++26.

```c++
#include <span>

int main() {
  bool data[4] = {true, false, true, false};
  bool* ptr = data;
  size_t size = 4;

  std::span<bool> span_nc{ptr, size};
  // OK in both C++23 and C++26
  assert(span_nc.size() == size);

  std::span<const bool> bad_span{ptr, size};
  // OK in C++23, but FAILS in C++26
  assert(bad_span.size() == size);
  return 0;
}
```

## Cause

In C++23, constructor `span(element_type*, size_t)` is called.  

Adoption of [P2447R6](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2023/p2447r6.html) for C++26 means that constructor `span(initializer_list<value_type>)` is selected instead.  It takes the conversion sequence from `bool*` to `bool` and `size_t` to `bool`.

P2447 authors understood that adoption of their proposal would be a breaking change.
The paper adds to Annex C some examples of code that this breaks.
This includes "silent" semantic changes to code, such as the following.

```c++
void *a[10];
// x is 2; previously 0
int x = span<void* const>{a, 0}.size();
any b[10];
// y is 2; previously 10
int y = span<const any>{b, b+10}.size();
```

The use case in [LWG4520](https://cplusplus.github.io/LWG/issue4520)
is most like the `span<const any>` break above,
in that both `ElementType*` and `size_t` are convertible to the `span`'s `value_type`.

However, `span<const any>` is a less common use case.
Creating a `span<const bool>` is likely more common, especially in generic code.
That means LEWG might not have realized the significance of the change.

Note that `span<bool>` does not have this issue.
Using `{}` with `mdspan` works just fine.
(Historically, `mdspan` came first.)

# It's not "silent," but implementations diverge

This is actually ill-formed code; it's a narrowing conversion from pointer to `bool`.  GCC trunk compiles this but emits narrowing warnings.  Clang stops with an error.

Per [[intro.compliance.general] 2.3](https://eel.is/c++draft/intro.compliance.general), both implementations are conforming, as they emit a "diagnostic."  GCC implements a conforming "extension" per [[intro.compliance.general] 11](https://eel.is/c++draft/intro#compliance.general-11.sentence-2), in that it attempts to give meaning (by compiling) to invalid code.

# P2447 introduced wording bugs, later fixed

Adoption of P2447 led to bugs in the Standard wording that later had to be fixed.  GCC [Bug 120997](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=120997) led to filing of [LWG 4293](https://cplusplus.github.io/LWG/issue4293).  The Standard was specified to use curly braces for the return values of some `span` member functions, such as `submdspan`.  Adoption of LWG 4293's Proposed Fix fixed that.

# Proposed fix

## Ansatz: Constrain the initializer list constructor

Constrain `span(initializer_list<value_type>)` constructor so that `value_type` is not `bool`.

```c++
constexpr
explicit(extent != dynamic_extent)
span(initializer_list<value_type>)
requires(
  is_const_v<ElementType &&
  (! is_same_v<value_type, bool>) // ADD
);
```

That would have the advantage that everything else works fine still, but we can avoid the breakage of valid user code, so only `span<const bool>` is affected.

```c++
// Works in current C++26 draft;
// fails to compile with the above change
std::span<const bool> span_from_bool{true, false, true};

// Work-around: Enclose initializer_list in ()
std::span<const bool> span_from_bool2({true, false, true});
assert(span_from_bool2.size() == 3u);

// Work-around: Enclose initializer_list in {}
std::span<const bool> span_from_bool3{{true, false, true}};
assert(span_from_bool3.size() == 3u);
```

## Fix initializer list construction from actual bool values

The above change would break existing code, specifically by making the following fail to compile.

```c++
std::span<const bool> span_from_bool{true, false, true};
```

We can fix this by adding a new `initializer_list` constructor overload specifically for `bool` input values.

```c++
// NEW CONSTRUCTOR
template<typename InitListType>
constexpr
explicit(extent != dynamic_extent)
my_span(initializer_list<InitListType> il)
requires (is_const_v<ElementType>
  && (is_same_v<value_type, bool>)
  && (is_same_v<InitListType, bool>)
);
```

Here is an [implementation](https://gcc.godbolt.org/z/Esa1nc1jY) (thanks to Giuseppe D'Angelo! who suggests that this constructor could be a C++29 feature, since it's an extension, as code would move from breaking to non-breaking).

## Fix initializer list construction from any non-pointer type convertible to bool?

It would be excellent to permit non-pointer types that are convertible to `bool`.
For example, this would permit the common case of initializing `bool` values from 0 and 1 `int` literals.

```c++
std::span<const bool> span_from_bool{1, 0, 1};
```

However, I'm not sure how to specify this.
Merely replacing the `is_same_v<InitListType, bool>` constraint with `is_integral_v<InitListType>` does *not* work.
Doing that results in `span<const bool>{0, 1, 0}` selecting the `initializer_list<value_type>` constructor,
which fails with a hard error,
because it attempts to initialize the `const bool*` pointer
with a `const int*` from the input `initializer_list<int>::const_iterator`.

# Alternatives

1. File a GCC bug to make GCC emit an error here.

2. Try to constrain `span<T>`'s constructor generically (for all `T`) so that it refuses to convert a pointer to `value_type`.

Regarding Option (1), Mattermost discussion during the meeting suggests that GCC is unlikely to change its current behavior.

Option (2) is higher risk because `span` has many constructors.

# Wording change

> Text in blockquotes is not proposed wording,
> but rather instructions for generating proposed wording.

## Increment `__cpp_lib_span` feature test macro

In [version.syn], increase the value of the `__cpp_lib_span` macro
by replacing YYYMML below with the integer literal
encoding the appropriate year (YYYY) and month (MM).

```c++
#define __cpp_lib_mdspan YYYYMML // also in <mdspan>
```

## Change [span.cons]

> Change the `initializer_list<value_type>` constructor of `span` in [span.cons] 21 as follows.

```
constexpr explicit(extent != dynamic_extent)
  span(std::initializer_list<value_type> il);
```

[21]{.pnum} *Constraints*: `is_const_v<element_type>` [`&& (! is_same_v<value_type, bool>)`]{.add} [is]{.rm}[are]{.add} `true`.

[22]{.pnum} *Hardened preconditions*: If `extent` is not equal to `dynamic_extent`, then `il.size() == extent` is `true`.

[23]{.pnum} *Effects*: Initializes `data_` with `il.data()` and `size_` with `il.size()`.

::: add
```
template<typename InitListType>
  constexpr explicit(extent != dynamic_extent)
    span(std::initializer_list<InitListType> il);
```

[24]{.pnum} *Constraints*:

* [24.1]{.pnum} `is_const_v<element_type>` is `true`,

* [24.2]{.pnum} `is_same_v<value_type, bool>` is `true`, and

* [24.3]{.pnum} `is_same_v<InitListType, bool>` is `true`.

[25]{.pnum} *Hardened preconditions*: If `extent` is not equal to `dynamic_extent`, then `il.size() == extent` is `true`.

[26]{.pnum} *Effects*: Initializes `data_` with `il.data()` and `size_` with `il.size()`.
:::

```
constexpr span(const span& other) noexcept = default;
```