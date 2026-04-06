---
title: "Restore `span`'s `initializer_list` constructor for C++29"
document: PXXXXR0
date: today
audience: LEWG
author:
  - name: Mark Hoemmen
    email: mark-dot-hoemmen-at-gmail-dot-com
  - name: Hana Dusíková
    email: hanicka-at-hanicka-dot-net
toc: true
---

# Revision history

* R0 to be submitted for the 2026-04 post-Croydon mailing

# Abstract

[P4144R1](https://isocpp.org/files/papers/P4144R1.html) reverted
[P2447R6](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2023/p2447r6.html)
by removing `span`'s `initializer_list` constructor.
We propose to add it back in C++29, but with constraints this time,
in order to avoid the unfortunate conversions that led to its removal.

# Why the constructor was removed

[P2447R6](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2023/p2447r6.html)
proposed adding the following `span` constructor for C++26.

```c++
constexpr explicit(extent != dynamic_extent)
  span(std::initializer_list<value_type> il);
```

As [P4144R1](https://isocpp.org/files/papers/P4144R1.html) explains,
the new constructor's lack of constraints led to silent changes in behavior
from C++23 to C++26.  P2447 gave some examples, but the following example
from P4144 motivated WG21 enough to reconsider the feature.

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

WG21 thus voted at the Croydon meeting in March 2026
to adopt P4144R1, which removed the new constructor entirely.

# Adding the constructor back, but with constraints

Coauthor Hana Dusíková came up with a way
to constrain the constructor generically (for all `T`)
so that it refuses to convert a pointer to `value_type`.

```c++
  template <same_as<value_type> InitListValueType> 
  constexpr
    my_span(std::initializer_list<const InitListValueType> il)
      requires (is_const_v<ElementType>);
```

This would prohibit the troublesome cases discussed in P4144R1.
However, it would also change the original P2447R6 design
by forcing the type match to be exact.
For example, `span<const bool>{true, false, true}` would work,
but `span<const bool>{1, 0, 1}` would fail overload resolution.

# This is still a breaking change

Adding the constructor is still a breaking change,
as the following two examples from P2447R6 show.
The difference is that this proposal would ensure that
the common case of a pointer and a size would always
resolve to the `span(It first, size_type count)` constructor.

```c++
void one(pair<int, int>);  // @_1_@
void one(span<const int>); // @_2_@
void t1() { one({1, 2}); } // @_ambiguous between 1 and 2_@
```

In C++26, `one({1, 2})` would call the `pair<int, int>` overload.
After adoption of this proposal, `one({1, 2})` would be ambiguous.
This is no different than what adoption of P2447R6 for C++26
would have caused for valid C++23 code.

```c++
void two(span<const int, 2>);
void t2() { two({{1, 2}}); }  // @_ill-formed; previously well-formed_@
```

In C++26, `two({{1, 2}})` would be well-formed.
After adoption of this proposal, `two({{1, 2}})` would become ill-formed.
This is no different than what adoption of P2447R6 for C++26
would have caused for valid C++23 code.

We do not think the proposed change would affect
overload resolution for any of the other constructors.
For example, the `span(It first, End last)`
constructor constrains `End` not to be convertible to `size_t`,
so `span(pointer, size)` would never resolve to this anyway.

# Should we adjust the constraints to permit "reasonable" conversions?

The constructor's `same_as<value_type>` constraint excludes some
use cases that would have an unambiguous meaning.  For example:

* `span<const bool>{true, false, true}` would work,
    but `span<const bool>{1, 0, 1}` would fail overload resolution; and

* `span<const float>{1.0f, 2.0f, 3.0f}` would work,
    but `span<const float>{1, 2, 3}` and
    `span<const float>{1.0, 2.0, 3.0}` would fail overload resolution.

One could imagine relaxing the constraint
to permit conversions between arithmetic types.
We do not propose this here.

# Proposed wording

## Add `__cpp_lib_span_initializer_list` macro

[In [version.syn], add the `__cpp_lib_span_initializer_list` feature test macro definition.  Adjust the placeholder value as needed so as to denote this proposal's date of adoption.]{.ednote}

```
#define __cpp_lib_span                              202311L // freestanding, also in <span>
```
::: add
```
#define __cpp_lib_span_initializer_list             YYYMML // freestanding, also in <span>
```
:::
```
#define __cpp_lib_spanstream                        202106L // also in <iosfwd>, <spanstream>
```

## Add `initializer_list` include to [span.syn]

[Add `#include <initializer_list>` to [span.syn] ("Header `<span>` synopsis") as shown below.]{.ednote}

::: add
```
#include <initializer_list>     // see [initializer.list.syn]
```
:::
```
// mostly freestanding
namespace std {
```

## Add `initializer_list` constructor to `span`'s class declaration

[Add `span(initializer_list<value_type>)` constructor to
`span`'s class declaration in [span.overview], as shown below.]{.ednote}

```
    // [span.cons], constructors, copy, and assignment
    constexpr span() noexcept;
    template<class It>
      constexpr explicit(extent != dynamic_extent) span(It first, size_type count);
    template<class It, class End>
      constexpr explicit(extent != dynamic_extent) span(It first, End last);
    template<size_t N>
      constexpr span(type_identity_t<element_type> (&arr)[N]) noexcept;
    template<class T, size_t N>
      constexpr span(array<T, N>& arr) noexcept;
    template<class T, size_t N>
      constexpr span(const array<T, N>& arr) noexcept;
    template<class R>
      constexpr explicit(extent != dynamic_extent) span(R&& r);
```
::: add
```
    constexpr explicit(extent != dynamic_extent)
      span(std::initializer_list<value_type> il);
```
:::
```
    constexpr span(const span& other) noexcept = default;
    template<class OtherElementType, size_t OtherExtent>
      constexpr explicit(@_see below_@)
        span(const span<OtherElementType, OtherExtent>& s) noexcept;
```

## Add `initializer_list` constructor to `span`'s description

[Add `span(initializer_list<value_type>)` constructor description to [span.cons].]{.ednote}

[19]{.pnum} *Effects*: Initializes `data_` with `ranges​::​data(r)` and `size_` with `ranges​::​size(r)`.

[20]{.pnum} *Throws*: What and when `ranges​::​data(r)` and `ranges​::​size(r)` throw.

  template <same_as<value_type> InitListValueType> 
  constexpr
    my_span(std::initializer_list<const InitListValueType> il)
      requires (is_const_v<ElementType>);


::: add
```
template<class InitListValueType>
  constexpr explicit(extent != dynamic_extent)
    span(std::initializer_list<const InitListValueType> il);
```

[21]{.pnum} *Constraints*:

* [21.1]{.pnum} `same_as<value_type, InitListValueType>` is `true`.

* [21.2]{.pnum} `is_const_v<element_type>` is `true`.

[22]{.pnum} *Hardened preconditions*: If `extent` is not equal to `dynamic_extent`, then `il.size() == extent` is `true`.

[23]{.pnum} *Effects*: Initializes `data_` with `il.data()` and `size_ with il.size()`.
:::

```
constexpr span(const span& other) noexcept = default;
```

[24]{.pnum} *Postconditions*: `other.size() == size() && other.data() == data()`.

## Add a new section [diff.cpp26.containers] section

[Add a new [diff.cpp26.containers] section, as shown below.]{.ednote}

::: add
[1]{.pnum} Affected subclause: [span.overview]

**Change**: `span<const T>` is constructible from `initializer_list<T>`.

**Rationale**: Permit passing a braced initializer list to a function taking `span`.

**Effect on original feature**: Valid C++ 2026 code that relies on the lack of this constructor may refuse to compile, or change behavior in this revision of C++.

[*Example 1*:
```
void one(pair<int, int>);       // @_#1_@
void one(span<const int>);      // @_#2_@
void t1() { one({1, 2}); }      // @_ambiguous between #1 and #2; previously called #1_@

void two(span<const int, 2>);
void t2() { two({{1, 2}}); }    // @_ill-formed; previously well-formed_@
```
— *end example*]
:::
