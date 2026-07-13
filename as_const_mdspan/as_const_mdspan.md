---
title: "Getting the const element type version of an mdspan accessor, and of an mdspan too"
document: PXXXXR0
date: today
audience: LEWG
author:
  - name: Mark Hoemmen
    email: <mhoemmen@nvidia.com>
  - name: Rob Parolin
    email: <>

toc: true
---

# Author

* Mark Hoemmen (NVIDIA)

* Rob Parolin (NVIDIA)

# Revision history

* Revision 0 to be submitted by 2026-07-13

# Summary

Given an accessor `a` of type `A`, a "const element type version of `a`" (if it exists) is an accessor `b` of a possibly different type `B`, where

* `B::data_handle_type` is constructible from `A::data_handle_type`;

* `B::reference` only permits read access to the element to which it refers; and

* if `a_dh` is `A::data_handle_type` and `b_dh` is `B::data_handle_type(a_dh)`, then

    * `b` and `b_dh` have the same accessible range as `a` and `a_dh`, and 

    * `b.access(b_dh, k)` and `a.access(a_dh, k)` access the same element for `k` in the accessible range of `a_dh`.

For example, a const element type version of any `default_accessor<T>` instance is any `default_accessor<const T>` instance, and a const element type version of any `aligned_accessor<T, ByteAlignment>` instance is any `aligned_accessor<const T, ByteAlignment>` instance.

Given an `mdspan` `x` whose accessor is `a` of type `A`, and assuming that `b` of type `B` is the const element type version of `a`, a "const element type version of `x`" is an `mdspan` `y` given by the following declaration.

```c++
mdspan y(B::data_handle_type(x.data_handle()), x.mapping(), b);
```

The Standard doesn't currently have a way to take an arbitrary accessor and get a const element type version of it.  This hinders development of generic `mdspan` algorithm libraries.

We propose adding three new features to the C++ Standard Library.

1. A customization point object (CPO) `as_const_access`.  It takes an accessor with element type `element_type`, and returns a const element type version of the accessor.

2. A new accessor `as_const_accessor`.  It wraps an existing accessor, has const `element_type`, and has a `reference` type that wraps the existing accessor's `reference` but only permits reads, not writes.  The result is a const element type version of the wrapped accessor.  This behaves like the `mdspan` analog of `ranges::as_const_view`.
 
3. A function template `as_const_mdspan` that takes an `mdspan` and uses (1) to return a const element type version of the `mdspan`.

The CPO behaves as follows.

1. For `default_accessor<T>`, it returns `default_accessor<const T>`.

2. For `aligned_accessor<T, ByteAlignment>`, it returns `aligned_accessor<const T, ByteAlignment>`.

3. For accessor types whose `element_type` is already const, it just returns a copy of the input accessor.

4. If the accessor has a (possibly static) `as_const_access` member, the CPO returns the result of calling that.
 
5. Otherwise, the CPO returns the result of wrapping the input accessor in `as_const_accessor`.

# Motivation

## Argument canonicalization

Suppose we want to write a library of generic mdspan algorithms.  The [linalg] library is an example.  We might have some complicated algorithm `my_algorithm` that takes a rank-3 mdspan as input and a rank-2 mdspan as output.

```c++
template<
  class InElementType, class InExtents, class InLayout, class InAccessor,
  class OutElementType, class OutExtents, class OutLayout, class OutAccessor
>
requires(
  InExtents::rank() == 3
  && OutExtents::rank() == 2
  /* && other constraints ... */
)
void my_algorithm(
  mdspan<InElementType, InExtents, InLayout, InAccessor> input,
  mdspan<OutElementType, OutExtents, OutLayout, OutAccessor> output)
requires(
  decltype(output)::mapping_type::is_always_unique()
  /* && other constraints ... */
{
  // ... very long, complicated algorithm ...
}
```

Note that we can't just declare or constrain `input` to have const element type.  This is because users might call this algorithm with `input` that has nonconst element type.  We don't propose fixing that here; it's just how argument matching works in C++.  The Standard algorithms have the same issue with their range types; this is why their input ranges are not constrained on being read-only.

The problem is that if users call this algorithm with `input` that has nonconst element type, and later call the algorithm with `input` that has const element type, `my_algorithm` will be instantiated twice.  The problem gets worse if we consider all the possible variations of `extents` specializations.  For instance, even if we don't care about optimizing for static extents and are perfectly happy with `InExtents` being `dims<3>`, the compiler will still instantiate different copies of the algorithm for `InExtents` `extents<size_t, dynamic_extents, 11, 13>`, `extents<size_t, 7, dynamic_extents, 13>`, `extents<unsigned, 7, 11, dynamic_extents>`, and so on.  If we just declare or constrain `InExtents` to be `dims<3>`, then the above algorithm won't be generic; it won't match users' extents types.

We solve this problem with a process we call *argument canonicalization*.  We start with the outer, fully generic interface of the algorithm.  Inside that, we "canonicalize" the `mdspan` arguments to the types that we want, for example by making their `extents` just `dims<3>`.  Then, we call the implementation of the algorithm.  The effect of this process is to reduce the number of instantiations of the actual algorithm.  Our experience is that if the algorithm is sufficiently complicated, then this reduces code size and compilation cost.  It also helps separate constraint and error checking from the actual algorithm.  Reducing the number of instantiations also makes techniques like explicit instantiation more effective.

```c++
template<
  class InElementType, class InExtents, class InLayout, class InAccessor,
  class OutElementType, class OutExtents, class OutLayout, class OutAccessor
>
requires(
  InExtents::rank() == 3
  && OutExtents::rank() == 2
  /* && other constraints ... */
)
void my_algorithm(
  mdspan<InElementType, InExtents, InLayout, InAccessor> input,
  mdspan<OutElementType, OutExtents, OutLayout, OutAccessor> output)
requires(
  decltype(output)::mapping_type::is_always_unique()
  /* && other constraints ... */
{
  // ... any run-time checks on input and output extents might go here ...
  auto canonical_input = impl::canonicalize_input_for_my_algorithm(input);
  auto canonical_output = impl::canonicalize_output_for_my_algorithm(output);
  impl::canonical_my_algorithm(input, output);
}

namespace impl {

template<
  class InElementType, class InExtents, class InLayout, class InAccessor
>
constexpr auto canonicalize_input_for_my_algorithm(
  mdspan<InElementType, InExtents, InLayout, InAccessor> input
)
{
  typename InLayout::template mapping<dims<3>> canonical_mapping(input.mapping());
  auto canonical_accessor = std::as_const_access(input.accessor()); // proposed in this paper
  decltype(canonical_accessor) canonical_data_handle(input.data_handle());
  return mdspan(canonical_data_handle, canonical_mapping, canonical_accessor);
}

// ... define canonicalize_output_for_my_algorithm analogously ...

template<
  class InValueType, class InLayout, class InAccessor,
  class OutValueType, class OutLayout, class OutAccessor
>
void canonical_my_algorithm(
  mdspan<const InValueType, dims<3>, InLayout, InAccessor> input,
  mdspan<OutValueType, dims<2>, OutLayout, OutAccessor> output)
{
  // ... long, complicated algorithm ...
}

} // namespace impl
```

Canonicalization can do whatever `my_algorithm` wants to do.  In the above example, input `mdspan` canonicalization has three steps.

1. It converts the input mapping with generic `InExtents` to the same input mapping with `dims<3>`.
2. It converts the input accessor to an accessor with const `element_type`.
3. It converts the input data handle to the new accessor's `data_handle_type`.

We know how to do (1).  If a layout mapping is well-formed for one extents specialization, it's required to work for all extents specializations of the same rank.  That is, if `Layout::mapping<Extents0>` is well-formed and `Extents1` is an `extents` specialization with `Extents1::rank()` equal to `Extents0::rank()`, then `Layout::mapping<Extents1>` is well-formed.  The Standard doesn't require that `Layout::mapping<Extents1>` is constructible from `Layout::mapping<Extents0>` (perhaps it should!), but we can generally assume this for all the Standard Layout mappings.  (We leave guaranteeing that this works for user-defined layout mappings to a separate proposal.)

We do NOT know how to (2).  We know how to do it for the Standard accessors like `default_accessor` and `aligned_accessor`, but we don't know how to do it for an arbitrary user-defined accessor.

This situation has come up in practice for the authors.  One author developed this two-layer scheme for generic algorithms in the kokkos-kernels subproject of the [Kokkos](https://github.com/kokkos/kokkos) project.  The scheme works there because Kokkos' analog of `mdspan`, `Kokkos::View`, has a way to get the const element type version of a `Kokkos::View`.  The same author consulted on how to implement the analogous two-layer scheme for an `mdspan`-based generic algorithms library, [RAPIDS RAFT](https://github.com/NVIDIA/raft).  It didn't work because there is no way currently to get the const element type version of an `mdspan`.

## Why can't users do this with the current Standard?

We know how to get the const element type versions of the Standard accessors `default_accessor` and `aligned_accessor`.  Users might guess that they can assume that all accessors have the form `Accessor<ElementType, Args...>`, where the first template argument is the element type, and the subsequent template arguments determine the accessor's behavior.  This assumption is false, for the following reasons.

* Accessors don't need to have template parameters at all.
* Even if they do have template parameters, the first template parameter doesn't have to be the element type.
* Even if the first template parameter is the element type, nothing requires that the accessor accept `const element_type` as its first template argument.  (Accessors don't have to be "const-able.")

In many cases, users know what the const element type version of their accessor would be.  However, the Standard doesn't give them a way to tell generic code how to get this.

# Design

## Summary

Add a customization point object (CPO) `std::as_const_access`.  It takes an accessor with element type `element_type`, and returns a new accessor with element type `std::add_const_t<element_type>`, and with a `reference` type that only permits reads, not writes.  The CPO behaves as follows.

1. For `default_accessor<ET>`, the CPO returns `default_accessor<const ET>()`.
2. For `aligned_accessor<ET, BA>`, the CPO returns `aligned_accessor<const ET, BA>`.
3. For accessor types whose `element_type` is already const, the CPO just returns a copy of its input.  
4. For accessor types that have an `as_const_access` member function taking zero arguments, the CPO returns the result of calling that.
5. Otherwise, the CPO wraps the accessor in a new accessor type `as_const_accessor` that behaves analogously to `ranges::as_const_view`.

As a convenience for users, add a function `std::as_const_mdspan` that takes an `mdspan` and uses `std::as_const_access` to return an `mdspan` with const element type.  It has the following definition.

```c++
template<class ElementType, class Extents, class Layout, class Accessor>
constexpr auto as_const_mdspan(
  mdspan<ElementType, Extents, Layout, Accessor> x)
{
  auto new_accessor = ::std::as_const_access(x.accessor());
  using new_data_handle_type = typename decltype(new_accessor)::data_handle_type;
  return mdspan(new_data_handle_type(x.data_handle()), x.mapping(), new_accessor);
}
```

## CPO design and naming

The problem we have is to name three separate things.

1. The CPO that returns the const element type version of an accessor
2. The function implementing the "customization" for the CPO
3. The accessor that wraps the input accessor to forbid write access

There are five options for disambiguating the CPO from the customization function.

1. The function, if it exists, is a member of the accessor 
2. The CPO has the same name as the function, but lives in a different namespace
3. The CPO has a different name from the function
4. The CPO is exposition-only
5. The CPO does not exist at all

We have the following design desiderata.

1. Users should not be required to define a customization for their custom accessors.

    a. The `mdspan` design aims to make defining custom accessors simple.  (That's why it doesn't ask users to define custom iterators, for example.)

    b. The accessor might already have const element type, in which case the CPO can just return its input.

    c. The "fall-back" behavior of a wrapping accessor might be acceptable.

2. Users should have a public interface for getting the const element type version of an accessor, not just a public interface for getting the const element type version of an mdspan.

    a. Users might want to apply multiple transformations to accessors before creating an mdspan from them.  For instance, they might start with an mdspan with nonconst element type, like `default_accessor<float>`, turn it to const, and then wrap it in another accessor that adds logging functionality.

3. The CPO and the customization should have the same name.

    a. Otherwise, users might get silently surprising behavior if they give their customization the CPO's name.

Only the first three options satisfy Desiderata (1) and (2), and only the first two options satisfy all three Desiderata.

What design precedent does the Standard offer?

Precedent from [exec] takes Option (1).  We specifically refer to P2855R1 (Member customization points for Senders and Receivers), which was merged into P2300R10 and voted into C++26.  P2855 changed customizations of Sender / Receiver operations such as `set_value` from using `tag_invoke` to just calling member functions of the same name.

Precedent from Ranges takes Option (2); it distinguishes the CPO from the function by namespace.  For example, the CPO `std::ranges::begin(t)` looks for unqualified `begin(t)` by ADL.  Putting the CPO in the `std::ranges` namespace avoids collision with existing nonmember `std::begin` overloads.  This is a natural solution for Ranges, because all Ranges features live in the `std::ranges` namespace anyway.  That resolves any ambiguities.  For example, the range adapter object `as_const` lives in the namespace `std::ranges::views`.  Despite "view" being ambiguous in the Standard (the [views] section defines both `span` and `mdspan`, but only `span` is a "view" in the Ranges sense), putting "`views`" inside `std::ranges` makes it clear that the Standard means "the Ranges definition of view."  However, this precedent doesn't apply very well here, because we don't have a nested namespace for `mdspan` features.  The `mdspan` class template and the Standard accessors like `default_accessor` all live in the `std` namespace.  We could create a nested namespace `std::accessors` and put the CPO in there, but that would forever reserve the term "accessor" for `mdspan`-related features.  Alternately, we could create a nested namespace `std::mdspans::accessors` and put the CPO in there, but that would not be consistent with all the other `mdspan` features living in the `std` namespace.

We know of no precedent for Option (3).  The CPO `std::compare_strong_order_fallback(a, b)` has an option to invoke the CPO `std::strong_order`, but there's no ADL happening.

Precedent from [linalg] takes Option (4); it makes the CPO exposition-only.  For example, _`abs-if-needed`_ is an exposition-only CPO that attempts to use ADL to find `abs(E)`.  The [linalg] library takes this approach because the CPO is an implementation detail.  Users influence the library's behavior by defining ADL-findable functions for their number types, as they normally would for custom number types.  The CPO just "regularizes" the behavior of `std::abs` for built-in number types, in order to simplify the definition of [linalg]'s algorithms.  However, this precedent would violate Desideratum (2).  Users have good reasons to want the const element type version of an accessor directly.

Precedent from [mdspan.sub] takes Option (5); it makes the CPO not exist at all.  The only public interface to get the mapping of the mdspan returned by `submdspan` is an unqualified call to `submdspan_mapping`.  The `mdspan` library lives in the `std` namespace.  There's no nested namespace in which to hide a CPO.  While the definition of `submdspan` attempts to find `submdspan_mapping` via ADL, that happens directly, not by means of a CPO (at least not a CPO that users can see).  This works because `submdspan` has no reasonable "fall-back" behavior for `submdspan_mapping`.  If calling it is well-formed, then `submdspan` can create the result mapping.  Otherwise, it's a custom mapping and `submdspan` would have no idea what to do with it, so `submdspan` is ill-formed.  However, this precedent violates Desideratum (1).  We don't want to force users to define the function for their custom accessors.

## Wrapper accessor naming

Existing precedent in [mdspan.accessor] (`default_accessor` and `aligned_accessor`) and [linalg] (`scaled_accessor` and `conjugated_accessor`) names accessors "`FOO_accessor`," where `FOO` is the distinguishing feature.  It also makes these accessors not exposition-only.  We also want the wrapper accessor to be not exposition-only because users might like to use it in unit tests, or as a first-pass implementation of their accessor's customization.

The wrapper accessor works analogously to the `std::ranges::as_const_view` view type.  Accessors are neither views in the sense of Ranges, nor views in the sense of [views] (including `span` and `mdspan`).  Nevertheless, the "`as_const`" part of the name still applies.  The wrapper takes an existing accessor, and makes it behave "as const," that is, as an accessor whose element type is const and that only permits read access.  This suggests the name `as_const_accessor`.

We want to use `as_const` in the name rather than just `const`, in order to avoid colliding with "constant."  A reasonable way to interpret `constant_accessor` would be an accessor where every element is just a compile-time constant.

```c++
template<class ValueType, ValueType Value>
  requires(std::is_same_v<std::remove_cvref_t<ValueType>, ValueType>)
struct constant_accessor {
  using element_type = std::add_const_t<ValueType>;
  using reference = ValueType; // access returns a value, not a reference
  using offset_policy = constant_accessor<ValueType, Value>;
  struct data_handle_type {};

  constexpr reference
  access(data_handle_type p, size_t k) const noexcept {
    return Value;
  }
  constexpr typename offset_policy::data_handle_type
  offset(data_handle_type p, size_t k) const noexcept {
    return {};
  }
};
```

## CPO naming

The CPO needs a different name from the wrapping accessor, since we have already resolved not to introduce new namespaces.

Precedent in [linalg] is `scaled_accessor` vs. `scaled`, and `conjugated_accessor` vs. `conjugated`.  There, the accessor has a name of the form "`ADJECTIVE_accessor`," while the function (not a CPO in this case) that creates an mdspan with that accessor has the name "`ADJECTIVE`."  This precedent would suggest `as_const` for the CPO.  However, `std::as_const` is already taken, and we definitely don't want to overload that with different behavior.  We also don't want to introduce another namespace to permit the same name with different behavior, as with `std::ranges::views::as_const`.

We don't want to overload `std::ranges::views::as_const`.  `mdspan` is not a range in the sense of `std::ranges::range`, so it cannot be a "view" in the sense of `std::ranges::view`.  The `mdspan` authors deliberately chose not to provide iterators.  (R3 of P0003 removed iterator support.  R2 only included `begin()` and `end()` for "contiguous" (what eventually became known as "exhaustive") layouts.)  Nevertheless, some WG21 members have expressed an interest in making `mdspan` a range.  If that happens, then we would need `std::ranges::views::as_const` to behave consistently for `mdspan`.  A reasonable way to do that would be for `std::ranges::views::as_const` to use the mechanism proposed by this paper.

Calling the CPO for accessors just "`as_const`" might also be confusing, because accessors often have no state.  The "view" (see previous paragraph for why this is in double quotes) is the data handle or the `mdspan` holding the data handle, not the accessor.

The above suggests that we want the CPO to include the words "`as_const`," but don't want to call it that alone.  The adjectival phrase "`as_const_access`" disambiguates the CPO from the accessor's noun form "`FOO_accessor`."  "`as_const_access_accessor`" would be redundant.  That leaves `as_const_access`.

## `mdspan` function naming

The previous section suggests a name including `as_const`, but not just `as_const`.

We don't want to use names that have "view" in them.  This is because the Standard defines "view" ambiguously.  The [views] section of the Standard includes both `span` and `mdspan`, yet of these, only `span` is a view in the sense of Ranges.  It could make sense to have a function that works for both `span` and `mdspan`, but it's not obvious to us what to call that function.  The `mdspan` proposal P0009 called the `submdspan` function `subspan` in R10, but changed the name to `submdspan` in R11, likely because of WG21 feedback ("Renamed `subspan` to `submdspan`, as this only applies to `mdspan`").  This suggests a WG21 preference for interfaces that keep `span` and `mdspan` separate.

We suggest `as_const_mdspan` as a reasonable disambiguation.  We only need a function, not a CPO and a class as with `as_const_access` and `as_const_accessor`.

## Wrapper accessor design

### Wrapper accessor should only permit read access

The wrapper accessor should only permit read access.  This means that its `access` function must return a `reference` type that does not permit changing the element.

If the original `reference` is cv-qualified `element_type&`, then the resulting `reference` should be `std::add_const_t<reference>`.

If the original `reference` is (possibly cv-qualified) `element_type`, then the accessor works like `const std::vector<bool>`.  That is, access returns a value.  This implies that `element_type` is const, because access offers no way to change the element.  Thus, the resulting `reference` can just be the original `reference`.

If the original `reference` is anything else, then it must be a proxy reference.  [mdspan.accessor.reqmts] 4 requires that the original `reference` models `common_reference_with<​reference&&, ​element_type&>`.  Thus, if `element_type` is const, the original `reference` must be read-only.  This means that we only have to change the original `reference` if the original `element_type` is not const.  We do that by wrapping the original proxy reference in a new proxy reference that stores the original proxy reference and has a conversion to `std::remove_cv_t<element_type>`.  We define the proxy reference like this.

```c++
template <class NestedAccessor>
class as_const_accessor_reference {
private:
  using nested_element_type = typename NestedAccessor::element_type;
  using nested_reference_type = typename NestedAccessor::reference;
  nested_reference_type ref_;
 
public:
  using value_type = std::remove_cv_t<nested_element_type>;

  constexpr as_const_accessor_reference(const as_const_accessor_reference&) = default;
  constexpr as_const_accessor_reference& operator=(const as_const_accessor_reference&) = default;

  constexpr explicit as_const_accessor_reference(nested_reference_type ref)
    noexcept(noexcept(ref_(ref)))
      : ref_(ref)
  {}

  constexpr explicit((! std::is_convertible_v<nested_reference_type, value_type>))
    operator value_type() const noexcept(noexcept(value_type(ref_)))
  {
    return value_type(ref_);
  }
};
```

# Should we generalize to "Accessor element type rebind"?

## What does "Accessor element type rebind" mean?

We want to solve this problem: "Given an arbitrary accessor, get the 'same' accessor but with const `element_type`."  We could instead solve a more general problem: "Given an arbitrary accessor, get the 'same' accessor but with a possibly arbitrarily different `element_type`."  We call this "*rebinding* the accessor's element type."  It turns out that we don't actually want to be able to solve this more general problem, but thinking about it will help us understand the design better.

Rebinding the accessor's element type would have two steps.

1. Find the new accessor's type
2. Construct the new accessor in some way from the old accessor

Note that rebinding does not necessarily mean creating a new data handle from the old data handle.  We don't have to try to view the same elements with a different `element_type`.  For example, rebinding `default_accessor<float>` to `default_accessor<int>` just means getting a new accessor with the "same properties" as the old one, not viewing the original `float` elements as `int` elements.  That's why `default_accessor<int>` is not constructible from `default_accessor<float>`.  It's also why we wrote "in some way" above; we don't necessarily imply constructibility or convertibility.  On the other hand, if an accessor `B` is constructible (or convertible) from another accessor `A`, that strongly suggests that `B::data_handle_type` should be constructible (or convertible) from `A::data_handle_type`.  (This is perhaps a missing requirement in the Standard.)

If we had rebinding and wanted to solve the original problem, we would also need a way to construct the new accessor's `data_handle_type` from the old accessor's `data_handle_type`.  We'll call that the third step in what follows.

We know how to rebind the Standard accessors `default_accessor` and `aligned_accessor`.  All these accessors can be represented by any class template `template<class ElementType, class...> Accessor`, where the first template parameter is the element type.

1. `Accessor<ElementType, Args...>` becomes `Accessor<std::add_const_t<ElementType>, Args...>`
2. `Accessor<std::add_const_t<ElementType>, Args...>` is constructible from  `Accessor<ElementType, Args...>`
3. `Accessor<std::add_const_t<ElementType>, Args...>::data_handle_type` is constructible from  `Accessor<ElementType, Args...>::data_handle_type`

This process does NOT necessarily work for arbitrary accessors.  Nothing currently requires accessors to have this form.  Accessors don't need to have template parameters at all.  If they do, the first template parameter doesn't need to be the element type.

## Allocators have "value type rebind"

C++ Standard allocators have "value type rebind."  Given any allocator `a_u` of type `A_U` whose value type is `U`, and a new value type `V`, users can rebind `V` to a new allocator `a_v` of type `A_V` whose value type is `V`.  This has two steps.

1. Get the new allocator type: `using A_V = allocator_traits<A_U>::rebind_alloc<V>`
2. Create the new allocator by copy or move construction from the original allocator

`allocator_traits<A_U>::rebind_alloc<V>` is defined to be `A_U::rebind<V>::other` if that type alias is present, otherwise `SomeAllocator<V, Args>` if `A_U` is of the form `SomeAllocator<U, Args>`, where `Args` is zero or more type arguments.  That is, one can always use `allocator_traits` to rebind the allocator type.

The *Allocator* named requirements also require that a rebound allocator is (both copy and move) constructible from the original allocator.  That is, an object of type `allocator_traits<A_U>::rebind_alloc<V>` can always be constructed from an object of type `A_U`.

One can say that the C++ Standard's allocators separate "policy" (`allocator_traits`) from the allocator itself.  The policy defines a function from value type to allocator type.

## Why don't accessors work like allocators?

The Standard requires that an allocator can be rebound to any value type.  This makes sense because all allocators produce allocations that "look the same" to C++ code.  As long as the same allocator is used to allocate and deallocate the memory, C++ code can access it just as if it were created via ordinary `new` and `delete`.  Thus, for any type `T`, as long as an allocator can allocate a `char` array with the same alignment as `alignof(T)`, it can allocate for `T`.

Accessors are not required to make sense for every possible element type.  For example, a user might define a "lock-free atomic accessor" that uses `atomic_ref<element_type>` as its `reference` type.  This would only make sense if `atomic_ref<element_type>::is_always_lock_free` is `true`.

In general, accessors don't even have to access C++ memory.  Accessors introduce to the C++ Standard the possibility of separate "memory" spaces (where "memory" is in quotes because actual memory need not be involved) that are inaccessible to ordinary C++ code, except through the accessor's `access` function.  Those separate spaces may have their own requirements on types, such as trivial copyability or vectorizability ([simd.general] 2).

This tells us that general rebinding of an arbitrary accessor's element type might not make sense.  Instead, we should focus on the nonconst-to-const case.

Even rebinding const to nonconst might not make sense.  This is because some accessors access in a fundamentally read-only way, so it only ever makes sense for them to have const `element_type`.  We showed one example above: a "`constant_accessor`" where every element is just a compile-time constant.

## Layout mappings have "extents rebind"

Layout mappings were explicitly designed to separate "policy" (the layout type, like `layout_right`) from the mapping itself (e.g., `layout_right::mapping<extents<int, 3, 5, 7>>`).  The policy describes a function from extents type to layout mapping type.

The Standard layout mappings generally also permit conversion between mapping types with the same layout type and convertible extents types.  The conversion is implicit only if the corresponding conversion between extents types would also be implicit.  This "rebind" facility is available for all the Standard layout types, as the `layout_stride` example below shows.

```c++
using original_extents_type = extents<int, 3, dynamic_extent, 7>;
std::array strides{1, 5, 15};
layout_stride::mapping<original_extents_type> map0(original_extents_type(3, 5, 7), strides);

// Conversion with no preconditions is implicit.
layout_stride::mapping<dims<3, int>> map1 = [&] () { return map0; } ();

// Conversion with preconditions is explicit.
layout_stride::mapping<extents<int, 3, 5, 7>> map2(map0);
```

These two features together form a facility to rebind a layout mapping with a different extents object.  The layout policy enables rebinding the mapping type, and the mapping's constructor from a mapping with a different but convertible extents type enables rebinding the mapping instance.

Note that mappings are not required to have this conversion.  They are only required to permit rebinding the type, as long as the mapping type supports an extents type of that rank (see proposed resolution of [LWG 4582](https://cplusplus.github.io/LWG/lwg-active.html#4582)).  It is perhaps a design flaw that the requirements include type rebinding but not conversion.

## Why don't accessors work like layout mappings?

Accessors don't separate "policy" type from the actual accessor.  One could imagine a different `mdspan` design in which the actual accessor type were a function of an accessor policy type and the element type.  It might look like this.

```c++
using original_accessor_type = default_accessor_policy::accessor<float>;
using new_accessor_type = original_accessor_policy::policy_type::accessor<const float>;
```

Layout mappings have a public `layout_type` type alias to get back to the layout policy, and that has a public `mapping<Extents>` type alias to rebind the mapping type.  A hypothetical accessor design could analogously put a `policy_type` alias in every accessor to get back to the accessor policy, and an `accessor<ElementType>` alias in the policy to rebind the accessor type.  If we also required all accessors (not just the Standard ones) to have constructors for conversion from nonconst to const element type, then that would solve our problem, at least syntactically.

We could draw an analogy between rebinding a layout mapping into a new mapping with equal extents of different type, and rebinding an accessor with nonconst element type into a new accessor with const element type.  The two extents objects `extents<int, 3, dynamic_extent, 7>(3, 5, 7)` to `extents<int, 3, 5, 7>()` are equal and represent the same multidimensional index space.  Going from the first to the second adds compile-time restrictions, analogous to going from nonconst to const element type with accessors.

```c++
layout_left::mapping map0(extents<int, 3, dynamic_extent, 7>(3, 5, 7));
// Explicit, because of the precondition that map0.extents().extent(1) == 5. 
layout_left::mapping<extents<int, 3, 5, 7>> map1(map0);
```

The analogy breaks down because changing from a nonconst to const `element_type` might change how the accessor accesses its elements.  For example, a nonconst accessor that represents elements on a networked file system might perform destructive, not-concurrent-safe accesses even on reads, because it happens to be more efficient on that platform.  It would be reasonable for users to assume that a const accessor would perform concurrent-safe reads.

Perhaps a destructively reading accessor would be a bad design.  Iterators are supposed to perform safe concurrent reads of distinct elements, regardless of whether the iterator is const or nonconst.  Shouldn't accessors behave the same way?  Shouldn't syntactically read-only access always be semantically read-only?

## Nonconst to const is all we need

The above discussion suggests that all we need is an ability to rebind an accessor from nonconst element type to const element type.

We do _not_ need the ability to rebind between arbitrary element types, e.g., from `float` to `std::string`, or from `const float` to `float`.

## Take inspiration from Ranges design

For an arbitrary accessor with nonconst element type, it needs to "make semantic sense" to rebind the accessor to have const element type (and to make its `reference` type syntactically read-only) without otherwise changing its behavior.

If we accept this, then we can take an approach similar to `std::ranges::views::as_const`.  That is, if the Standard "knows" how to get the const element type version of an accessor, then it can do so.  Otherwise, it can wrap the accessor in another accessor with const element type and with a `reference` type that only permits reads, analogously to how `std::ranges::as_const_view` wraps a base view.

## Unlike Ranges, use a customization point

How should the Standard "know" how to get the const element type version of an accessor?  Ranges does this with an enumeration of various possibilities in [range.as.const.overview] 2.  There is no customization point to change the behavior of `views::as_const` for user-defined types.  User-defined ranges that aren't already `constant_range` always get wrapped in `as_const_view`.  This is idiomatic Ranges design, but it's not idiomatic `mdspan` design.  For example, `submdspan` gets the resulting layout mapping from the `submdspan_mapping` customization point, and the resulting accessor from the (possibly custom) input accessor's `offset` function.

This suggests that we want a customization point so that users can define what "const version of an accessor" means for their own accessor types.

# Implementation

[This Compiler Explorer link](https://godbolt.org/z/rhM378djn) has a brief implementation.

One coauthor has an implementation of an earlier draft of this proposal in a branch of the [CCCL](https://github.com/NVIDIA/cccl/) repository.

# Proposed wording

> Text in blockquotes is not proposed wording, but rather instructions for generating proposed wording.

## Increment `__cpp_lib_mdspan` feature test macro

> In [version.syn], increase the value of the `__cpp_lib_mdspan` macro by replacing YYYMML below with the integer literal encoding the appropriate year (YYYY) and month (MM).

```
#define __cpp_lib_mdspan YYYYMML // also in <mdspan>
```

## Change [mdspan.syn]

> Change [mdspan.syn] (“Header <mdspan> synopsis”) as follows.

```
  // [mdspan.layout], layout mapping
  struct layout_left;
  struct layout_right;
  struct layout_stride;
  template<size_t PaddingValue = dynamic_extent>
    struct layout_left_padded;
  template<size_t PaddingValue = dynamic_extent>
    struct layout_right_padded;

  // [mdspan.accessor.default], class template default_accessor
  template<class ElementType>
    class default_accessor;

  // [mdspan.accessor.aligned], class template aligned_accessor
  template<class ElementType, size_t ByteAlignment>
    class aligned_accessor;
``` 
::: add
```

  // [mdspan.accessor.as_const_accessor], class template as_const_accessor
  class as_const_accessor;

  // [mdspan.accessor.as_const_access], customization point object as_const_access
  inline namespace @_unspecified_@ {
    inline constexpr @_unspecified_@ as_const_access = @_unspecified_@;
  }

  // [mdspan.accessor.as_const_mdspan], function template as_const_mdspan
  template<class ElementType, class Extents, class Layout, class Accessor>
  constexpr auto as_const_mdspan(mdspan<ElementType, Extents, Layout, Accessor>);

```
:::
```
  // [mdspan.mdspan], class template mdspan
  template<class ElementType, class Extents, class LayoutPolicy = layout_right,
           class AccessorPolicy = default_accessor<ElementType>>
    class mdspan;
```

## Add new section [mdspan.accessor.as_const_accessor]

> Add a new section "[mdspan.accessor.as_const_accessor], Class template `as_const_accessor`" between [mdspan.accessor.aligned] and [mdspan.mdspan],
> with the following content.

### [mdspan.accessor.as_const_accessor.overview] Overview

[1]{.pnum} The class template `as_const_accessor` is an `mdspan` accessor policy that wraps an existing `mdspan` accessor policy, and provides read-only access to the elements accessed by the wrapped policy.  It is part of the implementation of `as_const_access` ([mdspan.accessor.as_const_access]).

```
template <class NestedAccessor>
class as_const_accessor {
public:
  using data_handle_type = typename NestedAccessor::data_handle_type;
  using element_type = std::add_const_t<typename NestedAccessor::element_type>;
  using offset_policy = as_const_accessor<typename NestedAccessor::offset_policy>;

  // [mdspan.accessor.as_const_accessor.ref] Proxy reference type
  using reference = @_as-const-accessor-reference_@;
 
  // [mdspan.accessor.as_const_accessor.cons] Constructors
  constexpr as_const_accessor(NestedAccessor acc);

  template<class OtherNestedAccessor>
    explicit(@_see below_@)
      constexpr as_const_accessor(OtherNestedAccessor acc);

  // [mdspan.accessor.as_const_accessor.members] Members
  constexpr reference access(data_handle_type handle, std::size_t k) const
    noexcept(noexcept(reference(acc_.access(handle, k))));

  constexpr typename offset_policy::data_handle_type
    offset(data_handle_type handle, std::size_t k) const
      noexcept(noexcept(@_see below_@));

private:
  NestedAccessor acc_; // @_exposition-only_@
};
```

[2]{.pnum} *Constraints*: `NestedAccessor` meets the accessor policy requirements ([mdspan.accessor.reqmts]).

[3]{.pnum} Each specialization `as_const_accessor<NA>` of `as_const_accessor` models [`copyable`](https://eel.is/c++draft/concepts.object#concept:copyable) and

* [3.1]{.pnum} `is_nothrow_move_constructible_v<as_const_accessor<NA>>` is `true` if `is_nothrow_move_constructible_v<NA> is `true`,
 
* [3.2]{.pnum} `is_nothrow_move_assignable_v<as_const_accessor<NA>>` is `true` if `is_nothrow_move_assignable_v<NA> is `true`, and

* [3.3]{.pnum} `is_nothrow_swappable_v<as_const_accessor<NA>>` is `true` if `is_nothrow_swappable_v<NA> is `true`.

[4]{.pnum} Each specialization `as_const_accessor<NA>` is a trivially copyable type if `NA` is a trivially copyable type.

### [mdspan.accessor.as_const_accessor.ref] Proxy reference type

[1]{.pnum} _`as-const-accessor-reference`_ denotes the following exposition-only class type defined in the scope of `as_const_accessor`.

```
class @_as-const-accessor-reference_@ { // exposition-only
private:
  using nested_element_type = typename NestedAccessor::element_type;
  using nested_reference_type = typename NestedAccessor::reference;
  nested_reference_type ref_;
 
public:
  using value_type = std::remove_cv_t<nested_element_type>;

  constexpr explicit
    as_const_accessor_reference(nested_reference_type ref)
      noexcept(noexcept(nested_reference_type(ref)))
      : ref_(ref)
  {}

  constexpr operator value_type() const
    noexcept(noexcept(value_type(ref_)))
  {
    return value_type(ref_);
  }
};
```

### [mdspan.accessor.as_const_accessor.cons] Constructors

```
  constexpr as_const_accessor(NestedAccessor acc);
```

[1]{.pnum} *Effects*: Direct-non-list-initializes `acc_` with `acc`.

```
  template<class OtherNestedAccessor>
    explicit(@_see below_@)
      constexpr as_const_accessor(OtherNestedAccessor acc)
        : acc_(acc)
  {}
```

[2]{.pnum} *Constraints*:

* [2.1]{.pnum} `OtherNestedAccessor` meets the accessor policy requirements ([mdspan.accessor.reqmts]).
 
* [2.2]{.pnum} `NestedAccessor` is constructible from `OtherNestedAccessor`.

[3]{.pnum} *Effects*: Direct-non-list-initializes `acc_` with `acc`.

[4]{.pnum} *Remarks*: The expression inside `explicit` is equivalent to: `! std::is_convertible_v<OtherNestedAccessor, NestedAccessor>`.

### [mdspan.accessor.as_const_accessor.members] Members

```
  constexpr reference access(data_handle_type handle, std::size_t k) const
    noexcept(noexcept(reference(acc_.access(handle, k))));
```

[1]{.pnum} *Returns*: `reference(acc_.access(handle, k))`.

```
  constexpr typename offset_policy::data_handle_type
    offset(data_handle_type handle, std::size_t k) const
      noexcept(noexcept(@_see below_@))
  {
    return typename offset_policy::data_handle_type(acc_.offset(handle, k));
  }
```

[2]{.pnum} *Returns*: `typename offset_policy::data_handle_type(acc_.offset(handle, k))`.

[3]{.pnum} *Remarks*: The expression inside the `noexcept` specifier is equivalent to: `typename offset_policy::data_handle_type(acc_.offset(handle, k))`.

## Add new section [mdspan.accessor.as_const_access] 

> Add a new section "[mdspan.accessor.as_const_access], Customization point object `as_const_access`" between [mdspan.accessor.as_const_accessor] and [mdspan.accessor.aligned], with the following content.

[1]{.pnum} Given an `mdspan` accessor policy `a` of type `A`, a "const element type version of `a`" (if it exists) is an `mdspan` accessor `b` of a possibly different type `B`, where

* [1.1]{.pnum} `B::data_handle_type` is constructible from `A::data_handle_type`;

* [1.2]{.pnum} `B::reference` only permits read access to the element to which it refers; and

* [1.3]{.pnum} if `a_dh` is `A::data_handle_type` and `b_dh` is `B::data_handle_type(a_dh)`, then

    * [1.3.1]{.pnum} `b` and `b_dh` have the same accessible range as `a` and `a_dh`, and 

    * [1.3.2]{.pnum} `b.access(b_dh, k)` and `a.access(a_dh, k)` access the same element for `k` in the accessible range of `a_dh`.

[*Example*: A const element type version of any `default_accessor<T>` instance is any `default_accessor<const T>` instance, and a const element type version of any `aligned_accessor<T, ByteAlignment>` instance is any `aligned_accessor<const T, ByteAlignment>` instance. --*end example*]

[2]{.pnum} Let `A` be an `mdspan` accessor policy.  If the expression `as_const(std::declval<A>()).as_const_access()` is well-formed, then the type of that expression is a const element type version of `A`, else the program is ill-formed, no diagnostic required.

[*Editorial note*: Consider whether checking all of the requirements at compile time is always possible.  If so, then we can change IFNDR to simply ill-formed.  Also, consider moving the above paragraph to [mdspan.accessor.reqmts]. --*end editorial note*]

[3]{.pnum} The name `as_const_access` denotes a customization point object.  For a subexpression `a`, let `A` denote `decltype(a)`.  The expression `as_const_access(a)` is equivalent to the following.

* [3.1]{.pnum} `default_accessor<const T>()`, if `A` is `default_accessor<T>`;

* [3.2]{.pnum} `aligned_accessor<const T, ByteAlignment>, if `A` is `aligned_accessor<T, ByteAlignment>`;
 
* [3.3]{.pnum} `a`, if `A::element_type` is const;

* [3.4]{.pnum} `a.as_const_access()`, if that expression is well-formed;

* [3.5]{.pnum} `as_const_accessor(a)`, otherwise.

## Add new section [mdspan.accessor.as_const_mdspan] 

> Add a new section "[mdspan.accessor.as_const_mdspan], Function template `as_const_mdspan`" between [mdspan.accessor.as_const_access] and [mdspan.accessor.aligned], with the following content.

```
  template<class ElementType, class Extents, class Layout, class Accessor>
  constexpr auto as_const_mdspan(mdspan<ElementType, Extents, Layout, Accessor> x);
```

[1]{.pnum} *Effects*: Equivalent to:

```
  using result_accessor_type = decltype(as_const_access(x.accessor()));
  using result_data_handle_type = typename result_accessor_type::data_handle_type;
  return mdspan(result_data_handle_type(x.data_handle()), x.mapping(), as_const_access(x.accessor()));
```

# Acknowledgments

Thanks to our colleague Sam Li (NVIDIA) for reviewing the proposal!