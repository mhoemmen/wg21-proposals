#include <any>
#include <cassert>
#include <iterator>
#include <span>

#define PROPOSED_CHANGE 1

namespace std2 {

  using std::dynamic_extent;
  using std::ptrdiff_t;
  using std::size_t;

  template<class ElementType, size_t Extent = dynamic_extent>
  class span;

  template<class T>
  constexpr bool is_span_v = false;
  template<class ElementType, size_t Extent>
  constexpr bool is_span_v<span<ElementType, Extent>> = true;
  template<class ElementType, size_t Extent>
  constexpr bool is_span_v<std::span<ElementType, Extent>> = true;

  template<class T>
  constexpr bool is_std_array_v = false;
  template<class ValueType, size_t Extent>
  constexpr bool is_std_array_v<std::array<ValueType, Extent>> = true;

  template<class ElementType, size_t Extent>
  class span {
  private:
    ElementType* data_ = nullptr;
    size_t size_ = Extent == dynamic_extent ? size_t() : Extent;

  public:
    // constants and types
    using element_type = ElementType;
    using value_type = std::remove_cv_t<ElementType>;
    using size_type = size_t;
    using difference_type = ptrdiff_t;
    using pointer = element_type*;
    using const_pointer = const element_type*;
    using reference = element_type&;
    using const_reference = const element_type&;
    using iterator = pointer;
    using const_iterator = const_pointer;
    using reverse_iterator = std::reverse_iterator<iterator>;
#if ! defined(__clang__)
    // libc++ doesn't seem to define std::const_iterator or
    // std::make_const_iterator in <iterator>.
    using const_reverse_iterator = std::const_iterator<reverse_iterator>;
#else
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;
#endif
    static constexpr size_type extent = Extent;

    // [span.cons], constructors, copy, and assignment
    constexpr span() noexcept
      requires(Extent == dynamic_extent || Extent == 0) = default;

    template<class It>
      requires(
        std::contiguous_iterator<
          It
        > &&
        std::is_convertible_v<
          std::remove_reference_t<std::iter_reference_t<It>>(*)[],
          element_type(*)[]
        >
      )
      constexpr explicit(extent != dynamic_extent)
        span(It first, size_type count)
          : data_(::std::to_address(first)), size_(count) {}

    template<class It, class End>
      requires(
        std::is_convertible_v<
          std::remove_reference_t<std::iter_reference_t<It>>(*)[],
          element_type(*)[]
        > &&
        std::contiguous_iterator<
          std::remove_reference_t<std::iter_reference_t<It>>
        > &&
        std::sized_sentinel_for<End, It> &&
        ! std::is_convertible_v<End, size_t>
      )
      constexpr explicit(extent != dynamic_extent)
        span(It first, End last)
          : data_(::std::to_address(first)), size_(last - first) {}
  
    template<size_t N>
      constexpr span(std::type_identity_t<element_type> (&arr)[N]) noexcept
      requires(
        (extent == dynamic_extent || N == extent) && 
        std::is_convertible_v<
          std::remove_pointer_t<decltype(std::data(arr))>(*)[],
          element_type(*)[]
        >
      ) : data_(arr), size_(N) {}

    template<class T, size_t N>
      constexpr span(std::array<T, N>& arr) noexcept
      requires(
        (extent == dynamic_extent || N == extent) && 
        std::is_convertible_v<
          std::remove_pointer_t<decltype(std::data(arr))>(*)[],
          element_type(*)[]
        >
      ) : data_(arr.data()), size_(N) {}

    template<class T, size_t N>
      constexpr span(const std::array<T, N>& arr) noexcept
      requires(
        (extent == dynamic_extent || N == extent) && 
        std::is_convertible_v<
          std::remove_pointer_t<decltype(std::data(arr))>(*)[],
          element_type(*)[]
        >
      ) : data_(arr.data()), size_(N) {}

    template<class R>
      requires(
        std::ranges::contiguous_range<
          std::remove_reference_t<std::ranges::range_reference_t<R>>
        > &&
        std::ranges::sized_range<
          std::remove_reference_t<std::ranges::range_reference_t<R>>
        > &&
        (
          std::ranges::borrowed_range<
            std::remove_reference_t<std::ranges::range_reference_t<R>>
          > ||
          std::is_const_v<element_type>
        ) &&
        ! is_span_v<std::remove_cvref_t<R>> &&
        ! is_std_array_v<std::remove_cvref_t<R>> &&
        ! std::is_array_v<std::remove_cvref_t<R>> &&
        std::is_convertible_v<
          std::remove_reference_t<std::ranges::range_reference_t<R>>(*)[],
          element_type(*)[]
        >       
      )
      constexpr explicit(extent != dynamic_extent)
        span(R&& r)
          : data_( ::std::ranges::data(r)),
            size_( ::std::ranges::size(r))
        {}

#if defined(PROPOSED_CHANGE)
    template<std::same_as<value_type> InitListValueType>
      constexpr explicit(extent != dynamic_extent)
        span( ::std::initializer_list<const InitListValueType> il)
          requires(std::is_const_v<element_type>)
      : data_(std::data(il)), size_(il.size())
    {
      assert(size_ == 3);
    }
#endif        

    constexpr span(const span& other) noexcept = default;

    template<class OtherElementType, size_t OtherExtent>
      requires(
        extent == dynamic_extent ||
        OtherExtent == dynamic_extent ||
        extent == OtherExtent
      )
      constexpr
      explicit(
        extent != dynamic_extent &&
        OtherExtent == dynamic_extent
      )
      span(const span<OtherElementType, OtherExtent>& s) noexcept
        : data_(s.data()), size_(s.size())
      {}

    constexpr span& operator=(const span& other) noexcept = default;

    // [span.sub], subviews
    template<size_t Count>
      constexpr span<element_type, Count> first() const {
        return span<element_type, Count>(data(), Count);
      }
    template<size_t Count>
      constexpr span<element_type, Count> last() const {
        return span<element_type, Count>(data() + (size() - Count), Count);
      }
    template<size_t Offset, size_t Count = dynamic_extent>
      constexpr span<element_type,
        Count != dynamic_extent ? Count
          : (Extent != dynamic_extent ? Extent - Offset
            : dynamic_extent)
      > subspan() const {
        static_assert(Offset <= Extent && (Count == dynamic_extent || Count <= Extent - Offset));
        static constexpr size_t ResultExtent =
          Count != dynamic_extent ? Count
            : (Extent != dynamic_extent ? Extent - Offset
              : dynamic_extent);
        return span<ElementType, ResultExtent>(
          data() + Offset, Count != dynamic_extent ? Count : size() - Offset);
      }

    constexpr span<element_type, dynamic_extent> first(size_type count) const {
      return span<element_type, dynamic_extent>(data(), count);
    }
    constexpr span<element_type, dynamic_extent> last(size_type count) const {
      return span<element_type, dynamic_extent>(data() + (size() - count), count);
    }
    constexpr span<element_type, dynamic_extent> subspan(
      size_type offset, size_type count = dynamic_extent) const {
        return span<element_type, dynamic_extent>(
          data() + offset, count == dynamic_extent ? size() - offset : count
        );
      }

    // [span.obs], observers
    constexpr size_type size() const noexcept {
      return Extent == dynamic_extent ? size_ : Extent;
    }
    constexpr size_type size_bytes() const noexcept {
      return size() * sizeof(element_type);
    }
    constexpr bool empty() const noexcept {
      return size() == 0;
    }

    // [span.elem], element access
    constexpr reference operator[] (size_type idx) const {
      return data_[idx];
    }
    constexpr reference at(size_type idx) const {
      if (idx >= size()) {
        throw std::out_of_range("uh oh");
      }
      return data_[idx];
    }
    constexpr reference front() const {
      return *data();
    }
    constexpr reference back() const {
      return *(data() + (size() - 1));
    }
    constexpr pointer data() const noexcept {
      return data_;
    }

    // [span.iterators], iterator support
    constexpr iterator begin() const noexcept {
      return data();
    }
    constexpr iterator end() const noexcept {
      return data() + size();
    }
    constexpr const_iterator cbegin() const noexcept { return begin(); }
    constexpr const_iterator cend() const noexcept { return end(); }
    constexpr reverse_iterator rbegin() const noexcept {
      return reverse_iterator(end());
    }
    constexpr reverse_iterator rend() const noexcept {
      return reverse_iterator(begin());
    }
    constexpr const_reverse_iterator crbegin() const noexcept {
      return rbegin();
    }
    constexpr const_reverse_iterator crend() const noexcept {
      return rend();
    }
  };

  // [span.objectrep], views of object representation
  template<class ElementType, size_t Extent>
    requires(! ::std::is_volatile_v<ElementType>)
  span<const std::byte, Extent == dynamic_extent ? dynamic_extent : sizeof(ElementType) * Extent>
    as_bytes(span<ElementType, Extent> s) noexcept {
      using R = span<const std::byte, Extent == dynamic_extent ? dynamic_extent : sizeof(ElementType) * Extent>;
      return R{reinterpret_cast<const std::byte*>(s.data()), s.size_bytes()};
    }

  template<class ElementType, size_t Extent>
    requires(! ::std::is_const_v<ElementType> && ! ::std::is_volatile_v<ElementType>)
  span<std::byte, Extent == dynamic_extent ? dynamic_extent : sizeof(ElementType) * Extent>
    as_writable_bytes(span<ElementType, Extent> s) noexcept {
      using R = span<std::byte, Extent == dynamic_extent ? dynamic_extent : sizeof(ElementType) * Extent>;
      return R{reinterpret_cast<std::byte*>(s.data()), s.size_bytes()};
    }
} // namespace std2

namespace std::ranges {
  template<class ElementType, size_t Extent>
    constexpr bool enable_view< ::std2::span<ElementType, Extent>> = true;
  template<class ElementType, size_t Extent>
    constexpr bool enable_borrowed_range< ::std2::span<ElementType, Extent>> = true;
}

bool bool_true() { return true; }
bool bool_false() { return false; }

int main() {
#if defined(PROPOSED_CHANGE)
  {
    std2::span<const bool> b{true, false, true};
    assert(b.size() == 3);
    assert(b[0] && ! b[1] && b[2]);
  }
  {
    std2::span<const bool> b{bool_true(), bool_false(), bool_true()};
    assert(b.size() == 3);
    assert(b[0] && ! b[1] && b[2]);
  }

  //{
  //  std2::span<const bool> b{1, 0, 1};
  //  assert(b.size() == 3);
  //  assert(b[0] && ! b[1] && b[2]);
  //}
  //{
  //  std2::span<const std::any> b{"foo", true, 1.25f};
  //  assert(b.size() == 3);
  //}
  {
    std2::span<const std::any> b{std::any{"foo"}, std::any{true}, std::any{1.25f}};
    assert(b.size() == 3);
  }
  {
    std2::span<const float> b{1.0f, 2.0f, 3.0f};
    assert(b.size() == 3);
    assert(b[0] == 1.0f && b[1] == 2.0f && b[2] == 3.0f);
  }
  //{
  //  std2::span<const float> b{1.0, 2.0, 3.0};
  //  assert(b.size() == 3);
  //  assert(b[0] == 1.0f && b[1] == 2.0f && b[2] == 3.0f);
  //}
  //{
  //  std2::span<const float> b{1, 2, 3};
  //  assert(b.size() == 3);
  //  assert(b[0] == 1.0f && b[1] == 2.0f && b[2] == 3.0f);
  //}

#endif

  return 0;
}