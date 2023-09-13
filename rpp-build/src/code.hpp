namespace rpp {
    using i8 = int8_t;
    using i16 = int16_t;
    using i32 = int32_t;
    using i64 = int64_t;
    using u8 = uint8_t;
    using u16 = uint16_t;
    using u32 = uint32_t;
    using u64 = uint64_t;
    using f32 = float;
    using f64 = double;

    #if defined(_WIN32)
    using isize = SSIZE_T;
    #else
    using isize = ssize_t;
    #endif

    // The identity transformation
    template <typename T>
    using identity = T;

    // Rust Option type repr
    template <typename T>
    class alignas(T) option {
    public:
        option(T value) : value(value) {}
        operator T() const noexcept { return value; }
    private:
        T value;
    };

    // Check if function exists
    template <typename T>
    constexpr bool exists(T fun) { return sizeof(static_cast<T>(fun)) >= 1; }

    // Check is type exists
    template <typename T>
    constexpr bool exists() { return sizeof (T) >= 1; }

    // Trivially relocatable marker
    template <typename T>
    const bool is_trivially_relocatable = std::is_trivially_copyable_v<T> || std::is_void_v<T> || std::is_reference_v<T>;

    template <typename T> void clone_helper(const void *src, void *dst) {
        ::new (dst) T(*static_cast<const T *>(src));
    }

    template <typename T> void move_helper(T &&src, T *dst) {
        ::new (static_cast<void *>(dst)) T(std::move(src));
    }

    template <typename T> void drop_helper(void *ptr) {
        static_cast<T *>(ptr)->~T();
    }
}
