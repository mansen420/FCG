#pragma once

#include <algorithm>
#include <cstddef>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <iterator>
#include <ostream>
#include <sys/types.h>
#include <type_traits>
#include <utility>

constexpr uint DYNAMIC = 0;
struct empty 
{
    template <typename...types>
    empty(types...args){}
    
    template <typename...types>
    empty& operator=(types...args){return *this;}
    
    template <typename T>
    operator T()const{return T(0);}
};

template <typename T, size_t dim = DYNAMIC, bool inlined = 0>
requires (!(inlined && dim == DYNAMIC))
class list
{
    template<typename D, size_t size, bool inl>
requires (!(inl && size == DYNAMIC))
friend class list;

    [[no_unique_address]] std::conditional_t<dim == 0, size_t, empty> dynamicSize;
    [[no_unique_address]] std::conditional_t<inlined, empty, bool> ownsData = true;

    //TODO use template lambdas
    typedef std::function<T(size_t idx)> builder;
    typedef std::function<T(T value, size_t idx)> mapping;
    typedef std::function<void(T& value, size_t idx)> mutation;
    typedef std::function<void(T val, size_t idx)> process;
    template <typename D>
    using reduction = std::function<D(T val, size_t idx, D prev)>;

protected:
    std::conditional_t<inlined, T[dim], T*> data;
public:
    /// @return Number of elements in the list evaluated at compile time.
    inline constexpr size_t size()const requires (dim != DYNAMIC){return dim;}
    /// @return Number of elements in the list fetched at runtime. 
    inline size_t size()const requires (dim == DYNAMIC){return dynamicSize;}

    T& operator[](size_t idx)
    {
        assert(idx < size());
        return this->data[idx];
    }
    const T& operator[](size_t idx)const
    {
        assert(idx < size());
        return this->data[idx];
    }
    const T& operator()(size_t idx)const
    {
        assert(idx < size());
        return this->data[idx];
    }
    
    //******************************************************************************************//
    //                                      SUBLIST                                             //
    //******************************************************************************************//

    /// @brief
    /// Get sublist spanning two indices specified in run-time. Use the compile-time version when possible.
    /// @param fromIdx 
    /// Index of first element of the sublist, satisfying fromIdx <= toIdx
    /// @param toIdx 
    /// index of one past the final element of the sublist, satisfying toIdx <= size() + fromIdx.
    /// @return 
    /// A non-owning list pointing at the element at fromIdx, with a dynamic size of toIdx - fromIdx.
    list<T, DYNAMIC> sublist(size_t fromIdx, size_t toIdx)
    {
        assert (fromIdx + toIdx <= this->size() && fromIdx <= toIdx);
        return list<T, DYNAMIC>(this->data + fromIdx, toIdx - fromIdx);
    }

    /// @brief
    /// Get const sublist spanning two indices specified in run-time. Use the compile-time version when possible.
    /// @param fromIdx 
    /// Index of first element of the sublist, satisfying fromIdx <= toIdx
    /// @param toIdx 
    /// index of one past the final element of the sublist, satisfying toIdx <= size() + fromIdx.
    /// @return 
    /// A const non-owning list pointing at the element at fromIdx, with a dynamic size of toIdx - fromIdx.
    const list<T, DYNAMIC> sublist(size_t fromIdx, size_t toIdx)const 
    {
        assert (fromIdx + toIdx <= this->size() && fromIdx <= toIdx);
        return list<T, DYNAMIC>(this->data + fromIdx, toIdx - fromIdx);
    }
    /** 
     * @brief
     * Get sublist spanning two indices specified at compile-time.
     * @return
     * A non-owning list pointing at the element at fromIdx, with a static size of toIdx - fromIdx
     */
    template <size_t fromIdx, size_t toIdx>
    requires (fromIdx + toIdx <= dim && fromIdx <= toIdx) 
    list<T, toIdx - fromIdx> sublist()
    {
       assert (fromIdx + toIdx <= this->size() && fromIdx <= toIdx);
       return list<T, toIdx - fromIdx>(this->data + fromIdx); 
    } 
     /** 
     * @brief
     * Get const sublist spanning two indices specified at compile-time.
     * @return
     * A const non-owning list pointing at the element at fromIdx, with a static size of toIdx - fromIdx
     */
    template <size_t fromIdx, size_t toIdx>
    requires (fromIdx + toIdx <= dim && fromIdx <= toIdx) 
    const list<T, toIdx - fromIdx> sublist()const
    {
       assert (fromIdx + toIdx <= this->size() && fromIdx <= toIdx);
       return list<T, toIdx - fromIdx>(this->data + fromIdx); 
    }   

    //TODO make ctor that takes std::arrays as argument
    list<T, DYNAMIC> operator()(size_t fromIdx, size_t toIdx) requires(!inlined)
    {
        return sublist(fromIdx, toIdx);
    }
    const list<T, DYNAMIC> operator()(size_t fromIdx, size_t toIdx) const
    {
        return sublist(fromIdx, toIdx);
    }

    //******************************************************************************************//
    //                                    ITERATORS                                             //
    //******************************************************************************************//
    
    const T* begin()const requires(!inlined){return this->data;}
    const T* end()const requires(!inlined){return this->data + this->size();}
    T* begin()requires(!inlined){return this->data;}
    T* end()requires(!inlined){return this->data + this->size();}
    
    const auto begin()const requires(inlined){return std::begin(data);}
    const auto end()const requires(inlined){return std::end(data);}
    auto begin()requires(inlined){return std::begin(data);}
    auto end()requires(inlined){return std::end(data);}
    
    /// @brief Last element in list satisfying size() > 0.
    /// @return Non-const reference to last element in list.
    inline T& last()
    {
        assert(this->size() > 0);
        return this->data[this->size() - 1];
    }

    //******************************************************************************************//
    //                                   COPY SEMANTICS                                         //
    //******************************************************************************************//     

    //this allows lists of varying sizes to copied into one another, is this too loose?
    template<size_t size, typename D, bool inl>
    requires ( (dim >= size || dim * size == DYNAMIC) && std::is_convertible_v<D, T>)
    list& operator=(const list<D, size, inl>& rhs)
    {
        assert(this->size() >= rhs.size());
        std::copy(rhs.begin(), rhs.end(), this->begin());
        return *this;
    }
    
    list& operator=(const list& rhs)
    {
        if(this == &rhs)
            return *this;
        assert(rhs.size() == this->size());
        std::copy(rhs.begin(), rhs.end(), this->begin());
        return *this;
    }
    list& operator=(const std::initializer_list<T>& data) requires(inlined)
    {
        assert(data.size() == this->size());
        std::copy(data.begin(), data.end(), this->begin());
        return *this;
    }
    
    template<size_t size, typename D, bool inl>
    requires ( (dim >= size || size == DYNAMIC) && std::is_convertible_v<D, T>)
    explicit list(const list<D, size, inl>& copy) requires(dim != DYNAMIC) : list() {*this = copy;}
    
    template<size_t size, typename D, bool inl>
    requires ( (dim >= size || size == DYNAMIC) && std::is_convertible_v<D, T>)
    explicit list(const list<D, size, inl>& copy, size_t dynamicSize) requires(dim == DYNAMIC) : list(dynamicSize) {*this = copy;}
    
    list(const list& copy) requires(dim != DYNAMIC) : list() {*this = copy;}
    list(const list& copy) requires(dim == DYNAMIC) : list(copy.size()) {*this = copy;}

    //******************************************************************************************//
    //                                   MOVE SEMANTICS                                         //
    //******************************************************************************************//     

    template<size_t size>
    requires(dim == size || size == DYNAMIC)
    list(list<T, size, inlined>&& other) requires(!inlined && dim != DYNAMIC) : data{other.data}
    {
        assert(this->size() == other.size());
        other.ownsData = false;
    }
    template<size_t size>
    list(list<T, size, inlined>&& other) requires(!inlined && dim == DYNAMIC) : data{other.data}, dynamicSize{other.size()}
    {
        assert(this->size == other.size());
        other.ownsData = false;
    }

    list(list&& other)requires(!inlined && dim != DYNAMIC) : data{other.data}
    {
        other.ownsData = false;
    }
    list(list&& other)requires(!inlined && dim == DYNAMIC) : data{other.data}, dynamicSize{other.size()}
    {
        other.ownsData = false;
    }
    
    template<size_t size, typename D, bool inl>
    requires ( (dim >= size || dim * size == DYNAMIC) && std::is_convertible_v<D, T>)
    list& operator=(list<D, size, inl>&& rhs)
    {
        assert(this->size() >= rhs.size());
        const auto SIZE = this->size();
        for(size_t i = 0; i < SIZE; ++i)
            (*this)[i] = std::move(rhs[i]);
        return *this;
    }
    
    list& operator=(list&& rhs) requires(!inlined && dim == DYNAMIC)
    {
        assert(this->size() == rhs.size());
        free_data();
        this->ownsData = true;
        this->data = rhs.data;
        rhs.ownsData = false;
        return *this;
    }

    //******************************************************************************************//
    //                                   CONSTRUCTORS                                           //
    //******************************************************************************************//     

    explicit list() requires (dim != DYNAMIC && !inlined) : data{new T[dim]}{}
    explicit list(const size_t dynamicSize) requires(dim == DYNAMIC && !inlined) : dynamicSize{dynamicSize}, data{new T[dynamicSize]}{}
    
    explicit list() requires(inlined && dim != DYNAMIC) = default;
    
    explicit list(T* const data)  requires(dim != DYNAMIC && !inlined) : ownsData{false}, data{data} {}
    explicit list(T* const data, const size_t dynamicSize) requires(dim == DYNAMIC && !inlined) : 
    ownsData{false}, data{data}, dynamicSize(dynamicSize){}
    
    list(const std::initializer_list<T>& list) requires(dim == DYNAMIC) : list::list(list.size())
    {
        std::copy(list.begin(), list.end(), this->begin());
    }
    list(const std::initializer_list<T>& list) requires(dim != DYNAMIC) : list::list()
    {
        assert(list.size() <= dim);
        std::copy(list.begin(), list.end(), this->begin());
    }
    
    explicit list(const T& fillValue) requires(dim != DYNAMIC) : list()
    {
        std::fill(this->begin(), this->end(), fillValue);
    }
    explicit list(const T& fillValue, const size_t dynamicSize) requires(dim == DYNAMIC) : list(dynamicSize)
    {
        std::fill(this->begin(), this->end(), fillValue);
    }

    template <typename... types>
    requires((std::is_convertible_v<types, T> &&...) && dim != DYNAMIC)
    list(types... args) requires(sizeof...(args) == dim) : list()
    {
        size_t i = 0;
        ([&i, this, &args]{this->data[i++] = args;}(), ...);
    }
    template <typename... types>
    requires((std::is_convertible_v<types, T> &&...))
    list(types... args) requires(dim == DYNAMIC) : list(sizeof...(args))
    {
        size_t i = 0;
        ([&i, this, &args]{this->data[i++] = args;}(), ...);
    }

    explicit list(const builder& fnc, size_t dynamicSize) requires(dim == DYNAMIC) : list(dynamicSize)
    {
        create(fnc, *this);
    }
    explicit list(const builder& fnc) requires(dim != DYNAMIC):list() 
    {
        create(fnc, *this);
    }

    //                                      ****

    static void create(const builder& fnc, list& out)
    {
        //TODO the return type of fnc might be expensive ?
        //Also is it an error to not call the destructor of T?
        const auto SIZE = out.size();
        for(size_t i = 0; i < SIZE; ++i)
            new (out.begin() + i) T (fnc(i));
    }
    
    const list& map(mapping fnc) const
    {
        list result(size_t(this->dynamicSize));
        create([&fnc, this](size_t idx) -> T
        {
            return fnc(this->data[idx], idx);
        }, result);
        return result;
    }
    list& mutate(mutation fnc)
    {
        for(size_t i = 0; i < size(); ++i)
            fnc(this->data[i], i);
        return *this;
    }
    const list& for_each(process fnc)const
    {
        for(size_t i = 0; i < size(); ++i)
            fnc(this->data[i], i);
        return *this;
    }
    template <typename D>
    [[nodiscard]] D reduce(reduction<D> fnc, D initial = D(0)) const
    {
        D result = initial;
        for(size_t i = 0; i < size(); ++i)
            result = fnc(this->data[i], i, result);
        return result;
    }

    template<size_t size = DYNAMIC>
    [[nodiscard]] list<T, size * dim == DYNAMIC ? DYNAMIC : size + dim> join(const list<T, size>& other) const
    {
        list<T, size * dim == DYNAMIC ? DYNAMIC : size + dim> jointList(this->size() + other.size());
        std::copy(this->data, this->data + this->size(), jointList.data);
        std::copy(other.data, other.data + other.size(), jointList.data + this->size());
        return jointList;
    }

    template<typename D, size_t...sizes, typename...types>
    requires(std::is_convertible_v<types, D> && ...)
    friend list<D, calc_comptime_size(sizes...)> join(const list<types, sizes>&...lists);

    template<typename D, typename...types>
    requires(std::is_convertible_v<types, D> && ...)
    friend list<D, DYNAMIC> join(const std::initializer_list<types>&...lists);
    
    //TODO add static and dynamic dimension version of this
    //Also, this does a copy, not a move. Add require clase for D convertible to T
    template<typename D>
    operator list<D, dim>const()
    {
        list<D, dim> result(this->dynamicSize);
        list<D, dim>::create([this](size_t idx) -> D
        {
            return static_cast<D>(this->data[idx]);
        }, result);
        return result;
    }

    template<size_t size = DYNAMIC>
    void insert(const list<T, size>& data, size_t fromIdx)
    {
        auto pushedData = (*this)(fromIdx, this->size());

        this->resize(data.size() + this->size());

        this->overwrite_with(data, fromIdx);

        this->overwrite_with(pushedData, fromIdx + data.size());
    }
    template<size_t size = DYNAMIC>
    void push(const list<T, size>& data)
    {
        insert(data, this->size() - 1);
    }
    //If data.size() + fromIdx > size() then data will be truncated to fit into size() - fromIdx
    template<size_t size = DYNAMIC>
    void overwrite_with(const list<T, size>& data, size_t fromIdx)
    {
        size_t copySize = fromIdx + data.size() > this->size() ? this->size() - fromIdx : data.size();
        list<T, DYNAMIC> window(&this->data[fromIdx], copySize);    //pointer 
        window = data;
    }
    
    //TODO we can handle cases where newSize <= this->size() more efficiently by simply decrementing the data pointer
    void resize(size_t newSize) requires(dim == DYNAMIC)
    {
        assert(this->ownsData);

        T* newData = new T[newSize];
        
        size_t minSize = std::min(this->size(), newSize);
        std::copy(this->data, this->data + minSize, newData);
        
        delete[] this->data;
        this->data = newData;

        this->dynamicSize = newSize;
    }

    //returns deep copy of list, regardless of data ownership.
    list copy() const
    {
        const auto SIZE = this->size();
        list result(this->size());
        for(size_t i = 0; i < SIZE; ++i)
            result[i] = this[i];
        return result;
    }
    
    /**
     * @brief 
     * Prints list as {x1 x2 x3 ... xn} where xi is the ith element and n is the size of the list.
     * @param stream 
     * Output stream to print to.
     */
    inline void print(std::ostream& stream) const
    {
        stream << "{";
        const auto SIZE = this->size();
        this->for_each([&stream, SIZE](T value, size_t idx)
        {
            stream << value;
            if(idx != (SIZE - 1))
                stream << ' ';
        });
        stream << "}";
    }

    /// @warning For memory efficency, this destructor is not virtual. 
    /// As such, you should never use a list* to delete a derived type.
    /// I.e. the following code snippet is illegal, where 'derived' is a type extending this class
    ///
    /// \code{.cpp}
    /// list* base = new derived;
    /// 
    /// delete base;
    /// \endcode
    ~list()
    {
        free_data();
    }
private:
    void free_data()requires(inlined){}
    void free_data()requires(!inlined){if(ownsData)delete[]data;}
};

template <size_t dim, typename T, bool inlined>
std::ostream& operator<<(std::ostream& stream, const list<T, dim, inlined>& m){m.print(stream); return stream;}

template <typename firstT, typename...types>
requires (std::is_convertible_v<types, firstT> && ...)
list(firstT first, types...args) -> list<firstT, sizeof...(args) + 1>;

template <typename...args>
requires(std::is_same_v<args, size_t> && ...)
constexpr size_t calc_comptime_size(args...sizes) 
{
    size_t sum = 0;
    for(const size_t& size : std::initializer_list<size_t>{sizes...})
        if(size == DYNAMIC)
            return DYNAMIC;
        else
            sum += size;
    return sum;
}

template<typename D, size_t...sizes, typename...types>
requires(std::is_convertible_v<types, D> && ...)
[[nodiscard]] list<D, calc_comptime_size(sizes...)> join(const list<types, sizes>&...lists)
{
    size_t totalSize = 0;
    ([&]{totalSize += lists.size();}(), ...);
    list<D, calc_comptime_size(sizes...)> result(totalSize);
    
    size_t sizeSoFar = 0;
    ([&]
    {
        std::copy(lists.data, lists.data + lists.size(), result.data + sizeSoFar);
        sizeSoFar += lists.size();
    }(), ...);

    return result;
}

template<typename D, typename...types>
requires(std::is_convertible_v<types, D> && ...)
[[nodiscard]] list<D, DYNAMIC> join(const std::initializer_list<types>&...lists)
{
    size_t totalSize = 0;
    ([&]{totalSize += lists.size();}(), ...);
    list<D> result(totalSize);
    
    size_t sizeSoFar = 0;
    ([&]
    {
        std::copy(lists.begin(), lists.end() + lists.size(), result.data + sizeSoFar);
        sizeSoFar += lists.size();
    }(), ...);

    return result;
}

template <typename...types>
requires(std::is_same_v<types, size_t> && ...)
constexpr bool all_dynamic(types...args)
{
    return ((args == 0) &&...);
}
template <typename...types>
requires(std::is_same_v<types, size_t> && ...)
constexpr bool any_dynamic(types...args)
{
    return ((args == 0) ||...);
}

template <typename T, size_t dim>
using inline_list = list<T, dim, true>;

template <typename T, size_t dim = DYNAMIC>
using heap_list = list<T, dim, false>;

template <typename T, size_t stride = DYNAMIC, size_t nrStrides = DYNAMIC>
class list_view : public list<list<T, stride>, nrStrides, nrStrides == DYNAMIC ? false : true>
{
public:
    list_view(T* beginAddr) requires(stride != DYNAMIC && nrStrides != DYNAMIC) :
    list<list<T, stride>, nrStrides>([=](size_t idx)
    {
        return list<T, stride>(beginAddr + idx * stride);
    }){}
    list_view(T* beginAddr, size_t dynamicNrStrides) requires(stride != DYNAMIC && nrStrides == DYNAMIC) :
    list<list<T, stride>, DYNAMIC>([=](size_t idx)
    {
        return list<T, stride>(beginAddr + idx * stride);
    }, dynamicNrStrides){}
    list_view(T* beginAddr, size_t dynamicStride) requires(stride == DYNAMIC && nrStrides != DYNAMIC) :
    list<list<T, DYNAMIC>, nrStrides>([=](size_t idx)
    {
        return list<T, DYNAMIC>(beginAddr + idx * stride, dynamicStride);
    }, nrStrides){}
    list_view(T* beginAddr, size_t dynamicStride, size_t dynamicNrStrides) requires(stride == DYNAMIC && nrStrides == DYNAMIC) :
    list<list<T, DYNAMIC>, DYNAMIC>([=](size_t idx)
    {
        return list<T, DYNAMIC>(beginAddr + idx * dynamicStride, dynamicStride);
    }, dynamicNrStrides){}
};
template <typename T>
list_view(T*) -> list_view<T, DYNAMIC, DYNAMIC>;
