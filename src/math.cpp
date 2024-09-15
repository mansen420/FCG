#include <algorithm>
#include <cstddef>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <functional>
#include <iostream>
//IO
#include <iterator>
#include <ostream>

#include <chrono>

#include "SDL.h"

namespace math
{
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

    template <size_t, typename, bool>
    class vector;

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
        const T& operator()(size_t idx)const
        {
            assert(idx < size());
            return this->data[idx];
        }
        
        /// @param fromIdx 
        /// Index of first element of the sublist, satisfying fromIdx <= toIdx
        /// @param toIdx 
        /// index of one past the final element of the sublist, satisfying toIdx <= size() + fromIdx.
        /// @return 
        /// A non-owning list pointing at the element at fromIdx, with a dynamic size of toIdx - fromIdx.
        list<T, DYNAMIC> operator()(size_t fromIdx, size_t toIdx) requires(!inlined)
        {
            assert (fromIdx + toIdx <= this->size() && fromIdx <= toIdx);
            return list<T, DYNAMIC>(this->data + fromIdx, toIdx - fromIdx);
        }

        /// @param fromIdx 
        /// Index of first element of the sublist, satisfying fromIdx <= toIdx
        /// @param toIdx 
        /// index of one past the final element of the sublist, satisfying toIdx <= size() + fromIdx.
        /// @return 
        /// A const non-owning list pointing at the element at fromIdx, with a dynamic size of toIdx - fromIdx.
        const list<T, DYNAMIC> operator()(size_t fromIdx, size_t toIdx) const
        {
            return this->operator()(fromIdx, toIdx);
        }
        //TODO make the compile time version of the above operator()

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
      
        //                                      **copy semantics**

        //this allows lists of varying sizes to copied into one another, is this too loose?
        template<size_t size, typename D, bool inl>
        requires (dim >= size || dim * size == DYNAMIC && std::is_convertible_v<D, T>)
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
        requires (dim >= size || size == DYNAMIC && std::is_convertible_v<D, T>)
        explicit list(const list<D, size, inl>& copy) requires(dim != DYNAMIC) : list() {*this = copy;}
        
        template<size_t size, typename D, bool inl>
        requires (dim >= size || size == DYNAMIC && std::is_convertible_v<D, T>)
        explicit list(const list<D, size, inl>& copy, size_t dynamicSize) requires(dim == DYNAMIC) : list(dynamicSize) {*this = copy;}
        
        list(const list& copy) requires(dim != DYNAMIC) : list() {*this = copy;}
        list(const list& copy) requires(dim == DYNAMIC) : list(copy.size()) {*this = copy;}

        //                                      **move semantics**

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
        requires (dim >= size || dim * size == DYNAMIC && std::is_convertible_v<D, T>)
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
            const auto SIZE = this->size();
            for(size_t i = 0; i < SIZE; ++i)
                (*this)[i] = std::move(rhs[i]);
            return *this;
        }
        
        //                                      **constructors**

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
        explicit list(const builder& fnc) requires(dim != DYNAMIC) 
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
        
        inline void print(std::ostream& stream) const
        {
            stream << "{";
            this->for_each([&stream, this](T value, size_t idx)
            {
                stream << value;
                if(idx != (this->size() - 1))
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
            if(!inlined && ownsData)
                delete[] data;
        }
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

    template <typename T>
    class list_view : public list<list<T, DYNAMIC>, DYNAMIC>
    {
        const size_t stride, offset;
public:
        list_view(T* beginAddr, size_t stride, size_t offset, size_t nrStrides) : 
        list<list<T, DYNAMIC>, DYNAMIC>([=](size_t idx)
        {
            return list<T, DYNAMIC>(beginAddr + idx * (stride + offset), stride);
        }, nrStrides),
        stride{stride},
        offset{offset} 
        {}
    };
    template <typename T>
    list_view(T*) -> list_view<T>;

    template <int n_rows, int n_cols, typename T>
    requires (n_rows >= 0 && n_cols >= 0)
    class matrix;

    //Pure vector class
    template <size_t dim = DYNAMIC, typename T = float, bool inlined = false>
    class vector : public list<T, dim, inlined>
    {
protected:
        template <int n_rows, int n_cols, typename D>
        requires (n_rows >= 0 && n_cols >= 0)
        friend class matrix;
public:
        using list<T, dim, inlined>::list;

        template<size_t size>
        requires (dim >= size || dim * size == DYNAMIC)
        vector& operator=(const vector<size, T>& other)
        {
            list<T, dim>::operator=(other);
            return *this;
        }
        vector& operator=(const vector& other)
        {
            if(this == &other)
                return *this;
            list<T, dim>::operator=(other);
            return *this;
        };

        template<size_t size, typename D, bool inl>
        requires ((dim >= size || size == DYNAMIC) && std::is_convertible_v<D, T>)
        vector(const vector<size, D, inl>& other) requires(dim != DYNAMIC) : list<T, dim, inlined>(other){}
        
        template<size_t size, typename D, bool inl>
        requires (std::is_convertible_v<D, T>)
        vector(const vector<size, D, inl>& other) requires(dim == DYNAMIC) :
        list<T, dim, inlined>(other, other.size()){}

        vector(const vector& other) : list<T, dim, inlined>(other){}
        
        vector(const list<T, dim>& other) : list<T, dim, inlined>(other){}

        template<size_t size, typename D, bool inl>
        requires ((dim >= size || size == DYNAMIC) && std::is_convertible_v<D, T>)
        vector(const list<D, size>& other)requires(dim != DYNAMIC) : list<T, dim>(other){}

        template<size_t size, typename D, bool inl>
        requires (std::is_convertible_v<D, T>)
        vector(const list<D, size>& other, size_t dynamicSize)requires(dim == DYNAMIC) : list<T, dim>(other, dynamicSize){}

        const T& x = this->data[0];
        const T& y = this->data[1];
        const T& z = this->data[2];
        const T& w = this->data[3];

        const T& r = x;
        const T& g = y;
        const T& b = z;
        const T& a = w;

        static inline vector zero = vector(T(0));

        [[nodiscard]] inline matrix<dim, 1, T> col()const
        {
            return matrix<dim, 1, T>(*this, this->size());
        }
        [[nodiscard]] inline matrix<1, dim, T> row()const
        {
            return matrix<1, dim, T>(*this, this->size());
        }

        inline vector operator+ (const vector& rhs) const
        {
            const vector& lhs = *this;
            return create([lhs, &rhs](size_t idx)
            {
                return lhs[idx] + rhs[idx];
            });
        }
        inline T operator*(const vector& rhs) const
        {
            return reduce<T>([&rhs](T val, size_t idx, T prev)
            {
                return prev + val*rhs[idx];
            });
        }
        inline vector operator-()const
        {
            return map([](T value, size_t idx)
            {
                return -value;
            });
        }
        inline vector operator-(const vector& rhs)const
        {
            const vector& lhs = *this;
            return lhs + (-rhs);
        }
        inline vector operator* (float coeff)const
        {
            return this->map([&coeff](T val, size_t idx) -> T
            {
                return val * coeff;
            });
        }
        inline vector operator/(float coeff)const
        {
            return this->map([&coeff](T val, size_t idx){return val/coeff;});
        }
        [[nodiscard]] inline T magnitude() const
        {
            return sqrt((*this) * (*this));
        }
        inline bool operator== (const vector& rhs)
        {
            return reduce<bool>([&rhs](T value, size_t idx, bool prev)
            {
                if (prev == false)
                    return prev;
                return prev && (value == rhs[idx]);
            }, true);
        }
        [[nodiscard]] inline vector direction() const
        {
            if(*this == zero)
                return zero;
            return (*this)*(1.0/magnitude());
        }
        inline vector& normalize()
        {
            *this = this->direction();
            return *this;
        }
        inline vector& homogenize() requires(dim > 1)
        {
            if(this->last() == T(0))
                return *this;
            this->mutate([this](T& val, size_t){ val = val / this->last();});
            return *this;
        }
        [[nodiscard]] T sum()
        {
            return this->template reduce<T>([](T val, size_t idx, T prev)->T
            {
                return prev + val;
            });
        }
    };
    
    template <typename firstT, typename...types>
    requires (std::is_convertible_v<types, firstT> && ...)
    vector(firstT first, types...args) -> vector<sizeof...(args) + 1, firstT>;
    
    template <size_t dim, typename T>
    vector<dim, T> operator* (float coeff, const vector<dim, T>& u)
    {
        return u*coeff;
    }

    template <typename T, size_t dim> 
    inline vector<dim, T> operator*(T coeff, vector<dim, T> u)
    {
        return u*coeff;
    }
    inline vector<3> cross(const vector<3>& a, const vector<3>& b)
    {
        return vector<3>({a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x});
    }

    template <int n_rows, int n_cols = n_rows, typename T = float>
    requires (n_rows >= 0 && n_cols >= 0)
    class matrix
    {
        template <int r, int c, typename D>
        requires (r >= 0 && c >= 0)
        friend class matrix;

        [[no_unique_address]] std::conditional_t<n_rows == DYNAMIC, size_t, empty> rowSize;
        [[no_unique_address]] std::conditional_t<n_cols == DYNAMIC, size_t, empty> colSize; 
public:
        T& operator[](size_t idx) {return this->data[idx];}
        vector<n_rows*n_cols, T> data;

        static matrix get_identity(size_t dynamicSize = 0)
        {
            static_assert(n_rows == n_cols);
            matrix identity(dynamicSize);
            matrix::create([](int row, int col)
            {
                if (row == col)
                    return T(1);
                else
                    return T(0);
            }, identity);
            return identity;
        }
        
        inline size_t cols() const
        {
            if(n_cols == DYNAMIC)
                return colSize;
            return n_cols;
        }
        inline size_t rows() const
        {
            if(n_rows == DYNAMIC)
                return rowSize;
            return n_rows;
        }
        

        typedef std::function<T(T value, int row, int col)>      Fmapping;
        typedef std::function<T(int row, int col)>              Fbuilding;
        typedef std::function<void(T& value, int row, int col)> Fmutation;
        typedef std::function<void(T value, int row, int col)>    Faction;
        
        matrix(const matrix& other) : matrix(other.rows(), other.cols())
        {
            *this = other;
        }
        matrix& operator=(const matrix& other)
        {
            if(this == &other)
                return *this;
            this->data = other.data;
            return *this;
        }
        
        template<size_t nRows, size_t nCols>
        requires (nRows == DYNAMIC || nRows == n_rows && nCols == DYNAMIC || nCols == n_cols)
        matrix& operator=(const matrix<nRows, nCols, T>& other)
        {
            assert(other.rows() == this->rows() && other.cols() == this->cols());
            if(this == &other)
                return *this;
            this->data = other.data;
            return *this;
        }

        explicit matrix(size_t dynamicSizeRows = 0, size_t dynamicSizeCols = 0) : 
        data(static_cast<size_t>(dynamicSizeRows*dynamicSizeCols))
        {
            if(n_rows == DYNAMIC)
                rowSize = dynamicSizeRows;
            if(n_cols == DYNAMIC)
                colSize = dynamicSizeCols;
        }
        
        explicit matrix(const std::initializer_list<T>& list, size_t dynamicSizeRows = 0, size_t dynamicSizeCols = 0) : 
        data(list)
        {
            if(n_rows == DYNAMIC)
                rowSize = dynamicSizeRows;
            if(n_cols == DYNAMIC)
                colSize = dynamicSizeCols;
            assert(list.size() == this->cols() * this->rows());
        }
        
        explicit matrix(T fillValue, size_t dynamicSizeRows, size_t dynamicSizeCols) requires(n_cols + n_rows == DYNAMIC) : 
        data(fillValue, static_cast<size_t>(dynamicSizeRows*dynamicSizeCols))
        {
            rowSize = dynamicSizeRows;
            colSize = dynamicSizeCols;            
        }
        explicit matrix(T fillValue) requires(n_cols * n_rows != DYNAMIC) : 
        data(fillValue)
        {
        }
        
        explicit matrix(Fbuilding builder, size_t dynamicSizeRows = 0, size_t dynamicSizeCols = 0) : 
        data(static_cast<size_t>(dynamicSizeRows*dynamicSizeCols))
        {
            if(n_rows == DYNAMIC)
                rowSize = dynamicSizeRows;
            if(n_cols == DYNAMIC)
                colSize = dynamicSizeCols;
            create(builder, *this);
        }
        explicit matrix(const vector<n_rows*n_cols, T>& data, size_t dynamicSizeRows = 0, size_t dynamicSizeCols = 0) : data{data} 
        {
            if(n_rows == DYNAMIC)
                rowSize = dynamicSizeRows;
            if(n_cols == DYNAMIC)
                colSize = dynamicSizeCols;
            assert(data.size() == this->rows() * this->cols());
        }

        void set_data(const std::initializer_list<T>& list)
        {
            assert(list.size() == this->cols() * this->rows());
            this->data = vector<n_rows * n_cols, T>(list, this->rows()*this->cols());
        }
        void set_data(const std::initializer_list<std::initializer_list<T>>& list)
        {
            assert(list.size() == this->rows());
            const auto ROWS = rows();
            for(size_t i = 0; i < ROWS; ++i)
            {
                assert(list.begin()[i].size() == cols());
                this->row(i) = list.begin()[i];
            }
        }

        T operator()(size_t r, size_t c)const{return data(r*cols() + c);}

        [[nodiscard]]inline matrix map(Fmapping mapping) const
        {
            matrix<n_rows, n_cols, T> result(rows(), cols());
            for(size_t i = 0; i < rows(); ++i)
                for(size_t j = 0; j < cols(); ++j)
                    result.data[i*cols() + j] = mapping((*this)(i, j), i, j);
            //XXX returning by copy!
            return result;
        }
        inline static void create(Fbuilding build, matrix& result)
        {
            for(size_t i = 0; i < result.rows(); ++i)
                for(size_t j = 0; j < result.cols(); ++j)
                    result.data[i*result.cols() + j] = build(i, j);
        }
        inline matrix& mutate(const Fmutation& mutation)
        {
            for(size_t i = 0; i < rows(); ++i)
                for(size_t j = 0; j < cols(); ++j)
                    mutation(this->data[i * cols() + j], i, j);
            return *this;
        }
        inline const matrix& for_each(Faction action) const
        {
            for(size_t i = 0; i < rows(); ++i)
                for(size_t j = 0; j < cols(); ++j)
                    action((*this)(i, j), i, j);
            return *this;
        }

        const static inline matrix<n_rows, n_cols, T> zero = matrix(T(0));
        const static inline matrix<n_rows, n_cols, T> I = matrix::get_identity();

        //BEWARE! modifying this vector will mutate the matrix. Use vector::copy for a deep copy.
        inline vector<n_cols, T> row(size_t idx)
        {
            assert(idx < rows());
            const size_t startIdx = idx * cols();
            vector<n_cols, T> result(&this->data[startIdx], cols());
            return result;
        }
        inline vector<n_cols, T> row_cpy(size_t idx)const
        {
            assert(idx < rows());
            const size_t startIdx = idx * cols();
            const auto COLS = cols();
            vector<n_cols, T> result(cols());
            for(size_t i = 0; i < COLS; ++i)
                result[i] = this->data(i + startIdx);
            return result;
        }
        //A vector of column elements
        inline vector<n_rows, T> col(size_t idx)const
        {
            assert(idx < cols());
            vector<n_rows, T> result;
            for(size_t i = 0; i < n_rows; ++i)
                result.data[i] = this->data(idx + i*cols());
            return result;
        }
        
        void mutate_rows(const std::function<void(vector<n_cols, T>& row, size_t idx)>& mutation)
        {
            for(size_t i = 0; i < rows(); ++i)
            {
                vector<n_cols, T> row(this->row(i).data, cols());
                mutation(row, i);
            }
        }
        void for_each_row(const std::function<void(const vector<n_cols, T>& row, size_t idx)>& action)const
        {
            for(size_t i = 0; i < rows(); ++i)
                action(row(i).copy(), i);
        }
            
        template<size_t dim>
        requires (dim == n_cols || dim == DYNAMIC)
        void set_row(size_t idx, const vector<dim, T>& row)
        {
            assert(row.size() == cols());
            this->row(idx) = row;
        }
        
        [[nodiscard]]matrix map_rows(const std::function<vector<n_cols, T>(const vector<n_cols, T>& row, size_t idx)>& mapping) const
        {
            matrix result;
            for(size_t i = 0; i < rows(); ++i)
                result.set_row(i, mapping(this->row_copy(i), i));
            return result;
        }
        
        template <size_t nRows, size_t nCols>
        matrix& insert_rows(const matrix<nRows, nCols, T>& rows, size_t fromIdx) 
        requires (n_rows == DYNAMIC && (nCols == n_cols || n_cols == DYNAMIC))
        {
            assert(rows.cols() == this->cols());
            this->data.insert(rows.data, fromIdx * rows.cols());
            this->rowSize += rows.rows();

            return *this;
        }
        
        template <size_t nRows, size_t nCols>
        void push_rows(const matrix<nRows, nCols, T>& rows){insert_rows(rows, this->rows());} 

        //Returns reference to submatrix. Allows mutation! 
        //TODO add copy() method
        matrix<DYNAMIC, n_cols, T> subview(size_t fromIdx, size_t nRows)
        {
            vector<DYNAMIC, T> dataWindow(&this->data[fromIdx * this->cols()], nRows * this->cols());
            return matrix<DYNAMIC, n_cols, T>(dataWindow, nRows, this->cols());
        }

        T& element(size_t r, size_t c)
        {
            return data[r*cols() + c];
        } 
        const T& element(size_t r, size_t c)const
        {
            return data(r*cols() + c);
        }

        void print(std::ostream& stream) const
        {
            stream << "[";
            (*this).for_each([&](T value, int row, int col)
            {
                if (col == 0)
                    stream << "{";
                stream << value;
                if(col == cols() - 1)
                    if (row == rows() - 1)
                        stream << "}";
                    else
                        stream << "}, ";
                else 
                    stream << ' ';
            });
            stream << "]";
        }

        template<int rhs_rows, int rhs_cols>
        matrix<n_rows, n_cols, T> operator+ (const matrix<rhs_rows, rhs_cols, T> rhs) const
        requires ( (rhs_rows == n_rows || rhs_rows == DYNAMIC || n_rows == DYNAMIC) && (rhs_cols == n_cols || rhs_cols == DYNAMIC || n_cols == DYNAMIC) )
        {
            assert(rhs.rows() == this->rows());
            assert(rhs.cols() == this->cols());
            return this->map([&](T value, int row, int col){return value + rhs(row, col);});
        }
        matrix operator-() const
        {
            return this->map([](T val, int row, int col){return -val;});
        }
        matrix operator-(const matrix<n_rows, n_cols, T> rhs) const
        {
            return (*this) + -rhs;
        }

        template <int nr_rows, int nr_cols>
        requires (nr_rows == n_cols || nr_rows == DYNAMIC)
        matrix<n_rows, nr_cols> operator*(const matrix<nr_rows, nr_cols, T> rhs) const
        {
            const matrix& lhs = (*this);

            assert(rhs.rows() == lhs.cols());

            const size_t mul_size = rhs.rows();

            matrix<n_rows, nr_cols> result (this->rows(), rhs.cols());

            matrix<n_rows, nr_cols>::create([&](int row, int col)
            {
                T sum{0};
                for (size_t i = 0; i < mul_size; ++i)
                    sum += lhs(row, i) * rhs(i, col);
                return sum;
            }, result);

            return result;
        }
        
        template<size_t dim>
        requires (dim == n_cols || dim == DYNAMIC || n_cols == DYNAMIC)
        matrix<n_rows, 1, T> operator*(const vector<dim, T>& rhs) const
        {
            assert(rhs.size() == this->cols());
            return *this * rhs.col();
        }
        
        matrix operator*(const float coeff) const
        {
            return map([&](T value, int row, int col){return coeff*value;});
        }

        [[nodiscard]]inline matrix<n_cols, n_rows, T> transpose()const
        {
            matrix<n_cols, n_rows> result(cols(), rows());
            matrix<n_cols, n_rows, T>::create([&](int row, int col)
            {
                return (*this)(col, row);
            }, result);
            return result;
        }
        [[nodiscard]]inline matrix reciprocal()
        {
            return map([](T value, int row, int col)
            {
                return value == T(0) ? value : 1.0/value; 
            });
        }

        //Returns deep copy of matrix. Especially useful when you want to copy a subview()
        matrix copy()
        {
            return this->map_rows([](const vector<n_cols, T>& row, size_t idx)
            {
                return row.copy();
            });
        } 
        
        //row or column vectors convert to pure vectors
        //be careful of returning non-owned data 
        template <size_t dim, typename D>
        requires(dim == n_cols || dim == n_rows || dim * n_rows * n_cols == DYNAMIC)
        operator vector<dim, D>() requires (n_cols == 1 || n_cols == DYNAMIC || n_rows == 1 || n_rows == DYNAMIC)
        {
            assert(cols() == 1 || rows() == 1);
            return data;
        }

        void resize_rows(size_t nRows) requires(n_rows == DYNAMIC)
        {
            this->data.resize(nRows * cols());
            this->rowSize = nRows;
        }
        void add_row_capacity(size_t nRows) requires(n_rows == DYNAMIC)
        {
            resize_rows(this->rows() + nRows);
        }

        void resize_cols(size_t nCols) requires(n_cols == DYNAMIC)
        {
            this->data.resize(nCols * rows());
            this->colSize = nCols;
        }
        void add_col_capacity(size_t nCols) requires(n_cols == DYNAMIC)
        {
            resize_cols(nCols);
        }

        void resize(size_t nRows, size_t nCols) requires(n_rows == DYNAMIC && n_cols == DYNAMIC)
        {
            this->data.resize(nRows * nCols);
            this->colSize = nCols;
            this->rowSize = nRows;
        }

        template <int nRows, int nCols>
        requires (nCols <= n_cols || nCols * n_cols == DYNAMIC)
        void overwrite_rows_with(const matrix<nRows, nCols, T>& data, size_t startIdx)
        {
            assert(startIdx < this->rows());
            assert(startIdx + data.rows() <= this->rows());
            assert(data.cols() <= this->cols());
            
            const auto MINSIZE = std::min(data.rows(), this->rows() - startIdx);
            for (size_t i = 0; i < MINSIZE; ++i)
            {
                this->row(i + startIdx) = data.row_cpy(i);
            }
        }
    };
    template <int n_rows, int n_cols = n_rows, typename T>
    inline std::ostream& operator<<(std::ostream& stream, const matrix<n_rows, n_cols, T>& m){m.print(stream); return stream;}

    template <int n_rows, int n_cols = n_rows, typename T = float>
    inline matrix<n_rows, n_cols, T> operator*(const float coeff, const matrix<n_rows, n_cols, T> m)
    {
        return m*coeff;
    }


    template <int nRows, int nCols, typename T, size_t dim>
    requires (dim == nRows || dim == DYNAMIC || nRows == DYNAMIC)
    inline matrix<1, nCols, T> operator*(const vector<dim, T>& lhs, const matrix<nRows, nCols, T>& rhs)
    {
        assert(rhs.rows() == lhs.size());
        return lhs.row() * rhs;
    }

    //make this a build script?
    template <int dim, typename T = float>
    [[nodiscard]]inline matrix<dim, dim, T> scale(vector<dim, T> diagonals, size_t dynamicSize = 0)
    {
        return matrix<dim, dim, T>([&](int row, int col)
        {
            if (row == col)
                return diagonals[row];
            else
                return T(0);
        }, dynamicSize);
    }

    [[nodiscard]]inline matrix<2> rotation2D(float angle)
    {
        return matrix<2>(
            { cos(angle), -sin(angle),
              sin(angle),  cos(angle)
            });
    }

    [[nodiscard]]inline matrix<2> shear2D(float factor1, float factor2)
    {
        return matrix<2>(
            {
                1,     factor1,
                factor2,     1
            });
    }
    
    template<int dim>
    [[nodiscard]]inline matrix<dim> homogeneous(matrix<dim - 1> innerMatrix, vector<dim> bottomRow, vector<dim> rightCol, size_t dynamicSize = 0)
    {
        assert(bottomRow(dim - 1) == rightCol(dim - 1));

        matrix<dim> result(dynamicSize);
        matrix<dim>::create([&](int row, int col) -> float
        {
            if (row == dim - 1)
                return bottomRow(col);
            if (col == dim - 1)
                return rightCol(row);
            
            return innerMatrix(row, col);
        }, result);
        return result;
    }

    // convenience typedefs

    template <int dim, typename T = float>
    using col_vector = matrix<dim, 1, T>;
    
    template <int dim, typename T = float>
    using row_vector = matrix<1, dim, T>;

    template<int nRows, int nCols, typename T>
    using transformation = matrix<nRows, nCols, T>;

    template <typename T, size_t dim>
    using inline_list = list<T, dim, true>;

    template <typename T>
    using list3 = list<T, 3, true>;

    typedef inline_list<float, 3> list3f;
    typedef inline_list<float, 3> list3i;
    typedef inline_list<float, 3> list3d;
    typedef inline_list<float, 3> list3u;

    typedef vector<4, float>        vec4f;
    typedef vector<4, double>       vec4d;
    typedef vector<4, int>          vec4i;
    typedef vector<4, unsigned int> vec4u;

    typedef vector<3, float>        vec3f;
    typedef vector<3, double>       vec3d;
    typedef vector<3, int>          vec3i;
    typedef vector<3, unsigned int> vec3u;

    typedef vector<2, float>        vec2f;
    typedef vector<2, double>       vec2d;
    typedef vector<2, int>          vec2i;
    typedef vector<2, unsigned int> vec2u;

    typedef vector<1, float>        vec1f;
    typedef vector<1, double>       vec1d;
    typedef vector<1, int>          vec1i;
    typedef vector<1, unsigned int> vec1u;

    typedef matrix<4> mat4f;
    typedef matrix<3> mat3f;
    typedef matrix<2> mat2f;

    typedef matrix<4, 4, int> mat4i;
    typedef matrix<3, 3, int> mat3i;
    typedef matrix<2, 2, int> mat2i;

    template <size_t dim, typename T>
    using vec = vector<dim, T>;
};

namespace output
{
    typedef math::vector<3, u_int8_t> RGB24;
    struct RGBA32
    {
        uint8_t R;
        uint8_t G;
        uint8_t B;
        uint8_t A;
    };

    class window
    {
public:
        SDL_Window* handle;
        SDL_Surface* srf;
        SDL_Renderer* renderer;
        SDL_Texture* frame;
        window(const char* title, uint x, uint y, uint width, uint height)
        {
            auto res =SDL_CreateWindowAndRenderer(width, height, 0, &handle, &renderer);
            if(res != 0)
            {
                std::cout << SDL_GetError();
                throw std::runtime_error("WINDOW ERROR");
            }
            srf = SDL_GetWindowSurface(handle);
            frame = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_STREAMING, width, height);
        }

        template<int nLines, int lineSpan>
        void write_frame(math::matrix<nLines, lineSpan, RGBA32>& framedata)
        {
            void* texPtr;
            int texPitch;
            SDL_LockTexture(frame, nullptr, &texPtr, &texPitch);
            memcpy(texPtr, framedata.data.begin(), texPitch * framedata.rows());
            SDL_UnlockTexture(frame);

            SDL_RenderClear(renderer);
            SDL_Rect destination{0, 0, framedata.cols(), framedata.rows()};
            SDL_RenderCopy(renderer, frame, nullptr, nullptr);
            SDL_RenderPresent(renderer);
        }
    };
};

//TODO this class is temporary as fuck
namespace renderer
{

    class rasterizer
    {
public:
        math::matrix<math::DYNAMIC, math::DYNAMIC, output::RGBA32> raster;
        math::matrix<3> WtoSCR;

        rasterizer(size_t SCRwidth, size_t SCRheight, math::vector<2> worldXCoords, math::vector<2> worldYCoords) : 
        raster(output::RGBA32{255, 255, 255, 255}, SCRwidth, SCRheight)
        {
            const auto width = worldXCoords[1] - worldXCoords[0];
            const auto height = worldYCoords[1] - worldYCoords[0];
            const auto midX = worldXCoords.sum()/2;
            const auto midY = worldYCoords.sum()/2;

            WtoSCR = math::homogeneous<3>(math::matrix<2>::I, {0, 0, 1}, {-midX, -midY, 1});
            WtoSCR = WtoSCR * math::homogeneous<3>(math::scale<2>({float(SCRwidth)/(width),
            float(SCRheight)/(height)}), {0, 0, 1}, {0, 0, 1});
        }
        void rasterize(const math::vector<2, float>& worldLoc, const output::RGBA32& color)
        {
            //TODO this conversion is causing overflow.
            //TODO implement submatrix()
            math::vector<3, uint> pixelLoc = WtoSCR * math::vector<3, float>(math::join<float>(worldLoc, math::list({1.f})));
            
            if(pixelLoc.x > raster.cols() || pixelLoc.y > raster.rows())
                return;
            raster.element(pixelLoc.y, pixelLoc.x) = color;
        }

        math::matrix<math::DYNAMIC, math::DYNAMIC, output::RGBA32> get_framebuffer(const math::vector<2, uint>& renderTargetSize)
        {
            if(renderTargetSize.x == raster.cols() && renderTargetSize.y == raster.rows())
                return raster;

            math::matrix<math::DYNAMIC, math::DYNAMIC, output::RGBA32> renderTarget(renderTargetSize.y, renderTargetSize.x);
            //very slow, why? calling the function is the slowest part!
            //UPDATE : don't use std::function. ever. inline lambdas with template params.
            //https://stackoverflow.com/questions/67615330/why-stdfunction-is-too-slow-is-cpu-cant-utilize-instruction-reordering
            auto const ROWS = renderTarget.rows();
            auto const COLS = renderTarget.cols();
            for(size_t i = 0; i < ROWS ; ++i)
                for(size_t j = 0; j < COLS; ++j)
                {
                    constexpr uint halfPixel = 0.5;
                    float vCenter = float(i + halfPixel)/renderTargetSize.y; //[0, 1)
                    float hCenter = float(j + halfPixel)/renderTargetSize.x;

                    renderTarget[i * renderTarget.cols() + j] = this->raster(vCenter * raster.rows(), hCenter * raster.cols());
                }
            return renderTarget;
        }
    };
};

int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
{
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
        return -1;

    constexpr uint wFramebuffer = 20;
    constexpr uint hFramebuffer = 20;
    constexpr uint wWindow = 1000;
    constexpr uint hWindow = 1000;

    using namespace math;
    using namespace output;

    window window("title", 200, 200, wWindow, hWindow);

    const vector<2> Wx(0.f, 1.f);
    const vector<2> Wy(0.f, 1.f);
    
    struct ms_timer
    {
        std::chrono::high_resolution_clock::time_point start = std::chrono::high_resolution_clock::now();
        [[nodiscard]] std::chrono::milliseconds clock()
        {
            return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - start);
        }
    };

    renderer::rasterizer R(wFramebuffer, hFramebuffer, Wx, Wy);
    
    list<list<unsigned char, 4, true>> s(500*500);   
    RGBA32 k[100];

    bool quit = false;
    ms_timer frameTimer;
    while(!quit)
    {
        SDL_Event e;
        while(SDL_PollEvent(&e))
            if(e.type == SDL_QUIT)
                quit = true;

        R.rasterize(vec2f(0.f + 0.001f*float(frameTimer.clock().count()), 0.5f), RGBA32({255, 0, 0 , 255}));
        auto test = R.get_framebuffer(vec2u(wWindow, hWindow));
auto frameDelta = frameTimer.clock();
        window.write_frame(test);
    }
    return 0;
}
