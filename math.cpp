#include <cstddef>
#include <cassert>
#include <cmath>
#include <exception>
#include <functional>
#include <iostream>
#include <variant>
//IO
#include <fstream>
#include <streambuf>
#include <ostream>
#include <sstream>
#include <exception>

#include "SDL.h"

namespace math
{
    template <typename T>
    class basic_aggregate
    {
protected:
        bool ownsData = true;
        T* data;
public:
        operator T*(){return data;} //is this a good idea?
        basic_aggregate() = default;
        explicit basic_aggregate(T* const data) : ownsData{false}, data{data}{}
        virtual ~basic_aggregate()
        {
            if(ownsData)
                delete[] data;
        }
    };

    constexpr uint DYNAMIC = 0;
    
    template <typename T, size_t dim = DYNAMIC>
    requires (dim >= 0)
    class list : public basic_aggregate<T>
    {
        typedef std::function<T(size_t idx)> builder;
        typedef std::function<T(T value, size_t idx)> mapping;
        typedef std::function<void(T& value, size_t idx)> mutation;
        typedef std::function<void(T val, size_t idx)> process;
        template <typename D>
        using reduction = std::function<D(T val, size_t idx, D prev)>;

        [[no_unique_address]] std::conditional_t<dim == 0, size_t, decltype(0)> dynamicSize;
public:
        inline size_t size() const
        {
            if(dim == DYNAMIC)
                return dynamicSize;
            return dim;
        }

        T operator[](size_t idx)const{return this->data[idx];}
        list& operator=(const list& rhs)
        {
            if(this == &rhs)
                return *this;
            for(size_t i = 0; i < dim; ++i)
                this->data[i] = rhs.data[i];
            return *this;
        }
        list(const list& copy) : list() {*this = copy;}

        list(const size_t dynamicSize = 0)
        {
            if(dim == DYNAMIC)
            {
                this->dynamicSize = dynamicSize;
                this->data = new T[dynamicSize];
                return;
            }
            this->data = new T[dim];
        }
        explicit list(T* const data, const size_t dynamicSize = 0) : basic_aggregate<T>(data)
        {
            if(dim == DYNAMIC)
            {
                this->dynamicSize = dynamicSize;
                this->data = new T[dynamicSize];
                return;
            }
            this->data = new T[dim];
        }
        explicit list(const std::initializer_list<T>& list, const size_t dynamicSize = 0) : list::list(dynamicSize)
        {
            std::copy(list.begin(), list.end(), this->data);
        }
        explicit list(const T fillValue, const size_t dynamicSize = 0) : list(dynamicSize)
        {
            for(size_t i = 0; i < size(); ++i)
                this->data[i] = fillValue;
        }


        static void create(builder fnc, list& out)
        {
            for(size_t i = 0; i < out.size(); ++i)
                out.data[i] = fnc(i);
        }
        list map(mapping fnc) const
        {
            list result;
            create([&fnc, this](size_t idx) -> T
            {
                return fnc(this->data[idx], idx);
            }, result);
            return result;
        }
        list mutate(mutation fnc)
        {
            for(size_t i = 0; i < size(); ++i)
                fnc(this->data[i], i);
            return *this;
        }
        list for_each(process fnc)const
        {
            for(size_t i = 0; i < size(); ++i)
                fnc(this->data[i], i);
            return *this;
        }
        template <typename D>
        D reduce(reduction<D> fnc, D initial = D(0)) const
        {
            D result = initial;
            for(size_t i = 0; i < size(); ++i)
                result = fnc(this->data[i], i, result);
            return result;
        }

        virtual ~list() = default;
    };

    template <int n_rows, int n_cols, typename T>
    requires (n_rows >= 0 && n_cols >= 0)
    class matrix;

    //Pure vector class
    template <size_t dim = DYNAMIC, typename T = float>
    requires (dim >= 0)
    class vector : public list<T, dim>
    {
        template <int n_rows, int n_cols, typename D>
        requires (n_rows >= 0 && n_cols >= 0)
        friend class matrix;
public:
        vector(const size_t dynamicSize = 0) : list<T, dim>(dynamicSize) {}
        vector(const std::initializer_list<T>& list, const size_t dynamicSize = 0) : list<T, dim>(list, dynamicSize){}
        vector(const T fillValue, const size_t dynamicSize = 0) : list<T, dim>(fillValue, dynamicSize) {}

        vector(list<T, dim> base) : list<T, dim>(base) {}
        vector(const vector& copy) : list<T, dim>(copy) {}
        vector& operator=(const vector& copy)
        {
            list<T, dim>::operator=(copy);
            return *this;
        }
        vector& operator=(const list<T, dim>& base)
        {
            list<T, dim>::operator=(base);
            return *this;
        }

        const T& x = this->data[0];
        const T& y = this->data[1];
        const T& z = this->data[2];
        const T& w = this->data[3];

        const T& r = x;
        const T& g = y;
        const T& b = z;
        const T& a = w;

        static inline vector zero = vector(T(0));

        void print(std::ostream& stream) const
        {
            stream << "{";
            this->for_each([&](T value, size_t idx)
            {
                stream << value;
                if(idx != dim - 1)
                    stream << ' ';
            });
            stream << "}";
        }

        vector operator+ (const vector& rhs) const
        {
            const vector& lhs = *this;
            return create([lhs, &rhs](size_t idx)
            {
                return lhs[idx] + rhs[idx];
            });
        }
        T operator*(const vector& rhs) const
        {
            return reduce<T>([&rhs](T val, size_t idx, T prev)
            {
                return prev + val*rhs[idx];
            });
        }
        vector operator-()const
        {
            return map([](T value, size_t idx)
            {
                return -value;
            });
        }
        vector operator-(const vector& rhs)const
        {
            const vector& lhs = *this;
            return lhs + (-rhs);
        }
        vector operator* (float coeff)const
        {
            return this->map([&coeff](T val, size_t idx) -> T
            {
                return val * coeff;
            });
        }
        T magnitude() const
        {
            return sqrt((*this) * (*this));
        }
        bool operator== (const vector& rhs)
        {
            return reduce<bool>([&rhs](T value, size_t idx, bool prev)
            {
                if (prev == false)
                    return prev;
                return prev && (value == rhs[idx]);
            }, true);
        }
        vector direction() const
        {
            if(*this == zero)
                return zero;
            return (*this)*(1.0/magnitude());
        }
        vector& normalize()
        {
            *this = this->direction();
            return *this;
        }
    };
    template <size_t dim, typename T>
    std::ostream& operator<<(std::ostream& stream, const vector<dim, T>& m){m.print(stream); return stream;}

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

/*
    //nr_rows belongs to lhs, nr_cols belongs to rhs!
    template <int lhs_cols, int rhs_rows>
    concept multipliable = lhs_cols == rhs_rows;

    template <int n_rows, int n_cols = n_rows, typename T = float>
    requires (n_rows >= 0 && n_cols >= 0)
    class matrix
    {
        vector<n_rows*n_cols, T> data;
        T& operator[](size_t idx) {return this->data[idx];}
        matrix(){}
        static matrix get_identity()
        {
            static_assert(n_rows == n_cols);
            matrix identity;
            matrix::create([](int row, int col)
            {
                if (row == col)
                    return T(1);
                else
                    return T(0);
            }, identity);
            return identity;
        }
public:
        //functors
        typedef std::function<T(T value, int row, int col)>      Fmapping;
        typedef std::function<T(int row, int col)>              Fbuilding;
        typedef std::function<void(T& value, int row, int col)> Fmutation;
        typedef std::function<void(T value, int row, int col)>    Faction;

        matrix(Fbuilding builder){*this = matrix::create(builder);}
        matrix(const std::vector<T>& list) : data{list}{}
        
        matrix(const matrix& other)
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
        
        //Avoid this constructor when possible. Error prone.
        matrix(const std::initializer_list<T>& list)
        {
            create([&list](int row, int col)
            {
                return list.begin()[row*n_cols + col];
            }, *this);
        }
        matrix(T fillValue)
        {
            create([&fillValue](int row, int col)
            {
                return fillValue;
            }, *this);
        }
        T operator()(size_t r, size_t c)const{return data(r*n_cols + c);}

        matrix map(Fmapping mapping) const
        {
            matrix<n_rows, n_cols, T> result = matrix::zero;
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    result.data[i*n_cols + j] = mapping((*this)(i, j), i, j);
            //XXX returning by copy!
            return result;
        }
        static void create(Fbuilding build, matrix& result)
        {
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    result.data[i*n_cols + j] = build(i, j);
        }
        matrix& mutate(Fmutation mutation)
        {
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    mutation(this->data[i*n_cols + j], i, j);
            return *this;
        }
        const matrix& for_each(Faction action) const
        {
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    action((*this)(i, j), i, j);
            return *this;
        }

        const static inline matrix<n_rows, n_cols, T> zero = matrix(T(0));
        const static inline matrix<n_rows, n_cols, T> I = matrix::get_identity();

        //keep in mind that modifying this vector will mutate the matrix!
        inline vector<n_cols, T> row(size_t idx)
        {
            assert(idx < n_rows);
            const size_t startIdx = idx*n_cols;
            vector<n_cols, T> result(&this->data[startIdx]);
            return result;
        }
        inline vector<n_cols, T> row_copy(size_t idx) const
        {
            assert(idx < n_rows);
            const size_t startIdx = idx*n_cols;
            vector<n_cols, T> result;
            for(size_t i = 0; i < n_cols; ++i)
                result[i] = this->data(startIdx + i);
            return result;
        }
        inline vector<n_rows, T> col(size_t idx)const
        {
            assert(idx < n_cols);
            vector<n_rows, T> result;
            for(size_t i = 0; i < n_rows; ++i)
                result.data[i] = this->data(idx + i*n_cols);
            return result;
        }

        
        void mutate_rows(const std::function<void(vector<n_cols, T>& row, size_t idx)>& mutation)
        {
            for(size_t i = 0; i < n_rows; ++i)
            {
                vector<n_cols, T> row(this->row(i).data);
                mutation(row, i);
            }
        }
        void for_each_row(const std::function<void(const vector<n_cols, T>& row, size_t idx)>& action)const
        {
            for(size_t i = 0; i < n_rows; ++i)
                action(row_copy(i), i);
        }
        void set_row(size_t idx, const vector<n_cols, T>& row)
        {
            this->row(idx) = row;
        }
        matrix map_rows(const std::function<vector<n_cols, T>(const vector<n_cols, T>& row, size_t idx)>& mapping) const
        {
            matrix result = matrix::zero;
            for(size_t i = 0; i < n_rows; ++i)
                result.set_row(i, mapping(this->row_copy(i), i));
            return result;
        }

        void print(std::ostream& stream) const
        {
            stream << "[";
            (*this).for_each([&](T value, int row, int col)
            {
                if (col == 0)
                    stream << "{";
                stream << value;
                if(col == n_cols - 1)
                    if (row == n_rows - 1)
                        stream << "}";
                    else
                        stream << "}, ";
                else 
                    stream << ' ';
            });
            stream << "]";
        }

        matrix operator+ (const matrix<n_rows, n_cols, T> rhs) const
        {
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
        matrix<n_rows, nr_cols> operator*(const matrix<nr_rows, nr_cols, T> rhs) const requires math::multipliable<n_cols, nr_rows>
        {
            const matrix& lhs = (*this);
            const int mul_size = nr_rows; // == n_cols
            matrix result;
            matrix<n_rows, nr_cols>::create([&](int row, int col)
            {
                T sum{0};
                for (size_t i = 0; i < mul_size; ++i)
                    sum += lhs(row, i) * rhs(i, col);
                return sum;
            }, result);
            return result;
        }
        matrix operator*(const float coeff) const
        {
            return map([&](T value, int row, int col){return coeff*value;});
        }

        inline matrix<n_cols, n_rows, T> transpose()
        {
            matrix<n_cols, n_rows> result;
            matrix<n_cols, n_rows, T>::create([&](int row, int col)
            {
                return (*this)(col, row);
            }, result);
            return result;
        }
        inline matrix reciprocal()
        {
            return map([](T value, int row, int col)
            {
                return value == T(0) ? value : 1.0/value; 
            });
        }
    };
    template <int n_rows, int n_cols = n_rows, typename T = float>
    std::ostream& operator<<(std::ostream& stream, const matrix<n_rows, n_cols, T>& m){m.print(stream); return stream;}

    template <int n_rows, int n_cols = n_rows, typename T = float>
    inline matrix<n_rows, n_cols, T> operator*(const float coeff, const matrix<n_rows, n_cols, T> m)
    {
        return m*coeff;
    }

    template <int dim, typename T = float>
    inline matrix<dim, dim, T> get_scale(std::array<T, dim> diagonals)
    {
        return matrix<dim, dim, T>([&](int row, int col)
        {
            if (row == col)
                return diagonals[row];
            else
                return T(0);
        });
    }

    template <int dim, typename T = float>
    using col_vector = matrix<dim, 1, T>;
    
    template <int dim, typename T = float>
    using row_vector = matrix<1, dim, T>;
*/
};
/*
namespace output
{
    typedef math::vector<3, u_int8_t> RGB24;

    template<uint nRows, uint nCols>
    using colorbuffer = math::matrix<nRows, nCols, RGB24>;

    template<uint nRows, uint nCols>
    using alphabuffer = math::matrix<nRows, nCols, uint8_t>;

    template<uint nLines, uint lineSpan>
    struct framebuffer
    {
        colorbuffer<nLines, lineSpan> colorPlane = colorbuffer<nLines, lineSpan>(RGB24({100, 100, 100}));
        alphabuffer<nLines, lineSpan> alphaPlane = alphabuffer<nLines, lineSpan>(1.0);
    };
    class window
    {
public:
        SDL_Window* handle;
        window(const char* title, uint x, uint y, uint width, uint height)
        {
            handle = SDL_CreateWindow(title, x, y, width, height, 0);
            if(!handle)
                throw std::runtime_error("WINDOW ERROR");
        }

        template<uint nLines, uint lineSpan>
        void write_frame(const framebuffer<nLines, lineSpan>& frame)
        {
            SDL_Surface* srf = SDL_GetWindowSurface(handle);
            SDL_LockSurface(srf);
            uint32_t* pixelPtr = (uint32_t*)srf->pixels;
            for(size_t i = 0; i < nLines; ++i)
                for(size_t j = 0; j < lineSpan; ++j)
                {
                    const RGB24& RGB = frame.colorPlane(i, j);
                    pixelPtr[i*srf->w + j] = SDL_MapRGB(srf->format, RGB.r, RGB.g, RGB.b);
                }
            SDL_UnlockSurface(srf);
        }
        void update_surface()
        {
            SDL_UpdateWindowSurface(handle);
        }
    };
};
*/
int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
{
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
        return -1;

    constexpr uint a = 700;
    constexpr uint b = 500;
//    output::window window("title", 200, 200, a, b);

    using namespace math;
//    using namespace output;

    auto print = [](float v, size_t idx){std::cout << v;};

    vector<3> v1({1, 0, 1});
    std::cout << v1.direction();

    return 0;
}