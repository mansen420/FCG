#include <cstddef>
#include <cassert>
#include <cmath>
#include <exception>
#include <functional>
#include <iostream>
//IO
#include <fstream>
#include <streambuf>
#include <ostream>
#include <sstream>
#include <exception>

#include "SDL.h"

namespace math
{
    template <int n_rows, int n_cols, typename T>
    requires (n_rows > 0 && n_cols > 0)
    class matrix;

    //Pure vector class.
    template <int dim, typename T = float>
    requires (dim > 0)
    class vector
    {
        template <int n_rows, int n_cols, typename D>
        friend class matrix;
        T* data = new T[dim];
        vector(T* data){this->data = data;}
        T& operator[](int idx){return data[idx];}
public:
        T operator()(size_t idx)const{return data[idx];}

        ~vector(){delete[] data;}

        vector(const std::initializer_list<T>& list) 
        {
            for(size_t i = 0; i < dim; ++i)
                data[i] = list.begin()[i];
        }
        vector(const std::function<T(size_t idx, T)>& builder)
        {
            *this = create(builder);
        }
        vector(T fillValue = T(0))
        {
            for(size_t i = 0; i < dim; ++i)
                data[i] = fillValue;
        }

        vector& operator=(const std::initializer_list<T>& list)
        {
            for(size_t i = 0; i < dim; ++i)
                data[i] = list.begin()[i];
        }
        vector& operator=(const vector& rhs)
        {
            if(this == &rhs)
                return *this;
            for(size_t i = 0; i < dim; ++i)
                this->data[i] = rhs.data[i];
            return *this;
        }
        vector(const vector& rhs)
        {
            *this = rhs;
        }

        const T& x = data[0];
        const T& y = data[1];
        const T& z = data[2];

        const T& r = x;
        const T& g = y;
        const T& b = z;


        static inline vector zero = vector(T(0));

        static vector create(std::function<T(size_t idx, T srcValue)> builder, const vector& source = vector::zero)
        {
            T* data = new T[dim];
            for(size_t i = 0; i < dim; ++i)
                data[i] = builder(i, source[i]);
            vector result(data);
            return result;
        }

        template <typename D>
        D reduce(const std::function<void(T value, size_t idx, D& previous)>& reduction, D initial = D(0))const
        {
            D previous = initial;
            for(size_t i = 0; i < dim; ++i)
                reduction((*this)[i], i, previous);
            return previous;
        }
        void for_each(const std::function<void(T value, size_t idx)>& action)const
        {
            for(size_t i = 0; i < dim; ++i)
                action((*this)(i), i);
        }
        void mutate(const std::function<void(T& value, size_t idx)>& mutation)
        {
            for(size_t i = 0; i < dim; ++i)
                mutation(this->data[i], i);
        }
        vector map(const std::function<T(size_t idx, T value)>& mapping)const
        {
            return create(mapping, *this);
        }

        void print(std::ostream& stream) const
        {
            stream << "{";
            for_each([&](T value, size_t idx)
            {
                stream << value;
                if(idx != dim - 1)
                    stream << ' ';
            });
            stream << "}";
        }

        T operator*(const vector& rhs) const
        {
            return reduce<T>([&](T value, size_t idx, T& prev)
            {
                prev += value*rhs[idx];
            });
        }
        vector operator+ (const vector& rhs) const
        {
            const vector& lhs = *this;
            return create([&](size_t idx, T srcValue)
            {
                return lhs[idx] + rhs[idx];
            });
        }
        vector operator-()const
        {
            return map([](size_t idx, T value)
            {
                return -value;
            });
        }
        vector operator-(const vector& rhs)const
        {
            const vector& lhs = *this;
            return lhs + (-rhs);
        }
        vector operator* (T coeff)const
        {
            return map([&](size_t idx, T val)
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
            return this->reduce<bool>([&rhs](T value, size_t idx, bool& prev)
            {
                if (prev == false)
                    return;
                prev = prev && (value == rhs[idx]);
            }, true);
        }
        vector direction() const
        {
            if(*this == zero)
                return zero;
            return (*this)*(1/magnitude());
        }
        void normalize()
        {
            *this = this->direction();
        }
    };
    template <int dim, typename T>
    std::ostream& operator<<(std::ostream& stream, const vector<dim, T>& m){m.print(stream); return stream;}


    template <int dim, typename T> 
    inline vector<dim, T> operator*(T coeff, vector<dim, T> u)
    {
        return u*coeff;
    }
    inline vector<3> cross(const vector<3>& a, const vector<3>& b)
    {
        return vector<3>({a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x});
    }

    //nr_rows belongs to lhs, nr_cols belongs to rhs!
    template <int lhs_cols, int rhs_rows>
    concept multipliable = lhs_cols == rhs_rows;

    template <int n_rows, int n_cols = n_rows, typename T = float>
    requires (n_rows > 0 && n_cols > 0)
    class matrix
    {
        vector<n_rows*n_cols, T> data;
        //std::array<std::array<T, n_cols>, n_rows> data;
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


        vector<n_cols, T> row(size_t idx)
        {
            assert(idx < n_rows);
            vector<n_cols, T> result;
            const size_t startIdx = idx*n_cols;
            for(size_t i = 0; i < n_cols; ++i)
                result.data[i] = this->data(startIdx + i);
            return result;
        }
        vector<n_rows, T> col(size_t idx)
        {
            assert(idx < n_cols);
            vector<n_rows, T> result;
            for(size_t i = 0; i < n_rows; ++i)
                result.data[i] = this->data(idx + i*n_cols);
            return result;
        }

/*
        void mutate_rows(const std::function<void(std::array<T, n_cols>& row, size_t idx)>& mutation)
        {
            for(size_t i = 0; i < n_cols; ++i)
                mutation(data[i], i);
        }
        void for_each_row(const std::function<void(const std::array<T, n_cols>& row, size_t idx)>& action)
        {
            for(size_t i = 0; i < n_cols; ++i)
                action(data[i], i);
        }
        void set_row(size_t idx, const std::array<T, n_cols>& row)
        {
            this->data[idx] = row;
        }
        matrix map_rows(const std::function<std::array<T, n_cols>(std::array<T, n_cols> row, size_t idx)>& mapping)
        {
            matrix result = matrix::zero;
            for(size_t i = 0; i < n_cols; ++i)
                result.set_row(i, mapping(this->row(i), i));
            return result;
        }
*/

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
};

namespace output
{
    typedef math::vector<3, u_int8_t> RGB24;

    template<uint width, uint height>
    using colorbuffer = math::matrix<width, height, RGB24>;

    template<uint width, uint height>
    using alphabuffer = math::matrix<width, height, uint8_t>;

    template<uint width, uint height>
    struct framebuffer
    {
        colorbuffer<width, height> colorPlane;
        alphabuffer<width, height> alphaPlane;
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
        template<uint width, uint height>
        void write_frame(const framebuffer<width, height>& frame)
        {
            SDL_Surface* srf = SDL_GetWindowSurface(handle);
            SDL_LockSurface(srf);
            uint32_t* pixelPtr = (uint32_t*)srf->pixels;
            for(size_t i = 0; i < width; ++i)
                for(size_t j = 0; j < height; ++j)
                {
                    const RGB24& RGB = frame.colorPlane[i][j];
                    pixelPtr[i + srf->w*j] = SDL_MapRGB(srf->format, RGB.r, RGB.g, RGB.b);
                }
            SDL_UnlockSurface(srf);
        }
        void update_surface()
        {
            SDL_UpdateWindowSurface(handle);
        }
    };
};

int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
{
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
        return -1;

    //these segfault exactly when w,h>221. wtf? stack overflow...
    constexpr uint w = 700;
    constexpr uint h = 500;
   //output::window window("title", 200, 200, w, h);

    using namespace math;
    using namespace output;
    //stack overflow!

    //window.write_frame(frame);

    //window.update_surface();

    matrix<2, 3> m1 ({1, 2, 3, 4, 5, 6});
    std::cout << m1.col(2);   

   return 0;   
}