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
        basic_aggregate() = default;
        explicit basic_aggregate(T* const data) : ownsData{false}, data{data}{}
        virtual ~basic_aggregate()
        {
            if(ownsData)
                delete[] data;
        }
    };

    constexpr uint DYNAMIC = 0;
    struct empty 
    {
        empty& operator=(size_t){return *this;}
        operator size_t()const{return size_t(0);}
    };

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

        [[no_unique_address]] std::conditional_t<dim == 0, size_t, empty> dynamicSize;
public:
        inline size_t size() const
        {
            if(dim == DYNAMIC)
                return dynamicSize;
            return dim;
        }

        T& operator[](size_t idx){return this->data[idx];}
        T operator()(size_t idx)const{return this->data[idx];}

        list& operator=(const list& rhs)
        {
            if(this == &rhs)
                return *this;
            assert(this->size() == rhs.size());
            if(rhs.ownsData == false)
            {
                if(this->ownsData)
                    delete[] this->data;
                this->data = rhs.data;
                this->ownsData = false;
            }
            else
                for(size_t i = 0; i < size(); ++i)
                    this->data[i] = rhs.data[i];
            return *this;
        }
        list& operator=(const std::initializer_list<T>& data)
        {
            assert(data.size() == this->size());
            std::copy(data.begin(), data.end(), this->data);
            return *this;
        }
        list(const list& copy) : list(copy.size()) {*this = copy;}

        explicit list(const size_t dynamicSize = 0)
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
                this->dynamicSize = dynamicSize;
        }
        list(const std::initializer_list<T>& list, const size_t dynamicSize = 0) : list::list(dynamicSize)
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
            list result(size_t(this->dynamicSize));
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
protected:
        template <int n_rows, int n_cols, typename D>
        requires (n_rows >= 0 && n_cols >= 0)
        friend class matrix;
public:
        using list<T, dim>::list;

        vector& operator=(const vector& other)
        {
            list<T, dim>::operator=(other);
            return *this;
        };

        vector(const vector& other) : list<T, dim>(other){}
        vector(const list<T, dim>& other) : list<T, dim>(other){}

        template <typename D>
        operator vector<dim, D>const()
        {
            return list<T, dim>::operator const math::list<D, dim>();
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

    template <int n_rows, int n_cols = n_rows, typename T = float>
    requires (n_rows >= 0 && n_cols >= 0)
    class matrix
    {
        vector<n_rows*n_cols, T> data;

        T& operator[](size_t idx) {return this->data[idx];}
        
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
        
        [[no_unique_address]] std::conditional_t<n_rows == DYNAMIC, size_t, empty> rowSize;
        [[no_unique_address]] std::conditional_t<n_cols == DYNAMIC, size_t, empty> colSize; 
public:
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
        
        explicit matrix(size_t dynamicSizeRows = 0, size_t dynamicSizeCols = 0) : 
        data(static_cast<size_t>(dynamicSizeRows*dynamicSizeCols))
        {
            if(n_rows == DYNAMIC)
                rowSize = dynamicSizeRows;
            if(n_cols == DYNAMIC)
                colSize = dynamicSizeCols;
        }
        explicit matrix(const std::initializer_list<T>& list, size_t dynamicSizeRows = 0, size_t dynamicSizeCols = 0) : 
        data(list, static_cast<size_t>(dynamicSizeRows*dynamicSizeCols))
        {
            if(n_rows == DYNAMIC)
                rowSize = dynamicSizeRows;
            if(n_cols == DYNAMIC)
                colSize = dynamicSizeCols;
        }
        explicit matrix(T fillValue, size_t dynamicSizeRows = 0, size_t dynamicSizeCols = 0) : 
        data(fillValue, static_cast<size_t>(dynamicSizeRows*dynamicSizeCols))
        {
            if(n_rows == DYNAMIC)
                rowSize = dynamicSizeRows;
            if(n_cols == DYNAMIC)
                colSize = dynamicSizeCols;            
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
        explicit matrix(const vector<n_rows*n_cols>& data, size_t dynamicSizeRows = 0, size_t dynamicSizeCols = 0) : data{data} 
        {
            if(n_rows == DYNAMIC)
                rowSize = dynamicSizeRows;
            if(n_cols == DYNAMIC)
                colSize = dynamicSizeCols;
        }

        T operator()(size_t r, size_t c)const{return data(r*cols() + c);}


        inline matrix map(Fmapping mapping) const
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
        inline matrix& mutate(Fmutation mutation)
        {
            for(size_t i = 0; i < rows(); ++i)
                for(size_t j = 0; j < cols(); ++j)
                    mutation(this->data[i*cols() + j], i, j);
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

        //keep in mind that modifying this vector will mutate the matrix!
        inline vector<n_cols, T> row(size_t idx)
        {
            assert(idx < rows());
            const size_t startIdx = idx*cols();
            vector<n_cols, T> result(&this->data[startIdx], cols());
            return result;
        }
        inline vector<n_cols, T> row_copy(size_t idx) const
        {
            assert(idx < rows());
            const size_t startIdx = idx*cols();
            vector<n_cols, T> result(cols());
            for(size_t i = 0; i < cols(); ++i)
                result[i] = this->data(startIdx + i);
            return result;
        }
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
                action(row_copy(i), i);
        }
        void set_row(size_t idx, const vector<n_cols, T>& row)
        {
            assert(row.size() == cols());
            this->row(idx) = row;
        }
        matrix map_rows(const std::function<vector<n_cols, T>(const vector<n_cols, T>& row, size_t idx)>& mapping) const
        {
            matrix result;
            for(size_t i = 0; i < rows(); ++i)
                result.set_row(i, mapping(this->row_copy(i), i));
            return result;
        }
        
        T& element(size_t r, size_t c)
        {
            return data[r*cols() + c];
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
        matrix<n_rows, nr_cols> operator*(const matrix<nr_rows, nr_cols, T> rhs) const
        {
            const matrix& lhs = (*this);

            assert(rhs.rows() == lhs.cols());

            const int mul_size = rhs.rows();

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
        matrix operator*(const float coeff) const
        {
            return map([&](T value, int row, int col){return coeff*value;});
        }

        inline matrix<n_cols, n_rows, T> transpose()
        {
            matrix<n_cols, n_rows> result(cols(), rows());
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

        operator vector<n_rows * n_cols, T>() requires (n_cols == 1 || n_cols == DYNAMIC || n_rows == 1 || n_rows == DYNAMIC)
        {
            assert(cols() == 1 || rows() == 1);
            return data;
        }
    };
    template <int n_rows, int n_cols = n_rows, typename T = float>
    std::ostream& operator<<(std::ostream& stream, const matrix<n_rows, n_cols, T>& m){m.print(stream); return stream;}

    template <int n_rows, int n_cols = n_rows, typename T = float>
    inline matrix<n_rows, n_cols, T> operator*(const float coeff, const matrix<n_rows, n_cols, T> m)
    {
        return m*coeff;
    }

    //make this a build script
    template <int dim, typename T = float>
    inline matrix<dim, dim, T> get_scale(std::array<T, dim> diagonals, size_t dynamicSize = 0)
    {
        return matrix<dim, dim, T>([&](int row, int col)
        {
            if (row == col)
                return diagonals[row];
            else
                return T(0);
        }, dynamicSize);
    }

    template <int dim, typename T = float>
    using col_vector = matrix<dim, 1, T>;
    
    template <int dim, typename T = float>
    using row_vector = matrix<1, dim, T>;
};

namespace output
{
    typedef math::vector<3, u_int8_t> RGB24;

    template<uint nRows, uint nCols>
    using colorbuffer = math::matrix<nRows, nCols, RGB24>;

    template<uint nRows, uint nCols>
    using alphabuffer = math::matrix<nRows, nCols, uint8_t>;

    template<uint nLines = math::DYNAMIC, uint lineSpan = nLines>
    struct framebuffer
    {
        framebuffer(size_t nLinesDynamic = 0, size_t lineSpanDynamic = 0) : 
        colorPlane(RGB24(uint8_t(255)), size_t(nLinesDynamic), size_t(lineSpanDynamic)),
        alphaPlane(uint8_t(1.0), size_t(nLinesDynamic), size_t(lineSpanDynamic)) {}
        colorbuffer<nLines, lineSpan> colorPlane;
        alphabuffer<nLines, lineSpan> alphaPlane;
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
            for(size_t i = 0; i < frame.colorPlane.rows(); ++i)
                for(size_t j = 0; j < frame.colorPlane.cols(); ++j)
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

namespace renderer
{
    
    class rasterizer
    {
public:
        output::framebuffer<math::DYNAMIC> raster;
        rasterizer(size_t width, size_t height) : raster(height, width){}

        void rasterize(math::vector<2, uint> pixelLoc, output::RGB24 color)
        {
            raster.colorPlane.element(pixelLoc.y, pixelLoc.x) = color;
        }

        output::framebuffer<math::DYNAMIC> get_framebuffer(const math::vector<2, uint>& renderTargetSize)
        {
            if(renderTargetSize.x == raster.colorPlane.cols() && renderTargetSize.y == raster.colorPlane.rows())
                return raster;

            output::framebuffer<math::DYNAMIC> renderTarget(renderTargetSize.y, renderTargetSize.x);
            renderTarget.colorPlane.mutate([this, &renderTargetSize](output::RGB24& pixel, int row, int col) -> void
            {
                float vCenter = float(row + 0.5)/renderTargetSize.y; //[0, 1)
                float hCenter = float(col + 0.5)/renderTargetSize.x;

                pixel = this->raster.colorPlane(vCenter * raster.colorPlane.rows(), hCenter * raster.colorPlane.cols());
            });
            return renderTarget;
        }
    };
};

int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
{
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
        return -1;

    constexpr uint wFramebuffer = 50;
    constexpr uint hFramebuffer = 50;
    constexpr uint wWindow = 200;
    constexpr uint hWindow = 200;

    using namespace math;
    using namespace output;

    window window("title", 200, 200, wWindow, hWindow);

    //scene
    vector<2> canonY = {0, 1};
    vector<2> canonX = {1, 0};

    constexpr float Wx = 30.0;
    constexpr float Wy = 30.0;

    //construct screen transform 
    matrix<2> WtoNDC = get_scale<2>({1.f/Wx, 1.f/Wy});
    matrix<2> NDCtoSCR = get_scale<2>({wFramebuffer, hFramebuffer});
    matrix<2> WtoSCR = NDCtoSCR * WtoNDC;

    vector<2> scrX = WtoSCR * col_vector<2>(canonX);
    vector<2> scrY = WtoSCR * col_vector<2>(canonY);

    renderer::rasterizer R(wFramebuffer, hFramebuffer);
    R.rasterize(scrX, RGB24(uint8_t(0)));
    R.rasterize(scrY, RGB24(uint8_t(0)));
    // R.rasterize({0, 0}, {100, 0, 0});
    // R.rasterize({0, 1}, {0, 100, 0});
    // R.rasterize({1, 0}, {0, 0, 100});
    // R.rasterize({1, 1}, {0, 0, 0});

    window.write_frame(R.get_framebuffer(vector<2, uint>({wWindow, hWindow})));

    window.update_surface();

    SDL_Delay(5000);

    return 0;
}