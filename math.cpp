#include <cstddef>
#include <cassert>
#include <cmath>
#include <exception>
#include <functional>
#include <iostream>
namespace math
{
    //Pure vector class.
    template <int dim, typename T = float>
    requires (dim > 0)
    class vector
    {
        std::array<T, dim> data;
public:

        vector(const std::array<T, dim>& data) : data{data} {}
        vector(const std::initializer_list<T>& list) 
        {
            for(size_t i = 0; i < dim; ++i)
                data[i] = list.begin()[i];
        }
        vector(const std::function<T(int idx)>& builder)
        {
            *this = create(builder);
        }

        vector(T fillValue)
        {
            for(size_t i = 0; i < dim; ++i)
                data[i] = fillValue;
        }

        vector() = delete;

        T operator[](int idx)const{return data[idx];}
        operator std::array<T, dim>()const{return data;}

        static inline vector zero = vector(T{0});

        static vector create(std::function<T(size_t idx, T srcValue)> builder, const vector& source = vector::zero)
        {
            std::array<T, dim> data;
            for(size_t i = 0; i < dim; ++i)
                data[i] = builder(i, source[i]);
            vector result(data);
            return result;
        }

        T reduce(const std::function<void(T value, size_t idx, T& previous)>& reduction, T initial = T{0})const
        {
            T previous = initial;
            for(size_t i = 0; i < dim; ++i)
                reduction((*this)[i], i, previous);
            return previous;
        }
        void for_each(const std::function<void(T value, size_t idx)>& action)const
        {
            for(size_t i = 0; i < dim; ++i)
                action((*this)[i], i);
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
            return reduce([&](T value, size_t idx, T& prev)
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
            return this->data == rhs.data;
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
    inline vector<dim, T> operator*(T coeff, vector<dim, T> u)
    {
        return u*coeff;
    }
    inline vector<3> cross(const vector<3>& a, const vector<3>& b)
    {
        return vector<3>({a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1] - a[1]*b[0]});
    }



    //nr_rows belongs to lhs, nr_cols belongs to rhs!
    template <int lhs_cols, int rhs_rows>
    concept multipliable = lhs_cols == rhs_rows;

    template <int n_rows, int n_cols = n_rows, typename T = float>
    requires (n_rows > 0 && n_cols > 0)
    class matrix
    {   
        std::array<std::array<T, n_cols>, n_rows> data;
public:

        //functors
        typedef std::function<T(T value, int row, int col)>      Fmapping;
        typedef std::function<T(int row, int col)>              Fbuilding;
        typedef std::function<void(T& value, int row, int col)> Fmutation;
        typedef std::function<void(T value, int row, int col)>    Faction;

        matrix() = delete;

        matrix(Fbuilding builder){*this = matrix::create(builder);}
        matrix(const std::array<std::array<T, n_cols>, n_rows>& list) : data{list}{}
        //Avoid this constructor when possible. Error prone.
        matrix(const std::initializer_list<std::array<T, n_cols>>& list)
        {
            for(size_t i = 0; i < n_rows; ++i)
                data[i] = list.begin()[i];
        }



        vector<n_cols, T> operator[](size_t idx) {return this->data[idx];}
        T operator()(size_t r, size_t c)const{return data[r][c];}


        matrix map(Fmapping mapping) const
        {
            matrix<n_rows, n_cols, T> result = matrix::zero;
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    result.data[i][j] = mapping((*this)(i, j), i, j);
            return result;
        }
        static matrix create(Fbuilding build)
        {
            std::array<std::array<T, n_cols>, n_rows> data;
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    data[i][j] = build(i, j);
            matrix result(data);
            return result;
        }
        void mutate(Fmutation mutation)
        {
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    mutation(this->data[i][j], i, j);
        }
        void for_each(Faction action) const
        {
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    action((*this)(i, j), i, j);
        }

        const static inline matrix<n_rows, n_cols, T> zero = create([](int row, int col){return T{0};});
        const static inline matrix<n_rows, n_cols, T> I = matrix::create([](int row, int col)
        {
            static_assert(n_rows == n_cols);
            if (row == col)
                return T{1};
            else
                return T{0};
        });


        std::array<T, n_cols> row(size_t idx){return data[idx];}
        std::array<T, n_rows> col(size_t idx)
        {
            std::array<T, n_rows> result;
            for(size_t i = 0; i < n_rows; ++i)
                result[i] = data[i][idx];
        }

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
            return this->create([&](T value, int row, int col){return value + rhs(row, col);});
        }
        matrix operator-() const
        {
            return this->create([](T val, int row, int col){return -val;});
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
            return matrix<n_rows, nr_cols>::create([&](int row, int col)
            {
                T sum{0};
                for (size_t i = 0; i < mul_size; ++i)
                    sum += lhs(row, i) * rhs(i, col);
                return sum;
            });
        }
        matrix operator*(const float coeff) const
        {
            return map([&](T value, int row, int col){return coeff*value;});
        }

        inline matrix<n_cols, n_rows, T> transpose()
        {
            return matrix<n_cols, n_rows, T>::create([&](int row, int col)
            {
                return (*this)(col, row);
            });
        }
        inline matrix reciprocal()
        {
            return map([](T value, int row, int col)
            {
                return value == T{0} ? value : 1.0/value; 
            });
        }
    };
    
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
                return T{0};
        });
    }


    template <int dim, typename T = float>
    using col_vector = matrix<dim, 1, T>;
    
    template <int dim, typename T = float>
    using row_vector = matrix<1, dim, T>;
};

namespace output
{
    class ppm_writer
    {
public:
        
    };
};

int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
{
    using namespace math;

    vector<3> w = vector<3>({1, 1, 1}).direction();
    vector<3> v({0, 1, 0});
    auto u = cross(v, w);
    v = cross(u, w);
    matrix<3> m1({u, v, w});
    //change of frame matrix
    m1 = m1.transpose();

    m1.print(std::cout);

    return 0;   
}