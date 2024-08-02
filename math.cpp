#include <cstddef>
#include <iostream>
#include <functional>

namespace math
{
    template <typename T, int dim>
    class nvector
    {
    };

    //nr_rows belongs to lhs, nr_cols belongs to rhs!
    template <int lhs_cols, int rhs_rows>
    concept multipliable = lhs_cols == rhs_rows;

    template <int n_rows, int n_cols = n_rows, typename T = float>
    requires (n_rows > 0 && n_cols > 0)
    class matrix
    {
        T data[n_rows][n_cols];
        public:

        //calls matrix::create()
        matrix(std::function<T(int row, int col)>mapping)
        {
            *this = matrix::create(mapping);
        }

        const static inline matrix<n_rows, n_cols, T> zero = matrix::create([](int row, int col){return T{0};});
        const static inline matrix<n_rows, n_cols, T> I = matrix::create([](int row, int col)
        {
            static_assert(n_rows == n_cols);
            if (row == col)
                return T{1};
            else
                return T{0};
        });

        //returns pointer to row (use this when mutation is desired)
        T* operator[](int idx) {return this->data[idx];}
        //returns value of element at [r][c]
        T operator()(size_t r, size_t c)const{return data[r][c];}

        //returns mapped copy of object
        matrix map(std::function<T(T value, int row, int col)>mapping) const
        {
            matrix<n_rows, n_cols, T> result = matrix::zero;
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    result[i][j] = mapping((*this)(i, j), i, j);
            return result;
        }
        static matrix create(std::function<T(int row, int col)>mapping)
        {
            matrix<n_rows, n_cols, T> result = matrix::zero;
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    result[i][j] = mapping(i, j);
            return result;
        }
        //performs action on each element of matrix (allows mutation!)
        void mutate(std::function<void(T& value, int row, int col)>action)
        {
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    action((*this)[i][j], i, j);
        }
        void for_each(std::function<void(T value, int row, int col)>action) const
        {
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    action((*this)(i, j), i, j);
        }

        void print(std::ostream& stream) const
        {
            stream << "{";
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
            stream << "}";
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
        matrix operator*(const matrix<nr_rows, nr_cols, T> rhs) const requires math::multipliable<n_cols, nr_rows>
        {
            const matrix& lhs = (*this);
            const int mul_size = nr_rows; // == n_cols
            return matrix::create([&](int row, int col)
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
    };
    template <int n_rows, int n_cols = n_rows, typename T = float>
    matrix<n_rows, n_cols, T> operator*(const float coeff, const matrix<n_rows, n_cols, T> m)
    {
        return m*coeff;
    }
};


int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
{
    using namespace math;
    matrix<3, 2, int> m1([=](int row, int col){return row;});
    matrix<2, 2, int> m2([=](int row, int col){return col;});


   (2*matrix<3>::I).print(std::cout);

    return 0;   
}