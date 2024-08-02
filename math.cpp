#include <cstddef>
#include <iostream>
#include <functional>

namespace math
{
    //nr_rows belongs to lhs, nr_cols belongs to rhs!
    template <int lhs_cols, int rhs_rows>
    concept multipliable = lhs_cols == rhs_rows;

    //functors
    template <typename T>
    using mapping_fnc = std::function<T(T value, int row, int col)>;
    template <typename T>
    using building_fnc = std::function<T(int row, int col)>;
    template <typename T>
    using mutation_fnc = std::function<void(T& value, int row, int col)>;
    template <typename T>
    using action_fnc = std::function<void(T value, int row, int col)>;

    template <int n_rows, int n_cols = n_rows, typename T = float>
    requires (n_rows > 0 && n_cols > 0)
    class matrix
    {
        T data[n_rows][n_cols];
public:

        matrix() = delete;
        //calls matrix::create()
        matrix(building_fnc builder)
        {
            *this = matrix::create(builder);
        }

        matrix(const std::array<std::array<T, n_cols>, n_rows> list)
        {
            *this = matrix::create([&](int row, int col)
            {
                return list[row][col];
            });
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
        matrix map(mapping_fnc mapping) const
        {
            matrix<n_rows, n_cols, T> result = matrix::zero;
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    result[i][j] = mapping((*this)(i, j), i, j);
            return result;
        }
        //creates new matrix. Equivalent to mapping from the zero matrix
        static matrix create(building_fnc building)
        {
            matrix<n_rows, n_cols, T> result = matrix::zero;
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    result[i][j] = building(i, j);
            return result;
        }
        //Allows mutation of each each matrix element. Use sparingly.
        void mutate(mutation_fnc mutation)
        {
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    mutation((*this)[i][j], i, j);
        }
        //Performs action on each element of matrix by value.
        void for_each(action_fnc action) const
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
    
    template <int dim, typename T>
    inline matrix<dim, dim, T> get_scale(const std::array<T, dim> diagonals)
    {
        return matrix<dim, dim, T>([&](int row, int col)
        {
            if (row == col)
                return diagonals[row];
            else
                return T{0};
        });
    }
    
    template <int n_rows, int n_cols = n_rows, typename T = float>
    matrix<n_rows, n_cols, T> operator*(const float coeff, const matrix<n_rows, n_cols, T> m)
    {
        return m*coeff;
    }

    template <int dim, typename T = float>
    using col_vector = matrix<dim, 1, T>;
    
    template <int dim, typename T = float>
    using row_vector = matrix<1, dim, T>;

    //Pure vector class.
    //Should not be mixed directly with matrix class. Convert to column or row vector first.
    template <int dim, typename T = float>
    class vector
    {
        std::array<T, dim> data;
public:

        vector(const std::array& data) : data{data} {}

    };
};


int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
{
    using namespace math;

    return 0;   
}