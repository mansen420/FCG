#include <cstddef>
#include <iostream>
#include <functional>

namespace math
{
    template <typename T, int dim>
    class nvector
    {
    };
    template <typename T, int n_rows, int n_cols>
    class matrix
    {
        T data[n_rows][n_cols];

        public:

        //returns pointer to row (use this when mutation is desired)
        T* operator[](int idx) {return this->data[idx];}
        //returns value of element at [r][c]
        T operator()(size_t r, size_t c)const{return data[r][c];}

        //returns mapped copy of object
        matrix map(std::function<T(T value, int row, int col)>mapping) const
        {
            matrix<T, n_rows, n_cols> result;
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    result[i][j] = mapping((*this)(i, j), i, j);
            return result;
        }
        //performs action on each element of matrix (allows mutation!)
        void for_each(std::function<void(T& value, int row, int col)>action)
        {
            for(size_t i = 0; i < n_rows; ++i)
                for(size_t j = 0; j < n_cols; ++j)
                    action((*this)[i][j], i, j);
        }

        void print(std::ostream& stream)
        {
            stream << "{";
            (*this).for_each([&](T value, int row, int col)
            {
                if (col == 0)
                    stream << "{";
                stream << value;
                if(col == n_cols - 1)
                {
                    if (row == n_rows - 1)
                        stream << "}";
                    else
                        stream << "}, ";
                }
                else 
                    stream << ' ';
            });
            stream << "}";
        }

        matrix operator+ (const matrix<T, n_rows, n_cols> rhs) const
        {
            return this->map([&](int value, int row, int col){return value + rhs(row, col);});
        }
        matrix operator-() const
        {
            return this->map([](T val, int row, int col){return -val;});
        }
    };
};


int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
{
    using namespace math;
    matrix<int, 2, 2> m;
    m.for_each([](int& value, int row, int col){value = row;});
    m.print(std::cout);
    matrix<int, 2, 2> m2;
    m2.for_each([](int& value, int row, int col){value = col;});
    m2.print(std::cout);

    (m + -m2).print(std::cout);

    return 0;   
}