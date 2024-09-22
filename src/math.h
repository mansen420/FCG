#pragma once

#include <algorithm>
#include <cstddef>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <ostream>
#include <type_traits>

#include "list.h"

namespace math
{
    template <size_t, size_t, typename, bool>
    class matrix;

    //Pure vector class
    //TODO add template deduction guides
    template <size_t dim = DYNAMIC, typename T = float, bool inlined = true>
    class vector : public list<T, dim, inlined>
    {
protected:
        template <size_t, size_t, typename, bool>
        friend class matrix;
public:
        using list<T, dim, inlined>::list;
        using list<T, dim, inlined>::operator =;

        static inline vector zero = vector(T(0));

//        [[nodiscard]] inline matrix<dim, 1, T> col()const
//        {
//            return matrix<dim, 1, T>(*this, this->size());
//        }
//        [[nodiscard]] inline matrix<1, dim, T> row()const
//        {
//            return matrix<1, dim, T>(*this, this->size());
//        }

        //TODO these functions should not care about whether the vectors are inlined or not
        inline vector operator+ (const vector& rhs) const
        {
            const vector& lhs = *this;
            return vector([lhs, &rhs](size_t idx)
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
            return vector(this->map([&coeff](T val, size_t idx) -> T
            {
                return val * coeff;
            }));
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
        [[nodiscard]] T sum()const
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
        return vector<3>({a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1] - a[1]*b[0]});
    }
//Just make the matrix a list primarily, then optionally we can pass the list to a list viwe for row processing
    template <size_t n_rows, size_t n_cols = n_rows, typename T = float, bool inlined = true>
    class matrix : public list<T, n_rows * n_cols, inlined>
    {
        template <size_t, size_t, typename, bool>
        friend class matrix;
        

        [[no_unique_address]] std::conditional_t<n_cols == DYNAMIC, size_t, empty> dynamicNrCols; // == stride size
public:
        using list<T, n_rows * n_cols, inlined>::list;
        using list<T, n_cols * n_rows, inlined>::operator =;

        static matrix<n_rows, n_rows, T, inlined> get_identity() requires(n_rows == n_cols && !any_dynamic(n_rows, n_cols))
        {
            return matrix<n_rows, n_rows, T, inlined>([](size_t idx)
            {
                auto rowcol = matrix::convert_idx_to_rowcol(idx);
                if(rowcol[0] == rowcol[1])
                    return T(1);
                else
                    return T(0);
            });
        }
        static matrix<n_rows, n_rows, T, inlined> get_identity(size_t dimension) requires(any_dynamic(n_rows, n_cols))
        {
            return matrix<n_rows, n_rows, T, inlined>([](size_t idx)
            {
                auto rowcol = matrix::convert_idx_to_rowcol(idx);
                if(rowcol[0] == rowcol[1])
                    return T(1);
                else
                    return T(0);
            });
        }
        static inline matrix I = get_identity();

        inline constexpr size_t nr_cols()requires(n_cols != DYNAMIC){return n_cols;}
        inline size_t nr_cols()requires(n_cols == DYNAMIC){return dynamicNrCols;}
        inline constexpr size_t nr_rows()requires(n_rows != DYNAMIC){return n_rows;}
        inline size_t nr_rows()requires(n_rows == DYNAMIC){return this->size()/nr_cols();}
        
        template<size_t row, size_t col>
        inline T& element() requires(!any_dynamic(n_rows, n_cols))
        {
            static_assert(row < n_rows && col < n_cols, "Bounds checking failed");
            return (*this)[row * nr_cols() + col];
        }
        template<size_t row, size_t col>
        inline const T& element() const requires(!any_dynamic(n_rows, n_cols))
        {
            static_assert(row < n_rows && col < n_cols, "Bounds checking failed");
            return (*this)[row * nr_cols() + col];
        }

        inline T& element(size_t row, size_t col) 
        {
            assert(row < nr_rows() && col < nr_cols());
            return (*this)[row * nr_cols() + col];
        }
        inline const T& element(size_t row, size_t col)const
        {
            assert(row < nr_rows() && col < nr_cols());
            return (*this)[row * nr_cols() + col];
        }


        [[nodiscard]] inline static ::list<size_t, 2, true> convert_idx_to_rowcol(size_t idx)requires(!any_dynamic(n_rows, n_cols))
        {return {idx/n_cols, idx%n_cols};}
        
        
        [[nodiscard]] list_view<T, n_cols, n_rows> row_view() requires(!any_dynamic(n_rows, n_cols))
        {
            return list_view<T, n_cols, n_rows>(this->begin());
        }
    };

/*
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
*/
    // convenience typedefs

    template <int dim, typename T = float>
    using col_vector = matrix<dim, 1, T>;
    
    template <int dim, typename T = float>
    using row_vector = matrix<1, dim, T>;

    template<int nRows, int nCols, typename T>
    using transformation = matrix<nRows, nCols, T>;

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

