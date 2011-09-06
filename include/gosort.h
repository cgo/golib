/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOSORT_H
#define GOSORT_H

template <class Container, class T>
static void goQuickSort (typename Container::iterator& i1, typename Container::iterator& i2)
{
    if (i1 == i2)
    {
        return;
    }
    typename Container::iterator l = i1;
    typename Container::iterator r = i2;
    printf ("Sorting %d to %d\n", *i1, *i2);
    T temp;
    T ref = *i1;  //= Reference value: just take the beginning value.
    bool stop = false;
    while (!stop)
    {
        while (l != i2 && (*l < ref) && l != r) ++l;
        while (r != i1 && (*r > ref) && l != r) --r;
        if (l != r)
        {
            //= FIXME: For lists, do a faster swap here. 
            //=        This copies the actual data elements.
            T temp = *l;
            *l = *r;
            *r = temp;
            if (l != i2)
                ++l;
            if (l == r) stop = true;
            if (r != i1)
                --r;
            if (l == r) stop = true;
        }
        else
        {
            if (l != i2)
                ++l;
            if (r != i1)
                --r;
            stop = true;
        }
    }
    if (l != i2)
        goQuickSort<Container, T> (l, i2);
    if (r != i1)
        goQuickSort<Container, T> (i1, r);
}

template < class T >
static void goQuickSort (goIndex_t i1, goIndex_t i2, T* array) 
{
    goIndex_t l = i1, r = i2;
    T temp;
    if (r <= l)
    {
        return;
    }
    T ref = array[ (r + l) >> 1 ];
    while (l <= r) 
    {
        while ( (l < i2) && (array[l] < ref) ) l++;
        while ( (r > i1) && (array[r] > ref) ) r--;
        if (l <= r) 
        {
            temp = array[l];
            array[l] = array[r];
            array[r] = temp;
            l++; r--;
        }
    }
    if (l < i2)
        goQuickSort (l,i2,array);
    if (r > i1)
        goQuickSort (i1,r,array);
}

template < class T, class indexT >
static void goQuickSort (goIndex_t i1, goIndex_t i2, T* array, indexT* indexArray) 
{
    goIndex_t l = i1, r = i2;
    T temp;
    indexT tempIdx;
    if (r <= l)
    {
        return;
    }
    T ref = array[ (r + l) >> 1 ];
    while (l <= r) {
        while ( (l < i2) && (array[l] < ref) ) l++;
        while ( (r > i1) && (array[r] > ref) ) r--;
        //      cout << "ref = " << ref << endl;
        //      cout << "l = " << l << ", r = " << r << endl;
        if (l <= r) {
            //        cout << "swapping " << (*this)[l] << "," << (*this)[r] << endl;
            temp = array[l];
            array[l] = array[r];
            array[r] = temp;
            tempIdx = indexArray[l];
            indexArray[l] = indexArray[r];
            indexArray[r] = tempIdx;
            l++; r--;
        }
    }
    if (l < i2)
        goQuickSort (l,i2,array,indexArray);
    if (r > i1)
        goQuickSort (i1,r,array,indexArray);
}

template < class T, class T2, class indexT >
static void goQuickSort (goIndex_t i1, goIndex_t i2, T* array, T2* array2, indexT* indexArray) 
{
    goIndex_t l = i1, r = i2;
    T temp;
    T2 temp2;
    indexT tempIdx;
    if (r <= l)
    {
        return;
    }
    T ref = array[ (r + l) >> 1 ];
    while (l <= r) {
        while ( (l < i2) && (array[l] < ref) ) l++;
        while ( (r > i1) && (array[r] > ref) ) r--;
        //      cout << "ref = " << ref << endl;
        //      cout << "l = " << l << ", r = " << r << endl;
        if (l <= r) {
            //        cout << "swapping " << (*this)[l] << "," << (*this)[r] << endl;
            temp = array[l];
            array[l] = array[r];
            array[r] = temp;
            temp2 = array2[l];
            array2[l] = array2[r];
            array2[r] = temp2;
            tempIdx = indexArray[l];
            indexArray[l] = indexArray[r];
            indexArray[r] = tempIdx;
            l++; r--;
        }
    }
    if (l < i2)
        goQuickSort (l,i2,array,array2,indexArray);
    if (r > i1)
        goQuickSort (i1,r,array,array2,indexArray);
}

template <class T>
static void goSort (T* array, goSize_t size)
{
    goQuickSort (0, size-1, array);
}

template <class T, class indexT>
static void goSort (T* array, indexT* indexArray, goSize_t size)
{
    goQuickSort (0, size-1, array, indexArray);
}

template < class T, class T2, class indexT >
static void goSort (T* array, T2* array2, indexT* indexArray, goSize_t size)
{
    goQuickSort (0, size-1, array, array2, indexArray);
}

#endif
