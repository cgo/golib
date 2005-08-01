#ifndef GOSORT_H
#define GOSORT_H

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
        goQuickSort (l,i2,array,indexArray);
    if (r > i1)
        goQuickSort (i1,r,array,indexArray);
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

#endif
