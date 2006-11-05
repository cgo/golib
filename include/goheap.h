#ifndef GOHEAP_H
#define GOHEAP_H

#include <gotypes.h>

/** 
 * \brief Binary heap implementation.
 */
template <class Value, class Key>
class goHeap 
{
    public:
        goHeap ()
            : myValueArray (0),
              myKeyArray (0),
              myHeapSize (0),
              myArraySize (0)
        {
        };

        virtual ~goHeap()
        {
        };

        inline goSize_t left (goSize_t i)
        {
            return ((i+1) << 1) - 1;
        };
        inline goSize_t right (goSize_t i)
        {
            return (i+1) << 1;
        };
        inline goSize_t parent (goSize_t i)
        {
            if (i > 0)
            {
                return (i-1) >> 1;
            }
            else
            {
                return 0;
            }
        };

        inline void heapify(goSize_t i)
        {
            goSize_t l = left (i);
            goSize_t r = right (i);
            goSize_t smallest = i;
            if (l < this->myHeapSize && myKeyArray[l] < myKeyArray[r])
                smallest = l;
            if (r < this->myHeapSize && myKeyArray[r] < myKeyArray[smallest])
                smallest = r;
            if (smallest != i)
            {
                Key tempK              = myKeyArray[smallest];
                Value tempV            = myValueArray[smallest];
                myKeyArray[smallest]   = myKeyArray[i];
                myKeyArray[i]          = tempK;
                myValueArray[smallest] = myValueArray[i];
                myValueArray[i]        = tempV;
                this->heapify (smallest);
            }
        };

        void buildHeap (Value* valueArray, Key* keyArray, goSize_t length)
        {
            myHeapSize   = length;
            myArraySize  = length;
            myValueArray = valueArray;
            myKeyArray   = keyArray;
            if (length < 1)
            {
                return;
            }
            for (goSize_t i = (length >> 1) - 1; i > 0; --i)
            {
                this->heapify (i);
            }
            this->heapify (0);
        };

        void removeRoot ()
        {
            if (myHeapSize < 1)
            {
                return;
            }
            if (myArraySize < 1)
            {
                return;
            }

            // Key minK = myKeyArray[0];
            myKeyArray[0] = myKeyArray[myHeapSize-1];
            myValueArray[0] = myValueArray[myHeapSize-1];
            --myHeapSize;
            this->heapify (0);
            // return minK;
        };

    protected:
        Value*   myValueArray;
        Key*     myKeyArray;
        goSize_t myHeapSize;
        goSize_t myArraySize;
};


#endif
