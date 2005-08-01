#ifndef GOINDEXPAIR_H
#define GOINDEXPAIR_H

/** --------------------------------------------------------------------------
 * @brief Index pair class.
 *
 * Introduced as a supplement for goSparseMatrix multiplication.
 * @todo See fixme.
 ----------------------------------------------------------------------------*/
class goIndexPair
{
    public:
        goIndexPair (goIndex_t ii = 0, goIndex_t jj = 0) : i (ii), j (jj) {};
        ~goIndexPair () {};
        goIndex_t i;
        goIndex_t j;

        bool operator== (const goIndexPair& o) const { return ((o.i == this->i) && (o.j == this->j)); };
        bool operator!= (const goIndexPair& o) const { return !(*this == o); };
        //= Used by the hash table.
        goUInt32 operator& (goUInt32 a) const
        {
            // FIXME: Quick hack.
            return (i+j) & a;
        };
};

#endif
