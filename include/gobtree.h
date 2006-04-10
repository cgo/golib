#ifndef GOBTREE_H
#define GOBTREE_H

#include <goobjectbase.h>

/** @addtogroup data
 * @{
 */
/** 
 * @brief Element of a binary tree.
 */
template<class T>
class goBTreeElement : public goObjectBase
{
    public:
        goBTreeElement (const T&);
        virtual ~goBTreeElement ();

		goBTreeElement<T>* leftChild;
		goBTreeElement<T>* rightChild;
		goBTreeElement<T>* parent;
        T                  value;
};

template<class T> class goBTree;

/** 
 * @brief Running through a binary tree.
 *
 * Currently only offers depth first order.
 *
 * @todo Add breadth first order.
 */
template<class T>
class goBTreeAlgorithm
{
    public:
        goBTreeAlgorithm () {};
        virtual ~goBTreeAlgorithm () {};
        bool depthFirst (typename goBTree<T>::Element* root);
        virtual bool action (typename goBTree<T>::Element* node) { return false; };
};


/** 
 * @brief Binary tree.
 */
template<class T>
class goBTree : public goObjectBase
{
    public:
        typedef goBTreeElement<T> Element;
        typedef const goBTreeElement<T> ConstElement;

    public:
        goBTree ();
        goBTree (typename goBTree<T>::Element* root);
        virtual ~goBTree ();

        bool          isEmpty () const;
        Element*      getRoot ();
        ConstElement* getRoot () const;
        
    private:
        goBTreeElement<T>* myRoot;
};
/** @} */
#endif
