#ifndef GOBTREE_H
#define GOBTREE_H

#include <goobjectbase.h>
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

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
        goBTreeElement ();
        goBTreeElement (const T&);
        virtual ~goBTreeElement ();

		goAutoPtr<goBTreeElement<T> > leftChild;
		goAutoPtr<goBTreeElement<T> > rightChild;
		goAutoPtr<goBTreeElement<T> > parent;
        T                             value;
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
        bool breadthFirst (typename goBTree<T>::ElementPtr root);
        // bool breadthFirst (typename goBTree<T>::ElementConstPtr root) const;
        bool depthFirst (typename goBTree<T>::ElementPtr root);
        // bool depthFirst (typename goBTree<T>::ElementConstPtr root) const;
        virtual bool action (typename goBTree<T>::ElementPtr node) { return false; };
        // virtual bool action (typename goBTree<T>::ElementConstPtr node) const { return false; };
};


/** 
 * @brief Binary tree.
 */
template<class T>
class goBTree : public goObjectBase
{
    public:
        typedef goBTreeElement<T> Element;
        // typedef const goBTreeElement<T> ConstElement;
        typedef goAutoPtr<goBTreeElement<T> > ElementPtr;
        // typedef goAutoPtr<const goBTreeElement<T> > ElementConstPtr;

    public:
        goBTree ();
        goBTree (goAutoPtr<typename goBTree<T>::Element> root);
        virtual ~goBTree ();

        bool          isEmpty () const;
        void          setRoot (goAutoPtr<typename goBTree<T>::Element> e);
        ElementPtr      getRoot ();
        // ElementConstPtr getRoot () const;

        void          erase   ();
       
        bool          writeDOT (FILE* f); // const;

    private:
        ElementPtr myRoot;
};
/** @} */
#endif
