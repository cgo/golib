#ifndef GOTREE_H
#define GOTREE_H

#include <goobjectbase.h>

/** @addtogroup data
 * @{
 */
/** 
 * @brief Element of a tree.
 */
template<class T>
class goTreeElement : public goObjectBase
{
    public:
        goTreeElement ();
        goTreeElement (const T&);
        virtual ~goTreeElement ();

		goList<goTreeElement<T>*> children;
		goTreeElement<T>*         parent;
        T                         value;
};

template<class T> class goTree;

/** 
 * @brief Running through a binary tree.
 *
 * Currently only offers depth first order.
 *
 * @todo Add breadth first order.
 */
template<class T>
class goTreeAlgorithm
{
    public:
        goTreeAlgorithm () {};
        virtual ~goTreeAlgorithm () {};
        bool breadthFirst (typename goTree<T>::Element* root);
        bool breadthFirst (typename goTree<T>::ConstElement* root) const;
        bool depthFirst (typename goTree<T>::Element* root);
        bool depthFirst (typename goTree<T>::ConstElement* root) const;
        virtual bool action (typename goTree<T>::Element* node) { return false; };
        virtual bool action (typename goTree<T>::ConstElement* node) const { return false; };
};


/** 
 * @brief Binary tree.
 */
template<class T>
class goTree : public goObjectBase
{
    public:
        typedef goTreeElement<T> Element;
        typedef const goTreeElement<T> ConstElement;

    public:
        goTree ();
        goTree (typename goTree<T>::Element* root);
        virtual ~goTree ();

        bool          isEmpty () const;
        void          setRoot (typename goTree<T>::Element* e);
        Element*      getRoot ();
        ConstElement* getRoot () const;

        void          erase   ();
       
        bool          writeDOT (FILE* f) const;

    private:
        goTreeElement<T>* myRoot;
};
/** @} */
#endif
