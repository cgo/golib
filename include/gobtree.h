#ifndef GOBTREE_H
#define GOBTREE_H

#include <goobjectbase.h>

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

template<class T>
class goBTreeAlgorithm
{
    public:
        goBTreeAlgorithm () {};
        virtual ~goBTreeAlgorithm () {};
        bool depthFirst (typename goBTree<T>::Element* root);
        virtual bool action (typename goBTree<T>::Element* node) { return false; };
};


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

#endif
