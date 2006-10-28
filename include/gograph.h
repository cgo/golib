#ifndef GOGRAPH_H
#define GOGRAPH_H

#include <goobjectbase.h>
#include <golist.h>

/** @addtogroup data
 * @{
 */
/** 
 * @brief TO BE IMPLEMENTED Node of a Graph.
 */
template<class T>
class goGraphNode : public goObjectBase
{
    public:
        goGraphNode ();
        goGraphNode (const T&);
        virtual ~goGraphNode ();

		goList<goGraphNode<T>*> adj;   //= Adjacency list
        T                       value;

        enum Status
        {
            NORMAL,
            VISITED
        };

        enum goGraphNode<T>::Status status;
};

template<class T> class goGraph;

/** 
 * @brief Running through a binary Graph.
 *
 * Currently only offers depth first order.
 *
 * @todo Add breadth first order.
 */
template<class T>
class goGraphAlgorithm
{
    public:
        goGraphAlgorithm () {};
        virtual ~goGraphAlgorithm () {};
        bool breadthFirst (typename goGraph<T>::Node* root, goGraph<T>& graph);
        // bool breadthFirst (typename goGraph<T>::ConstNode* root) const;
        bool depthFirst (typename goGraph<T>::Node* root, goGraph<T>& graph);
        // bool depthFirst (typename goGraph<T>::ConstNode* root) const;
        virtual bool action (typename goGraph<T>::Node* node) { return false; };
        virtual bool action (typename goGraph<T>::ConstNode* node) const { return false; };

    private:
        bool depthFirstVisit (typename goGraph<T>::Node* root);
};


/** 
 * @brief Graph.
 */
template<class T>
class goGraph : public goObjectBase
{
    public:
        typedef goGraphNode<T> Node;
        typedef const goGraphNode<T> ConstNode;
        typedef goList<goGraphNode<T>*> NodeList;

    public:
        goGraph ();
        virtual ~goGraph ();

        goGraphNode<T>*          newNode ();
        goList<goGraphNode<T>*>& getNodes ();
        
        bool          isEmpty () const;
        void          erase   ();
        bool          writeDOT (FILE* f) const;

    private:
        goList<goGraphNode<T>*> myNodes;
};
/** @} */
#endif
