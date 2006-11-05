#ifndef __GOTREE_H
#define __GOTREE_H

#include <golist.h>
#include <goqueue.h>

template <class T>
class goTreeNode 
{
public:
  goTreeNode ();
  virtual ~goTreeNode ();
  
  T		                  elem;
  goList<goTreeNode<T>* > sons;
  goTreeNode<T>*          parent;

  
  goTreeNode<T>* getParentPtr () { return parent; }
  void		     setParentPtr (goTreeNode<T>*);
  goTreeNode<T>* getNextSonPtr ();
  void           prevSon ();
  void		     addSonPtr (goTreeNode<T>* n);
  T		         getContent () { return (T)content; }
  void		     setContent (T newCont) { content = newCont; }

};

/*!
 * \brief Tree class.
 *
 * This is a tree class with a few extensions that 
 * were needed in some university project.
 */
template <class T>
class goTree {
public:
  goTree ();
  virtual ~goTree ();

  void gotoRoot   ();
  void gotoParent ();
  T   getNextSon ();
  T   getCurrent ();
  /// Depth of the current node. Root is 0.
  unsigned int getDepth () { return depth; }
  unsigned int getSize () { return size; }
  unsigned int getNumberOfSons ();
  void gotoSon ();
  void add (T item);

  bool isLeaf ();
  bool isRoot ();

  /// Push / pop the current state (node + son pointer)
  bool push ();
  bool pop ();
  
  bool enqueue ();
  bool dequeue ();
  bool dequeueTail ();
protected:
  unsigned int depth;
  unsigned int size;
  T dummy;
  goTreeNode<T>* root;
  goTreeNode<T>* current;

  goTreeNode<T>* pushed;
  unsigned int pushedDepth;
  goTreeNode<T>* pushedSonPtr;

  goQueue<void*> Q;
  goQueue<unsigned int> depthQ;
};

#endif







