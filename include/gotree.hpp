template <class T>
goTreeNode<T>::goTreeNode () {
  parent = 0;
}

template <class T>
goTreeNode<T>::~goTreeNode () {
  sons.erase();
}

template <class T>
void
goTreeNode<T>::setParentPtr (goTreeNode<T>* t) {
  parent = t;
}

template <class T>
goTreeNode<T>*
goTreeNode<T>::getNextSonPtr () {
  goTreeNode<T>* retval;
  if (sons.isEmpty()) {
    return 0;
  }
  if (sons.isTail()) {
    retval = *(goTreeNode<T>**)sons.getCurrent();
    sons.resetToFront();
    return retval;
  }
  return *(goTreeNode<T>**)sons.getNext();
}

template <class T>
void
goTreeNode<T>::prevSon () {
  if (sons.getSize() == 1) {
    return;
  }
  if (sons.isFront()) {
    while (!sons.isTail())
      sons.getNext();
  } else {
    sons.getPrev();
  }
}

template <class T>
void
goTreeNode<T>::addSonPtr (goTreeNode<T>* n) {
  sons.append ((void*&)n);
  sons.resetToFront();
  // cout << "added " << n->getContent() << " to sons list of " << getContent() << endl;
  // cout << "sons list is now of size " << sons.getSize() << endl;
}

template <class T>
goList<void*>*
goTreeNode<T>::getSonsListPtr () {
  return &sons;
}


template <class T>
goTree<T>::goTree () {
  root    = 0;
  current = root;
  depth   = 0;
  pushed  = 0;
  pushedDepth = 0;
  size    = 0;
}

template <class T>
goTree<T>::~goTree () {
}

template <class T>
void
goTree<T>::gotoRoot () {
  current = root;
  depth   = 0;
}

template <class T>
void
goTree<T>::gotoParent () {
  if (current != 0) {
    goTreeNode<T>* temp;
    temp = current->getParentPtr();
    if (temp != 0) {
      current = temp;
      depth--;
    }
  }
}

template <class T>
T
goTree<T>::getNextSon () {
  if (current != 0) {
    goTreeNode<T>* temp;
    temp = current->getNextSonPtr();
    if (temp != 0) {
      return (temp->getContent());
    }
  }
  return dummy;
}

template <class T>
unsigned int
goTree<T>::getNumberOfSons () {
  return (unsigned int)(current->getSonsListPtr()->getSize());
}

template <class T>
T
goTree<T>::getCurrent () {
  if (current) {
    return current->getContent();
  }
  return dummy;
}

template <class T>
void
goTree<T>::gotoSon () {
  if (current != 0) {
    if (getNumberOfSons() > 0) {
      current->prevSon ();
      goTreeNode<T>* temp;
      temp = current->getNextSonPtr();
      if (temp) {
	current = temp;
	depth++;
      }
    }
  }
}

template <class T>
void
goTree<T>::add (T item) {
  goTreeNode<T>* newNode = new goTreeNode<T>;
  if (root == 0) {
    // cout << "adding root as ";
    root = newNode;
    current = newNode;
    current->setContent (item);
    current->setParentPtr (0);
    // cout << current->getContent() << endl;
  } else {
    // cout << "adding new son as ";
    newNode->setContent (item);
    newNode->setParentPtr (current);
    current->addSonPtr (newNode);
    // cout << newNode->getContent() << endl;
  }
  size++;
}

template <class T>
bool
goTree<T>::isLeaf () {
  return (current->getSonsListPtr()->isEmpty());
}

template <class T>
bool
goTree<T>::isRoot () {
  return (root == current);
}

template <class T>
bool
goTree<T>::push () {
  pushed = current;
  pushedDepth = depth;
  current->prevSon();
  pushedSonPtr = current->getNextSonPtr();
  return true;
}

template <class T>
bool
goTree<T>::pop () {
  if (pushed != 0) {
    current = pushed;
    depth   = pushedDepth;
    while (current->getNextSonPtr() != pushedSonPtr);
    return true;
  }
  return false;
}

template <class T>
bool
goTree<T>::enqueue () {
  Q.add ((void*&)current);
  depthQ.add (depth);
  return true;
}

template <class T>
bool
goTree<T>::dequeue () {
  current = (goTreeNode<T>*)Q.getHead();
  Q.remove();
  depth   = depthQ.getHead();
  depthQ.remove();
  return true;
}

template <class T>
bool
goTree<T>::dequeueTail () {
  current = (goTreeNode<T>*)Q.getTail();
  Q.removeTail();
  depth   = depthQ.getTail();
  depthQ.removeTail();
  return true;
}

