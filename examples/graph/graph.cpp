#include <gograph.h>

int main ()
{
    goGraph<int> g;
    
    for (int i = 0; i < 10; ++i)
    {
        goGraph<int>::Node* node;
        (node = g.newNode())->value = i;
    }
    goGraph<int>::NodeList& nodeList = g.getNodes ();
    goGraph<int>::NodeList::Element* el = nodeList.getFrontElement ();
    while (el)
    {
        if (el->next)
        {
            el->elem->adj.append (el->next->elem);
            el->next->elem->adj.append (el->elem);
        }
        el = el->next;
    }
    el = nodeList.getFrontElement ();
    goSize_t sz = nodeList.getSize ();
    for (goSize_t i = 0; i < sz; ++i, el = el->next)
    {
        goGraph<int>::Node* n = g.newNode();
        el->elem->adj.append (n);
        n->value = (i + 1) * sz;
    }

    class Print : public goGraphAlgorithm<int>
    {
        public:
            Print () : goGraphAlgorithm<int> () {};
            virtual ~Print () {};

        virtual bool action (goGraph<int>::Node* node) 
        { 
            printf ("%d\n", node->value);
            return true;
        };
    };
    
    Print p;
    p.depthFirst (g.getNodes().getFrontElement()->elem, g);

    return 1;
}

#include <gograph.hpp>
