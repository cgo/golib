#include <gosumproduct.h>

int main ()
{
    goSumProduct<goFloat> sp;
    goSumProduct<goFloat>::NodeList nodelist;
    
    goAutoPtr<goFGNode<goFloat> > node;
    
    node.set (new goFGNodeVariable<goFloat>);
    node->value = 1.0f;
    nodelist.append (node);

    node.set (new goFGNodeVariable<goFloat>);
    node->value = 2.0f;
    nodelist.append (node);

    node.set (new goFGNodeVariable<goFloat>);
    node->value = 3.0f;
    nodelist.append (node);

    node.set (new goFGNodeFactor<goFloat>);
    nodelist.append (node);

    nodelist(0)->elem->adj.append (nodelist(1)->elem);
    nodelist(1)->elem->adj.append (nodelist(0)->elem);
    nodelist(1)->elem->adj.append (nodelist(2)->elem);
    nodelist(2)->elem->adj.append (nodelist(1)->elem);
    nodelist(3)->elem->adj.append (nodelist(0)->elem);
    nodelist(0)->elem->adj.append (nodelist(3)->elem);
    
    sp.run (nodelist);
   
    //= Build a second graph to test:
    {
        nodelist.erase ();
        goFloat f = 1.0;
        for (goSize_t i = 0; i < 11; ++i, f += 1.0f)
        {
            nodelist.append (new goFGNodeVariable<goFloat>);
            nodelist.getTail()->value = f;
        }
        nodelist(0)->elem->adj.append (nodelist(1)->elem);
        nodelist(1)->elem->adj.append (nodelist(0)->elem);
        nodelist(1)->elem->adj.append (nodelist(8)->elem);
        nodelist(8)->elem->adj.append (nodelist(1)->elem);
        nodelist(1)->elem->adj.append (nodelist(2)->elem);
        nodelist(2)->elem->adj.append (nodelist(1)->elem);
        nodelist(2)->elem->adj.append (nodelist(3)->elem);
        nodelist(3)->elem->adj.append (nodelist(2)->elem);
        nodelist(2)->elem->adj.append (nodelist(4)->elem);
        nodelist(4)->elem->adj.append (nodelist(2)->elem);
        nodelist(2)->elem->adj.append (nodelist(5)->elem);
        nodelist(5)->elem->adj.append (nodelist(2)->elem);
        nodelist(4)->elem->adj.append (nodelist(6)->elem);
        nodelist(6)->elem->adj.append (nodelist(4)->elem);
        nodelist(6)->elem->adj.append (nodelist(7)->elem);
        nodelist(7)->elem->adj.append (nodelist(6)->elem);
        nodelist(8)->elem->adj.append (nodelist(9)->elem);
        nodelist(9)->elem->adj.append (nodelist(8)->elem);
        nodelist(9)->elem->adj.append (nodelist(10)->elem);
        nodelist(10)->elem->adj.append (nodelist(9)->elem);

        //= Insert a loop
        nodelist(2)->elem->adj.append (nodelist(8)->elem);
        nodelist(8)->elem->adj.append (nodelist(2)->elem);
        
        sp.run (nodelist);
    }
    
    exit(1);
}
