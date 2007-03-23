#include <gosumproduct.h>

int main ()
{
    goSumProduct<goSize_t,goFloat> sp;
    goFactorGraph<goSize_t,goFloat> fg;
    goFactorGraph<goSize_t,goFloat>::NodeList& nodelist = fg.myNodes;
    
    goAutoPtr<goFGNode<goSize_t,goFloat> > node;
    
    node.set (new goFGNodeVariable<goSize_t,goFloat>);
    node->value = 0.0f;
    nodelist.append (node);

    node.set (new goFGNodeVariable<goSize_t,goFloat>);
    node->value = 1.0f;
    nodelist.append (node);

    node.set (new goFGNodeVariable<goSize_t,goFloat>);
    node->value = 2.0f;
    nodelist.append (node);

    node.set (new goFGNodeFactor<goSize_t,goFloat>);
    node->value = 3.0f;
    nodelist.append (node);

    fg.connect (nodelist(0)->elem,nodelist(1)->elem);
    fg.connect (nodelist(1)->elem,nodelist(2)->elem);
    fg.connect (nodelist(0)->elem,nodelist(3)->elem);
    
    //nodelist(0)->elem->adj.append (nodelist(1)->elem);
    //nodelist(1)->elem->adj.append (nodelist(0)->elem);
    //nodelist(1)->elem->adj.append (nodelist(2)->elem);
    //nodelist(2)->elem->adj.append (nodelist(1)->elem);
    //nodelist(3)->elem->adj.append (nodelist(0)->elem);
    //nodelist(0)->elem->adj.append (nodelist(3)->elem);
    
    printf ("First test graph:\n");
    sp.run (fg);
   
    printf ("\nSecond test graph:\n");
    //= Build a second graph to test:
    {
        nodelist.erase ();
        fg.myEdges.erase ();
        goFloat f = 1.0;
        for (goSize_t i = 0; i < 11; ++i, f += 1.0f)
        {
            nodelist.append (new goFGNodeVariable<goSize_t,goFloat>);
            nodelist.getTail()->value = f;
        }

        fg.connect (nodelist(0)->elem, nodelist(1)->elem);
        fg.connect (nodelist(1)->elem, nodelist(8)->elem);
        fg.connect (nodelist(1)->elem, nodelist(2)->elem);
        fg.connect (nodelist(2)->elem, nodelist(3)->elem);
        fg.connect (nodelist(2)->elem, nodelist(4)->elem);
        fg.connect (nodelist(2)->elem, nodelist(5)->elem);
        fg.connect (nodelist(4)->elem, nodelist(6)->elem);
        fg.connect (nodelist(6)->elem, nodelist(7)->elem);
        fg.connect (nodelist(8)->elem, nodelist(9)->elem);
        fg.connect (nodelist(9)->elem, nodelist(10)->elem);
        
        //nodelist(0)->elem->adj.append (nodelist(1)->elem);
        //nodelist(1)->elem->adj.append (nodelist(0)->elem);
        //nodelist(1)->elem->adj.append (nodelist(8)->elem);
        //nodelist(8)->elem->adj.append (nodelist(1)->elem);
        //nodelist(1)->elem->adj.append (nodelist(2)->elem);
        //nodelist(2)->elem->adj.append (nodelist(1)->elem);
        //nodelist(2)->elem->adj.append (nodelist(3)->elem);
        //nodelist(3)->elem->adj.append (nodelist(2)->elem);
        //nodelist(2)->elem->adj.append (nodelist(4)->elem);
        //nodelist(4)->elem->adj.append (nodelist(2)->elem);
        //nodelist(2)->elem->adj.append (nodelist(5)->elem);
        //nodelist(5)->elem->adj.append (nodelist(2)->elem);
        //nodelist(4)->elem->adj.append (nodelist(6)->elem);
        //nodelist(6)->elem->adj.append (nodelist(4)->elem);
        //nodelist(6)->elem->adj.append (nodelist(7)->elem);
        //nodelist(7)->elem->adj.append (nodelist(6)->elem);
        //nodelist(8)->elem->adj.append (nodelist(9)->elem);
        //nodelist(9)->elem->adj.append (nodelist(8)->elem);
        //nodelist(9)->elem->adj.append (nodelist(10)->elem);
        //nodelist(10)->elem->adj.append (nodelist(9)->elem);

        //= Insert a loop
        // fg.connect (nodelist(2), nodelist(8));
        //nodelist(2)->elem->adj.append (nodelist(8)->elem);
        //nodelist(8)->elem->adj.append (nodelist(2)->elem);
        
        sp.run (fg);
    }
    
    exit(1);
}
