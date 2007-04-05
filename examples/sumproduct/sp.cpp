#include <gosumproduct.h>
#include <gomaxsum.h>
#include <goautoptr.h>
#include <gofunctor.h>

class MyFactor : public goFGNodeFactor<goSize_t,float>
{
    public:
        MyFactor (goSize_t n) : goFGNodeFactor<goSize_t,float>(n)
        {
            this->ff.set (new goFunctor1<float, MyFactor, const goVector<goSize_t>&> (this, &MyFactor::my_f));
            this->setFunctor (&*this->ff);
        };
        virtual ~MyFactor () {};

        float my_f (const goVector<goSize_t>& X)
        {
            if (X[1] - X[0] == 2 || X[1] - X[0] == 4)
                return 1.0f;
            else
                return 0.0f;
        };

        goAutoPtr< 
            goFunctor1< float, MyFactor, const goVector<goSize_t>& > > ff;
};

int main ()
{
    goSumProduct<goSize_t,goFloat> sp;
    goFactorGraph<goSize_t,goFloat> fg;
    goFactorGraph<goSize_t,goFloat>::FactorArray& factors = fg.myFactors;
    goFactorGraph<goSize_t,goFloat>::VariableArray& vars = fg.myVariables;

    factors.setSize (1);
    vars.setSize (3);

    vars[0].set (new goFGNodeVariable<goSize_t,goFloat> (2));
    vars[1].set (new goFGNodeVariable<goSize_t,goFloat> (2));
    vars[2].set (new goFGNodeVariable<goSize_t,goFloat> (1));
    factors[0].set (new goFGNodeFactor<goSize_t,goFloat> (1));

    vars[0]->value = 0;
    vars[1]->value = 1;
    vars[2]->value = 2;
    factors[0]->value = 3;

    fg.connect (vars[0], 0, vars[1], 0);
    fg.connect (vars[1], 1, vars[2], 0);
    fg.connect (factors[0], 0, vars[0], 1);
    
    //nodelist(0)->elem->adj.append (nodelist(1)->elem);
    //nodelist(1)->elem->adj.append (nodelist(0)->elem);
    //nodelist(1)->elem->adj.append (nodelist(2)->elem);
    //nodelist(2)->elem->adj.append (nodelist(1)->elem);
    //nodelist(3)->elem->adj.append (nodelist(0)->elem);
    //nodelist(0)->elem->adj.append (nodelist(3)->elem);
    
    printf ("First test graph:\n");
    sp.setValueCount (10);
    sp.run (fg.myVariables[0], fg);
   
    printf ("\nSecond test graph:\n");
    //= Build a second graph to test:
    {
        vars.setSize (11);
        factors.setSize (0);
        for (goSize_t i = 0; i < 11; ++i)
        {
            vars[i].set (new goFGNodeVariable<goSize_t,goFloat> (0));
            vars[i]->value = i + 1;
        }

        //= TODO: Anzahl edges setzen und testen.
        vars[0]->adj.setSize (1);
        vars[1]->adj.setSize (3);
        vars[2]->adj.setSize (4);
        vars[3]->adj.setSize (1);
        vars[4]->adj.setSize (2);
        vars[5]->adj.setSize (1);
        vars[6]->adj.setSize (2);
        vars[7]->adj.setSize (1);
        vars[8]->adj.setSize (2);
        vars[9]->adj.setSize (2);
        vars[10]->adj.setSize (1);
        
        fg.connect (vars[0], 0, vars[1], 0);
        fg.connect (vars[1], 1, vars[8], 0);
        fg.connect (vars[1], 2, vars[2], 0);
        fg.connect (vars[2], 1, vars[3], 0);
        fg.connect (vars[2], 2, vars[4], 0);
        fg.connect (vars[2], 3, vars[5], 0);
        fg.connect (vars[4], 1, vars[6], 0);
        fg.connect (vars[6], 1, vars[7], 0);
        fg.connect (vars[8], 1, vars[9], 0);
        fg.connect (vars[9], 1, vars[10], 0);
        
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
        
        sp.run (fg.myVariables[0], fg);
    }

#if 1
    {
        goFactorGraph<goSize_t, float> fg;
        goFactorGraph<goSize_t,goFloat>::VariableArray& vars = fg.myVariables;
        goFactorGraph<goSize_t,goFloat>::FactorArray&   factors = fg.myFactors;

        vars.setSize (4);
        vars[0].set (new goFGNodeVariable<goSize_t,float> (2));
        vars[1].set (new goFGNodeVariable<goSize_t,float> (2));
        vars[2].set (new goFGNodeVariable<goSize_t,float> (3));
        vars[3].set (new goFGNodeVariable<goSize_t,float> (1));

        factors.setSize (4);
        factors[0].set (new MyFactor(2));
        factors[1].set (new MyFactor(2));
        factors[2].set (new MyFactor(2));

        fg.connect (vars[0], 0, factors[0], 0);
        fg.connect (vars[0], 1, factors[1], 0);
        fg.connect (vars[1], 0, factors[0], 1);
        fg.connect (vars[2], 0, factors[1], 1);
        fg.connect (vars[2], 1, factors[2], 0);
        fg.connect (vars[3], 0, factors[2], 1);

        //= Add a loop and see what happens ...
        factors[3].set (new MyFactor(2));
        fg.connect (vars[1], 1, factors[3], 0);
        fg.connect (vars[2], 2, factors[3], 1);

        // goSumProduct<goSize_t,float> sp;
        goMaxSum<goSize_t,float> ms;
        ms.setValueCount (10);
        ms.run (fg.myVariables[0], fg);

        printf ("Values: ");
        for (goSize_t i = 0; i < fg.myVariables.getSize(); ++i)
        {
            printf ("%d ", fg.myVariables[i]->value);
        }
        printf ("\n");
    }
#endif
    
    //= Make one factor graph just for drawing:
    {
        goFactorGraph<goSize_t,float> fg;
        fg.myVariables.setSize (3);
        fg.myFactors.setSize (3);
        fg.myVariables[0].set (new goFGNodeVariable<goSize_t,float> (3));
        fg.myVariables[1].set (new goFGNodeVariable<goSize_t,float> (1));
        fg.myVariables[2].set (new goFGNodeVariable<goSize_t,float> (2));
        fg.myFactors[0].set (new goFGNodeFactor<goSize_t,float> (1));
        fg.myFactors[1].set (new goFGNodeFactor<goSize_t,float> (3));
        fg.myFactors[2].set (new goFGNodeFactor<goSize_t,float> (2));

        fg.connect (fg.myVariables[0], 0, fg.myFactors[0], 0);
        fg.connect (fg.myVariables[0], 1, fg.myFactors[1], 0);
        fg.connect (fg.myVariables[1], 0, fg.myFactors[1], 1);
        fg.connect (fg.myVariables[2], 0, fg.myFactors[1], 2);
        fg.connect (fg.myVariables[2], 1, fg.myFactors[2], 0);
        fg.connect (fg.myVariables[0], 2, fg.myFactors[2], 1);

        FILE* f = fopen ("fg.dot","w");
        if (!f)
        {
            printf ("Could not open fg.dot for writing.\n");
            exit(0);
        }
        goFGGraphWriteDOT<goSize_t,float> (fg.myVariables[0], f);
        fclose (f);
    }

    //= Make a circular factor graph just for drawing:
    {
        goFactorGraph<goSize_t,float> fg;
        fg.myVariables.setSize (5);
        fg.myFactors.setSize (10);
        for (goSize_t i = 0; i < 5; ++i)
        {
            fg.myVariables[i].set (new goFGNodeVariable<goSize_t,float> (3));
        }
        for (goSize_t i = 0; i < 5; ++i)
        {
            fg.myFactors[i].set (new goFGNodeFactor<goSize_t,float> (2));
            fg.connect (fg.myFactors[i], 0, fg.myVariables[i], 0);
            fg.connect (fg.myFactors[i], 1, fg.myVariables[(i+1) % 5], 1);
        }
        for (goSize_t i = 5; i < 10; ++i)
        {
            fg.myFactors[i].set (new goFGNodeFactor<goSize_t,float> (1));
            fg.connect (fg.myFactors[i], 0, fg.myVariables[i-5], 2);
        }

        FILE* f = fopen ("fg2.dot","w");
        if (!f)
        {
            printf ("Could not open fg2.dot for writing.\n");
            exit(0);
        }
        goFGGraphWriteDOT<goSize_t,float> (fg.myVariables[0], f);
        fclose (f);
    }

    exit(1);
}
