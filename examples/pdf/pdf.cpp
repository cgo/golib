#include <gogausspdf.h>
#include <gotypes.h>
#include <gorandom.h>

#include <engine.h>
#include <gomatlab.h>

int main (int argc, char* argv[])
{
    {
        goMath::goMultiGaussPDF<goVectorf, goFloat> pdf;
        pdf.reset (2);
        for (goSize_t i = 0; i < 100; ++i)
        {
            goVectorf v (2);
            v[0] = goRandom() * 2;
            v[1] = goRandom() * 3;
            pdf.update (v);
        }
        pdf.updateFlush();
        printf ("Mean: ");
        for (goSize_t i = 0; i < pdf.getMean().getSize(); ++i)
        {
            printf ("%f ", pdf.getMean()[i]);
        }
        printf ("\n");
        printf ("Covariance: \n");
        pdf.getCovariance().print();
        printf ("Covariance^(-1): \n");
        pdf.getCovarianceInv().print();

        goMatrixf cov (2,2);
        cov.setIdentity();
        // pdf.setCovariance (cov);

        goVectorf v (2);
        FILE* f = fopen ("gauss.txt","w");
        for (v[0] = -2.0f + pdf.getMean()[0]; v[0] <= 2.0f + pdf.getMean()[0]; v[0] += 0.1f)
        {
            for (v[1] = -2.0f + pdf.getMean()[1]; v[1] <= 2.0f + pdf.getMean()[1]; v[1] += 0.1f)
            {
                fprintf (f, "%f %f %f\n", v[0], v[1], pdf (v));
            }
            fprintf (f, "\n");
        }
        fclose (f);
        
        {
            goMath::goMultiGaussPDF<goVectorf, goFloat> pdf;
            goMatlab matlab;
            matlab.matlabCall ("load workspace.mat");
            goVectord v;
            matlab.getVector (&v, "normrv");
            pdf.reset (1);
            for (goSize_t i = 0; i < v.getSize(); ++i)
            {
                goVectorf f (1);
                f[0] = v[i];
                pdf.update (f);
            }
            pdf.updateFlush ();
            printf ("Mean: %f\n", pdf.getMean()[0]);
            printf ("Covariance: \n");
            pdf.getCovariance().print();
            printf ("Covariance^(-1): \n");
            pdf.getCovarianceInv().print();
            {
                goVectorf v (1);
                FILE* f = fopen ("gauss1.txt","w");
                for (v[0] = -3.0f; v[0] <= 3.0f; v[0] += 0.1f)
                {
                    fprintf (f, "%f %f\n", v[0], pdf (v));
                }
                fclose (f);
            }
        }
        
        exit (1);
    }
    
    if (argc < 3)
    {
        printf ("Usage: %s <mean> <variance>\n", argv[0]);
        return 1;
    }
    goMath::goGaussPDF<goDouble, goDouble> pdf;

    pdf.setMean     (atof (argv[1]));
    pdf.setVariance (atof (argv[2]));

    goDouble x;
    for (x = -5.0; x < 5.0; x += 0.1)
      {
	printf ("%f\n", pdf(x));
      }
    return 0;
}
       
