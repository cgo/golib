#include <gogausspdf.h>
#include <gotypes.h>

int main (int argc, char* argv[])
{
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
       
