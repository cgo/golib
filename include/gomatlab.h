#ifndef GOMATLAB_H
#define GOMATLAB_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#include <gosignal3d.h>
#include <gosignalmacros.h>
#include <gosignalhelper.h>
#include <gostring.h>
#include <gosparsematrix.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif
#ifndef GOLIST_H
# include <golist.h>
#endif
#include <golog.h>

class goMatlabPrivate;

enum {
    /** @brief Get Matlab engine. 
     *
     * @return param->myVoidPointers[0]: Engine* to the engine. May be NULL.
     */
    GO_MATLAB_GET_ENGINE = GO_OBJECTMETHOD_MATLAB,
    /** @brief Put signal as a matrix with given variable name in the matlab engine.
     *
     * @param param->myVoidPointers[0]: goSignal3DBase<void>* to the signal.
     * @param param->myVoidPointers[1]: const char*, the variable name for Matlab.
     */
    GO_MATLAB_PUT_SIGNAL,
    /** @brief Put goArray<goDouble> as a matrix with given variable name in the matlab engine.
     *
     * @param param->myVoidPointers[0]: goArray<goDouble>* to the array/vector.
     * @param param->myVoidPointers[1]: const char*, the variable name for Matlab.
     */
    GO_MATLAB_PUT_VECTOR,
    /** @brief Put goSparseMatrix as a sparse matrix with given variable name in the matlab engine.
     *
     * @param param->myVoidPointers[0]: goSparseMatrix* to the sparse matrix.
     * @param param->myVoidPointers[1]: const char*, the variable name for Matlab.
     */
    GO_MATLAB_PUT_SPARSE,
    /** @brief Put a double as a 1x1 matrix with given variable name in the matlab engine.
     *
     * @param param->myVoidPointers[0]: const char*, the variable name for Matlab.
     * @param param->myDoubles[0]:      The double value.
     */
    GO_MATLAB_PUT_SCALAR,
    /** @brief Get a double from a 1x1 matrix with given variable name.
     *
     * @param param->myVoidPointers[0]: const char*, the variable name for Matlab.
     * @return param->myDoubles[0]:      The double value.
     */
    GO_MATLAB_GET_SCALAR,
    /** @brief Get signal from a matrix with given variable name in the matlab engine.
     *
     * @param param->myVoidPointers[0]: goSignal3D<void>* to the signal. This
     *      must be a goSignal3D<void>* because it will be resized as needed.
     * @param param->myVoidPointers[1]: const char*, the variable name of the Matlab matrix.
     */
    GO_MATLAB_GET_SIGNAL,
    /** @brief Get a goArray<goDouble> from a matrix with given variable name in the matlab engine.
     *
     * @param param->myVoidPointers[0]: goArray<goDouble>* to the array.
     * @param param->myVoidPointers[1]: const char*, the variable name of the Matlab matrix.
     */
    GO_MATLAB_GET_VECTOR,
    /** @brief Evaluate a Matlab command string.
     *
     * @param param->myVoidPointers[0]: const char*, the command string.
     * @param param->myVoidPointers[1]: _optional_: goString*, the command result (size must be set prior to call).
     */
    GO_MATLAB_EVALUATE
};

class goMatlab : public goObjectBase 
{
    public:
        goMatlab ();
        virtual ~goMatlab ();

        bool     putString   (const goString&, const char* name);
        bool     getString   (goString& str, const char* name); 
        bool     putRGBImage (const goSignal3DBase<void>* sig, const char* name);
        bool     putSignal (const goSignal3DBase<void>* sig, const char* name);
        bool     getSignal (goSignal3D<void>* sig, const char* name);
        bool     putArray  (const goDouble* p, goSize_t length, const char* name);
        bool     putArray  (const goArray<goDouble>* array, const char* name);
        bool     getArray  (goArray<goDouble>* array, const char* name);
        bool     putVector (const goVectord* vec, const char* name);
        bool     getVector (goVectord* vec, const char* name);
        bool     putVector (const goVectorf* vec, const char* name);
        bool     getVector (goVectorf* vec, const char* name);
        bool     putDouble (goDouble d, const char* name);
        bool     getDouble (goDouble& d, const char* name);
        bool     putSparse (goSparseMatrix* sm, const char* name);

        template <class T>
            bool     putMatrix (const goMatrix<T>& matrix, const char* name)
            {
                if (!this->getEngine())
                {
                    goLog::warning ("goMatlab::putMatrix(): No matlab engine.");
                    return false;
                }
                goIndex_t M = matrix.getRows();
                goIndex_t N = matrix.getColumns();
                mxArray* mMatrix = this->matlabCreateMatrix (M,N);
                if (!mMatrix)
                {
                    return false;
                }
                goIndex_t i;
                goIndex_t j;
                double* mP = mxGetPr (mMatrix);
                for (j = 0; j < N; ++j)
                {
                    for (i = 0; i < M; ++i)
                    {
                        *mP = matrix(i,j);
                        ++mP;
                    }
                }
                engPutVariable (this->getEngine(), name, mMatrix);
                mxDestroyArray (mMatrix);
                return true;
            };
        template <class T>
            bool getMatrix (goMatrix<T>& matrix, const char* name)
            {
                if (!this->getEngine())
                {
                    goLog::warning ("goMatlab::putMatrix(): No matlab engine.");
                    return false;
                }
                mxArray* temp = engGetVariable (this->getEngine(), name);
                if (!temp)
                {
                    return false;
                }
                if (!matrix.resize (mxGetM(temp), mxGetN(temp)))
                {
                    return false;
                }
                double* mP = mxGetPr (temp);
                goSize_t N = matrix.getColumns();
                goSize_t M = matrix.getRows();
                for (goSize_t j = 0; j < N; ++j)
                {
                    for (goSize_t i = 0; i < M; ++i)
                    {
                        matrix(i,j) = *mP;
                        ++mP;
                    }
                }
                return true;
            };

        bool     put2DPoints (const goList<goVectorf>& l, const char* variableName);
        bool     get2DPoints (goList<goVectorf>& l, const char* variableName);
        bool     put2DPoints (goList<goVectorf>::ConstElement* begin, 
                              goIndex_t size, 
                              const char* variableName);

        bool     startEngine          ();
        bool     stopEngine           ();
        Engine*  getEngine            ();
        const Engine*  getEngine      () const;
        mxArray* matlabCreateMatrix   (int rows, int columns);
        mxArray* matlabCreateSparse   (int rows, int columns, int elements);

        
        bool     copyToMatlab         (const goSignal3DBase<void>* sig, mxArray* matrix);
        bool     copyToMatlab         (const goArray<goDouble>* array, mxArray* matrix);
        bool     copyToMatlab         (const goVectord* array, mxArray* matrix);
        bool     copyToMatlab         (const goVectorf* array, mxArray* matrix);
        bool     copyToMatlab         (const goDouble* array, goSize_t length, mxArray* matrix);
        bool     copyToMatlab         (goSparseMatrix* sp, mxArray* matrix);
        bool     copyFromMatlab       (mxArray* matrix, goSignal3DBase<void>* sig);
        bool     copyFromMatlab       (mxArray* matrix, goArray<goDouble>* array);
        bool     copyFromMatlab       (mxArray* matrix, goVectord* array);
        bool     copyFromMatlab       (mxArray* matrix, goVectorf* array);
        bool     sparseToMatlabSparse (goSparseMatrix* sp, const char* name);
        bool     signalToVariable     (const goSignal3DBase<void>* sig, const char* name);
        bool     arrayToVariable      (const goArray<goDouble>* array, const char* name);
        bool     arrayToVariable      (const goDouble* array, goSize_t length, const char* name);
        bool     doubleToVariable     (goDouble d, const char* name);
        bool     variableToSignal     (goSignal3D<void>* sig, const char* name);
        bool     variableToSignal     (goSignal3DBase<void>* sig, const char* name);
        bool     variableToArray      (goArray<goDouble>* vector, const char* name);
        bool     variableToDouble     (goDouble& d, const char* name);
        bool     matlabCall           (const char* command, goString* resultBuffer = 0);
        bool     call                 (const char* command, goString* resultBuffer = 0);

        virtual bool callObjectMethod (int methodID, goObjectMethodParameters* param = NULL);

    private:
        goMatlabPrivate* myPrivate;
        
};

#endif
