/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: gomatlab.cpp,v 1.1.1.1 2006/04/19 15:27:15 gosch Exp $
 */
#include <stdio.h>

// Matlab
#include <engine.h>
#include <gomatlab.h>

using namespace std;

class goMatlabPrivate
{
    public:
        goMatlabPrivate ();
        ~goMatlabPrivate ();

        Engine*  matlabEngine;
};

goMatlabPrivate::goMatlabPrivate ()
    : matlabEngine (NULL)
{
}

goMatlabPrivate::~goMatlabPrivate ()
{
}

bool goMatlab::putSignal(goSignal3DBase<void>* sig, const char* name)
{
    return this->signalToVariable (sig, name);
}

bool goMatlab::getSignal(goSignal3D<void>* sig, const char* name)
{
    return this->variableToSignal (sig, name);
}

bool goMatlab::putArray(const goDouble* p, goSize_t length, const char* name)
{
    return this->arrayToVariable (p, length, name);
}

bool goMatlab::putArray(const goArray<goDouble>* array, const char* name)
{
    this->arrayToVariable (array, name);
}

bool goMatlab::getArray(goArray<goDouble>* array, const char* name)
{
    this->variableToArray (array, name);
}

bool goMatlab::putVector(const goVectord* vec, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    mxArray* temp = matlabCreateMatrix (vec->getSize(), 1);
    if (!temp)
    {
        return false;
    }
    if (!copyToMatlab (vec, temp))
    {
        return false;
    }
    engPutVariable (myPrivate->matlabEngine, name, temp);
    mxDestroyArray (temp);
    return true;
}

bool goMatlab::getVector(goVectord* vec, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    mxArray* temp = engGetVariable (myPrivate->matlabEngine, name);
    if (!temp)
    {
        return false;
    }
    if (mxIsSparse (temp))
    {
        return false;
    }
    if (!mxIsDouble (temp))
    {
        return false;
    }
    if (vec->getSize() != (goSize_t)mxGetN(temp) * (goSize_t)mxGetM(temp))
    {
        vec->resize (mxGetN(temp) * mxGetM(temp));
    }
    if (!copyFromMatlab (temp, vec))
    {
        return false;
    }
    mxDestroyArray (temp);
    return true;
}

bool goMatlab::putDouble (goDouble d, const char* name)
{
    return this->doubleToVariable (d, name);
}

bool goMatlab::getDouble (goDouble& d, const char* name)
{
    return this->variableToDouble (d, name);
}

bool goMatlab::putSparse (goSparseMatrix* sm, const char* name)
{
    return this->sparseToMatlabSparse (sm, name);
}

/** 
 * @brief Put 2d points from list to matlab matrix.
 *
 * The x and y coordinates of the points are stored in a matlab matrix
 * of size 2xN, where N is the number of points. The first row contains the x
 * coordinates, the second row contains the y coordinates of the points.
 * 
 * @see goMatlab::get2DPoints()
 * 
 * @param l List of points (only x and y coordinates are used).
 * @param variableName Name of matlab matrix.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::put2DPoints (const goList<goPointf>& l, const char* variableName)
{
    assert(this->getEngine());
    mxArray* array = mxCreateDoubleMatrix(2,l.getSize(),mxREAL);
    goIndex_t sz = 2 * static_cast<goIndex_t>(l.getSize());
    goIndex_t i;
    double* a = mxGetPr(array);
    goList<goPointf>::ConstElement* el = l.getFrontElement();
    for (i = 0; i < sz && el; i+=2)
    {
        a[i] = el->elem.x;
        a[i+1] = el->elem.y;
        el = el->next;
    }
    if (engPutVariable(this->getEngine(),variableName,array) != 0)
    {
        mxDestroyArray(array);
        return false;
    }
    mxDestroyArray(array);
    return true;
}

/** 
 * @brief Get a matrix of 2d points from matlab into a list.
 * 
 * Stores the points from a matrix that looks like described in the documentation
 * of goMatlab::put2DPoints() into a list.
 * 
 * @see goMatlab::put2DPoints()
 * 
 * @param l The list.
 * @param variableName The matlab variable name of the matrix containing the 
 *                     coordinates.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::get2DPoints (goList<goPointf>& l, const char* variableName)
{
    assert (this->getEngine());
    mxArray* temp = engGetVariable (this->getEngine(), variableName);
    assert (temp);
    if (!temp)
        return false;
    if (mxGetM(temp) != 2)
    {
        mxDestroyArray(temp);
        return false;
    }
    goIndex_t sz = (goIndex_t)mxGetN(temp);
    goIndex_t i;
    double* ptr = mxGetPr(temp);
    assert(ptr);
    for (i = 0; i < sz; ++i, ptr+=2)
    {
        l.append(goPointf(*ptr,*(ptr+1)));
    }
    assert (l.getSize() == sz);
    mxDestroyArray(temp);
    return true;
}

bool goMatlab::put2DPoints (goList<goPointf>::ConstElement* begin, 
                            goIndex_t size, 
                            const char* variableName)
{
    assert(this->getEngine());
    mxArray* array = mxCreateDoubleMatrix(2,size,mxREAL);
    goIndex_t sz = 2 * size;
    goIndex_t i;
    double* a = mxGetPr(array);
    goList<goPointf>::ConstElement* el = begin;
    for (i = 0; i < sz && el; i+=2)
    {
        a[i] = el->elem.x;
        a[i+1] = el->elem.y;
        el = el->next;
    }
    if (engPutVariable(this->getEngine(),variableName,array) != 0)
    {
        mxDestroyArray(array);
        return false;
    }
    mxDestroyArray(array);
    return true;
}
        
bool goMatlab::stopEngine ()
{
    if (myPrivate->matlabEngine)
    {
        engClose (myPrivate->matlabEngine);
        myPrivate->matlabEngine = NULL;
    }
    return true;
}

Engine* goMatlab::getEngine ()
{
    return myPrivate->matlabEngine;
}

const Engine* goMatlab::getEngine () const
{
    return myPrivate->matlabEngine;
}

bool goMatlab::startEngine ()
{
    if (myPrivate->matlabEngine)
        this->stopEngine();
    myPrivate->matlabEngine = engOpen ("matlab -nodesktop -nosplash -nojvm");
    if (!myPrivate->matlabEngine)
    {
        printf ("Could not open matlab engine.\n");
        return false;
    }
    return true;
}

bool 
goMatlab::sparseToMatlabSparse (goSparseMatrix* sp, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    mxArray* temp = matlabCreateSparse (sp->getRowCount(), sp->getColumnCount(), sp->getElementCount());
    if (!temp)
    {
        return false;
    }
    if (!copyToMatlab (sp, temp))
    {
        return false;
    }
    engPutVariable (myPrivate->matlabEngine, name, temp);
    mxDestroyArray (temp);
    return true;
}

bool 
goMatlab::signalToVariable (goSignal3DBase<void>* sig, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    mxArray* temp = matlabCreateMatrix (sig->getSizeY(), sig->getSizeX());
    if (!temp)
    {
        return false;
    }
    if (!copyToMatlab (sig, temp))
    {
        return false;
    }
    engPutVariable (myPrivate->matlabEngine, name, temp);
    mxDestroyArray (temp);
    return true;
}

bool
goMatlab::arrayToVariable (const goArray<goDouble>* array, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    mxArray* temp = matlabCreateMatrix (array->getSize(), 1);
    if (!temp)
    {
        return false;
    }
    if (!copyToMatlab (array, temp))
    {
        return false;
    }
    engPutVariable (myPrivate->matlabEngine, name, temp);
    mxDestroyArray (temp);
    return true;
}

bool
goMatlab::arrayToVariable (const goDouble* array, goSize_t length, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    mxArray* temp = matlabCreateMatrix (length, 1);
    if (!temp)
    {
        return false;
    }
    if (!copyToMatlab (array, length, temp))
    {
        return false;
    }
    engPutVariable (myPrivate->matlabEngine, name, temp);
    mxDestroyArray (temp);
    return true;
}

bool 
goMatlab::doubleToVariable (goDouble d, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    mxArray* temp = matlabCreateMatrix (1, 1);
    if (!temp)
    {
        return false;
    }
    *mxGetPr(temp) = d;
    engPutVariable (myPrivate->matlabEngine, name, temp);
    mxDestroyArray (temp);
    return true;
}

bool 
goMatlab::variableToSignal (goSignal3D<void>* sig, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    mxArray* temp = engGetVariable (myPrivate->matlabEngine, name);
    if (!temp)
    {
        return false;
    }
    if (mxIsSparse (temp))
    {
        return false;
    }
    if (!mxIsDouble (temp))
    {
        return false;
    }
    if (sig->getDataType().getID() != GO_FLOAT ||
        sig->getSizeX() != (goSize_t)mxGetN(temp) ||
        sig->getSizeZ() != (goSize_t)mxGetM(temp))
    {
        sig->setDataType (GO_FLOAT);
        sig->make (mxGetN(temp), mxGetM(temp), 1, 32, 32, 1, 16, 16, 0);
    }
    if (!copyFromMatlab (temp, sig))
    {
        return false;
    }
    mxDestroyArray (temp);
    return true;
}

bool
goMatlab::variableToArray (goArray<goDouble>* vector, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    mxArray* temp = engGetVariable (myPrivate->matlabEngine, name);
    if (!temp)
    {
        return false;
    }
    if (mxIsSparse (temp))
    {
        return false;
    }
    if (!mxIsDouble (temp))
    {
        return false;
    }
    if (vector->getSize() != (goSize_t)mxGetN(temp) * (goSize_t)mxGetM(temp))
    {
        vector->resize (mxGetN(temp) * mxGetM(temp));
    }
    if (!copyFromMatlab (temp, vector))
    {
        return false;
    }
    mxDestroyArray (temp);
    return true;
}

bool 
goMatlab::variableToDouble (goDouble& d, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    mxArray* temp = engGetVariable (myPrivate->matlabEngine, name);
    if (!temp)
    {
        return false;
    }
    if (mxIsSparse (temp))
    {
        return false;
    }
    if (!mxIsDouble (temp))
    {
        return false;
    }
    if (mxGetM(temp) != 1 || mxGetN(temp) != 1)
    {
        return false;
    }
    d = *mxGetPr(temp);
    mxDestroyArray (temp);
    return true;
}

bool 
goMatlab::variableToSignal (goSignal3DBase<void>* sig, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    mxArray* temp = engGetVariable (myPrivate->matlabEngine, name);
    if (!temp)
    {
        return false;
    }
    if (mxIsSparse (temp))
    {
        return false;
    }
    if (!mxIsDouble (temp))
    {
        return false;
    }
    if (sig->getDataType().getID() != GO_FLOAT ||
        sig->getSizeX() != (goSize_t)mxGetN(temp) ||
        sig->getSizeZ() != (goSize_t)mxGetM(temp))
    {
        return false;
    }
    if (!copyFromMatlab (temp, sig))
    {
        return false;
    }
    mxDestroyArray (temp);
    return true;
}

mxArray* goMatlab::matlabCreateSparse (int rows, int columns, int elements)
{
    mxArray* m = mxCreateSparse (rows, columns, elements, mxREAL);
    return m;
}

mxArray*
goMatlab::matlabCreateMatrix (int rows, int columns)
{
    mxArray* m = mxCreateDoubleMatrix (rows, columns, mxREAL);
    return m;
}


bool
goMatlab::copyToMatlab (goSignal3DBase<void>* sig, mxArray* m)
{
    if ((int)sig->getSizeX() != mxGetN(m) || (int)sig->getSizeY() != mxGetM(m))
    {
        printf ("Signal and matrix are not of the same size\n");
        return false;
    }
    double* mPtr = mxGetPr (m);
    if (!mPtr)
    {
        return false;
    }
    sig->swapXY();
    switch (sig->getDataType().getID())
    {
        case GO_FLOAT: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(mPtr++) = *(goFloat*)__ptr, (*sig)); break;
        case GO_DOUBLE: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(mPtr++) = *(goDouble*)__ptr, (*sig)); break;
    }
    sig->swapXY();
    return true;
}

bool
goMatlab::copyToMatlab (const goArray<goDouble>* array, mxArray* m)
{
    assert (sizeof(double) == sizeof(goDouble));
    if ((int)array->getSize() != mxGetN(m) * mxGetM(m))
    {
        printf ("Signal and matrix are not of the same size\n");
        return false;
    }
    double* mPtr = mxGetPr (m);
    if (!mPtr)
    {
        return false;
    }
    memcpy (mPtr, array->getPtr(), sizeof(double) * array->getSize());
    return true;
}

bool
goMatlab::copyToMatlab (const goVectord* vec, mxArray* m)
{
    assert (sizeof(double) == sizeof(goDouble));
    if ((int)vec->getSize() != mxGetN(m) * mxGetM(m))
    {
        printf ("Signal and matrix are not of the same size\n");
        return false;
    }
    double* mPtr = mxGetPr (m);
    if (!mPtr)
    {
        return false;
    }
    memcpy (mPtr, vec->getPtr(), sizeof(double) * vec->getSize());
    return true;
}

bool
goMatlab::copyToMatlab (const goDouble* array, goSize_t length, mxArray* m)
{
    assert (sizeof(double) == sizeof(goDouble));
    if ((int)length != mxGetN(m) * mxGetM(m))
    {
        goLog::warning ("Array and matrix are not of the same size\n",this);
        return false;
    }
    double* mPtr = mxGetPr (m);
    if (!mPtr)
    {
        return false;
    }
    memcpy (mPtr, array, sizeof(double) * length);
    return true;
}

bool     
goMatlab::copyToMatlab (goSparseMatrix* sp, mxArray* m)
{
    assert (sizeof(double) == sizeof(goDouble));
    assert (sp);
    assert (m);
    if (sp->getColumnCount() != mxGetM(m) || sp->getRowCount() != mxGetM(m))
    {
        printf ("Signal and matrix are not of the same size\n");
        return false;
    }
    return goGetMatlabSparse (m, *sp); // sp->getMatlabSparse (m);
}


bool
goMatlab::copyFromMatlab (mxArray* m, goSignal3DBase<void>* sig)
{
    if ((int)sig->getSizeX() != mxGetN(m) || (int)sig->getSizeY() != mxGetM(m))
    {
        printf ("Signal and matrix are not of the same size\n");
        return false;
    }
    const double* mPtr = mxGetPr (m);
    if (!mPtr)
    {
        return false;
    }
    sig->swapXY();
    switch (sig->getDataType().getID())
    {
        case GO_FLOAT: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(goFloat*)__ptr = *(mPtr++), (*sig)); break;
        case GO_DOUBLE: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(goDouble*)__ptr = *(mPtr++), (*sig)); break;
        default: sig->swapXY(); return false; break;
    }
    sig->swapXY();
    return true;
}

bool
goMatlab::copyFromMatlab (mxArray* m, goArray<goDouble>* array)
{
    assert (sizeof(double) == sizeof(goDouble));
    if ((int)array->getSize() != mxGetN(m) * mxGetM(m))
    {
        printf ("Signal and array are not of the same size\n");
        return false;
    }
    const double* mPtr = mxGetPr (m);
    if (!mPtr)
    {
        return false;
    }
    memcpy (array->getPtr(), mPtr, sizeof(double) * array->getSize());
    return true;
}

bool
goMatlab::copyFromMatlab (mxArray* m, goVectord* vec)
{
    assert (sizeof(double) == sizeof(goDouble));
    if ((int)vec->getSize() != mxGetN(m) * mxGetM(m))
    {
        printf ("Signal and array are not of the same size\n");
        return false;
    }
    const double* mPtr = mxGetPr (m);
    if (!mPtr)
    {
        return false;
    }
    memcpy (vec->getPtr(), mPtr, sizeof(double) * vec->getSize());
    return true;
}

bool
goMatlab::matlabCall (const char* command, goString* resultBuffer)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }
    if (resultBuffer)
    {
        resultBuffer->fill (0);
        engOutputBuffer (myPrivate->matlabEngine, resultBuffer->getPtr(), resultBuffer->getSize());
        if (engEvalString (myPrivate->matlabEngine, command) != 0)
        {
            return false;
        }
        engOutputBuffer (myPrivate->matlabEngine, NULL, 0);
    }
    else
    {
        if (engEvalString (myPrivate->matlabEngine, command) != 0)
        {
            return false;
        }
    }
    return true;
}

// ===========================================================


goMatlab::goMatlab ()
 : goObjectBase (),
   myPrivate (NULL)
{
    this->setClassName ("goMatlab");
    myPrivate = new goMatlabPrivate;
    assert (myPrivate);

    this->startEngine();
}

goMatlab::~goMatlab ()
{
    if (myPrivate)
    {
        this->stopEngine();
        delete myPrivate;
        myPrivate = 0;
    }
}

bool goMatlab::callObjectMethod (int methodID, goObjectMethodParameters* param)
{
    switch (methodID)
    {
        case GO_MATLAB_GET_ENGINE:
            {
                if (!param)
                {
                    return false;
                }
                param->myVoidPointers.resize(1);
                param->myVoidPointers[0] = myPrivate->matlabEngine;
                return true;
            }
            break;
        case GO_MATLAB_PUT_SIGNAL:
            {
                if (!param || !myPrivate->matlabEngine)
                {
                    return false;
                }
                if (param->myVoidPointers.getSize() != 2)
                {
                    return false;
                }
                goSignal3DBase<void>* sig = reinterpret_cast<goSignal3DBase<void>*>(param->myVoidPointers[0]);
                const char* variableName  = reinterpret_cast<const char*>(param->myVoidPointers[1]);
                if (!sig || !variableName)
                {
                    return false;
                }
                if (!this->signalToVariable (sig, variableName))
                {
                    return false;
                }
                return true;
            }
            break;
        case GO_MATLAB_PUT_VECTOR:
            {
                if (!param || !myPrivate->matlabEngine)
                {
                    return false;
                }
                if (param->myVoidPointers.getSize() != 2)
                {
                    return false;
                }
                goArray<goDouble>* array = reinterpret_cast<goArray<goDouble>* >(param->myVoidPointers[0]);
                const char* variableName  = reinterpret_cast<const char*>(param->myVoidPointers[1]);
                if (!array || !variableName)
                {
                    return false;
                }
                if (!this->arrayToVariable (array, variableName))
                {
                    return false;
                }
                return true;
            }
            break;
        case GO_MATLAB_PUT_SPARSE:
            {
                if (!param || !myPrivate->matlabEngine)
                {
                    return false;
                }
                if (param->myVoidPointers.getSize() != 2)
                {
                    return false;
                }
                goSparseMatrix* matrix = reinterpret_cast<goSparseMatrix* >(param->myVoidPointers[0]);
                const char* variableName  = reinterpret_cast<const char*>(param->myVoidPointers[1]);
                if (!matrix || !variableName)
                {
                    return false;
                }
                if (!this->sparseToMatlabSparse (matrix, variableName))
                {
                    return false;
                }
                return true;
            }
            break;
        case GO_MATLAB_GET_SIGNAL:
            {
                if (!param || !myPrivate->matlabEngine)
                {
                    return false;
                }
                if (param->myVoidPointers.getSize() != 2)
                {
                    return false;
                }
                goSignal3D<void>* sig    = reinterpret_cast<goSignal3D<void>*>(param->myVoidPointers[0]);
                const char* variableName = reinterpret_cast<const char*>(param->myVoidPointers[1]);
                if (!sig || !variableName)
                {
                    return false;
                }
                if (!this->variableToSignal (sig, variableName))
                {
                    return false;
                }
                return true;
            }
            break;
        case GO_MATLAB_GET_VECTOR:
            {
                if (!param || !myPrivate->matlabEngine)
                {
                    return false;
                }
                if (param->myVoidPointers.getSize() != 2)
                {
                    return false;
                }
                goArray<goDouble>* array = reinterpret_cast<goArray<goDouble>* >(param->myVoidPointers[0]);
                const char* variableName = reinterpret_cast<const char*>(param->myVoidPointers[1]);
                if (!array || !variableName)
                {
                    return false;
                }
                if (!this->variableToArray (array, variableName))
                {
                    return false;
                }
                return true;
            }
            break;
        case GO_MATLAB_GET_SCALAR:
            {
                if (!param || !myPrivate->matlabEngine)
                {
                    return false;
                }
                if (param->myVoidPointers.getSize() != 1)
                {
                    return false;
                }
                const char* variableName = reinterpret_cast<const char*>(param->myVoidPointers[0]);
                if (!variableName)
                {
                    return false;
                }
                param->myDoubles.resize(1);
                if (!this->variableToDouble (param->myDoubles[0], variableName))
                {
                    return false;
                }
                return true;
            }
            break;
        case GO_MATLAB_PUT_SCALAR:
            {
                if (!param || !myPrivate->matlabEngine)
                {
                    return false;
                }
                if (param->myVoidPointers.getSize() != 1 ||
                    param->myDoubles.getSize() != 1)
                {
                    return false;
                }
                const char* variableName = reinterpret_cast<const char*>(param->myVoidPointers[0]);
                if (!variableName)
                {
                    return false;
                }
                if (!this->doubleToVariable (param->myDoubles[0], variableName))
                {
                    return false;
                }
                return true;
            }
            break;
        case GO_MATLAB_EVALUATE:
            {
                if (!param || !myPrivate->matlabEngine)
                {
                    return false;
                }
                if (param->myVoidPointers.getSize() < 1)
                {
                    return false;
                }
                const char* commandString = reinterpret_cast<const char*>(param->myVoidPointers[0]);
                if (!commandString)
                {
                    return false;
                }
                if (param->myVoidPointers.getSize() > 1)
                {
                    goString* resultBuffer = (goString*)param->myVoidPointers[1];
                    return this->matlabCall (commandString, resultBuffer);
                }
                return this->matlabCall (commandString);
            }
            break;
        default:
            break;
    }
    return goObjectBase::callObjectMethod (methodID, param);
}
