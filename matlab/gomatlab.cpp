/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * $Id: gomatlab.cpp,v 1.2 2006/04/25 17:01:53 gosch Exp $
 */
#include <stdio.h>

// Matlab
#include <engine.h>
#include <goconfig.h>
#include <gomatlab.h>

#include <gosignal3dgenericiterator.h>

#ifndef HAVE_MATLAB_GEQ_2007A
# define mwIndex int
# define mwSize int
#endif

using namespace std;

/** @addtogroup matlab
 * @{
 */

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

        
/*
 * @brief Copy sig to buffer at mp.
 * 
 * @param sig 
 * @param mp 
 * 
 * @return True.
 */
template <class T>
static bool putRGBImage1 (goSignal3DBase<void>* sig, unsigned char* mp)
{
    // unsigned char* bp = buffer;
    int X = sig->getSizeX ();
    int Y = sig->getSizeY ();
    int Z = goMath::min<int> (3, sig->getChannelCount());
    int Zmax = Z;
    int x;
    int y;
    int z;

    goSize_t oldChan = sig->getChannel ();

    // for (z = Z-1; z >= 0; --z)
    for (z = 0; z < Zmax; ++z)
    {
        sig->setChannel(z);
        // bp = buffer + z;
        goSignal3DGenericConstIterator it (sig);
        unsigned char* mpy = mp;
        for (y = 0; y < Y && !it.endY(); ++y)
        {
            unsigned char* mpx = mpy;
            it.resetX ();
            for (x = 0; x < X && !it.endX(); ++x)
            {
                *mpx = (unsigned char)*(T*)*it;
                mpx += Y; // += Y; // dims[0];
                it.incrementX ();
            }
            ++mpy; // ++mpy;
            it.incrementY();
        }
        mp += X * Y;
    }
    sig->setChannel (oldChan);

    return true;
}

/** 
 * @brief Put a string to the matlab engine.
 * 
 * @param str  String to put.
 * @param name Name of the matlab variable.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::putString (const goString& str, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }

    mxArray* mstring = mxCreateString (str.toCharPtr());
    if (!mstring)
        return false;

    engPutVariable (myPrivate->matlabEngine, name, mstring);
    return true;
}

/** 
 * @brief Get a string variable from matlab.
 * 
 * @param str   Holds the string on successful return.
 * @param name  Name of the matlab variable.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::getString (goString& str, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }

    mxArray* tempArray = engGetVariable (myPrivate->matlabEngine, name);
    if (!tempArray)
        return false;
    
    if (mxGetClassID(tempArray) != mxCHAR_CLASS)
        return false;
    
    char* temp = mxArrayToString (tempArray);
    if (temp)
    {
        str = temp;
        ::free (temp);
        temp = 0;
    }
    return true;
}

/** 
 * @brief Puts a multichannel signal as RGB image to matlab.
 *
 * The image created in matlab is of type mxUINT8_CLASS and has
 * 3 channels (R,G,B). The data type of sig does not matter,
 * the data is simply cast to goUInt8. No quantisation is done.
 * 
 * @param sig   goSignal3DBase to put into matlab (multi-channel)
 * @param name  Name of the matlab variable.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::putRGBImage (const goSignal3DBase<void>* sig, const char* name)
{
    if (!myPrivate->matlabEngine)
    {
        return false;
    }

    //int dimsMatlab[3];
    mwSize dimsMatlab[3];
    dimsMatlab[0] = sig->getSizeY();
    dimsMatlab[1] = sig->getSizeX();
    dimsMatlab[2] = 3;
    mxArray* image = mxCreateNumericArray(3, dimsMatlab, mxUINT8_CLASS, mxREAL);
    if (!image)
    {
        goLog::error ("goMatlab::putRGBImage: Matlab could not allocate the image.");
        mexErrMsgTxt ("goMatlab::putRGBImage: Could not allocate image.");
    }

    bool ok = true;
    {
        unsigned char* mp = (unsigned char*)mxGetPr(image);
        goSignal3DBase<void>* sig2 = const_cast<goSignal3DBase<void>*> (sig);
        switch (sig2->getDataType().getID())
        {
            case GO_UINT8: ok = putRGBImage1<goUInt8> (sig2, mp); break;
            case GO_INT8: ok = putRGBImage1<goInt8> (sig2, mp); break;
            case GO_UINT16: ok = putRGBImage1<goUInt16> (sig2, mp); break;
            case GO_INT16: ok = putRGBImage1<goInt16> (sig2, mp); break;
            case GO_UINT32: ok = putRGBImage1<goUInt32> (sig2, mp); break;
            case GO_INT32: ok = putRGBImage1<goInt32> (sig2, mp); break;
            case GO_FLOAT: ok = putRGBImage1<goFloat> (sig2, mp); break;
            case GO_DOUBLE: ok = putRGBImage1<goDouble> (sig2, mp); break;
            default: goLog::error ("goMatlab::putRGBImage: unknown data type."); ok = false; break;
        }
    } 
    engPutVariable (myPrivate->matlabEngine, name, image);
    mxDestroyArray (image);
    return ok;
}

/** 
 * @brief Put a single channel of \c sig as matrix to matlab.
 * 
 * @param sig  Signal to put to matlab.
 * @param name Variable name in matlab.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::putSignal(const goSignal3DBase<void>* sig, const char* name)
{
    return this->signalToVariable (sig, name);
}

/** 
 * @brief Get a matlab matrix as goSignal3D.
 * 
 * @param sig   Pointer to a goSignal3D<void> that holds the data on successful return.
 * If the size of sig does not match the matrix, or if its type is not GO_FLOAT,
 * the type is set to GO_FLOAT and it is resized.
 * @param name  Name of the matlab variable.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::getSignal(goSignal3D<void>* sig, const char* name)
{
    return this->variableToSignal (sig, name);
}

/** 
 * @brief Put a C array to matlab.
 * 
 * @param p       Pointer to first element.
 * @param length  Length of the array (number of elements).
 * @param name    Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::putArray(const goDouble* p, goSize_t length, const char* name)
{
    return this->arrayToVariable (p, length, name);
}

/** 
 * @brief Put a resizable array to matlab.
 * 
 * @param array Array to put.
 * @param name  Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::putArray(const goArray<goDouble>* array, const char* name)
{
    return this->arrayToVariable (array, name);
}

/** 
 * @brief Get array from matlab.
 * 
 * @param array Holds the array on successful return.
 * @param name  Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::getArray(goArray<goDouble>* array, const char* name)
{
    return this->variableToArray (array, name);
}

/** 
 * @brief Put a goVector to matlab.
 * 
 * @param vec  Vector to put.
 * @param name Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
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

/** 
 * @brief Put a goVector to matlab.
 * 
 * @param vec  Vector to put.
 * @param name Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::putVector(const goVectorf* vec, const char* name)
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

/** 
 * @brief Get a vector from matlab into a goVector.
 * 
 * @param vec  goVector that holds the data on successful return.
 * @param name Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
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

/** 
 * @brief Get a vector from matlab into a goVector.
 * 
 * @param vec  goVector that holds the data on successful return.
 * @param name Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::getVector(goVectorf* vec, const char* name)
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

/** 
 * @brief Put a double value to matlab.
 * 
 * @param d     Value to put.
 * @param name  Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::putDouble (goDouble d, const char* name)
{
    return this->doubleToVariable (d, name);
}

/** 
 * @brief Get a double from matlab.
 * 
 * @param d     Variable to hold the result.
 * @param name  Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::getDouble (goDouble& d, const char* name)
{
    return this->variableToDouble (d, name);
}

/** 
 * @brief Put a sparse matrix stored in a goSparseMatrix.
 *
 * goSparseMatrix objects can be used to assemble a sparse matrix quicker than
 * in matlab itself. Its computational possibilities are limited.
 * 
 * @param sm   Sparse matrix to put.
 * @param name Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::putSparse (goSparseMatrix* sm, const char* name)
{
    return this->sparseToMatlabSparse (sm, name);
}

template <class T>
static bool put_matrix (goMatlab& m, const goMatrix<T>& matrix, const char* name)
{
    if (!m.getEngine())
    {
        goLog::warning ("goMatlab::putMatrix(): No matlab engine.");
        return false;
    }
    goIndex_t M = matrix.getRows();
    goIndex_t N = matrix.getColumns();
    mxArray* mMatrix = m.matlabCreateMatrix (M,N);
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
    engPutVariable (m.getEngine(), name, mMatrix);
    mxDestroyArray (mMatrix);
    return true;
}

template <class T>
static bool get_matrix (goMatlab& m, goMatrix<T>& matrix, const char* name)
{
    if (!m.getEngine())
    {
        goLog::warning ("goMatlab::getMatrix(): No matlab engine.");
        return false;
    }
    mxArray* temp = engGetVariable (m.getEngine(), name);
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
}

/** 
 * @brief Put a goMatrix to matlab.
 * 
 * @param matrix Matrix to put.
 * @param name   Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::putMatrix (const goMatrixd& matrix, const char *name)
{
    return put_matrix<goDouble> (*this, matrix, name);
}

/** 
 * @brief Get a matrix from matlab into a goMatrix object.
 * 
 * @param matrix Holds the data upon successful return.
 * @param name Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::getMatrix (goMatrixd& matrix, const char* name)
{
    return get_matrix<goDouble> (*this, matrix, name);
}

/** 
 * @brief Put a goMatrix to matlab.
 * 
 * @param matrix Matrix to put.
 * @param name   Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::putMatrix (const goMatrixf& matrix, const char *name)
{
    return put_matrix<goFloat> (*this, matrix, name);
}

/** 
 * @brief Get a matrix from matlab into a goMatrix object.
 * 
 * @param matrix Holds the data upon successful return.
 * @param name Matlab variable name.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::getMatrix (goMatrixf& matrix, const char* name)
{
    return get_matrix<goFloat> (*this, matrix, name);
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
bool goMatlab::put2DPoints (const goList<goVectorf>& l, const char* variableName)
{
    assert(this->getEngine());
    mxArray* array = mxCreateDoubleMatrix(2,l.getSize(),mxREAL);
    goIndex_t sz = 2 * static_cast<goIndex_t>(l.getSize());
    goIndex_t i;
    double* a = mxGetPr(array);
    goList<goVectorf>::ConstElement* el = l.getFrontElement();
    for (i = 0; i < sz && el; i+=2)
    {
        a[i] = el->elem[0];
        a[i+1] = el->elem[1];
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
bool goMatlab::put2DPoints (const goList<goVectord>& l, const char* variableName)
{
    assert(this->getEngine());
    mxArray* array = mxCreateDoubleMatrix(2,l.getSize(),mxREAL);
    goIndex_t sz = 2 * static_cast<goIndex_t>(l.getSize());
    goIndex_t i;
    double* a = mxGetPr(array);
    goList<goVectord>::ConstElement* el = l.getFrontElement();
    for (i = 0; i < sz && el; i+=2)
    {
        a[i] = el->elem[0];
        a[i+1] = el->elem[1];
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
bool goMatlab::get2DPoints (goList<goVectorf>& l, const char* variableName)
{
    assert (this->getEngine());
    mxArray* temp = engGetVariable (this->getEngine(), variableName);
    if (!temp)
    {
        goString msg = "get2DPoints(): Variable ";
        msg += variableName;
        msg += " does not exist.";
        goLog::warning(msg,this);
        return false;
    }
    assert (temp);
    if (mxGetM(temp) != 2)
    {
        mxDestroyArray(temp);
        return false;
    }
    goIndex_t sz = (goIndex_t)mxGetN(temp);
    goIndex_t i;
    double* ptr = mxGetPr(temp);
    assert(ptr);
    goVectorf tempv (2);
    for (i = 0; i < sz; ++i, ptr+=2)
    {
        tempv[0] = *ptr;
        tempv[1] = *(ptr + 1);
        l.append(tempv);
    }
    assert (l.getSize() == sz);
    mxDestroyArray(temp);
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
bool goMatlab::get2DPoints (goList<goVectord>& l, const char* variableName)
{
    assert (this->getEngine());
    mxArray* temp = engGetVariable (this->getEngine(), variableName);
    if (!temp)
    {
        goString msg = "get2DPoints(): Variable ";
        msg += variableName;
        msg += " does not exist.";
        goLog::warning(msg,this);
        return false;
    }
    assert (temp);
    if (mxGetM(temp) != 2)
    {
        mxDestroyArray(temp);
        return false;
    }
    goIndex_t sz = (goIndex_t)mxGetN(temp);
    goIndex_t i;
    double* ptr = mxGetPr(temp);
    assert(ptr);
    goVectord tempv (2);
    for (i = 0; i < sz; ++i, ptr+=2)
    {
        tempv[0] = *ptr;
        tempv[1] = *(ptr + 1);
        l.append(tempv);
    }
    assert (l.getSize() == sz);
    mxDestroyArray(temp);
    return true;
}

bool goMatlab::put2DPoints (goList<goVectorf>::ConstElement* begin, 
                            goIndex_t size, 
                            const char* variableName)
{
    assert(this->getEngine());
    mxArray* array = mxCreateDoubleMatrix(2,size,mxREAL);
    goIndex_t sz = 2 * size;
    goIndex_t i;
    double* a = mxGetPr(array);
    goList<goVectorf>::ConstElement* el = begin;
    for (i = 0; i < sz && el; i+=2)
    {
        a[i] = el->elem[0];
        a[i+1] = el->elem[1];
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

bool goMatlab::put2DPoints (goList<goVectord>::ConstElement* begin, 
                            goIndex_t size, 
                            const char* variableName)
{
    assert(this->getEngine());
    mxArray* array = mxCreateDoubleMatrix(2,size,mxREAL);
    goIndex_t sz = 2 * size;
    goIndex_t i;
    double* a = mxGetPr(array);
    goList<goVectord>::ConstElement* el = begin;
    for (i = 0; i < sz && el; i+=2)
    {
        a[i] = el->elem[0];
        a[i+1] = el->elem[1];
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
 * @brief Stops the matlab engine.
 * 
 * @return True.
 */
bool goMatlab::stopEngine ()
{
    if (myPrivate->matlabEngine)
    {
        engClose (myPrivate->matlabEngine);
        myPrivate->matlabEngine = NULL;
    }
    return true;
}

/** 
 * @brief Get the matlab engine structure.
 * 
 * @return Pointer to the matlab \c Engine structure.
 */
Engine* goMatlab::getEngine ()
{
    return myPrivate->matlabEngine;
}

/** 
 * @brief Get the matlab engine structure.
 * 
 * @return Const pointer to the matlab \c Engine structure.
 */
const Engine* goMatlab::getEngine () const
{
    return myPrivate->matlabEngine;
}

/** 
 * @brief Starts a new matlab engine.
 * 
 * @return True if successful, false otherwise.
 */
bool goMatlab::startEngine ()
{
    if (myPrivate->matlabEngine)
        this->stopEngine();
    myPrivate->matlabEngine = engOpen ("matlab -nodesktop -nosplash");
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
goMatlab::signalToVariable (const goSignal3DBase<void>* sig, const char* name)
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
        sig->getSizeY() != (goSize_t)mxGetM(temp))
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
    if ((unsigned int)vector->getSize() != (goSize_t)mxGetN(temp) * (goSize_t)mxGetM(temp))
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
goMatlab::copyToMatlab (const goSignal3DBase<void>* sig, mxArray* m)
{
    if ((unsigned int)sig->getSizeX() != mxGetN(m) || (unsigned int)sig->getSizeY() != mxGetM(m))
    {
        goLog::warning("Signal and matrix are not of the same size\n",this);
        return false;
    }
    double* mPtr = mxGetPr (m);
    if (!mPtr)
    {
        return false;
    }
    //= This is NOT nice --- but we need to swap. I could also use a subsignal, 
    //= but that would be slower.
    const_cast<goSignal3DBase<void>*>(sig)->swapXY();
    switch (sig->getDataType().getID())
    {
        case GO_FLOAT: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(mPtr++) = *(const goFloat*)__ptr, (*sig)); break;
        case GO_DOUBLE: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(mPtr++) = *(const goDouble*)__ptr, (*sig)); break;
        case GO_INT8: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(mPtr++) = *(const goInt8*)__ptr, (*sig)); break;
        case GO_UINT8: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(mPtr++) = *(const goUInt8*)__ptr, (*sig)); break;
        case GO_INT16: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(mPtr++) = *(const goInt16*)__ptr, (*sig)); break;
        case GO_UINT16: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(mPtr++) = *(const goUInt16*)__ptr, (*sig)); break;
        case GO_INT32: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(mPtr++) = *(const goInt32*)__ptr, (*sig)); break;
        case GO_UINT32: GO_SIGNAL3D_EACHELEMENT_GENERIC (*(mPtr++) = *(const goUInt32*)__ptr, (*sig)); break;
        default: goLog::error("copyToMatlab(): unknown data type.",this);
                 const_cast<goSignal3DBase<void>*>(sig)->swapXY();
                 return false;
                 break;
    }
    //= This is NOT nice --- but we need to swap. I could also use a subsignal, 
    //= but that would be slower.
    const_cast<goSignal3DBase<void>*>(sig)->swapXY();
    return true;
}

bool
goMatlab::copyToMatlab (const goArray<goDouble>* array, mxArray* m)
{
    assert (sizeof(double) == sizeof(goDouble));
    if ((unsigned int)array->getSize() != mxGetN(m) * mxGetM(m))
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
    if ((unsigned int)vec->getSize() != mxGetN(m) * mxGetM(m))
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
goMatlab::copyToMatlab (const goVectorf* vec, mxArray* m)
{
    assert (sizeof(double) == sizeof(goDouble));
    if ((unsigned int)vec->getSize() != mxGetN(m) * mxGetM(m))
    {
        printf ("Signal and matrix are not of the same size\n");
        return false;
    }
    double* mPtr = mxGetPr (m);
    if (!mPtr)
    {
        return false;
    }
    goSize_t sz = vec->getSize();
    for (goSize_t i = 0; i < sz; ++i)
    {
        *mPtr = (*vec)[i];
        ++mPtr;
    }
    // memcpy (mPtr, vec->getPtr(), sizeof(double) * vec->getSize());
    return true;
}

bool
goMatlab::copyToMatlab (const goDouble* array, goSize_t length, mxArray* m)
{
    assert (sizeof(double) == sizeof(goDouble));
    if ((unsigned int)length != mxGetN(m) * mxGetM(m))
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
    if ((unsigned int)sp->getColumnCount() != mxGetM(m) || (unsigned int)sp->getRowCount() != mxGetM(m))
    {
        printf ("Signal and matrix are not of the same size\n");
        return false;
    }
    return goGetMatlabSparse (m, *sp); // sp->getMatlabSparse (m);
}


bool
goMatlab::copyFromMatlab (mxArray* m, goSignal3DBase<void>* sig)
{
    if ((unsigned int)sig->getSizeX() != mxGetN(m) || (unsigned int)sig->getSizeY() != mxGetM(m))
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
    if ((unsigned int)array->getSize() != mxGetN(m) * mxGetM(m))
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
    if ((unsigned int)vec->getSize() != mxGetN(m) * mxGetM(m))
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
goMatlab::copyFromMatlab (mxArray* m, goVectorf* vec)
{
    assert (sizeof(double) == sizeof(goDouble));
    if ((unsigned int)vec->getSize() != mxGetN(m) * mxGetM(m))
    {
        printf ("Signal and array are not of the same size\n");
        return false;
    }
    const double* mPtr = mxGetPr (m);
    if (!mPtr)
    {
        return false;
    }
    goSize_t sz = vec->getSize();
    for (goSize_t i = 0; i < sz; ++i)
    {
        (*vec)[i] = *mPtr;
        ++mPtr;
    }
    // memcpy (vec->getPtr(), mPtr, sizeof(double) * vec->getSize());
    return true;
}

/** 
 * @brief Execute matlab command.
 * 
 * @param command       Command string, any valid matlab script will do.
 * @param resultBuffer  If not 0, will be filled with the string output of the call,
 *                      up to the length of resultBuffer. If 0 (default), no output buffer
 *                      is used.
 * 
 * @return True if successful, false otherwise.
 */
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

/** 
 * @brief Synonym for matlabCall.
 * 
 * @param command Command
 * @param resultBuffer  Result buffer.
 * 
 * @return True if successful, false otherwise.
 */
bool
goMatlab::call (const char* command, goString* resultBuffer)
{
    return this->matlabCall (command, resultBuffer);
}

// ===========================================================


goMatlab::goMatlab ()
 : goObjectBase (),
   myPrivate (NULL)
{
    this->setClassID(GO_MATLAB);
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
/** @} */
