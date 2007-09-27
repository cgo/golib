#ifndef GOMATRIXSIGNAL_H
#define GOMATRIXSIGNAL_H


#include <gotypes.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalmacros.h>
#ifndef GOROWVECTOR_H
# include <gorowvector.h>
#endif

template <class T> class goVector;

/* This is deprecated. */

/*
 * \brief Matrix class.
 *
 * This class uses goSignal3D for low level data storage and 
 * can therefore be very efficient accessing the matrix data.
 * To enable the [][] indexing, a helper template class for
 * row vectors, goRowVector, was introduced.
 *
 * If you need to instantiate the template for a new type,
 * include goMatrixSignal.hpp
 *
 * \author Christian Gosch
 */
template <class T>
class goMatrixSignal 
{
 public:
  /*
   * @param y Number of rows.
   * @param x Number of columns.
   * @param linear If true, the data is stored linearly in memory.
   *        If false, the data is stored in tiles, so you cannot rely on it being
   *        linear. True is the default.
   */
  goMatrixSignal (goSize_t y = 4, goSize_t x = 4, bool linear = true);
  goMatrixSignal (const goMatrixSignal<T>& other);
  virtual ~goMatrixSignal ();
  
  virtual bool             setData   (goSignal3DBase<T>* data);
  virtual bool             resize    (goSize_t rows, goSize_t columns);
  virtual void             transpose ();
  virtual goMatrixSignal<T>&     operator= (const goMatrixSignal<T>& other);
  goSignal3DBase<T>*       getData   () { return matrix; };
  const goSignal3DBase<T>* getData   () const { return matrix; };
  bool                     isLinear  () const { return this->linearStorage; }
  
 protected:
  goMatrixSignal (goSignal3DBase<T>* data);
  bool initializeRows ();

 public:
  goSize_t   getColumns () const;
  goSize_t   getRows    () const;
 
  // TNT compatibility methods BEGIN
  inline int        dim1 () const;
  inline int        dim2 () const;
  inline const goMatrixSignal<T>& copy () const;  // NOTE: Makes a deep copy here
                                            // and a reference in TNT
  // TNT compatibility methods END  
  
  inline T&         elem (goSize_t i, goSize_t j);
  /*
   * @return Number of columns
   */
  inline goSize_t	getSizeX() const; 
  /*
   * @return Number of rows
   */
  inline goSize_t	getSizeY() const;
 
  T&                operator() (goIndex_t i, goIndex_t j);
  const T&          operator() (goIndex_t i, goIndex_t j) const;
  goRowVector<T>&       operator[] (goSize_t y);
  const goRowVector<T>& operator[] (goSize_t y) const;
  goMatrixSignal<T>		operator*  (const goMatrixSignal<T>& other);
  goMatrixSignal<T>		operator-  (const goMatrixSignal<T>& other);
  goMatrixSignal<T>		operator+  (const goMatrixSignal<T>& other);
  goMatrixSignal<T>&		operator*= (const goMatrixSignal<T>& other);
  goMatrixSignal<T>&		operator+= (const goMatrixSignal<T>& other);
  goMatrixSignal<T>&		operator-= (const goMatrixSignal<T>& other);
  goVector<T>       operator*  (const goVector<T>& v);
  goMatrixSignal<T>&		operator*= (T scalar);
  goMatrixSignal<T>&		operator/= (T scalar);

  bool              multiplyElements (const goMatrixSignal<T>& other);

  T                 norm () const;

  // Loads identity
  void setUnity();
  // Loads identity
  void setIdentity();
  void fill(T v);
  void print();

 protected:
  bool               linearStorage;
  bool               externalData;
  goSignal3DBase<T>* matrix;
  goSubSignal3D<T>*  rows;
  goRowVector<T>*    rowVectors;
};

typedef goMatrixSignal<goDouble> goMatrixSignald;
typedef goMatrixSignal<goFloat>  goMatrixSignalf;

#endif 
