#ifndef __GOMATRIX_H
#define __GOMATRIX_H


#include <gotypes.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalmacros.h>
#include <gomatrix.h>
#include <goerror.h>
#ifndef GOROWVECTOR_H
# include <gorowvector.h>
#endif

/*!
 * Matrix class.
 */
template <class T>
class goMatrix 
{
 public:
  /*!
   * @param y Number of rows.
   * @param x Number of columns.
   */
  goMatrix (goSize_t y = 4, goSize_t x = 4);
  goMatrix     (const goMatrix<T>& other);
  virtual ~goMatrix ();
  
  virtual bool setData   (goSignal3DBase<T>* data);
  virtual bool resize    (goSize_t rows, goSize_t columns);
  virtual void transpose ();
  virtual goMatrix<T>& operator= (const goMatrix<T>& other);
  
 protected:
  goMatrix (goSignal3DBase<T>* data);
  bool initializeRows ();

 public:
  inline goSize_t   getColumns() const;
  inline goSize_t   getRows()    const;

  inline T&         elem (goSize_t i, goSize_t j);
  /*!
   * @return Number of columns
   */
  inline goSize_t	getSizeX() const; 
  /*!
   * @return Number of rows
   */
  inline goSize_t	getSizeY() const;
  
  goRowVector<T>&   operator[] (goSize_t y);
  goMatrix<T>		operator*  (const goMatrix<T>& other);
  goMatrix<T>		operator-  (const goMatrix<T>& other);
  goMatrix<T>		operator+  (const goMatrix<T>& other);
  goMatrix<T>&		operator*= (const goMatrix<T>& other);
  goMatrix<T>&		operator+= (const goMatrix<T>& other);
  goMatrix<T>&		operator-= (const goMatrix<T>& other);
  goMatrix<T>&		operator*= (T scalar);
  goMatrix<T>&		operator/= (T scalar);

  /// Loads unity
  void unity();
  void fill(T v);
  void print();

 protected:
  bool               externalData;
  goSignal3DBase<T>* matrix;
  goSubSignal3D<T>*  rows;
  goRowVector<T>*    rowVectors;
};


#endif /* __GOMATRIX_H */
