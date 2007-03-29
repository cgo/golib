/*!
 * \defgroup types Types
 */
/*!
 * \defgroup signal Data handling for up to 3D data
 *  \section signal-intro Introduction
 *  Golib provides a few classes for handling data storage and access
 *  of data arranged in an up to 3-dimensional regular grid.
 *  Data can be stored in goSignal3D objects and 
 *  be accessed through the base class goSignal3DBase or 
 *  through goSubSignal3D, which provides access to
 *  rectilinear sub-regions of a goSignal3DBase.
 *  All classes are provided as templates and there are instantiations
 *  for the most important simple scalar data types,
 *  but it is recommended to use the <void> template parameter
 *  and use the (newer) generic interface to the data.
 *  The special instantiations do work, but will be deprecated some time
 *  in the future and new features will be provided for
 *  the <void> instantiations.
 *  For those, the data type of the stored data is managed 
 *  by the class itself. This makes the signal classes more
 *  flexible to use and takes away the necessity for functions and classes
 *  that use the signal classes also to be templates (which can be a pain).
 *  
 *  \section signal-orga Organisation
 *  goSignal3DBase is the base class for all signal classes.
 *  All data is organised as a rectilinear grid of
 *  up to 3 dimensions. The memory is allocated
 *  following the concept of data locality by allocating
 *  the data in blocks the size of which can be adjusted by the user.
 *  This allows for speed-up of code which accesses local
 *  neighbourhoods of data points by providing block sizes which 
 *  enable efficient use of cache structures of the used processor
 *  architecture (which naturally is machine dependent).
 *  Of cource, by choosing the block size accordingly, data can also
 *  be organised linearly.
 *  A mechanism allowing to provide borders for a signal
 *  is also provided, making it easier to use algorithms which
 *  rely on certain assumptions on the border, e.g. that a signal
 *  is periodic or that the border is constant.<br>
 *  goSignal3D is the class that actually allocates memory and
 *  stores data.<br>
 *  goSubSignal3D can be used to get a window or region of interest
 *  from another goSignal3DBase.
 *  
 *  \section signal-usage Usage
 *  \subsection usage-general General Usage And Iterators
 *  Using the signal classes is quite simple once you
 *  get used to the fact the data is not linear in memory in general.
 *  In the simplest case, data can be accessed by the goSignal3DBase::getPtr()
 *  methods. However, this is quite slow if you want to 
 *  go through a lot of points and not just, say, 10 out of a 1024^3 cube.
 *  If you want to iterate over all data points, you can
 *  make use of goSignal3DGenericIterator and goSignal3DGenericConstIterator:
 *  <pre>
 *   <code>
 *      goSignal3D<void> mySignal;
 *      ... // fill the signal, load data, whatever.
 *      goSignal3DGenericIterator it(&mySignal);
 *      while(!it.endZ())
 *      {
 *          it.resetY();
 *          while (!it.endY())
 *          {
 *              it.resetX();
 *              while (!it.endX())
 *              {
 *                  *(goFloat*)*it = calculate_something();
 *                  it.incrementX();
 *              }
 *              it.incrementY();
 *          } 
 *          it.incrementZ();
 *      }
 *    </code>
 *  </pre>
 *  The end[X|Y|Z]() methods indicate the end of one of the three dimensions.
 *  You can think of the resetX(), resetY(), resetZ() methods
 *  as carriage returns for each dimension. The increment methods
 *  of course increment the iterator in one dimension.
 *  Note that after this loop, you have to resetZ() the iterator
 *  in order to be able to use it again in the same manner,
 *  since it reached the end of the Z dimension.
 *  Also note that if you have multi-channel signals,
 *  you must choose the active channel using setChannel() BEFORE
 *  initialising (i.e. creating) the iterator.
 *  
 *  \subsection usage-multichannel Multiple Channels
 *  Signals can hold multi-channel data which can be 
 *  accessed by selecting an active channel with setChannel().
 *  The number of channels can be retrieved with getChannelCount().
 *  Note that the channel data is stored linearly at a point, so
 *  by getting the pointer to the 0th channel with
 *  the dereference operator of an iterator, you can access the other
 *  channels simply by adding the respective value to the pointer.
 *  I expect that I will keep this data organisation in the future,
 *  so go ahead and use it. But keep in mind that this means using
 *  some sort of internal knowledge about the signal storage,
 *  which should be avoided in general because of course it is not
 *  completely impossible that something is changed about this in the future
 *  (however unlikely). However, I think
 *  in this case it can mean significant speed-ups.
 */
/*!
 * \defgroup math Mathematics
 *  \section math-la Linear Algebra Objects
 *  Use goMatrix and goVector for matrix and vector operations.
 *  Others, like go4Vector and such will be deprecated and replaced
 *  solely by the former two classes.
 *  goMatrix and goVector use CBLAS routines for some operations
 *  and more will be added. Using CBLAS moves the matter of optimisation
 *  for a specific platform outside of golib.
 *  The operator* and operator*= operators use CBLAS.
 *  For best performance, when you want to do something like
 *  \f$C = A^\top \cdot B + C\f$, use goMatrixMult().
 *  Explicitly transposing a matrix should not be necessary,
 *  if I find it is for some operation, I will add functions
 *  that overcome this. Transposition of a goMatrix is possible
 *  with getTranspose() or transpose(), but is very slow since
 *  the data are copied.
 *
 *  Singular value decomposition can be done using goSVD.
 *  Eigenvalues and eigenvectors can be calculated with goEigenvalue.
 *  Both are adapted versions from the Template Numerical Toolkit, 
 *  a freely available implementation of a few linear algebra 
 *  algorithms.
 *  
 * \defgroup misc Miscellaneous and basic classes
 *  \section misc-intro Introduction
 *   This group contains miscellaneous and basic classes, like the goObjectBase class.
 *   This class provides for
 *   - Generic method calls
 *   - Inter-object "communication"
 *   - Interfaces for writing and reading to and from files (this is not being used much
 *    but is indended to provide for a mechanism for storing and retrieving 
 *    objects from secondary memory)
 *   - Identification of classes by numeric IDs, objects by names.
 *    Class names exist through a hash table in goGlobal, that takes a numeric class ID
 *    as input and returns a goString.
 *
 *   \section misc-goobjectbase goObjectBase
 * 
 *    \section misc-gofileio File Operations With goFileIO
 *     \subsection misc-gofileio-images Loading and Writing Images
 *     goFileIO::readImage() and goFileIO::writeImage() can be used to
 *     read and write images to and from goSignal3D objects.
 *     This works only if the use of libdevil was enabled when golib
 *     was compiled (see building instructions).
 *     Naturally, golib can read and write any file format that
 *     can be read and written by the installed libdevil.
 *     There are also functions for reading and writing
 *     ASCII strings from and to files. See goFileIO for details.
 *   
 * \defgroup system System
 * \defgroup net Network classes
 * \defgroup thread Multithreading classes
 * \defgroup video Video capture and related
 * \defgroup gm Belief propagation and factor graphs
 */
