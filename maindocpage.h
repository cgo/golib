/*!
 * \mainpage Online source documentation for golib
 * - \subpage main Main, general documentation (READ THIS!)
 * - \subpage notes Notes
 */
/*!
 * \page main Main Documentation
 *
 * \section license License
 * golib is put under the GNU General Public License.
 * It can be found in the source directory in the file COPYING or
 * at http://www.gnu.org/licenses/gpl
 * 
 * \section contact How to contact the author
 * You can contact me at golib at goschs dot de, with the usual replacements
 * to form a valid email address.
 *
 * \section intro What it is
 * \subsection intro2 Introduction
 goLib is a C++ class library written by 
 Christian Gosch</a>.
 It contains some classes which have proven useful to me and has grown gradually.
 Some parts are better maintained and better written than others, that depends completely
 on how much I have used them.

 I have also started to rename some things to use a bunch of namespaces (such as goMath),
 but that has only been done in a few places. If the library turns out to be used out there
 somewhere, I will rework the naming to be more consistent if that is what is needed.

 Some of the things golib includes are:
 - <b>Arrays, strings, lists, hashtables, binary trees, heaps</b> and so forth
 - <b>Multithreading</b> wrappers (goThreadObject and related) 
 - A class for grabbing frames from a <b>video4linux</b> device (recent, goVideoCapture)
   including some small matlab mex modules to use v4l under matlab
 - Classes for <b>image processing</b>, like container classes for <b>3D grid data</b>
   which supports multi-channel data
   (source tree under src/signal), wrapper for <b>writing and reading images</b> (goFileIO)
   which rely on libdevil
 - A <b>file system interface</b> in goFileIO to do some basic tasks like
   reading/writing ASCII files, checking for file existance, making directories, and so on.
 - Some <b>matrix and vector classes</b>, and a sparse matrix class 
   that acts mainly as a golib-side representation of sparse matrices for the Matlab
   interface
 - A few <b>numerical linear algebra routines</b>,
   using BLAS/LAPACK/ATLAS, and
   some parts from the free TNT library (incorporated in the code
   and marked as such). They include the standard conjugate gradients method 
   for sparse matrices, Eigenvalue and Singular Value decompositions, LU decomposition
   for solving linear systems, Eigenvalues of complex Hermitian matrices (the latter from
   SEISPACK).
 - Some optimisation classes, see the "math and numerics" module.
 - An interface to run <b>gnuplot</b> (goPlotter, goMultiPlotter)
   comfortably from C++, e.g. to plot a goVector
 - A <b>process interface</b> for external program calls (goProcess)
 - Some classes for <b>factor graphs</b> and <b>message passing algorithms</b>
 - <b>Function-like objects</b> (often called "Functors" in the C++ world), see goFunctorBase,
   goFunction(), and goMemberFunction().
 - A few networking classes (not well tested)
 - ... it's slowly growing as I find use for more stuff :)

Check out the modules section to get another overview.

 There are also additional libraries which use golib and are all included:
 - A <b>matlab interface wrapper</b>  
   to use matlab engines from C++ and swap data
   between golib and matlab (you need matlab for this, of course)
 - A GUI base library using gtkmm, a (very good) C++ wrapper for gtk+
   <ul>
   <li>http://www.gtkmm.org</li>
   </ul>
   You need version 2.4 of gtkmm for the GUI library.
 - A small OpenGL helper library.
   You need OpenGL libraries to use this.
 - A library helping to use embedded Python with the Swig-generated
   wrappers for golib classes (good for application scripting)
 
 All additional libraries can be selected to be built from the ccmake
 interface. 

 * \par 
 * <strong>Important notice:</strong> All of this was made because I had
 * a use for it. If you want to make your own contributions, please see further 
 * below! <br>
 * golib does contain some deprecated classes and functions, which will be
 * removed in the future.
 * I can not guarantee perfect suitability for
 * any purpose. However, the signal related stuff seems to work quite fine (and surely
 * still has bugs). Also, the classes in src/data seem to work ok (the basic 
 * arrays, fixed arrays, strings, lists, hashtables and goSignal3DBase-based classes 
 * are used by me a lot and therefore
 * get more bug fixing than other classes like the network server or 
 * similar).
 * \subsection intro3 Scripting using SWiG
 * \subsubsection intro31 Python
 * I have tried Python for scripting and prototyping and it fits quite well into
 * my programming habits and environment. Therefore,
 * I will continue work on the Python interface for golib and
 * neglect other interpreted languages.
 * The Python module can be selected in the ccmake interface.
 * The matlab and OpenGL modules also provide for a Python interface,
 * which will automatically be built if you additionally select to build the Python and
 * the Matlab interface in cmake/ccmake.
 * You should also select the install directory for your Python modules
 * in cmake/ccmake (the directory where the environment variable 
 * PYTHONPATH points to).
 * You do need SWiG >= 1.3.29 and Python >= 2.4 for this. It might work
 * with different versions, but was tried only with these.
 * <ul>
 *   <li>http://www.swig.org</li>
 *   <li>http://www.python.org</li>
 * </ul>
 * 
 * \subsubsection intro32 Guile Scheme (not recommended, discontinued)
 * Work on the scheme module is discontinued.
 * 
 * \section howto How to build and use
 *	\subsection pre Prerequisites
 *	 You will definitely need:
 *	 - A recent cmake: http://www.cmake.org
 *	 - GNU compiler collection (gcc) with C++ -- other compilers may work but were not tested.
 *	 - ATLAS generated CBLAS library: http://math-atlas.sourceforge.net/  for some matrix and vector operations
 *	 - ATLAS's LAPACK C implementation.
 *	 - An f2c translated or compatible 
 *	   LAPACK, like the reference implementation from www.netlib.org.
 *	   (ATLAS's LAPACK only implements a subset of LAPACK).
 *	   Many Linux distributors provide for this library, just as for ATLAS.
 *	   You will also need the header files cblas.h and clapack.h from ATLAS.
 *       - video4linux header files and library
 *	 You may replace ATLAS's CBLAS with another CBLAS implementation. 
 *	 In that case, you have to remove the ATLAS library from
 *	 CMakeFiles.txt and only leave CBLAS. You will be on your own since I haven't tried that.
 *
 *	 Optional, but very highly recommended:
 *	 - <b>Developer's Image Library: http://openil.sourceforge.net</b><br>
 *	 You need this in order to load and store image files with goFileIO:: methods.
 *
 *	 Completely optional:
 *	 - SWiG: http://www.swig.org
 *	 - Python 2.4: http://www.python.org
 *	 These are both needed for the Python modules for golib.
 *
 *	 You will always need the pthread library. It is
 *	 available in many modern Unix environments. golib has in an early version
 *	 also been compiled on Windows, and there are Windows implementations
 *	 for the multithreading classes too. However, many other things will probably
 *	 not compile anymore in Windows.
 *	 Depending on the version you are compiling, libSDL might also
 *	 be necessary (only for golib 0.2). You can get it at http://www.libsdl.org.
 *
 *  \subsection ubuntu Package Dependencies in Ubuntu
    This holds also possibly for other Debian based
    GNU/Linux distributions.
    <ul>
     <li>Necessary:</li>
     <ul>
      <li>libatlas-base-dev (lapack, blas, atlas)</li>
      <li>libf2c2-dev (lapack)</li>
      <li>libv4l-dev (for govideocapture.cpp)</li>
     </ul>
     <li>Recommended:</li>
       <ul><li>libdevil-dev</li></ul>
     <li>GUI:</li>
      <ul>
       <li>libgtkmm-2.4-dev</li>
       <li>libgtkglext1-dev</li>
       </ul>
     <li>GL:</li>
     <li>Python:</li>
        <ul><li>python-dev</li></ul>
    </ul>
 *
 *
 * 	\subsection env Environment variables
 * 	You need to set:
 * 	- MATLAB should point to your Matlab installation path,
 * 	  if you want to build the Matlab module. E.g., <br>
 * 	    setenv MATLAB /usr/opt/matlab
 * 	- PYTHONPATH after installing the Python language modules
 * 	  in order to use the modules from Python.
 *
 * 	You may have to set:
 *      - For some of the examples in the examples/ subdirectory,
 *        you may need to set the GOPATH environment variable to point
 *        to the base directory of golib.
 * 	- LD_LIBRARY_PATH accordingly, depending on where you install golib
 * 	- LDFLAGS for any non standard library directories
 * 	- Add any non standard include directories to CPPFLAGS.<br>
 * 	If you don't know what the latter two are, you probably don't need them.
 *
 *	 To be able to run programs linked to golib, add 
 *	 the path to the shared library to your LD_LIBRARY_PATH environment
 *	 variable. It would also be wise to set the
 *	 environment variable GOPATH to the root path of the golib distribution,
 *	 for example to /home/myUserName/golib.
 * 	 The example programs in the distribution need this variable to be set.
 *
 *	\subsection build Compilation
 *  Using CMake:
 *  - If you don't have it, get it at http://www.cmake.org
 *  - I recommend using the newest version, as of now that is 2.4
 *  - In the main distribution directory, say, ~/golib, do
 *    <ul>
 *      <li>mkdir build</li>
 *      <li>cd build</li>
 *      <li>cmake ..  or  ccmake ..</li>
 *      <li>  --> select whichever modules you need, and configure/generate</li>
 *      <li> Notice you may have to turn LINK_GFORTRAN to OFF in ccmake, depending on whether the linker complains about it.</li>
 *      <li>make</li>
 *      <li>make install</li>
 *    </ul>
 * 
 *	\subsection examples Examples
 *	 There are several little example programs in $(GO_PATH)/examples.
 *	 For more recent examples, there is a subdirectory for each example.
 *	 Makefiles are provided but may need small modifications (like 
 *	 -L linker options). Just try them, if they don't work, you need to
 *	 modify them. They are very small and easy to understand.
 *	 For very old example programs,
 *	 there should be a makefile called
 *	 Makefile.NAME_OF_THE_EXAMPLE_PROGRAM. Use these makefiles
 * 	 to compile the examples. Be sure to have the environment variables
 *	 mentioned above set correctly.
 *
 * \section state State of development
 * Development has started some time in 1996/97.
 * Some of this is work in progress.
 * Note that I am currently not working very much on golib,
 * since I have a day job, a non-virtual life, and other projects which
 * have precedence.</br>
 * The current version number is <strong>0.5</strong>.
 * Some significant changes are always imminent (;-)).
 * Some of the basic features are not very well documented, but I will fix that
 * in the future. Particularly, "how-to-use" sections are missing in the documentation,
 * even though many classes are straight forward to use.
 * I plan to write some introductory documentation
 * which explains some concepts.<br>
 * <b>You should also be aware that this library was created over a long time
 * in small parts, some of which I used more than others. The parts I
 * used more are likely to be better documented, better maintained, and generally in better shape.</b> <br>
 * I remove bugs
 * as soon as I find them. As I only used to use parts of the library very often, these parts
 * will probably contain less bugs than others.
 * If you find a bug or think you found a bug, please
 * do not hesitate to send me an email to <code>golib __at__ goschs __ de</code>.
 *
 * \section download How to get the source
 * <b>A current version is not available online. If you are really interested in
 *    trying the library, please drop me a line.</b>
 * 
 * \section warranty Warranty
 * No warranty at all.
 */

 /**
 * \page notes Notes
 * - \subpage notes_atlas Notes on CBLAS and ATLAS
 * - Some PDF on non-recursive depth first search from the web is in doc/gographalgorithm
 * - \subpage notes_matlab Notes on the matlab module
 */

 /**
 * \page notes_atlas Notes on CBLAS and ATLAS
 *  - For explanations about "leading dimension", see e.g. here:
 *    http://www.inf.bv.tum.de/~heisserer/softwarelab04/index.html
 *
 *  "Note that for cblas-functions the leading dimension 
 *  (for 2D arrays in C-fashion, i.e. row major order) is the number of 
 *  columns of the matrix (not the rows as in Fortran).
 *  The leading dimension is the number of entries in 
 *  memory that separate the e.g. first elements of rows in c-fashion 
 *  storage (row major order, i.e. elements of one row are contiguous in memory).
 *  As Fortran stores in column major order the leading dimension is the number of rows."
 */
 /**
  * \page notes_matlab Notes on the matlab module
  * Deeply buried in the more or less sensible information that
  * can be found on the Mathworks web site, you can find
  * that you <b>must have csh installed</b> in order to run
  * "engine programs", i.e. programs that use the C interface
  * to the matlab engine. So in order to use the Gomatlab library
  * with matlab, you must have csh (the c-shell).
  */
