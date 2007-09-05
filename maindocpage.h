/*!
 * \mainpage Online source documentation for goLib
 * - \subpage main Main, general documentation (READ THIS!)
 * - \subpage notes Notes
 */
/*!
 * \page main Main Documentation
 * <b>A current version is not available online. If you are really interested in
 *    trying the library, please drop me a line.</b>
 * \section intro What it is
 * \subsection intro2 Introduction
 * goLib is a C++ class library written by 
 * Christian Gosch</a>.
 * It contains some classes which have proven useful to me, like 
 * - <b>Arrays, strings, lists, hashtables, binary trees, heaps</b> and so forth
 * - <b>Multithreading</b> wrappers (goThreadObject and related) 
 * - A class for grabbing frames from a <b>video4linux</b> device (recent, goVideoCapture)
 *   including some small matlab mex modules to use v4l under matlab
 * - Classes for <b>image processing</b>, like container classes for <b>3D grid data</b>
 *   which supports multi-channel data
 *   (source tree under src/signal), wrapper for <b>writing and reading images</b> (goFileIO)
 *   which rely on libdevil
 * - A <b>file system interface</b> in goFileIO to do some basic tasks like
 *   reading/writing ASCII files, checking for file existance, making directories, and so on.
 * - Some <b>matrix and vector classes</b>, and a sparse matrix class 
 *   that acts mainly as a golib-side representation of sparse matrices for the Matlab
 *   interface
 * - Few <b>numerical linear algebra routines</b>, 
 *   like the ones from the free TNT library (incorporated in the code
 *   and marked as such) and the standard conjugate gradients method 
 *   for sparse matrices, Eigenvalue and Singular Value decompositions, LU decomposition
 *   for solving linear systems, Eigenvalues of complex Hermitian matrices (the latter from
 *   SEISPACK).
 * - An interface to <b>gnuplot</b> (goPlotter, goMultiPlotter)
 * - A few networking classes (not well tested)
 * - A <b>process interface</b> for external program calls (goProcess)
 * - ... it's slowly growing as I find use for more stuff :)
 *
 * There are also additional libraries which use golib and are all included 
 * in the distribution:
 * - A <b>matlab interface wrapper</b>  
 *   to use matlab engines from C++ and swap data
 *   between the two (you need matlab for this, of course)
 * - A GUI base library using gtkmm, a (very good) C++ wrapper for gtk+
 *   <ul>
 *   <li>http://www.gtkmm.org</li>
 *   </ul>
 *   You need version 2.4 of gtkmm for the GUI library.
 * - A small OpenGL helper library (quite recent and currently very small).
 *   You need OpenGL libraries to use this.
 * 
 * All additional libraries can be selected to be built from the ccmake
 * interface. However, build and install golib itself first, without
 * selecting additional libraries (since they rely on finding golib).
 * See the building instructions further below.
 *
 * \par 
 * <strong>Important notice:</strong> All of this was made because I had
 * a use for it. If you want to make your own contributions, please see further 
 * below! <br>
 * It may well be the case that classes contained in the library are not well tested
 * or have never been in actual use. I can not guarantee perfect suitability for
 * any purpose. However, the signal related stuff seems to work quite fine (and surely
 * still has bugs). Also, the classes in src/data seem to work ok (the basic 
 * arrays, fixed arrays, strings, lists, hashtables and goSignal3DBase-based classes 
 * are used by me a lot and therefore
 * get more bug fixing than other classes like the network server or 
 * similar).
 * \subsection intro3 Scripting using SWiG
 * \subsubsection intro31 Python
 * I have recently tried Python and it fits quite well into
 * my programming habits and environment. Therefore,
 * I will continue work on the Python interface for golib and
 * neglect other interpreted languages.
 * The Python module can be selected in the ccmake interface,
 * but should only be built when libGo has been built and installed.
 * The matlab module also provides for a Python interface,
 * which will automatically be built if you select to build the Python and
 * the Matlab interface in cmake/ccmake.
 * You should also select the install directory for your Python modules
 * in cmake/ccmake (the directory where the environment variable 
 * PYTHONPATH points to).
 * You do need SWiG >= 1.3.29 and Python >= 2.4 for this. It might work
 * with different versions, but was tried only with these.
 * 
 * \subsubsection intro32 Guile Scheme (not recommended, discontinued)
 * If you have GNU Guile >= 1.6.7, you can use Guile's implementation of the Scheme
 * programming language to write scripts that use the functionality of golib.
 * In the directory <code>swig</code>, you will find code and a CMakeLists.txt
 * to create a Guile module using SWiG (so you need SWiG >= 1.3.25 too).
 * If you want to use the Guile module, install the created .so library
 * to a location included in the LD_LIBRARY_PATH environment variable and
 * copy the created .scm files to a path that can be found by Guile
 * (e.g., a path contained in the GUILE_LOAD_PATH environment variable).
 * Also, if the file common.scm, which comes from SWiG, is not available on your system,
 * copy it to the same location as the other .scm files (it resides in golib's swig directory).
 * Note that golib's Guile Scheme support is highly experimental and subject to changes.<br>
 * <ul>
 *   <li>http://www.swig.org</li>
 *   <li>http://www.gnu.org/software/guile</li>
 * </ul>
 * 
 * \section howto How to build and use
 *	\subsection pre Prerequisites
 *	 You will definitely need:
 *	 - A recent cmake: http://www.cmake.org
 *	 - GNU compiler collection (gcc) with C++ -- other compilers may work but were not tested.
 *	 - ATLAS generated CBLAS library: http://math-atlas.sourceforge.net/  for some matrix and vector operations<br>
 *	 - ATLAS's LAPACK C implementation.
 *	 - An f2c translated or compatible 
 *	   LAPACK, like the reference implementation from www.netlib.org.
 *	   (ATLAS's LAPACK only implements a subset of LAPACK).
 *	   Many Linux distributors provide for this library, just as for ATLAS.
 *	   You will also need the header files cblas.h and clapack.h from ATLAS.
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
 *	 - Guile: http://www.gnu.org/software/guile<br>
 *	 These are both needed for the respective modules for golib.
 *
 *	 You will always need the pthread library to compile goLib. It is
 *	 available in many modern Unix environments.
 *	 Depending on the version you are compiling, libSDL might also
 *	 be necessary. You can get it at http://www.libsdl.org.
 *	 In order to use the goFileIO::readImage() and goFileIO::writeImage() methods,
 *	 you need libIL which you can get at http://openil.sourceforge.net <br>
 *	 I recently switched from GNU Autotools to CMake, which can be obtained from 
 *	 http://www.cmake.org <br>
 *	 I have not yet decided if I cease using Autotools completely or support both.
 *	 Probably I will drop Autotools in favour of CMake.<br>
 *	 In order to use the guile scripting support (which is experimental,
 *	 see below), you need swig (http://www.swig.org) and libguile
 *	 (http://www.gnu.org/software/guile).
 *
 * 	\subsection env Environment variables
 * 	You need to set:
 * 	- GOPATH to the golib base directory.
 * 	  E.g., if you have unpacked the source to /home/user/golib, then set
 * 	  GOPATH to that.<br>
 * 	  E.g. in csh, do<br>
 * 	  <code>setenv GOPATH /home/user/golib</code><br>
 * 	  in bash, you would do<br>
 * 	  <code>export GOPATH=/home/user/golib</code><br>
 * 	- MATLAB should point to your Matlab installation path,
 * 	  if you want to build the Matlab module. E.g., <br>
 * 	    setenv MATLAB /usr/opt/matlab
 * 	- PYTHONPATH after installing the Python language modules
 * 	  in order to use the modules from Python.
 *
 * 	You may have to set:
 * 	- LD_LIBRARY_PATH accordingly, depending on where you install golib
 * 	- LDFLAGS for any non standard library directories
 * 	- Add any non standard include directories to CPPFLAGS.<br>
 * 	If you don't know what the latter two are, you probably don't need them.
 *
 *	 To be able to run programs linked to goLib, add 
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
 *      <li>  --> select no additional modules
 *      <li>make</li>
 *      <li>make install</li>
 *      <li>If you like to build some additional modules, do<br>
 *          ccmake<br>
 *            --> select additional modules, like the Matlab module, GTKMM GUI module,
 *            Python module, etc., and do<br>
 *           make<br>
 *           make install<br>
 *          again.
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
 * \section da  ---Diplomarbeit---
 * Version 0.2.1 of this library contains the source code for my Diplomarbeit 
 * (final thesis in Germany). 
 * Everything in the namespace Vol is only for the Diplomarbeit, which has
 * been in cooperation with Siemens Medical Solutions, Erlangen, Germany.
 * <strong> 
 * 	 According to the cooperation agreement,
 *   this code is NOT free and NOT intended for the open public. 
 * </strong>
 * If you received this code by accident, please be so kind to delete
 * it and get a different version of the library. You are not allowed to
 * use it without written consent of all copyright holders.
 *
 * \section state State of development
 * Most of this, except for the src/data tree, is work in progress.
 * The current version number is <strong>0.5</strong>.
 * Some significant changes are imminent.
 * There are currently non-free parts in it which are unlikely to
 * be released to open public any time soon (if you should receive them,
 * you must delete them).
 * Some of the basic features are not very well documented, but I will fix that
 * in the future. I plan to write some introductory documentation
 * which explains some concepts.<br>
 * You should also be aware that this library was created over a long time
 * in small parts, some of which I used more than others. The parts I
 * used more are likely to be better documented and better maintained.<br>
 * I remove bugs
 * as soon as I find them. As I only use parts of the library very often, these parts
 * will probably contain less bugs than others.
 * If you find a bug or think you found a bug, please
 * do not hesitate to send me an email to <code>golib __at__ goschs __ de</code>.
 *
 * \section download How to get the source
 * You should be able to download the source at <a href="http://www.goschs.de">www.goschs.de</a> in the [projects] section. 
 * <b>A current version is not available online. If you are really interested in
 *    trying the library, please drop me a line.</b>
 * 
 * 
 * \section license License
 * Please read the files COPYING and README in the distribution directory. 
 * This library is distributed under the terms of the GNU General Public License,
 * but if there is demand, I will provide a <b>commercial license</b> in addition to that
 * (the GPL'ed version will not be touched by this).
 * Of course a commercial version would <b>not</b> be free but would enable
 * you to use the library in a closed source, commercial product.
 * 
 * If you should find that the version you downloaded is out of date
 * and does not match this documentation, please notify the me, the author,
 * at <code>christian __at__ goschs __ de</code>.
 * Since it is under construction quite heavily at the moment,
 * that might well be the case. <br>
 * <strong> 
 * 		Version 0.2.1 of this library is NOT free and NOT for the open
 *		public. If you have it and don't know if you are allowed 
 * 		to use it, please contact the author or simply delete the code
 *		and get another version!
 * </strong>
 * 
 * \section warranty Warranty
 * This is very simple. There is no warranty at all, since this software is provided
 * free of charge.
 * 
 * \section contributions Contributions
 * If you would like to make contributions to goLib, you are more
 * than welcome to! Please send suggestions or source (or both ;-) ) to
 * <code>christian __at__ goschs __ de</code>.
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
