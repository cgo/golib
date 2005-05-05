/*!
 * \mainpage Online source documentation for goLib
 * \latexonly
 * 	This documentation is only a part of the source code documentation,
 * 	describing the namespace Vol and the most important classes used
 *  by those. For a complete documentation, please refer to the HTML version.
 * \endlatexonly 
 * \section intro What it is
 * goLib is a C++ class library written by 
 * Christian Gosch</a>.
 * It contains some useful classes for everyday use, like arrays, strings, a tree, heap
 * and so forth. Other functionalities are
 * - Classes for signal processing, especially image processing and processing
 * of 3D grid data (source tree under src/signal)
 * - A base for 1D signal processing is there, but not really functional. I am going to
 * add that as soon as I have time and have a use for it.
 * - Some limited matrix and vector classes
 * \par 
 * <strong>Important notice:</strong> All of this was made because I had
 * a use for it. If you want to make your own contributions, please see further 
 * below!
 *
 * \section howto How to build and use
 *	\subsection pre Prerequisites
 *	 You will always need the pthread library to compile goLib. It is
 *	 available in many modern Unix environments.
 *	 Depending on the version you are compiling, libSDL might also
 *	 be necessary. You can get it at http://www.libsdl.org.
 *	 In order to use the goFileIO::readImage() and goFileIO::writeImage() methods,
 *	 you need libIL which you can get at http://openil.sourceforge.net 
 
 *	\subsection build Compilation
 *  Using CMake:
 *  - If you don't have it, get it at http://www.cmake.org
 *  - In the main distribution directory, say, ~/golib, do
 *    <ul>
 *      <li>mkdir build</li>
 *      <li>cd build</li>
 *      <li>cmake ..  or  ccmake ..</li>
 *      <li>make</li>
 *      <li>(and if you like) make install</li>
 *    </ul>
 * 
 *	DEPRECATED: Using GNU Autotools
 * 	In the main distribution directory, say, ~/golib,
 *	do the following:
 *	<ul>
 *		<li>./configure</li>
 *		<li>make</li>
 *	</ul>
 * 	If the configure script fails, check for error messages and fix the
 *	errors (e.g., missing libraries).
 *	Depending on the library version, there are some custom command line options
 *	for the configure script. To get a list, type ./configure --help in the
 * 	shell.
 * 	\subsection env Environment variables
 *	 To be able to run programs linked to goLib, add 
 *	 the path to the shared library to your LD_LIBRARY_PATH environment
 *	 variable. It would also be wise to set the
 *	 environment variable GOPATH to the root path of the golib distribution,
 *	 for example to /home/myUserName/golib.
 * 	 The example programs in the distribution need this variable to be set.
 *	\subsection examples Examples
 *	 There are several little example programs in $(GO_PATH)/examples.
 *	 For each example program, there should be a makefile called
 *	 Makefile.NAME_OF_THE_EXAMPLE_PROGRAM. Use these makefiles
 * 	 to compile the examples. Be sure to have the environment variables
 *	 mentioned above set correctly.
 * 	 
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
 * The current version number is <strong>0.4.0</strong>.
 * The non-free parts will be removed and the library will get a higher version 
 * number then.
 * I remove bugs
 * as soon as I find them. As I only use parts of the library very often, these parts
 * will probably contain less bugs than others.
 * If you find a bug or think you found a bug, please
 * do not hesitate to send me an email to <code>golib __at__ goschs __ de</code>.
 *
 * \section download How to get the source
 * You should be able to download the source at <a href="http://www.goschs.de">www.goschs.de</a> in the [projects] section. If it's out of date,
 * let me know!
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
