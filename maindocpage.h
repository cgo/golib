/*!
 * \mainpage Online source documentation for goLib
 * \latexonly
 * 	This documentation is only a part of the source code documentation,
 * 	describing the namespace Vol and the most important classes used
 *  by those. For a complete documentation, please refer to the HTML version.
 * \endlatexonly 
 * \section intro What it is
 * goLib is a C++ class library written by <a href="mailto:christian@goschs.de">
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
 
 *	\subsection build Compilation
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
 * The current version number is <strong>0.2.1</strong>.
 * The non-free parts will be removed and the library will get a higher version 
 * number then (probably 0.3).
 * I remove bugs
 * as soon as I find them. If you find a bug or think you found a bug, please
 * do not hesitate to send me an <a href="mailto:christian@goschs.de">email</a>.
 *
 * \section download How to get the source
 * You should be able to download the source at <a href="http://www.goschs.de">www.goschs.de</a> in the [projects] section. If it's out of date,
 * let me know!
 * 
 * \section license License
 * I am not yet totally sure about this topic, but most probably this
 * library will be GPL'ed or LGPL'ed.
 * Until then, you may use the library at will, but you have to report
 * changes you make on it to me, the author, and you may not
 * redistribute a changed version of the library.
 * You may, however, redistribute the source unchanged as a whole, including
 * all documentation including this text and all README files
 * eventually included, with <strong>NO CHARGE</strong>. If you distribute a binary version, you have
 * to include the source as a whole again, including all documentation 
 * and all other files included in the original distribution.
 * No version of goLib may be distributed for a fee or charge, except some small fee
 * for copying the library on a storage medium, like a CD or a floppy disk.
 * 
 * If you should find that the version you downloaded is out of date
 * and does not match this documentation, please notify the me, the author,
 * at <a href="mailto:christian@goschs.de">christian@goschs.de</a>.
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
 * <a href="mailto:christian@goschs.de">christian@goschs.de</a>.
 */
