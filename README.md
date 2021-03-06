libGo (golib) README
--------------------

LICENSE ISSUES
--------------
Please read the file COPYING.
Golib is put under the GNU General Public license (GPL).

About
-----
libGo is a C++ class library containing all kinds of 
things that proved useful to me.
For details, see the HTML source documentation in golib/doc.
(See "How to build the HTML documentation" below.)

You can find the Doxygen documentation here:
- https://cgo.github.io/golib/html/index.html
- A useful part is probaby the Modules page: https://cgo.github.io/golib/html/modules.html

This project has been moved from its old home in Mercurial at SourceForge, at https://sourceforge.net/p/libgo/code/ci/default/tree/.
Note that the latter is no longer maintained and this has been moved to github to consolidate.

Directories
-----------
- ./		- Main libGo distribution directory
- ./docs	- Documentation
- ./examples	- Example source code and makefiles
- ./exp		- Experimental stuff, guaranteed to be unstable ;)
- ./include	- All include files
- ./src		- Source code, subdivided in different subdirectories
- ./tools	- Some scripts, including a Perl script to create makefiles
- ./rpm		- Work in progress -- this will contain the files necessary
		- to build an rpm package

What you need
-------------
- golib was last compiled on a Ubuntu 11.10 (oneiric) system.
- You will need a C++ compiler (tested with gnu c++ 4.6.1)
- A current version of CMake (http://www.cmake.org). Earlier versions also
  supported GNU autotools, but that is no longer supported.
- The pthreads library must be installed.
- For version 0.2.1, you will also need the SDL library
  (see http://www.libsdl.org).
- For image loading/writing support you need 
  libdevil (http://openil.sourceforge.net/). You will not be able
  to read/write images when you don't have libdevil.
- If you want to use the matlab support library, you obviously
  need Matlab. *NOTE* I do not have access to Matlab anymore,
  so I can not be of much help if something does not work.
- If you want the (somewhat restricted) Python interface and
  want to use the Python embedding helpers, you need Python 2.7
  or a compatible version. Just give it a try. Python 3 will probably
  not work.
  You will also need SWiG for the Python stuff:
  http://www.swig.org

Also see the html documentation as it might be more up to date.
  
Package Dependencies in Ubuntu
------------------------------
This holds possibly also for other Debian based
GNU/Linux distributions.
Necessary:
 libatlas-base-dev (lapack, blas, atlas)
 libf2c2-dev (lapack)
 libv4l-dev (for govideocapture.cpp)
Recommended:
 libdevil-dev
GUI:
 libgtkmm-2.4-dev
 libgtkglext1-dev
GL:
Python:
 python-dev


Package dependencies in Mac OS X El Capitan
-------------------------------------------
These notes assume you have homebrew. If you use another
or no package manager, you need to install the packages
in another way (or get homebrew).

CMake:
 brew install cmake

DevIL: you need to do the following:
 brew install homebrew/versions/gcc49
 brew install devil

LAPACKE: See tools/lapacke.
XCode in OS X comes with the Accelerate framework, which contains
lapack and cblas. /But/, the lapack version is only 3.2.1, and LAPACKE,
the official C interface to LAPACK, was only added in 3.4.
You can use the scripts and SConstruct file for scons to build lapacke.
You should keep lapacke.a and delete lapacke.dylib after building
if you want to only statically link to lapacke.
To get scons:
 brew install scons

GTKMM (for the GUI stuff):
 brew install gtkmm3
 brew install adwaita-icon-theme
 

How to build the HTML documentation
-----------------------------------
Get /doxygen/, in Ubuntu e.g. say "sudo apt-get install doxygen".
Then in the golib base directory, simply call doxygen
from the command line.
The documentation will be in doc/html.


How to build
------------
Set the environment variable GOPATH to the path where golib resides, e.g. in bash
 export GOPATH=/home/christian/golib
In the golib directory, do
 mkdir build
 cd build
 cmake ..
 ccmake . 
   (or cmake-gui), and set the things you want; set LINK_GFORTRAN to OFF, if the linker complains about it.
 make
Also see the html documentation for details.
There is also a matlab support library in the matlab directory
(of course, you need matlab for this) and Python support using SWiG
in the python directory.


I have not had the opportunity to try this on many different machines, 
so if there are difficulties, please either report them to golib@goschs.de
or fix them and send the fix to the same address.

Examples
--------
To build the examples, you will have to build the library first,
then go to the golib/examples directory and use one of the provided makefiles, like this:
make -f <YourChosenMakefile>

More recent examples have their own sub-directories in the examples
directory, and mostly have CMakeLists.txt files for use with cmake.
You may have to edit some of these files to fit your machine, in particular
the paths where the libraries and include files are located.

Note there is also a pkg-config file created by cmake. It is called
 golib.pc
and is located in the golib base directory after cmake ran successfully.


Further reading
---------------
I suggest you go through the online documentation in the golib/doc
directory.

Contact
-------
Feel free to contact me at golib@goschs.de!


