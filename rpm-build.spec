Summary: C++ general purpose library with signal processing elements.
Name: golib
Version: 0.1
Release: 0
Copyright: GPL
Group: Development/Libraries
Source: http://www.goschs.de
Patch: none
BuildRoot: /var/tmp/%{name}-buildroot


%description
This library provides convenient classes for accessing networks, doing
multithreading, handling arrays and strings and doing some image processing.
I wrote it in the course of my studies in computer science and beyond.

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT
install * $RPM_BUILD_ROOT/

%files
include
src
lib
doc
Makefile
Makefile.in
configure
configure.in
doxygen.config
