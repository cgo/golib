# 
# libGo spec file
#
Summary: libGo general class library
Name: golib
Version: 0.2.1
Release: 1
Copyright: GPL
Group: Development/Libraries
Source: golib-0.2.1.tgz
URL: http://www.goschs.de
Distribution: All
Vendor: goschs.de
Packager: Christian Gosch

%description 
libGo is a C++ class library containing all kinds of 
things that proved useful to me.
For details, see the source documentation in golib/doc.

%prep 
%setup 


%build
configure
make

%install
cp lib/libGo.so.0.2.1 /usr/local/lib
ln -s /usr/local/lib/libGo.so.0.2.1 /usr/local/lib/libGo.so
# make install

%files
/usr/local/lib/libGo.so.0.2.1
/usr/local/lib/libGo.so

%postun
#rmdir /usr/local/golib/include
#rmdir /usr/local/golib

