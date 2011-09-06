/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <network/goconnection.h>
#include <iostream.h>
#include <netinet/in.h>
#include <gostring.h>

int main (int argc, char* argv[]) {
  goConnection conn;
  in_addr addr;
  goString buffer;
  char buffer2[20];
  addr.s_addr = 0;
  conn.setMyAddress (addr);
  conn.setMyPort (atoi(argv[1]));
  conn.link(1);
  if (conn.fail()) {
    cout << "link() failed\n";
  }
  else { cout << "link() succeeded\n"; }
  buffer = "Ich bin connect2 !";
  conn.send ((const char*)buffer.toCharPtr(), buffer.getSize());
  buffer = "";
  buffer.resize (20);
  conn.recv (buffer2, 20);
  if (!conn.fail()) {
    cout << "received " << buffer2 << "\n";
  }
  exit (0);
}
