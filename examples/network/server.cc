/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goserver.h>
#include <goconnection.h>
#include <gonetwork.h>
#include <gotypes.h>
#include <iostream.h>

#include <stdlib.h>
#include <stdio.h>

using namespace goNet;

/*
 * This server does not do anything.
 * It accepts connections and maintains them in the internal connection
 * list, that's all.
 */
int main(int argc, char* argv[]) 
{
  if (argc < 2)
  {
      printf ("Usage: %s <port-number>\n",argv[0]);
      return(2);
  }
  // Set up a server
  int port = atoi(argv[1]);
  goServer server(port);
  server.runServer();

  // give the server some time to set up
  int i;
  for (i = 0; i < 10000000; i++);

  // Connect to the server
  goConnection conn[10];

  for (i = 0; i < 10; i++)
    {
      conn[i].setRemoteName ("localhost");
      conn[i].setRemotePort (port);
      cout << "Linking connection " << i << " to localhost port " << port << endl;
      if (conn[i].link())
	{
	  cout << "Connection to localhost established." << endl;
	} else {
	  cout << "Connection to localhost not established." << endl;
	}
    }

  for (i = 0; i < 10; i++)
    {
      goString s;
      char num[10];
      s = "Hi, I am client number ";
      sprintf(&num[0],"%d",i);
      s += &num[0];
      cout << "Client " << i << " about to send string " << s << " to server..." << endl;
      goNetwork::sendData ((const void*)s.toCharPtr(), s.getSize() * sizeof(char), conn[i].getSocket());
      if (goNetwork::fail())
	{
	  cout << "Client " << i << " failed sending data." << endl;
	} else
	  {
	    cout << "\t...sent." << endl;
	  }
    }

  for (i = 0; i < 10; i++)
    {
      if (conn[i].close())
	{
	  cout << "Client " << i << " closed." << endl;
	} else 
	  {
	    cout << "Client " << i << " failed closing." << endl;
	  }
    }

  char c;
  cin >> c;
  server.stopServer();
  exit(1);
}
