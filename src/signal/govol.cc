#include <govol.h>
#include <fstream.h>
#include <goerror.h>

void Vol::setVolBehaviour(unsigned int b)
{
	Vol::VolBehaviour = b;
}

unsigned int Vol::getVolBehaviour()
{ 
	return Vol::VolBehaviour; 
}

goInt32 Vol::DWTVGetDataType(const char* filename)
{
	ifstream infile;
	infile.open(filename, ios::bin|ios::in);
	if (infile.fail())
	{
		goError::print("Vol::DWTVGetDataType()","File cannot be opened.");
		return -1;
	}
  	infile.seekg((streamoff)(-sizeof(goInt32)), ios::end);
	goInt32 buff;
	infile.read((char*)&buff, sizeof(goInt32));
	infile.close();
	return buff;
}
