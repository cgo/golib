#include <goeqsystem.h>

goEqSystem::goEqSystem (goSize_t y, goSize_t x) : goMatrix<goDouble> (x, y) {
  goIndex_t i;
  solution.resize (y);
  for (i = 0; i < y; i++) {
    solution[i] = 0;
  }
}

goEqSystem::~goEqSystem () {
}

bool 
goEqSystem::solve () {
  register goIndex_t x, x2, y, startY, startX;
  goIndex_t max_coeff = sizeX - 1;
  
  goDouble multValue = 0.0f;
  
  startY = 1;
  startX = 0;
  while ( startY < sizeY ) {
    for (y = startY; y < sizeY; y++) {
      if ((int)(*this)[y][startX] != 0) {
	multValue = (*this)[startY - 1][startX] / (float)(*this)[y][startX];
	(*this)[y][startX] = 0;
	for (x = (startX + 1); x < sizeX; x++) {
	  (*this)[y][x] = multValue * (*this)[y][x] - (*this)[y - 1][x];
	}
      }
    }
    startY++;
    startX++;
    cout << *this << endl;
  }

  solution[sizeY - 1] = (*this)[sizeY - 1][sizeX - 1] / (*this)[sizeY - 1][sizeX - 2];
  startY = sizeY - 2;
  startX = sizeX - 2;
  for (y = (sizeY - 2); y >= 0; y--) {
    for (x = (sizeX - 2); x >= startX; x--) {
      (*this)[y][sizeX - 1] -= (*this)[y][x] * solution[x];
    }
    startX++;
    solution[startY--] = (*this)[y][sizeX - 1] / (*this)[y][y];
  }
  return true;
}

ostream&
operator<< (ostream& o, goEqSystem& s) {
  goIndex_t x, y;
  for (y = 0; y < s.getSizeY(); y++) {
    for (x = 0; x < s.getSizeX(); x++) {
      o << s[y][x] << " ";
    }
    o << endl;
  }
  return o;
}
