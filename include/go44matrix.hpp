template <class T>
go44Matrix<T>::go44Matrix () {
  matrix = new T[16];
}


template <class T>
go44Matrix<T>::go44Matrix (T i11, T i12, T i13, T i14,
			   T i21, T i22, T i23, T i24,
			   T i31, T i32, T i33, T i34,
			   T i41, T i42, T i43, T i44) {
  matrix = new T[16];
  matrix[0] = i11;
  matrix[1] = i12;	
  matrix[2] = i13;
  matrix[3] = i14;
  matrix[4] = i21;
  matrix[5] = i22;	
  matrix[6] = i23;
  matrix[7] = i24;
  matrix[8] = i31;
  matrix[9] = i32;	
  matrix[10] = i33;
  matrix[11] = i34;
  matrix[12] = i41;
  matrix[13] = i42;	
  matrix[14] = i43;
  matrix[15] = i44;
}

template <class T>
go44Matrix<T>::go44Matrix (const go44Matrix<T>& other)
{
  matrix = new T[16];
  (*this) = other;
}

template <class T>
go44Matrix<T>::go44Matrix (go44Matrix<T>& other)
{
  matrix = new T[16];
  (*this) = (const go44Matrix<T>&)other;
}

template <class T>
go44Matrix<T>::~go44Matrix () {
  delete[] matrix;
}
