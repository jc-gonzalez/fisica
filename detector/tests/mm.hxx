#include <stdio.h>
#include <stdlib.h>
#include <iostream.hxx>

#ifndef __MYMATRIX_H__
#define __MYMATRIX_H__

template<class T>
class MyMatrix {

public:
  MyMatrix(int n, int *d);
  ~MyMatrix(void);
  T & operator[](int *it);
  T * foreach(void);
  void dump(void);
  void idx_reset(void);
  int idx_linear(int *it);
  void idx_incr(void);
  int * idx(void);
  
private:
  T *data;
  int dims[10];
  int cumdims[10];
  int dimension;
  int items;
  int nitem;
  int index[10];

};
     
template<class T>
MyMatrix<T>::MyMatrix(int n, int *d)
{
  register int i;

  dimension = n;
  
  items = 1;
  for (i=0; i<n; ++i) 
    dims[i] = d[i];

  cumdims[n-1] = 1;
  for (i=n-2; i>=0; --i)
    cumdims[i] = cumdims[i+1] * dims[i+1];
  
  items = d[0]*cumdims[0];
  data = new T[items];
  nitem = 0;
}

template<class T>
MyMatrix<T>::~MyMatrix(void)
{
  delete data;
  data = NULL;
}

template<class T>
void MyMatrix<T>::dump(void)
{
  register int i;
  
  cout << "dump::\n  {" << data[0] << "[0]";
  for (i=1; i<items; ++i) 
    cout << ',' << data[i] << '[' << i << ']';
  cout << '}' << endl << flush;
}

template<class T>
void MyMatrix<T>::idx_reset(void)
{
  register int i;
  for (i=0; i<dimension; ++i) 
    index[i] = 0;
  nitem = 0;
}

template<class T>
void MyMatrix<T>::idx_incr(void)
{
  register int i;
  int carry;
  
  for (i=dimension-1,carry=TRUE; 
       i>-1 && carry; 
       --i) 
    {
      index[i] = (index[i] + 1) % dims[i];
      if (index[i]>0)
        carry=FALSE;
    }
}

template<class T>
int * MyMatrix<T>::idx(void)
{
  return( index );
}

template<class T>
int MyMatrix<T>::idx_linear(int *it)
{
  register int i;
  int l;

  for (i=0,l=0; i<dimension; ++i)
    l += it[i]*cumdims[i];

  return( l );
}

template<class T>
T &MyMatrix<T>::operator[](int *it)
{
  return( data[ MyMatrix<T>::idx_linear(it) ] );
}

template<class T> 
T * MyMatrix<T>::foreach(void)
{
  if (nitem == items) {
    nitem = 0;
    return( NULL );
  } else {
    ++nitem;
    MyMatrix<T>::idx_incr();
    return( &data[nitem] );
  }
}

MyMatrix<float> *mymatrix_float = NULL;
MyMatrix<double> *mymatrix_double = NULL;
MyMatrix<int> *mymatrix_int = NULL;
MyMatrix<long> *mymatrix_long = NULL;

#endif
