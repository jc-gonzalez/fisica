#include <stdio.h>
#include <stdlib.h>
#include <iostream.hxx>

#ifndef __MYMATRIX_H__
#define __MYMATRIX_H__

class MyMatrix {

public:
  MyMatrix(int n, int *d);
  ~MyMatrix();
  float item(int *it);
  float & operator[](int *it);
  float * foreach();

private:
  float *data;
  int dims[10];
  int cumdims[10];
  int dimension;
  int items;
  int nitem;

};
     
MyMatrix::MyMatrix(int n, int *d)
{
  int i;

  dimension = n;
  
  items = 1;
  for (i=0; i<n; ++i) 
    dims[i] = d[i];

  cumdims[n-1] = 1;
  for (i=n-2; i>=0; --i)
    cumdims[i] = cumdims[i+1] * dims[i+1];
  
  items = d[0]*cumdims[0];
  data = new float[items];
  nitem = 0;
}

MyMatrix::~MyMatrix()
{
  delete data;
  data = NULL;
}

float MyMatrix::item(int *it)
{
  register int i;
  int l = 0;

  for (i=0; i<dimension; ++i)
    l += it[i]*cumdims[i];

  return( data[l] );
}

float &MyMatrix::operator[](int *it)
{
  register int i;
  int l = 0;

  for (i=0; i<dimension; ++i)
    l += it[i]*cumdims[i];

  return( data[l] );
}

float * MyMatrix::foreach()
{
  if (nitem == items) {
    nitem = 0;
    return( NULL );
  } else {
    ++nitem;
    return( &data[nitem] );
  }
}

#endif
