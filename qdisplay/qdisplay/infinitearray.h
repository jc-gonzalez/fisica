/***************************************************************************
                          infinitearray.h  -  description
                             -------------------
    begin                : Sat Jan 15 2000
    copyright            : (C) 2000 by Jose Carlos Gonzalez
    email                : gonzalez@mppmu.mpg.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef INFINITEARRAY_H
#define INFINITEARRAY_H

#include <iostream.h>

template<class T> class InfiniteArray;

typedef int Boolean;

/**Provides an infinitely growing array, depending on the needs
  *@author unknown
  */

template<class T>
class InfiniteArrayIterator
{
public:
  InfiniteArrayIterator(InfiniteArray<T>& a)
    : array(a), ptr(a.data), endptr(a.data+a.sz) {}

  InfiniteArrayIterator(InfiniteArrayIterator<T>& i)
    : array(i.array), ptr(i.ptr), endptr(i.endptr) {}

  void init()
  {
    ptr=array.data;
    endptr=array.data+array.sz;
  }

  Boolean ok()
  {
    // This was elaborated to allow for checking of
    // decremented iterators, but now it's inefficient.
    // Needs work.
    return (ptr<endptr) && (ptr>=array.data);
  }

  InfiniteArrayIterator<T>& operator++()
  {
    ++ptr;
    return *this;
  }

  InfiniteArrayIterator<T>& operator--()
  {
    --ptr;
    return *this;
  }

  Boolean operator==(const InfiniteArrayIterator<T>& j) const
  {
    return ptr==j.ptr;
  }

  Boolean operator!=(const InfiniteArrayIterator<T>& j) const
  {
    return ptr!=j.ptr;
  }

  Boolean operator==(int i) const
  {
      return ptr==(array.data+i);
  }

  Boolean operator!=(int i) const // to allow for "while (i!=0) --i;"
  {
    return ptr!=(array.data+i);
  }

  const T& operator()() const
  {
    return *ptr;
  }

  T& operator()()
  {
    return *ptr;
  }

  const T& succ() const   // successor element
  {
    return *(ptr+1);
  }

  T& succ()     // successor element
  {
    return *(ptr+1);
  }

  const T& pred() const   // predecessor element
  {
    return *(ptr-1);
  }

  T& pred()              // predecessor element
  {
    return *(ptr-1);
  }

  private:
    const InfiniteArray<T>& array;
    T* ptr;
    T* endptr;
};

/**Simple function to dump the contents of the array in stdout
  *@author unknown
  */
template<class T>
ostream& operator<<(ostream& o, const InfiniteArray<T>& a)
{
  o << "[ ";
  T* p1=a.data;
  T* p2=a.ptr;
  while(p1!=p2) {
    o << *(p1++) << " ";
  }
  o << "]";
  return o;
}


/**Provides an infinitely growing array, depending on the needs
  *@author unknown
  */
template<class T>
class InfiniteArray
{
  friend InfiniteArrayIterator<T>;
public:
  typedef InfiniteArrayIterator<T> iterator;

  InfiniteArray(int initialCapacity=10,int growthFactor=2)
    : icap(initialCapacity), capacity(initialCapacity), gf(growthFactor), sz(0)
  {
    if(icap<1) icap=1;
    data = new T[icap];
    ptr = data;
  }

  ~InfiniteArray()
  {
    delete [] data;
  }

  InfiniteArray<T>( const InfiniteArray<T>& a )
    : capacity(a.capacity), gf(a.gf), sz(a.sz),
      data( new T[capacity] ), ptr(data)
  {
    T* ap=a.data;
    T* aptr=a.ptr;
    while( ap!=aptr ) {
      *(ptr++) = *(ap++);
    }
  }

  InfiniteArray& operator=( const InfiniteArray<T>& a )
  {
    if( this==&a ) return *this;  // assignment to self

    delete [] data;
    capacity=a.capacity;
    gf=a.gf;
    sz=a.sz;
    data = new T[capacity];
    ptr = data;
    T* ap=a.data;
    T* aptr=a.ptr;
    while( ap!=aptr ) {
      *(ptr++) = *(ap++);
    }
    return *this;
  }

  void append(const T& t)
  {
    if(sz==capacity) {
      cout << " Growing from " << capacity
           << " to " << capacity*gf << endl;
      capacity*=gf;
      T* tmp=new T[capacity];
      T* p1=data;
      T* p2=tmp;
      while(p1!=ptr) {
        *(p2++)=*(p1++);
      }
      delete [] data;
      data=tmp;
      ptr=data+sz;
    }
    *(ptr++)=t;
    ++sz;
  }

  void clear()
  {
    delete [] data;
    capacity=icap;
    sz=0;
    data = new T[capacity];
    ptr = data;
  }

  const T& operator[](int i) const { return *(data+i); }

  T& operator[](int i) { return *(data+i); }

  const T& at(int i) const { return *(data+i); }

  T& at(int i) { return *(data+i); }

  T* startOfData()  { return data; }

  int size() const { return sz; }

  friend ostream& operator<< <>(ostream& o, const InfiniteArray<T>& a);

private:
  int icap;
  int capacity;
  int gf;
  int sz;
  T* data;
  T* ptr;

};

#endif // INFINITEARRAY_H

/* EOF */
