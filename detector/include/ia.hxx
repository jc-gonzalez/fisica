#ifndef INIFINITE_ARRAY_H
#define INIFINITE_ARRAY_H

// 7/31/97 Added iterator.
// 7/28/97
// Need a list that keeps its elements in contiguous storage
// so that the start address can be passed to a routine that
// wants an array.

#include <iostream.h>

template<class T> class InfiniteArray;

typedef int Boolean;

template<class T>
class InfiniteArrayIterator
{
	public:
		InfiniteArrayIterator(InfiniteArray<T>& a)
		: array(a), ptr(a.data), endptr(a.data+a.sz)
		{}
		InfiniteArrayIterator(InfiniteArrayIterator<T>& i)
		: array(i.array), ptr(i.ptr), endptr(i.endptr)
		{}
		void init()
		{
			ptr=array.data;
			endptr=array.data+array.sz;
		}
		Boolean ok() 	// This was elaborated to allow for checking of
				// decremented iterators, but now it's inefficient.
				// Needs work.
		{ 	return (ptr<endptr) && (ptr>=array.data);
		}
		InfiniteArrayIterator<T>& operator++()
		{	++ptr;
			return *this;
		}
		InfiniteArrayIterator<T>& operator--()
		{	--ptr;
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
		Boolean operator!=(int i) const	// to allow for "while (i!=0) --i;"
		{	
			return ptr!=(array.data+i);
		}
		const T& operator()() const
		{	return *ptr;
		}
		T& operator()()
		{	return *ptr;
		}
		const T& succ() const		// successor element
		{	return *(ptr+1);
		}
		T& succ()			// successor element
		{	return *(ptr+1);
		}
		const T& pred() const		// predecessor element
		{	return *(ptr-1);
		}
		T& pred()
		{	return *(ptr-1);		// predecessor element
		}

	private:
		const InfiniteArray<T>& array;
		T* ptr;
		T* endptr;
};

template<class T>
class InfiniteArray
{
   friend InfiniteArrayIterator<T>;
   public:
	typedef InfiniteArrayIterator<T> iterator;

	InfiniteArray(int initialCapacity=10,int growthFactor=2)
	: icap(initialCapacity), capacity(initialCapacity), gf(growthFactor), sz(0)
	{	if(icap<1) icap=1;
		data = new T[icap];
		ptr = data;
	}
	~InfiniteArray()
	{	delete [] data;
	}

	InfiniteArray<T>( const InfiniteArray<T>& a )
	: capacity(a.capacity), gf(a.gf), sz(a.sz),
	  data( new T[capacity] ), ptr(data)
	{	T* ap=a.data;
		T* aptr=a.ptr;	
		while( ap!=aptr )
		{	*(ptr++)=*(ap++);
		}
	}
		
	InfiniteArray& operator=( const InfiniteArray<T>& a )
	{
		if( this==&a ) return *this;	// assignment to self

		delete [] data;
		capacity=a.capacity;
		gf=a.gf;
		sz=a.sz;
	  	data = new T[capacity];
		ptr = data;
		T* ap=a.data;
		T* aptr=a.ptr;	
		while( ap!=aptr )
		{	*(ptr++)=*(ap++);
		}
		return *this;
	}
		
	void append(const T& t)
	{	if(sz==capacity)
		{
cout << " Growing from " << capacity << " to " << capacity*gf << endl;
			capacity*=gf;
			T* tmp=new T[capacity];
			T* p1=data;
			T* p2=tmp;
			while(p1!=ptr)
			{	*(p2++)=*(p1++);
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

	T* startOfData()  { return data; }

	int size() const { return sz; }

	friend ostream& operator<<(ostream& o, const InfiniteArray<T>& a);

    private:
	int icap;
	int capacity;
	int gf;
	int sz;
	T* data;
	T* ptr;
	
};

template<class T>
ostream& operator<<(ostream& o, const InfiniteArray<T>& a)
{			
	o << "[ ";
	T* p1=a.data;
	T* p2=a.ptr;
	while(p1!=p2)
	{	o << *(p1++) << " ";
	}
	o << "]";
	return o;
}

#endif
