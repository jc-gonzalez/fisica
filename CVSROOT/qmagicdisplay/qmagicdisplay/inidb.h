/*************************************************************************
*                                                                          
* inidb.h  -  description
*
* Copyright (C) 2001  J C Gonzalez
* gonzalez@gae.ucm.es
*
*------------------------------------------------------------------------
*
* Copyright (C) 2001 J C Gonzalez
*  
* This program is free software;  you can redistribute it and/or  modify
* it under the terms  of the GNU General  Public License as published by
* the Free Software Foundation; either version  2 of the License, or (at
* your option) any later version.
* 
* This piece of code is distributed in the hope  that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS
* FOR A PARTICULAR PURPOSE.
* 
* In no  event shall his author  be liable for  any special, incidental,
* indirect  or  consequential  damages  of any  kind,   or  any  damages
* whatsoever resulting from loss of use, data or profits, whether or not
* advised of the possibility of damage, and on  any theory of liability,
* arising out  of or in connection  with the use  or performance of this
* software. You've been warned.
* 
************************************************************************/

#ifndef _INIDB_H 
#define _INIDB_H

#include <string>
#include <iostream>
#include <strstream>
#include <vector>

class fstream;

class INIDB
{
public:  
  INIDB();
  INIDB(const char *fileName);
  ~INIDB();

public:
  void SetFilename( const char *fileName );

  void Write( const char *section, const char *key, 
              const char *valStr );

  void Write( const char *section, const char *key, int val );

  void Write( const char *section, const char *key, double val );

  void Read( const char *section, const char *key, 
             char *valStr, int maxBufLen );

  void Read( const char *section, const char *key, const char* defaultString,
             char *valStr, int maxBufLen );

  int Read( const char *section, const char *key,
            int defaultInt=0);

  double Read( const char *section, const char *key,
               double defaultDouble=0.0);

public:

  int getProfileInt( const char *section, const char *entry,
                     int defaultInt );

  int getProfileString( const char *section, const char *entry,
                        const char *defaultString,
                        char *buffer, int bufLen );

  int writeProfileString( const char *section, const char *entry,
                          const char *string );

protected:
  char* titlePos( char *buf, int *len );
  int   isTitleLine( char *bufPtr );
  int   containTitle( char *buf, const char *section );
  int   gotoSection( fstream &is, const char *section );
  char* textPos( char *buf, const char *entry );
  void  stripQuotationChar( char *buf );
  int   readEntry( fstream &is, const char *entry,
                   char *buf, int bufSize, int strip );
  void  writeEntry( fstream & os, const char *entry,
                    const char *string );

private:
  char* _fileName;
};


class IOAble {
public:
  virtual void Write(INIDB &db, const char *section)=0;
  virtual void Read(INIDB &db, const char *section)=0;
  virtual void Print()=0;
};

class IOParamBlock
{
public:
  std::vector<IOAble *> ioVars;
  void AddVar(IOAble *v) { ioVars.push_back(v); }
  void WriteAll(INIDB &db, const char *section) { 
    unsigned int i; 
    for (i=0; i<ioVars.size(); i++) 
      ioVars[i]->Write(db, section);
  }
  void ReadAll(INIDB &db, const char *section) { 
    unsigned int i; 
    for (i=0; i<ioVars.size(); i++) 
      ioVars[i]->Read(db, section);
  }
  void Print() { 
    unsigned int i; 
    for (i=0; i<ioVars.size(); i++) 
      ioVars[i]->Print();
  }
};

/*
#define TYPEIS(a)  #a

typedef enum {
NAMEDVAL_TYPE_CHAR, 
NAMEDVAL_TYPE_SHORT, 
NAMEDVAL_TYPE_INT, 
NAMEDVAL_TYPE_LONG, 
NAMEDVAL_TYPE_FLOAT, 
NAMEDVAL_TYPE_DOUBLE, 
NAMEDVAL_TYPE_LONG_DOUBLE, 
NAMEDVAL_TYPE_STRING, 
NAMEDVAL_TYPE_CHARPTR }       TypeEnum;
*/

// Template class that wraps a value of type T and has a string as its name.
// Can also be inserted into IOParamBlock for automatic IO
template <class T> class NamedVal : public IOAble {
protected:
  std::string name;
  T val;
  /*
    static TypeEnum type;
  */

public:
  NamedVal(const char *varName, const T& initialVal=T())
    : name(varName), val(initialVal) {}
  NamedVal(const NamedVal &rhs)
  { name = rhs.name; val = rhs.val; type = rhs.type; }
  NamedVal(IOParamBlock *parent, const char *varName, const T& iVal=T())
    : name(varName), val(iVal)
  { parent->AddVar(this); }

  virtual ~NamedVal() {}

  // conversion operators to make this class look like T
  operator T & ()    { return val; }
  T* operator & ()   { return &val; }

  // access
  T& get()           { return val; }
  void set(T x)      { val = x; }

  // allow assignment of a value of type T
  NamedVal<T> & operator = (const T& rhs) {
    val=rhs; return *this; }

  const char *Name() { return name.c_str(); }

  void Print() { cout << name << "=" << val << endl; }

  virtual void Write(INIDB &db, const char *section) {
    char buf[256]; std::strstream strs;
    strs << val; strs >> buf; db.Write(section, Name(), buf);
  }

  virtual void Read(INIDB &db, const char *section) {
    char buf[256]; db.Read(section, Name(), buf, 256);
    std::strstream strs; strs << buf; strs >> val;
  }
};

/*
TypeEnum NamedVal<char>::type        = NAMEDVAL_TYPE_CHAR;       
TypeEnum NamedVal<short>::type       = NAMEDVAL_TYPE_SHORT;      
TypeEnum NamedVal<int>::type         = NAMEDVAL_TYPE_INT;        
TypeEnum NamedVal<long>::type        = NAMEDVAL_TYPE_LONG;       
TypeEnum NamedVal<float>::type       = NAMEDVAL_TYPE_FLOAT;      
TypeEnum NamedVal<double>::type      = NAMEDVAL_TYPE_DOUBLE;     
TypeEnum NamedVal<long double>::type = NAMEDVAL_TYPE_LONG_DOUBLE;
TypeEnum NamedVal<string>::type      = NAMEDVAL_TYPE_STRING;    
TypeEnum NamedVal<char*>::type       = NAMEDVAL_TYPE_CHARPTR;
*/

void NamedVal<string>::Read(INIDB &db, const char *section) {
  char buf[256]; db.Read(section, Name(), buf, 256);
  val.assign(buf);
}

void NamedVal<char*>::Read(INIDB &db, const char *section) {
  char buf[256]; db.Read(section, Name(), buf, 256);
  strcpy(val,buf);
}

void NamedVal<string>::Write(INIDB &db, const char *section) {
  db.Write(section, Name(), val.c_str());
}

void NamedVal<char*>::Write(INIDB &db, const char *section) {
  db.Write(section, Name(), val);
}

#define DECLARE_INTERFACE(C,T,V)  \
T    get_##V(void); \
void set_##V(T _x);

#define DEFINE_IMPLEMENTATION(C,T,V)  \
inline T    C##::get_##V(void) { return V; } \
inline void C##::set_##V(T _x) { V = _x; }

// declare a named variable
#define DECL_NAMED_VAL(type,name) NamedVal<type> name

#define DECL_INTERFACE_NAMED_VAL(type,name) \
private:\
NamedVal<type> name; \
public:\
type get_##name(void) { return (name##.get()); } \
const char* get_##name##_name(void) { return (name##.Name()); } \
void set_##name(type _x) { name = _x; }

// define, construct, allocate a named variable
#define DEF_NAMED_VAL(type,name) NamedVal<type> name(#name)
#define DEF_NAMED_VAL_I(type,name,val) NamedVal<type> name(#name,val)

#define CONSTRUCT_NAMED_VAL(name) name(#name)
#define CONSTRUCT_NAMED_VAL_I(name, val) name(#name, val)

#define NEW_NAMED_VAL(type,name) name=new NamedVal<type>(#name)
#define NEW_NAMED_VAL_I(type,name,val)  name=new NamedVal<type> (#name,val)

// construct a NamedVal and insert into IOParamBlock
#define CONSTRUCT_AUTOIO_VAL(name, val) name(this, #name, val)

// construct a NamedVal and insert into IOParamBlock
#define INTERFACE_NAMEDVAL(type,name) name(this, #name, val)


#endif /* _INIDB_H */

// Local Variables:
// mode: c++
// End:
//EOF
