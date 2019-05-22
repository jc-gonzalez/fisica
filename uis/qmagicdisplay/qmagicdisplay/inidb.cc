/*************************************************************************
 *                                                                          
 * inidb.cc  -  description
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <fstream.h>

#include <errno.h>

#include "inidb.h"

#define False 0
#define True  1

//--------------------------------------------------------------------
// titlePos: get a section title position & length in a string.
//--------------------------------------------------------------------
INIDB::INIDB() {}

//--------------------------------------------------------------------
// titlePos: get a section title position & length in a string.
//--------------------------------------------------------------------
INIDB::INIDB(const char *fileName)
{
  _fileName = (char*)(fileName);
}

//--------------------------------------------------------------------
// titlePos: get a section title position & length in a string.
//--------------------------------------------------------------------
INIDB::~INIDB() {}

//--------------------------------------------------------------------
// titlePos: get a section title position & length in a string.
//--------------------------------------------------------------------
char* INIDB::titlePos( char *buf, int *len )
{
  char *p = buf, *q;

  while( *p && isspace(*p) ) p++;
  if( *p != '[' )
    return 0;

  q = p+1;
  while( *q && *q != ']' ) q++;
  if( *q != ']' )
    return 0;
  if( len )
    *len = int(q - p - 1);
  return p+1;
}

//--------------------------------------------------------------------
// isTitleLine: check if a string is a section title line
//--------------------------------------------------------------------
int INIDB::isTitleLine( char *bufPtr )
{
  return titlePos( bufPtr, 0 ) != 0;
}

//--------------------------------------------------------------------
// containTitle: check if a string contain a section a title
//--------------------------------------------------------------------
int INIDB::containTitle( char *buf, const char *section )
{
  char *p;
  int len;

  p = titlePos( buf, &len );
  if( p ) {
    if( (int(strlen( section )) == len) && (strncasecmp( section, p, len ) == 0) )
      return True;
  }
  return False;
}

//--------------------------------------------------------------------
// gotoSection: move file position to start line of a section
//--------------------------------------------------------------------
int INIDB::gotoSection( fstream &is, const char *section )
{
  char line[256];
  while( is.getline( line, 256 ) )
    if( containTitle( line, section ) )
      return True;
  return False;
}

//--------------------------------------------------------------------
// textPos: get content's position of a entry
//--------------------------------------------------------------------
char* INIDB::textPos( char *buf, const char *entry )
{
  if( buf[0] == ';' ) // it is comment line
    return 0;

  char *p = strchr( buf, '=' );
  if( !p )
    return 0;

  int len = int(p - buf);
  if( (int(strlen(entry)) == len) && (strncasecmp( buf, entry, len ) == 0) )
    return p+1;

  return 0;
}

//--------------------------------------------------------------------
// stripQuotationChar: strip a pair of quotation chars in a string
//--------------------------------------------------------------------
void INIDB::stripQuotationChar( char *buf )
{
  char *p;
  char *q;

  p = buf;
  while( *p && isspace(*p) ) p++;

  if( !(*p == '\"' || *p == '\'') )
    return;

  q = p+strlen(p);
  while( *q != *p && q > p ) q--;
  if( q == p )
    return;
  int len = int(q - p - 1);
  memmove( buf, p+1, len );
  buf[len] = 0;
}

//--------------------------------------------------------------------
// readEntry: read content of entry
//--------------------------------------------------------------------
int INIDB::readEntry( fstream &is, const char *entry,
                      char *buf, int bufSize,
                      int strip )
{
  char lineBuf[256];
  char *p, *cur;
  int  len;

  cur  = buf;
  *cur = '\0';
  len  = -1;
  while( is.getline( lineBuf, 256 ) ) {
    if( isTitleLine( lineBuf ) )       // section is ended
      break;
    
    p = textPos( lineBuf, entry );     // not equal this entry
    if( p == 0 )
      continue;
    
    if( strip )
      stripQuotationChar( p );
    
    len = strlen(p);
    if( bufSize-1 < len )
      len = bufSize-1;
    
    strncpy( cur, p, len );
    cur[len] = 0;
    break;
  }

  return len;
}

//--------------------------------------------------------------------
// getProfileString:
//--------------------------------------------------------------------
int INIDB::getProfileString( const char *section, const char *entry,
                             const char *defaultString,
                             char *buffer, int bufLen )
{
  fstream is( _fileName, ios::in|ios::nocreate );
  int len = -1;

  if( is && gotoSection( is, section ) )
    len = readEntry(is, entry, buffer, bufLen, True);

  if( len < 0 ) { //can not read entry, use default string 
    strncpy( buffer, defaultString, bufLen-1 );
    buffer[bufLen-1] = 0;
    len = strlen(buffer);
  }
  return len;
}

//----------------------------------------------------------------------------
// getProfileInt:
//----------------------------------------------------------------------------
int INIDB::getProfileInt( const char *section, const char *entry,
                          int defaultInt )
{
  char buf[256];
  char iBuf[34];   //"34" is max space "itoa" required under 32 bit C++
  
  /* itoa( defaultInt, iBuf, 10 ); */
  sprintf(iBuf, "%10d", defaultInt);
  getProfileString( section, entry, iBuf, buf, 256 );
  return atoi( buf );
}

void INIDB::writeEntry( fstream & os, const char *entry,
                        const char *string )
{
  os << entry << '=' << string << endl;
}

//--------------------------------------------------------------------
// writeProfileString:
//--------------------------------------------------------------------
int INIDB::writeProfileString( const char *section, const char *entry,
                               const char *string )
{
  /*
    char path [_MAX_PATH];
    char drive[_MAX_DRIVE];
    char dir  [_MAX_DIR];
    char file [_MAX_FNAME];
    char ext  [_MAX_EXT];
  */
  
  char tmpfile[] = "./tmpini.XXX";
  
  char buf  [256];

  int  titleFound, entryFound;

  /*
    // work better on network!
    _splitpath( path, drive, dir, file, ext );
    _makepath(  path, drive, dir, tmpnam(0), "" );
  */

  mkstemp(tmpfile);
  
  fstream is( _fileName, ios::in  );
  fstream os( tmpfile, ios::out );

  if( !os || entry == 0 )     //maybe can not create file or invalid entry
    return 0;

  titleFound = False;
  entryFound = False;
  while( is.getline(buf, 256) ) {
    os << buf << endl;
    if( containTitle(buf, section) ) {
      titleFound = True;
      break;
    }
  }
  
  if( !titleFound ) {   // add section

    os << '[' << section << ']' << endl;
    writeEntry( os, entry, string );

  } else {

    while( is.getline(buf, 256) ) {

      if( isTitleLine( buf ) ) // section ended, but still not found the entry
        break;
      
      if( textPos( buf, entry ) ) { // entry found, so rewrite it
        entryFound = True;
        break;
      }  

      os << buf << endl;
    }
    
    writeEntry( os, entry, string );
    
    if( is.gcount() > 0 && !entryFound )
      os << buf << endl;
    
    while( is.getline(buf, 256) )  // copy left lines
      os << buf << endl;
  }
  is.close();
  os.close();
  unlink( _fileName );

  if (rename( tmpfile, _fileName ) == -1)
    perror("rename");

  return strlen(string);
}




void INIDB::SetFilename( const char *fileName )
{
  _fileName = (char*)fileName;
}

void INIDB::Write( const char *section, const char *key, 
                   const char *valStr )
{
  writeProfileString( section, key, valStr );
}
  
void INIDB::Write( const char *section, const char *key, int val )
{
  char str[35];
  sprintf(str, "%d", val);
  writeProfileString( section, key, str);
}

void INIDB::Write( const char *section, const char *key, double val )
{
  char str[35];
  sprintf(str, "%g", val);
  writeProfileString( section, key, str);
}

void INIDB::Read( const char *section, const char *key, 
                  char *valStr, int maxBufLen )
{
  getProfileString( section, key, "", valStr, maxBufLen ); 
}

void INIDB::Read( const char *section, const char *key,
                  const char *defaultString,
                  char *valStr, int maxBufLen )
{
  getProfileString( section, key, defaultString, valStr, maxBufLen );
}

int INIDB::Read( const char *section, const char *key,
                 int defaultInt)
{
  return getProfileInt( section, key, defaultInt ); 
}

double INIDB::Read( const char *section, const char *key,
                    double defaultDouble )
{
  char number[] = "0.0                              ";
  char defaultString[35];
  float d;
  sprintf(defaultString, "%g", defaultDouble);
  getProfileString( section, key, defaultString, number, 32 );
  sscanf(number, "%g", &d);
  return double(d);
}
