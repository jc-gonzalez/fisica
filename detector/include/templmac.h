// This is -*- C++ -*-
// $Id$

//------------------------------------------------------------
// templmac -- template and macros for C++
//
// Kopyleft (K) 2000 J C Gonzalez
//------------------------------------------------------------

// @T \newpage

//!@section Source code of |templmac.h|.

//!@subsection Definitions.

//!@{

#ifndef TEMPLMAC_H
#define TEMPLMAC_H

//++
// utility functions/templates
//--

// define square of a number
template<class T>
inline T sqr(T x) { return (x*x); }

// define cube of a number
template<class T>
inline T cube(T x) { return (x*x*x); }

// define maximum of two numbers
template<class T>
inline T max(T a, T b) { return ((a>b) ? a : b); }

// define square of a number
template<class T>
inline T min(T a, T b) { return ((a<b) ? a : b); }

/*
template<class T>
inline T round(float x) { return static_cast<T>(floor(x + 0.5)); }

#define nint(x)   round<int>( (x) )
#define rint(x)   round<int>( (x) )
#define frint(x)  round<float>( (x) )
*/

inline int rint(float x) { return static_cast<int>(floor(x + 0.5)); }
inline float frint(float x) { return static_cast<float>(floor(x + 0.5)); }

#define nint(x)   rint( x )

#endif // ! TEMPLMAC_H

//!@}

//=------------------------------------------------------------
//!@subsection Log of this file.

//!@{

/*
 * $Log$
 * Revision 1.1.1.1  2000/03/22  15:25:13  gonzalez
 * Imported sources
 *
 * Revision 1.1  2000/02/11  19:36:35  gonzalez
 * *** empty log message ***
 *
 * Revision 1.12  2000/01/27  10:48:51  gonzalez
 * Trigger patterns + timing seem to work
 *
 */

//=EOF
