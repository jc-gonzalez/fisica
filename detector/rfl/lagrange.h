/////////////////////////////////////////////////////////////////
//
// lagrange
//_______________________________________________________________
//
//  Created: Sun Jun 14 14:10:18 MET DST 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Macro for Lagrange interpolation of 3rd. order
//  Notes:   
//  
/////////////////////////////////////////////////////////////////


// @T \newpage

// @section Source code of {\tt lagrange.h}

// @code

//++
// Formula for Lagrange interpolation of 3rd. order
// x: value to be interpolated
// t: table(2xN), table[0]: abscissas, table[1]: ordinates
// n: higher value of abscissas, such that t[0][n] <= x
//--

#define Lagrange(t,n,x)   ((t[1][ (n) ]*((x-t[0][(n)+1])*(x-t[0][(n)+2]))/ \
                ((t[0][ (n) ]-t[0][(n)+1])*(t[0][ (n) ]-t[0][(n)+2])))+ \
               (t[1][(n)+1]*((x-t[0][ (n) ])*(x-t[0][(n)+2]))/ \
                ((t[0][(n)+1]-t[0][ (n) ])*(t[0][(n)+1]-t[0][(n)+2])))+ \
               (t[1][(n)+2]*((x-t[0][ (n) ])*(x-t[0][(n)+1]))/ \
                ((t[0][(n)+2]-t[0][ (n) ])*(t[0][(n)+2]-t[0][(n)+1]))) \
               )                             
  
//++
// Macro to find, and save in variable "m", the value of 
// "n" to be used in the "Lagrange{t,n,x)" macro
//--

#define FindLagrange(t,m,x)  {m = 0; while (t[0][++m] < x);} --m

// @endcode

// @subsection Sample program 

// @code

//////////////////////////////////////////////////////////////////////////////
// Here follows a sample program using this two macros
//////////////////////////////////////////////////////////////////////////////
// #include <iostream>
// #include "lagrange.h"
// 
// void main(void)
// {
//   float data[2][20];
//   int i, number;
//   float x, y;
// 
//   for (i=0; i<20; ++i) {
//     data[0][i] = i*10.;
//     data[1][i] = 3.0*data[0][i]*data[0][i];
//     cout << data[0][i] << ' ' << data[1][i] << '\n';
//   }
// 
//   while (1==1) {
//     cout << "Enter x = ";
//     cin >> x;
//     FindLagrange(data,number,x);
//     y = Lagrange(data,number,x);
//     cout << x << ' ' << y << '\n';
//   }
// }
//////////////////////////////////////////////////////////////////////////////   

// @endcode
