
************************************************************
*
* sample
*
* @file        sample.f
* @title       Sample source file (Fortran)
* @desc        Sample source file, coded in Fortran.
* @author      J C Gonzalez
* @email       gonzalez@mppmu.mpg.de
*
* @maintitle
*************************************************************
*
* @tableofcontents

* @section About this program

* @text
*
*   This program is just a stupid loop, and it's only 
*   to show you that also Fortran files can be parsed 
*   by \SuS\ v 1.0
*
* @endtext 

* @section The code

* @subsection Main program

* @code

      program main

      integer i
      real x

      x=2

* @comment
*
*     This is a comment in the middle of the code. It could
*     be have written here, for example, to clarify some
*     special and/or tricky thing in the core of the code.
*
* @endcomment

      do 10 i=1,10
        x=x*i
 10   continue

      end

* @endcode

* @bye

      
