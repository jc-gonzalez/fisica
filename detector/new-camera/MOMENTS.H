//=//////////////////////////////////////////////////////////////////////
//=
//= imagestr -- classes with information about the image
//=
//= @file		 imagestr.h
//= @desc		 Classes with information about the image
//= @author 	 J C Gonzalez
//= @email		 gonzalez@mppmu.mpg.de
//= @date		 Thu May  7 16:24:22 1998
//=
//=----------------------------------------------------------------------
//= $RCSfile$
//= $Revision$
//= $Author$
//= $Date$
//=//////////////////////////////////////////////////////////////////////

// @T \newpage

//# Source code of |imagestr.h|.

//{
#ifndef IMAGESTR_H
#define IMAGESTR_H
//}

//## Class Moments.

//{

class Moments {

 private:

  // moments
  float m1x, m1y;						// first moments (mean)
  float m2xx, m2xy, m2yy;				// second moments (around origin)
  float m2cxx, m2cxy, m2cyy;			// second moments (around mean)
  float m3xxx, m3xxy, m3xyy, m3yyy; 	// third moments (around origin)
  float m3cxxx, m3cxxy, m3cxyy, m3cyyy; // third moments (around mean)

  // charges
  float charge; 						// total charge in the image
  float xmax, ymax; 					// position of the maximum
  float smax;							// charge of the block of maximum
  float maxs[10];						// charges of the first 10 max.
  int nmaxs[10];						// number of pixels of 10 max.

  // parameters of the image
  float length, width, dist, xdist, azw, miss, alpha, conc[9], beta;
  float phi, asymx, asymy;

};

//}


//## Class Islands.

//{

class Islands {

 private:

  float * fi;
  int	* isl;
  int	* islands;
  float * vislands;
  int	  numisl;

};

//}


//## Class LenWid.

//{

class LenWid {

 private:

  float length1;
  float length2;
  float width1;
  float width2;

};

//}

//{
#endif // ! IMAGESTR_H
//}

//=------------------------------------------------------------
//## Log of this file.

//{

/*
 *$Log$
 * Revision 1.6  1999/11/17  13:32:04  gonzalez
 * Nov.1999
 *
 * Revision 1.5  1999/10/05  11:42:35  gonzalez
 * Sep. 1999
 *
 * Revision 1.4  1999/03/15  14:59:10  gonzalez
 * camera-1_1
 *
 * Revision 1.3  1999/03/02  09:56:15  gonzalez
 * *** empty log message ***
 *
 */

//}
//=EOF
