/*-----------------------------------------------------------*/
/* trigpatt.h                                                */
/*                                                           */
/* Output generated by program 'patterns.dat'                */
/* List of possible trigger configurations                   */
/*                                                           */
/* (c) 2000 J C Gonzalez                                     */
/*-----------------------------------------------------------*/

/* Command line: '# patterns patterns.dat'   */


#ifndef TRIGGER_PATTERNS
#define TRIGGER_PATTERNS


/*
 -- Three_Pixels_Not_NN : 3 pixels (9 conf.)
  1:  b 0 0 0 0 1 0 1 1   = 0x0b
  2:  b 0 0 0 1 0 1 0 1   = 0x15
  3:  b 0 0 1 0 1 0 0 1   = 0x29
  4:  b 0 1 0 1 0 0 0 1   = 0x51
  5:  b 0 0 1 0 0 0 1 1   = 0x23
  6:  b 0 1 0 0 0 1 0 1   = 0x45
  7:  b 0 0 0 1 0 0 1 1   = 0x13
  8:  b 0 0 1 0 0 1 0 1   = 0x25
  9:  b 0 1 0 0 1 0 0 1   = 0x49
*/

const int eThree_Pixels_Not_NN  = 1;
const int iThree_Pixels_Not_NN  = 9;
const int xThree_Pixels_Not_NN [ iThree_Pixels_Not_NN  ] = 
  { 0x0b, 0x15, 0x29, 0x51, 0x23, 0x45, 0x13, 0x25, 0x49 };


/*
 -- Three_Pixels_NN : 3 pixels, 2 NN (6 conf.)
 10:  b 0 0 0 0 0 1 1 1   = 0x07
 11:  b 0 0 0 0 1 1 0 1   = 0x0d
 12:  b 0 0 0 1 1 0 0 1   = 0x19
 13:  b 0 0 1 1 0 0 0 1   = 0x31
 14:  b 0 1 1 0 0 0 0 1   = 0x61
 15:  b 0 1 0 0 0 0 1 1   = 0x43
*/

const int eThree_Pixels_NN      = 2;
const int iThree_Pixels_NN      = 6;
const int xThree_Pixels_NN     [ iThree_Pixels_NN      ] = 
  { 0x07, 0x0d, 0x19, 0x31, 0x61, 0x43 };


/*
 -- Four_Pixels_Not_NN : 4 pixels in an "opened" configuration (12 conf.)
 16:  b 0 0 0 1 0 1 1 1   = 0x17
 17:  b 0 0 1 0 1 1 0 1   = 0x2d
 18:  b 0 1 0 1 1 0 0 1   = 0x59
 19:  b 0 0 1 1 0 0 1 1   = 0x33
 20:  b 0 1 1 0 0 1 0 1   = 0x65
 21:  b 0 1 0 0 1 0 1 1   = 0x4b
 22:  b 0 0 0 1 1 0 1 1   = 0x1b
 23:  b 0 0 1 1 0 1 0 1   = 0x35
 24:  b 0 1 1 0 1 0 0 1   = 0x69
 25:  b 0 1 0 1 0 0 1 1   = 0x53
 26:  b 0 0 1 0 0 1 1 1   = 0x27
 27:  b 0 1 0 0 1 1 0 1   = 0x4d
*/

const int eFour_Pixels_Not_NN   = 4;
const int iFour_Pixels_Not_NN   = 12;
const int xFour_Pixels_Not_NN  [ iFour_Pixels_Not_NN   ] = 
  { 0x17, 0x2d, 0x59, 0x33, 0x65, 0x4b, 0x1b, 0x35, 0x69, 0x53, 0x27, 0x4d };


/*
 -- Four_Pixels_NN : 4 pixels in a "closed packet", 2 NN (6 conf.)
 28:  b 0 0 0 0 1 1 1 1   = 0x0f
 29:  b 0 0 0 1 1 1 0 1   = 0x1d
 30:  b 0 0 1 1 1 0 0 1   = 0x39
 31:  b 0 1 1 1 0 0 0 1   = 0x71
 32:  b 0 1 1 0 0 0 1 1   = 0x63
 33:  b 0 1 0 0 0 1 1 1   = 0x47
*/

const int eFour_Pixels_NN       = 8;
const int iFour_Pixels_NN       = 6;
const int xFour_Pixels_NN      [ iFour_Pixels_NN       ] = 
  { 0x0f, 0x1d, 0x39, 0x71, 0x63, 0x47 };


/*
 -- Five_Pixels_Not_NN : 5 pixels, opened configuration (9 conf.)
 34:  b 0 0 1 1 1 0 1 1   = 0x3b
 35:  b 0 1 1 1 0 1 0 1   = 0x75
 36:  b 0 1 1 0 1 0 1 1   = 0x6b
 37:  b 0 1 0 1 0 1 1 1   = 0x57
 38:  b 0 0 1 0 1 1 1 1   = 0x2f
 39:  b 0 1 0 1 1 1 0 1   = 0x5d
 40:  b 0 0 1 1 0 1 1 1   = 0x37
 41:  b 0 1 1 0 1 1 0 1   = 0x6d
 42:  b 0 1 0 1 1 0 1 1   = 0x5b
*/

const int eFive_Pixels_Not_NN   = 16;
const int iFive_Pixels_Not_NN   = 9;
const int xFive_Pixels_Not_NN  [ iFive_Pixels_Not_NN   ] = 
  { 0x3b, 0x75, 0x6b, 0x57, 0x2f, 0x5d, 0x37, 0x6d, 0x5b };


/*
 -- Five_Pixels_NN : 5 pixels, "closed packet" (6 conf.)
 43:  b 0 0 0 1 1 1 1 1   = 0x1f
 44:  b 0 0 1 1 1 1 0 1   = 0x3d
 45:  b 0 1 1 1 1 0 0 1   = 0x79
 46:  b 0 1 1 1 0 0 1 1   = 0x73
 47:  b 0 1 1 0 0 1 1 1   = 0x67
 48:  b 0 1 0 0 1 1 1 1   = 0x4f
*/

const int eFive_Pixels_NN       = 32;
const int iFive_Pixels_NN       = 6;
const int xFive_Pixels_NN      [ iFive_Pixels_NN       ] = 
  { 0x1f, 0x3d, 0x79, 0x73, 0x67, 0x4f};

const int iTP_IDs[] = { 1, 2, 4, 8, 16, 32 };
const int iTP_Numbers[] = { 9, 6, 12, 6, 9, 6 };
const int * iTP_Lists[] = { xThree_Pixels_Not_NN,
                            xThree_Pixels_NN,
                            xFour_Pixels_Not_NN,
                            xFour_Pixels_NN,
                            xFive_Pixels_Not_NN,
                            xFive_Pixels_NN };

const int nTP_ExclusivePatterns = 6;


#endif // TRIGGER_PATTERNS
/* EOF */
