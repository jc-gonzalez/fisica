#include <stdio.h>
//extern "C" {
//#include "system_declaration.h"
//#include "resize_array.h"
//#include "reduced_file_sys.h"
//}
#include "MCEventHeader.hxx"

#define PROGRAM camera
#define VERSION 0.2

#define GLUE_prep(x,y) #x" "#y
char SIGNATURE[] = GLUE_prep( PROGRAM,VERSION ); 

void main( int argc , char *argv[] )
{
  FILE *f,*fOut;
  int i,iResult=1, iTel = 2;
  MCEventHeader head;
  float a[919];
  float fTest;
  char pcTemp[256];
  int iHeaderWritten = FALSE , iEvent = 0;

  printf("%d \n",sizeof( MCEventHeader ));

  if( argc != 3 ) {
     printf( "usage : magic2ascii <infile> <outfile>\n" );
     exit( 1 );     
  }

       
  f = fopen( argv[1] , "r" );
 
  if( f ) {
    fread( pcTemp , 1 , 11 , f );
    

    printf( "Version string : %s\n" , pcTemp );    

    fOut = fopen( argv[2] , "w" );

    if( fOut ) {

      while( iResult ) {

        iResult = fread( &head , 1 , head.mysize() , f );
        //        iResult = fread( &head , 1 , sizeof( MCEventHeader ) , f );
        //printf( "Energy : %f\n" , head.get_energy() );
        //if( head.get_trigger() ) {
           iEvent ++;

           fread( a , sizeof( float ) , 919 , f  );

           for( i = 0 ; i < 919 ; i++ ) {
              fprintf( fOut ,  "%d %d %f\n" , iEvent , i , a[i] );
           }
           //}

      }
      
      fclose( fOut );
    }
    fclose( f );
  } 
}

