/****************************************************************
 * patterns -- calculate hex. values of charged pixels patterns
 * Kopyleft (K) 2000, J C Gonzalez
 *------------------------------------------------------------
 * This program reads the file patterns.dat and generates
 * comment and code blocks with its information
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define EXIT_NO_FILENAME    1
#define EXIT_CANNOT_OPEN    2
#define EXIT_END_OF_FILE    4


int main(int argc, char **argv)
{

  FILE *Finput;
  char cfilename[80];
  char cline[80];
  char chline[40];
  char ckey[40];
  int  ihex[7];
  int  ivalue[100];
  int  i, j, k;
  int  n = 0;
  int  m = 0;
  int  kk = 1;

  int idx = 0;
  char * ckeys[20];
  int icodes[20];
  int inumbers[20];
  
  char cbuffer[40960];
  char * p;

  /* if no arguments are give in the command line
   * show help and exit
   */
  if ( argc < 1 ) {
    puts("patterns - generate hexadecimal values of hexagonal patterns");
    puts("usage:     patterns  [filename]");
    exit( EXIT_NO_FILENAME );
  }

  /* get name of file from command line */
  strcpy( cfilename, argv[1] );

  /* try to open the file */
  Finput = fopen( cfilename, "rt" );

  /* if open fails, exit */
  if ( Finput == NULL )
    exit( EXIT_CANNOT_OPEN );

  puts("/*************************************************************/");
  puts("/* trigpatt.h                                                */");
  puts("/*                                                           */");
  puts("/* Output generated by program 'patterns.dat'                */");
  puts("/* List of possible trigger configurations                   */");
  puts("/*                                                           */");
  puts("/* (c) 2000 J C Gonzalez                                     */");
  puts("/*************************************************************/\n");

  printf("/* Command line: '# %s %s'   */\n\n", argv[0], argv[1]);
  
  /* loop over the whole file, until the end is reached */
  while ( ! feof( Finput ) ) {

    /* read spurious line */
    if ( fgets( cline, 80, Finput ) == NULL )
      break;

    /* printf( "[%s,%d,%d]\n", cline, n, m ); */

    /* read pattern */
    fgets( chline, 40, Finput );
    sscanf( chline, "%d %d", &ihex[3], &ihex[2]);
    fgets( chline, 40, Finput );
    sscanf( chline, "%d %d %d", &ihex[4], &ihex[0], &ihex[1]);
    fgets( chline, 40, Finput );
    sscanf( chline, "%d %d", &ihex[5], &ihex[6]);

    /* if there is a comment in the spurious line, show it now */
    if ( cline[0] == '#' ) {
          
      /* only print code block if first conf. block was scanned */
      if (n > 0) {

        icodes[idx] = kk;
        inumbers[idx] = n-m;
        ckeys[idx] = strdup(ckey);
        idx++;
        
        printf( "*/\n\nconst int e%-20s = %d;\n", ckey, kk ); 
        printf( "const int i%-20s = %d;\n", ckey, n-m ); 
        printf( "const int x%-20s[ i%-20s ] = \n  { 0x%02x", 
                 ckey, ckey, ivalue[m] );

        for (i=m+1; i<n; i++) 
          printf( ", 0x%02x", ivalue[i] );

        printf( " };\n\n" );

        kk = kk << 1;
        m = n;

      }
      
      printf("\n/*\n -- %s", cline+2);
      
      sscanf( cline+2, "%s", ckey );

    }

    /* generate hexagonal value */
    ivalue[n] = 0;
    for (i=0; i<7; i++)
      ivalue[n] |= (ihex[i] << i);

    /* print values */
    printf( "%3d:  b 0", n+1 );

    for (i=6; i>=0; i--)
      printf( " %01d", ihex[i] );

    printf( "   = 0x%02x\n", ivalue[n] );

    /* increment index */
    n++;

  }

  /* close file */
  fclose( Finput );

  icodes[idx] = kk;
  inumbers[idx] = n-m;
  ckeys[idx] = ckey;
  idx++;
  
  printf( "*/\n\nconst int e%-20s = %d;\n", ckey, kk ); 
  printf( "const int i%-20s = %d;\n", ckey, n-m ); 
  printf( "const int x%-20s[ i%-20s ] = \n  { 0x%02x", 
           ckey, ckey, ivalue[m] );

  for (i=m+1; i<n; i++) 
    printf( ", 0x%02x", ivalue[i] );
  
  printf( "};\n\nconst int iTP_IDs[] = { %d", icodes[0]);
  for (i=1; i<idx; i++) 
    printf( ", %d", icodes[i] );
  printf( " };\nconst int iTP_Numbers[] = { %d", inumbers[0]);
  for (i=1; i<idx; i++) 
    printf( ", %d", inumbers[i] );
  printf( " };\nconst int * iTP_Lists[] = { x%s", ckeys[0]);
  for (i=1; i<idx; i++) 
    printf( ",\n                            x%s", ckeys[i] );
  printf( " };\n\nconst int nTP_ExclusivePatterns = %d;\n\n\n/* EOF */\n", idx );
  
  /* end */
  return(0);

}

/* EOF */

