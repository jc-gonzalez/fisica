/*
 *    MACROS and DEFINITIONS used by various programs
 *
 * (if necessary do-while is used to allow semicolons after the macro call
 *  in the source code)
 */

/* prevent multiple includes
 */
#ifndef MYDEFS_H
#define MYDEFS_H 1

#include <stdio.h>
#include <string.h>


#ifndef __cplusplus

/* we're not dealing with C++
 * if true was not defined before define boolean stuff
 */
#ifndef true
typedef int bool;
#define true 1
#define false 0
#endif

#endif



/*
 * error handling stuff
 *
 */
#define iERRCODE 1
#define merror(a, b)                                                         \
 do { fprintf (stderr, a, b);                                                \
      fprintf (stderr, "file: %s\t line: %d\n", __FILE__, __LINE__);         \
      exit (iERRCODE);                                                       \
    }                                                                        \
 while (0)


/*
 * malloc stuff
 *
 */
/* linked list to remember all mallocs;
 * used for free() at exit time
 */
typedef struct mlc_handler
{
  void *pmem;
  struct mlc_handler *p2next;
} mlc_handler_list;

typedef struct mlc_handler MLC_HANDLER;

/* functions for allocating memory
 * last argument is an error string
 */
#define pmem_alloc pmemallc
extern void *pmem_alloc (size_t, char *);
#define pmem_realloc pmreallc
extern void *pmem_realloc (void *, size_t, char *);
extern void vfree (void *);

#define mmalloc(a, b, c) ((a *) pmem_alloc ((size_t) (b) * sizeof (a), c))

#define mrealloc(a, b, c, d) \
 ((b *) pmem_realloc ((void *) a, (size_t) (c) * sizeof (b), d))

#define mfree(a) vfree ((void *) a)

/*
 * file access stuff
 *
 */
/* struct to handle file access
 */
typedef struct   
{
  char *pname;
  char *acm;
  FILE *pfile;
} FILE_TYPE;

/* file access functions
 */
#define vopen_file vop_file
extern void vopen_file (FILE_TYPE *);

#define vclose_file vcl_file
extern void vclose_file (FILE_TYPE *);

/* associate stream to file 
 */
#define vreopen_stream vrostrm
extern void vreopen_stream (FILE_TYPE *, FILE *);

/* reads in data from a file and puts it into arrays (each column into
 * one array)
 */
#define vread_data_file vrdatfil
extern void vread_data_file (int, char *, double ***, int *);

/* reset the passed array to '0.'
 */
#define vreset_array vres_arr
extern void vreset_array (double *, int);

/* reset the passed array to '0.f'
 */
#define vreset_farray vres_far
extern void vreset_farray (float *, int);

/* reset the passed array to '0'
 */
#define vreset_iarray vres_iar
extern void vreset_iarray (int *, int);

/* modified strtod and strtol routines
 */
extern double dstrtod (char *);
extern long lstrtol (char *, int);

/* converge a real number into a string
 */
#define creal_to_str creal2st
extern char *creal_to_str (double, int, char *);

/* converge a (long) int into a string
 */
#define cint_to_str cint2str
char *cint_to_str (long int, char *);



/*
 * miscs
 *
 */
#define max(a, b)  ((a) > (b) ? (a) : (b))

#define min(a, b)  ((a) < (b) ? (a) : (b))

#define m2(a) ((a)*(a))

#define msign(a) ((a) > 0. ? 1. : ((a) < 0. ? -1. : 0.))

#define mdiv(a,b) \
  ((fabs ((double)(b)) > dEPSILON) ? (double) (a) / (double) (b) : 0.)

/* returns true if c lies within a and b
 */
#define mc_in_ab(a, b, c)  (((a) < (c)) && ((c) < (b)))


/* this array is used to have an encoding (ASCII, EBCDIC) independent
 * relation between ints and chars
 */
#define cABC "abcdefghijklmnopqrstuvwxyz"

/* some constants
 */
#define d2PI         6.28318530717958647692
#define dPIDIV180    0.017453292519943295769
#define dSQRT_PIDIV2 1.2533141373155002512
#define d180DIVPI    57.295779513082320877
#define dEPSILON     1e-16

/* numeric escape sequences
 */
#define NUL '\0'
#define EOT '\004'
#define ACK '\006'
#define NAK '\025'

#endif /* MYDEFS_H */
