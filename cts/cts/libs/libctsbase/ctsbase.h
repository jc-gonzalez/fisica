/*
 *    MACROS and DEFINITIONS used by various programs
 *
 */

/* prevent multiple includes
 */
#ifndef CTSBASE_H
#define CTSBASE_H 1

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
#define cts_merror(a, b)                                                     \
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
extern void *cts_pmem_alloc (size_t, char *);
extern void *cts_pmem_realloc (void *, size_t, char *);
extern void cts_vfree (void *);

#define cts_mmalloc(a, b, c) \
 ((a *) cts_pmem_alloc ((size_t) (b) * sizeof (a), c))

#define cts_mrealloc(a, b, c, d) \
 ((b *) cts_pmem_realloc ((void *) a, (size_t) (c) * sizeof (b), d))

#define cts_mfree(a) cts_vfree ((void *) a)


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

/*
 * file access functions
 */
extern void cts_vopen_file (FILE_TYPE *);

extern void cts_vclose_file (FILE_TYPE *);

/* associate stream to file 
 */
extern void cts_vset_stream (FILE_TYPE *, FILE *);

/* reads in data from a file and puts it into arrays (each column into
 * one array)
 */
extern void cts_vget_dat_file (int, char *, double ***, int *);

/* checks fscanf return code whether:
 *     - exactly number of values is read;
 *     - an unexpected EOF occured;
 */
#define cts_mcheck_ifs(a, b, c) if ((a) != (b) || (a) == EOF)            \
 cts_merror ("%s: invalid number of args or unexpected EOF read\n", c);  \
 else ;



/*
 * functions to reset arrays to '0'
 */
extern void cts_vreset_darray (double *, int);

extern void cts_vreset_farray (float *, int);

extern void cts_vreset_iarray (int *, int);

/*
 * functions to convert string <=> numbers
 */
/* modified strtod and strtol routines
 */
extern double cts_dstrtod (char *);
extern long cts_lstrtol (char *, int);

/* converge a real number into a string
 */
extern char *cts_preal_to_str (double, int, char *);

/* converge a (long) int into a string
 */
extern char *cts_pint_to_str (long int, char *);

/* append several strings
 */
extern char *cts_pstrcat (int, char *, ...);

/* sorting stuff
 */
typedef struct sort_list
{
  int index;               /* index for block of sorting data */
  double dsval;            /* value which is sorted */
  struct sort_list *p2prev;
  struct sort_list *p2next;
} sorting_list;

typedef struct sort_list SORT_LIST;

/* function sorts set of data arrays(this is a simple version, but
 * should be enough for small data-samples or partly sorted data)
 */
extern void cts_vsort_dat (double **, int, int, int, bool);



/*
 * miscs
 *
 */
/* function to check flags, passed via command line
 */
extern int cts_icheck_flag (char *, char *_str, int);

/* this array is used to have an encoding (ASCII, EBCDIC) independent
 * relation between ints and chars
 */
#define cABC "abcdefghijklmnopqrstuvwxyz"

/* numeric escape sequences
 */
#define NUL '\0'
#define EOT '\004'
#define ACK '\006'
#define NAK '\025'

#endif /* CTSBASE_H */
