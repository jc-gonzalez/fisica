#ifndef VERSION 

#define PROGRAM camera
#define VERSION 0.2

#define GLUE_prep(x,y) #x" "#y
#define GLUE_postp(x,y) GLUE_prep(x,y)

const char SIGNATURE[] = GLUE_postp( PROGRAM, VERSION );

#endif // ! VERSION

