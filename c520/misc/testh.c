/********************************************************************** 
 ********************************************************************** 
 * testh                                                               
 *                                                                      
 *   Created: Tue Mar  3 10:18:13 1998
 *   Author.: Jose Carlos Gonzales
 *   Notes..: 
 *                                                                      
 ********************************************************************** 
 **********************************************************************/

/* System Header files */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>

/* User Header files */
#include "jchisto.h"
#include "jcmacros.h"

/* Pre-proc definitions */

/* Begin */

void
main(void)
{
  int i,j;
  float w,x;
  int hs[11], hc;
  char t[20];

  /* crear los histogramas */
  puts("Creo los histogramas");
  for (i=0;i<10;i++) {
    sprintf(t, "h%02d",i+1);
    hs[i] = jc_hcreate (t, 0., 1., 10);
    puts("Lo lleno...");
    /* llenarlo */
    for (j=0;j<10000;j++) {
      w = drand48();
      x = drand48();
      jc_hfill(hs[i], x, w);
    }
    puts("Lo ense~no");
    jc_hshow(hs[i]);
  }
      
  /* crea el acumulado */
  puts("Creo el acumulado");
  hc = jc_hcreate("Acumulado", 0., 1., 10);
  /* lo llena */
  puts("Lo lleno");
  for (i=0;i<10;i++) {
    jc_hcum (hs[i], hc);
  }
  puts("Lo ense~no");
  jc_hshow(hc);

  hs[10] = hc;

  /* calcula errores */
  puts("Calculo errores");
  for (i=0;i<=10;i++) 
    jc_hcalcstat ( hs[i] );
  
  /* los salva */
  puts("Los salvo en ASCII");
  jc_hsave_all ( "zzz%03d.hst" );  
  puts("Los salvo en binario");
  jc_hsavebin_all ( "zzz.hsb" );
  
  /* los borro todos * /
  puts("Los borro todos");
  for (i=10;i>=00;i--) 
    jc_hremove ( hs[i] );
  */
  /* intento leerlos * /
  puts("Los leo del binario");
  printf("%d histogramas leidos.\n", jc_hreadbin ( "zzz.hsb" ));
*/
  /* los salvo de nuevo, en formato octave */
  puts("Los salvo de nuevo para octave");
  jc_hsaveoc_all ( "zzz.oc" );  
}

