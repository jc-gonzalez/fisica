#include <stdio.h>
#include <math.h>
#include <gtk/gtkmain.h>
#include <gtk/gtksignal.h>

#include <camerapixel.h>


/* unit in VERTICES is RADIUS = APOT / COS(30) */

#define VERTICES_X    { \
 0.00000000, \
   0.86602540, \
   0.86602540, \
   0.00000000, \
   -0.86602540, \
   -0.86602540, \
    0.00000000 }

#define VERTICES_Y    { \
 1.00000000, \
   0.50000000, \
   -0.50000000, \
   -1.00000000, \
   -0.50000000, \
   0.50000000, \
   1.00000000 }

#define SQRT3OVER2      0.866025404


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



guint      gtk_camerapixel_get_type   (void);
GtkWidget* gtk_camerapixel_new        (gint number, gint ncph);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_CAMERAPIXEL_H__ */
