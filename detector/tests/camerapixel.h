/* 
 * CAMERAPIXEL Widget
 */
#ifndef __GTK_CAMERAPIXEL_H__
#define __GTK_CAMERAPIXEL_H__


#include <gdk/gdk.h>
#include <gtk/gtkwidget.h>


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GTK_CAMERAPIXEL(obj)          GTK_CHECK_CAST (obj, gtk_camerapixel_get_type (), GtkCamerapixel)
#define GTK_CAMERAPIXEL_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, gtk_camerapixel_get_type (), GtkCamerapixelClass)
#define GTK_IS_CAMERAPIXEL(obj)       GTK_CHECK_TYPE (obj, gtk_camerapixel_get_type ())


typedef struct _GtkCamerapixel       GtkCamerapixel;
typedef struct _GtkCamerapixelClass  GtkCamerapixelClass;

struct _GtkCamerapixel
{
  GtkWidget widget;

  /* number of the pixel (starting from 0) */
  gint number;
  
  /* coordinates in the camera view */
  gint xc, yc;
  
  /* charge in that pixel */
  gint nphe;
  
  /* distribution of charge (histogram) */
  /* gint *phedist; */

  /* is this pixel over threshold? */
  guint trigger : 1;
  
  /* is this pixel over tail-cut? */
  guint tailcut : 1;
  
  /* is this pixel belonging to the main island? */
  guint mainisland : 1;  
};

struct _GtkCamerapixelClass
{
  GtkWidgetClass parent_class;
};


guint      gtk_camerapixel_get_type   (void);
GtkWidget* gtk_camerapixel_new        (gint number, gint ncph);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_CAMERAPIXEL_H__ */
