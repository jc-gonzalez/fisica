/* 
 * GtkCamerapixel Widget
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

  /* button */
  guint8 button;
  
  /* number of the pixel (starting from 0) */
  gint number;
  
  /* coordinates in the camera view */
  gint xc, yc;
  
  /* size of the pixel (large radius, or radius of the outer circle) */
  gfloat radius;
  
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
GtkWidget* gtk_camerapixel_new(gint number, 
                               gint nphe,
                               gfloat radius);
gint       gtk_camerapixel_get_nphe(GtkCamerapixel *camerapixel);
void       gtk_camerapixel_set_nphe(GtkCamerapixel *camerapixel,
                                    gint nphe);
gfloat     gtk_camerapixel_get_radius(GtkCamerapixel *camerapixel);
void       gtk_camerapixel_set_radius(GtkCamerapixel *camerapixel,
                                      gfloat radius);
gint       gtk_camerapixel_get_trigger(GtkCamerapixel *camerapixel);
void       gtk_camerapixel_set_trigger(GtkCamerapixel *camerapixel,
                                       gint trigger);
gint       gtk_camerapixel_get_tailcut(GtkCamerapixel *camerapixel);
void       gtk_camerapixel_set_tailcut(GtkCamerapixel *camerapixel,
                                       gint tailcut);
gint       gtk_camerapixel_get_mainisland(GtkCamerapixel *camerapixel);
void       gtk_camerapixel_set_mainisland(GtkCamerapixel *camerapixel,
                                          gint mainisland);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_CAMERAPIXEL_H__ */
