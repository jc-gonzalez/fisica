/* 
 * Camera Widget
 */
#ifndef __CAMERA_H__
#define __CAMERA_H__
  

#include <gdk/gdk.h>
#include <gtk/gtkframe.h>


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define CAMERA(obj)          GTK_CHECK_CAST (obj, camera_get_type(), Camera)
#define CAMERA_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, camera_get_type(), CameraClass)
#define IS_CAMERA(obj)       GTK_CHECK_TYPE (obj, camera_get_type())


typedef struct _Camera       Camera;
typedef struct _CameraClass  CameraClass;

struct _Camera
{
  GtkFrame frame;

  GtkWidget **pix;
  GtkWidget *btn;

  gfloat radius;
  gint npixels;
  gint *nphe;
  guint *trigger;
  guint *tailcut;
  guint *mainisland;

  guint8 button;
};

struct _CameraClass
{
  GtkFrame parent_class;
  
  void (* camera) (Camera *ccc);
};

guint      camera_get_type   (void);
GtkWidget* camera_new        (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __CAMERA_H__ */
