#include <stdio.h>
#include <string.h>
#include <math.h>
#include <gtk/gtk.h>

#include <camera.h>

#define CAMERA_DEFAULT_WIDTH   500
#define CAMERA_DEFAULT_HEIGHT  500

enum {
  CAMERA_SIGNAL,
  LAST_SIGNAL
};

static gint camera_signals[LAST_SIGNAL] = { 0 };

static GtkFrameClass *parent_class = NULL;

static void camera_class_init (CameraClass *class);
static void camera_init(Camera *ccc);

guint
camera_get_type ()
{
  static guint camera_type = 0;

  if (!camera_type) {
    GtkTypeInfo camera_info = 
    {
      "Camera",
      sizeof(Camera),
      sizeof(CameraClass),
      (GtkClassInitFunc) camera_class_init,
      (GtkObjectInitFunc) camera_init,
      (GtkArgSetFunc) NULL,
      (GtkArgGetFunc) NULL,
    };

    camera_type = gtk_type_unique (gtk_frame_get_type(), &camera_info);
  }
  
  return camera_type;
}

static void
camera_class_init (CameraClass *class)
{
  GtkObjectClass *object_class;

  object_class = (GtkObjectClass*) class;

  camera_signals[CAMERA_SIGNAL] = 
    gtk_signal_new ("camera",
                    GTK_RUN_FIRST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (CameraClass, camera),
                    gtk_signal_default_marshaller, GTK_TYPE_NONE, 0);

  gtk_object_class_add_signals (object_class, camera_signals, LAST_SIGNAL);

  class->camera = NULL;
}

static void
camera_init(Camera *ccc)
{
  GtkWidget *k;
  GtkWidget *fr;

  k = gtk_button_new();
  gtk_container_add(GTK_CONTAINER(ccc), k);
  gtk_widget_show( k );
  ccc->btn = GTK_WIDGET( k );
}

GtkWidget*
camera_new (void)
{
  return ( GTK_WIDGET( gtk_type_new( camera_get_type() ) ) );
}







 





  
