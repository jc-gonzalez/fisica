#include <stdio.h>
#include <string.h>
#include <math.h>
#include <gtk/gtk.h>

#include "camera.h"
#include "pixelsinfo.h"

#include "hexsmall.xpm"
#include "hexbig.xpm"

/* unit in VERTICES is RADIUS = APOT / COS(30) */

static float VERTICES_X[7] =
{ 
  0.00000000, 
  0.86602540, 
  0.86602540, 
  0.00000000, 
  -0.86602540, 
  -0.86602540, 
   0.00000000 
};

static float VERTICES_Y[7] = 
{
  1.00000000, 
  0.50000000, 
  -0.50000000, 
  -1.00000000, 
  -0.50000000, 
  0.50000000, 
  1.00000000 
};

/* Backing pixmap for drawing area */
GdkPixmap *backing_pixmap = NULL;

/* Camera drawing area */
GtkWidget *camera;
gint NPixels;
gint npixels;
gint *nphe;
guint *trigger;
guint *tailcut;
guint *mainisland;

/* Center of the camera */
gint xc = CAMERA_DEFAULT_WIDTH / 2;
gint yc = CAMERA_DEFAULT_WIDTH / 2;

/* Number of pixels for the camera */
gint npixels = 0;

void camera_init(GtkWidget *widget);

/* Create a new backing pixmap of the appropriate size */
gint
configure_event (GtkWidget *widget, GdkEventConfigure *event)
{
  puts("CONFIGURE:EVENT");
  if (backing_pixmap)
    gdk_pixmap_unref(backing_pixmap);

  backing_pixmap = gdk_pixmap_new(widget->window,
                                  widget->allocation.width,
                                  widget->allocation.height,
                                  -1);
  gdk_draw_rectangle (backing_pixmap,
                      widget->style->white_gc,
                      TRUE,
                      0, 0,
                      widget->allocation.width,
                      widget->allocation.height);

  return TRUE;
}

/* Redraw the screen from the backing pixmap */
gint
expose_event (GtkWidget *widget, GdkEventExpose *event)
{
  puts("EXPOSE:EVENT");
  gdk_draw_pixmap(widget->window,
                  widget->style->fg_gc[GTK_WIDGET_STATE (widget)],
                  backing_pixmap,
                  event->area.x, event->area.y,
                  event->area.x, event->area.y,
                  event->area.width, event->area.height);
  
  return FALSE;
}

/* Draw a pixel on the screen */
void
draw_pixel (GtkWidget *widget, gint npix, gint nphe)
{  
  GdkRectangle update_rect;
  gint x, y;

  puts("DRAW:PIXELS");
  x = xc + pixel_coordinates[npix][0]*9-14;                         
  y = yc + pixel_coordinates[npix][1]*9-14;
  printf("(%d,%d)", x,y);
  update_rect.x = x - 14;
  update_rect.y = y - 14;
  update_rect.width = 28;
  update_rect.height = 28;

  gdk_draw_arc(backing_pixmap, widget->style->black_gc,
               TRUE, x, y, 28, 28, 0, 90);
    
  gtk_widget_draw (widget, &update_rect);
}

gint
button_press_event (GtkWidget *widget, GdkEventButton *event)
{
  puts("BUTTON:PRESS:EVENT");
  /*  
      if (event->button == 1 && backing_pixmap != NULL)
      draw_brush (widget, event->x, event->y);
  */
  return TRUE;
}

GtkWidget *
camera_new(gint npixels)
{
  puts("CAMERA:NEW");
  /* Create the drawing area */

  camera = gtk_drawing_area_new ();
  gtk_drawing_area_size (GTK_DRAWING_AREA (camera), 
                         CAMERA_DEFAULT_WIDTH,
                         CAMERA_DEFAULT_HEIGHT);

  NPixels = npixels;

  return ( GTK_WIDGET(camera) );
}

void
camera_init(GtkWidget *widget)
{
  GtkWidget *obj;

  register int i;
  
  puts("CAMERA:INIT");

  nphe       = (gint  *)calloc(npixels, sizeof(gint ));
  trigger    = (guint *)calloc(npixels, sizeof(guint));
  tailcut    = (guint *)calloc(npixels, sizeof(guint));
  mainisland = (guint *)calloc(npixels, sizeof(guint));

  memset( nphe,       0, sizeof(gint )*npixels );
  memset( trigger,    0, sizeof(guint)*npixels );
  memset( tailcut,    0, sizeof(guint)*npixels );
  memset( mainisland, 0, sizeof(guint)*npixels );
 
  for ( i = 0 ; i < NPixels ; ++i ) 
    draw_pixel(widget, i, 0);
}

void 
camera_connections(void)
{
  camera_init( camera );

  /* Signals used to handle backing pixmap */
  gtk_signal_connect (GTK_OBJECT (camera), "expose_event",
                      (GtkSignalFunc) expose_event, NULL);
  gtk_signal_connect (GTK_OBJECT(camera),"configure_event",
                      (GtkSignalFunc) configure_event, NULL);

  /* Event signals */

  gtk_signal_connect (GTK_OBJECT (camera), "button_press_event",
                      (GtkSignalFunc) button_press_event, NULL);

  gtk_widget_set_events (camera, GDK_EXPOSURE_MASK
                         | GDK_LEAVE_NOTIFY_MASK
                         | GDK_BUTTON_PRESS_MASK
                         | GDK_POINTER_MOTION_HINT_MASK);

}
/*
static void 
camera_destroy (GtkObject *object)
{
  Camera *camera;

  g_return_if_fail(object != NULL);
  g_return_if_fail(IS_CAMERA(object));

  camera = CAMERA(object);

  if (GTK_OBJECT_CLASS(parent_class)->destroy)
    (*GTK_OBJECT_CLASS(parent_class)->destroy)(object);
}

gint 
camera_get_nphe(Camera *camera, gint np)
{
  g_return_if_fail(camera != NULL);
  g_return_if_fail(IS_CAMERA(camera));

  return ( camera->nphe[np] );
}

void 
camera_set_nphe(Camera *camera, gint np, gint nphe)
{
  g_return_if_fail(camera != NULL);
  g_return_if_fail(IS_CAMERA(camera));

  camera->nphe[np] = nphe;
}

gfloat 
camera_get_radius(Camera *camera)
{
  g_return_if_fail(camera != NULL);
  g_return_if_fail(IS_CAMERA(camera));

  return ( camera->radius );
}

void 
camera_set_radius(Camera *camera, gfloat radius)
{
  g_return_if_fail(camera != NULL);
  g_return_if_fail(IS_CAMERA(camera));

  camera->radius = radius;
}

gint 
camera_get_trigger(Camera *camera, gint np)
{
  g_return_if_fail(camera != NULL);
  g_return_if_fail(IS_CAMERA(camera));

  return ( camera->trigger[np] );
}

void 
camera_set_trigger(Camera *camera, gint np, gint trigger)
{
  g_return_if_fail(camera != NULL);
  g_return_if_fail(IS_CAMERA(camera));

  camera->trigger[np] = trigger;
}

gint 
camera_get_tailcut(Camera *camera, gint np)
{
  g_return_if_fail(camera != NULL);
  g_return_if_fail(IS_CAMERA(camera));

  return ( camera->tailcut[np] );
}

void 
camera_set_tailcut(Camera *camera, gint np, gint tailcut)
{
  g_return_if_fail(camera != NULL);
  g_return_if_fail(IS_CAMERA(camera));

  camera->tailcut[np] = tailcut;
}

gint 
camera_get_mainisland(Camera *camera, gint np)
{
  g_return_if_fail(camera != NULL);
  g_return_if_fail(IS_CAMERA(camera));

  return ( camera->mainisland[np] );
}

void 
camera_set_mainisland(Camera *camera, gint np, gint mainisland)
{
  g_return_if_fail(camera != NULL);
  g_return_if_fail(IS_CAMERA(camera));
  
  camera->mainisland[np] = mainisland;
}

static void 
camera_realize(GtkWidget *widget)
{
  Camera *camera;
  GdkWindowAttr attributes;
  gint attributes_mask;
  
  g_return_if_fail(widget != NULL);
  g_return_if_fail(IS_CAMERA(widget));
  
  GTK_WIDGET_SET_FLAGS(widget, GTK_REALIZED);
  camera = CAMERA(widget);
  
  attributes.x = widget->allocation.x;
  attributes.y = widget->allocation.y;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.event_mask = gtk_widget_get_events(widget) |
    GDK_EXPOSURE_MASK;

  / * | GDK_BUTTON_PRESS_MASK |
    GDK_BUTTON_RELEASE_MASK | GDK_POINTER_MOTION_MASK |
    GDK_POINTER_MOTION_HINT_MASK;
  * /
  attributes.visual = gtk_widget_get_visual(widget);
  attributes.colormap = gtk_widget_get_colormap(widget);
  
  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
  widget->window = gdk_window_new(widget->parent->window, 
                                  &attributes, attributes_mask);
  
  widget->style = gtk_style_attach(widget->style, widget->window);
  
  gdk_window_set_user_data(widget->window, widget);
  
  gtk_style_set_background(widget->style, widget->window, 
                           GTK_STATE_ACTIVE);
}

static void
camera_size_request(GtkWidget *widget,
                             GtkRequisition *requisition)
{
  requisition->width = CAMERA_DEFAULT_HEIGHT;
  requisition->height = CAMERA_DEFAULT_WIDTH;
}

static void
camera_size_allocate(GtkWidget *widget,
                              GtkAllocation *allocation)
{
  Camera *camera;
  
  g_return_if_fail(widget != NULL);
  g_return_if_fail(IS_CAMERA(widget));
  g_return_if_fail(allocation != NULL);
  
  widget->allocation = *allocation;
  if (GTK_WIDGET_REALIZED(widget)) {
    camera = CAMERA(widget);
    
    gdk_window_move_resize(widget->window,
                           allocation->x, allocation->y,
                           allocation->width, allocation->height);
  }
}

static gint
camera_expose(GtkWidget *widget,
                       GdkEventExpose *event)
{
  Camera *camera;
  GdkPoint points[6];
  GtkStyle *style;
  gdouble s,c;
  gint xc,yc;
  gint i;
  
  g_return_val_if_fail(widget != NULL, FALSE);
  g_return_val_if_fail(IS_CAMERA(widget), FALSE);
  g_return_val_if_fail(event != NULL, FALSE);

  if (event->count > 0)
    return FALSE;

  camera = CAMERA(widget);

  gdk_window_clear_area(widget->window, 0, 0, 
                        widget->allocation.width,
                        widget->allocation.height);

  xc = widget->allocation.width/2;
  yc = widget->allocation.height/2;

  / *
  for (i=0; i<6; ++i) {
    points[i].x = xc + VERTICES_X[i]*camera->radius;
    points[i].y = yc + VERTICES_Y[i]*camera->radius;
  }

  style = widget->style;
  style->fg[0].red = 65000;

  gtk_draw_polygon(style, widget->window,
                   GTK_STATE_NORMAL, GTK_SHADOW_NONE, 
                   points, 6, TRUE);
  * /     
  return (FALSE);
}
*/
 






 





  
