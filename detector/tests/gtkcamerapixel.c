#include <stdio.h>
#include <math.h>
#include <gtk/gtkmain.h>
#include <gtk/gtksignal.h>

#include <gtkcamerapixel.h>

#include <pixelsinfo.h>

#define CAMERAPIXEL_DEFAULT_WIDTH   17
#define CAMERAPIXEL_DEFAULT_HEIGHT  20

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

#define SQRT3OVER2      0.866025404

static GtkWidgetClass *parent_class = NULL;

static void gtk_camerapixel_class_init (GtkCamerapixelClass *class);
static void gtk_camerapixel_init(GtkCamerapixel *camerapixel);
GtkWidget* gtk_camerapixel_new (gint number,
                                gint nphe,
                                gfloat radius);
static void  gtk_camerapixel_destroy (GtkObject *object);
static void  gtk_camerapixel_realize(GtkWidget *widget);
static void gtk_camerapixel_size_request(GtkWidget *widget,
                                         GtkRequisition *requisition);
static void gtk_camerapixel_size_allocate(GtkWidget *widget,
                                          GtkAllocation *allocation);
static gint gtk_camerapixel_expose(GtkWidget *widget,
                                   GdkEventExpose *event);

guint
gtk_camerapixel_get_type ()
{
  static guint camerapixel_type = 0;

  if (!camerapixel_type) {
    GtkTypeInfo camerapixel_info = 
    {
      "GtkCamerapixel",
      sizeof(GtkCamerapixel),
      sizeof(GtkCamerapixelClass),
      (GtkClassInitFunc) gtk_camerapixel_class_init,
      (GtkObjectInitFunc) gtk_camerapixel_init,
      (GtkArgSetFunc) NULL,
      (GtkArgGetFunc) NULL,
    };

    camerapixel_type = gtk_type_unique (gtk_widget_get_type(), 
                                        &camerapixel_info);
  }

  return(camerapixel_type);
}

static void
gtk_camerapixel_class_init (GtkCamerapixelClass *class)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;

  object_class = (GtkObjectClass*) class;
  widget_class = (GtkWidgetClass*) class;
  
  parent_class = gtk_type_class (gtk_widget_get_type());

  object_class->destroy = gtk_camerapixel_destroy;

  widget_class->realize = gtk_camerapixel_realize;
  widget_class->expose_event = gtk_camerapixel_expose;
  widget_class->size_request = gtk_camerapixel_size_request;
  widget_class->size_allocate = gtk_camerapixel_size_allocate;
  /*
  widget_class->button_press_event = gtk_camerapixel_button_press;
  widget_class->button_release_event = gtk_camerapixel_button_release;
  widget_class->motion_notify_event = gtk_camerapixel_motion_notify;
  */
}

static void
gtk_camerapixel_init(GtkCamerapixel *camerapixel)
{
  camerapixel->button = 0;
  camerapixel->trigger = 0;
  camerapixel->tailcut = 0;
  camerapixel->mainisland = 0;
  
  camerapixel->xc = pixel_coordinates[camerapixel->number][0];
  camerapixel->yc = pixel_coordinates[camerapixel->number][1];
}

GtkWidget*
gtk_camerapixel_new (gint number, gint nphe, gfloat radius)
{
  GtkCamerapixel *camerapixel;
  
  camerapixel = gtk_type_new( gtk_camerapixel_get_type() );
  camerapixel->number = number;
  camerapixel->nphe = nphe;
  camerapixel->radius = radius;

  return ( GTK_WIDGET( camerapixel ) );
}

static void 
gtk_camerapixel_destroy (GtkObject *object)
{
  GtkCamerapixel *camerapixel;

  g_return_if_fail(object != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(object));

  camerapixel = GTK_CAMERAPIXEL(object);

  if (GTK_OBJECT_CLASS(parent_class)->destroy)
    (*GTK_OBJECT_CLASS(parent_class)->destroy)(object);
}

gint 
gtk_camerapixel_get_nphe(GtkCamerapixel *camerapixel)
{
  g_return_if_fail(camerapixel != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(camerapixel));

  return ( camerapixel->nphe );
}

void 
gtk_camerapixel_set_nphe(GtkCamerapixel *camerapixel, gint nphe)
{
  g_return_if_fail(camerapixel != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(camerapixel));

  camerapixel->nphe = nphe;
}

gfloat 
gtk_camerapixel_get_radius(GtkCamerapixel *camerapixel)
{
  g_return_if_fail(camerapixel != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(camerapixel));

  return ( camerapixel->radius );
}

void 
gtk_camerapixel_set_radius(GtkCamerapixel *camerapixel, gfloat radius)
{
  g_return_if_fail(camerapixel != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(camerapixel));

  camerapixel->radius = radius;
}

gint 
gtk_camerapixel_get_trigger(GtkCamerapixel *camerapixel)
{
  g_return_if_fail(camerapixel != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(camerapixel));

  return ( camerapixel->trigger );
}

void 
gtk_camerapixel_set_trigger(GtkCamerapixel *camerapixel, gint trigger)
{
  g_return_if_fail(camerapixel != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(camerapixel));

  camerapixel->trigger = trigger;
}

gint 
gtk_camerapixel_get_tailcut(GtkCamerapixel *camerapixel)
{
  g_return_if_fail(camerapixel != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(camerapixel));

  return ( camerapixel->tailcut );
}

void 
gtk_camerapixel_set_tailcut(GtkCamerapixel *camerapixel, gint tailcut)
{
  g_return_if_fail(camerapixel != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(camerapixel));

  camerapixel->tailcut = tailcut;
}

gint 
gtk_camerapixel_get_mainisland(GtkCamerapixel *camerapixel)
{
  g_return_if_fail(camerapixel != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(camerapixel));

  return ( camerapixel->mainisland );
}

void 
gtk_camerapixel_set_mainisland(GtkCamerapixel *camerapixel, gint mainisland)
{
  g_return_if_fail(camerapixel != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(camerapixel));
  
  camerapixel->mainisland = mainisland;
}

static void 
gtk_camerapixel_realize(GtkWidget *widget)
{
  GtkCamerapixel *camerapixel;
  GdkWindowAttr attributes;
  gint attributes_mask;
  
  g_return_if_fail(widget != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(widget));
  
  GTK_WIDGET_SET_FLAGS(widget, GTK_REALIZED);
  camerapixel = GTK_CAMERAPIXEL(widget);
  
  attributes.x = widget->allocation.x;
  attributes.y = widget->allocation.y;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.event_mask = gtk_widget_get_events(widget) |
    GDK_EXPOSURE_MASK;

  /* | GDK_BUTTON_PRESS_MASK |
    GDK_BUTTON_RELEASE_MASK | GDK_POINTER_MOTION_MASK |
    GDK_POINTER_MOTION_HINT_MASK;
  */
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
gtk_camerapixel_size_request(GtkWidget *widget,
                             GtkRequisition *requisition)
{
  static gfloat r;
  static gint w, h;

  r = GTK_CAMERAPIXEL(widget)->radius;
  h = (gint)(2.0 * r);
  w = (gint)(2.0 * r * cos(M_PI/6.0));
  requisition->width = w;
  requisition->height = h;
}

static void
gtk_camerapixel_size_allocate(GtkWidget *widget,
                              GtkAllocation *allocation)
{
  GtkCamerapixel *camerapixel;
  
  g_return_if_fail(widget != NULL);
  g_return_if_fail(GTK_IS_CAMERAPIXEL(widget));
  g_return_if_fail(allocation != NULL);
  
  widget->allocation = *allocation;
  if (GTK_WIDGET_REALIZED(widget)) {
    camerapixel = GTK_CAMERAPIXEL(widget);
    
    gdk_window_move_resize(widget->window,
                           allocation->x, allocation->y,
                           allocation->width, allocation->height);
    /*    camerapixel->radius = MIN(allocation->width, allocation->height);*/
  }
}

static gint
gtk_camerapixel_expose(GtkWidget *widget,
                       GdkEventExpose *event)
{
  GtkCamerapixel *camerapixel;
  GdkPoint points[6];
  GtkStyle *style;
  gdouble s,c;
  gint xc,yc;
  gint i;
  
  g_return_val_if_fail(widget != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_CAMERAPIXEL(widget), FALSE);
  g_return_val_if_fail(event != NULL, FALSE);

  if (event->count > 0)
    return FALSE;

  camerapixel = GTK_CAMERAPIXEL(widget);

  gdk_window_clear_area(widget->window, 0, 0, 
                        widget->allocation.width,
                        widget->allocation.height);

  xc = widget->allocation.width/2;
  yc = widget->allocation.height/2;

  for (i=0; i<6; ++i) {
    points[i].x = xc + VERTICES_X[i]*camerapixel->radius;
    points[i].y = yc + VERTICES_Y[i]*camerapixel->radius;
  }

  style = widget->style;
  style->fg[0].red = 65000;

  gtk_draw_polygon(style, widget->window,
                   GTK_STATE_NORMAL, GTK_SHADOW_NONE, 
                   points, 6, TRUE);
         
  return (FALSE);
}

 






 





  
