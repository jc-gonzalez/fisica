#include <stdio.h>
#include <string.h>
#include <gtk/gtk.h>

#include "ctv.h"
#include "menuf.h"
#include "evt.h"

#include "pixelsinfo.h"

#include "palette.xpm"
#include "arrow_up.xpm"
#include "arrow_down.xpm"

#define CAMERA_DEFAULT_WIDTH   500
#define CAMERA_DEFAULT_HEIGHT  500
#define SQRT3OVER2      0.866025404

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

gfloat default_pal[32][3] =
{{0.000,0.000,0.000},
 {0.000,0.000,0.504},
 {0.000,0.059,0.525},
 {0.000,0.117,0.545},
 {0.000,0.176,0.566},
 {0.000,0.234,0.587},
 {0.000,0.293,0.608},
 {0.000,0.351,0.629},
 {0.000,0.410,0.649},
 {0.000,0.469,0.670},
 {0.000,0.527,0.691},
 {0.000,0.586,0.712},
 {0.000,0.644,0.733},
 {0.000,0.703,0.753},
 {0.000,0.762,0.774},
 {0.000,0.820,0.795},
 {0.000,0.879,0.816},
 {0.067,0.887,0.761},
 {0.133,0.895,0.707},
 {0.200,0.903,0.653},
 {0.267,0.911,0.598},
 {0.333,0.919,0.544},
 {0.400,0.927,0.490},
 {0.467,0.935,0.435},
 {0.533,0.943,0.381},
 {0.600,0.952,0.326},
 {0.667,0.960,0.272},
 {0.733,0.968,0.218},
 {0.800,0.976,0.163},
 {0.867,0.984,0.109},
 {0.933,0.992,0.054},
 {1.000,1.000,0.000}};  

/* Backing pixmap for drawing area */
static GdkPixmap *backing_pixmap = NULL;

/* Palette */
static GdkColormap *palette = NULL;
static GdkColor colors[32];
static gfloat ** palettecol;
static gchar ** palettexpm=NULL;

/* Camera drawing area */
static GtkWidget *camera;
static gint NPixels;
static gfloat *nphe;
static guint *trigger;
static guint *tailcut;
static guint *mainisland;

static gint radPix = 13;
static gint radPix_2 = 6;
static gfloat scalePix = 4;

static gint highC = 10;
static gint lowC = 1;
static gint increment = 1;

static gint offset=0;
static gint Pixels_were_drawn = FALSE;

/* Center of the camera */
static gint xc = CAMERA_DEFAULT_WIDTH / 2;
static gint yc = CAMERA_DEFAULT_WIDTH / 2;

static gchar sentence_about[]="
Cherenkov Telescope Event Viewer\n
Copyright (c) 1998, J C Gonzalez\n
All rights reserved\n
\n
\n
\"Every program has at least one bug and can be shortened
by at least one instruction - from which, by induction,
one can deduce that every program can be reduced to one
instruction which doesn't work.\"
                                      (XX Century, Unknown)\n\n";

static gchar *info_names[] =
{
  " Event ",
  " Image ",
  " Analysis ",
  " Filters ",
  " File ",
  " About "
};

static int ninfo_names = sizeof(info_names) / sizeof(info_names[0]);

static gchar *file_rows[5][2] =
{
  {"File", "<none>"},
  {"Directory", "<none>"},
  {"Space", "<none>"},
  {"User", "<none>"},
  {"Host", "<none>"}
};

static int nfile_rows = sizeof(file_rows) / sizeof(file_rows[0]);

static gchar *ana_rows[20][4] =
{
  {"Charge(core)",  "  -  ",   "Length",    "  -  "}, 
  {"Max. 1",        "  -  ",   "Width",     "  -  "},  
  {"Max. 2",        "  -  ",   "Dist",      "  -  "},   
  {"Max. 3",        "  -  ",   "MDist",     "  -  "},  
  {"Max. 4",        "  -  ",   "AzWidth",   "  -  "},
  {"Max. 5",        "  -  ",   "Miss",      "  -  "},   
  {"Max. 6",        "  -  ",   "Alpha",     "  -  "},  
  {"Max. 7",        "  -  ",   "Phi",       "  -  "},    
  {"Max. 8",        "  -  ",   "AsimX",     "  -  "},  
  {"Max. 9",        "  -  ",   "AsimY",     "  -  "},  
  {"Max.10",        "  -  ",   "Conc. 1",   "  -  "},       
  {"Center",        "  -  ",   "Conc. 2",   "  -  "},       
  {"Center[core]",  "  -  ",   "Conc. 3",   "  -  "},       
  {"Raw Evt. #",    "  -  ",   "Conc. 4",   "  -  "},       
  {"X Shower",      "  -  ",   "Conc. 5",   "  -  "},       
  {"Y Shower",      "  -  ",   "Conc. 6",   "  -  "},       
  {"d",             "  -  ",   "Conc. 7",   "  -  "},       
  {"e",             "  -  ",   "Conc. 8",   "  -  "},       
  {"f",             "  -  ",   "Conc. 9",   "  -  "},       
  {"g",             "  -  ",   "Conc.10",   "  -  "}        
};                     

static int nana_rows = sizeof(ana_rows) / sizeof(ana_rows[0]);

gint configure_event (GtkWidget *widget, GdkEventConfigure *event);
gint expose_event (GtkWidget *widget, GdkEventExpose *event);
void draw_pixel (GtkWidget *widget, gint npix, gfloat nphe);
void draw_pixels (GtkWidget *widget);
gint button_press_event (GtkWidget *widget, GdkEventButton *event);
void camera_init(GtkWidget *widget);
void change_palette();
void entry_enter_cb(GtkWidget *widget, GtkWidget *entry);
void entry_chgval_cb(GtkWidget *widget, GtkWidget *entry);
gint draw_image (GtkWidget *widget, gpointer data);
gint replot (GtkWidget *widget, gpointer data);
void chgpalpixmap( gfloat ** g );
gint setlowC (GtkWidget *widget, gpointer data);
gint sethighC (GtkWidget *widget, gpointer data);
void put_analysis (void);
gint do_analysis (GtkWidget *widget, gpointer data);

gint DoAnalysis = TRUE;

static GtkWidget *window;
static GtkWidget *main_vbox, *hbox1, *hbox2, *hbox3, *vbox1, *vbox2;
static GtkWidget *menubar;
static GtkWidget *toolbar;
static GtkWidget *camera;
static GtkWidget *cameraview;
static GtkWidget *info;
static GtkWidget *statusbar;
static GtkWidget *scrl_numshow;
static GtkObject *adj_numshow;
static GtkWidget *disp_numshow;
static GtkWidget *entry_numshow;
static GtkWidget *palette_pixmap;
static GtkWidget *palette_pixmap_window;
static GtkObject *adjlow, *adjhigh;
static GtkWidget *lowCspin, *highCspin;

static GtkStyle  *style;
static GdkPixmap *pixmap;
static GdkBitmap *mask;
static GtkWidget *lbl_about;

static GtkWidget *tbl_file_info;
static GtkWidget *tbl_ana;
static GtkWidget *tbl_update, *tbl_autoupdate;

static guint lowCbtnUP_handler = 0;
static guint lowCbtnDOWN_handler = 0;
static guint highCbtnUP_handler = 0;
static guint highCbtnDOWN_handler = 0;

int main(int argc, char **argv)
{
  GtkWidget *frame, *label;
  GtkWidget *obj, *obj1, *obj2;
  GtkWidget *pxmpw1, *pxmpw2;
  GdkPixmap *pxmp1, *pxmp2;
  GdkBitmap *mask1, *mask2;
  GtkStyle  *stl;

  gint i, j;

  gtk_init(&argc, &argv);

  gtk_rc_parse(".ctv-gtkrc");

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_signal_connect(GTK_OBJECT(window), "destroy",
                     GTK_SIGNAL_FUNC(file_quit_cmd_cb), 
                     "WM_destroy");
  gtk_window_set_title(GTK_WINDOW(window), 
                       "CTV :: Cherenkov Telescopes Event Viewer");

  main_vbox = gtk_vbox_new(FALSE, 1);
  gtk_container_border_width(GTK_CONTAINER(main_vbox), 1);
  gtk_container_add(GTK_CONTAINER(window), main_vbox);

  {

    get_main_menu(window, &menubar);
    gtk_box_pack_start(GTK_BOX(main_vbox), menubar, FALSE, TRUE, 0);
    gtk_widget_show(menubar);
    
    /*
      toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);
      gtk_toolbar_append_space (GTK_TOOLBAR(toolbar));
      gtk_box_pack_end_defaults(GTK_BOX(main_vbox), toolbar);
      gtk_widget_show(toolbar);
    */
    
    hbox1 = gtk_hbox_new(FALSE, 1);
    gtk_container_add(GTK_CONTAINER(main_vbox), hbox1);
    
    {

      cameraview = gtk_frame_new((char*)NULL);
      gtk_widget_set_usize(GTK_WIDGET(cameraview), 500, 500);
      gtk_box_pack_start_defaults(GTK_BOX(hbox1), cameraview);
      {
        camera = gtk_drawing_area_new ();
        gtk_drawing_area_size (GTK_DRAWING_AREA (camera), 
                               CAMERA_DEFAULT_WIDTH,
                               CAMERA_DEFAULT_HEIGHT);
        gtk_container_add(GTK_CONTAINER(cameraview), camera);
        gtk_widget_show(camera);

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
        
        NPixels = 919;
        camera_init(camera);
        puts("Hi!");
      }
      gtk_widget_show(cameraview);
      
      info = gtk_notebook_new();
      gtk_widget_set_usize(GTK_WIDGET(info), 450, 500);
      gtk_box_pack_start_defaults(GTK_BOX(hbox1), info);

      for (i=0; i<ninfo_names; ++i) {
        frame = gtk_frame_new(info_names[i]);
        gtk_container_border_width(GTK_CONTAINER(frame), 4);
        label = gtk_label_new(info_names[i]);
        gtk_notebook_append_page(GTK_NOTEBOOK(info), 
                                 frame, label);
        
        switch (i) {
        case 0:
          break;
        case 1:
          obj1 = gtk_table_new(4,6,FALSE);
          gtk_container_add(GTK_CONTAINER(frame), obj1);
          
          obj = gtk_label_new("    Lower ph.e.: ");
          gtk_table_attach(GTK_TABLE(obj1), obj, 0, 1, 0, 1,
                           GTK_SHRINK, GTK_SHRINK, 0, 0);
          gtk_widget_show(obj);

          adjlow = gtk_adjustment_new (1.0, 1.0, 1001.0, 1.0, 10.0, 1.0);
          lowCspin = obj = gtk_spin_button_new (GTK_ADJUSTMENT (adjlow),
                                                 1.0, 0);
          gtk_widget_set_usize(GTK_WIDGET(obj), 60, 22);
          gtk_table_attach(GTK_TABLE(obj1), obj, 1, 2, 0, 1,
                           GTK_SHRINK, GTK_SHRINK, 0, 0);
          gtk_widget_show(obj);

          obj = gtk_label_new("    Upper ph.e.: ");
          gtk_table_attach(GTK_TABLE(obj1), obj, 2, 3, 0, 1,
                           GTK_SHRINK, GTK_SHRINK, 0, 0);
          gtk_widget_show(obj);

          adjhigh = gtk_adjustment_new (1.0, 1.0, 1001.0, 1.0, 10.0, 1.0);
          highCspin = obj = gtk_spin_button_new (GTK_ADJUSTMENT (adjhigh),
                                                 1.0, 0);
          gtk_widget_set_usize(GTK_WIDGET(obj), 60, 22);
          gtk_table_attach(GTK_TABLE(obj1), obj, 3, 4, 0, 1,
                           GTK_SHRINK, GTK_SHRINK, 0, 0);
          gtk_widget_show(obj);

          gtk_signal_connect(GTK_OBJECT(adjlow), "value_changed",
                             GTK_SIGNAL_FUNC(setlowC), NULL);
          gtk_signal_connect(GTK_OBJECT(adjhigh), "value_changed",
                             GTK_SIGNAL_FUNC(sethighC), NULL);

          obj = gtk_fixed_new();
          gtk_table_attach(GTK_TABLE(obj1), obj, 4, 5, 0, 1,
                           GTK_EXPAND, GTK_SHRINK, 0, 0);
          gtk_widget_show(obj);
          
          obj = gtk_button_new_with_label("  Replot  ");
          gtk_widget_set_usize(GTK_WIDGET(obj), 50, 22);
          gtk_table_attach(GTK_TABLE(obj1), obj, 5, 6, 0, 1,
                           0, GTK_SHRINK, 0, 0);
          gtk_signal_connect(GTK_OBJECT(obj), "clicked",
                             GTK_SIGNAL_FUNC(replot), NULL);
          gtk_widget_show(obj);
          
          obj = gtk_fixed_new();
          gtk_table_attach(GTK_TABLE(obj1), obj, 6, 7, 0, 1,
                           GTK_EXPAND, GTK_SHRINK, 0, 0);
          gtk_widget_show(obj);
          
          /*--------------------*/

          obj = gtk_hseparator_new();
          gtk_table_attach_defaults(GTK_TABLE(obj1), obj, 0, 6, 1, 2);
          gtk_widget_show(obj);

          /*--------------------*/
          
          gtk_widget_show(obj1);
          
          break;
        case 2: /* analysis */
          obj1 = gtk_table_new(2,2,FALSE);
          gtk_container_add(GTK_CONTAINER(frame), obj1);

          tbl_ana = obj = gtk_clist_new(4);
          gtk_clist_set_border(GTK_CLIST(obj), 
                               GTK_SHADOW_ETCHED_IN);
          gtk_clist_set_column_width(GTK_CLIST(obj), 0, 85);
          gtk_clist_set_column_width(GTK_CLIST(obj), 1, 100);
          gtk_clist_set_column_width(GTK_CLIST(obj), 2, 85);
          gtk_clist_set_column_width(GTK_CLIST(obj), 3, 100);
          for (j=0; j<nana_rows; ++j) 
            gtk_clist_append(GTK_CLIST(obj), ana_rows[j]);
          gtk_table_attach_defaults(GTK_TABLE(obj1), obj, 0, 2, 0, 1);

          tbl_autoupdate = obj=gtk_check_button_new_with_label("Auto update");
          gtk_signal_connect(GTK_OBJECT(tbl_autoupdate), "toggled",
                             GTK_SIGNAL_FUNC(do_analysis), NULL);
          gtk_table_attach(GTK_TABLE(obj1), obj, 0, 1, 1, 2,
                           GTK_EXPAND, GTK_SHRINK, 0, 0);

          tbl_update = obj=gtk_button_new_with_label("Update!");
          gtk_table_attach(GTK_TABLE(obj1), obj, 1, 2, 1, 2,
                           GTK_EXPAND, GTK_SHRINK, 0, 0);

          gtk_widget_show(tbl_ana);      
          gtk_widget_show(tbl_autoupdate);      
          gtk_widget_show(tbl_update);      
          gtk_widget_show(obj1);      

          break;
        case 3:
          break;
        case 4:
          tbl_file_info = gtk_clist_new(2);
          gtk_clist_set_border(GTK_CLIST(tbl_file_info), 
                               GTK_SHADOW_ETCHED_IN);
          gtk_clist_set_column_width(GTK_CLIST(tbl_file_info), 0, 80);
          for (j=0; j<nfile_rows; ++j) 
            gtk_clist_append(GTK_CLIST(tbl_file_info), file_rows[j]);
          gtk_container_add(GTK_CONTAINER(frame), tbl_file_info);
          gtk_widget_show(tbl_file_info);      
          break;
        case 5:
          lbl_about = gtk_label_new(sentence_about);
          gtk_container_add(GTK_CONTAINER(frame), lbl_about);
          gtk_widget_show(lbl_about);      
          break;
        }
        
        gtk_widget_show(frame);      
      }
      
      gtk_widget_show(info);
      
    } /* hbox1 */
    
    gtk_widget_show(hbox1);
    
    hbox2 = gtk_hbox_new(FALSE, 1);
    gtk_container_add(GTK_CONTAINER(main_vbox), hbox2);
    {  
      vbox1 = gtk_vbox_new(FALSE, 1);
      gtk_box_pack_start_defaults(GTK_BOX(hbox2), vbox1);
      {
        /* value, lower, upper, step_increment, page_increment, page_size */
        /* note that the page_size value only makes a difference for
           scrollbar widgets, and the highest value you'll get is actually
           (upper - page_size). */
        adj_numshow = gtk_adjustment_new (1.0, 1.0, 110.0, 1.0, 10.0, 10.0);
        scrl_numshow = gtk_hscrollbar_new (GTK_ADJUSTMENT (adj_numshow));
        gtk_range_set_update_policy (GTK_RANGE (scrl_numshow),
                                     GTK_UPDATE_CONTINUOUS);
        gtk_box_pack_start(GTK_BOX(vbox1), scrl_numshow, FALSE, FALSE, FALSE);
        gtk_widget_set_usize(GTK_WIDGET(scrl_numshow), 450, 16);
        gtk_widget_show(scrl_numshow);

        hbox3 = gtk_hbox_new(FALSE, 0);
        gtk_box_pack_start_defaults(GTK_BOX(vbox1), hbox3);    
        {  
          entry_numshow = gtk_entry_new_with_max_length(10);
          gtk_widget_set_usize(GTK_WIDGET(entry_numshow), 60, 20);
          gtk_signal_connect(GTK_OBJECT(entry_numshow), "activate",
                             GTK_SIGNAL_FUNC(entry_enter_cb), entry_numshow);
          gtk_signal_connect(GTK_OBJECT(adj_numshow), "value_changed",
                             GTK_SIGNAL_FUNC(entry_chgval_cb), entry_numshow);
          gtk_signal_connect(GTK_OBJECT(adj_numshow), "value_changed",
                             GTK_SIGNAL_FUNC(draw_image), NULL);
          gtk_entry_set_text (GTK_ENTRY(entry_numshow), "1");
          gtk_box_pack_start(GTK_BOX(hbox3), entry_numshow, 
                             FALSE, FALSE, TRUE);
          gtk_widget_show(entry_numshow);

          obj = gtk_vseparator_new();
          gtk_box_pack_start_defaults(GTK_BOX(hbox3), obj);
          gtk_widget_show(obj);
          
        }
        gtk_widget_show(hbox3);
      }
      gtk_widget_show(vbox1);
      
      vbox2 = gtk_vbox_new(FALSE, 1);
      gtk_box_pack_start_defaults(GTK_BOX(hbox2), vbox2);
      {
        obj = gtk_fixed_new();
        gtk_widget_set_usize(GTK_WIDGET(obj), 400, 20);
        gtk_box_pack_start_defaults(GTK_BOX(vbox2), obj);
        gtk_widget_show(obj);

      }
      gtk_widget_show(vbox2);

    } /* hbox2 */
    
    gtk_widget_show(hbox2);
    
    statusbar = gtk_statusbar_new();
    gtk_box_pack_start_defaults(GTK_BOX(main_vbox), statusbar);
    gtk_widget_show(statusbar);

  } /* main_vbox */

  gtk_widget_show(main_vbox);
  
  gtk_widget_show(window);
  style = gtk_widget_get_style( window );
  pixmap = gdk_pixmap_create_from_xpm_d( GTK_WIDGET(hbox3)->window,  
                                         &mask,
                                         &style->bg[GTK_STATE_NORMAL],
                                         (gchar **)palette_xpm );
  
  palette_pixmap = gtk_pixmap_new( pixmap, mask );
  palette_pixmap_window = GTK_WIDGET(hbox3);
  gtk_box_pack_start_defaults(GTK_BOX(hbox3), palette_pixmap);
  gtk_widget_show( palette_pixmap );

  gtk_window_set_policy(GTK_WINDOW(window), FALSE, FALSE, FALSE);
 
  analysis( NPixels, NULL, 1 );

  gtk_main();

  return(0);
}

void file_quit_cmd_cb(GtkWidget *widget, gpointer data)
{
  g_print("%s\n", (char*)data);

  gtk_exit(0);

}

void opt_test_cmd_cb(GtkWidget *widget, gpointer data)
{
  register int i;
  change_palette();
  if (backing_pixmap != NULL)
    draw_pixels(camera);
}

/* Create a new backing pixmap of the appropriate size */
gint
configure_event (GtkWidget *widget, GdkEventConfigure *event)
{
  if (backing_pixmap)
    gdk_pixmap_unref(backing_pixmap);

  backing_pixmap = gdk_pixmap_new(widget->window,
                                  widget->allocation.width,
                                  widget->allocation.height,
                                  -1);
  gdk_draw_rectangle (backing_pixmap,
                      widget->style->dark_gc[GTK_STATE_NORMAL],
                      TRUE,
                      0, 0,
                      widget->allocation.width,
                      widget->allocation.height);

  if (Pixels_were_drawn)
    draw_pixels(widget);

  return TRUE;
}

/* Redraw the screen from the backing pixmap */
gint
expose_event (GtkWidget *widget, GdkEventExpose *event)
{
  gdk_draw_pixmap(widget->window,
                  widget->style->dark_gc[GTK_STATE_NORMAL],
                  backing_pixmap,
                  event->area.x, event->area.y,
                  event->area.x, event->area.y,
                  event->area.width, event->area.height);
  
  return FALSE;
}

gint
draw_image (GtkWidget *widget, gpointer data)
{
  if ( !Pixels_were_drawn )
    return FALSE;
  file_locate( (GTK_ADJUSTMENT(widget))->value );
  file_get_data( (gfloat*)nphe );
  if (!palette)
    change_palette();
  draw_pixels(camera);

  if (DoAnalysis)
    put_analysis();

  return TRUE;
}

gint
replot (GtkWidget *widget, gpointer data)
{
  return draw_image( GTK_OBJECT(adj_numshow), NULL);
}

/* Draw a pixel on the screen */
void
draw_pixel (GtkWidget *widget, gint npix, gfloat nphe)
{  
  static GdkGC *gc = NULL;
  GdkRectangle update_rect;
  gint x, y;

  x = xc + pixel_coordinates[npix][0]*scalePix-radPix_2;
  y = yc + pixel_coordinates[npix][1]*scalePix-radPix_2;
  update_rect.x = x;
  update_rect.y = y;
  update_rect.width = radPix;
  update_rect.height = radPix;
    
  if (gc)
    gdk_gc_unref(gc);

  gc = gdk_gc_new(widget->window);
  gdk_gc_set_foreground(gc, &colors[npix % 32]);
  gdk_draw_arc(backing_pixmap, gc,
               TRUE, 
               update_rect.x, update_rect.y,
               update_rect.width, update_rect.height, 
               0, 23040);
  gtk_widget_draw (widget, &update_rect);
}

/* Draw a pixel on the screen */
void
draw_pixels (GtkWidget *widget)
{  
  static GdkGC *gc = NULL;
  GdkRectangle update_rect;
  register int i;
  gint x, y, color_index;

  if (gc)
    gdk_gc_unref(gc);

  gc = gdk_gc_new(widget->window);
  
  update_rect.x = 0;
  update_rect.y = 0;
  update_rect.width  = widget->allocation.width;
  update_rect.height = widget->allocation.height;

  for (i=0;i<NPixels;++i) {

    x = xc + pixel_coordinates[i][0]*scalePix-radPix_2;
    y = yc + pixel_coordinates[i][1]*scalePix-radPix_2;

    color_index = (gint)nphe[i];
    color_index = (31*(color_index-lowC)) / (highC-lowC);
    color_index = (color_index <  0) ?  0 : color_index;
    color_index = (color_index > 31) ? 31 : color_index;

    gdk_gc_set_foreground(gc, &colors[color_index]);
    gdk_draw_arc(backing_pixmap, gc,
                 TRUE, 
                 x, y, radPix, radPix,
                 0, 23040);
  }
  
  gtk_widget_draw (widget, &update_rect);
  ++offset;
  Pixels_were_drawn = TRUE;
}

gint
button_press_event (GtkWidget *widget, GdkEventButton *event)
{
  /*  
      if (event->button == 1 && backing_pixmap != NULL)
      draw_brush (widget, event->x, event->y);
  */
  return TRUE;
}

void
camera_init(GtkWidget *widget)
{
  GtkWidget *obj;

  nphe       = (gfloat*)calloc(NPixels, sizeof(gfloat));
  trigger    = (guint *)calloc(NPixels, sizeof(guint));
  tailcut    = (guint *)calloc(NPixels, sizeof(guint));
  mainisland = (guint *)calloc(NPixels, sizeof(guint));

  memset( nphe,       0, sizeof(gfloat)*NPixels );
  memset( trigger,    0, sizeof(guint)*NPixels );
  memset( tailcut,    0, sizeof(guint)*NPixels );
  memset( mainisland, 0, sizeof(guint)*NPixels );
}

void
change_palette()
{
  register int i;

  palette = gdk_window_get_colormap(camera->window);

  for (i=0;i<32;++i) {
    colors[i].red   = default_pal[i][0] * 65535;
    colors[i].green = default_pal[i][1] * 65535;
    colors[i].blue  = default_pal[i][2] * 65535;
    gdk_color_alloc( palette, &colors[i] );
  }
  
  palettecol=(gfloat**)g_malloc(32); 
  for (i=0;i<32;++i) {
    palettecol[i]=(gfloat*)g_malloc(3); 
    palettecol[i][0] = default_pal[i][0];
    palettecol[i][1] = default_pal[i][1];
    palettecol[i][2] = default_pal[i][2];
  }
  /*  chgpalpixmap( palettecol );  */
}

void chgpalpixmap( gfloat ** g )
{              
  register gint i;
  gint j,k,c;
  gchar chars[]=" .+$#@abcdefghijklmnopqrstuvwxyz"; /* 32 chars */

  if (palettexpm)    
    free(palettexpm); 

  palettexpm = (gchar**)g_malloc(1+32+20);
  palettexpm[0]=(gchar*)g_malloc(20);
  sprintf(palettexpm[0]," %3d %3d %3d %3d ", 320, 20, 32, 1);
  printf("\"%s\",", palettexpm[0]);
  for (i=0;i<32;++i) {
    palettexpm[i+1]=(gchar*)g_malloc(20);
    sprintf(palettexpm[i+1], "%c   c #%02X%02X%02X", 
            chars[i], 
            (gint)((gfloat)g[i][0]*255), 
            (gint)((gfloat)g[i][1]*255), 
            (gint)((gfloat)g[i][2]*255));
    printf("\"%s\",", palettexpm[i+1]);
  }
  for (i=0;i<20;++i) 
    palettexpm[i+33]=(gchar*)g_malloc(321);  
  for (j=0;j<32;++j)
    for (k=0;k<10;++k) {
      c = j*10+k;
      for (i=33;i<53;++i)
        palettexpm[i][c] = chars[j];
    }
  for (i=0;i<20;++i) 
    printf("\"%s\",", palettexpm[i+33]);
  style = gtk_widget_get_style( window );
  gdk_pixmap_unref(pixmap);
  pixmap = gdk_pixmap_create_from_xpm_d(
            GTK_WIDGET(palette_pixmap_window)->window,  
            &mask,
            &style->bg[GTK_STATE_NORMAL],
            (gchar **)palettexpm );
  
  gtk_pixmap_set( GTK_PIXMAP(palette_pixmap), pixmap, mask );
}

void entry_enter_cb(GtkWidget *widget, GtkWidget *entry)
{
  gchar *entry_text;
  entry_text = gtk_entry_get_text(GTK_ENTRY(entry));
  if (atoi(entry_text) < 1)
    gtk_entry_set_text(GTK_ENTRY(entry), "1");
  gtk_adjustment_set_value(GTK_ADJUSTMENT(adj_numshow), atof(entry_text));
}

void entry_chgval_cb(GtkWidget *widget, GtkWidget *entry)
{
  static gchar entry_text[10];
  sprintf(entry_text, "%d", (int)(GTK_ADJUSTMENT(widget))->value);
  gtk_entry_set_text(GTK_ENTRY(entry), entry_text);
}

void file_ok_sel (GtkWidget *w, GtkFileSelection *fs)
{
  gint nshw;
  gchar fname[100];
  GtkAdjustment *adj;

  strcpy ( fname,
           gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs)));
  nshw = pre_read_file( fname );
  if (nshw > 0) {
    adj = GTK_ADJUSTMENT(adj_numshow);
    adj->value = 1.0;
    adj->lower = 1.0;
    adj->upper = nshw + adj->page_size;
    Pixels_were_drawn = TRUE;
  }
}

void file_open_cmd_cb(GtkWidget *widget, gpointer data)
{
  GtkWidget *filew;
    
  filew = gtk_file_selection_new ("File selection");
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filew)->ok_button),
                      "clicked", (GtkSignalFunc) file_ok_sel, filew );
  gtk_signal_connect_object(GTK_OBJECT(GTK_FILE_SELECTION(filew)->ok_button),
                            "clicked", (GtkSignalFunc)gtk_widget_destroy,
                            GTK_OBJECT (filew));
  gtk_signal_connect_object(
    GTK_OBJECT(GTK_FILE_SELECTION(filew)->cancel_button),
                            "clicked", (GtkSignalFunc)gtk_widget_destroy,
                            GTK_OBJECT (filew));
  
  gtk_widget_show(filew);
}

gint
setlowC (GtkWidget *widget, gpointer data)
{
  GtkAdjustment *adj;

  adj = GTK_ADJUSTMENT(widget);
  lowC = (gint)adj->value;
  if (lowC>=highC) {
    adj->value = adj->value - adj->step_increment;
    --lowC;
  }
  set_threshold( (float)lowC );
  return TRUE;
}

gint
sethighC (GtkWidget *widget, gpointer data)
{
  GtkAdjustment *adj;

  adj = GTK_ADJUSTMENT(widget);
  highC = (gint)adj->value;
  if (lowC>=highC) {
    adj->value = adj->value + adj->step_increment;
    ++highC;
  }
  return TRUE;
}

gint
do_analysis(GtkWidget *widget, gpointer data)
{
  DoAnalysis = !DoAnalysis;
  return(TRUE);
}

void
put_analysis(void)
{
  GtkWidget *obj;
  float length, width, dist, xdist, azw, miss, alpha, conc[9]; 
  float phi, asymx, asymy;
  float charge, smax;
  float m[10];
  int nm[10], i;
  char values[40][12];
  static MCEventHeader *mcevth;

  obj = tbl_ana;
  analysis( NPixels, nphe, 0 );

  get_charge(&charge, &smax);
  get_maxs(m, nm);
  get_hillas(&length, &width, &dist, &xdist, &azw, &miss, &alpha, conc);
  get_asym(&asymx, &asymy, &phi);

  mcevth = get_mcevth();

  /*
   * be aware: here we use the side effect of ++
   * there are two possibilities of using the operator ++:
   * 1) a = ++i  => first increments i, then evaluates expresion
   * 2) a = i++  => first evaluates expresion, then increments i
   * we INTENTIONALLY use the second form
   */

  i=0;
  sprintf(values[i++], "%5d(%4d)", (int)charge, (int)smax);
  sprintf(values[i++], "%6d", (int)m[0]);
  sprintf(values[i++], "%6d", (int)m[1]);
  sprintf(values[i++], "%6d", (int)m[2]);
  sprintf(values[i++], "%6d", (int)m[3]);
  sprintf(values[i++], "%6d", (int)m[4]);
  sprintf(values[i++], "%6d", (int)m[5]);
  sprintf(values[i++], "%6d", (int)m[6]);
  sprintf(values[i++], "%6d", (int)m[7]);
  sprintf(values[i++], "%6d", (int)m[8]);
  sprintf(values[i++], "%6d", (int)m[9]);
  sprintf(values[i++], "(%4.2f,%4.2f)", 0.,0.);
  sprintf(values[i++], "(%4.2f,%4.2f)", 0.,0.);
  sprintf(values[i++], "#%d", (int)(mcevth->EvtNumber));
  sprintf(values[i++], "%6.2f", mcevth->CorePos[0][0]/100.);
  sprintf(values[i++], "%6.2f", mcevth->CorePos[1][0]/100.);
  sprintf(values[i++], "%6.0f", 0.);
  sprintf(values[i++], "%6.0f", 0.);
  sprintf(values[i++], "%6.0f", 0.);
  sprintf(values[i++], "%6.0f", 0.);
  sprintf(values[i++], "%6.2f", length);
  sprintf(values[i++], "%6.2f", width);
  sprintf(values[i++], "%6.2f", dist);
  sprintf(values[i++], "%6.2f", xdist);
  sprintf(values[i++], "%6.2f", azw);
  sprintf(values[i++], "%6.2f", miss);
  sprintf(values[i++], "%6.2f", alpha);
  sprintf(values[i++], "%6.2f", phi);
  sprintf(values[i++], "%6.2f", asymx);
  sprintf(values[i++], "%6.2f", asymy);
  sprintf(values[i++], "%6.2f", conc[0]);
  sprintf(values[i++], "%6.2f", conc[1]);
  sprintf(values[i++], "%6.2f", conc[2]);
  sprintf(values[i++], "%6.2f", conc[3]);
  sprintf(values[i++], "%6.2f", conc[4]);
  sprintf(values[i++], "%6.2f", conc[5]);
  sprintf(values[i++], "%6.2f", conc[6]);
  sprintf(values[i++], "%6.2f", conc[7]);
  sprintf(values[i++], "%6.2f", conc[8]);
  sprintf(values[i++], "%6.2f", conc[9]);
  for (i=0;i<nana_rows;i++) {
    gtk_clist_set_text(GTK_CLIST(obj), i, 1, (gchar*)values[i]);
    gtk_clist_set_text(GTK_CLIST(obj), i, 3, (gchar*)values[i+nana_rows]);
  }
   
  return;
} 
