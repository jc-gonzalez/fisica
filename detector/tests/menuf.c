#include <gtk/gtk.h>
#include <strings.h>

#include "ctv.h"

static void 
print_hello(GtkWidget *w, gpointer data);

static void 
opt_read_all_phe_cmd_cb(GtkWidget *w, gpointer data);

static GtkMenuEntry menu_items[] =
{
  {"<Main>/File/Open", "<control>O", file_open_cmd_cb, NULL},
  {"<Main>/File/Print...", "<control>S", print_hello, NULL},
  {"<Main>/File/<separator>", NULL, NULL, NULL},
  {"<Main>/File/Quit", "<control>Q", file_quit_cmd_cb, "OK, I'll quit"},
  {"<Main>/Options/Read ALL events", NULL, opt_read_all_phe_cmd_cb, NULL},
  {"<Main>/Options/Test", "<control>T", opt_test_cmd_cb, NULL}
};

static void 
print_hello(GtkWidget *w, gpointer data)
{
  printf("hello!\n");
}

static void 
opt_read_all_phe_cmd_cb(GtkWidget *w, gpointer data)
{
  set_read_all_phe(TRUE);
}

void 
get_main_menu(GtkWidget *w, GtkWidget **menubar)
{
  int nmenu_items = sizeof(menu_items) / sizeof(menu_items[0]);
  GtkMenuFactory *factory;
  GtkMenuFactory *subfactory;
  
  factory = gtk_menu_factory_new(GTK_MENU_FACTORY_MENU_BAR);
  subfactory = gtk_menu_factory_new(GTK_MENU_FACTORY_MENU_BAR);
  
  gtk_menu_factory_add_subfactory(factory, subfactory, "<Main>");
  gtk_menu_factory_add_entries(factory, menu_items, nmenu_items);
  /*  gtk_window_add_accelerator_table(GTK_WINDOW(w), subfactory->table);*/
  
  if (menubar)
    *menubar = subfactory->widget;
  
  /*
    int nmenu_items = sizeof(menu_items) / sizeof(menu_items[0]);
    GtkItemFactory *factory;
    GtkItemFactory *subfactory;
    
    factory = gtk_menu_factory_new(GTK_MENU_FACTORY_MENU_BAR);
    subfactory = gtk_menu_factory_new(GTK_MENU_FACTORY_MENU_BAR);
    
    gtk_menu_factory_add_subfactory(factory, subfactory, "<Main>");
    gtk_menu_factory_add_entries(factory, menu_items, nmenu_items);
    gtk_window_add_accelerator_table(GTK_WINDOW(w), subfactory->table);
    
    if (menubar)
    *menubar = subfactory->widget;
    */
}



