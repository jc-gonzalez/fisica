/* 
 * Camera Widget
 */

#ifndef __CAMERA_H__
#define __CAMERA_H__


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define CAMERA_DEFAULT_WIDTH   500
#define CAMERA_DEFAULT_HEIGHT  500
#define SQRT3OVER2      0.866025404

GtkWidget* camera_new(gint npixels);
void camera_connections(void);


/*
gint       camera_get_nppixels(Camera *c);
void       camera_set_npixels(Camera *c, gint npix);

gint       camera_get_nphe(Camera *c, gint np);
void       camera_set_nphe(Camera *c, gint np, gint nphe);

gfloat     camera_get_radius(Camera *c);
void       camera_set_radius(Camera *c, gfloat radius);

gint       camera_get_trigger(Camera *c, gint np);
void       camera_set_trigger(Camera *c, gint np, gint trigger);

gint       camera_get_tailcut(Camera *c, gint np);
void       camera_set_tailcut(Camera *c, gint np, gint tailcut);

gint       camera_get_mainisland(Camera *c, gint np);
void       camera_set_mainisland(Camera *c, gint np, gint mainisland);
*/


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __CAMERA_H__ */
