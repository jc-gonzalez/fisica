/*************************************************************************
*                                                                          
* qmagicdisplayview.h  -  description
*
* Copyright (C) 2001  J C Gonzalez
* gonzalez@gae.ucm.es
*
*------------------------------------------------------------------------
*
* Copyright (C) 2001 J C Gonzalez
*  
* This program is free software;  you can redistribute it and/or  modify
* it under the terms  of the GNU General  Public License as published by
* the Free Software Foundation; either version  2 of the License, or (at
* your option) any later version.
* 
* This piece of code is distributed in the hope  that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS
* FOR A PARTICULAR PURPOSE.
* 
* In no  event shall his author  be liable for  any special, incidental,
* indirect  or  consequential  damages  of any  kind,   or  any  damages
* whatsoever resulting from loss of use, data or profits, whether or not
* advised of the possibility of damage, and on  any theory of liability,
* arising out  of or in connection  with the use  or performance of this
* software. You've been warned.
* 
************************************************************************/


#ifndef QMAGICDISPLAYVIEW_H
#define QMAGICDISPLAYVIEW_H

// include files for QT
#include <qwidget.h>

// application specific includes
#include "qmagicdisplaydoc.h"


class CameraPixels;
class QListBox;
class QTextView;
class QStrList;
class Palette;
class wDATGUI;
class wRFLGUI;

/**
* This class provides an incomplete base for your application view. 
*/

class QMAGICDisplayView : public QWidget
{
  Q_OBJECT

public:
  QMAGICDisplayView(QWidget *parent,
                    QMAGICDisplayDoc* doc,
                    EventHeader& eh,
                    EventData& ed,
                    EventPostData& epd);
  ~QMAGICDisplayView();

  typedef union {
    QWidget* widget;
    wDATGUI* datgui;
    wRFLGUI* rflgui; }  GUIPtr;
    
  void initView();

signals:
  void initChildren();

protected slots:
  void slotDocumentChanged();
  void slotEventChanged();
  void slotAddedPalette(const char*);
  void slotSetPalette(int);
  void slotSetPaletteMode(int);
  void slotSetPaletteMin(int);
  void slotSetPaletteMax(int);

private:
  QMAGICDisplayDoc* theDoc;
  CameraPixels*     theCamera;
  QTextView*        theEVTHlog;
  QListBox*         theINFOlist;
  GUIPtr            gui;
  int               doctype;
  QStrList*         paletteList;
  Palette*          palette;

  EventHeader&      eheader;
  EventData&        edata;
  EventPostData&    epdata;
};

#endif

// Local Variables:
// mode: c++
// End:
//EOF
