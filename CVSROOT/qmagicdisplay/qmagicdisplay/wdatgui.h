/*************************************************************************
*                                                                          
* wdatgui.h  -  description
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

#ifndef WDATGUI_H
#define WDATGUI_H

#include "wdatgui.design.h"

#include <qcombobox.h>

class QMAGICDisplayDoc;

class wDATGUI : public wDATGUIDesign
{ 
  Q_OBJECT

public:
  wDATGUI( QMAGICDisplayDoc* d = 0,
           QWidget* parent = 0,
           const char* name = 0, WFlags fl = 0 );
  ~wDATGUI();

  void setTotalEvents(int);
  void setTrigger(bool t);
  void addPalette(const char*, bool redisplay=false);
  void setFilename(const char* s);

signals:
  void eventChanged(int);
  void addingPalette(const char*);
  void setPalette(int);
  void setPaletteMode(int);
  void setPaletteMin(int);
  void setPaletteMax(int);
  void reDisplayEvent();
  void toggleShowItem(int);

public slots:
  void slotBrowsePalettes();
  void slotSelectEvent(int);
  void slotSetPalette(int);
  void slotSetPaletteMax(int);
  void slotSetPaletteMin(int);
  void slotSetPaletteMode(int);
  void slotToggleShowItem(int);
  void slotInit();
          
protected:
  QMAGICDisplayDoc* doc;
  bool              trigger_status;
  QPalette*         palON;
  QPalette*         palOFF;
};

#endif // WDATGUI_H
