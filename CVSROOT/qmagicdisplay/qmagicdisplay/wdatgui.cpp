/*************************************************************************
*                                                                          
* wdatgui.cpp  -  description
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

#include "wdatgui.h"

#include <qlcdnumber.h>
#include <qspinbox.h>
#include <qlabel.h>
#include <qtooltip.h>
#include <qfiledialog.h>

#include "qmagicdisplaydoc.h"

/* 
*  Constructs a wDATGUI which is a child of 'parent', with the 
*  name 'name' and widget flags set to 'f' 
*/
wDATGUI::wDATGUI( QMAGICDisplayDoc* d,
                  QWidget* parent,
                  const char* name, WFlags fl )
  : wDATGUIDesign( parent, name, fl )
{
  qDebug("# wDATGUI::wDATGUI(...) constructor");
  
  doc = d;

  palOFF = new QPalette(this->palette());
  palON  = new QPalette;

  QColorGroup cg;
  cg.setColor( QColorGroup::Foreground, black );
  cg.setColor( QColorGroup::Button, QColor( 192, 192, 192) );
  cg.setColor( QColorGroup::Light, white );
  cg.setColor( QColorGroup::Midlight, QColor( 223, 223, 223) );
  cg.setColor( QColorGroup::Dark, QColor( 96, 96, 96) );
  cg.setColor( QColorGroup::Mid, QColor( 128, 128, 128) );
  cg.setColor( QColorGroup::Text, black );
  cg.setColor( QColorGroup::BrightText, white );
  cg.setColor( QColorGroup::ButtonText, black );
  cg.setColor( QColorGroup::Base, white );
  cg.setColor( QColorGroup::Background, QColor( 0, 255, 0) );
  cg.setColor( QColorGroup::Shadow, black );
  cg.setColor( QColorGroup::Highlight, QColor( 0, 0, 128) );
  cg.setColor( QColorGroup::HighlightedText, white );
  palON->setActive( cg );
  cg.setColor( QColorGroup::Foreground, black );
  cg.setColor( QColorGroup::Button, QColor( 192, 192, 192) );
  cg.setColor( QColorGroup::Light, white );
  cg.setColor( QColorGroup::Midlight, QColor( 220, 220, 220) );
  cg.setColor( QColorGroup::Dark, QColor( 96, 96, 96) );
  cg.setColor( QColorGroup::Mid, QColor( 128, 128, 128) );
  cg.setColor( QColorGroup::Text, black );
  cg.setColor( QColorGroup::BrightText, white );
  cg.setColor( QColorGroup::ButtonText, black );
  cg.setColor( QColorGroup::Base, white );
  cg.setColor( QColorGroup::Background, QColor( 0, 255, 0) );
  cg.setColor( QColorGroup::Shadow, black );
  cg.setColor( QColorGroup::Highlight, QColor( 0, 0, 128) );
  cg.setColor( QColorGroup::HighlightedText, white );
  palON->setInactive( cg );
  cg.setColor( QColorGroup::Foreground, QColor( 128, 128, 128) );
  cg.setColor( QColorGroup::Button, QColor( 192, 192, 192) );
  cg.setColor( QColorGroup::Light, white );
  cg.setColor( QColorGroup::Midlight, QColor( 220, 220, 220) );
  cg.setColor( QColorGroup::Dark, QColor( 96, 96, 96) );
  cg.setColor( QColorGroup::Mid, QColor( 128, 128, 128) );
  cg.setColor( QColorGroup::Text, QColor( 128, 128, 128) );
  cg.setColor( QColorGroup::BrightText, white );
  cg.setColor( QColorGroup::ButtonText, QColor( 128, 128, 128) );
  cg.setColor( QColorGroup::Base, white );
  cg.setColor( QColorGroup::Background, QColor( 0, 255, 0) );
  cg.setColor( QColorGroup::Shadow, black );
  cg.setColor( QColorGroup::Highlight, QColor( 0, 0, 128) );
  cg.setColor( QColorGroup::HighlightedText, white );
  palON->setDisabled( cg );

  trigger_status = true;
  setTrigger( false );
  
  wDATGUIDesign::update();
}

/*  
*  Destroys the object and frees any allocated resources
*/
wDATGUI::~wDATGUI()
{
  // no need to delete child widgets, Qt does it all for us
}

void wDATGUI::setTrigger(bool t)
{
  if (t != trigger_status) {
    trigger_status = !trigger_status;
    frmShowTrigger->setPalette( (t ? *palON : *palOFF) );
  }

  if (t) qDebug("  T R I G G E R");
}

void wDATGUI::setTotalEvents(int n)
{
  qDebug("setting total number of events to %d",n);
  
  LCDTotalEvents->display(n);
  spbxEvtNumber->setMaxValue(n);
}

/* 
* public slot
*/
void wDATGUI::slotBrowsePalettes()
{
  QString fileName = QFileDialog::getOpenFileName(0,0,this);

  if (!fileName.isEmpty()) {
    addPalette( fileName, true );
  }
}
/* 
* public slot
*/
void wDATGUI::slotSelectEvent(int e)
{
  emit eventChanged(e);
}
/* 
* public slot
*/
void wDATGUI::slotSetPalette(int n)
{
  emit setPalette( n );
  emit reDisplayEvent();
}
/* 
* public slot
*/
void wDATGUI::slotSetPaletteMax(int n)
{
  if (n > spbxPalMin->value()) {
    emit setPaletteMax( n );
    emit reDisplayEvent();
  } else {
    spbxPalMax->setValue(n+1);
  }
}
/* 
* public slot
*/
void wDATGUI::slotSetPaletteMin(int n)
{
  if (n < spbxPalMax->value()) {
    emit setPaletteMin( n );
    emit reDisplayEvent();
  } else {
    spbxPalMin->setValue(n-1);
  }
}
/* 
* public slot
*/
void wDATGUI::slotSetPaletteMode(int n)
{
  emit setPaletteMode( n );
  emit reDisplayEvent();
}
/* 
* public slot
*/
void wDATGUI::slotToggleShowItem(int i)
{
  emit toggleShowItem(i);
}
/*
* public slot, activated from parent
*/
void wDATGUI::slotInit()
{
  addPalette( "/tmp/White2Black" );
  addPalette( "/tmp/Black2White" );
  addPalette( "/tmp/Firecode" );
}
/*
* changed the filename
*/
void wDATGUI::setFilename(const char* s)
{
  lblFilename->setText( s );
  lblFilename->update();
  QToolTip::remove( lblFilename );
  QToolTip::add( lblFilename, "Filename: " + QString(s) );
}
/** add palette name into list */
void wDATGUI::addPalette( const char* fname, bool redisplay )
{
  QString fileName( fname );
  QFileInfo fpalInfo( fileName );
  QString basename( fpalInfo.baseName() );
  //QString absname( fpalInfo.absFilePath() );

  cbxPalFile->insertItem( basename );
  cbxPalFile->setCurrentItem( cbxPalFile->count() - 1 );

  emit addingPalette( fname );
  if ( redisplay )
    emit reDisplayEvent();
}

