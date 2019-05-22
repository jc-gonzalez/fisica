/***************************************************************************
                          qrunhist.h  -  description
                             -------------------
    begin                : Fri Oct 13 2000
    copyright            : (C) 2000 by J C Gonzalez
    email                : gonzalez@gae.ucm.es
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef QRUNHIST_H
#define QRUNHIST_H

//Generated area. DO NOT EDIT!!!(begin)
#include <qwidget.h>
#include <qpushbutton.h>
//Generated area. DO NOT EDIT!!!(end)

#include <qdialog.h>

/**
  *@author J C Gonzalez
  */

class QRunHist : public QDialog  {
   Q_OBJECT
public: 
	QRunHist(QWidget *parent=0, const char *name=0);
	~QRunHist();

protected: 
	void initDialog();
	//Generated area. DO NOT EDIT!!!(begin)
	QWidget *QWidget_1;
	QPushButton *QPushButton_1;
	//Generated area. DO NOT EDIT!!!(end)

private: 
};

#endif
