/***************************************************************************
                          qprintoptions.h  -  description
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

#ifndef QPRINTOPTIONS_H
#define QPRINTOPTIONS_H

//Generated area. DO NOT EDIT!!!(begin)
#include <qwidget.h>
#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qbuttongroup.h>
#include <qradiobutton.h>
#include <qlineedit.h>
#include <qlabel.h>
//Generated area. DO NOT EDIT!!!(end)

#include <qdialog.h>

/**
  *@author J C Gonzalez
  */

class QPrintOptions : public QDialog  {
   Q_OBJECT
public: 
	QPrintOptions(QWidget *parent=0, const char *name=0);
	~QPrintOptions();

  void set_PrintInColor(bool b);
  bool get_PrintInColor();
  bool get_PrintImage();
  bool get_PrintStat();
  bool get_PrintHist();
  const char * get_PrintCommand();
  const char * get_PrintFilePrefix();
  const char * get_PrintFileName();
  int get_PrintOutputTo();

protected:
	void initDialog();
	//Generated area. DO NOT EDIT!!!(begin)
	QPushButton *btnOK;
	QPushButton *btnCancel;
	QCheckBox *chkbxPrintInColor;
	QCheckBox *chkbxPrintImage;
	QCheckBox *chkbxPrintStatistics;
	QCheckBox *chkbxPrintHistograms;
	QButtonGroup *btngrpOutput;
	QRadioButton *rbPrinterSetup;
	QRadioButton *rbPrinterPrinter;
	QRadioButton *rbPrinterFile;
	QRadioButton *rbPrinterFileAll;
	QLineEdit *lePrintCommand;
	QLabel *QLabel_1;
	QLabel *QLabel_2;
	QLineEdit *lePrintPrefix;
	QLabel *QLabel_3;
	QLineEdit *lePrintFilename;
	QPushButton *QPushButton_3;
	//Generated area. DO NOT EDIT!!!(end)

protected slots:
  void set_PrintOutputTo(int i);

private: 
  int printOutputTo;

};

#endif
