/*************************************************************************
 *                                                                          
 * qscanplotdoc.cpp  -  description
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


#include "qscanplotdoc.h"

#include <qfile.h>
#include <qmsgbox.h>

#include "resource.h"

QScanPlotDoc::QScanPlotDoc()
{
  modified = false;

  theCurves = new QIntDict<Curve>(20);
  theCurves->insert(0, new Curve());

  theAxes = new QIntDict<Axes>(5);
  theAxes->insert(0, new Axes());
}

QScanPlotDoc::~QScanPlotDoc()
{
}

void QScanPlotDoc::newDoc()
{
}

bool QScanPlotDoc::save()
{
  return true;
}

bool QScanPlotDoc::saveAs(const QString &filename)
{
  QFile f( filename );
  f.open( IO_WriteOnly );                     // open file for writing
  QTextStream s( &f );                        // serialize using f

  s << *this;

  f.close();
  
  return true;
}

bool QScanPlotDoc::load(const QString &filename)
{
  QFile f( filename );
  f.open( IO_ReadOnly );                     // open file for writing
  QTextStream s( &f );                        // serialize using f

  s >> *this;

  f.close();
  
  if (! loadImage( imageName ))
    return false;

  emit documentChanged();
  return true;
}

bool QScanPlotDoc::isModified() const
{
  return modified;
}

/** Loads image file in any of the supported formats */
bool QScanPlotDoc::loadImage(const char* filename)
{
  bool ok = true;

  if ( filename )
    ok = image.load(filename, 0);

  imageName = QString(filename);
  
	return ok;
}

/** Exports image */
QImage* QScanPlotDoc::getImage()
{
	return &image;
}

QTextStream& operator<<(QTextStream & output, const QScanPlotDoc & aDoc)
{
  output << "############################################################\n";
  output << "# QScanPlot  version " << VERSION << '\n';
  output << "############################################################\n";
  output << "#\n";

  output << SERIALIZE_VERSION << "\n#\n";

  output << "# Image file\n" << aDoc.imageName << "\n#\n";

  output << "#----------------------------------------\n";
  output << "# Number of axes systems\n" << aDoc.nAxes << "\n#\n";

  int i;
  Axes *a;
  for (i=0; i<aDoc.nAxes; i++) {
    a = aDoc.theAxes->take(i);
    output << *a;
  }

  output << "#----------------------------------------\n";
  output << "# Number of curves defined\n" << aDoc.nCurves << "\n#\n";

  Curve *c;
  for (i=0; i<aDoc.nCurves; i++) {
    c = aDoc.theCurves->take(i);
    output << *c;
  }

  output << "#\n##EOF\n";

  return output;
}

QTextStream& operator>>(QTextStream & input, QScanPlotDoc & aDoc)
{
  float sv;  
  input >> sv;
  QString s;

  s = "#";
  while (s.at(0) == '#') s=input.readLine();
  sv = s.toFloat();

  qDebug("%% sv: [%s] -> [%f]", s.latin1(), sv);

  if (int(sv * 10) != int(SERIALIZE_VERSION * 10)) {
    QMessageBox::about(0, "ERROR", "File version incorrect");
    return input;
  }

  //-- read image name

  s = "#";
  while (s.at(0) == '#') s=input.readLine();

  aDoc.imageName = s;

  qDebug("%% aDoc.imageName: [%s]", aDoc.imageName.latin1());

  //-- read axes

  s = "#";
  while (s.at(0) == '#') s=input.readLine();

  aDoc.nAxes = s.toInt();

  qDebug("%% aDoc.nAxes:   [%d]", aDoc.nAxes);

  aDoc.theAxes->clear();
  aDoc.theAxes->resize( aDoc.nAxes );

  int i;
  Axes *a;
  for (i=0; i<aDoc.nAxes; i++) {
    a = new Axes();
    input >> *a;
    aDoc.theAxes->insert(i, a);
  }

  //-- read curves

  s = "#";
  while (s.at(0) == '#') s=input.readLine();

  aDoc.nCurves = s.toInt();

  qDebug("%% aDoc.nCurves:   [%d]", aDoc.nCurves);

  aDoc.theCurves->clear();
  aDoc.theCurves->resize( aDoc.nCurves );

  Curve *c;
  for (i=0; i<aDoc.nCurves; i++) {
    c = new Curve();
    input >> *c;
    aDoc.theCurves->insert(i, c);
  }

  return input;
}

/** Returns pointer to i-th curve */
Curve* QScanPlotDoc::getCurve(int i)
{
  return theCurves->take(i);
}

/** Returns pointer to i-th axes */
Axes* QScanPlotDoc::getAxes(int i)
{
  return theAxes->take(i);
}
