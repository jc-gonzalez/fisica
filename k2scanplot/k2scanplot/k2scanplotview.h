/*************************************************************************
 *                                                                          
 * k2scanplotview.h  -  description
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


#ifndef K2SCANPLOTVIEW_H
#define K2SCANPLOTVIEW_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif 

// include files for Qt
#include <qwidget.h>

class K2ScanPlotDoc;

/** The K2ScanPlotView class provides the view widget for the K2ScanPlotApp instance.	
 * The View instance inherits QWidget as a base class and represents the view object of a KTMainWindow. As K2ScanPlotView is part of the
 * docuement-view model, it needs a reference to the document object connected with it by the K2ScanPlotApp class to manipulate and display
 * the document structure provided by the K2ScanPlotDoc class.
 * 	
 * @author Source Framework Automatically Generated by KDevelop, (c) The KDevelop Team.
 * @version KDevelop version 0.4 code generation
 */
class K2ScanPlotView : public QWidget
{
  Q_OBJECT
  public:
    /** Constructor for the main view */
    K2ScanPlotView(QWidget *parent = 0, const char *name=0);
    /** Destructor for the main view */
    ~K2ScanPlotView();

    /** returns a pointer to the document connected to the view instance. Mind that this method requires a K2ScanPlotApp instance as a parent
     * widget to get to the window document pointer by calling the K2ScanPlotApp::getDocument() method.
     *
     * @see K2ScanPlotApp#getDocument
     */
    K2ScanPlotDoc *getDocument() const;

    /** contains the implementation for printing functionality */
    void print(QPrinter *pPrinter);
	
  private:
	
};

#endif // K2SCANPLOTVIEW_H
