/***************************************************************************
                          qdisplayview.h  -  description
                             -------------------
    begin                : mié ene 12 22:01:32 CET 2000
    copyright            : (C) 2000 by Jose Carlos Gonzalez
    email                : gonzalez@mppmu.mpg.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef QDISPLAYVIEW_H
#define QDISPLAYVIEW_H

// include files for QT
#include <qwidget.h>
#include <qfile.h>
#include <qframe.h>

// application specific includes
#include "qdisplaydoc.h"

#include "infinitearray.h"

class QFrame;
class QLabel;
class QSpinBox;
class QComboBox;
class QPushButton;
class QButtonGroup;
class QLCDNumber;
class QRadioButton;;
class QPixmap;
class QPaintEvent;
class QTextStream;
class QColor;
class QStrList;
class QPrinter;
class QRect;
class QMainWindow;

class QwtPlot;

class QCamera : public QFrame {
  Q_OBJECT
  public:
    QCamera( QWidget* p=0, const char* name=0,
             int n=0, float *d=0, QRect *r=0, QMainWindow* mw=0 );
  protected:
    void mousePressEvent ( QMouseEvent * mev );
    void mouseReleaseEvent ( QMouseEvent * mev );
  private:
    int npixels;
    QWidget * parent;
    QLabel * tip;
    float * data;
    QRect * pRect;
    QMainWindow* mainWindow;
};

class EvtHdr;

/**
 * This class provides an incomplete base for your application view. 
 */

class QDisplayView : public QWidget
{
  Q_OBJECT
  public:
    QDisplayView(QWidget *parent=0, QDisplayDoc* doc=0);
    ~QDisplayView();
  
  	/** draws the camera frame */
  	void initCamera();
  	
  	/** prints the current event */
    void print();

    /** shows/hides the palette in the camera frame */
    void showPalette();
    void hidePalette();
    bool isPaletteVisible() const;
    void printInColor(bool flag);
    bool isPrintInColor() const;
    bool isDisplaySignal() const;
    bool isDisplayTimes() const;
    void setDisplaySignal(bool st);
    void setDisplayTimes(bool st);
    void printSetOptions( bool pInColor, bool pImage, bool pStat, bool pHist,
                          const char *pCommand, const char *pFilePrefix,
                          const char *pFileName, int pOutputTo );

  protected slots:
    void slotDocumentChanged();
    void slotReadEvent(int ev_);
    void slotOpenPalette();
    void slotSetModePalette(int mode);
    void slotSelectedPalette(int index);
    void slotSetMinDataScale(int x);
    void slotSetMaxDataScale(int x);

	private:
    QMainWindow* myParent;
    void clearView();
    void readPalette( const char * filename );
    void addPalette( const char * fname );
    void readASCIIEventHeader();
    void readASCIIEventImage();
    void readASCIIEventAdditionalData();
    int  readEvent(int ev);
    int  displayEvent();
    void setColors();
    void paintEvent( QPaintEvent * );
  	void enableWidgets();
  	void disableWidgets();
  	void repaintWidgets();
    void showPaletteColors();

    enum eModePalette { modePaletteLinear,
                        modePaletteLogarithmic,
                        modePaletteAuto };
    int modePalette;

  	const InfiniteArray<int> * evt;
  	const InfiniteArray<int> * evtToSave; 
  	
  	int nEvents;
  	int currentEvent;
  	
  	float* data;
  	
    EvtHdr* evthdr;

    QFile*       dataFile;
    QDisplayDoc* thisDoc;
    QTextStream* textFile;

    QPrinter*    printer;
    bool prnColor;
    bool prnImage;
    bool prnStat;
    bool prnHist;
    char prnCommand[256];
    char prnFilePrefix[256];
    char prnFileName[256];
    int prnOutputTo;

    bool displaySignal;
    bool displayTimes;

    enum {ePrnSetup, ePrnDirect, ePrnFiles, ePrnOneFile};

    QPainter *prnPainter;

    int numberOfPixelsInCamera;

    float zoomFactor;
    float zoom;
    int izoom;

    QRect * pixelRect;
    QPointArray * pixelHex;

    float minData;
    float maxData;

    float minDataScale;
    float maxDataScale;
    float lminDataScale;
    float lmaxDataScale;

    int* colors;
    int nColors;

    QColor*   palette;
    QStrList* paletteList;
 
    QPixmap *cameraViewBuffer;
    QPixmap *paletteSpectrumBuffer;

    QwtPlot* histSpectrum;
    long     histCurve;
    double   *histX;
    double   *histY;

    /** GUI widgets */
    QFrame* frmCamera;
    QSpinBox* spbxEventNumber;
    QLabel* lblMaxEvtNumber;
    QLabel* lblFileName;
    QComboBox* cboxPalette;
    QSpinBox* spbxPaletteMin;
    QSpinBox* spbxPaletteMax;
    QFrame* paletteSpectrum;
    QButtonGroup* rbPaletteMode;
    QLCDNumber* lcdEvtNumber;
    QPushButton* btnTrigger;

};

#endif // QDISPLAYVIEW_H
