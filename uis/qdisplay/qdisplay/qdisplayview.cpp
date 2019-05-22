/***************************************************************************
                          qdisplayview.cpp  -  description
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

#include "qdisplayview.h"

#include "resource.h"
#include "pixels_simplearray.h"
#include "evthdr.h"

//#include "qmytips.h"

#include <qtooltip.h>

#include <qframe.h>
#include <qspinbox.h>
#include <qlabel.h>
#include <qcombobox.h>
#include <qpushbutton.h>
#include <qpainter.h>
#include <qpixmap.h>
#include <qdrawutil.h>
#include <qtextstream.h>
#include <qfileinfo.h>
#include <qfiledialog.h>
#include <qstrlist.h>
#include <qprinter.h>
#include <qpaintdevicemetrics.h>
#include <qdatetime.h>
#include <qbuttongroup.h>
#include <qradiobutton.h>
#include <qlcdnumber.h>
#include <qrect.h>
#include <qmainwindow.h>
#include <qstatusbar.h>

#include <qwt_plot.h>

#include <math.h>

#define RGB(x)  static_cast<QRgb>(x)

static const float primaryZoom = 26.0;
static const int maxX = 1000;
static const int maxY = 1000;
static const int ox = maxX / 2;
static const int oy = maxY / 2;

static char wholeLine[20000];

inline int   irint(float x) { return int(floor(x+0.5)); }
inline float frint(float x) { return floor(x+0.5); }

/*
inline int   irint(float x) { return int(ceil(x)); }
inline float frint(float x) { return ceil(x); }
*/
#define SIN60   0.866025403784439
#define COS60   0.500000000000000
#define COS30   SIN60
#define SIN30   COS60

static const int numberOfColors = 64;

QCamera::QCamera( QWidget* p=0, const char* name=0,
                  int n, float *d=0, QRect *r=0,
                  QMainWindow* mw ) : QFrame( p, name )
{
  tip = new QLabel(this);
  tip->hide();
  tip->setBackgroundColor( yellow );
  npixels = n;
  data = d;
  pRect = r;
  mainWindow = mw;
}

void QCamera::mousePressEvent ( QMouseEvent * mev )
{
  if ( mev->button() == RightButton ) {
    register int i;

    QPoint p( ((mev->pos()).x()*maxX)/this->width(),
              ((mev->pos()).y()*maxY)/this->height() );

    for ( i=0; i<npixels; i++ ) {
      if ( pRect[i].contains( p ) ) {
        cout << "Pertenece al pixel: " << i << endl << flush;
        QString s;
        s.sprintf("Pixel # %3d: Value %f", i, data[i]);
        mainWindow->statusBar()->message(s);
        /*
        tip->setText( s.sprintf("%f", data[i]) );
        cout << "Valor: " << s << endl << flush;
        tip->move( mev->pos() );
        cout << "Moved label" << endl << flush;
        tip->show();
        cout << "Shown" << endl << flush;
        */
        break;
      }
    }

  }
}

void QCamera::mouseReleaseEvent ( QMouseEvent * mev )
{
  mainWindow->statusBar()->message(IDS_STATUS_DEFAULT, 2000);
  //tip->hide();
}

QDisplayView::QDisplayView(QWidget *parent, QDisplayDoc *doc) : QWidget(parent)
{
  register int i;

  myParent = (QMainWindow*)parent;

  thisDoc = doc;

  evt          = thisDoc->ptr_evt();
  evtToSave    = thisDoc->ptr_evtToSave();

  evthdr = new EvtHdr();

  numberOfPixelsInCamera = numberOfPixels;

  pixelRect = new QRect [ numberOfPixelsInCamera ];
  pixelHex  = new QPointArray [ numberOfPixelsInCamera ];

  zoomFactor = 1.0;
  zoom = primaryZoom * zoomFactor;
  izoom = int(zoom*1.0);

  float ftempsize,ftempsize_circle;
  float fctempsize, fstempsize;
  float fpx, fpy;

  for (i=0; i<numberOfPixelsInCamera; i++) {
    fpx = ox + pix[i][0]*zoom;
    fpy = oy - pix[i][1]*zoom;
    ftempsize = zoom*pixsize[i]*0.55; // for overlapping (trick  :-P
    fctempsize = ftempsize*COS30;
    fstempsize = ftempsize*SIN30;
    pixelHex[i].putPoints( 0, 6,
                           irint(fpx+fctempsize), irint(fpy-fstempsize),
                           irint(fpx),            irint(fpy-ftempsize),
                           irint(fpx-fctempsize), irint(fpy-fstempsize),
                           irint(fpx-fctempsize), irint(fpy+fstempsize),
                           irint(fpx),            irint(fpy+ftempsize),
                           irint(fpx+fctempsize), irint(fpy+fstempsize)
                          );
    ftempsize_circle = fctempsize * cos( M_PI / 4.0 );
    pixelRect[i].setRect( irint(fpx-ftempsize_circle),
                          irint(fpy-ftempsize_circle),
                          irint(2.0*ftempsize_circle),
                          irint(2.0*ftempsize_circle) );
  }

  data = new float[ numberOfPixelsInCamera ];

  // initialize values for colors
  minDataScale = 1;
  maxDataScale = 1000;

  initCamera();

  cameraViewBuffer = new QPixmap( frmCamera->size() );
  paletteSpectrumBuffer = new QPixmap( paletteSpectrum->size() );

  palette = new QColor [ numberOfColors ];
  paletteList = new QStrList( TRUE );
  addPalette( "/tmp/White2Black" );
  addPalette( "/tmp/Black2White" );
  addPalette( "/tmp/Firecode" );

	histX = new double[ numberOfColors ];
	histY = new double[ numberOfColors ];
	
  rbPaletteMode->setButton( modePaletteLinear );

  colors = new int [ numberOfPixelsInCamera ];

  printer = new QPrinter;
  printer->setMinMax( 1, 10 );

  prnColor = false;
  prnImage = true;
  prnStat = true;
  prnHist = true;
  strcpy(prnCommand, "lpr ");
  strcpy(prnFilePrefix, "qd_");
  strcpy(prnFileName, "/tmp/qd.ps");
  prnOutputTo = ePrnSetup;
  prnPainter = 0;

  displaySignal = true;
  displayTimes = false;

  textFile = new QTextStream;

  /** initializes view (clears buffer) */
  clearView();

  /** connect doc with the view*/
  connect(thisDoc, SIGNAL(documentChanged()),
          this, SLOT(slotDocumentChanged()));

  disableWidgets();

  setMouseTracking( TRUE );
}

QDisplayView::~QDisplayView()
{
  delete [] colors;
  delete [] data;
  delete evthdr;
}

/** clears the view, through the buffer */
void QDisplayView::clearView()
{
  QPainter buffer_painter;
  buffer_painter.begin( cameraViewBuffer );
  QBrush brush( frmCamera->backgroundColor() );   
  qDrawShadeRect(&buffer_painter,
                 0, 0,
                 frmCamera->width(),
                 frmCamera->height(),
                 frmCamera->colorGroup(),
                 frmCamera->frameStyle(),
                 frmCamera->lineWidth(),
                 frmCamera->midLineWidth(),
                 &brush);
  buffer_painter.end();
  update();
}

/** draws the camera frame */
void QDisplayView::initCamera()
{
	QFrame* qtarch_Frame_4;
	qtarch_Frame_4 = new QFrame( this, "Frame_4" );
	qtarch_Frame_4->setGeometry( 520, 110, 400, 210 );
	qtarch_Frame_4->setMinimumSize( 0, 0 );
	qtarch_Frame_4->setMaximumSize( 32767, 32767 );
	qtarch_Frame_4->setFocusPolicy( QWidget::NoFocus );
	qtarch_Frame_4->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_Frame_4->setFontPropagation( QWidget::NoChildren );
	qtarch_Frame_4->setPalettePropagation( QWidget::NoChildren );
	qtarch_Frame_4->setFrameStyle( 33 );

	QFrame* qtarch_Frame_5;
	qtarch_Frame_5 = new QFrame( this, "Frame_5" );
	qtarch_Frame_5->setGeometry( 530, 220, 380, 50 );
	qtarch_Frame_5->setMinimumSize( 0, 0 );
	qtarch_Frame_5->setMaximumSize( 32767, 32767 );
	qtarch_Frame_5->setFocusPolicy( QWidget::NoFocus );
	qtarch_Frame_5->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_Frame_5->setFontPropagation( QWidget::NoChildren );
	qtarch_Frame_5->setPalettePropagation( QWidget::NoChildren );
	qtarch_Frame_5->setFrameStyle( 49 );

	rbPaletteMode = new QButtonGroup( this, "rbPaletteMode" );
	rbPaletteMode->setGeometry( 530, 160, 380, 50 );
	rbPaletteMode->setMinimumSize( 0, 0 );
	rbPaletteMode->setMaximumSize( 32767, 32767 );
	rbPaletteMode->setFocusPolicy( QWidget::NoFocus );
	rbPaletteMode->setBackgroundMode( QWidget::PaletteBackground );
	rbPaletteMode->setFontPropagation( QWidget::NoChildren );
	rbPaletteMode->setPalettePropagation( QWidget::NoChildren );
	rbPaletteMode->setFrameStyle( 49 );
	rbPaletteMode->setTitle( "Palette scale mode:" );
	rbPaletteMode->setAlignment( 1 );

	//frmCamera = new QFrame( this, "frmCamera" );
  frmCamera = new QCamera( this, "frmCamera",
                           numberOfPixelsInCamera, data, pixelRect,
                           myParent );
	frmCamera->setGeometry( 10, 10, 500, 500 );
	frmCamera->setMinimumSize( 500, 500 );
	frmCamera->setMaximumSize( 500, 500 );
	frmCamera->setFocusPolicy( QWidget::NoFocus );
	frmCamera->setBackgroundMode( QWidget::PaletteBackground );
	frmCamera->setFontPropagation( QWidget::NoChildren );
	frmCamera->setPalettePropagation( QWidget::NoChildren );
	frmCamera->setFrameStyle( 50 );

	QFrame* qtarch_Frame_2;
	qtarch_Frame_2 = new QFrame( this, "Frame_2" );
	qtarch_Frame_2->setGeometry( 520, 10, 400, 90 );
	qtarch_Frame_2->setMinimumSize( 0, 0 );
	qtarch_Frame_2->setMaximumSize( 32767, 32767 );
	qtarch_Frame_2->setFocusPolicy( QWidget::NoFocus );
	qtarch_Frame_2->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_Frame_2->setFontPropagation( QWidget::NoChildren );
	qtarch_Frame_2->setPalettePropagation( QWidget::NoChildren );
	qtarch_Frame_2->setFrameStyle( 33 );

	QLabel* qtarch_Label_2;
	qtarch_Label_2 = new QLabel( this, "Label_2" );
	qtarch_Label_2->setGeometry( 530, 60, 110, 30 );
	qtarch_Label_2->setMinimumSize( 0, 0 );
	qtarch_Label_2->setMaximumSize( 32767, 32767 );
	qtarch_Label_2->setFocusPolicy( QWidget::NoFocus );
	qtarch_Label_2->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_Label_2->setFontPropagation( QWidget::NoChildren );
	qtarch_Label_2->setPalettePropagation( QWidget::NoChildren );
	qtarch_Label_2->setText( "Event number:" );
	qtarch_Label_2->setAlignment( 289 );
	qtarch_Label_2->setMargin( -1 );

	spbxEventNumber = new QSpinBox( this, "spbxEventNumber" );
	spbxEventNumber->setGeometry( 640, 65, 80, 25 );
	spbxEventNumber->setMinimumSize( 0, 0 );
	spbxEventNumber->setMaximumSize( 32767, 32767 );
	spbxEventNumber->setFocusPolicy( QWidget::StrongFocus );
	spbxEventNumber->setBackgroundMode( QWidget::PaletteBackground );
	spbxEventNumber->setFontPropagation( QWidget::NoChildren );
	spbxEventNumber->setPalettePropagation( QWidget::NoChildren );
	spbxEventNumber->setFrameStyle( 50 );
	spbxEventNumber->setLineWidth( 2 );
	spbxEventNumber->setRange( 0, 99 );
	spbxEventNumber->setSteps( 1, 0 );
	spbxEventNumber->setPrefix( "" );
	spbxEventNumber->setSuffix( "" );
	spbxEventNumber->setSpecialValueText( "" );
	spbxEventNumber->setWrapping( FALSE );

	lblMaxEvtNumber = new QLabel( this, "Label_3" );
	lblMaxEvtNumber->setGeometry( 750, 60, 100, 30 );
	lblMaxEvtNumber->setMinimumSize( 0, 0 );
	lblMaxEvtNumber->setMaximumSize( 32767, 32767 );
	lblMaxEvtNumber->setFocusPolicy( QWidget::NoFocus );
	lblMaxEvtNumber->setBackgroundMode( QWidget::PaletteBackground );
	lblMaxEvtNumber->setFontPropagation( QWidget::NoChildren );
	lblMaxEvtNumber->setPalettePropagation( QWidget::NoChildren );
	lblMaxEvtNumber->setText( "of     ######" );
	lblMaxEvtNumber->setAlignment( 289 );
	lblMaxEvtNumber->setMargin( -1 );

	lblFileName = new QLabel( this, "lblFileName" );
	lblFileName->setGeometry( 530, 20, 380, 30 );
	lblFileName->setMinimumSize( 0, 0 );
	lblFileName->setMaximumSize( 32767, 32767 );
	lblFileName->setFocusPolicy( QWidget::NoFocus );
	lblFileName->setBackgroundMode( QWidget::PaletteBackground );
	lblFileName->setFontPropagation( QWidget::NoChildren );
	lblFileName->setPalettePropagation( QWidget::NoChildren );
	lblFileName->setText( "        <no file open>" );
	lblFileName->setAlignment( 289 );
	lblFileName->setMargin( -1 );

	QLabel* qtarch_Label_5;
	qtarch_Label_5 = new QLabel( this, "Label_5" );
	qtarch_Label_5->setGeometry( 530, 120, 60, 30 );
	qtarch_Label_5->setMinimumSize( 0, 0 );
	qtarch_Label_5->setMaximumSize( 32767, 32767 );
	qtarch_Label_5->setFocusPolicy( QWidget::NoFocus );
	qtarch_Label_5->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_Label_5->setFontPropagation( QWidget::NoChildren );
	qtarch_Label_5->setPalettePropagation( QWidget::NoChildren );
	qtarch_Label_5->setText( "Palette:" );
	qtarch_Label_5->setAlignment( 289 );
	qtarch_Label_5->setMargin( -1 );

	cboxPalette = new QComboBox( FALSE, this, "cboxPalette" );
	cboxPalette->setGeometry( 630, 120, 180, 30 );
	cboxPalette->setMinimumSize( 0, 0 );
	cboxPalette->setMaximumSize( 32767, 32767 );
	cboxPalette->setFocusPolicy( QWidget::StrongFocus );
	cboxPalette->setBackgroundMode( QWidget::PaletteBackground );
	cboxPalette->setFontPropagation( QWidget::AllChildren );
	cboxPalette->setPalettePropagation( QWidget::AllChildren );
	cboxPalette->setSizeLimit( 100 );
	cboxPalette->setAutoResize( FALSE );

	QPushButton* qtarch_PushButton_1;
	qtarch_PushButton_1 = new QPushButton( this, "PushButton_1" );
	qtarch_PushButton_1->setGeometry( 820, 120, 90, 30 );
	qtarch_PushButton_1->setMinimumSize( 0, 0 );
	qtarch_PushButton_1->setMaximumSize( 32767, 32767 );
	qtarch_PushButton_1->setFocusPolicy( QWidget::TabFocus );
	qtarch_PushButton_1->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_PushButton_1->setFontPropagation( QWidget::NoChildren );
	qtarch_PushButton_1->setPalettePropagation( QWidget::NoChildren );
	qtarch_PushButton_1->setText( "&Browse..." );
	qtarch_PushButton_1->setAutoRepeat( FALSE );
	qtarch_PushButton_1->setAutoResize( FALSE );

	paletteSpectrum = new QFrame( this, "paletteSpectrum" );
	paletteSpectrum->setGeometry( 560, 280, 330, 30 );
	paletteSpectrum->setMinimumSize( 0, 0 );
	paletteSpectrum->setMaximumSize( 32767, 32767 );
	paletteSpectrum->setFocusPolicy( QWidget::NoFocus );
	paletteSpectrum->setBackgroundMode( QWidget::PaletteBackground );
	paletteSpectrum->setFontPropagation( QWidget::NoChildren );
	paletteSpectrum->setPalettePropagation( QWidget::NoChildren );

	lcdEvtNumber = new QLCDNumber( this, "lcdEvtNumber" );
	lcdEvtNumber->setGeometry( 405, 15, 100, 30 );
	lcdEvtNumber->setMinimumSize( 0, 0 );
	lcdEvtNumber->setMaximumSize( 32767, 32767 );
  {
		QColorGroup normal( QColor( RGB(0) ), QColor( RGB(11846340) ), QColor( RGB(10789024) ), QColor( RGB(8421504) ), QColor( RGB(10789024) ), QColor( RGB(0) ), QColor( RGB(0) ) );
		QColorGroup disabled( QColor( RGB(8421504) ), QColor( RGB(12632256) ), QColor( RGB(16777215) ), QColor( RGB(8421504) ), QColor( RGB(10789024) ), QColor( RGB(8421504) ), QColor( RGB(12632256) ) );
		QColorGroup active( QColor( RGB(0) ), QColor( RGB(12632256) ), QColor( RGB(16777215) ), QColor( RGB(8421504) ), QColor( RGB(10789024) ), QColor( RGB(0) ), QColor( RGB(16777215) ) );
		QPalette palette( normal, disabled, active );
		lcdEvtNumber->setPalette( palette );
  }
	lcdEvtNumber->setFocusPolicy( QWidget::NoFocus );
	lcdEvtNumber->setBackgroundMode( QWidget::PaletteBackground );
	lcdEvtNumber->setFontPropagation( QWidget::NoChildren );
	lcdEvtNumber->setPalettePropagation( QWidget::NoChildren );
	lcdEvtNumber->setFrameStyle( 33 );
	lcdEvtNumber->setSmallDecimalPoint( FALSE );
	lcdEvtNumber->setNumDigits( 6 );
	lcdEvtNumber->setMode( QLCDNumber::DEC );
	lcdEvtNumber->setSegmentStyle( QLCDNumber::Filled );

	QRadioButton* qtarch_RadioButton_1;
	qtarch_RadioButton_1 = new QRadioButton( this, "RadioButton_1" );
	qtarch_RadioButton_1->setGeometry( 550, 180, 100, 20 );
	qtarch_RadioButton_1->setMinimumSize( 0, 0 );
	qtarch_RadioButton_1->setMaximumSize( 32767, 32767 );
	qtarch_RadioButton_1->setFocusPolicy( QWidget::TabFocus );
	qtarch_RadioButton_1->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_RadioButton_1->setFontPropagation( QWidget::NoChildren );
	qtarch_RadioButton_1->setPalettePropagation( QWidget::NoChildren );
	qtarch_RadioButton_1->setText( "Linear" );
	qtarch_RadioButton_1->setAutoRepeat( FALSE );
	qtarch_RadioButton_1->setAutoResize( FALSE );

	QRadioButton* qtarch_RadioButton_2;
	qtarch_RadioButton_2 = new QRadioButton( this, "RadioButton_2" );
	qtarch_RadioButton_2->setGeometry( 670, 180, 100, 20 );
	qtarch_RadioButton_2->setMinimumSize( 0, 0 );
	qtarch_RadioButton_2->setMaximumSize( 32767, 32767 );
	qtarch_RadioButton_2->setFocusPolicy( QWidget::TabFocus );
	qtarch_RadioButton_2->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_RadioButton_2->setFontPropagation( QWidget::NoChildren );
	qtarch_RadioButton_2->setPalettePropagation( QWidget::NoChildren );
	qtarch_RadioButton_2->setText( "Logarithmic" );
	qtarch_RadioButton_2->setAutoRepeat( FALSE );
	qtarch_RadioButton_2->setAutoResize( FALSE );

	QRadioButton* qtarch_RadioButton_3;
	qtarch_RadioButton_3 = new QRadioButton( this, "RadioButton_3" );
	qtarch_RadioButton_3->setGeometry( 790, 180, 100, 20 );
	qtarch_RadioButton_3->setMinimumSize( 0, 0 );
	qtarch_RadioButton_3->setMaximumSize( 32767, 32767 );
	qtarch_RadioButton_3->setFocusPolicy( QWidget::TabFocus );
	qtarch_RadioButton_3->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_RadioButton_3->setFontPropagation( QWidget::NoChildren );
	qtarch_RadioButton_3->setPalettePropagation( QWidget::NoChildren );
	qtarch_RadioButton_3->setText( "Auto" );
	qtarch_RadioButton_3->setAutoRepeat( FALSE );
	qtarch_RadioButton_3->setAutoResize( FALSE );

	spbxPaletteMin = new QSpinBox( this, "spbxPaletteMin" );
	spbxPaletteMin->setGeometry( 610, 230, 80, 25 );
	spbxPaletteMin->setMinimumSize( 0, 0 );
	spbxPaletteMin->setMaximumSize( 32767, 32767 );
	spbxPaletteMin->setFocusPolicy( QWidget::StrongFocus );
	spbxPaletteMin->setBackgroundMode( QWidget::PaletteBackground );
	spbxPaletteMin->setFontPropagation( QWidget::NoChildren );
	spbxPaletteMin->setPalettePropagation( QWidget::NoChildren );
	spbxPaletteMin->setFrameStyle( 50 );
	spbxPaletteMin->setLineWidth( 2 );
	spbxPaletteMin->setRange( 0, 99 );
	spbxPaletteMin->setSteps( 1, 0 );
	spbxPaletteMin->setPrefix( "" );
	spbxPaletteMin->setSuffix( "" );
	spbxPaletteMin->setSpecialValueText( "" );
	spbxPaletteMin->setWrapping( FALSE );

	QLabel* qtarch_Label_6;
	qtarch_Label_6 = new QLabel( this, "Label_6" );
	qtarch_Label_6->setGeometry( 540, 230, 70, 20 );
	qtarch_Label_6->setMinimumSize( 0, 0 );
	qtarch_Label_6->setMaximumSize( 32767, 32767 );
	qtarch_Label_6->setFocusPolicy( QWidget::NoFocus );
	qtarch_Label_6->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_Label_6->setFontPropagation( QWidget::NoChildren );
	qtarch_Label_6->setPalettePropagation( QWidget::NoChildren );
	qtarch_Label_6->setText( "Min:" );
	qtarch_Label_6->setAlignment( 289 );
	qtarch_Label_6->setMargin( -1 );

	QLabel* qtarch_Label_7;
	qtarch_Label_7 = new QLabel( this, "Label_7" );
	qtarch_Label_7->setGeometry( 730, 230, 60, 20 );
	qtarch_Label_7->setMinimumSize( 0, 0 );
	qtarch_Label_7->setMaximumSize( 32767, 32767 );
	qtarch_Label_7->setFocusPolicy( QWidget::NoFocus );
	qtarch_Label_7->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_Label_7->setFontPropagation( QWidget::NoChildren );
	qtarch_Label_7->setPalettePropagation( QWidget::NoChildren );
	qtarch_Label_7->setText( "Max:" );
	qtarch_Label_7->setAlignment( 289 );
	qtarch_Label_7->setMargin( -1 );

	spbxPaletteMax = new QSpinBox( this, "spbxPaletteMax" );
	spbxPaletteMax->setGeometry( 800, 230, 80, 25 );
	spbxPaletteMax->setMinimumSize( 0, 0 );
	spbxPaletteMax->setMaximumSize( 32767, 32767 );
	spbxPaletteMax->setFocusPolicy( QWidget::StrongFocus );
	spbxPaletteMax->setBackgroundMode( QWidget::PaletteBackground );
	spbxPaletteMax->setFontPropagation( QWidget::NoChildren );
	spbxPaletteMax->setPalettePropagation( QWidget::NoChildren );
	spbxPaletteMax->setFrameStyle( 50 );
	spbxPaletteMax->setLineWidth( 2 );
	spbxPaletteMax->setRange( 1, 1000 );
	spbxPaletteMax->setSteps( 1, 0 );
	spbxPaletteMax->setPrefix( "" );
	spbxPaletteMax->setSuffix( "" );
	spbxPaletteMax->setSpecialValueText( "" );
	spbxPaletteMax->setWrapping( FALSE );

	QFrame* qtarch_Frame_9;
	qtarch_Frame_9 = new QFrame( this, "Frame_9" );
	qtarch_Frame_9->setGeometry( 528, 320, 384, 200 );
	qtarch_Frame_9->setMinimumSize( 384, 200 );
	qtarch_Frame_9->setMaximumSize( 384, 200 );
	qtarch_Frame_9->setFocusPolicy( QWidget::NoFocus );
	qtarch_Frame_9->setBackgroundMode( QWidget::PaletteBackground );
	qtarch_Frame_9->setFontPropagation( QWidget::NoChildren );
	qtarch_Frame_9->setPalettePropagation( QWidget::NoChildren );
	qtarch_Frame_9->setFrameStyle( 0 );

  btnTrigger = new QPushButton( this, "btnTrigger" );
  btnTrigger->setGeometry( 470, 470, 30, 30 );
  btnTrigger->setMinimumSize( 30, 30 );
  btnTrigger->setMaximumSize( 30, 30 );
  {
    QColorGroup normal( QColor( RGB(0) ), QColor( RGB(65280) ), QColor( RGB(16777215) ), QColor( RGB(8421504) ), QColor( RGB(10789024) ), QColor( RGB(0) ), QColor( RGB(16777215) ) );
    QColorGroup disabled( QColor( RGB(8421504) ), QColor( RGB(12632256) ), QColor( RGB(16777215) ), QColor( RGB(8421504) ), QColor( RGB(10789024) ), QColor( RGB(8421504) ), QColor( RGB(12632256) ) );
    QColorGroup active( QColor( RGB(0) ), QColor( RGB(12632256) ), QColor( RGB(16777215) ), QColor( RGB(8421504) ), QColor( RGB(10789024) ), QColor( RGB(0) ), QColor( RGB(16777215) ) );
    QPalette palette( normal, disabled, active );
    btnTrigger->setPalette( palette );
  }
  btnTrigger->setFocusPolicy( QWidget::TabFocus );
  btnTrigger->setBackgroundMode( QWidget::PaletteBackground );
  btnTrigger->setFontPropagation( QWidget::NoChildren );
  btnTrigger->setPalettePropagation( QWidget::NoChildren );
  btnTrigger->setText( "T" );
  btnTrigger->setAutoRepeat( FALSE );
  btnTrigger->setAutoResize( FALSE );

 	rbPaletteMode->insert( qtarch_RadioButton_1 );
	rbPaletteMode->insert( qtarch_RadioButton_2 );
	rbPaletteMode->insert( qtarch_RadioButton_3 );

	resize( 930,620 );
	setMinimumSize( 930, 620 );
	setMaximumSize( 930, 620 );

  frmCamera->setBackgroundColor( QColor( QRgb( 0x660000 ) ) );
  //frmCamera->setBackgroundColor( darkblue );

  // add plot widget
  histSpectrum = new QwtPlot("", qtarch_Frame_9);
  CHECK_PTR( histSpectrum );
	
  // insert new curve
  histCurve = histSpectrum->insertCurve("nphes");

  // set curve styles
  histSpectrum->setCurvePen(histCurve, QPen(red));
  histSpectrum->setCurveStyle(histCurve, QwtCurve::Sticks);


  // disable axes titles
  //histSpectrum->enableAxis(QwtPlot::xBottom, TRUE);
  histSpectrum->setAxisTitle(QwtPlot::xBottom, "number of ph.e-s per pixel");
  histSpectrum->enableAxis(QwtPlot::yLeft, FALSE);

  // we want a legend
  histSpectrum->enableLegend(FALSE);

  histSpectrum->setFrameStyle( 0 );
  histSpectrum->resize( (histSpectrum->parentWidget())->size() );

  //-- SIGNAL/SLOT CONNECTIONS --
  
  //connect(spbxEventNumber, SIGNAL(valueChanged(int)),
  //        SLOT(slotReadEvent(int)));
  connect(spbxEventNumber, SIGNAL(valueChanged(int)),
          SLOT(slotReadEvent(int)));
  connect(spbxEventNumber, SIGNAL(valueChanged(int)),
          lcdEvtNumber, SLOT(display(int)));
  connect(qtarch_PushButton_1, SIGNAL(clicked()),
          SLOT(slotOpenPalette()));
  connect(cboxPalette, SIGNAL(activated(int)),
          SLOT(slotSelectedPalette(int)));
  connect(rbPaletteMode, SIGNAL(clicked(int)),
          SLOT(slotSetModePalette(int)));
  connect(spbxPaletteMin, SIGNAL(valueChanged(int)),
          SLOT(slotSetMinDataScale(int)));
  connect(spbxPaletteMax, SIGNAL(valueChanged(int)),
          SLOT(slotSetMaxDataScale(int)));

}

/** disable interface */
void QDisplayView::disableWidgets() {
  rbPaletteMode->setEnabled( FALSE );
  frmCamera->setEnabled( FALSE );
  lblFileName->setEnabled( FALSE );
  spbxEventNumber->setEnabled( FALSE );
  lblMaxEvtNumber->setEnabled( FALSE );
  cboxPalette->setEnabled( FALSE );
  spbxPaletteMin->setEnabled( FALSE );
  spbxPaletteMax->setEnabled( FALSE );
  lcdEvtNumber->setEnabled( FALSE );
}

/** enable interface */
void QDisplayView::enableWidgets() {
  rbPaletteMode->setEnabled( TRUE );
  frmCamera->setEnabled( TRUE );
  lblFileName->setEnabled( TRUE );
  spbxEventNumber->setEnabled( TRUE );
  lblMaxEvtNumber->setEnabled( TRUE );
  cboxPalette->setEnabled( TRUE );
  spbxPaletteMin->setEnabled( TRUE );
  spbxPaletteMax->setEnabled( TRUE );
  lcdEvtNumber->setEnabled( TRUE );
}

/** repaint widgets */
void QDisplayView::repaintWidgets() {
  rbPaletteMode->repaint( FALSE );
  frmCamera->repaint( FALSE );
  lblFileName->repaint( FALSE );
  spbxEventNumber->repaint( FALSE );
  lblMaxEvtNumber->repaint( FALSE );
  cboxPalette->repaint( FALSE );
  spbxPaletteMin->repaint( FALSE );
  spbxPaletteMax->repaint( FALSE );
  lcdEvtNumber->repaint( FALSE );
//  paletteSpectrum->repaint( FALSE );
}

/** shows palette */
void QDisplayView::showPalette()
{
  paletteSpectrum->show();
  showPaletteColors();
}

/** hides palette */
void QDisplayView::hidePalette()
{
  paletteSpectrum->hide();
  showPaletteColors();
}

/** returns whether palette is visible or not */
bool QDisplayView::isPaletteVisible() const
{
  return paletteSpectrum->isVisible();
}

/** returns whether we display Cherenkov signal or not */
bool QDisplayView::isDisplaySignal() const
{
  return displaySignal;
}

/** returns whether we display Cherenkov pulse arrival times or not */
bool QDisplayView::isDisplayTimes() const
{
  return displayTimes;
}

/** set display Cherenkov signal or not */
void QDisplayView::setDisplaySignal(bool st)
{
  displaySignal = st;
}

/** set display Cherenkov pulse arrival times or not */
void QDisplayView::setDisplayTimes(bool st)
{
  displayTimes = st;
}

/** show palette in camera frame*/
void QDisplayView::showPaletteColors()
{
  static int x;

  QPainter p;
  p.begin( paletteSpectrumBuffer );   // start painting widget
  p.setWindow( paletteSpectrum->rect() );   // define coord.

  int len = int(float(paletteSpectrumBuffer->width()) / nColors);

  cout << "showing Palette Colors ..." << endl << flush;

  p.setPen( paletteSpectrum->backgroundColor() );
  p.setBrush( paletteSpectrum->backgroundColor() );
  p.drawRect( paletteSpectrum->rect() );

  register int i;
  for (i=0, x=0; i<nColors; i++, x+=len) {
    p.setPen( palette[i] );
    p.setBrush( palette[i] );
    p.drawRect( x, 0, len, paletteSpectrumBuffer->height() );
    //x += len;//  , x<paletteSpectrumBuffer->width()
  }

  update();

  p.end();

}

/** set lower value for palette colors in image */
void QDisplayView::slotSetMinDataScale(int x)
{
  if (x >= maxDataScale) {
    spbxPaletteMin->setValue( x-1 );
    return;
  }

  minDataScale = float(x);
  lminDataScale = log(minDataScale + 1);
  
  setColors();
  displayEvent();
}

/** set upper value for palette colors in image */
void QDisplayView::slotSetMaxDataScale(int x)
{
  maxDataScale = float(x);
  lmaxDataScale = log(maxDataScale + 1);

  setColors();
  displayEvent();
}

/** read colors paletter from a give text file */
void QDisplayView::readPalette( const char * filename )
{
  int r=-1, g, b;

  // qApp->processEvents();

  QString sfilename(filename);

  QFile fpal( sfilename );
  if ( ! fpal.exists() ) {
    return;
  }

  fpal.open( IO_ReadOnly );
  QTextStream t(&fpal);

  nColors = 0;

  QString line;

  /** We expect a ppm file, and therefore 4 lines before the RGB
    *	values start: 1) P3, 2) Header comment, 3) 0 64, 4) 255
    */

  line = t.readLine();
  line = t.readLine();

  cout << "Reading: " << line << endl;

  line = t.readLine();
  line = t.readLine();

  while ( ! t.eof() ) {
    line = t.readLine();
    sscanf( line, "%d %d %d", &r, &g, &b);
    if (r < 0) continue;  // this avoids problems with comments
    
    if ( nColors < numberOfColors ) {
      palette[nColors].setRgb( r, g, b );
    } else {
      break;
    }
    
    r = -1;
    nColors++;
  }
  
  fpal.close();

  showPaletteColors();

}

/** add palette name into list */
void QDisplayView::addPalette( const char* fname )
{
  QString fileName( fname );
  QFileInfo fpalInfo( fileName );
  QString basename( fpalInfo.baseName() );
  QString absname( fpalInfo.absFilePath() );

  int index;
  index = paletteList->count();

  cout << "Inserting paletteDict[ " << basename << ':' << index << " ] = "
       << absname << endl << flush;

  paletteList->insert( index, absname );
  cboxPalette->insertItem( basename );
  cboxPalette->setCurrentItem( index );

  cout << "There are " << paletteList->count() << " palettes"
       << endl << flush;
  /*
  cout << "Checking paletteDict[ "
       << basename << ':' << index << " ] ==? "
       << paletteList->at( index )
       << endl << flush;
  */
  readPalette( fname );
}

/** gets new palette from palette list */
void QDisplayView::slotSelectedPalette(int index)
{
  //cout << "[" << sizeof( index ) << ":" << index << "]" << endl << flush;

  cout << "Reading palette '" << index
       << "' from file '" << paletteList->at( index ) << "'"
       << endl << flush;

  readPalette( paletteList->at( index ) );
  displayEvent();
}

/** gets new palette from palette file */
void QDisplayView::slotOpenPalette()
{
  //myParent->statusBar()->message("Opening palette file...");

  QString fileName = QFileDialog::getOpenFileName(0,0,this);

  if (!fileName.isEmpty()) {

    addPalette( fileName );

    //QString message="Loaded palette from: "+fileName;
    //myParent->statusBar()->message(message, 2000);

  }
  // else {
  //   myParent->statusBar()->message("Opening palette aborted", 2000);
  // }

  displayEvent();
}

/** gets new palette from palette file */
void QDisplayView::slotSetModePalette(int mode)
{
  modePalette = mode;

  setColors();
  displayEvent();
}

/** set the colors of the event */
void QDisplayView::setColors()
{
  register int i;
  static float m;

  //qApp->processEvents();

  switch ( modePalette ) {

  case modePaletteLinear: // linear

    m = nColors / (maxDataScale - minDataScale);

  	if (isDisplaySignal()) {
      for( i=0; i<numberOfPixelsInCamera; ++i )
        colors[i] = int( m * (data[i] - minDataScale)
                         / (pixsize[i]*pixsize[i]) - 0.5 );
    } else {
      for( i=0; i<numberOfPixelsInCamera; ++i )
        colors[i] = int( m * (data[i] - minDataScale) - 0.5 );
    }

	 	for (i=0; i<numberOfColors; i++ )
	    histX[i] = (maxDataScale - minDataScale) * i / numberOfColors + minDataScale;

    break;

  case modePaletteLogarithmic: // logarithmic

    m = nColors / (lmaxDataScale - lminDataScale);

    for( i=0; i<numberOfPixelsInCamera; ++i )
      colors[i] = int( m * (log(data[i]+1) - lminDataScale) - 0.5 );

	 	for (i=0; i<numberOfColors; i++ )
	    histX[i] = (lmaxDataScale - lminDataScale) * i / numberOfColors + lminDataScale;

    break;

  case modePaletteAuto: // auto

    m = nColors / (maxData - minData);

  	if (isDisplaySignal()) {
      for( i=0; i<numberOfPixelsInCamera; ++i )
        colors[i] = int( m * (data[i] - minData)
                         / (pixsize[i]*pixsize[i]) - 0.5);
    } else {
      for( i=0; i<numberOfPixelsInCamera; ++i )
        colors[i] = int( m * (data[i] - minData) - 0.5 );
    }

	 	for (i=0; i<numberOfColors; i++ )
	    histX[i] = (maxData - minData) * i / numberOfColors + minData;

  }

  for( i=0; i<numberOfPixelsInCamera; ++i ) {
    if ( colors[i] > nColors) colors[i] = nColors-1;
    if ( colors[i] < 0) colors[i] = 0;
    //cout << data[i] << " - " << colors[i] << endl;
  }

  cout << '[' << minData << ',' << maxData << ']' << endl << flush;

 	for (i=0; i<numberOfColors; i++ )
 		histY[i] = 0.0;
	 	
 	for (i=0; i<numberOfPixelsInCamera; i++ )
 	  if (colors[i]>0)
      histY[colors[i]] += 1.0;

  histSpectrum->setCurveData(histCurve, histX, histY, 64);

  histSpectrum->replot();
}

/** initializes values when a document has been changed (opened or closed) */
void QDisplayView::slotDocumentChanged()
{
  dataFile     = thisDoc->ptr_dataFile();
  nEvents      = thisDoc->val_nEvents();

  textFile->setDevice( dataFile );

  if ( (dataFile != 0) && (dataFile->isOpen()) ) {

    cout << "Number of events in file " << dataFile->name()
         << ": " << nEvents
         << endl << flush;

    QString fileName = dataFile->name();
    lblFileName->setText( " File: "+fileName );
    spbxEventNumber->setRange( 1, nEvents );
    QString s;
    s.sprintf(" of  %d", nEvents );
    lblMaxEvtNumber->setText( s );
    enableWidgets();
    
  } else {

    cout << "File has been closed" << endl << flush;
    lblFileName->setText( " File: <none>" );
    spbxEventNumber->setRange( 0, 0 );
    lblMaxEvtNumber->setText( " of  #####" );
    clearView();
    disableWidgets();
    
  }

  repaint();
}

/** a new event is requested */
void QDisplayView::slotReadEvent(int ev_)
{
  currentEvent = ev_ - 1;
  readEvent(currentEvent);
}

/** reads an event header from data file */
void QDisplayView::readASCIIEventHeader()
{
  register int i;
  //cout << "Event:";
  for (i=0; i<evthdr->size(); i++ ) {
    *textFile >> (*evthdr)[i];
    printf("%d : %s = %f\n", i, cEvtHdr_Variables[i], (*evthdr)[i]);
  }
  dataFile->readLine( wholeLine, 20000 );
}

/** reads an event image from data file */
void QDisplayView::readASCIIEventImage()
{
  register int i;
  static float dummy;

  *textFile >> dummy;

  maxData = -1.0;
  minData =  1.0e8;

  for (i=0; i<numberOfPixelsInCamera; i++ ) {
    *textFile >> data[i];
    if (data[i] > maxData) maxData = data[i];
    if (data[i] > 0.0) {
      if (data[i] < minData) minData = data[i];
    }
    //cout << data[i] << ' ';
  }
  //cout << endl;
  dataFile->readLine( wholeLine, 20000 );
}

/** reads additional data for an event from data file */
void QDisplayView::readASCIIEventAdditionalData()
{
  dataFile->readLine( wholeLine, 20000 );
}

/** reads an event from data file */
int QDisplayView::readEvent(int ev)
{
  if (ev == -1) { return false; }

  cout << "Reading Event number " << ev+1
       << " at pos " << (*evt)[ev] << "..." << endl << flush;

  /** puts the head at right position to read the data */
  dataFile->at( (*evt)[ev] );

  /** reads the data */
  readASCIIEventHeader();
  if ( isDisplaySignal() ) {
    readASCIIEventImage();
    readASCIIEventAdditionalData();
  } else {
    readASCIIEventAdditionalData();
    readASCIIEventImage();
  }

  /** converts the data to color indexes */
  setColors();
  
  /** display this event */
  displayEvent();

  return true;
}

/** display event in camera view */
int QDisplayView::displayEvent()
{
  register int i;         // index of the loop

  QPainter p;
  p.begin( cameraViewBuffer );   // start painting widget
  p.setPen( QPen( palette[ 0 ], 1 )  ); // set pen color to lowest value
  p.setWindow( 0, 0, maxX, maxY );

  QString s;

  // QMyTips::clear();

  // QToolTip::add( frmCamera, "camera frame" );

  for (i=0; i<numberOfPixelsInCamera; i++) {
    p.setBrush( palette[ colors[i] ] );

    //p.drawEllipse( pixelRect[i] );

    p.drawPolygon( pixelHex[i] );

    // s.sprintf("%d", data[i]);
    // QToolTip::remove( frmCamera, QRect( px, py, izoom, izoom ));
    // QToolTip::add( frmCamera, QRect( px, py, izoom, izoom ), s);
    // cout << '(' << px << ',' << py << ')';
  }

  if ( data[EvtHdr_ncphs] > 0. )
  	btnTrigger->setEnabled(true);
  else
  	btnTrigger->setEnabled(false);

  update();

  p.end();                // painting done

  return true;
}

/** paint event dumps buffer in camera frame */
void QDisplayView::paintEvent( QPaintEvent * )
{
  blockSignals( TRUE );
  bitBlt( frmCamera, 0, 0, cameraViewBuffer );
  bitBlt( paletteSpectrum, 0, 0, paletteSpectrumBuffer );
  blockSignals( FALSE );

  repaintWidgets();
}

/** print event in printer */
void QDisplayView::printInColor(bool value)
{
  prnColor = value;
}

/** print event in printer */
bool QDisplayView::isPrintInColor() const
{
  return prnColor;
}

/** print event in printer */
void QDisplayView::printSetOptions( bool pInColor, bool pImage,
                                    bool pStat, bool pHist,
                                    const char *pCommand,
                                    const char *pFilePrefix,
                                    const char *pFileName,
                                    int pOutputTo )
{
  prnColor = pInColor;
  prnImage = pImage;
  prnStat = pStat;
  prnHist = pHist;
  strcpy(prnCommand, pCommand);
  strcpy(prnFilePrefix, pFilePrefix);
  strcpy(prnFileName, pFileName);
  prnOutputTo = pOutputTo;
  if ((prnOutputTo != ePrnOneFile) && (prnPainter != 0)) {
    prnPainter->end();
    delete prnPainter;
    prnPainter = 0;
  }
}

/** print event in printer */
void QDisplayView::print()
{

  static int prnFile = 0;
  static bool prnOneFile = false;
  static bool prnOneFile_open = false;

  const int MARGIN = 40;
  // int pageNo = 1;
  int yPos = 0;   // y position for each line
  int xPos = 0;   // x position for image
  int px, py;
  int wx, wy;
  int i;
  int opt;
  QString s;

  // if we were printing to one single file, but
  // now we don't, close the painter
  if ( prnOneFile_open && (! prnOutputTo) ) {
    prnPainter->end();
    delete prnPainter;
    prnPainter = 0;
  }

  switch ( prnOutputTo ) {

  case ePrnSetup:

    prnFile = 0;
    prnOneFile = false;
    prnOneFile_open = false;
    if ( ! printer->setup(this) ) {
      return;
    }

    break;

  case ePrnDirect:

    prnFile = 0;
    prnOneFile = false;
    prnOneFile_open = false;
    printer->setOutputToFile( FALSE );

    break;

  case ePrnFiles:

    prnOneFile = false;
    prnOneFile_open = false;
    prnFile++;
    printer->setOutputToFile( TRUE );
    printer->setOutputFileName( s.sprintf("%s%d.ps",
                                          prnFilePrefix, prnFile ) );
    break;

  case ePrnOneFile:

    prnFile = 0;
    prnOneFile = true;
    if ( ! prnOneFile_open ) {
      printer->setOutputToFile( TRUE );
      printer->setOutputFileName( prnFileName );
      prnOneFile_open = true;
      prnPainter = new QPainter( printer );
    } else {
      printer->newPage();
    }

    break;

  }

  if ( ! prnOneFile ) {
    prnPainter = new QPainter( printer );
  }

  QPainter &p = (*prnPainter);

  p.setWindow(0, 0, 600, 600*1.4142);
  p.setPen( black );        // set pen to black

  // need width/height of printer surface
  QPaintDeviceMetrics metrics( printer );

  //-- BANNER --

  p.setBrush( black );
  p.drawRect ( MARGIN, MARGIN +  8, metrics.width() - 2*MARGIN, 4 ) ;
  p.drawRect ( MARGIN, MARGIN + 74, metrics.width() - 2*MARGIN, 2 ) ;

  p.setBrush( NoBrush );

  p.setFont( QFont( "Times", 24 ) );
  QFontMetrics fm = p.fontMetrics();

  p.drawText( MARGIN, MARGIN + 20, metrics.width(), fm.lineSpacing(),
              ExpandTabs | DontClip, "QDisplay" );

  p.setFont( QFont( "Times", 10) );
  fm = p.fontMetrics();

  p.drawText( MARGIN + 100, MARGIN + 20, metrics.width(), fm.lineSpacing(),
              ExpandTabs | DontClip, VERSION);

  p.setFont( QFont( "Times", 16) );
  fm = p.fontMetrics();

  p.drawText( MARGIN, MARGIN + 50, metrics.width(), fm.lineSpacing(),
              ExpandTabs | DontClip, "Event Display for Monte Carlo data");

  //-- EVENT HEADER --

  p.setFont( QFont( "helvetica", 10 ) );
  fm = p.fontMetrics();

  yPos = 100;
  px = MARGIN;
  py = MARGIN + yPos;
  wx = metrics.width();
  wy = fm.lineSpacing();
  opt = ExpandTabs | DontClip;

  p.drawText(px, py, wx, wy, opt, "File:");             py+=wy;
  p.drawText(px, py, wx, wy, opt, "Event:");            py+=2*wy;

  p.drawText(px, py, wx, wy, opt, "Primary:");          py+=wy;
  p.drawText(px, py, wx, wy, opt, "Energy:");           py+=wy;
  p.drawText(px, py, wx, wy, opt, "Impact Par.(m):");   py+=wy;
  p.drawText(px, py, wx, wy, opt, "Core Pos.(m):");     py+=wy;
  p.drawText(px, py, wx, wy, opt, "Direction (deg):");  py+=wy;
  p.drawText(px, py, wx, wy, opt, "N.Ch.phot.:");       py+=2*wy;

  p.drawText(px, py, wx, wy, opt, "SIZE (ph.e-):");     py+=wy;
  p.drawText(px, py, wx, wy, opt, "SIZE core(ph.e-):"); py+=wy;
  p.drawText(px, py, wx, wy, opt, "ALPHA (deg):");      py+=wy;
  p.drawText(px, py, wx, wy, opt, "LENGTH (deg)");      py+=wy;
  p.drawText(px, py, wx, wy, opt, "WIDTH (deg):");      py+=wy;
  p.drawText(px, py, wx, wy, opt, "DIST (deg):");       py+=wy;
  p.drawText(px, py, wx, wy, opt, "ASYMMETRY:");        py+=wy;
  p.drawText(px, py, wx, wy, opt, "CONC:");             py+=2*wy;

  p.drawText(px, py, wx, wy, opt, "Maximum (deg):");    py+=wy;
  p.drawText(px, py, wx, wy, opt, "Centroid (deg):");   py+=wy;

  p.setFont( QFont( "courier", 10, 0, TRUE) );

  px += 85;
  yPos +=2; // fine-tunning
  py = MARGIN + yPos;
  EvtHdr &e = *evthdr;

  p.drawText(px, py, wx, wy, opt, dataFile->name());
  py+=wy;
  p.drawText(px, py, wx, wy, opt, spbxEventNumber->text());
  py+=2*wy;

  p.drawText(px, py, wx, wy, opt,
    QString((e[EvtHdr_primary] == 1)? "Gammas (" : "Protons (") +
    QString((e[EvtHdr_trigger] == 1)? "TRIGGER)" : ")"));
  py+=wy;
  p.drawText(px, py, wx, wy, opt, s.setNum(e[EvtHdr_energy],'f',1));
  py+=wy;
  p.drawText(px, py, wx, wy, opt, s.setNum(e[EvtHdr_impact]/100.,'f',1));
  py+=wy;
  p.drawText(px, py, wx, wy, opt,
             s.sprintf("(%.1f,%.1f)", e[EvtHdr_xcore],e[EvtHdr_ycore]));
  py+=wy;
  p.drawText(px, py, wx, wy, opt,
             s.sprintf("(%.1f,%.1f)",
                       e[EvtHdr_theta]*180./M_PI,
                       e[EvtHdr_phi]*180./M_PI));
  py+=wy;
  p.drawText(px, py, wx, wy, opt, s.setNum(int(e[EvtHdr_ncphs])));
  py+=2*wy;

  p.drawText(px, py, wx, wy, opt, s.setNum(int(e[EvtHdr_nphes])));
  py+=wy;
  p.drawText(px, py, wx, wy, opt, s.setNum(int(e[EvtHdr_nphes2])));
  py+=wy;
  p.drawText(px, py, wx, wy, opt, s.setNum(e[EvtHdr_alpha],'f',1));
  py+=wy;
  p.drawText(px, py, wx, wy, opt, s.setNum(e[EvtHdr_length],'f',1));
  py+=wy;
  p.drawText(px, py, wx, wy, opt, s.setNum(e[EvtHdr_width],'f',1));
  py+=wy;
  p.drawText(px, py, wx, wy, opt, s.setNum(e[EvtHdr_dist],'f',1));
  py+=wy;
  p.drawText(px, py, wx, wy, opt, s.setNum(e[EvtHdr_asymx],'f',1));
  py+=wy;
  p.drawText(px, py, wx, wy, opt, s.setNum(e[EvtHdr_conc6],'f',1));
  py+=2*wy;

  p.drawText(px, py, wx, wy, opt,
             s.sprintf("(%.1f,%.1f)",e[EvtHdr_xmax],e[EvtHdr_ymax]));
  py+=wy;
  p.drawText(px, py, wx, wy, opt,
             s.sprintf("(%.1f,%.1f)",e[EvtHdr_xm],e[EvtHdr_ym]));
  py+=wy;


  //-- SIGNATURE --

  p.setFont( QFont( "times", 10 ) );
  fm = p.fontMetrics();

  p.drawText( MARGIN, 600*1.4142 - 4*fm.lineSpacing(),
              metrics.width(), fm.lineSpacing(),
              ExpandTabs | DontClip, IDS_APP_ABOUT );

  QDateTime dt = QDateTime::currentDateTime();

  p.drawText( MARGIN + 400, 600*1.4142 - 2*fm.lineSpacing(),
              metrics.width(), fm.lineSpacing(),
              ExpandTabs | DontClip, dt.toString() );

  //-- GRAPHIC INFO --

  // now we plot the image

  int oldPalette = cboxPalette->currentItem();
  if ( ! prnColor ) {
    // change the palette to WhiteToBlack, to save toner ;-)
    slotSelectedPalette( 0 );  // White2Black
  }

  // set window metrics, and pen color
  int metricX = int(maxX*2);
  int metricY = int(maxY*2*1.4142);

  xPos = int(metricX * 0.61);
  yPos = int(metricY * 0.35);

  p.setWindow(0, 0, metricX, metricY);

  // draw border of camera

  int nRings = 17;
  //nRings = int( ( sqrt( 9 + 12 * (numberOfPixelsInCamera-1) )
  //                - 3 ) / 6 + 0.99999);
  int cameraHalfWidth = (nRings+1)*izoom;
  p.drawEllipse( xPos-cameraHalfWidth,
                 yPos-cameraHalfWidth,
                 2*cameraHalfWidth,
                 2*cameraHalfWidth );

  // draw camera

  // set pen to lowest color
  p.setPen( QPen(black, 1) );

  float ftempsize;
  float fctempsize, fstempsize;
  float fpx, fpy;

  QPointArray hexagon(6);

  for (i=0; i<numberOfPixelsInCamera; i++) {
    fpx = xPos + pix[i][0]*zoom;
    fpy = yPos - pix[i][1]*zoom;
    ftempsize = zoom*pixsize[i]*0.56; // for overlapping (trick  :-)
    fctempsize = ftempsize*COS30;
    fstempsize = ftempsize*SIN30;
    hexagon.putPoints( 0, 6,
                       irint(fpx+fctempsize), irint(fpy-fstempsize),
                       irint(fpx),            irint(fpy-ftempsize),
                       irint(fpx-fctempsize), irint(fpy-fstempsize),
                       irint(fpx-fctempsize), irint(fpy+fstempsize),
                       irint(fpx),            irint(fpy+ftempsize),
                       irint(fpx+fctempsize), irint(fpy+fstempsize)
                      );
    p.setBrush( palette[ colors[i] ] );
    p.drawPolygon( hexagon );
  }

  // draw scale

  // initialize position of camera
  px = int(metricX * 0.90);
  py = yPos - cameraHalfWidth;
  int yInterval = 2*cameraHalfWidth / nColors;

  // set font
  p.setFont( QFont( "helvetica", 20 ) );
  fm = p.fontMetrics();

  float m;
  int val;
  
  for (i=0; i<nColors; i++) {
      
    // put color of scale
    p.setPen( palette[i] );
    p.setBrush( palette[i] );
    p.drawRect( px, py, (metricX - px)/3, yInterval);

    // put label near the scale each 4 colors
    if ( ((i % 4) == 0) || (i==nColors-1) ) {
      
      p.setPen( black );

      switch ( modePalette ) {
      case modePaletteLinear: // linear
        m = nColors / (maxDataScale - minDataScale);
        val = int((i+0.5)/m + minDataScale);
        break;
      case modePaletteLogarithmic: // logarithmic
        m = nColors / (lmaxDataScale - lminDataScale);
        val = int(exp((i+0.5)/m + lminDataScale)-1);
        break;
      case modePaletteAuto: // auto
        m = nColors / (maxData - minData);
        val = int((i+0.5)/m + minData);
      }

      s.sprintf("%d", val);

      p.drawText( px - (metricX - px)/3, py + yInterval/2,
                  metrics.width(), fm.lineSpacing(),
                  ExpandTabs | DontClip, s );
    }

    // increment 
    py += yInterval;
      
  }

  // clear brush and restore palette
  p.setBrush( NoBrush );

  if (! prnColor) {
    slotSelectedPalette( oldPalette );
  }

  // close painter, and we're done...
  if ( ! prnOneFile ) {
    prnPainter->end();
    delete prnPainter;
    prnPainter = 0;
  }
}

/* EOF */
