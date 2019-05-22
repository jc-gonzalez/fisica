/****************************************************************************
** Form implementation generated from reading ui file 'dlgaxispoint.ui'
**
** Created: Wed Aug 8 23:50:17 2001
**      by:  The User Interface Compiler (uic)
**
** WARNING! All changes made in this file will be lost!
****************************************************************************/
#include "dlgaxispoint.h"

#include <qgroupbox.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qpushbutton.h>
#include <qlayout.h>
#include <qvariant.h>
#include <qtooltip.h>
#include <qwhatsthis.h>

/* 
 *  Constructs a dlgAxisPoint which is a child of 'parent', with the 
 *  name 'name' and widget flags set to 'f' 
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  TRUE to construct a modal dialog.
 */
dlgAxisPoint::dlgAxisPoint( QWidget* parent,  const char* name, bool modal, WFlags fl )
    : QDialog( parent, name, modal, fl )
{
    if ( !name )
	setName( "dlgAxisPoint" );
    resize( 299, 272 ); 
    setSizePolicy( QSizePolicy( (QSizePolicy::SizeType)5, (QSizePolicy::SizeType)5, sizePolicy().hasHeightForWidth() ) );
    setCaption( tr( "Edit Axis Point" ) );

    QWidget* privateLayoutWidget = new QWidget( this, "Layout7" );
    privateLayoutWidget->setGeometry( QRect( 8, 2, 286, 264 ) ); 
    Layout7 = new QVBoxLayout( privateLayoutWidget ); 
    Layout7->setSpacing( 6 );
    Layout7->setMargin( 0 );

    Layout4 = new QVBoxLayout; 
    Layout4->setSpacing( 6 );
    Layout4->setMargin( 0 );

    axesName = new QLabel( privateLayoutWidget, "axesName" );
    axesName->setText( tr( "Axes name" ) );
    QFont axesName_font(  axesName->font() );
    axesName_font.setBold( TRUE );
    axesName->setFont( axesName_font ); 
    Layout4->addWidget( axesName );

    axesDesc = new QLabel( privateLayoutWidget, "axesDesc" );
    axesDesc->setText( tr( "Axes system description" ) );
    Layout4->addWidget( axesDesc );
    QSpacerItem* spacer = new QSpacerItem( 20, 20, QSizePolicy::Minimum, QSizePolicy::Expanding );
    Layout4->addItem( spacer );

    GroupBox1 = new QGroupBox( privateLayoutWidget, "GroupBox1" );
    GroupBox1->setTitle( tr( "Coordinates over image" ) );

    QWidget* privateLayoutWidget_2 = new QWidget( GroupBox1, "Layout1" );
    privateLayoutWidget_2->setGeometry( QRect( 18, 18, 246, 24 ) ); 
    Layout1 = new QHBoxLayout( privateLayoutWidget_2 ); 
    Layout1->setSpacing( 6 );
    Layout1->setMargin( 0 );

    TextLabel3 = new QLabel( privateLayoutWidget_2, "TextLabel3" );
    TextLabel3->setText( tr( "GX:" ) );
    Layout1->addWidget( TextLabel3 );

    txtGX = new QLineEdit( privateLayoutWidget_2, "txtGX" );
    Layout1->addWidget( txtGX );

    TextLabel3_2 = new QLabel( privateLayoutWidget_2, "TextLabel3_2" );
    TextLabel3_2->setText( tr( "GY:" ) );
    Layout1->addWidget( TextLabel3_2 );

    txtGY = new QLineEdit( privateLayoutWidget_2, "txtGY" );
    Layout1->addWidget( txtGY );
    Layout4->addWidget( GroupBox1 );
    QSpacerItem* spacer_2 = new QSpacerItem( 20, 20, QSizePolicy::Minimum, QSizePolicy::Expanding );
    Layout4->addItem( spacer_2 );

    GroupBox1_2 = new QGroupBox( privateLayoutWidget, "GroupBox1_2" );
    GroupBox1_2->setTitle( tr( "Coordinates over plot" ) );

    QWidget* privateLayoutWidget_3 = new QWidget( GroupBox1_2, "Layout1_2" );
    privateLayoutWidget_3->setGeometry( QRect( 18, 18, 246, 24 ) ); 
    Layout1_2 = new QHBoxLayout( privateLayoutWidget_3 ); 
    Layout1_2->setSpacing( 6 );
    Layout1_2->setMargin( 0 );

    TextLabel3_3 = new QLabel( privateLayoutWidget_3, "TextLabel3_3" );
    TextLabel3_3->setText( tr( "  X:" ) );
    Layout1_2->addWidget( TextLabel3_3 );

    txtX = new QLineEdit( privateLayoutWidget_3, "txtX" );
    Layout1_2->addWidget( txtX );

    TextLabel3_2_2 = new QLabel( privateLayoutWidget_3, "TextLabel3_2_2" );
    TextLabel3_2_2->setText( tr( "  Y:" ) );
    Layout1_2->addWidget( TextLabel3_2_2 );

    txtY = new QLineEdit( privateLayoutWidget_3, "txtY" );
    Layout1_2->addWidget( txtY );
    Layout4->addWidget( GroupBox1_2 );
    Layout7->addLayout( Layout4 );
    QSpacerItem* spacer_3 = new QSpacerItem( 20, 20, QSizePolicy::Minimum, QSizePolicy::Expanding );
    Layout7->addItem( spacer_3 );

    Layout5 = new QHBoxLayout; 
    Layout5->setSpacing( 6 );
    Layout5->setMargin( 0 );
    QSpacerItem* spacer_4 = new QSpacerItem( 20, 20, QSizePolicy::Expanding, QSizePolicy::Minimum );
    Layout5->addItem( spacer_4 );

    PushButton1 = new QPushButton( privateLayoutWidget, "PushButton1" );
    PushButton1->setText( tr( "&OK" ) );
    Layout5->addWidget( PushButton1 );

    PushButton1_2 = new QPushButton( privateLayoutWidget, "PushButton1_2" );
    PushButton1_2->setText( tr( "&Cancel" ) );
    Layout5->addWidget( PushButton1_2 );
    Layout7->addLayout( Layout5 );

    // signals and slots connections
    connect( PushButton1, SIGNAL( clicked() ), this, SLOT( accept() ) );
    connect( PushButton1, SIGNAL( clicked() ), this, SLOT( reject() ) );
    connect( PushButton1_2, SIGNAL( clicked() ), this, SLOT( reject() ) );

    // tab order
    setTabOrder( txtGX, txtGY );
    setTabOrder( txtGY, txtX );
    setTabOrder( txtX, txtY );
    setTabOrder( txtY, PushButton1 );
    setTabOrder( PushButton1, PushButton1_2 );
}

/*  
 *  Destroys the object and frees any allocated resources
 */
dlgAxisPoint::~dlgAxisPoint()
{
    // no need to delete child widgets, Qt does it all for us
}

/*  
 *  Main event handler. Reimplemented to handle application
 *  font changes
 */
bool dlgAxisPoint::event( QEvent* ev )
{
    bool ret = QDialog::event( ev ); 
    if ( ev->type() == QEvent::ApplicationFontChange ) {
	QFont axesName_font(  axesName->font() );
	axesName_font.setBold( TRUE );
	axesName->setFont( axesName_font ); 
    }
    return ret;
}

