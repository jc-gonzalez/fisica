/****************************************************************************
** Form interface generated from reading ui file 'dlgaxispoint.ui'
**
** Created: Wed Aug 8 23:49:51 2001
**      by:  The User Interface Compiler (uic)
**
** WARNING! All changes made in this file will be lost!
****************************************************************************/
#ifndef DLGAXISPOINT_H
#define DLGAXISPOINT_H

#include <qvariant.h>
#include <qdialog.h>
class QVBoxLayout; 
class QHBoxLayout; 
class QGridLayout; 
class QGroupBox;
class QLabel;
class QLineEdit;
class QPushButton;

class dlgAxisPoint : public QDialog
{ 
    Q_OBJECT

public:
    dlgAxisPoint( QWidget* parent = 0, const char* name = 0, bool modal = FALSE, WFlags fl = 0 );
    ~dlgAxisPoint();

    QLabel* axesName;
    QLabel* axesDesc;
    QGroupBox* GroupBox1;
    QLabel* TextLabel3;
    QLineEdit* txtGX;
    QLabel* TextLabel3_2;
    QLineEdit* txtGY;
    QGroupBox* GroupBox1_2;
    QLabel* TextLabel3_3;
    QLineEdit* txtX;
    QLabel* TextLabel3_2_2;
    QLineEdit* txtY;
    QPushButton* PushButton1;
    QPushButton* PushButton1_2;

protected:
    QVBoxLayout* Layout7;
    QVBoxLayout* Layout4;
    QHBoxLayout* Layout1;
    QHBoxLayout* Layout1_2;
    QHBoxLayout* Layout5;
    bool event( QEvent* );
};

#endif // DLGAXISPOINT_H
