#include <iostream>
#include <fstream>

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
  
  int inpix = atoi( argv[1] );

  float * fxpix = new float[inpix];
  float * fypix = new float[inpix];

  ifstream iFpixels("pixels-showimage.dat");

  int ndmy;
  for (int i=0; i<inpix; i++)
    iFpixels >> ndmy >> fxpix[i] >> fypix[i];

  iFpixels.close();

  float data, x, y;
  for (int i=0; i<inpix; i++){
    
    cin >> data;

    x = fxpix[i];
    y = fypix[i];

    cout << x+0.5 << ' ' << y-0.5 << ' ' << data << endl;
    cout << x+0.5 << ' ' << y+0.5 << ' ' << data << endl;
    cout << x-0.5 << ' ' << y+0.5 << ' ' << data << endl;
    cout << x-0.5 << ' ' << y-0.5 << ' ' << data << endl;

    cout << x+1.5 << ' ' << y-0.5 << ' ' << data << endl;
    cout << x+1.5 << ' ' << y+0.5 << ' ' << data << endl;

    cout << x-1.5 << ' ' << y-0.5 << ' ' << data << endl;
    cout << x-1.5 << ' ' << y+0.5 << ' ' << data << endl;

    cout << x+0.5 << ' ' << y+1.5 << ' ' << data << endl;
    cout << x-0.5 << ' ' << y+1.5 << ' ' << data << endl;

    cout << x+0.5 << ' ' << y-1.5 << ' ' << data << endl;
    cout << x-0.5 << ' ' << y-1.5 << ' ' << data << endl;

    cout << flush;
  
  }

  delete fxpix, fypix;
  fxpix = 0;
  fypix = 0;
  
  return 0;
  
}
