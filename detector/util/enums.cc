#include <iostream>

enum My_Enum_Type {ONE=1, TWO=2, THREE=4, FOUR=8};
My_Enum_Type MyList [] = {ONE, TWO, THREE, FOUR};
const char* const cMyList [] = {"ONE", "TWO", "THREE", "FOUR"};
const int  nMyList = sizeof( cMyList ) / sizeof( char* );

int main()
{
  int n;
  My_Enum_Type i;

  cout << "Number of items: " << nMyList << endl;

  for ( n = 0; n<nMyList; n++ ) {

    i = MyList[ n ];
    cout << n << ':' << i << endl << flush;

  }

  return 0;
}
