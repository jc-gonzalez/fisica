BEGIN {
  print "# Color palette for ctv 0.1";
  print "# Copyright (c) 1998, J C Gonzalez";
  print "#";
  print "# 32 colors, r g b values";
  print "#";
}
	
/[0-9]/ {
  printf("%d %d %d\n",$1*255,$2*255,$3*255);
}
