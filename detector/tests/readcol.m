## -*- octave -*-
function y=readcol(file, col)

  f1=fopen(file,"r");
  y=fscanf(f1,"%f",[col,Inf])';
  fclose(f1);

return;
  