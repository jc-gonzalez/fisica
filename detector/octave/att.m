## -*- octave -*-

f1=fopen("attenu.0");
data=fscanf(f1,"%f",[Inf,3])';
fclose(f1);
