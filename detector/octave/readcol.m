function d=readcol(file, ncols)
f=fopen(file,"r");
d=fscanf(f,"%f",[ncols,Inf]);
fclose(f);
endfunction
