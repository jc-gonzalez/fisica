## -*- octave -*-

load -ascii logNe.dat
load -ascii sconstant.dat

for i=1:11;
  filename = sprintf("lne%d.dat", i);
  f=fopen(filename, "w");
  for j=1:40;
    fprintf(f, "%g %g\n", t(j), logne(i,j));
  end
  fclose(f);
end

for i=1:15;
  filename = sprintf("scons%d.dat", i);
  f=fopen(filename, "w");
  for j=1:11;
    fprintf(f, "%g %g\n", tsconst(i,j), sconst(i,j));
  end
  fclose(f);
end


