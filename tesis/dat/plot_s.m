## -*- octave -*-

load -ascii -force logNe.dat
load -ascii -force sconstant.dat

for i=1:11;
  filename = sprintf("lne%d.dat", i);
  f=fopen(filename, "w");
  for j=1:40;
    fprintf(f, "%g %g\n", t(j), lne(j,i));
  end
  fclose(f);
  plot(t,lne(:,i))
end

for i=1:15;
  filename = sprintf("scons%d.dat", i);
  f=fopen(filename, "w");
  for j=1:11;
    fprintf(f, "%g %g\n", tsconst(i,j), sconst(i,j));
  end
  fclose(f);
end


