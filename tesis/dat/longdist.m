## -*- octave -*-

function y=longdist(t,p)
y = p(1).*((t-p(2))/(p(3)-p(2))).^((p(3)-t)/(p(4)+p(5).*t+p(6).*t.^2));
endfunction
