function y=pmt_response(x)
n=max(size(x));
y=x;
for i=1:n,
  if ( x(i) < 7 ),
    y(i) = exp(-(x(i)-7)^2/8);
  else,
    y(i) = exp(-(x(i)-7)^2/64);
  end,
end,
return;
  