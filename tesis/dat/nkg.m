function y=nkg(s,r)
c=gamma(4.5-s)./(2*pi.*gamma(s).*gamma(4.5-2.*s));
f=(r.^(s-2)).*((1+r).^(s-4.5)).*c;
y=f;
endfunction
