\input rboxes

% IF boxes

newinternal if_dx, if_dy;
if_dx := 15bp;
if_dy := 13bp;

vardef ifit@#(text tt) =
  beginbox_("ifpath_","sizeif_",@#,tt);
  generic_declare(pair) _n.sw, _n.s, _n.se, _n.e, _n.ne, _n.n, _n.nw, _n.w;
  0 = xpart (@#nw-@#sw) = ypart(@#se-@#sw);
  0 = xpart(@#ne-@#se) = ypart(@#ne-@#nw);
  @#w = .5[@#nw,@#sw];
  @#s = .5[@#sw,@#se];
  @#e = .5[@#ne,@#se];
  @#n = .5[@#ne,@#nw];
  @#ne-@#c = @#c-@#sw = (@#dx,@#dy) + .5*(urcorner pic_@# - llcorner pic_@#);
  endbox_(cleari_,@#);
enddef;

def ifpath_(suffix $) =
  $.w -- $.s -- $.e -- $.n -- cycle
enddef;

def sizeif_(suffix $) =
  if unknown $.dx: $.dx=if_dx; fi
  if unknown $.dy: $.dy=if_dy; fi
enddef;

vardef cleari_(suffix $) =
  _n_ := str $;
  generic_redeclare(numeric) _n.sw, _n.s, _n.se, _n.e, _n.ne, 
    _n.n, _n.nw, _n.w, _n.c, _n.off, _n.dx, _n.dy;
enddef;

