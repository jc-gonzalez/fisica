\input rboxes

numeric hsep, vsep;
hsep=50pt;
vsep=40pt;

beginfig(1)

%\vbox{\hbox{}\hbox{}}

boxit.aa(btex INTERGALACTIC~~GAS etex);
boxit.ab(btex Infall etex);
boxit.ac(btex \vbox{\hbox{Galactic Wind}\hbox{or "Sweeping"}} etex);
boxit.ad(btex GAS IN GALAXY etex);
boxit.ae(btex Star formation etex);
boxit.af(btex Mass loss etex);
boxit.ag(btex STARS IN GALAXY etex);
boxit.ah(btex Infall etex);
boxit.ai(btex \vbox{\hbox{DENSE GAS CLOUD}\hbox{IN CENTER OF GALAXY}} etex);
boxit.aj(btex Star formation etex);
boxit.ak(btex \vbox{\hbox{Mass loss and}\hbox{stellar collisions}} etex);
boxit.al(btex \vbox{\hbox{CENTRAL}\hbox{STAR CLUSTER}} etex);
boxit.am(btex Wind etex);
boxit.an(btex Accretion etex);
boxit.ao(btex Contraction etex);
boxit.ap(btex \vbox{\hbox{Stellar}\hbox{coalescence}} etex);
boxit.aq(btex \vbox{\hbox{SUPERMASSIVE}\hbox{OBJECT "SPINAR"}\hbox{OR "MAGNETOID"}} etex);
boxit.ar(btex Supernovae, etc. etex);
boxit.as(btex \vbox{\hbox{CLUSTER OF}\hbox{COMPACT STELLAR}\hbox{REMNANTS}} etex);
boxit.at(btex \vbox{\hbox{"Post-Newtonian"}\hbox{Instability}} etex);
boxit.au(btex \vbox{\hbox{Tidal Disruption}\hbox{of Stars}} etex);
boxit.av(btex MASSIVE~~BLACK~~HOLE etex);

aa.c = (100pt,200pt);

ab.c = aa.c - ( hsep,vsep);
ac.c = aa.c - (-hsep,vsep);

ad.c = aa.c - (0,vsep*2);



drawboxed(aa, ab, ac, ad);

drawarrow .5[aa.sw,aa.s]--ab.n; drawarrow .5[aa.se,aa.s]--ac.n;
drawarrow ab.s--.5[ad.nw,ad.n]; drawarrow ac.s--.5[ad.ne,ad.n];

endfig;

end;