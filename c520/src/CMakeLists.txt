# CMake project file for C520

cmake_minimum_required (VERSION 2.6)
project (C520)
enable_language (Fortran)

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

# default installation
get_filename_component (default_prefix ".." ABSOLUTE)
set (CMAKE_INSTALL_PREFIX ${default_prefix} CACHE STRING
      "Choose the installation directory; by default it installs in the C520 directory."
      FORCE)

# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "g77")
  # g77
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O0 -g -DJC_UNDERSCORES")

# build executables
set (EXECUTABLES c520)

add_executable (c520 
addang.f
addani.f
age.f
amoeba.f
amotry.f
annih.f
ausgab.f
ausgb2.f
avage.f
avept.f
bhabha.f
block1.f
box2.f
box3.f
box60.f
box61.f
box62.f
box63.f
box65.f
box66.f
box67.f
box68.f
box69.f
box70.f
box71.f
box72.f
box73.f
box74.f
brems.f
cerenkov.f
cghei.f
cghini.f
cghsig.f
chisq.f
compt.f
coorin.f
datac.f
decay1.f
decay6.f
difrac.f
dpfunc.f
egs4bd.f
egs4.f
egsini.f
electr.f
em.f
etadec.f
flpsor.f
fstack.f
gam.f
garndm.f
gbrsgm.f
GHEISHA.f
gprsgm.f
granor.f
grndm.f
hatch.f
hdpm.f
heigh.f
hmeson.f
howfar.f
ininkg.f
inprm.f
isobar.f
istack.f
jadach.f
jcdate.c
jchisto.c
jcio.c
kdecay.f
leaddf.f
ledeny.f
lepacx.f
longft.f
lowup.f
lpm.f
main.f
mitage.f
mmol4.f
mmolie.f
moller.f
mpoiss.f
mpprop.f
mscat.f
mubrem.f
mucoul.f
mudecy.f
mupair.f
muprpr.f
mutrac.f
nihila.f
nkg.f
nsd.f
nucint.f
outend.f
output.f
pair.f
pamaf.f
parnum.f
parrap.f
pcl.f
photo.f
photon.f
pi0dec.f
pigen1.f
pigen2.f
pigen.f
pparam.f
prange.f
prtime.f
ptram.f
ptran.f
ptrans.f
QGSJET.f
rangen.f
rannor.f
resdec.f
rhof.f
rmmaq.f
rmmar.f
rnegbi.f
sdpm.f
selcor.f
shower.f
SIBYLL.f
single.f
sobseq.f
staend.f
stankg.f
start.f
strdec.f
tarint.f
thick.f
tobuf.f
tstack.f
tstend.f
tstini.f
tstout.f
ucopy.f
update.f
uphi.f
utqsea.f
utqval.f
vapor.f
vendat.f
venini.f
venlnk.f
VENUS.f
vhmeso.f
vstore.f
vzero.f
)

# install executables and scripts
install (TARGETS ${EXECUTABLES}
         RUNTIME DESTINATION "bin")
install (PROGRAMS ${SCRIPTS}
         DESTINATION "bin")
