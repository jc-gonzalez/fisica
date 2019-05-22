RUNNR   @RUNNR                              number of run
EVTNR   1                              number of first shower event
NSHOW   @STEP                             number of showers to generate
PRMPAR  @PRIM                              particle type of prim. particle
ESLOPE  0.                            slope of primary energy spectrum
ERANGE  @ENERGY  @ENERGY                     energy range of primary particle
THETAP  0.  0.                        range of zenith angle (degree)
PHIP    0.  0.                       range of azimuth angle (degree)
SEED    @RND11  0 0                       seed for 1. random number sequence
SEED    @RND21  0 0                      seed for 2. random number sequence
SEED    @RND31  0 0                      seed for 3. random number sequence
OBSLEV  2200.E2                         observation level (in cm)
ELMFLG  T   F                          em. interaction flags (NKG,EGS)
RADNKG  200.E2                         outer radius for NKG lat.dens.determ.
ARRANG  0.                             rotation of array to north
FIXHEI  0.  0                          first interaction height & target
FIXCHI  0.                             starting altitude (g/cm**2)
MAGNET  20.0  42.8                     magnetic field centr. europe
HADFLG  0  0  0  0  0  0               flags for hadr. interaction
GHEISH  T                              use gheisha for low energy hadrons
VENUS   T                              use venus for high energy hadrons
VENSIG  T                              use VENUS hadronic cross sections
ECUTS   0.3  0.3  0.020 0.020         energy cuts for particles
MUADDI  T                              additional info for muons
MUMULT  T                              muon multiple scattering angle
LONGI   T  1.  T                      longit.distr. & step size & fit
MAXPRT  @STEP                            max. number of printed events
ECTMAP  1.E4                           cut on gamma factor for printout
STEPFC  10.0                          mult. scattering step length fact.
DEBUG   F  6  F  1000000               debug flag and log.unit for out
VENDBG  0                              venus debug option
DIRECT  /c520.data/                   
CWAVLG  290.  600.                     Cherenkov wavelength band
CSCAT   1  0.  0.                     scatter Cherenkov events
CERSIZ  1.                             bunch size Cherenkov photons
CERFIL  T                              Cherenkov output to extra file
CERTEL         1                       Number of areas
        0. 0. 0. 0. 0. 100000. 40000. 1700.     x, y, z, theta, phi, diam, dist, foc
EXIT                                   terminates input
