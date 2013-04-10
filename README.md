equinox
=========

R package implementing equinox functions.
This file calculates the equinox dates, it is a wrapper for Fortran functions. The source of these calculations is: Andre L. Berger, 1978, "Long-Term Variations of Daily Insolation and Quaternary Climatic Changes", JAS, v.35, p.2362.  Also useful is: Andre L. Berger, May 1978, "A Simple Algorithm to Compute Long Term Variations of Daily Insolation", published by Institut D'Astronomie de Geophysique, Universite Catholique de Louvain, Louvain-la Neuve, No. 18.

Build instructions for general Linux machine.

1) download and unzip to a folder called equinox-master
2) from the parent folder: 
 a) R CMD build equinox-master/
 b) R CMD check equinox_0.1-1.tar.gz (optional)
 c) R CMD INSTALL equinox_0.1-1.tar.gz

Note: The version number may change, and you will likely have to set the PATH variable for other operating systems. This will also will require gfortran. Please see the documention for your Linux distribution, Fink for Mac, or Rtools on CRAN for Windows.


