## note the hard coded location of the shared object /dll file.
## please change this


##dyn.load(paste("~/RLibs/equinox", .Platform$dynlib.ext, sep=""));

isLeapYear <- function(year) {
    res <- if( ((year %% 4 ==  0) && (year %% 100 != 0 )) ||
              (year %% 400 == 0)) TRUE else FALSE
    return(res)
}

## integers in R are 32 bits.
vernal <- function(iyear) {
    ## C****
    ## C**** For a given year, VERNAL calculates an approximate time of vernal
    ## C**** equinox in days measured from 2000 January 1, hour 0.
    ## C****
    ## C**** VERNAL assumes that vernal equinoxes from one year to the next
    ## C**** are separated by exactly 365.2425 days, a tropical year
    ## C**** [Explanatory Supplement to The Astronomical Ephemeris].  If the
    ## C**** tropical year is 365.2422 days, as indicated by other references,
    ## C**** then the time of the vernal equinox will be off by 2.88 hours in
    ## C**** 400 years.
    ## C****
    ## C**** Time of vernal equinox for year 2000 A.D. is March 20, 7:36 GMT
    ## C**** [NASA Reference Publication 1349, Oct. 1994].  VERNAL assumes
    ## C**** that vernal equinox for year 2000 will be on March 20, 7:30, or
    ## C**** 79.3125 days from 2000 January 1, hour 0.  Vernal equinoxes for
    ## C**** other years returned by VERNAL are also measured in days from
    ## C**** 2000 January 1, hour 0.  79.3125 = 31 + 29 + 19 + 7.5/24.
    res <- .Fortran("vernalr", as.integer(iyear), res1=double(1))
    
    return(res$res1)

}


DtoYMDHM <- function(day) {
    res <- .Fortran("dtomdhmr", as.double(day), iyear=integer(1),
                    imonth=integer(1), idate=integer(1), ihour=integer(1),
                    iminut=integer(1))
    return(list(iyear=res$iyear, imonth=res$imonth, idate=res$idate,
                ihour=res$ihour, iminut=res$iminut))
}

DtoYMD <- function(day) {
    res <- .Fortran("dtoymdr", as.double(day), iyear=integer(1),
                    imonth=integer(1), date=double(1))
    return(list(iyear=res$iyear, imonth=res$imonth, date=res$date))
}

orbpar <- function(year) {
    res <- .Fortran("orbparr", as.double(year), eccen=double(1),
                    obliq=double(1),
                    omegvp=double(1))
    return(list(eccen=res$eccen, obliq=res$obliq, omegvp=res$omegvp))
}

equinox <- function(iyear) {
    ## calculates vernal and automal equniox
    ## measured in days from 2000, Jan 1 hour 0.
    ## see vernal for more details.
    ## Time of vernal equinox for year 2000 A.D. is March 20, 7:36 GMT
    ## [NASA Reference Publication 1349, Oct. 1994].
    ## vernal(2000) gives  79.3125
    
    EDAYzY <- 365.2425
    
    verEQX <- vernal(iyear)
    orbPar <- orbpar(iyear)
    
    bsemi <- sqrt(1 - orbPar$eccen * orbPar$eccen)
    TAofVE <- - orbPar$omegvp
    EAofVE <- atan2(sin(TAofVE)*bsemi, cos(TAofVE)+orbPar$eccen)
    MAofVE <- EAofVE - orbPar$eccen * sin(EAofVE)
    if(MAofVE < 0)
        MAofVE <- MAofVE + 2*pi
    
    TAofAE <- TAofVE + pi
    EAofAE <- atan2(sin(TAofAE)*bsemi, cos(TAofAE)+orbPar$eccen)
    MAofAE <- EAofAE - orbPar$eccen * sin(EAofAE)
    if(MAofAE < 0)
        MAofAE <- MAofAE + 2*pi
    ##print(MAofAE)
    ##print(MAofVE)
    autEQX <- verEQX + (MAofAE - MAofVE) * EDAYzY/(2*pi)
    return(list(verEQX=verEQX, autEQX=autEQX))
}
    
    
equinoxDate <- function(iyear) {
    equinoxDay <- equinox(iyear)

    verEQX_Day <- DtoYMDHM(equinoxDay$verEQX)
    autEQX_Day <- DtoYMDHM(equinoxDay$autEQX)

    return(list(verEQX=verEQX_Day, autEQX=autEQX_Day))
}
