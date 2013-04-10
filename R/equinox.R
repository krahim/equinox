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
