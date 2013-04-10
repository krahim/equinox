      SUBROUTINE dtomdhmr (DAY, IYEAR,IMONTH,IDATE,IHOUR,IMINUT)
C****
C**** DtoYMDHM receives DAY measured since 2000 January 1, hour 0 and
C**** returns YEAR, MONTH, DATE, HOUR and MINUTE based on the Gregorian
C**** calendar.
C****
      integer iyear, imonth, idate, ihour, iminut

      REAL*8 DAY,DATE
      CALL dtoymdr (DAY, IYEAR,IMONTH,DATE)
      IDATE  = DATE+1
      IMINUT = NINT ((DATE-IDATE+1)*24.*60.)
      IHOUR  = IMINUT / 60
      IMINUT = IMINUT - IHOUR*60
      RETURN
      END SUBROUTINE dtomdhmr
