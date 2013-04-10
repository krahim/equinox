      SUBROUTINE VERNALR (IYEAR, VERNAL)
C****
C**** For a given year, VERNAL calculates an approximate time of vernal
C**** equinox in days measured from 2000 January 1, hour 0.
C****
C**** VERNAL assumes that vernal equinoxes from one year to the next
C**** are separated by exactly 365.2425 days, a tropical year
C**** [Explanatory Supplement to The Astronomical Ephemeris].  If the
C**** tropical year is 365.2422 days, as indicated by other references,
C**** then the time of the vernal equinox will be off by 2.88 hours in
C**** 400 years.
C****
C**** Time of vernal equinox for year 2000 A.D. is March 20, 7:36 GMT
C**** [NASA Reference Publication 1349, Oct. 1994].  VERNAL assumes
C**** that vernal equinox for year 2000 will be on March 20, 7:30, or
C**** 79.3125 days from 2000 January 1, hour 0.  Vernal equinoxes for
C**** other years returned by VERNAL are also measured in days from
C**** 2000 January 1, hour 0.  79.3125 = 31 + 29 + 19 + 7.5/24.
C**** 
      IMPLICIT REAL*8 (A-H,O-Z)
      integer IYEAR
      double precision VERNAL
      PARAMETER (EDAYzY=365.2425d0, VE2000=79.3125)
      VERNAL = VE2000 + (IYEAR-2000)*EDAYzY

      END SUBROUTINE VERNALR
