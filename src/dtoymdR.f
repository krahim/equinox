      SUBROUTINE dtoymdr (DAY, IYEAR,IMONTH,DATE)
C****
C**** For a given DAY measured from 2000 January 1, hour 0, determine
C**** the IYEAR (A.D.), IMONTH and DATE (between 0. and 31.).
C****
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 JDSUMN(12),JDSUML(12)
      INTEGER*8 N4CENT
      PARAMETER (JDAY4C = 365*400 + 97, !  number of days in 4 centuries
     *           JDAY1C = 365*100 + 24, !  number of days in 1 century
     *           JDAY4Y = 365*  4 +  1, !  number of days in 4 years
     *           JDAY1Y = 365)          !  number of days in 1 year
      DATA JDSUMN /0,31,59, 90,120,151, 181,212,243, 273,304,334/
      DATA JDSUML /0,31,60, 91,121,152, 182,213,244, 274,305,335/
C****
      N4CENT = FLOOR(DAY/JDAY4C)
      DAY4C  = DAY - N4CENT*JDAY4C
      N1CENT = (DAY4C-1)/JDAY1C
      IF(N1CENT.gt.0)  GO TO 10
C**** First of every fourth century: 16??, 20??, 24??, etc.
      DAY1C  = DAY4C
      N4YEAR = DAY1C/JDAY4Y
      DAY4Y  = DAY1C - N4YEAR*JDAY4Y
      N1YEAR = (DAY4Y-1)/JDAY1Y
      IF(N1YEAR.gt.0)  GO TO 200
      GO TO 100
C**** Second to fourth of every fourth century: 21??, 22??, 23??, etc.
   10 DAY1C  = DAY4C - N1CENT*JDAY1C - 1
      N4YEAR = (DAY1C+1)/JDAY4Y
      IF(N4YEAR.gt.0)  GO TO 20
C**** First four years of every second to fourth century when there is
C**** no leap year: 2100-2103, 2200-2203, 2300-2303, etc.
      DAY4Y  = DAY1C
      N1YEAR = DAY4Y/JDAY1Y
      DAY1Y  = DAY4Y - N1YEAR*JDAY1Y
      GO TO 210
C**** Subsequent four years of every second to fourth century when
C**** there is a leap year: 2104-2107, 2108-2111 ... 2204-2207, etc.
   20 DAY4Y  = DAY1C - N4YEAR*JDAY4Y + 1
      N1YEAR = (DAY4Y-1)/JDAY1Y
      IF(N1YEAR.gt.0)  GO TO 200
C****
C**** Current year is a leap frog year
C****
  100 DAY1Y = DAY4Y
      DO 120 M=1,11
  120 IF(DAY1Y.lt.JDSUML(M+1))  GO TO 130
C     M=12
  130 IYEAR  = 2000 + N4CENT*400 + N1CENT*100 + N4YEAR*4 + N1YEAR
      IMONTH = M
      DATE   = DAY1Y - JDSUML(M)
      RETURN
C****
C**** Current year is not a leap frog year
C****
  200 DAY1Y  = DAY4Y - N1YEAR*JDAY1Y - 1
  210 DO 220 M=1,11
  220 IF(DAY1Y.lt.JDSUMN(M+1))  GO TO 230
C     M=12
  230 IYEAR  = 2000 + N4CENT*400 + N1CENT*100 + N4YEAR*4 + N1YEAR
      IMONTH = M
      DATE   = DAY1Y - JDSUMN(M)
      RETURN
      END SUBROUTINE dtoymdr
