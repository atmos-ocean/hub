INTEGER YR0,MO0,DY0,HR0,MI0,SS0
INTEGER year,month,day,hour,min,sec !FOR TEST
REAL*8 julian_day

YR0=2021; MO0=8; DY0=12; HR0=0; MI0=0; SS0=0

PRINT '(A)',' Y    MO  D  H MI'
PRINT '(I5,5I3)',YR0,MO0,DY0,HR0,MI0

call date2jd(YR0,MO0,DY0,HR0,MI0,SS0,julian_day)

PRINT '(A,F12.3)',&
&'MODIFIED JULIAN DAY (FROM 1858/11/1700:00:00)=',julian_day
PRINT *

call jd2date(year,month,day,hour,min,sec,julian_day)
PRINT '(I5,5I3)',year,month,day,hour,min

END
