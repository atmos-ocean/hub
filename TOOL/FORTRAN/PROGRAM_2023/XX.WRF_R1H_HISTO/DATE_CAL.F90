      subroutine date2jd(year,month,day,hh,mm,ss,julian_day)
      implicit none
!-----------------------------------------------------------------------
!     get Julian day from Gregolian caldendar
!-----------------------------------------------------------------------

! ... intent(in)
      integer :: year, month, day, hh, mm, ss
! ... intent(out)
      real(8) :: julian_day
! ... parameter
      real(8), parameter :: jd0  = 1720996.5d0 ! BC4713/ 1/ 1 12:00:00
      real(8), parameter :: mjd0 = 2400000.5d0 !   1858/11/17 00:00:00
! ... local
      integer :: y, m
      if (month < 3) then
        y = year - 1
        m = month + 12
      else
        y = year
        m = month
      end if

      julian_day = 365*(y) + int(y)/4 - int(y)/100 + int(y)/400  &
     &           + int((m+1)*30.6001d0) &
     &           + day           &
     &           + hh/24.0d0     &
     &           + mm/1440.0d0   &
     &           + ss/86400.0d0  &
     &           + jd0

! ... convert julian day to modified julian day
      julian_day = julian_day - mjd0

      end subroutine date2jd

      subroutine jd2date(year,month,day,hour,min,sec,julian_day)
      implicit none
!-----------------------------------------------------------------------
!     get Gregolian caldendar from Julian day
!-----------------------------------------------------------------------

! ... intent(in)
      real(8) :: julian_day
! ... intent(out)
      integer :: year, month, day, hour, min, sec
! ... parameter
      real(8), parameter :: mjd0 = 2400000.5d0 !  1858/11/17 00:00:00
! ... local
      integer :: jalpha, ia, ib, ic, id
      integer :: itime
      real(8) :: jday, d, xtime

! ... convert modified julian day to julian day
      jday = julian_day + mjd0

      jday = jday + 0.5d0
      if (jday >= 2299161) then
        jalpha = int( (jday - 1867216.25d0)/36524.25d0 )
        d = jday + 1 + jalpha - int(0.25d0*jalpha)
      else
        d = jday
      end if

      ia = int(d) + 1524
      ib = int(6680.0d0 + ((ia-2439870) - 122.1d0)/365.25d0)
      ic = 365*ib + int(0.25d0*ib)
      id = int((ia-ic)/30.6001d0)
      xtime = (d-int(d))*86400.d0
      itime = xtime
      if ( xtime-itime > 0.5d0 ) itime = itime + 1

      day   = ia - ic - int(30.6001*id)

      month = id - 1
      if (month > 12) month = month - 12

      year  = ib - 4715
      if (month > 2) year = year - 1
      if (year <= 0) year = year - 1

      hour  = itime/3600.
      min   = (itime - hour*3600)/60
      sec   = itime - hour*3600 - min*60

      end subroutine jd2date

