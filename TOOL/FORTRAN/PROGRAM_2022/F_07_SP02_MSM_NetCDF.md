# プログラミング入門 07_SP

**MSMデータ(NetCDF形式)読み込み用Fortranプログラム**

[[_TOC_]]

## 使用例

### コンパイル

```
$ 00.COMPILE.sh
```

EXE FILE: test_nc.exe

-rwxr-xr-x. 1 am oc 876K 10月 20 22:11 test_nc.exe

### リンク先の確認

```
$ ldd test_nc.exe |grep netcdf
```

​        libnetcdff.so.7 => **/usr/local/netcdf-c-4.8.0/lib/libnetcdff.so.7** (0x00007fbc37860000)
​        libnetcdf.so.19 => **/usr/local/netcdf-c-4.8.0/lib/libnetcdf.so.19** (0x00007fbc367cd000)

### 実行

```
$ test_nc.exe
```

INPUT: /work01/DATA/MSM/MSM-P/2022/0601.nc

Open : /work01/DATA/MSM/MSM-P/2022/0601.nc
Read lon
Read lat
Read p
Read time
Read z
Read w
Read u
Read v
Read temp
Read rh



## ソースファイル

### コンパイル用スクリプト

#### 00.COMPILE.sh 

```Fortran
# EXECUTABLE FILE NAME
EXE=test_nc.exe

# LIBRARY INFO
INCLUDE_DIR=/usr/local/netcdf-c-4.8.0/include
LIBRARY_DIR=/usr/local/netcdf-c-4.8.0/lib
LIBRARY=netcdff

# COMPILE
## SUBROUTINES
ifort -c mod_msmp_var.f90 -I$INCLUDE_DIR -L$LIBRARY_DIR -l$LIBRARY
ifort -c mod_msms_var.f90 -I$INCLUDE_DIR -L$LIBRARY_DIR -l$LIBRARY
ifort -c mod_input.f90  -I$INCLUDE_DIR -L$LIBRARY_DIR -l$LIBRARY
## MAIN ROUTINE
ifort -c main.f90 -I$INCLUDE_DIR -L$LIBRARY_DIR -l$LIBRARY

# LINK
ifort mod_msms_var.o mod_msmp_var.o mod_input.o main.o -o $EXE -I$INCLUDE_DIR -L$LIBRARY_DIR -l$LIBRARY

# SHOW INFO
echo; echo EXE FILE: $EXE; echo
ls -lh $EXE
```



### FORTRANプログラム

#### main.f90

```Fortran
PROGRAM TEST_NC_MSM

USE mod_msmp_var
USE mod_msms_var
USE mod_input

CHARACTER(LEN=500)::INDIR,INFLE
CHARACTER(LEN=1000)::IN

! MSM pressure level data
type(msmp)::p

INDIR='/work01/DATA/MSM/MSM-P/2022/'
INFLE='0601.nc'

IN=TRIM(INDIR)//TRIM(INFLE)

PRINT '(A,A)','INPUT: ',TRIM(IN)

CALL read_msmp(IN, p)

print *,p%p(1)
print *,p%lon(p%plon/2),p%lat(p%plat/2)
print *,p%u(1,1, p%plon/2,p%plat/2)

END PROGRAM TEST_NC_MSM
```



#### mod_msmp_var.f90

```Fortran
module mod_msmp_var

!  use
!  implicit none

!  write(*,'(a)')'Module: module_msmp_var'
!  write(*,*)''
!  private
! ncid:ファイルのID番号、 varid: 変 数のID番号
  integer,public :: ncid,varid
  integer,public :: stat

  integer,public,parameter :: plon=241, plat=253, ptime=8, ppres=16
  real,public,parameter :: dlon=0.1250, dlat=0.1000
  real,public,parameter :: dtime=3.0

! 3D grid data at the pressure level
! MSM gridded data
  type, public :: msmp
    character(len=50):: var
    real,dimension (plon) :: lon
    real,dimension (plat) :: lat
    real,dimension (ppres) :: p
    real,dimension (ptime) :: time

    real,dimension(plon, plat, ppres, ptime) :: z, w, u, v, temp, rh

  end type msmp

contains

subroutine read_msmp_vars(varname, varout)

  include 'netcdf.inc'

  character(len=*),intent(in)::varname
  real,dimension(:,:,:,:),intent(inout)::varout

  integer(2),dimension(plon,plat,ppres,ptime)::varout_int
  real(8),dimension(plon,plat,ppres,ptime)::varout_dbl

  integer,allocatable :: dimids(:),nshape(:)
  character(len=70) :: err_message
  character(len=100) :: dimname !dimnameは各次元の名前。

!  print '(A)',varname(1:lnblnk(varname))
  ! ファイルID(ncid)からs%varで設定した変数のID(varid)を得る。
  stat = nf_inq_varid(ncid,varname,varid)
!  print *,'ncid=', ncid
!  print *,'varid=', varid
!  print *,'stat= ', stat
!  print *

  !  スケールファクターを得る。(*1)
  stat = nf_get_att_real(ncid,varid,'scale_factor',scale)
!  print *,'scale= ',scale
!  print *,'stat= ',stat
!  print *

  !  オフセットの 値を得る(*2)
  stat = nf_get_att_real(ncid,varid,'add_offset',offset)
!  print *,'offset=', offset
!  print *,'stat= ',stat
!  print *

! varidからndimsを得る。ndimsとは次元の数。二次元データなら2
  stat = nf_inq_varndims(ncid, varid, ndims)
!  print *,'stat=',stat
!  print *,'ndims= ', ndims
!  print *

! varidからdimidsを得る。dimidsとはそれぞれの次元に対するID
  allocate(dimids(ndims))
  stat = nf_inq_vardimid(ncid, varid, dimids)
!  print *,'stat= ',stat
!  print *,'ndims= ', ndims
!  print *,dimids
!  do i=1,ndims
!    write(6,'("dimids(",i3,")=",i9 )') i,dimids(i)
!  enddo
!  print *

  allocate(nshape(ndims))
  do i=1,ndims
!   dimidsからnshapeを得る。
!   nshapeとはそれぞれの次元に対する最大データ数 (格子数)
    stat = nf_inq_dimlen(ncid, dimids(i), nshape(i))
!    print *,'stat=',stat
!    print *,nshape
!    write(6,'("nshape(",i3,")=",i9 )') i,nshape(i)
  enddo !i
!  print *

  do i=1,ndims
    stat = nf_inq_dimname(ncid, dimids(i), dimname)
!    print *,'stat=',stat
!    print *,'dimname=',dimname
!    write(6,'("dimname(",i3,")=",1x,a23)') i,dimname
  enddo
!  print *

!  print *,'Read ',varname(1:lnblnk(varname))

  if(varname == 'u' .or. varname == 'v' .or. varname == 'temp' &
  .or. varname == 'rh')then

    stat = nf_get_var_int2(ncid, varid, varout_int)

  else if (varname == 'z' .or. varname == 'w')then

    stat = nf_get_var_double(ncid, varid, varout_dbl)

  endif

  if(stat /= 0)then
    print *,'stat= ',stat
    print *,"Terminated."
    print *
    stop
  endif

  if(varname == 'u' .or. varname == 'v' .or. varname == 'temp' &
  .or. varname == 'rh')then

    varout(:,:,:,:)=float(varout_int(:,:,:,:))*scale + offset

  else if (varname == 'z' .or. varname == 'w')then

    varout(:,:,:,:)=sngl(varout_dbl(:,:,:,:))

  endif

  deallocate(dimids,nshape)


end subroutine

end module
```





#### mod_msms_var.f90

```Fortran
module mod_msms_var


!  write(*,'(a)')'Module: module_msms_var'
!  write(*,*)''
!  private
! ncid:ファイルのID番号、 varid: 変 数のID番号
  integer,public :: ncid,varid
  integer,public :: stat

  integer,public,parameter :: slon=481, slat=505, stime=24
  real,public,parameter :: sdlon=0.0625, sdlat=0.0500
  real,public,parameter :: sdtime=1.0

! 2D grid data at the surface
! MSM gridded data
  type, public :: msms
    character(len=50):: var
    real,dimension (slon) :: lon
    real,dimension (slat) :: lat
    real,dimension (stime) :: time
    real,dimension(slon,slat,stime) :: psea, sp, u, v, temp, &
&   rh, r1h, ncld_upper, ncld_mid, ncld_low, ncld
  end type

! Station time series data at the surface
! Interpolated in space on the surface
  type, public :: msms_ts
    character(len=50):: var
    real,dimension (slon) :: lon
    real,dimension (slat) :: lat
    real,dimension (stime) :: time
    real:: psea(stime), sp(stime), u(stime), v(stime), temp(stime), &
&   rh(stime), r1h(stime), ncld_upper(stime), ncld_mid(stime), &
&   ncld_low(stime), ncld(stime)
  end type

! Station data at the surface
! Interpolated both space in time
  type, public :: msms_stn
    character(len=50):: var
    real,dimension (slon) :: lon
    real,dimension (slat) :: lat
    real,dimension (stime) :: time
    real:: psea, sp, u, v, temp, &
&   rh, r1h, ncld_upper, ncld_mid, ncld_low, ncld
    real::    pt,vpt,ept, wmix,q
    real:: w !(=omega) 
!   w is dummy variable since omega is always zero at the surface.

  end type



contains

subroutine read_msms_vars(varname, varout)

  include 'netcdf.inc'

  character(len=*),intent(in)::varname
  real,dimension(:,:,:),intent(inout)::varout

  integer(2),dimension(slon,slat,stime)::varout_int

  integer,allocatable :: dimids(:),nshape(:)
  character(len=70):: err_message
  character(len=100) :: dimname !dimnameは各次元の名前。


!  print '(A)',varname(1:lnblnk(varname))
  ! ファイルID(ncid)からs%varで設定した変数のID(varid)を得る。
  sta = nf_inq_varid(ncid,varname,varid)
!  print *,'varid=', varid
!  print *,'sta= ', sta
!  print *

  !  スケールファクターを得る。(*1)
  sta = nf_get_att_real(ncid,varid,'scale_factor',scale)
!  print *,'scale= ',scale
!  print *,'sta= ',sta
!  print *

  !  オフセットの 値を得る(*2)
  sta = nf_get_att_real(ncid,varid,'add_offset',offset)
!  print *,'offset=', offset
!  print *,'sta= ',sta
!  print *

! varidからndimsを得る。ndimsとは次元の数。二次元データなら2
  stat = nf_inq_varndims(ncid, varid, ndims)
!  print *,'ndims= ', ndims
!  print *

! varidからdimidsを得る。dimidsとはそれぞれの次元に対するID
  allocate(dimids(ndims))
  stat = nf_inq_vardimid(ncid, varid, dimids)
!  do i=1,ndims
!    write(6,'("dimids(",i2,")=",i9 )') i,dimids(i)
!  enddo
!  print *

  allocate(nshape(ndims))
  do i=1,ndims
!   dimidsからnshapeを得る。
!   nshapeとはそれぞれの次元に対する最大データ数 (格子数)
    stat = nf_inq_dimlen(ncid, dimids(i), nshape(i))
!    write(6,'("nshape(",i2,")=",i9 )') i,nshape(i)
  enddo !i
!  print *

  do i=1,ndims
    stat = nf_inq_dimname(ncid, dimids(i), dimname)
!    write(6,'("dimname(",i2,")=",1x,a23)') i,dimname
  enddo
!  print *

!  print *,'Read ',varname(1:lnblnk(varname))
  stat = nf_get_var_int2(ncid, varid, varout_int)
  if(stat /= 0)then
    print *,'stat= ',stat
    print *,"Terminated."
    print *
    stop
  endif
  deallocate(dimids,nshape)

  varout(:,:,:)=float(varout_int(:,:,:))*scale + offset

end subroutine read_msms_vars

!  write(*,'(a)')'Done module module_msms_var.'
!  write(*,*) 
end module
```



#### mod_input.f90

```FORTRAN
module mod_input

contains

subroutine read_msmp(in_msmp, p)
  use mod_msmp_var

  include 'netcdf.inc'

  character(len=*),intent(in) :: in_msmp
  type(msmp),intent(inout)::p


!  write(*,'(a)')'Subroutine: read_msmp'
!  write(*,*)''
  print *
  stat = nf_open(in_msmp, nf_nowrite, ncid) ! ファイルのopenとNetCDF ID(ncid)の取得
!  print *,'ncid= ',ncid
  if(stat == 0)then
    print '(A,A)','Open : ',in_msmp(1:lnblnk(in_msmp))
  else
    print '(A,A)','Error while opening ',in_msmp(1:lnblnk(in_msmp))
    print *,'status= ',stat
    stop
  endif

!==================
  p%var='lon'
!==================
  ! ファイルID(ncid)からvarで設定した変数のID(varid)を得る。
  stat = nf_inq_varid(ncid,p%var,varid)
!  print '(A)','varid=', varid
!  print *,'sta= ', sta
  print '(A,A)','Read ',p%var(1:lnblnk(p%var))
  stat = nf_get_var_real(ncid, varid, p%lon)
!  print *,'stat= ',stat



!==================
  p%var='lat'
!==================
  ! ファイルID(ncid)からvarで設定した変数のID(varid)を得る。
  stat = nf_inq_varid(ncid,p%var,varid)
!  print *,'varid=', varid
!  print *,'sta= ', sta
  print '(A,A)','Read ',p%var(1:lnblnk(p%var))
  stat = nf_get_var_real(ncid, varid, p%lat)
!  print *,'stat= ',stat
!  print *

!==================
  p%var='p'
!==================
  ! ファイルID(ncid)からvarで設定した変数のID(varid)を得る。
  stat = nf_inq_varid(ncid,p%var,varid)
!  print *,'varid=', varid
!  print *,'sta= ', sta
  print '(A,A)','Read ',p%var(1:lnblnk(p%var))
  stat = nf_get_var_real(ncid, varid, p%p)


!==================
  p%var='time'
!==================
  ! ファイルID(ncid)からvarで設定した変数のID(varid)を得る。
  stat = nf_inq_varid(ncid,p%var,varid)
!  print *,'varid=', varid
!  print *,'sta= ', sta
  print '(A,A)','Read ',p%var(1:lnblnk(p%var))
  stat = nf_get_var_real(ncid, varid, p%time)
!  print *,'stat= ',stat
!  print *

  print '(A)', 'Read z'; call read_msmp_vars('z',p%z)
  print '(A)', 'Read w'; call read_msmp_vars('w',p%w)
  print '(A)', 'Read u'; call read_msmp_vars('u',p%u)
  print '(A)', 'Read v'; call read_msmp_vars('v',p%v)
  print '(A)', 'Read temp'; call read_msmp_vars('temp',p%temp)
  print '(A)', 'Read rh'; call read_msmp_vars('rh',p%rh)

end subroutine read_msmp



subroutine read_msms(in_msms, s)

  use mod_msms_var

  include 'netcdf.inc'

  character(len=*),intent(in) :: in_msms
  type(msms),intent(inout)::s


!  write(*,'(a)')'Subroutine: read_msms'
!  write(*,*)''
  print *
  stat = nf_open(in_msms, nf_nowrite, ncid) ! ファイルのopenとNetCDF ID(ncid)の取得
  if(stat == 0)then
    print '(A,A)','Open : ',in_msms(1:lnblnk(in_msms))
  else
    print '(A,A)','Error while opening ',in_msms(1:lnblnk(in_msms))
    print *,'status= ',stat
    stop
  endif


!==================
  s%var='lon'
!==================
  ! ファイルID(ncid)からvarで設定した変数のID(varid)を得る。
  stat = nf_inq_varid(ncid,s%var,varid)
!  print *,'varid=', varid
!  print *,'sta= ', sta
  print *,'Read ',s%var(1:lnblnk(s%var))
  stat = nf_get_var_real(ncid, varid, s%lon)
!  print *,'stat= ',stat



!==================
  s%var='lat'
!==================
  ! ファイルID(ncid)からvarで設定した変数のID(varid)を得る。
  stat = nf_inq_varid(ncid,s%var,varid)
!  print *,'varid=', varid
!  print *,'sta= ', sta
  print *,'Read ',s%var(1:lnblnk(s%var))
  stat = nf_get_var_real(ncid, varid, s%lat)
!  print *,'stat= ',stat
!  print *


!==================
  s%var='time'
!==================
  ! ファイルID(ncid)からvarで設定した変数のID(varid)を得る。
  stat = nf_inq_varid(ncid,s%var,varid)
!  print *,'varid=', varid
!  print *,'sta= ', sta
  print *,'Read ',s%var(1:lnblnk(s%var))
  stat = nf_get_var_real(ncid, varid, s%time)
!  print *,'stat= ',stat
!  print *

  call read_msms_vars('psea',s%psea)
  call read_msms_vars('sp',s%sp)
  call read_msms_vars('u',s%u)
  call read_msms_vars('v',s%v)
  call read_msms_vars('temp',s%temp)
  call read_msms_vars('rh',s%rh)
  call read_msms_vars('r1h',s%r1h)
  call read_msms_vars('ncld_upper',s%ncld_upper)
  call read_msms_vars('ncld_mid',s%ncld_mid)
  call read_msms_vars('ncld_low',s%ncld_low)
  call read_msms_vars('ncld',s%ncld)


end subroutine read_msms

end module
```



## データファイルの構造

### 気圧面データ
```fortran
! $ ncdump ~/Data/MSM/MSM-P/1012.nc -h
! netcdf 1012 {
! dimensions:
!         lon = 241 ;
!         lat = 253 ;
!         p = 16 ;
!         time = 8 ;
! variables:
!         float lon(lon) ;
!                 lon:long_name = "longitude" ;
!                 lon:units = "degrees_east" ;
!                 lon:standard_name = "longitude" ;
!         float lat(lat) ;
!                 lat:long_name = "latitude" ;
!                 lat:units = "degrees_north" ;
!                 lat:standard_name = "latitude" ;
!         float p(p) ;
!                 p:long_name = "pressure level" ;
!                 p:standard_name = "air_pressure" ;
!                 p:units = "hPa" ;
!         float time(time) ;
!                 time:long_name = "time" ;
!                 time:standard_name = "time" ;
!                 time:units = "hours since 2012-10-12 00:00:00+00:00" ;
!         double z(time, p, lat, lon) ;
!                 z:long_name = "geopotential height" ;
!                 z:units = "m" ;
!                 z:standard_name = "geopotential_height" ;
!         double w(time, p, lat, lon) ;
!                 w:long_name = "vertical velocity in p" ;
!                 w:units = "Pa/s" ;
!                 w:standard_name = "lagrangian_tendency_of_air_pressure" ;
!         short u(time, p, lat, lon) ;
!                 u:scale_factor = 0.006116208155 ;
!                 u:add_offset = 0. ;
!                 u:long_name = "eastward component of wind" ;
!                 u:units = "m/s" ;
!                 u:standard_name = "eastward_wind" ;
!         short v(time, p, lat, lon) ;
!                 v:scale_factor = 0.006116208155 ;
!                 v:add_offset = 0. ;
!                 v:long_name = "northward component of wind" ;
!                 v:units = "m/s" ;
!                 v:standard_name = "northward_wind" ;
!         short temp(time, p, lat, lon) ;
!                 temp:scale_factor = 0.002613491379 ;
!                 temp:add_offset = 255.4004974 ;
!                 temp:long_name = "temperature" ;
!                 temp:units = "K" ;
!                 temp:standard_name = "air_temperature" ;
!         short rh(time, p, lat, lon) ;
!                 rh:scale_factor = 0.002293577883 ;
!                 rh:add_offset = 75. ;
!                 rh:long_name = "relative humidity" ;
!                 rh:units = "%" ;
!                 rh:standard_name = "relative_humidity" ;
! 
! // global attributes:
!                 :Conventions = "CF-1.0" ;
!                 :history = "created by create_1daync_msm_p.rb  2012-10-14" ;
! }
```

### 地表面データ
```fortran
! $ ncdump -h 1012.nc
! netcdf 1012 {
! dimensions:
!         lon = 481 ;
!         lat = 505 ;
!         time = 24 ;
!         ref_time = 8 ;
! variables:
!         float lon(lon) ;
!                 lon:long_name = "longitude" ;
!                 lon:units = "degrees_east" ;
!                 lon:standard_name = "longitude" ;
!         float lat(lat) ;
!                 lat:long_name = "latitude" ;
!                 lat:units = "degrees_north" ;
!                 lat:standard_name = "latitude" ;
!         float time(time) ;
!                 time:long_name = "time" ;
!                 time:standard_name = "time" ;
!                 time:units = "hours since 2012-10-12 00:00:00+00:00" ;
!         float ref_time(ref_time) ;
!                 ref_time:long_name = "forecaset reference time" ;
!                 ref_time:standard_name = "forecaset_reference_time" ;
!                 ref_time:units = "hours since 2012-10-12 00:00:00+00:00" ;
!         short psea(time, lat, lon) ;
!                 psea:scale_factor = 0.4587155879 ;
!                 psea:add_offset = 95000. ;
!                 psea:long_name = "sea level pressure" ;
!                 psea:units = "Pa" ;
!                 psea:standard_name = "air_pressure" ;
!         short sp(time, lat, lon) ;
!                 sp:scale_factor = 0.4587155879 ;
!                 sp:add_offset = 95000. ;
!                 sp:long_name = "surface air pressure" ;
!                 sp:units = "Pa" ;
!                 sp:standard_name = "surface_air_pressure" ;
!         short u(time, lat, lon) ;
!                 u:scale_factor = 0.006116208155 ;
!                 u:add_offset = 0. ;
!                 u:long_name = "eastward component of wind" ;
!                 u:units = "m/s" ;
!                 u:standard_name = "eastward_wind" ;
!         short v(time, lat, lon) ;
!                 v:scale_factor = 0.006116208155 ;
!                 v:add_offset = 0. ;
!                 v:long_name = "northward component of wind" ;
!                 v:units = "m/s" ;
!                 v:standard_name = "northward_wind" ;
!         short temp(time, lat, lon) ;
!                 temp:scale_factor = 0.002613491379 ;
!                 temp:add_offset = 255.4004974 ;
!                 temp:long_name = "temperature" ;
!                 temp:units = "K" ;
!                 temp:standard_name = "air_temperature" ;
!         short rh(time, lat, lon) ;
!                 rh:scale_factor = 0.002293577883 ;
!                 rh:add_offset = 75. ;
!                 rh:long_name = "relative humidity" ;
!                 rh:units = "%" ;
!                 rh:standard_name = "relative_humidity" ;
!         short r1h(time, lat, lon) ;
!                 r1h:scale_factor = 0.006116208155 ;
!                 r1h:add_offset = 200. ;
!                 r1h:long_name = "rainfall in 1 hour" ;
!                 r1h:units = "mm/h" ;
!                 r1h:standard_name = "rainfall_rate" ;
!         short ncld_upper(time, lat, lon) ;
!                 ncld_upper:scale_factor = 0.001666666591 ;
!                 ncld_upper:add_offset = 50. ;
!                 ncld_upper:long_name = "upper-level cloudiness" ;
!                 ncld_upper:units = "%" ;
!         short ncld_mid(time, lat, lon) ;
!                 ncld_mid:scale_factor = 0.001666666591 ;
!                 ncld_mid:add_offset = 50. ;
!                 ncld_mid:long_name = "mid-level cloudiness" ;
!                 ncld_mid:units = "%" ;
!         short ncld_low(time, lat, lon) ;
!                 ncld_low:scale_factor = 0.001666666591 ;
!                 ncld_low:add_offset = 50. ;
!                 ncld_low:long_name = "low-level cloudiness" ;
!                 ncld_low:units = "%" ;
!         short ncld(time, lat, lon) ;
!                 ncld:scale_factor = 0.001666666591 ;
!                 ncld:add_offset = 50. ;
!                 ncld:long_name = "cloud amount" ;
!                 ncld:units = "%" ;
!                 ncld:standard_name = "cloud_area_fraction" ;
```



## 上達のためのポイント

**エラーが出た時の対応の仕方でプログラミングの上達の速度が大幅に変わる**。

ポイントは次の3つである

1. エラーメッセージをよく読む
2. エラーメッセージを検索し，ヒットしたサイトをよく読む
3. 変数に関する情報を書き出して確認する

エラーメッセージは，プログラムが不正終了した直接の原因とその考えられる理由が書いてあるので，よく読むことが必要不可欠である。

記述が簡潔なため，内容が十分に理解できないことも多いが，その場合**エラーメッセージをブラウザで検索**してヒットした記事をいくつか読んでみる。

エラーの原因だけでなく，**考えうる解決策**が記載されていることも良くある。

エラーを引き起こしていると思われる箇所の**変数の情報**や**変数の値そのものを書き出して**，**期待した通りにプログラムが動作しているか確認する**ことも重要である。

エラーの場所が特定できれば，エラーの修正の大部分は完了したと考えてもよいほどである。

エラーメッセージや検索してヒットするウェブサイトは英語で記載されていることも多いが，**重要な情報は英語で記載されていることが多い**ので，よく読むようにする。

重要そうに思われるが，一回で理解できないものは，PDFなどに書き出して後で繰り返し読んでみる。どうしても**内容が頭に入らないものは印刷してから読む**。