PROGRAM READ_1PNT_SUB
! https://gitlab.com/infoaofd/lab/-/blob/master/00.SKILL/ANALYSIS/DATA_HANDLING/2022-12-07_NETCDF%E3%81%AB%E6%85%A3%E3%82%8C%E3%82%8B/0NETCDF_F90_SAMPLE/IMERG/TEST07.F90

use netcdf

CHARACTER(LEN=100)::VNAME1,VNAME2,VNAME3,VNAME4,VNAME5,VNAME
CHARACTER(LEN=500)::INFLE1,INFLE2,INFLE3,INFLE4,INFLE5,INFLE
CHARACTER(LEN=500)::OFLE1,OFLE2,OFLE3,OFLE4,OFLE5,OFLE

CHARACTER INDIR*500,IN*1000,ODIR*500,OUT*1000

INTEGER ncid, varid, status
INTEGER ncido, varido
REAL,PARAMETER::FILLVALUE=-9999.9

INTEGER,PARAMETER::IM=1,JM=1
real,ALLOCATABLE,DIMENSION(:,:,:) :: var2d
real,ALLOCATABLE,DIMENSION(:,:,:) :: QA,QS,SST,TA10,WND
real(8)::lon(IM),lat(JM)
INTEGER,ALLOCATABLE,DIMENSION(:)::time

namelist /para/INDIR,&
VNAME1,VNAME2,VNAME3,VNAME4,VNAME5, INFLE1,INFLE2,INFLE3,INFLE4,INFLE5, &
ODIR,OFLE1,OFLE2,OFLE3,OFLE4,OFLE5,NM

read(5,nml=para)

ALLOCATE(time(NM))
ALLOCATE(var2d(IM,JM,NM),QA(IM,JM,NM),QS(IM,JM,NM),SST(IM,JM,NM),&
TA10(IM,JM,NM),WND(IM,JM,NM))


DO I=1,5
var2d=0.0

IF(I==1)INFLE=INFLE1; IF(I==2)INFLE=INFLE2
IF(I==3)INFLE=INFLE3; IF(I==4)INFLE=INFLE4
IF(I==5)INFLE=INFLE5

IF(I==1)VNAME=VNAME1; IF(I==2)VNAME=VNAME2
IF(I==3)VNAME=VNAME3; IF(I==4)VNAME=VNAME4
IF(I==5)VNAME=VNAME5

IN=TRIM(INDIR)//TRIM(VNAME)//'/'//TRIM(INFLE)

CALL READ_NC(VNAME, IN, IM,JM,NM,var2d)

IF(I==1)QA=var2d; IF(I==2)QS=var2d
IF(I==3)SST=var2d; IF(I==4)TA10=var2d
IF(I==5)WND=var2d

END DO !I

!print '(A,A)','MMMMM OUTPUT'

DO I=1,5

IF(I==1)VNAME=VNAME1; IF(I==2)VNAME=VNAME2
IF(I==3)VNAME=VNAME3; IF(I==4)VNAME=VNAME4
IF(I==5)VNAME=VNAME5

IF(I==1)OFLE=OFLE1; IF(I==2)OFLE=OFLE2
IF(I==3)OFLE=OFLE3; IF(I==4)OFLE=OFLE4
IF(I==5)OFLE=OFLE5

var2d=0.0
IF(I==1)var2d=QA ; IF(I==2)var2d=QS
IF(I==3)var2d=SST; IF(I==4)var2d=TA10
IF(I==5)var2d=WND

OUT=TRIM(ODIR)//TRIM(OFLE)

CALL WRITE_NC(VNAME, OUT, IM, JM, NM, var2d)

END DO !I
!print '(A,A)','MMMMM END OF MAIN ROUTINE'


CONTAINS

SUBROUTINE READ_NC(VNAME, IN, IM,JM,NM,var2d)
CHARACTER(LEN=*),INTENT(IN)::VNAME
CHARACTER(LEN=*),INTENT(IN)::IN
INTEGER,INTENT(IN)::IM,JM,NM
real,INTENT(INOUT)::var2d(IM,JM,NM)

PRINT '(A)',"MMMMM INPUT FILE : "
PRINT '(A)',TRIM(IN)

CALL CHECK( nf90_open(IN, nf90_nowrite, ncid) )

CALL CHECK( nf90_inq_varid(ncid, "time", varid) )
print *,"time varid", varid
CALL CHECK( nf90_get_var(ncid, varid, time) )

CALL CHECK( nf90_inq_varid(ncid, "lon", varid) )
print *,"lon varid", varid
CALL CHECK(  nf90_get_var(ncid, varid, lon) )

CALL CHECK( nf90_inq_varid(ncid, "lat", varid) )
print *,"lat varid", varid
CALL CHECK( nf90_get_var(ncid, varid, lat) )

CALL CHECK( nf90_inq_varid(ncid, TRIM(VNAME), varid) )
print *,TRIM(VNAME), varid
CALL CHECK( nf90_get_var(ncid, varid, var2d) )
print *,'MIN=',minval(var2d)
print *,'MAX=',maxval(var2d)

status = nf90_close(ncid)
print *
END SUBROUTINE READ_NC



SUBROUTINE WRITE_NC(VNAME, OUT, IM, JM, NM, var2d)
CHARACTER(LEN=*),INTENT(IN)::VNAME
CHARACTER(LEN=*),INTENT(IN)::OUT
INTEGER,INTENT(IN)::IM,JM,NM
real,INTENT(INOUT)::var2d(IM,JM,NM)

status=nf90_create( trim(OUT), NF90_HDF5, ncido)

!print '(A,A)','MMMMM DEFINE DIMENSIONS'
status=nf90_def_dim(ncido, 'time', NM, id_time_dim)
status=nf90_def_dim(ncido, 'lon', IM, id_lon_dim)
status=nf90_def_dim(ncido, 'lat', JM, id_lat_dim)

!print '(A,A)','MMMMM DEFINE VARIABLES'
status=nf90_def_var(ncido, 'time', NF90_INT, id_time_dim, id_time)
status=nf90_def_var(ncido, 'lat', NF90_DOUBLE, id_lat_dim, id_lat)
status=nf90_def_var(ncido, 'lon', NF90_DOUBLE, id_lon_dim, id_lon)

status=nf90_put_att(ncido, id_lat, 'units','degrees_north')
status=nf90_put_att(ncido, id_lon, 'units','degrees_east')

CALL CHECK( NF90_DEF_VAR(NCIDO, TRIM(VNAME), NF90_REAL, &
     (/ID_LAT_DIM, ID_LON_DIM, ID_TIME_DIM/), ID_VARO))
CALL CHECK( NF90_PUT_ATT(NCIDO, ID_VARO,'units','W/m2') )
CALL CHECK( NF90_PUT_ATT(NCIDO, ID_VARO,'_FillValue', -9999.) )
CALL CHECK( NF90_PUT_ATT(NCIDO, ID_VARO,'missing_value', -9999.) )

! print '(A,A)','MMMMM GLOBAL ATTRIBUTES'
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"input_dir", LEN(TRIM(INDIR)),TRIM(INDIR))
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"input_file", LEN(TRIM(INFLE)),TRIM(INFLE))
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"output_dir", LEN(TRIM(ODIR)),TRIM(ODIR))
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"output_file", LEN(TRIM(OFLE)),TRIM(OFLE))

status=nf90_enddef(ncido) 



! print '(A,A)','MMMMM WRITE VARIABLES'
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_TIME, time ) )
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_LAT, lat ) )
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_LON, lon ) )
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_VARO, var2d ) )

CALL CHECK(NF90_CLOSE(NCIDO))

PRINT '(A)',"MMMMM OUTPUT FILE: "
PRINT '(A)',TRIM(OUT)

END SUBROUTINE WRITE_NC

SUBROUTINE CHECK( STATUS )
  INTEGER, INTENT (IN) :: STATUS
  IF(STATUS /= NF90_NOERR) THEN 
    PRINT '(A,A)','EEEEE ERROR ',TRIM(NF90_STRERROR(STATUS))
    STOP "ABNORMAL END"
  END IF
END SUBROUTINE CHECK


END PROGRAM
