PROGRAM READ_1PNT
! https://gitlab.com/infoaofd/lab/-/blob/master/00.SKILL/ANALYSIS/DATA_HANDLING/2022-12-07_NETCDF%E3%81%AB%E6%85%A3%E3%82%8C%E3%82%8B/0NETCDF_F90_SAMPLE/IMERG/TEST07.F90

use netcdf

CHARACTER INDIR*500,INFLE*500,IN*1000,ODIR*500,OFLE*500,&
OUT*1000

INTEGER ncid, varid, status, grp_ncid
INTEGER ncido, varido
REAL,PARAMETER::FILLVALUE=-9999.9

INTEGER,PARAMETER::IM=1,JM=1
real,ALLOCATABLE,DIMENSION(:,:,:) :: var2d
real::lon(IM),lat(JM)
INTEGER,ALLOCATABLE,DIMENSION(:)::time
CHARACTER(len=100)::VNAME

namelist /para/INDIR,INFLE,ODIR,OFLE,NM,VNAME

read(5,nml=para)

ALLOCATE(var2d(IM,JM,NM), time(NM))

IN=TRIM(INDIR)//TRIM(INFLE)
OUT=TRIM(ODIR)//TRIM(OFLE)

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



print '(A,A)','MMMMM OUTPUT'

status=nf90_create( trim(OUT), NF90_HDF5, ncido)

print '(A,A)','MMMMM DEFINE DIMENSIONS'
status=nf90_def_dim(ncido, 'time', NM, id_time_dim)
status=nf90_def_dim(ncido, 'lon', IM, id_lon_dim)
status=nf90_def_dim(ncido, 'lat', JM, id_lat_dim)

print '(A,A)','MMMMM DEFINE VARIABLES'
status=nf90_def_var(ncido, 'time', NF90_INT, id_time_dim, id_time)
status=nf90_def_var(ncido, 'lat', NF90_REAL, id_lat_dim, id_lat)
status=nf90_def_var(ncido, 'lon', NF90_REAL, id_lon_dim, id_lon)

status=nf90_put_att(ncido, id_lat, 'units','degrees_north')
status=nf90_put_att(ncido, id_lon, 'units','degrees_east')
status=nf90_put_att(ncido, id_time, 'units', 'seconds since 1970-01-01 00:00:00 UTC' )

CALL CHECK( NF90_DEF_VAR(NCIDO, TRIM(VNAME), NF90_REAL, &
     (/ID_LAT_DIM, ID_LON_DIM, ID_TIME_DIM/), ID_VARO))
CALL CHECK( NF90_PUT_ATT(NCIDO, ID_VARO,'units','W/m2') )


print '(A,A)','MMMMM GLOBAL ATTRIBUTES'
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"input_dir", LEN(TRIM(INDIR)),TRIM(INDIR))
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"input_file", LEN(TRIM(INFLE)),TRIM(INFLE))
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"output_dir", LEN(TRIM(ODIR)),TRIM(ODIR))
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"output_file", LEN(TRIM(OFLE)),TRIM(OFLE))

status=nf90_enddef(ncido) 



print '(A,A)','MMMMM WRITE VARIABLES'
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_TIME, time ) )
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_LAT, lat ) )
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_LON, lon ) )
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_VARO, var2d ) )


CALL CHECK(NF90_CLOSE(NCIDO))

PRINT '(A)',"MMMMM OUTPUT FILE: "
PRINT '(A)',TRIM(OUT)

contains
SUBROUTINE CHECK( STATUS )
  INTEGER, INTENT (IN) :: STATUS
  IF(STATUS /= NF90_NOERR) THEN 
    PRINT '(A,A)','EEEEE ERROR ',TRIM(NF90_STRERROR(STATUS))
    STOP "ABNORMAL END"
  END IF
END SUBROUTINE CHECK


END PROGRAM
