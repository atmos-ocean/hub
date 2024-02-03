PROGRAM IMERG_TEST

use netcdf

CHARACTER INDIR*500,INFLE*500,IN*1000,ODIR*500,OFLE*500,&
OUT*1000

INTEGER ncid, varid, status, grp_ncid
INTEGER ncido, varido
REAL,PARAMETER::FILLVALUE=-9999.9

INTEGER,PARAMETER::IM=3600,JM=1800,NM=1
real,DIMENSION(JM,IM,NM) :: var3d
real,DIMENSION(IM,JM) :: var2d
real::lon(IM),lat(JM)
INTEGER::time(NM)

namelist /para/INDIR,INFLE,ODIR,OFLE

read(5,nml=para)

IN=TRIM(INDIR)//TRIM(INFLE)
OUT=TRIM(ODIR)//TRIM(OFLE)

PRINT '(A)',"MMMMM INPUT FILE : "
PRINT '(A)',TRIM(IN)

status = nf90_open(IN, nf90_nowrite, ncid)

status = nf90_inq_grp_ncid(ncid, "Grid", grp_ncid)
print *,"grp_ncid", grp_ncid

status = nf90_inq_varid(grp_ncid, "time", varid)
print *,"time varid", varid
status = nf90_get_var(grp_ncid, varid, time)

status = nf90_inq_varid(grp_ncid, "lon", varid)
print *,"lon varid", varid
status = nf90_get_var(grp_ncid, varid, lon)

status = nf90_inq_varid(grp_ncid, "lat", varid)
print *,"lat varid", varid
status = nf90_get_var(grp_ncid, varid, lat)

CALL CHECK( nf90_inq_varid(grp_ncid, "precipitationCal", varid) )
print *,"precipitationCal varid", varid
CALL CHECK( nf90_get_var(grp_ncid, varid, var3d) )
print *,'MIN=',minval(var3d)
print *,'MAX=',maxval(var3d)
print *,'SUM=',sum(var3d)

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

CALL CHECK( NF90_DEF_VAR(NCIDO, "precipitationCal", NF90_REAL, &
     (/ID_LAT_DIM, ID_LON_DIM, ID_TIME_DIM/), ID_VARO))
CALL CHECK( NF90_PUT_ATT(NCIDO, ID_VARO,'units','mm/h') )


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
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_VARO, var3d ) )


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

END PROGRAM IMERG_TEST



