! /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/12.14.TEST_JOF3_1PNT/32.10.TEST_READ_1PNT
! 7.TEST_READ_SIMPLIFY.F90
!/work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/22.12.DECOMP_GLOBE/32.13.DECOMP_GLOBE_COARE30_ALL_RUN
!COR3.0_DECOMP_GLOBE_NetCDF.F90
SUBROUTINE READ_NC1FLT(VNAME, IN, ND, var1d)
  USE netcdf
CHARACTER(LEN=*),INTENT(IN)::VNAME
CHARACTER(LEN=*),INTENT(IN)::IN
INTEGER,INTENT(IN)::ND
real,INTENT(INOUT)::var1d(ND)
INTEGER ncid, varid, status

!PRINT '(A)',"MMMMM INPUT FILE : "
!PRINT '(A)',TRIM(IN)

CALL CHECK( nf90_open(IN, nf90_nowrite, ncid) )

CALL CHECK( nf90_inq_varid(ncid, TRIM(VNAME), varid) )
!print *,TRIM(VNAME), varid
CALL CHECK( nf90_get_var(ncid, varid, var1d) )
!print *,'MIN=',minval(var1d)
!print *,'MAX=',maxval(var1d)

status = nf90_close(ncid)
print *
END SUBROUTINE READ_NC1FLT



SUBROUTINE READ_NC1DBL(VNAME, IN, ND, var1d)
  USE netcdf
CHARACTER(LEN=*),INTENT(IN)::VNAME
CHARACTER(LEN=*),INTENT(IN)::IN
INTEGER,INTENT(IN)::ND
real(8),INTENT(INOUT)::var1d(ND)
INTEGER ncid, varid, status

!PRINT '(A)',"MMMMM INPUT FILE : "
!PRINT '(A)',TRIM(IN)

CALL CHECK( nf90_open(IN, nf90_nowrite, ncid) )

CALL CHECK( nf90_inq_varid(ncid, TRIM(VNAME), varid) )
!print *,TRIM(VNAME), varid
CALL CHECK( nf90_get_var(ncid, varid, var1d) )
!print *,'MIN=',minval(var1d)
!print *,'MAX=',maxval(var1d)

status = nf90_close(ncid)
print *
END SUBROUTINE READ_NC1DBL



SUBROUTINE READ_NC2(VNAME, IN, IM,JM,NM,var2d)
  USE netcdf
CHARACTER(LEN=*),INTENT(IN)::VNAME
CHARACTER(LEN=*),INTENT(IN)::IN
INTEGER,INTENT(IN)::IM,JM,NM
real,INTENT(INOUT)::var2d(IM,JM,NM)
INTEGER ncid, varid, status

PRINT '(A,A,A)',"MMMMM ",TRIM(VNAME)," INPUT FILE : "
PRINT '(A)',TRIM(IN)

CALL CHECK( nf90_open(IN, nf90_nowrite, ncid) )

!print *,"time"
!CALL CHECK( nf90_inq_varid(ncid, "time", varid) )
!CALL CHECK( nf90_get_var(ncid, varid, time) )

!print *,"longitude"
!CALL CHECK( nf90_inq_varid(ncid, "lon", varid) )
!CALL CHECK(  nf90_get_var(ncid, varid, lon) )

!print *,"latitude"
!CALL CHECK( nf90_inq_varid(ncid, "lat", varid) )
!CALL CHECK( nf90_get_var(ncid, varid, lat) )

!print *,'VNAME=',TRIM(VNAME)
CALL CHECK( nf90_inq_varid(ncid, TRIM(VNAME), varid) )
!print *,TRIM(VNAME),' varid=',varid
CALL CHECK( nf90_get_var(ncid, varid, var2d) )

!print *,'MIN=',minval(var2d); print *,'MAX=',maxval(var2d)

status = nf90_close(ncid)
!print *
END SUBROUTINE READ_NC2



SUBROUTINE READ_NC2_CLM(VNAME, IN, IM,JM,NM,var2d)
  USE netcdf
CHARACTER(LEN=*),INTENT(IN)::VNAME
CHARACTER(LEN=*),INTENT(IN)::IN
INTEGER,INTENT(IN)::IM,JM,NM
real,INTENT(INOUT)::var2d(IM,JM,NM)
INTEGER ncid, varid, status

PRINT '(A,A,A)',"MMMMM ",TRIM(VNAME)," INPUT FILE : "
PRINT '(A)',TRIM(IN)

CALL CHECK( nf90_open(IN, nf90_nowrite, ncid) )

CALL CHECK( nf90_inq_varid(ncid, "year_day", varid) )
!print *,"year_day varid", varid
CALL CHECK( nf90_get_var(ncid, varid, year_day) )

CALL CHECK( nf90_inq_varid(ncid, "longitude", varid) )
!print *,"longitude varid", varid
CALL CHECK(  nf90_get_var(ncid, varid, longitude) )

CALL CHECK( nf90_inq_varid(ncid, "latitude", varid) )
!print *,"latitude varid", varid
CALL CHECK( nf90_get_var(ncid, varid, latitude) )

CALL CHECK( nf90_inq_varid(ncid, TRIM(VNAME), varid) )
! print *,TRIM(VNAME), ' varid=', varid
CALL CHECK( nf90_get_var(ncid, varid, var2d) )

!print *,'MIN=',minval(var2d); print *,'MAX=',maxval(var2d)

status = nf90_close(ncid)
print *
END SUBROUTINE READ_NC2_CLM



SUBROUTINE WRITE_NC(VNAME, OUT, IM, JM, NM, var2d, time, lat, lon, &
INDIR, INFLE, ODIR, OFLE)
  USE netcdf
CHARACTER(LEN=*),INTENT(IN)::VNAME
CHARACTER(LEN=*),INTENT(IN)::OUT, INDIR, INFLE, ODIR, OFLE
INTEGER,INTENT(IN)::IM,JM,NM
real,INTENT(IN)::var2d(IM,JM,NM)
real,INTENT(IN)::time(NM)
real(8),INTENT(IN)::lat(JM),lon(IM)
INTEGER ncido, varido, status

status=nf90_create( trim(OUT), NF90_HDF5, ncido)

!print '(A,3I5)','MMMMM DEFINE DIMENSIONS IM,JM,NM=',IM,JM,NM
!PRINT '(a)','TIME'
status=nf90_def_dim(ncido, 'time', NM, id_time_dim)
!PRINT '(a)','LAT'
status=nf90_def_dim(ncido, 'lat',  JM, id_lat_dim)
!PRINT '(a)','LON'
status=nf90_def_dim(ncido, 'lon',  IM, id_lon_dim)


!print '(A,3I5)','MMMMM DEFINE VARIABLES IM,JM,NM=',IM,JM,NM
!PRINT '(a)','TIME'
status=nf90_def_var(ncido, 'time', NF90_FLOAT, id_time_dim, id_time)
!PRINT '(a)','LAT'
status=nf90_def_var(ncido, 'lat', NF90_DOUBLE, id_lat_dim, id_lat)
!PRINT '(a)','LON'
status=nf90_def_var(ncido, 'lon', NF90_DOUBLE, id_lon_dim, id_lon)

!print '(A,3I5)','MMMMM DEFINE UNITS IM,JM,NM=',IM,JM,NM
!PRINT '(a)','TIME'
status=nf90_put_att(ncido, id_time, 'units','hours since 1800-01-01 00:00')
!PRINT '(a)','LAT'
status=nf90_put_att(ncido, id_lat, 'units','degrees_north')
!PRINT '(a)','LON'
status=nf90_put_att(ncido, id_lon, 'units','degrees_east')

!PRINT '(a,3I5)','MMMMM DEF_VAR OUT IM,JM,NM=',IM,JM,NM
CALL CHECK( NF90_DEF_VAR(NCIDO, TRIM(VNAME), NF90_REAL, &
     (/ID_LON_DIM, ID_LAT_DIM, ID_TIME_DIM/), ID_VARO))
!CALL CHECK( NF90_DEF_VAR(NCIDO, TRIM(VNAME), NF90_REAL, &
!     (/ID_LAT_DIM, ID_LON_DIM, ID_TIME_DIM/), ID_VARO))
!PRINT '(a)','UNITS'
CALL CHECK( NF90_PUT_ATT(NCIDO, ID_VARO,'units','W/m2') )
!PRINT '(a)','FILLVALUE'
CALL CHECK( NF90_PUT_ATT(NCIDO, ID_VARO,'_FillValue', -9999.) )
!PRINT '(a)','MISSING'
CALL CHECK( NF90_PUT_ATT(NCIDO, ID_VARO,'missing_value', -9999.) )

!print '(A,A,3I5)','MMMMM GLOBAL ATTRIBUTES IM,JM,NM=',IM,JM,NM
!PRINT '(a)','INPUT_DIR'
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"input_dir", LEN(TRIM(INDIR)),TRIM(INDIR))
!PRINT '(a)','INPUT_FILE'
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"input_file", LEN(TRIM(INFLE)),TRIM(INFLE))
!PRINT '(a)','OUTPUT_DIR'
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"output_dir", LEN(TRIM(ODIR)),TRIM(ODIR))
!PRINT '(a)','OUTPUT_FILE'
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"output_file", LEN(TRIM(OFLE)),TRIM(OFLE))

status=nf90_enddef(ncido) 



!print '(A,3I5)','MMMMM WRITE VARIABLES IM,JM,NM=',IM,JM,NM
!PRINT '(a)','TIME'
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_TIME, time ) )
!PRINT '(a)','LAT'
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_LAT, lat ) )
!PRINT '(a)','LON'
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_LON, lon ) )
!PRINT '(a)','VAR2D'
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_VARO, var2d ) )

CALL CHECK(NF90_CLOSE(NCIDO))

!PRINT '(A)',"MMMMM OUTPUT FILE: "
!PRINT '(A)',TRIM(OUT)

END SUBROUTINE WRITE_NC



SUBROUTINE WRITE_NC_ALL(NVAR, NAME_OUT, OUT, IM, JM, NM, VAR_OUT, time, lat, lon, &
INDIR, INFLE, ODIR, OFLE)
USE netcdf
CHARACTER(LEN=*),INTENT(IN)::NAME_OUT(NVAR)
CHARACTER(LEN=*),INTENT(IN)::OUT, INDIR, INFLE, ODIR, OFLE
INTEGER,INTENT(IN)::IM,JM,NM
real,INTENT(IN)::VAR_OUT(NVAR,IM,JM,NM)
real,INTENT(IN)::time(NM)
real(8),INTENT(IN)::lat(JM),lon(IM)
INTEGER ncido, varido, status
INTEGER::ID_VARO(NVAR)

REAL::VAR_WRITE(IM,JM,NM)

status=nf90_create( trim(OUT), NF90_HDF5, ncido)

status=nf90_def_dim(ncido, 'time', NM, id_time_dim)
!PRINT '(a)','LAT'
status=nf90_def_dim(ncido, 'lat',  JM, id_lat_dim)
!PRINT '(a)','LON'
status=nf90_def_dim(ncido, 'lon',  IM, id_lon_dim)


!print '(A,3I5)','MMMMM DEFINE VARIABLES IM,JM,NM=',IM,JM,NM
!PRINT '(a)','TIME'
status=nf90_def_var(ncido, 'time', NF90_FLOAT, id_time_dim, id_time)
!PRINT '(a)','LAT'
status=nf90_def_var(ncido, 'lat', NF90_DOUBLE, id_lat_dim, id_lat)
!PRINT '(a)','LON'
status=nf90_def_var(ncido, 'lon', NF90_DOUBLE, id_lon_dim, id_lon)

!print '(A,3I5)','MMMMM DEFINE UNITS IM,JM,NM=',IM,JM,NM
!PRINT '(a)','TIME'
status=nf90_put_att(ncido, id_time, 'units','hours since 1800-01-01 00:00')
!PRINT '(a)','LAT'
status=nf90_put_att(ncido, id_lat, 'units','degrees_north')
!PRINT '(a)','LON'
status=nf90_put_att(ncido, id_lon, 'units','degrees_east')


DO N=1,NVAR
!PRINT '(a,3I5)','MMMMM DEF_VAR OUT IM,JM,NM=',IM,JM,NM
CALL CHECK( NF90_DEF_VAR(NCIDO, TRIM(NAME_OUT(N)), NF90_REAL, &
     (/ID_LON_DIM, ID_LAT_DIM, ID_TIME_DIM/), ID_VARO(N) ))
!CALL CHECK( NF90_DEF_VAR(NCIDO, TRIM(VNAME), NF90_REAL, &
!     (/ID_LAT_DIM, ID_LON_DIM, ID_TIME_DIM/), ID_VARO))
!PRINT '(a)','UNITS'
CALL CHECK( NF90_PUT_ATT(NCIDO, ID_VARO(N) ,'units','W/m2') )
!PRINT '(a)','FILLVALUE'
CALL CHECK( NF90_PUT_ATT(NCIDO, ID_VARO(N) ,'_FillValue', -9999.) )
!PRINT '(a)','MISSING'
CALL CHECK( NF90_PUT_ATT(NCIDO, ID_VARO(N) ,'missing_value', -9999.) )
END DO !N

!print '(A,A,3I5)','MMMMM GLOBAL ATTRIBUTES IM,JM,NM=',IM,JM,NM
!PRINT '(a)','INPUT_DIR'
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"input_dir", LEN(TRIM(INDIR)),TRIM(INDIR))
!PRINT '(a)','INPUT_FILE'
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"input_file", LEN(TRIM(INFLE)),TRIM(INFLE))
!PRINT '(a)','OUTPUT_DIR'
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"output_dir", LEN(TRIM(ODIR)),TRIM(ODIR))
!PRINT '(a)','OUTPUT_FILE'
STATUS=NF_PUT_ATT_TEXT(NCIDO,NF_GLOBAL,"output_file", LEN(TRIM(OFLE)),TRIM(OFLE))

status=nf90_enddef(ncido) 



!print '(A,3I5)','MMMMM WRITE VARIABLES IM,JM,NM=',IM,JM,NM
!PRINT '(a)','TIME'
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_TIME, time ) )
!PRINT '(a)','LAT'
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_LAT, lat ) )
!PRINT '(a)','LON'
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_LON, lon ) )

DO N=1,NVAR
PRINT '(a,a)','WRITING ',TRIM(NAME_OUT(N))
VAR_WRITE=0.0
VAR_WRITE(:,:,:)=VAR_OUT(N,:,:,:)
CALL CHECK( NF90_PUT_VAR(NCIDO, ID_VARO(N), VAR_WRITE ) )
END DO !N

CALL CHECK(NF90_CLOSE(NCIDO))

!PRINT '(A)',"MMMMM OUTPUT FILE: "
!PRINT '(A)',TRIM(OUT)

END SUBROUTINE WRITE_NC_ALL



SUBROUTINE CHECK( STATUS )
  USE netcdf
  INTEGER, INTENT (IN) :: STATUS
  IF(STATUS /= NF90_NOERR) THEN 
    PRINT '(A,A)','EEEEE ERROR ',TRIM(NF90_STRERROR(STATUS))
    STOP "ABNORMAL END"
  END IF
END SUBROUTINE CHECK

