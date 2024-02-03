PROGRAM IMERG_TEST

use netcdf

CHARACTER INDIR*500,INFLE*500,IN*1000,ODIR*500,OFLE*500,&
OUT*1000

INTEGER ncid, varid, status,d, grp_ncid

INTEGER,PARAMETER::IM=3600,JM=1800,NM=1
real,DIMENSION(IM,JM,NM) :: var3d
real,DIMENSION(IM,JM) :: var2d
real::lon(IM),lat(JM)
INTEGER::time(NM)

namelist /para/INDIR,INFLE,ODIR,OFLE

read(5,nml=para)

IN=TRIM(INDIR)//TRIM(INFLE)
OUT=TRIM(ODIR)//TRIM(OFLE)

PRINT '(A)',"MMMMM INPUT : "
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

status = nf90_inq_varid(grp_ncid, "precipitationCal", varid)
print *,"precipitationCal varid", varid
status = nf90_get_var(grp_ncid, varid, var3d)

status = nf90_close(ncid)

PRINT '(A)',"MMMMM OUTPUT: "
PRINT '(A)',TRIM(OUT)

END PROGRAM IMERG_TEST
