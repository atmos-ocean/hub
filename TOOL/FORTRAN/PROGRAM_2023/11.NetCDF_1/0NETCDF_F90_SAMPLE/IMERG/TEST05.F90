PROGRAM IMERG_TEST

use netcdf

CHARACTER INDIR*500,INFLE*500,IN*1000,ODIR*500,OFLE*500,&
OUT*1000

INTEGER ncid, varid, status,d, grp_ncid

namelist /para/INDIR,INFLE,ODIR,OFLE

read(5,nml=para)

IN=TRIM(INDIR)//TRIM(INFLE)
OUT=TRIM(ODIR)//TRIM(OFLE)

PRINT '(A)',"MMMMM INPUT : "
PRINT '(A)',TRIM(IN)
PRINT '(A)',"MMMMM OUTPUT: "
PRINT '(A)',TRIM(OUT)

status = nf90_open(IN, nf90_nowrite, ncid)

status = nf90_inq_grp_ncid(ncid, "Grid", grp_ncid)
print *,"grp_ncid", grp_ncid
status = nf90_inq_varid(grp_ncid, "lon", varid)
print *,"lon varid", varid
status = nf90_inq_varid(grp_ncid, "lat", varid)
print *,"lat varid", varid
status = nf90_inq_varid(grp_ncid, "precipitationCal", varid)
print *,"precipitationCal varid", varid
status = nf90_close(ncid)

print *,"ncdump -h "//TRIM(IN)
CALL SYSTEM("ncdump -h "//TRIM(IN))
print *

END PROGRAM IMERG_TEST
