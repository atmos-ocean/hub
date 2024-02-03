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
print *,'nf90_open: status=',status
status = nf90_close(ncid)
print *,'nf90_close: status=',status

END PROGRAM IMERG_TEST
