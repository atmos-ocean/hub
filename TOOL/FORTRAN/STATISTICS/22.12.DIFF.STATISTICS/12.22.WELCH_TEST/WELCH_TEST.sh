SRC=WELCH_TEST.F90
SUB="SUB_WELCH.F"

EXE=$(basename $SRC .F90).EXE

rm -vf $EXE
ifort -traceback -CB $SUB $SRC -o $EXE
if [ ! -f $EXE ];then echo NO SUCH FILE $EXE; exit 1;fi

$EXE

#SUB="DBETAI.F DLBETA.F XERMSG.F D9LGMC.F DLNREL.F DLNGAM.F \
#DGAMMA.F J4SAVE.F XERPRN.F XERSVE.F XERHLT.F XERCNT.F \
#DCSEVL.F D1MACH.F FDUMP.F INITDS.F DGAMLM.F XGETUA.F \
#I1MACH.F SETERU.F XERROR.F SETERR.F I8SAVE.F E9RINT.F \
#EPRINT.F S88FMT.F"
