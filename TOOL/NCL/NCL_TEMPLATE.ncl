; ${now}
; ${cwd}
;
script_name  = get_script_name()
LOG=systemfunc("basename "+script_name+ " .ncl")+".LOG"

NOW=systemfunc("date '+%y%m%d_%H%M' ")
;print("NOW="+NOW)
; LOG=systemfunc("basename "+script_name+ " .ncl")+NOW+".LOG"

NOW=systemfunc("date -R")
HOST=systemfunc("hostname")
CWD=systemfunc("pwd")

print("script="+script_name)

FINFO=systemfunc("ls -lh --time-style=long-iso "+script_name)

LOG_HEADER = (/\
"# "+ NOW, \
"# "+ HOST, \
"# "+ CWD, \
"# "+ FINFO \
/)

hlist=[/LOG_HEADER/]

write_table(LOG, "w", hlist, "%s")

print("")
print("Done " + script_name)
print("")
print("LOG: "+LOG)
print("")
