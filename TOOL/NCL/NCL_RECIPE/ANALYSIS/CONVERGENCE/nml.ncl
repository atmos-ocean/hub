begin

wallClock1 = systemfunc("date -R") ; retrieve wall clock time

scriptname_in = getenv("NCL_ARG_1")
scriptname=systemfunc("basename " + scriptname_in + " .ncl")
nml    = getenv("NCL_ARG_2")

print("script name ="+scriptname_in)
print("namelist    ="+nml)
print("")

print("READ NAMELIST FILE")

runname=systemfunc("grep runname "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
indir=systemfunc("grep indir "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
domain=systemfunc("grep domain "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
wrfout=systemfunc("grep wrfout "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
sdatetime=systemfunc("grep sdatetime "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
edatetime=systemfunc("grep edatetime "+nml+ "|cut -f2 -d'='|cut -f1 -d','")

print("DONE.")
print("")

wallClock2 = systemfunc("date -R") ; retrieve wall clock time


print("CHECK NAMELIST FILE")

print("runname:   "+runname)
print("indir:     "+indir)
print("wrfout:    "+wrfout )
print("sdatetime: "+sdatetime)
print("edatetime: "+edatetime)
print("")

print("DONE "+scriptname_in+".")
print("")
print("STARTED : "+wallClock1)
print("ENDED   : "+wallClock2)
print("")

end
