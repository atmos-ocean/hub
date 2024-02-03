begin

wallClock1 = systemfunc("date -R") ; retrieve wall clock time

scriptname_in = getenv("NCL_ARG_1")
scriptname=systemfunc("basename " + scriptname_in + " .ncl")
arg    = getenv("NCL_ARG_2")

print("script name ="+scriptname_in)
print("ARG         ="+arg)
print("")


wallClock2 = systemfunc("date -R") ; retrieve wall clock time

print("DONE "+scriptname_in+".")
print("")
print("STARTED : "+wallClock1)
print("ENDED   : "+wallClock2)
print("")

end
