external wrapit_lib "./wrapit_lib.so"

begin

text="HELLO!"
arg=5.0

wrapit_lib::F77SUB(text,arg)

end
