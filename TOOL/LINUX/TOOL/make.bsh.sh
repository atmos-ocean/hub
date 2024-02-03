#!/bin/bash

this_script=$(basename $0)
if [ $# -ne 1 ]; then
  echo ERROR in ${this_script} : Wrong argument.
  echo "Usage: $this_script <filaneme>"
  exit 1
fi

if [ -f $1 ]; then
echo ERROR in ${this_script}: File, $1 exists.
echo
ls -lh --time-style=long-iso $1
echo
exit 1
fi

cat <<EOF>$1
#!/bin/bash

TIMESTAMP=\$(date -R)
HOST=\$(hostname)
CWD=\$(pwd)
COMMAND="\$0 \$@"
LOG=LOG_\$(basename \$0 .sh).log

echo "# "             |tee    \$LOG
echo "# \$TIMESTAMP  " |tee -a \$LOG
echo "# \$HOST       " |tee -a \$LOG
echo "# \$CWD        " |tee -a \$LOG
echo "# \$COMMAND    " |tee -a \$LOG
echo "# "             |tee -a \$LOG

# ls -lh --time-style=long-iso

exit 0
EOF

chmod u+x $1

echo
echo "Created new file, $1"
echo
ls -lh --time-style=long-iso $1
echo
