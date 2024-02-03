
INFLE=AIR_2018_925.nc
OFLE=$(basename $INFLE .nc)_DAILY.nc
echo $INFLE
cdo daymean $INFLE $OFLE
echo $OFLE
cdo sinfo $OFLE
echo
echo INPUT: $INFLE
echo OUTPUT: $OFLE

<<COMMENT
INFLE=HGT_1981-2017_200.nc
OFLE=$(basename $INFLE .nc)_DAILY.nc
echo $INFLE
cdo daymean $INFLE $OFLE
echo $OFLE
cdo sinfo $OFLE
echo



INFLE=AIR_1981-2017_925.nc
OFLE=$(basename $INFLE .nc)_DAILY.nc

echo $INFLE
cdo daymean $INFLE $OFLE
echo $OFLE
cdo sinfo $OFLE
echo
COMMENT
