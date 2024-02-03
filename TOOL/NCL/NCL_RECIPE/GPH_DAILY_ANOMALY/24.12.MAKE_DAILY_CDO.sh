INFLE=HGT_1981-2017_300.nc
OFLE=$(basename $INFLE .nc)_DAILY.nc
echo $INFLE
cdo daymean $INFLE $OFLE
echo $OFLE
cdo sinfo $OFLE
echo

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
