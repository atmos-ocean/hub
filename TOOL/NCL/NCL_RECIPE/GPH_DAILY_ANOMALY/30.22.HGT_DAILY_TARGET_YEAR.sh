
INFLE=hgt_2018_300.nc
OFLE=$(basename $INFLE .nc)_DAILY.nc
echo $INFLE
cdo daymean $INFLE $OFLE
echo $OFLE
cdo sinfo $OFLE
echo
echo INPUT: $INFLE
echo OUTPUT: $OFLE


INFLE=hgt_2018_200.nc
OFLE=$(basename $INFLE .nc)_DAILY.nc
echo $INFLE
cdo daymean $INFLE $OFLE
echo $OFLE
cdo sinfo $OFLE
echo
echo INPUT: $INFLE
echo OUTPUT: $OFLE
