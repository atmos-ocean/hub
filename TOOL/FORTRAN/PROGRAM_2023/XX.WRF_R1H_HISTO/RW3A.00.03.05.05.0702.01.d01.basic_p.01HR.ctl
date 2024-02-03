* /work00/DATA/HD01/RW3A.ARWpost.DAT/basic_p/ARWpost_RW3A.00.03.05.05.0702.01/RW3A.00.03.05.05.0702.01.d01.basic_p.01HR.ctl
dset ^RW3A.00.03.05.05.0702.01.d01.basic_p.01HR_%y4-%m2-%d2_%h2:%n2.dat
options  byteswapped template
undef 1.e30
title  OUTPUT FROM WRF V4.1.5 MODEL
pdef  599 599 lcc  27.000  130.500  300.000  300.000  32.00000  27.00000  130.50000   3000.000   3000.000
xdef 1469 linear  120.56867   0.01351351
ydef 1231 linear   18.56023   0.01351351
zdef   30 levels  
1000.00000
 990.00000
 980.00000
 970.00000
 960.00000
 950.00000
 940.00000
 930.00000
 920.00000
 910.00000
 900.00000
 880.00000
 860.00000
 840.00000
 820.00000
 800.00000
 750.00000
 700.00000
 650.00000
 600.00000
 550.00000
 500.00000
 450.00000
 400.00000
 350.00000
 300.00000
 250.00000
 200.00000
 150.00000
 100.00000
tdef   73 linear 00Z12AUG2021      60MN      
VARS   30
U             30  0  x-wind component (m s-1)
V             30  0  y-wind component (m s-1)
W             30  0  z-wind component (m s-1)
Q2             1  0  QV at 2 M (kg kg-1)
T2             1  0  TEMP at 2 M (K)
U10            1  0  U at 10 M (m s-1)
V10            1  0  V at 10 M (m s-1)
QVAPOR        30  0  Water vapor mixing ratio (kg kg-1)
QCLOUD        30  0  Cloud water mixing ratio (kg kg-1)
QRAIN         30  0  Rain water mixing ratio (kg kg-1)
HGT            1  0  Terrain Height (m)
RAINC          1  0  ACCUMULATED TOTAL CUMULUS PRECIPITATION (mm)
RAINRC         1  0  RAIN RATE CONV (mm per output interval)
RAINNC         1  0  ACCUMULATED TOTAL GRID SCALE PRECIPITATION (mm)
RAINRNC        1  0  RAIN RATE NON-CONV (mm per output interval)
XLAND          1  0  LAND MASK (1 FOR LAND, 2 FOR WATER) (-)
PBLH           1  0  PBL HEIGHT (m)
HFX            1  0  UPWARD HEAT FLUX AT THE SURFACE (W m-2)
QFX            1  0  UPWARD MOISTURE FLUX AT THE SURFACE (kg m-2 s-1)
LH             1  0  LATENT HEAT FLUX AT THE SURFACE (W m-2)
SST            1  0  SEA SURFACE TEMPERATURE (K)
ept           30  0  Equivalent Potential Temperature (K)
sept          30  0  Saturated Equivalent Potential Temperature (K)
pressure      30  0  Model pressure (hPa)
height        30  0  Model height (km)
tk            30  0  Temperature (K)
theta         30  0  Potential Temperature (K)
rh            30  0  Relative Humidity (%)
slp            1  0  Sea Levelp Pressure (hPa)
dbz           30  0  Reflectivity (-)
ENDVARS
@ global String comment TITLE =  OUTPUT FROM WRF V4.1.5 MODEL
@ global String comment START_DATE = 2021-08-10_12:00:00
@ global String comment SIMULATION_START_DATE = 2021-08-10_12:00:00
@ global String comment WEST-EAST_GRID_DIMENSION =   600
@ global String comment SOUTH-NORTH_GRID_DIMENSION =   600
@ global String comment BOTTOM-TOP_GRID_DIMENSION =    50
@ global String comment DX =      3000.00
@ global String comment DY =      3000.00
@ global String comment AERCU_OPT =     0
@ global String comment AERCU_FCT =         1.00
@ global String comment IDEAL_CASE =     0
@ global String comment DIFF_6TH_SLOPEOPT =     0
@ global String comment AUTO_LEVELS_OPT =     2
@ global String comment DIFF_6TH_THRESH =         0.10
@ global String comment DZBOT =        50.00
@ global String comment DZSTRETCH_S =         1.30
@ global String comment DZSTRETCH_U =         1.10
@ global String comment SKEBS_ON =     0
@ global String comment SPEC_BDY_FINAL_MU =     1
@ global String comment USE_Q_DIABATIC =     0
@ global String comment GRIDTYPE = C
@ global String comment DIFF_OPT =     1
@ global String comment KM_OPT =     4
@ global String comment DAMP_OPT =     0
@ global String comment DAMPCOEF =         0.20
@ global String comment KHDIF =         0.00
@ global String comment KVDIF =         0.00
@ global String comment MP_PHYSICS =     5
@ global String comment RA_LW_PHYSICS =     1
@ global String comment RA_SW_PHYSICS =     1
@ global String comment SF_SFCLAY_PHYSICS =     5
@ global String comment SF_SURFACE_PHYSICS =     2
@ global String comment BL_PBL_PHYSICS =     5
@ global String comment CU_PHYSICS =     0
@ global String comment SF_LAKE_PHYSICS =     0
@ global String comment SURFACE_INPUT_SOURCE =     1
@ global String comment SST_UPDATE =     0
@ global String comment GRID_FDDA =     0
@ global String comment GFDDA_INTERVAL_M =     0
@ global String comment GFDDA_END_H =     0
@ global String comment GRID_SFDDA =     0
@ global String comment SGFDDA_INTERVAL_M =     0
@ global String comment SGFDDA_END_H =     0
@ global String comment HYPSOMETRIC_OPT =     2
@ global String comment USE_THETA_M =     1
@ global String comment GWD_OPT =     1
@ global String comment SF_URBAN_PHYSICS =     0
@ global String comment SF_SURFACE_MOSAIC =     0
@ global String comment SF_OCEAN_PHYSICS =     0
@ global String comment SHCU_PHYSICS =     0
@ global String comment MFSHCONV =     0
@ global String comment FEEDBACK =     0
@ global String comment SMOOTH_OPTION =     0
@ global String comment SWRAD_SCAT =         1.00
@ global String comment W_DAMPING =     0
@ global String comment DT =        10.00
@ global String comment RADT =        30.00
@ global String comment BLDT =         0.00
@ global String comment CUDT =         5.00
@ global String comment AER_OPT =     0
@ global String comment SWINT_OPT =     0
@ global String comment AER_TYPE =     1
@ global String comment AER_AOD550_OPT =     1
@ global String comment AER_ANGEXP_OPT =     1
@ global String comment AER_SSA_OPT =     1
@ global String comment AER_ASY_OPT =     1
@ global String comment AER_AOD550_VAL =         0.12
@ global String comment AER_ANGEXP_VAL =         1.30
@ global String comment AER_SSA_VAL =         0.85
@ global String comment AER_ASY_VAL =         0.90
@ global String comment MOIST_ADV_OPT =     1
@ global String comment SCALAR_ADV_OPT =     1
@ global String comment TKE_ADV_OPT =     1
@ global String comment DIFF_6TH_OPT =     0
@ global String comment DIFF_6TH_FACTOR =         0.12
@ global String comment OBS_NUDGE_OPT =     0
@ global String comment BUCKET_MM =        -1.00
@ global String comment BUCKET_J =        -1.00
@ global String comment PREC_ACC_DT =         0.00
@ global String comment ISFTCFLX =     0
@ global String comment ISHALLOW =     0
@ global String comment ISFFLX =     1
@ global String comment ICLOUD =     1
@ global String comment ICLOUD_CU =     0
@ global String comment TRACER_PBLMIX =     1
@ global String comment SCALAR_PBLMIX =     0
@ global String comment YSU_TOPDOWN_PBLMIX =     0
@ global String comment GRAV_SETTLING =     0
@ global String comment DFI_OPT =     0
@ global String comment SIMULATION_INITIALIZATION_TYPE = REAL-DATA CASE
@ global String comment WEST-EAST_PATCH_START_UNSTAG =     1
@ global String comment WEST-EAST_PATCH_END_UNSTAG =   599
@ global String comment WEST-EAST_PATCH_START_STAG =     1
@ global String comment WEST-EAST_PATCH_END_STAG =   600
@ global String comment SOUTH-NORTH_PATCH_START_UNSTAG =     1
@ global String comment SOUTH-NORTH_PATCH_END_UNSTAG =   599
@ global String comment SOUTH-NORTH_PATCH_START_STAG =     1
@ global String comment SOUTH-NORTH_PATCH_END_STAG =   600
@ global String comment BOTTOM-TOP_PATCH_START_UNSTAG =     1
@ global String comment BOTTOM-TOP_PATCH_END_UNSTAG =    49
@ global String comment BOTTOM-TOP_PATCH_START_STAG =     1
@ global String comment BOTTOM-TOP_PATCH_END_STAG =    50
@ global String comment GRID_ID =     1
@ global String comment PARENT_ID =     0
@ global String comment I_PARENT_START =     1
@ global String comment J_PARENT_START =     1
@ global String comment PARENT_GRID_RATIO =     1
@ global String comment CEN_LAT =        27.00
@ global String comment CEN_LON =       130.50
@ global String comment TRUELAT1 =        27.00
@ global String comment TRUELAT2 =        32.00
@ global String comment MOAD_CEN_LAT =        27.00
@ global String comment STAND_LON =       130.50
@ global String comment POLE_LAT =        90.00
@ global String comment POLE_LON =         0.00
@ global String comment GMT =        12.00
@ global String comment JULYR =  2021
@ global String comment JULDAY =   222
@ global String comment MAP_PROJ =     1
@ global String comment MAP_PROJ_CHAR = Lambert Conformal
@ global String comment MMINLU = MODIFIED_IGBP_MODIS_NOAH
@ global String comment NUM_LAND_CAT =    21
@ global String comment ISWATER =    17
@ global String comment ISLAKE =    21
@ global String comment ISICE =    15
@ global String comment ISURBAN =    13
@ global String comment ISOILWATER =    14
@ global String comment HYBRID_OPT =     2
@ global String comment ETAC =         0.20
