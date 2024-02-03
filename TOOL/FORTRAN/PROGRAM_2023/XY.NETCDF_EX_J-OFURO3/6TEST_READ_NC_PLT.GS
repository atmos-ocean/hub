
'sdfopen /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/22.12.DECOMP_GLOBE/42.12.FRACTIONAL_CONTRIBUTION/./6TEST_READ_NC_LHF_RAW_RUN_1988.nc';'sdfopen /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/22.12.DECOMP_GLOBE/42.12.FRACTIONAL_CONTRIBUTION/./6TEST_READ_NC_LHF_CLM_RUN_1988.nc';'sdfopen /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/22.12.DECOMP_GLOBE/42.12.FRACTIONAL_CONTRIBUTION/./6TEST_READ_NC_LHF_SST_RUN_1988.nc';
'sdfopen /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/22.12.DECOMP_GLOBE/42.12.FRACTIONAL_CONTRIBUTION/./6TEST_READ_NC_LHF_WND_RUN_1988.nc';'sdfopen /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/22.12.DECOMP_GLOBE/42.12.FRACTIONAL_CONTRIBUTION/./6TEST_READ_NC_LHF_QA_RUN_1988.nc';

xmax = 2; ymax = 3;nmax=5


'set vpage 0.0 8.5 0.0 11'

ytop=9
xwid = 5.0/xmax; ywid = 6.0/ymax
xmargin=0.5; ymargin=0.5

'cc'

nmap = 1;ymap = 1
while (ymap <= ymax)
xmap = 1
while (xmap <= xmax)

xs = 1 + (xwid+xmargin)*(xmap-1); xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1); ys = ye - ywid

# SET PAGE
'set parea 'xs ' 'xe' 'ys' 'ye

# SET COLOR BAR
'color -80 80 10 -kind midnightblue->blue->deepskyblue->lightcyan->yellow->orange->red->crimson -gxout shaded'

# 'set lon  '; 'set lat  '
# 'set lev '
'set time 00Z10AUG1988'
'set xlopts 1 3 0.07';'set ylopts 1 3 0.07';
'd LHF.'nmap

# GET COORDINATES OF 4 CORNERS
'q gxinfo'
line3=sublin(result,3);xl=subwrd(line3,4); xr=subwrd(line3,6);
line4=sublin(result,4);yb=subwrd(line4,4);yt=subwrd(line4,6)

if(nmap=1);title='RAW';endif
if(nmap=2);title='CLM';endif
if(nmap=3);title='SST';endif
if(nmap=4);title='WND';endif
if(nmap=5);title='QA';endif

x=(xl+xr)/2; y=yt+0.2
'set strsiz 0.12 0.15'; 'set string 1 c 3 0'
'draw string 'x' 'y' 'title

xmap=xmap+1; nmap=nmap+1
if(nmap >= nmax);break;endif
endwhile ;#x
ymap=ymap+1
endwhile ;#y

# LEGEND COLOR BAR
x1=1; x2=7; y1=yb-1; y2=y1+0.1
'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.1 -fh 0.13 -fs 2 -ft 3 -line on -edge circle'
x=x2+0.5; y=y1
'set strsiz 0.12 0.15'; 'set string 1 l 3 0'
'draw string 'x' 'y' W/m2'


# TEXT
x=4; y=10
'set strsiz 0.12 0.15'; 'set string 1 c 3 0'
'draw string 'x' 'y' J-OFURO3 V1.2_PRE LHF 10 AUG 1988'

# HEADER
'set strsiz 0.1 0.12'; 'set string 1 l 3 0'
xx = 0.1; yy=10
'draw string ' xx ' ' yy ' '
yy = yy+0.2
'draw string ' xx ' ' yy ' .'
yy = yy+0.2
'draw string ' xx ' ' yy ' /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/22.12.DECOMP_GLOBE/42.12.FRACTIONAL_CONTRIBUTION'
yy = yy+0.2
'draw string ' xx ' ' yy ' ./6TEST_READ_NC_PLT.sh '
yy = yy+0.2
'draw string ' xx ' ' yy ' Mon, 08 Jan 2024 13:29:06 +0900'

'gxprint 6TEST_READ_NC_PLT_1988081000.PDF'
'quit'
