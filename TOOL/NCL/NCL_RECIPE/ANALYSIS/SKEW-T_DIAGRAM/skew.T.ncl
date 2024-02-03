; dewtemp_trh
;   http://www.ncl.ucar.edu/Document/Functions/Built-in/dewtemp_trh.shtml
; Dew Point Calculator
;   http://www.dpcalc.org/
; Skew-T
;   https://www.ncl.ucar.edu/Applications/skewt.shtml
; Missing values in NCL
;   http://www.ncl.ucar.edu/Document/Language/fillval.shtml


load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "./skewt_func.ncl"


begin
  wallClock1 = systemfunc("date") ; retrieve wall clock time

  scriptname = getenv("NCL_ARG_1")
  stn   = getenv("NCL_ARG_2")
  input = getenv("NCL_ARG_3")
  nline = getenv("NCL_ARG_4")
  ncolm = getenv("NCL_ARG_5")
  output = getenv("NCL_ARG_6")
  in_model=getenv("NCL_ARG_7")

  nlvl=stringtointeger(nline) ;155
  ncol=stringtointeger(ncolm) ;6

  ncol_m=5

  Data_o = asciiread (input ,    (/nlvl,ncol/), "float") 
  Data_m = asciiread (in_model , (/nlvl,ncol_m/), "float") 

  p    = Data_o (:,0)
  wdir = Data_o (:,1)
  wspd = Data_o (:,2)
  z    = Data_o (:,3)
  tc   = Data_o(:,4)
  rh   = Data_o (:,5)

   p@_FillValue    = -999.0
   wdir@_FillValue = -999.0
   wspd@_FillValue = -999.0
   z@_FillValue    = -999.0
  rh@_FillValue    = -999.0
  tc@_FillValue    = -999.0000
   p@units = "hPa"
   wdir@units = "deg"
   wspd@units = "m/s"
   z@units = "m"
  rh@units = "%"
  tc@units = "C"

  tk= new( (/nlvl/),"float")
  tdk=new( (/nlvl/),"float")
  tdc=new( (/nlvl/),"float")

  do k=0,nlvl-1

    if( ismissing(tc(k)))then
      tk(k)=tc@_FillValue

    else
      tk(k)   = tc(k) + 273.15                ; C ==> Kelvin

      tdk(k)  = dewtemp_trh(tk(k),rh(k))    ; dew pt temperature [K]
      tdc(k)  = tdk(k)-273.15              ; [C]

    end if
  end do
  tk@_FillValue = -999.0
  tdk@_FillValue = -999.0
  tdc@_FillValue = -999.0
  tk@units = "K"                ; add "units" attribute
  tdk@units = "K"                ; add "units" attribute
  tdc@units = "C"                ; add "units" attribute

  ; print(tdc)
  

;*************************
; create plot
;*************************
  wks  = gsn_open_wks ("eps", output)
; --- Create background skew-T; plot sounding ---------------------

  skewtOpts                 = True
  skewtOpts@DrawColAreaFill = True    ; default is False
  skewtOpts@tiMainString    = stn
  skewtOpts@DrawFahrenheit  = False   ; default is True
  skewtOpts@DrawIsotherm      = False
  skewtOpts@DrawIsobar        = False
  skewtOpts@DrawMixRatio      = False
  skewtOpts@DrawDryAdiabat    = False
  skewtOpts@DrawMoistAdiabat  = False ; aka: saturation or pseudo adibat
  skewtOpts@DrawWind          = True
  skewtOpts@DrawStandardAtm   = False
  skewtOpts@DrawColLine       = False
  skewtOpts@DrawColAreaFill   = False 

  skewt_bkgd = skewT_BackGround (wks, skewtOpts)
  draw (skewt_bkgd)



; First sounding (Obs)
  dataOpts           = True     ; options describing data and ploting
  dataOpts@Wthin     = 8        ; plot every n-th wind barb
  dataOpts@PrintZ           = False    ; do not print Z 
  dataOpts@ThermoInfo       = False    ; print thermodynamic info
  dataOpts@colTemperature  = "red"
  dataOpts@colDewPt        = dataOpts@colTemperature
  dataOpts@colWindP        = dataOpts@colTemperature
  dataOpts@colDewPt        = dataOpts@colTemperature
  dataOpts@linePatternDewPt = 1

  skewt_data = skewT_PlotData   (wks, skewt_bkgd, p,tc,tdc,z \
                                    , wspd,wdir, dataOpts)
  draw (skewt_data)



; Second sounding (Model)
  tc   = Data_m(:,1)
  rh   = Data_m (:,2)
  wdir = Data_m (:,3)
  wspd = Data_m (:,4)

  do k=0,nlvl-1

    if( ismissing(tc(k)))then
      tk(k)=tc@_FillValue

    else
      tk(k)   = tc(k) + 273.15                ; C ==> Kelvin

      tdk(k)  = dewtemp_trh(tk(k),rh(k))    ; dew pt temperature [K]
      tdc(k)  = tdk(k)-273.15              ; [C]

    end if
  end do

  dataOpts@xpWind    = 40.      ; new location for winds [default 45]
  dataOpts@colTemperature  = "black"
  dataOpts@colDewPt        = dataOpts@colTemperature
  dataOpts@colWindP        = dataOpts@colTemperature

  skewt_data = skewT_PlotData   (wks, skewt_bkgd, p,tc,tdc,z \
                                    , wspd,wdir, dataOpts)

;Header
txres               = True
txres@txJust="CenterLeft"
txres@txFontHeightF = 0.010
lst = systemfunc("ls -lh "+input)
strs = str_split(lst, " ")
str1 = "Input: "+strs(8)
str2 = strs(0)+" "+strs(1)+" "+strs(2)+" "+strs(3)+" "+strs(4)+" "+\
strs(5)+" "+strs(6)+" "+strs(7)
gsn_text_ndc(wks,str1,0.05,0.99,txres)
gsn_text_ndc(wks,str2,0.05,0.97,txres)

lst = systemfunc("ls -lh "+in_model)
strs = str_split(lst, " ")
str1 = "Input: "+strs(8)
str2 = strs(0)+" "+strs(1)+" "+strs(2)+" "+strs(3)+" "+strs(4)+" "+\
strs(5)+" "+strs(6)+" "+strs(7)
gsn_text_ndc(wks,str1,0.05,0.95,txres)
gsn_text_ndc(wks,str2,0.05,0.93,txres)



;txres@txJust="CenterCenter"
;txres@txFontHeightF = 0.016
;text=runname + "   " + domain
;gsn_text_ndc(wks,text,0.5,0.90,txres)

;Footer
txres@txFontHeightF = 0.01
txres@txJust="CenterLeft"
text = systemfunc("export LANC=C; date")
gsn_text_ndc(wks,text,  0.05,0.055,txres)

cwd =systemfunc("pwd")
gsn_text_ndc(wks,"Current dir: "+cwd,   0.05,0.040,txres)
gsn_text_ndc(wks,"Script: "+scriptname, 0.05,0.025,txres)
gsn_text_ndc(wks,"Output file: "+output, 0.05,0.010,txres)



  frame(wks)


  print("Input: "  + input)
  print("nlvl= "   + nlvl)
  print("ncol= "   + ncol)
  print("output: " + output)

  wallClock2 = systemfunc("date") ; retrieve wall clock time
  print("Started  at " + wallClock1)
  print("Finished at " + wallClock2)
end
