# 安定度の時間的変化

$$
\frac{\partial}{\partial t}\bigg(-\frac{\partial \theta_e}{\partial p}\bigg)
=\frac{\partial}{\partial p}\bigg(u\frac{\partial \theta_e}{\partial x}
+v\frac{\partial \theta_e}{\partial y}
+\omega\frac{\partial \theta_e}{\partial p}\bigg)
+\frac{\partial}{\partial p}\bigg(\frac{\partial \overline{\omega'\theta_e'}}{\partial p}\bigg)
$$

右辺第一項: 鉛直差分移流 (differential advection)　**負号がつかない**ことに注意

成層不安定化をもたらす過程

1. 上層の寒気・乾気移流
2. 下層の暖気・湿気移流
3. 上昇流 ($\omega$による$\theta_e$移流, または気柱の伸びによる効果)
4. 下層の非断熱熱源と水蒸気源, 上層の放射冷却



# advect_variable_cfd

Using centered-finite_differences (cfd) to estimate gradients, advect a variable horizontally on a regional or global rectilinear grid.

https://www.ncl.ucar.edu/Document/Functions/Contributed/advect_variable_cfd.shtml

## Description

Calculate the horizontal advection of a quantity on the globe.

```
             adv_X =  U*(dX/dlon) + V*(dX/dlat)
```

## Arguments

*u*

Array containing zonal wind components (m/s). The array **must be rectilinear and ordered south to north**.

*v*

Array containing meridional wind components (m/s). Same size and shape as *u*.

*x*

Array containing a scalar quantity (eg: temperature, specific humidity, *etc*). Same size and shape as *u*.

*cyclic*

Grid type: *cyclic*=True means grid is cyclic in longitude; *cyclic*=False means regional grid.

*longName*

A string to be used as the long_name of the advected variable.

*units*

A string specifying the units of the returned variable.

*opt*

option.

- opt=**0** means return the the **advection** result.
- opt=**1** means return the **advection** result, **longitudinal** and **latitudinal** **gradients** as part of a three-element variable of type list

**Example 2** Similar to Example 1 but for a **regional rectilinear** grid. Return the result as a variable of type list containing the advected quantity and the meridional and zonal gradients of the vatiable being advected.

```
; region

  latS =  25
  latN =  50.
  lonL = 230.
  lonR = 300.

  U = u(:,:,{latS:latN},{lonL:lonR})
  V = v(:,:,{latS:latN},{lonL:lonR})
  T = t(:,:,{latS:latN},{lonL:lonR})

  cyclic  = False            ; Grid is regional
  opt_adv = 1                ; Return a variable of type list containing 3 variables

  T_list  = advect_variable_cfd(U,V,T, T&lat,T&lon, cyclic \
                               ,"tcfd: Region","m-K/s", opt_adv)

  Tadv = T_list[0]           ; advected quantity
  Tgrx = T_list[1]           ; zonal (longitudinal) gradient of (here) temperature
  Tgry = T_list[2]           ; meridional (latitudinal) gradient

  printVarSummary(Tadv)    ; advection of temperature
  printVarSummary(Tgrx)    ; zonal gradient
  printVarSummary(Tgry)    ; meridional gradient
```

The (edited) output is:

```
          Variable: Tadv
          Type: float
          Total Size: 151844 bytes
                      37961 values
          Number of Dimensions: 4
          Dimensions and sizes:   [time | 7] x [level | 17] x [lat | 11] x [lon | 29]
          Coordinates: 
                      time: [1823280..1823424]
                      level: [1000..10]
                      lat: [25..50]                       region
                      lon: [230..300]
          Number Of Attributes: 4
            _FillValue :  -9.96921e+36
            long_name :   tcfd: Region
            units :       m-K/s
            NCL_tag :     advect_variable_cfd
          
          (0)     tcfd: Region (m-K/s) : min=-0.000617743   max=0.000705905
          
            ===
          
          Variable: Tgrx
          Type: float
          Total Size: 151844 bytes
                      37961 values
          Number of Dimensions: 4
          Dimensions and sizes:   [time | 7] x [level | 17] x [lat | 11] x [lon | 29]
          Coordinates: 
                      time: [1823280..1823424]
                      level: [1000..10]
                      lat: [25..50]
                      lon: [230..300]
          Number Of Attributes: 3
            units :       ?/m
            long_name :   cfd: zonal gradient
            _FillValue :  -9.96921e+36
          
          (0)     cfd: zonal gradient (?/m) : min=-3.29331e-05   max=2.25408e-05
```