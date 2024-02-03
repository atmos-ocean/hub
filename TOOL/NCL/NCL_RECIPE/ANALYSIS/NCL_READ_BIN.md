�o�C�i���f�[�^�̓ǂݍ���
============================
[TOC]
�}�j���A��
---------------------------------------
http://www.atmos.rcast.u-tokyo.ac.jp/shion/NCLtips/index.php?cmd=read&page=%E3%83%87%E3%83%BC%E3%82%BF%E3%81%AE%E8%AA%AD%E3%81%BF%E6%9B%B8%E3%81%8D#content_1_4

Fri, 24 Jul 2020 10:46:03 +0900
calypso.bosai.go.jp
/work05/manda/20.AGO_WAN/FLUX.MSM/02.FLUX.HIROSE/CHK.MSM.TOPOGRAPHY

```
srcdump.sh CHK.MSM.TERRAIN.ncl
```

### HOW TO RUN

### INFO
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB

### SOURCE FILES
- CHK.MSM.TERRAIN.ncl
  
#### CHK.MSM.TERRAIN.ncl
```
; 2020-07-24_10-24 
; manda@calypso
; /work05/manda/20.AGO_WAN/FLUX.MSM/02.FLUX.HIROSE/CHK.MSM.TOPOGRAPHY
;
indir="/work05/manda/DATA/MSM.200719/TERRAIN"
infle="TOPO.MSM_5K"

setfileoption("bin", "ReadByteOrder", "BigEndian")
TOPO= fbindirread(indir+"/"+infle,0,(/505,481/),"float")

;���\���l�\�񃂃f��GPV
;       �i�q���F  ��������   481    ��k����   505
;       �i�q�Ԋu�F�������� 0.0625�x  ��k���� 0.05�x
;       �擪�̊i�q�_�F�k��47.6�x  ���o120�x 


lat=fspan(47.6 ,22.35   ,505)
lon=fspan(120.0,150.0625,481)
;print(lat)
;print(lon)

TOPO!0="lat"
TOPO!1="lon"

TOPO&lat=lat
TOPO&lon=lon

TOPO&lat@units = "degrees_north"
TOPO&lon@units = "degrees_east"

printVarSummary(TOPO)
printVarSummary(TOPO&lat)
printVarSummary(TOPO&lon)



res=True
res@cnFillOn = True
res@gsnAddCyclic = False
res@mpMinLonF    = 136.5
res@mpMaxLonF    = 137
res@mpMinLatF    = 34
res@mpMaxLatF    = 34.5

;res@mpMinLatF    = min(TOPO&lat)
;res@mpMaxLatF    = max(TOPO&lat)
;res@mpMinLonF    = min(TOPO&lon)
;res@mpMaxLonF    = max(TOPO&lon)

res@mpDataBaseVersion="HighRes"
res@mpGridLonSpacingF = 0.1
res@mpGridLatSpacingF = 0.1

res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF = 0.
res@cnMaxLevelValF = 400.
res@cnLevelSpacingF = 20.

wks = gsn_open_wks("eps", "TOPO.MSM_5K")

gsn_define_colormap(wks,"GMT_topo")

plot = gsn_csm_contour_map_ce(wks, TOPO, res)

```

  





README�i�x���Z���^�[�j_201406.txt
-------------------------------
http://database.rish.kyoto-u.ac.jp/arch/jmadata/data/gpv/original/etc/README%ef%bc%88%e6%94%af%e6%8f%b4%e3%82%bb%e3%83%b3%e3%82%bf%e3%83%bc%ef%bc%89_201406.txt



/work05/manda/DATA/MSM.200719/TERRAIN



### 1 �������\������t�@�C��


  ���̎�����15�̃t�@�C���ɂ��\������Ă��܂��B

####  �n�\�W�I�|�e���V�������x�f�[�^

    (a) "TOPO.LFM_2K"              �ǒn���l�\�񃂃f��GPV�p
    (b) "TOPO.MSM_5K"              ���\���l�\�񃂃f��GPV�p
    (c) "TOPO.TL959RGG_0.5"        �S�����l�\�񃂃f���i�S����jGPV�p
    (d) "TOPO.TL959RGG_0.2_JP"     �S�����l�\�񃂃f���i���{��jGPV�p
    (e) "TOPO.TL479RGG_2.5"        �T�ԃA���T���u�����l�\�񃂃f��GPV�p(�S��)
    (f) "TOPO.TL479RGG_1.25_JP"    �T�ԃA���T���u�����l�\�񃂃f��GPV�p(���{��)
    (g) "TOPO.TL319RGG_2.5"        �P�����A���T���u�����l�\�񃂃f��GPV�p(�S��)



 ###  �C�����z�f�[�^

    (a) "LANDSEA.LFM_2K"           �ǒn���l�\�񃂃f��GPV�p
    (b) "LANDSEA.MSM_5K"           ���\���l�\�񃂃f��GPV�p
    (c) "LANDSEA.TL959RGG_0.5"     �S�����l�\�񃂃f���i�S����jGPV�p
    (d) "LANDSEA.TL959RGG_0.2_JP"  �S�����l�\�񃂃f���i���{��jGPV�p
    (e) "LANDSEA.TL479RGG_2.5"     �T�ԃA���T���u�����l�\�񃂃f��GPV�p(�S��)
    (f) "LANDSEA.TL479RGG_1.25_JP" �T�ԃA���T���u�����l�\�񃂃f��GPV�p(���{��)
    (g) "LANDSEA.TL319RGG_2.5"     �P�����A���T���u�����l�\�񃂃f��GPV�p(�S��)



####    ���̃t�@�C��

        "README�i�x���Z���^�[�j_201406.txt"  ���̃t�@�C��

�i���j���l�\�񃂃f���Ŏg�p���鍂�x�f�[�^���u�n�\�W�I�|�e���V�������x�f�[�^�v�ƌĂсC���y��ʏȋy�э��y�n���@�����J���Ă���n�}��W���f�[�^���̂��̂ƍ������Ȃ��悤�ɂ��Ă��܂��B�������C�ȉ��̕����ł͔ώG��������邽�߁u���x�f�[�^�v�ƌĂт܂��B



### 2 �O��(2011�N6��)�񋟂��������Ƃ̑���


2014�N3���ɋǒn���l�\�񃂃f��GPV�̒񋟊J�n�ɔ����A���x�f�[�^����ъC�����z�f�[�^��V�K�ɒǉ����Ă��܂��B(a)

���\���l�\�񃂃f������ёS�����l�\�񃂃f���̓f�[�^�̍쐬���ɂ�鏬���Ȓl�̕ω��͂���܂����A��{�I�ɕύX����܂���B(b)(c)(d)

2014�N2���ɏT�ԃA���T���u�����l�\�񃂃f���̍��𑜓x���ɔ����A���x�f�[�^����ъC�����z�f�[�^���X�V���Ă��܂��B(e)(f)

2014�N3���̂P�����A���T���u�����l�\�񃂃f���̕ύX�ɔ����A���x�f�[�^����ъC�����z�f�[�^���X�V���Ă��܂��B(g)

�����f���̓��{��ɂ��Ă�GPV�f�[�^�̒񋟂��s��Ȃ����߁A�n�`�f�[�^�̒񋟂ɂ��Ă��s��Ȃ����Ƃƒv���܂����B



### 3 �f�[�^�̒l


���x�f�[�^�͊i�q�_�̍��x�B�P�ʂ� m�B

�C�����z�f�[�^�́A������O�C������P�Ƃ����C���̊����B

�Ȃ��A�ǒn���l�\�񃂃f��GPV�ł͗̈�͈͊O�ƂȂ�i�q�_������܂��B
�@
���x�f�[�^�ɂ��Ă�,���l�\�񃂃f���̗̈�͈͊O�ƂȂ�i�q�_�ɂ��Ă͍��x��999.0m�ɐݒ肵�Ă���܂��B
�@
�C�����z�f�[�^�ɂ��Ă�,���l�\�񃂃f���̗̈�͈͊O�ƂȂ�i�q�_�ɂ��Ắ|�P���ݒ肵�Ă���܂��B

�C�ۋƖ��x���Z���^�[���烆�[�U�ɒ񋟂����GPV�f�[�^�̍��W�n�́A�e���l�\�񃂃f���̐��l�v�Z�Ŏg�p����Ă�����W�n�ƈقȂ��Ă��邽�߁A���l�\�񃂃f�����g�p���Ă���l����}���Ă��܂��B



### 4 �t�@�C���`��

####  (1) �f�[�^�`��

4�o�C�g����(IEEE754)��2�����z��ł��B

�o�C�g�̕��т�big endian�ł��B

####   (2) ���W�n

���ܓx�o�x�i�q���W�n�ł��B

####     (a) �ǒn���l�\�񃂃f��GPV

�i�q���F  ��������   1201    ��k����   1261

�i�q�Ԋu�F�������� 0.025�x  ��k���� 0.02�x

�擪�̊i�q�_�F�k��47.6�x  ���o120�x 

####     (b) ���\���l�\�񃂃f��GPV

�i�q���F  ��������   481    ��k����   505

�i�q�Ԋu�F�������� 0.0625�x  ��k���� 0.05�x

�擪�̊i�q�_�F�k��47.6�x  ���o120�x 

####     (c) �S�����l�\�񃂃f���i�S����jGPV

�i�q���F  ��������   720    ��k����   361

�i�q�Ԋu�F0.5�x�i�����C��k�������Ƃ��j

�擪�̊i�q�_�F�k��90�x  �o�x0�x 

####     (d) �S�����l�\�񃂃f���i���{��jGPV

�i�q���F  ��������  121    ��k����  151

�i�q�Ԋu�F�������� 0.25�x  ��k���� 0.2�x

�擪�̊i�q�_�F�k��50�x  ���o120�x

####     (e) �T�ԃA���T���u�����l�\�񃂃f��GPV�i�S���j

�i�q���F  ��������  144    ��k����   73

�i�q�Ԋu�F2.5�x�i�����C��k�������Ƃ��j

�擪�̊i�q�_�F�k��90�x  �o�x0�x

####     (f) �T�ԃA���T���u�����l�\�񃂃f��GPV�i���{��j

�i�q���F  ��������   73    ��k����   40

�i�q�Ԋu�F1.25�x�i�����C��k�������Ƃ��j

�擪�̊i�q�_�F�k��71.25�x  ���o90�x

####     (g) �P�����A���T���u�����l�\�񃂃f��GPV�i�S���j

�i�q���F  ��������  144    ��k����   73

�i�q�Ԋu�F2.5�x�i�����C��k�������Ƃ��j

�擪�̊i�q�_�F�k��90�x  �o�x0�x

####   (3) �f�[�^�i�[����

�擪�̊i�q�_����ܓx�̓����i�q�_���o�x�����������Ɋi�[���C

���̂�����̈ܓx�œ��l�ɌJ��Ԃ��i�[���Ă��܂��B