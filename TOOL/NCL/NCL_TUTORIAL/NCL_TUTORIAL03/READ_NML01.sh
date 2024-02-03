#!/bin/bash

# namelist�t�@�C���������ǂނ���

NCL=READ_NML.ncl #NCL�X�N���v�g��

if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCL�X�N���v�g�����݂��Ȃ��ꍇ�C�G���[���b�Z�[�W��\�����ďI��

which runncl.sh &> /dev/null
if [ $? -ne 0 ]; then echo NO SUCH FILE,runncl.sh;exit 1;fi
# ����runncl.sh�Ƃ����t�@�C����������Ȃ�������G���[

NML=NAMELIST.TXT
if [ ! -f $NML ];then echo NO SUCH FILE,$NML;exit 1;fi
# namelist file�����݂��Ȃ��ꍇ�C�G���[���b�Z�[�W��\�����ďI��

runncl.sh $NCL $NML
