#!/bin/bash

# ������ύX���Ȃ���J��Ԃ�NCL�X�N���v�g�����s����

NCL=GET_ARG.ncl #NCL�X�N���v�g��

if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCL�X�N���v�g�����݂��Ȃ��ꍇ�C�G���[���b�Z�[�W��\�����ďI��

which runncl.sh &> /dev/null
if [ $? -ne 0 ]; then echo NO SUCH FILE,runncl.sh;exit 1;fi
# ����runncl.sh�Ƃ����t�@�C����������Ȃ�������G���[

DSET=ERA5
VARLIST="DLR DSR NLR"

for VAR in $VARLIST; do
runncl.sh $NCL $DSET $VAR
done


