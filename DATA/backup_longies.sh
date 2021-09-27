#! /bin/sh

tar cvf - US*.csv | (mkdir BackupLongies`date +20%y%m%d%H%M`; cd BackupLongies`date +20%y%m%d%H%M`; tar xvf - )