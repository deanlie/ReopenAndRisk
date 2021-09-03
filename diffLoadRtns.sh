#! /bin/sh

tail -279 loadUSVaccinationData.R | head -254 | cat - > LUVD.RX
tail -451 loadAllUSData.R | head -254 | cat - > LATD.RX

diff LUVD.RX LATD.RX > loadDataDiffs.txt

