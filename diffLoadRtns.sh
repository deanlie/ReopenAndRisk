#! /bin/sh

tail -279 loadUSVaccinationData.R | cat - > LUVD.RX
tail -450 loadAllUSData.R | head -258 | cat - > LATD.RX

diff LUVD.RX LATD.RX > loadDataDiffs.txt

