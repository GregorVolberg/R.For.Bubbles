library(tidyverse)
library(haven)
library(R.matlab)


switch(Sys.info()["nodename"],
       "DESKTOP-MDU4B4V" = setwd('C:/Users/Gregor/Filr/Meine Dateien/Bubbles_BKH/zwischenauswertung'),
       "PC1012101290"    = setwd('C:/Users/LocalAdmin/Filr/Meine Dateien/Bubbles_BKH/zwischenauswertung'))


df = readMat('./rp/rp_S14_scale_1_raw.mat')


# ds = read_spss(file = 'SPSS_Bubbles_final.sav') %>%
#      mutate(ERS_total  = rowSums(across(ERS_1:ERS_21), na.rm = F),
#             DERS_total = rowSums(across(DERS_1:DERS_36), na.rm = F),
#             BDI2_total = rowSums(across(BDI2_1:BDI2_20), na.rm = F))

