library(tidyverse)
library(haven)

fpath   = 'D:/BHK_Bubbles/erg/'
fname = 'clusterdata.txt'
fnameSPSS = 'Bubbles_SPSS.csv'


ds  = read.delim(paste(fpath, fname, sep=''),
                header = TRUE)
ds2 = read_csv2(file = paste(fpath, fnameSPSS, sep = '')) %>%
      mutate(ERS_total  = rowSums(across(ERS_1:ERS_21), na.rm = F),
         DERS_total = rowSums(across(DERS_1:DERS_36), na.rm = F),
         BDI2_total = rowSums(across(BDI2_1:BDI2_20), na.rm = F),
         vpcode = paste('S', 
                        str_split_fixed(as.character(dsp$VPNummer), '', 2)[,2],
                        sep = ''))
dsall = merge(ds, ds2, by.x = 'vp', by.y = 'vpcode')

dsc = dsall  %>% filter(grp==0)
plot(dsall$s4hap-dsall$s4hne, dsall$ERS_total)
points(dsc$s4hap-dsc$s4hne, dsc$ERS_total, col = 'green')


plot(dsall$s4hap-dsall$s4hne, dsall$BDI2_total)
points(dsc$s4hap-dsc$s4hne, dsc$BDI2_total, col = 'green')


dsc = ds %>%
      filter(grp == 0)  # control
dse = ds %>%
  filter(grp == 1)      # experiment

     
barplot(c(mean(dsc$s4hap), mean(dsc$s4hne)), ylim = c(-2,2))
barplot(c(mean(dse$s4hap), mean(dse$s4hne)), ylim = c(-2,2))


dsp = read_csv2(file = paste(fpath, fnameSPSS, sep = '')) %>%
      mutate(ERS_total  = rowSums(across(ERS_1:ERS_21), na.rm = F),
             DERS_total = rowSums(across(DERS_1:DERS_36), na.rm = F),
             BDI2_total = rowSums(across(BDI2_1:BDI2_20), na.rm = F),
             vpcode = paste('S', 
                            str_split_fixed(as.character(dsp$VPNummer), '', 2)[,2],
                            sep = ''))

