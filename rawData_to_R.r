library(tidyverse)
library(R.matlab)

fpath = 'D:/BHK_Bubbles/erg/raw/'
df = readMat(paste(fpath, 'BubblesRawData_zwisch.mat', sep=''))

# dim(df) ist [12 1 13]
vp  = unlist(df$rawData[1,1,1:dim(df$rawData)[3]])
nvp = unlist(lapply(df$rawData[3,1,], nrow))
grp = unlist(df$rawData[2,1,1:dim(df$rawData)[3]])
scon = unlist(df$rawData[12,1,1:dim(df$rawData)[3]])
outmat  = do.call(rbind,
          df$rawData[3,1,1:dim(df$rawData)[3]])

ds = as_tibble(outmat) %>%
     mutate(vp = as.factor(rep(vp, nvp)),
            group = as.factor(rep(grp, nvp)),
            start_condition = as_factor(rep(scon, nvp))) %>%
     rename(trial = V1, trialInBlock = V2, stimIdentifier = V3,
            keyCode = V4, rtime = V5, response = V6, f_gender = V8,
            f_emotion = V9, f_condition = V10, n_scale1 = V11,
            n_scale2 = V12, n_scale3 = V13, n_scale4 = V14,
            n_scale5 = V15, session_type = V16, 
            condition_codes = V17) %>%
     mutate(response = fct_recode(as_factor(response), wrong = "0", correct = "1"),
            f_gender = fct_recode(as_factor(f_gender), male = "1", female = "2"),
            f_emotion = fct_recode(as_factor(f_emotion), happy = "1", sad = "2"),
            f_condition = fct_recode(as_factor(f_condition), neutral = "1", emotion = "2"),
            f_session = fct_recode(as_factor(session_type), "NE-HA" = "1", "NE-SA" = "2"),
            condition_codes =   fct_recode(as_factor(condition_codes), 
                                           "NE-HA happy correct" = "1",
                                           "NE-HA happy incorrect" = "2",
                                           "NE-HA neutral correct" = "3",
                                           "NE-HA neutral incorrect" = "4",
                                           "NE-SA sad correct" = "5",
                                           "NE-SA sad incorrect" = "6",
                                           "NE-SA neutral correct" = "7",
                                           "NE-SA neutral incorrect" = "8")) %>%
  select(trial, vp, group, response, rtime, n_scale1, f_gender, f_emotion, f_condition,
         f_session,  start_condition, condition_codes, n_scale2, n_scale3, n_scale4, n_scale5)
                                        
fname = 'Bubbles_rawData.rds'
saveRDS(ds, file = paste(fpath, fname, sep = ''))
