library(tidyverse)
library(ggplot2)


fname = '../raw/Bubbles_rawData.rds'
ds    = readRDS(file = fname) %>%
        mutate(condition = paste(f_condition,
                           f_emotion,
                           sep=' '))
tabs = ds %>% 
       select(vp, group) %>%
       unique() %>%
       select(group) %>%
       table()

erg = ds %>%
  group_by(vp, group, condition) %>%
  summarize(n = n(),
            prz = mean(response == 'correct')*100,
            n_scale1   = round(mean(n_scale1)),
            efficiency = prz/n_scale1) %>%
 mutate(group = fct_recode(group, 
                           'control (n = 37)' = 'control',
                           'NSSI (n = 32)' = 'experimental'))


ggplot(erg, aes(x = condition,
                y = prz,
                shape = condition,
                color = condition)) +
  geom_jitter(position = position_jitterdodge(0.3), size = 2) +
  stat_summary(size = 1.5, fun = 'mean',   col='red', shape = 95)+
  scale_shape_manual(values = c(16, 17, 1, 2))+
  scale_color_manual(values = c("grey40", "goldenrod1", "grey40", "goldenrod1")) +   theme_classic() + 
  ylim(0, 100) +
  labs(x = 'Groups', y = 'Accuracy (% correct)') +
  facet_grid(.~group, switch = "x") +
  theme(strip.placement = "outside",
        strip.background.x = element_rect(color = NA,  fill=NA),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
ggsave('Accuracy.png')

ggplot(erg, aes(x = condition,
                y = n_scale1,
                shape = condition,
                color = condition)) +
  geom_jitter(position = position_jitterdodge(0.3), size = 2) +
  stat_summary(size = 1.5, fun = 'mean',   col='red', shape = 95)+
  scale_shape_manual(values = c(16, 17, 1, 2))+
  scale_color_manual(values = c("grey40", "goldenrod1", "grey40", "goldenrod1")) +   theme_classic() + 
  ylim(20, 550) +
  labs(x = 'Groups', y = 'Number of bubbles in first scale') +
  facet_grid(.~group, switch = "x") +
  theme(strip.placement = "outside",
        strip.background.x = element_rect(color = NA,  fill=NA),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
ggsave('n_Bubbles.png')


ggplot(erg, aes(x = condition,
                y = efficiency,
                shape = condition,
                color = condition)) +
  geom_jitter(position = position_jitterdodge(0.2), size = 2) +
  stat_summary(size = 1.5, fun = 'mean',   col='red', shape = 95)+
  scale_shape_manual(values = c(16, 17, 1, 2))+
  scale_color_manual(values = c("grey40", "goldenrod1", "grey40", "goldenrod1")) +   theme_classic() + 
  ylim(0, 1.2) +
  labs(x = 'Groups', y = 'Accuracy per bubble') +
  facet_grid(.~group, switch = "x") +
  theme(strip.placement = "outside",
        strip.background.x = element_rect(color = NA,  fill=NA),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
ggsave('efficiency.png')

ergAlex <- erg %>% 
           ungroup %>% 
           select(-n) %>%
           mutate(condition = str_replace(condition, ' ', ''),
                  group = fct_recode(group, 
                            'control' = 'control (n = 37)' ,
                            'NSSI' = 'NSSI (n = 32)')) %>%
           pivot_wider(., id_cols = c('vp', 'group'),
                         names_from = c('condition'),
                         values_from = c('prz', 'n_scale1', 'efficiency'))

write_csv2(ergAlex, 'AlexResults.csv')