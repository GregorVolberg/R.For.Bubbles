library(tidyverse)
library(ggplot2)

ds = as_tibble(data.frame(
  grp  = rep(c('A', 'B'), each = 20),
  cond = rep(c(rep('C1', 5), rep('C2', 5)), each = 2),
  erg  = c(rnorm(10, 10,1),rnorm(10,12,2), rnorm(10,8,2), rnorm(10,15,3))))

ggplot(ds, aes(x = cond, y = erg, color = cond)) +
 geom_jitter(position = position_jitterdodge(0.2)) +
 scale_color_manual(values=c("#999999", "#E69F00", "#999999", "#E69F00")) + 
 theme_classic() + 
 ylim(0,25) +
 labs(x = 'Groups', y = paste('hallo', quote(x==10^3))) +
 #ylab('Accuracy per Bubble') +
 facet_grid(.~grp, switch = "x") +
 theme(strip.placement = "outside",
       strip.background.x=element_rect(color = NA,  fill=NA)) + 
  geom_text(aes(y = 26, label = "High"))
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 26),
                     labels = c(0, 5, 10, 15, 20, " times 10-3"))
# ggbreak
