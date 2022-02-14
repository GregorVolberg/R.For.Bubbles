library(tidyverse)
#library(R.matlab)

fpath = 'D:/BHK_Bubbles/erg/raw/'
fname = 'Bubbles_rawData.rds'
ds = readRDS(file = paste(fpath, fname, sep = ''))

newcon = factor(1:length(ds$f_emotion), levels = c('happy', 'sad', 'neutral_H', 'neutral_S'))
newcon[ds$f_emotion == 'happy' & ds$f_condition == 'emotion'] = 'happy'
newcon[ds$f_emotion == 'sad' & ds$f_condition == 'emotion'] = 'sad'
newcon[ds$f_condition == 'neutral' & ds$f_session == 'NE-HA'] = 'neutral_H'
newcon[ds$f_condition == 'neutral' & ds$f_session == 'NE-SA'] = 'neutral_S'
ds$f_emotion2 = newcon

erg = ds %>%
      group_by(vp, group, f_emotion2) %>%
              summarize(n = n(),
                        prz = mean(response == 'correct'),
                        n_scale1  = round(mean(n_scale1))) 
erg2 = erg %>%
      group_by(group, f_emotion2) %>%
      summarize(n = mean(n),
                prz = mean(prz),
                n_scale1  = mean(n_scale1))

clrs  = gray(c(0.3, 0.5, 0.8, 1))
labls = c('happy', 'sad', 'neut_H', 'neut_S')

k=barplot(prz ~ f_emotion2 + group, beside = T, data = erg2, ylim = c(0,1),
                        col = clrs, args.legend = list(x=1, y=1), ylab = 'f(correct)',
          space = c(0.2,0,0.2,0,1,0,0.2,0))
axis(3, at=k, labels = rep(labls,2), las=2, pos=1, lwd=0)
abline(h = 0.75, lty = 'dashed')
abline(h = 0.5, col = 'red')

grpvec = c('control', 'experimental')
convec = c('happy', 'sad', 'neutral_H', 'neutral_S')
for (grp in 1:2){
  for (con in 1:4){
    mm=erg %>% filter(group == grpvec[grp] & f_emotion2 == convec[con]) %>% pull(prz)
    points(rep(k[con, grp], length(mm)), mm, col = 'black')
}
}
text(x = colMeans(k)[1], 0.95, paste('N = ', n_distinct(erg$vp[erg$group == 'control'])))
text(x = colMeans(k)[2], 0.95, paste('N = ', n_distinct(erg$vp[erg$group == 'experimental'])))


# nbubbles
k=barplot(n_scale1 ~ f_emotion2 + group, beside = T, data = erg2,
          ylim = c(0,round(range(erg2$n_scale1),-2)[2]),
          col = clrs, args.legend = list(x=1, y=1), ylab = 'n Bubbles',
          space = c(0.2,0,0.2,0,1,0,0.2,0))
axis(3, at=k, labels = rep(labls,2), las=2, pos=1, lwd=0)

grpvec = c('control', 'experimental')
convec = c('happy', 'sad', 'neutral_H', 'neutral_S')
for (grp in 1:2){
  for (con in 1:4){
    mm=erg %>% filter(group == grpvec[grp] & f_emotion2 == convec[con]) %>% pull(n_scale1)
    points(rep(k[con, grp], length(mm)), mm, col = 'black')
  }
}
text(x = colMeans(k)[1], 0.95, paste('N = ', n_distinct(erg$vp[erg$group == 'control'])))
text(x = colMeans(k)[2], 0.95, paste('N = ', n_distinct(erg$vp[erg$group == 'experimental'])))

# efficiency score, "bubbles per unit accuracy"
erg = erg %>% mutate(efficiency = prz*100 / n_scale1)
plot(x = NULL, y = NULL, xlim = c(0,9), ylim = c(0, 1.8),
     ylab = 'Bubbles per unit accuracy', xlab = 'condition',
     xaxt = 'n')

nn=NULL
for (grp in 1:2){
  for (con in 1:4){
    nn[[(grp-1)*4 + con]] = erg %>% filter(group == grpvec[grp] & f_emotion2 == convec[con]) %>% pull(efficiency)
  }
}

points(nn[[1]] ~ jitter(numeric(length(nn[[1]]))+1.2, 1, 0.1), 
       col = 'blue', pch = 19)
points(nn[[2]] ~ jitter(numeric(length(nn[[2]]))+1.8, 1, 0.1), 
       col = 'blue', pch = 1)
points(nn[[3]] ~ jitter(numeric(length(nn[[3]]))+3.2,1, 0.1), 
       col = 'green', pch = 19)
points(nn[[4]] ~ jitter(numeric(length(nn[[4]]))+3.8,1, 0.1), 
       col = 'green', pch = 1)

points(nn[[5]] ~ jitter(numeric(length(nn[[5]]))+6.2, 1, 0.1), 
       col = 'blue', pch = 19)
points(nn[[6]] ~ jitter(numeric(length(nn[[6]]))+6.8, 1, 0.1), 
       col = 'blue', pch = 1)
points(nn[[7]] ~ jitter(numeric(length(nn[[7]]))+8.2,1, 0.1), 
       col = 'green', pch = 19)
points(nn[[8]] ~ jitter(numeric(length(nn[[8]]))+8.8,1, 0.1), 
       col = 'green', pch = 1)

text(x = 2, 1.6, 'control')
text(x = 7, 1.6, 'NSSI')
legend(3.8,1.8,legend = c('happy', 'sad', 'neutral happy', 'neutral sad'),
       bty='n', pch = c(19,1,19,1), col = c('blue', 'blue', 'green', 'green'), cex=0.8)
axis(1, at = c(1.2, 1.8, 3.2,3.8, 6.2, 6.8, 8.2, 8.8), labels = F, pos = 3)


k = boxplot(prz ~ f_emotion2, 
            data = erg,
            ylim = c(0.3,1),
            las=2, at=1:4)

library(car) # for Boxplot
boxplot(NULL,
        ylim = c(0.3,1),
        xlim=c(0,10), las=2,
        ylab = c('f(correct)'))

tmp = boxplot(prz ~ f_emotion2, 
        data = filter(erg, group == 'control'),
        las=2, at=1:4, col = gray(0.6), add=T)

tmp2 = boxplot(prz ~ f_emotion2, 
        data = filter(erg, group == 'experimental'),
        las=2, at=6:9, col = gray(0.9), add=T)
abline(h=0.75, lty='dashed')
bxpdat <- boxplot(vv)
text(tmp$group,                                              # the x locations 
     tmp$out,                                                # the y values
     rownames(vv)[which(vv == bxpdat$out, arr.ind=TRUE)[, 1]],  # the labels
     pos = 4)  
