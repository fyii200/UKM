rm(list=ls())
setwd('/Users/fabianyii/Desktop/UKM RA/lyc')
d <- read.csv('data/lyc.csv')

# install.packages(('rbin'))
# library('rbin')

#optimize column names
names(d)[c(1,2,4,8,10,14,15,18,21:25)] <- c('id','name','RLAL','RAL','Rcyl','Lheight','Lsaglength','Lcyl','weight','head circum','age','gender','k')
d <- d[,-3]

#Divide px into low and high myopes based on RSE
lowm <- d[which(d$RSE > -3),] #display px with low myopia
highm <- d[-which(d$RSE > -3),] #display px with high myopia (RSE smaller or equal to -3D)

#plot (with emmetropes)
plotfunc <- function(a,b,c,d,e,f) {
plot(a, b, bty='n', pch=19, ylab=f, xlab='SE (D)', main=d, col='maroon', xlim=c(-5,0), ylim=c(21,28),yaxt=e, cex=0.7)
abline(a=lm(RLAL~RSE, lowm)$coefficients[1], b=lm(RLAL~RSE, lowm)$coefficients[2], col='maroon', lty=3)
cor.test(lowm$RSE, lowm$RLAL, 'pearson', alternative='two.sided')

points(highm$RSE, highm$RLAL, pch=17, col='blue', cex=0.7)
abline(a=lm(RLAL~RSE, highm)$coefficients[1], b=lm(RLAL~RSE, highm)$coefficients[2], col='blue', lty=3)
cor.test(highm$RSE, highm$RLAL, 'pearson', alternative='two.sided')

legend('topleft', c('High Myopia (≤ -3D)','Low Myopia (> -3D)'), pch=c(17,19), col=c('blue','maroon'), bty='n', cex=0.8, text.col='gray')
legend('topright', c('0.27 (-0.10, 0.57); p=0.15' ,c), lty=1, col=c('blue','maroon'),
       title="Pearson's Correlation (95% CI)", bty='n', cex=0.8, lwd=2.5, text.col='gray') }

pdf(file='LALvsSE.pdf', width=13, height=12)
par(mfrow=c(2,2))
plotfunc(lowm$RSE, lowm$RLAL,'-0.63 (-0.79, -0.40); p<0.05','Longitudinal Axial Length (LAL) vs SE','s','LAL (mm)')

#plot (without emmetropes)
cor.test(lowm[-which(lowm$RSE==0),]$RSE, lowm[-which(lowm$RSE==0),]$RLAL, 'pearson', alternative='two.sided') #correlation
plotfunc(lowm[-which(lowm$RSE==0),]$RSE, lowm[-which(lowm$RSE==0),]$RLAL, '-0.56 (-0.76, -0.25); p<0.005','LAL vs SE (w/o Emmetropes)','s','LAL (mm)')

#bin RSE in 0.5D steps (low myopia)
lowm[which(lowm$RSE<= 0 & lowm$RSE> -0.5),]$RSE <- 0
lowm[which(lowm$RSE<= -0.5 & lowm$RSE> -1),]$RSE <- -0.5
lowm[which(lowm$RSE<= -1 & lowm$RSE> -1.5),]$RSE <- -1
lowm[which(lowm$RSE<= -1.5 & lowm$RSE> -2),]$RSE <- -1.5
lowm[which(lowm$RSE<= -2 & lowm$RSE> -2.5),]$RSE <- -2
lowm[which(lowm$RSE<= -2.5 & lowm$RSE> -3),]$RSE <- -2.5

#bin RSE in 0.5D steps (high myopia)
highm[which(highm$RSE<= -3 & highm$RSE> -3.5),]$RSE <- -3
highm[which(highm$RSE<= -3.5 & highm$RSE> -4),]$RSE <- -3.5
highm[which(highm$RSE<= -4 & highm$RSE> -4.5),]$RSE <- -4
highm[which(highm$RSE<= -4.5 & highm$RSE> -5),]$RSE <- -4.5

#binned plot (low myopia and high myopia)
plot(lowm$RSE, lowm$RLAL, bty='n', ylab='LAL (mm)', xlab='SE (D)', pch=19, col='gray88', main='LAL vs Binned SE',
     xlim=c(-5,0), ylim=c(21,28))
points(0, mean(lowm[which(lowm$RSE==0),]$RLAL), pch=19, col='maroon', cex=1.5)
points(-0.5, mean(lowm[which(lowm$RSE==-0.5),]$RLAL), pch=19, col='maroon', cex=1.5)
points(-1, mean(lowm[which(lowm$RSE==-1),]$RLAL), pch=19, col='maroon', cex=1.5)
points(-1.5, mean(lowm[which(lowm$RSE==-1.5),]$RLAL), pch=19, col='maroon', cex=1.5)
points(-2, mean(lowm[which(lowm$RSE==-2),]$RLAL), pch=19, col='maroon', cex=1.5)
points(-2.5, mean(lowm[which(lowm$RSE==-2.5),]$RLAL), pch=19, col='maroon', cex=1.5)
abline(a=lm(RLAL~RSE, lowm)$coefficients[1], b=lm(RLAL~RSE, lowm)$coefficients[2], col='maroon', lty=3)

points(highm$RSE, highm$RLAL, pch=17, col='gray88')
points(-3, mean(highm[which(highm$RSE==-3),]$RLAL), pch=17, col='blue', cex=1.5)
points(-3.5, mean(highm[which(highm$RSE==-3.5),]$RLAL), pch=17, col='blue', cex=1.5)
points(-4, mean(highm[which(highm$RSE==-4),]$RLAL), pch=17, col='blue', cex=1.5)
points(-4.5, mean(highm[which(highm$RSE==-4.5),]$RLAL), pch=17, col='blue', cex=1.5)
abline(a=lm(RLAL~RSE, highm)$coefficients[1], b=lm(RLAL~RSE, highm)$coefficients[2], col='blue', lty=3)
legend('topleft', c('High Myopia: Mean LAL','Low Myopia: Mean LAL'), pch=c(17,19), bty='n', col=c('blue', 'maroon'), cex=1, text.col='gray')

boxplot(RLAL~RSE, rbind(lowm, highm), bty='n', frame=F, ylab='LAL (mm)', xlab='SE (D)', col='yellow', pch=19, main='Boxplot: LAL vs SE')
abline(v=3.5, lty=3, col='red')
legend('topleft','High', bty='n', cex=2.5, text.col='gray88')
legend('top','Low', bty='n', cex=2.5, text.col='gray88')

dev.off()


