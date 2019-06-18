###################################
#SCRIPT FIGURE
#
#   Authors : Flo, Marina & Simon
################################

rm(list = ls())

#Packages
########################
library(ggplot2)
library(reshape2)
library(gridExtra)
library(splitstackshape)
library(extrafont)
#fonctions

root = "/home/soniakefi/Dropbox/Shared_SimonBenateau/MarinaProject/"
setwd(root)

source("code/Code_Flo/FonctionsSimon2.R")

#Where do I save my figures ?
dir.create("Figures/Article6/")

saveFolder <- "/Figures/Article6/"

path <- 'ToFloAndSonia/sim_16-05-09/results_file/' 


######################### 
####### Figure 1 ########
### Survival function ###
#########################

#survival function
surv <- function(xw, a_ , b_, h_) {
  (a_*(xw^(h_)))/(1 + a_*b_*(xw^(h_)))
}

rescal = 3 #rescaling the Q data to be similar to the T values

# de 0 a -1 Mpa
tg <- (c(90,87,84,84,60,22,10,5,0,0))/100 # germination percentage
tw <- c(-0,-0.02,-0.05,-0.10,-0.31,-0.39,-0.49,-0.62,-0.78,-0.99) # water stress (MPa)
te <- (1+tw) # water availability

qgerm <- (c(30,28,20,22,22))/100  # germination percentage
qw <- c(0,-0.165,-0.256,-0.368,-0.5) # water stress
qe <- (1+qw)  # water availability
qg <-qgerm*rescal #rescaling the Q value to be similar to the T values

### Statistical model

tdata <- data.frame(tg,te)
qdata <- data.frame(qg,qe)

fit_T <- nls(tg~surv(te, 7.5, 1, h), data=data.frame(tdata), start=list(h=1))
fit_Q <- nls(qg~surv(qe, 7.5, 1, h), data=data.frame(qdata), start=list(h=1))

#figure
pdf(paste(saveFolder,"early_surv.pdf",sep = ""),  width = 7, height = 6, paper = "special")

textcol<- c("green3","darkorange1")
par(family="serif", mar=c(5,5,2,2))
# mar: bottom, left, top and right

plot(NA, xlim = c(0,1.2), ylim =c(0,1), ann = F, axes = F, asp = 1)
axis(1)
axis(2, las = 2)

points(tg~te, col = textcol[1], pch = 16, cex = 1.2)
points(qg~qe, col=textcol[2], pch=16, cex=1.2)
#plot(tg~te, ylim=c(0,1), xlim=c(0,1.2), col=textcol[1], pch = 16, cex=1.2,cex.lab=1.5, cex.axis=1.3,
#     xlab=expression(Water~availability~(italic(E))), ylab=expression(Early~Survival~italic(S(E,h))))

mtext(side = 1, line = 2, expression(Water~availability~(italic(E))))
mtext(side = 2, line = 2, expression(Early~Survival~italic(S(E,h))))

lines(seq(0,1.2, length = 120) , predict(fit_T, newdata = list(te = seq(0,1.2, length = 120))), col=textcol[1], lwd=2.5 )
lines(seq(0,1.2, length = 120) , predict(fit_Q, newdata = list(qe = seq(0,1.2, length = 120))), col=textcol[2], lwd=2.5 )

# Labels

text(0.64,0.15, labels = expression(bold("Tertiary")), col=textcol[1], cex=1.2)
text(0.64,0.10, labels = expression(bold(paste("(", h[T], "= 5.7)"))), col=textcol[1], cex=1.2)

text(0.47,0.35, labels = expression(bold("Quaternary")),  col=textcol[2], cex=1.2)
text(0.47,0.3, labels = expression(bold(paste("(", h[Q], "= 2.4)"))), col=textcol[2], cex=1.2)

for (i in 2) # seeing other h values
  lines(surv(seq(0,1.2, length = 120),7.5,1,i)~seq(0,1.2, length = 120), lwd=2, col="grey60")
#text(0.13,0.6, labels = expression(bold("h=1")), col="grey40", cex=1.2)
text(0.23,0.4, labels = expression(bold("h=2")), col="grey40", cex=1.2)

dev.off()


######################### 
####### Figure 2 ########
# Facilitation mecanism #
#########################

# A. plot environment
#################

#parameters
eo = 0.6
f = 0.5
emax = 1.2
a_s = 7.5
g_s = 1
c_intra = 0.2
c_glob = 0.2
B = 0.8
del = 0.1

#lattice
matrixIni <- c(6, NA, 6, NA, NA, NA, NA,
               6, 6, NA, 6, 6, 6, 6,
               6, NA, NA, NA, 6, 6, 6,
               NA, NA, NA, NA, 6, NA, 6,
               6, 6, NA, 6, NA, 6, NA,
               6, NA, NA, NA, NA, NA, NA,
               NA, NA, NA, NA, NA, NA, NA)

#plot environment
width = height = sqrt(length(matrixIni))

#get neighbours
interact <- setInteract(width,height)$interact
x_with_border <- setInteract(width,height)$x_with_border
x_to_evaluate <- setInteract(width,height)$x_to_evaluate

Qtt <- rowSums(sapply(interact,	function(j) (!is.na(matrixIni))[x_with_border][x_to_evaluate+j] ))/4 
e_fac <- eo*(1 + (Qtt*(emax-eo)*f))

pdf(paste(saveFolder,"env_surv.pdf",sep = ""),  width = 7, height = 3, paper = "special")

layout(matrix(c(1,2,3), 1, 3, byrow = TRUE), widths=c(2,1,3), heights=c(1))
plotMatH(e_fac,type = 'env', text = FALSE)
par(new=TRUE)
plotMatH(matrixIni,border = "white")

# *. Legend
#################
#legend (one plot)
plot(NA, xlim = c(0,1), ylim =c(0,1), ann = F, axes = F)
#legend('left', legend = sort(unique(e_fac)), fill = highlight(sort(unique(e_fac)), colrange = rev(grey.colors(5)), steps = 50, range = c(0.6, 0.8)),bty = 'n', title = expression(E[ij]))


mar = 0.6
int = 0.02
space <- seq(0,1-mar,length.out = 6)
rect(0.4,space[-6]+int,0.6,space[-1]-int, col = highlight(sort(unique(e_fac)), colrange = rev(grey.colors(5)), steps = 50, range = c(0.6, 0.8)))
text(y = (space[-6] - space[-1])/2 + space[-1], x = 0.42, sort(unique(e_fac)), pos = 2)
text(y = (space[-6] - space[-1])/2 + space[-1], x = 0.58, seq(0,1, 0.25), pos = 4)
text(x = 0.2, y = 1-mar+0.025, expression(E[ij]), font = 2)
text(x = 0.8, y = 1-mar+0.025, expression(q[paste('+','|',0)[ij]]), font = 2)

# B. survival
#################
par(mar = c(4,4.5,3,0))
h_new <- seq(2,14,length.out = 1000)
eo = 0.6

Qtt = 0:4
emax = 1.2
f = 0.5

a_s = 7.5
g_s = 1


plot(h_new,seq(0,1,length.out = 1000), type = 'n', bty = 'n', xlab = expression(paste('Trait values', (h))), ylab = 'Early survival (S(E,h))', axes = FALSE)
axis(1, at = seq(2,14,4) )
axis(2, las = TRUE, at = seq(0,1,0.5))

colorPlot <- highlight(sort(unique(e_fac)), colrange = rev(grey.colors(5)), steps = 50, range = c(0.6, 0.8))
stoMat <- matrix(NA, nrow = 5, ncol = 1000)
for (i in 1:5){
  e_fac <- eo*(1 + (Qtt[i]*(emax-eo)*f))
  surv <- a_s*(e_fac^(h_new))/(1+(g_s*a_s*(e_fac^(h_new))))
  lines(h_new, surv, col = colorPlot[i], lwd = 3)
  stoMat[i, ] <- surv
}

dev.off()


######################### 
####### Figure 3 ########
# Evolutionary dynamics #
####### Constant ########
###### Wet vs Dry #######
#########################


#Evolutionary dynamics
##################

#pdf(paste(root, saveFolder,"FigDryWet.pdf",sep = ""),  width = 7, height = 9,  paper = "special")
png(paste(root, saveFolder,"FigDryWet.png",sep = ""), width = 7, height = 9, units = "in", res = 300)
layout(matrix(c(1, 3, 2, 3, 4, 6, 5, 6, 7,9,8,9), 6, 2, byrow = TRUE), widths=c(2, 1, 2, 1), heights = c(5, 4, 5, 4 ,5,4))

#Evolution in constant environment E0 = 1.1
#----------------------------------------

load(paste0(root, path ,'result3'))

#cat(paste0("$h = ", round(mean(result$h[[401]][result$h[[401]] != 0]),2), " \\pm ", round(sd(result$h[[401]][result$h[[401]] != 0]), 2), "$"))
#eco_c <- result$h[[401]][result$h[[401]] > 1 & result$h[[401]] < 6 ]
#eco_b <- result$h[[401]][result$h[[401]] > 6 & result$h[[401]] < 9 ]
#eco_a <- result$h[[401]][result$h[[401]] > 9]
#cat(paste0("$h = ", round(mean(eco_a),2), " \\pm ", round(sd(eco_a), 2), "$"))
#cat(paste0("$h = ", round(mean(eco_b),2), " \\pm ", round(sd(eco_b), 2), "$"))
#cat(paste0("$h = ", round(mean(eco_c),2), " \\pm ", round(sd(eco_c), 2), "$"))


plotH()
#ecotype labels
text(c(rep(200000,3),78000), c(10,6.7,5.1,5.7), letters[1:4], xpd = TRUE, pos = 4, font = 2)
#label
PlotLabel = 'a)'
text(x = -28000, y = 15, PlotLabel, font = 2, cex = 1.75, xpd = TRUE, pos = 2)
plotRho()
plotMatH(result$h_final)


#Evolution in constant environment E0 = 0.9
#----------------------------------------

load(paste0(root, path,'result124'))
plotH()

#cat(paste0("$h = ", round(mean(result$h[[401]][result$h[[401]] != 0]),2), " \\pm ", round(sd(result$h[[401]][result$h[[401]] != 0]), 2), "$"))
#eco_g <- result$h[[401]][result$h[[401]] > 1 & result$h[[401]] < 2.6 ]
#eco_f <- result$h[[401]][result$h[[401]] > 2.6 & result$h[[401]] < 3.6 ]
#eco_e <- result$h[[401]][result$h[[401]] > 4]
#eco_d <- result$h[[321]][result$h[[321]] > 5.6  ]

#cat(paste0("$h = ", round(mean(eco_d),2), " \\pm ", round(sd(eco_d), 2), "$"))
#cat(paste0("$h = ", round(mean(eco_e),2), " \\pm ", round(sd(eco_e), 2), "$"))
#cat(paste0("$h = ", round(mean(eco_f),2), " \\pm ", round(sd(eco_f), 2), "$"))
#cat(paste0("$h = ", round(mean(eco_g),2), " \\pm ", round(sd(eco_g), 2), "$"))

#ecotype labels
text(c(180000,rep(200000,3)), c(7.43,4.82,3.03,2.145), letters[5:8], xpd = TRUE, pos = 4, font = 2)
#label
PlotLabel = 'b)'
text(x = -28000, y = 15, PlotLabel, font = 2, cex = 1.75, xpd = TRUE, pos = 2)
plotRho()
plotMatH(result$h_final)

#Evolution in constant environment E0 = 0.6
#----------------------------------------

load(paste0(root, path,'result366'))

#cat(paste0("$h = ", round(mean(result$h[[401]][result$h[[401]] != 0]),2), " \\pm ", round(sd(result$h[[401]][result$h[[401]] != 0]), 2), "$"))
mean(result$rho$T[1950:2001])
plotH()
#ecotype labels
text(c(rep(200000,1)), c(2.145), letters[9], xpd = TRUE, pos = 4, font = 2)
#label
PlotLabel = 'c)'
text(x = -28000, y = 15, PlotLabel, font = 2, cex = 1.75, xpd = TRUE, pos = 2)
plotRho()
plotMatH(result$h_final)

dev.off()

######################### 
##### Figure sup1 #######
# Evolutionary dynamics #
####### Constant ########
## Facilitation vs No ###
#########################

pdf(paste(saveFolder,"FigDryFac.pdf",sep = ""),  width = 7, height = 7, paper = "special")
layout(matrix(c(1, 3, 2, 3, 4, 6, 5, 6), 4, 2, byrow = TRUE), widths=c(2, 1), heights = c(5, 4, 5, 4))

#Evolution in constant environment E0 = 0.6 Without facilitation
#-----------------------------------------
load(paste(path,'result366',sep = ""))
plotH()
#ecotype labels
text(200000, 2.1, letters[4], xpd = TRUE, pos = 4, font = 2)
#label
PlotLabel = 'A.'
text(x = -28000, y = 15, PlotLabel, font = 2, cex = 1.75, xpd = TRUE, pos = 2)
plotRho()
plotMatH(result$h_final)

#Evolution in constant environment E0 = 0.6 With facilitation (f = 0.4)
#-----------------------------------------
load(paste(path,'result410',sep = ""))
plotH()
#ecotype labels
text(c(rep(200000,4),79500,145500), c(5.2,4.25,3.3,2.1,5.4,4.5), letters[1:6], xpd = TRUE, pos = 4, font = 2)
#label
PlotLabel = 'B.'
text(x = -28000, y = 15, PlotLabel, font = 2, cex = 1.75, xpd = TRUE, pos = 2)
plotRho()
plotMatH(result$h_final)
dev.off()

######################### 
####### Figure 4 ########
# Evolutionary dynamics #
#### Climate change #####
#########################

pdf(paste(saveFolder,"climatechange.pdf",sep = ""),  width = 7, height = 4, paper = "special")
layout(matrix(c(1, 3, 2, 3), 2, 2, byrow = TRUE), widths=c(2, 1), heights = c(5, 4))

load("~/Documents/simulation Simon/sim_16-06-27_facil0.4/results_file/result86")
plotH()
#ecotype labels
#text(c(rep(200000,4),79500,145500), c(5.2,4.25,3.3,2.1,5.4,4.5), letters[1:6], xpd = TRUE, pos = 4, font = 2)
#label
#PlotLabel = 'B.'
#text(x = -28000, y = 15, PlotLabel, font = 2, cex = 1.75, xpd = TRUE, pos = 2)
plotRho()
plotMatH(result$h_final)
dev.off()


######################### 
##### Figure 4 bis ######
# Evolutionary dynamics #
#### Climate change #####
#### Facil vs Comp ######
#########################

pdf(paste(saveFolder,"climatechange2.pdf",sep = ""),  width = 7, height = 10.5, paper = "special")
par(oma = c(4.5,0,0,1))
#layout(matrix(c(1, 3, 2, 3, 4, 6, 5, 6, 7 , 9, 8, 9), 6, 2, byrow = TRUE), widths=c(2, 1), heights = c(5, 4, 5, 4, 5, 4)) #plot all
layout(matrix(c(1, 2, 3), 3, 1, byrow = TRUE), widths= 1, heights = 1) #plot only h

#without facilitation
#-----------------------------------------
load("~/Documents/simulation Simon/sim_16-05-09_4/results_file/result3")
plotH()
#ecotype labels
#text(c(rep(200000,4),79500,145500), c(5.2,4.25,3.3,2.1,5.4,4.5), letters[1:6], xpd = TRUE, pos = 4, font = 2)
#label
PlotLabel = 'A.'
text(x = -28000, y = 15, PlotLabel, font = 2, cex = 1.75, xpd = TRUE, pos = 2)
#plotRho()
#plotMatH(result$h_final)

#without competition
#-----------------------------------------
load("~/Documents/simulation Simon/sim_16-05-09_4/results_file/result45")
plotH()
#ecotype labels
#text(c(rep(200000,4),79500,145500), c(5.2,4.25,3.3,2.1,5.4,4.5), letters[1:6], xpd = TRUE, pos = 4, font = 2)
#label
PlotLabel = 'B.'
text(x = -28000, y = 15, PlotLabel, font = 2, cex = 1.75, xpd = TRUE, pos = 2)
#plotRho()
#plotMatH(result$h_final)

#with competition and facilitation
#-----------------------------------------
load("~/Documents/simulation Simon/sim_16-06-27_facil0.4/results_file/result86")
plotH(xax = TRUE)
#ecotype labels
#text(c(rep(200000,4),79500,145500), c(5.2,4.25,3.3,2.1,5.4,4.5), letters[1:6], xpd = TRUE, pos = 4, font = 2)
#label
PlotLabel = 'C.'
text(x = -28000, y = 15, PlotLabel, font = 2, cex = 1.75, xpd = TRUE, pos = 2)
#plotRho()
#plotMatH(result$h_final)

dev.off()

#########################
####### Figure 5 ########
####### Matrices ########
#### Climate change #####
###### Facil/Comp #######
#########################


#traits + mean + sd

# merge data from all folders |constant climate|
folders <- c('~/Dropbox/Shared_SimonBenateau/MarinaProject/ToFloAndSonia/sim_16-05-09/results_file/'
             ,'~/Dropbox/Shared_SimonBenateau/MarinaProject/ToFloAndSonia/sim_16-06-20/results_file/'
             ,'~/Dropbox/Shared_SimonBenateau/MarinaProject/ToFloAndSonia/sim_16-07-05/results_file/'
)

repDf <- extractData(folders, extH = TRUE)
#filter environment
repDf1 <- subset(repDf$stoDf, env == 0.6)
repDf2 <- ddply(repDf1, .variables = c("c","f"), meanh)

# merge data from all folders |climate change|
folders <- c('~/Documents/simulation Simon/sim_16-05-09_4/results_file/'
             ,'~/Documents/simulation Simon/sim_16-07-08/results_file/'
)

repDfC <- extractData(folders, extH = TRUE)
#filter environment
repDfC2 <- ddply(repDfC$stoDf, .variables = c("c","f"), meanh)


pdf(paste0(saveFolder,"constantGroupH.pdf"), width = 14, height = 7)
par(mfrow = c(1,2))
plotMatrixCF(repDf2,"group", bars = FALSE)
plotMatrixCF(repDf2,"h", bars = FALSE)
dev.off()

pdf(paste0(saveFolder,"changeGroupH.pdf"), width = 14, height = 7)
par(mfrow = c(1,2))
plotMatrixCF(repDfC2,"h", bars = FALSE)
plotMatrixCF(repDfC2,"group", bars = FALSE)
dev.off()

pdf(paste0(saveFolder,"constantRhoClus.pdf"), width = 14, height = 7)
par(mfrow = c(1,2))
plotMatrixCF(repDf2,"clus", bars = FALSE)
plotMatrixCF(repDf2,"rho", bars = FALSE)
dev.off()

pdf(paste0(saveFolder,"changeRhoClus.pdf"), width = 14, height = 7)
par(mfrow = c(1,2))
plotMatrixCF(repDfC2,"clus", bars = FALSE)
plotMatrixCF(repDfC2,"rho", bars = FALSE)
dev.off()

plotMatrixCF(repDf2,"groupsd", bars = FALSE)
plotMatrixCF(repDf2,"hsd", bars = FALSE)
plotMatrixCF(repDfC2,"count", bars = FALSE)
plotMatrixCF(repDfC2,"countNoNA", bars = FALSE)

DataPlotHD <- subset(repDf$hDf, env == 0.6)
DataPlotHD <- DataPlotHD[DataPlotHD$h != 0,]

DataPlotHD <- repDfC$hDf
DataPlotHD <- DataPlotHD[DataPlotHD$h != 0,]

error <- function (df) {
  data.frame(mean = mean(df[ ,"h"]),
             high = mean(df[ ,"h"]) + sd(df[ ,"h"]),
             low = mean(df[ ,"h"]) - sd(df[ ,"h"]),
             median = median(df[ ,"h"]),
             quant0 = quantile(df[ ,"h"])[1],
             quant25 = quantile(df[ ,"h"])[2],
             quant50 = quantile(df[ ,"h"])[3],
             quant75 = quantile(df[ ,"h"])[4],
             quant100 = quantile(df[ ,"h"])[5])
}

errorBars <- ddply(DataPlotHD, c("c","f"),error)

#supplementary figure
pdf(paste0(saveFolder,"finalH.pdf"), width = 14, height = 5)
fac <- unique(DataPlotHD$f)
par(mfrow = c(1,length(fac)), oma = c(4,3,1,1), mar = c(0,1,0,0))
for (i in fac){
  DataPlotHDfac <- subset(DataPlotHD, f == i)
  errorBarsI <- subset(errorBars, f == i)
  colPlot <- highlight(DataPlotHDfac$h, colrange = rainbow(32), steps = 50, range = c(0, 15))
  plot((DataPlotHDfac$c + rnorm(length(DataPlotHDfac$c),0,0.05/8))
       ,DataPlotHDfac$h
       , ann = FALSE, axes = FALSE
       , ylim = c(min(DataPlotHD$h),max(DataPlotHD$h))
       , xlim = c(-0.05,0.55)
       , col = colPlot, pch = 16, cex = 0.5)
  #points(errorBarsI$c,errorBarsI$mean, pch = 16, cex = 0.7)
  points(errorBarsI$c,errorBarsI$median, pch = 16, cex = 0.7, col = 2)
  #segments(errorBarsI$c,errorBarsI$low,errorBarsI$c,errorBarsI$high)
  segments(errorBarsI$c,errorBarsI$quant25,errorBarsI$c,errorBarsI$quant75)
  if (i == min(fac)){
    mtext(side = 2, line = 2, expression(Trait~values~(italic(h))))
    mtext(side = 1, line = 3, expression(Facilitation~(italic(f))),outer = TRUE)
    axis(2, las = 2, tck = -0.08)
  } else {
    axis(2, labels = FALSE, tck = -0.08)
  }
  axis(1, tck = -0.08, at = seq(0,0.5,0.25), labels = c('0', '0.25', '0.5'))
}
dev.off()
  
#verif des différences entre les valeurs de h obtenues avec h = 2.4 et h = 6
for (i in seq(0,1,.1)){
  for (j in seq(0,0.5,0.05)){
    par(mfrow = c(1,2))
    DataPlotHDVerif <- subset(DataPlotHD, c == j & f == i)
    DataPlotHDVerif6 <- subset(DataPlotHD, ho == 6 & c == j & f == i)
    DataPlotHDVerif2 <- subset(DataPlotHD, ho == 2.4 & c == j & f == i)
    if (length(DataPlotHDVerif6$h[DataPlotHDVerif6$h != 0]) != 0){
      plot(density(DataPlotHDVerif6$h[DataPlotHDVerif6$h != 0])$y,density(DataPlotHDVerif6$h[DataPlotHDVerif6$h != 0])$x,type = 'l', lwd = 2)
      lim <- par("usr")[3:4]
      for (k in 1:4){
        evalH <- DataPlotHDVerif6$h[seq(((k-1)*2500+1), k*2500)]
        if (length(evalH[evalH != 0])){
        lines(density(evalH[evalH != 0])$y,density(evalH[evalH != 0])$x, col = "grey", lwd = 2)
        }
      }
      lim <- par("usr")[3:4]
      lines(density(DataPlotHDVerif2$h[DataPlotHDVerif2$h != 0])$y,density(DataPlotHDVerif2$h[DataPlotHDVerif2$h != 0])$x, col = 2, lwd = 2)
      plot(sort(DataPlotHDVerif6$h), ylim = lim)
      points(seq(0,length(DataPlotHDVerif6$h),length.out = length(DataPlotHDVerif2$h)),sort(DataPlotHDVerif2$h), col = 2)
      title(paste("facil =",i, "comp =",j))
    }
  }
}


