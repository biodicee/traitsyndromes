
#Packages
########################
library(ggplot2)
library(reshape2)
#library(gridExtra)
#library(splitstackshape)
#library(extrafont)
#fonctions

root = "/home/sonia/Dropbox/Shared_SimonBenateau/MarinaProject/"
setwd(root)

source("code/Code_Flo/FonctionsSimon2.R")

#Where do I save my figures ?
dir.create("Figures/")

saveFolder <- "/Figures/"

path <- 'ToFloAndSonia/sim_16-05-09/results_file/' 

#figure
pdf(paste0(root, saveFolder,"fig1.pdf",sep = ""),  width = 9, height = 4)

layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(4,3,1,4), heights=c(1))


######################### 
####### Figure 1 A ########
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


# A. early survival function
############################

textcol<- c("green3","darkorange1")
par(mar=c(5,5,2,2))
# mar: bottom, left, top and right

plot(NA, xlim = c(0,1.2), ylim =c(0,1), ann = F, axes = F, asp = 1)
axis(1)
axis(2, las = 2)
mtext('A', adj = 0, cex = 1.5, at = -0.3, line = 1)


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



# B. plot environment
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


plotMatH(e_fac,type = 'env', text = FALSE)
par(new=TRUE)
plotMatH(matrixIni,border = "white")
mtext('B', adj = 0, cex = 1.5, at = -0.3, line = 1)

# *. Legend
#################
#legend (one plot)
plot(NA, xlim = c(0,1), ylim =c(0,1), ann = F, axes = F)
#legend('left', legend = sort(unique(e_fac)), fill = highlight(sort(unique(e_fac)), colrange = rev(grey.colors(5)), steps = 50, range = c(0.6, 0.8)),bty = 'n', title = expression(E[ij]))
mtext('C', adj = 0, cex = 1.5, at = -0.3, line = 1)

mar = 0.6
int = 0.02
space <- seq(0,1-mar,length.out = 6)
rect(0.4,space[-6]+int,0.6,space[-1]-int, col = highlight(sort(unique(e_fac)), colrange = rev(grey.colors(5)), steps = 50, range = c(0.6, 0.8)))
text(y = (space[-6] - space[-1])/2 + space[-1], x = 0.42, sort(unique(e_fac)), pos = 2)
text(y = (space[-6] - space[-1])/2 + space[-1], x = 0.58, seq(0,1, 0.25), pos = 4)
text(x = 0.2, y = 1-mar+0.025, expression(E[ij]), font = 2)
text(x = 0.8, y = 1-mar+0.025, expression(q[paste('+','|',0)[ij]]), font = 2)

# C. survival
#################
par(mar = c(5,5,2,2))
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

