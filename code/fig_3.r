# Code for reproducing fig 4 on climate change scenario

#library(ggplot2)
#library(reshape2)
#library(gridExtra)
#library(splitstackshape)
#library(extrafont)

#root = "/home/sonia/Dropbox/Shared_SimonBenateau/MarinaProject"

#setwd(root)

source("code/functions.R")

# Where do I save my figures ?
if(!dir.exists("figures/")) dir.create("figures/")
saveFolder <- "figures/"

#extractData() sequentially extracts information from all result files, which takes some time
repDf <- extractData('simresults/constant/results_file/', extH = TRUE)
repDf1 <- subset(repDf$stoDf, env == 0.6 & ho == 6.0)
repDf2 <- ddply(repDf1, .variables = c("c","f"), meanh)
repDf2$loss <- repDf2$countNoNA/repDf2$count*100


parameters <- read.table('simresults/change/parameters.txt', header =TRUE)

#extractData() sequentially extracts information from all result files, which takes some time
repDfC <- extractData('simresults/change/results_file/', extH = TRUE)
#filter environment
repDfC2 <- ddply(subset(repDfC$stoDf, ho == 6.0) , .variables = c("c","f"), meanh)
repDfC2$loss <- repDfC2$countNoNA/repDfC2$count*100




pdf(paste0(saveFolder,"fig3.pdf"), width = 9, height = 6)

layout(matrix(c(1:12), ncol = 6, byrow = TRUE), widths = c(8,1,8,1,8,1))
par(oma = c(1,1,0,1))
plotMatrixCF(repDf2,"group", range = c(0,8))
mtext('A', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('num. ecotypes', adj = 0, cex = 1, at = 0, line = 1)

plot_colscale(repDf2$group, 
              colpal = rev(heat.colors(32)), range = c(0,8), mar = c(4,0,8,2))

plotMatrixCF(repDf2,"h", rainbow(32), range = c(0, 15))
mtext('B', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('aver. trait value', adj = 0, cex = 1, at = 0, line = 1)

plot_colscale(repDf2$h, 
              colpal = rainbow(32), range = c(0,15),mar = c(4,0,8,2))

plotMatrixCF(repDf2,"loss", range = c(0,100), gray.colors(10))
mtext('C', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('stability of vegetation', adj = 0, cex = 1, at = 0, line = 1)

plot_colscale(repDf2$loss, 
              colpal = (gray.colors(10)), at = c(0,25,50,75), mar = c(4,0,8,2))

plotMatrixCF(repDfC2,"group", range = c(0,8))
mtext('D', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('num. ecotypes', adj = 0, cex = 1, at = 0, line = 1)

plot_colscale(repDfC2$group, 
              colpal = rev(heat.colors(32)), range = c(0,8), mar = c(4,0,8,2))

plotMatrixCF(repDfC2,"h", rainbow(32), range = c(0, 15))
mtext('E', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('aver. trait value', adj = 0, cex = 1, at = 0, line = 1)

plot_colscale(repDfC2$h, 
              colpal = rainbow(32), range = c(0,15),mar = c(4,0,8,2))

plotMatrixCF(repDfC2,"loss", range = c(0,100), gray.colors(10))
mtext('F', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('stability of vegetation', adj = 0, cex = 1, at = 0, line = 1)

plot_colscale(repDfC2$loss, 
              colpal = (gray.colors(10)), at = c(0,25,50,75), mar = c(4,0,8,2))

dev.off()
