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
#repDf2$clus[repDf2$clus >= 2.5 & !is.na(repDf2$clus)] <- 2.4999


parameters <- read.table('simresults/change/parameters.txt', header =TRUE)

#extractData() sequentially extracts information from all result files, which takes some time
repDfC <- extractData('simresults/change/results_file/', extH = TRUE)
#filter environment
repDfC2 <- ddply(subset(repDfC$stoDf, ho == 6.0) , .variables = c("c","f"), meanh)
repDfC2$loss <- repDfC2$countNoNA/repDfC2$count*100

#repDfC2$clus[repDfC2$clus >= 2.5 & !is.na(repDfC2$clus)] <- 2.4999


vegetation <- colorRampPalette(c("#D4D4D4", "darkgreen") )


pdf(paste0(saveFolder,"fig5.pdf"), width = 6.5, height = 6)

layout(matrix(c(1,2,0,3,4,5,6,0,7,8), ncol = 5, byrow = TRUE), widths = c(8,1,1,8,1))
par(oma = c(1,2,0,1))

repDf2$clus_cutoff <- repDf2$clus
repDf2$clus_cutoff[repDf2$clus >= 1.2 & !is.na(repDf2$clus)] <- 1.1999

repDfC2$clus_cutoff <- repDfC2$clus
repDfC2$clus_cutoff[repDfC2$clus >= 1.2 & !is.na(repDfC2$clus)] <- 1.1999


plotMatrixCF(repDf2,"clus_cutoff", rev(heat.colors(6)),steps = 6,  range = c(0.9,1.2))
mtext('A', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('aver. clustering', adj = 0, cex = 1, at = 0, line = 1)
plot_colscale(repDf2$clus_cutoff, 
              colpal = rev(heat.colors(6)), range = c(0.9,1.2), mar = c(4,0,8,2), labels = c("0.9","1.0", "1.1", expression("">=1.2)), at = c(0.9,1,1.1,1.2))

plotMatrixCF(repDf2,"rho", vegetation(5), steps = 5,  range = c(0, 1))
mtext('B', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('aver. vegetation cover', adj = 0, cex = 1, at = 0, line = 1)
plot_colscale(repDf2$h, 
              colpal = vegetation(5), range = c(0,1),mar = c(4,0,8,2))

plotMatrixCF(repDfC2,"clus_cutoff",rev(heat.colors(6)), steps = 6, range = c(0.9,1.2))
mtext('C', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('aver. clustering', adj = 0, cex = 1, at = 0, line = 1)
plot_colscale(repDf2$clus_cutoff, 
              colpal = rev(heat.colors(6)), range = c(0.9,1.2), mar = c(4,0,8,2), labels = c("0.9","1.0", "1.1", expression("">=1.2)), at = c(0.9,1,1.1,1.2))

plotMatrixCF(repDfC2,"rho", vegetation(5), steps = 5, range = c(0, 1))
mtext('D', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('aver. vegetation cover', adj = 0, cex = 1, at = 0, line = 1)
plot_colscale(repDfC2$h, 
              colpal = vegetation(5), range = c(0,1),mar = c(4,0,8,2))

dev.off()
