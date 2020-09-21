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



vegetation <- colorRampPalette(c("#D4D4D4", "darkgreen") )


pdf(paste0(saveFolder,"fig3.pdf"), width = 7, height = 12)

#layout(matrix(c(1:12), ncol = 6, byrow = TRUE), widths = c(8,1,8,1,8,1))
layout(matrix(c(1:15), ncol = 3, byrow = TRUE), widths = c(5,5,2))


par(oma = c(2,2,5,1))

marlegend <- c(3,2,5,4)

# --

plotMatrixCF(repDf2,"group", range = c(0,8))
mtext('constant climate', adj = 0, cex = 1.2, at = 0, line = 2.5)
mtext('E = 0.6', adj = 0, cex = 1, at = 0, line = 0.5)

mtext('A', adj = 0, cex = 1.5, at = -0.35, line = -1.2)

mtext(side = 2, line = 3.5, 'local competition')
mtext(side = 2, line = 2, expression( italic(c[l])))

plotMatrixCF(repDfC2,"group", range = c(0,8))
mtext('F', adj = 0, cex = 1.5, at = -0.35, line = -1.2)
mtext('climate change', adj = 0, cex = 1.2, at = 0, line = 2.5)

#mtext(side = 1, line = 2.5, expression(italic(f)))

plot_colscale(repDf2$group, 
              colpal = rev(heat.colors(8)), range = c(0,8), mar = marlegend)
mtext('number of \n ecotypes', adj = 0, cex = 0.8, at = 0, line = 1)

# --

plotMatrixCF(repDf2,"h", rainbow(32), range = c(0, 15))
mtext('B', adj = 0, cex = 1.5, at = -0.35, line = -1.2)


mtext(side = 2, line = 3.5, 'local competition')
mtext(side = 2, line = 2, expression( italic(c[l])))

plotMatrixCF(repDfC2,"h", rainbow(32), range = c(0, 15))
mtext('G', adj = 0, cex = 1.5, at = -0.35, line = -1.2)

plot_colscale(repDf2$h, 
              colpal = rainbow(32), range = c(0,15),mar = marlegend)
mtext('average \n trait value', adj = 0, cex = 0.8, at = 0, line = 1)

# --

plotMatrixCF(repDf2,"loss", range = c(0,100), gray.colors(10))
mtext('C', adj = 0, cex = 1.5, at = -0.35, line = -1.2)

mtext(side = 2, line = 3.5, 'local competition')
mtext(side = 2, line = 2, expression( italic(c[l])))

plotMatrixCF(repDfC2,"loss", range = c(0,100), gray.colors(10))
mtext('H', adj = 0, cex = 1.5, at = -0.35, line = -1.2)

plot_colscale(repDfC2$loss, 
              colpal = (gray.colors(10)), at = c(0,25,50,75), labels = c('0%', '25%', '50%', '75%'), mar = marlegend)
mtext('stability of \n vegetation', adj = 0, cex = 0.8, at = 0, line = 1)


# --

repDf2$clus_cutoff <- repDf2$clus
repDf2$clus_cutoff[repDf2$clus >= 1.2 & !is.na(repDf2$clus)] <- 1.1999

repDfC2$clus_cutoff <- repDfC2$clus
repDfC2$clus_cutoff[repDfC2$clus >= 1.2 & !is.na(repDfC2$clus)] <- 1.1999


plotMatrixCF(repDf2,"clus_cutoff", rev(heat.colors(6)),steps = 6,  range = c(0.9,1.2))
mtext('D', adj = 0, cex = 1.5, at = -0.35, line = -1.2)

mtext(side = 2, line = 3.5, 'local competition')
mtext(side = 2, line = 2, expression( italic(c[l])))

plotMatrixCF(repDfC2,"clus_cutoff",rev(heat.colors(6)), steps = 6, range = c(0.9,1.2))
mtext('I', adj = 0, cex = 1.5, at = -0.35, line = -1.2)

plot_colscale(repDf2$clus_cutoff, 
              colpal = rev(heat.colors(6)), range = c(0.9,1.2), mar = marlegend, labels = c("0.9","1.0", "1.1", expression("">=1.2)), at = c(0.9,1,1.1,1.2))
mtext('average \n clustering', adj = 0, cex = 0.8, at = 0, line = 1)


# --


plotMatrixCF(repDf2,"rho", vegetation(5), steps = 5,  range = c(0, 1))
mtext('E', adj = 0, cex = 1.5, at = -0.35, line = -1.2)

mtext(side = 2, line = 3.5, 'local competition')
mtext(side = 2, line = 2, expression( italic(c[l])))
mtext(side = 1, line = 2, expression(italic(f)))
mtext(side = 1, line = 3.5, 'local facilitation')

plotMatrixCF(repDfC2,"rho", vegetation(5), steps = 5, range = c(0, 1))
mtext('J', adj = 0, cex = 1.5, at = -0.35, line = -1.2)
mtext(side = 1, line = 2, expression(italic(f)))
mtext(side = 1, line = 3.5, 'local facilitation')

plot_colscale(repDfC2$h, 
              colpal = vegetation(5), range = c(0,1),mar = marlegend, labels = c('0%', '20%', '40%', '60%', '80%', '100%'), at  = seq(0,1,0.2))
mtext('average \n veget. cover', adj = 0, cex = 0.8, at = 0, line = 1)


dev.off()
