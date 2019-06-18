# Code for reproducing fig 4 on climate change scenario

library(ggplot2)
library(reshape2)
library(gridExtra)
library(splitstackshape)
library(extrafont)

root = "/home/sonia/Dropbox/Shared_SimonBenateau/MarinaProject"

setwd(root)

source("code/Code_Flo/FonctionsSimon2.R")

# results of climate-change scenario are found in 
simname <- c('sim_16-05-09', 'sim_16-06-20')  #  'sim_16-07-05'

# Where do I save my figures ?
if(!dir.exists("Figures/")) dir.create("Figures/")
saveFolder <- "Figures/"


path <- paste0('/ToFloAndSonia/', simname)

# parameters <- rbind(read.table(paste0(root,path[1],'/parameters.txt'), header =TRUE),
#                     read.table(paste0(root,path[2],'/parameters.txt'), header =TRUE),
#                     read.table(paste0(root,path[3],'/parameters.txt'), header =TRUE))

#traits + mean + sd

# merge data from all folders |constant climate|
folders <- paste0(root, path, '/results_file/')

#extractData() sequentially extracts information from all result files, which takes some time
repDf <- extractData(folders, extH = TRUE)
repDf1 <- subset(repDf$stoDf, env == 0.6 & ho == 6.0)
repDf2 <- ddply(repDf1, .variables = c("c","f"), meanh)
repDf2$clus[repDf2$clus >= 2.5 & !is.na(repDf2$clus)] <- 2.4999
setwd(root)


# results of climate-change scenario are found in 
simname <- 'FullClimateChangeSimuv3'

path <- paste0("/data/",simname)
# Where do I save my figures ?
if(!dir.exists("Figures/")) dir.create("Figures/")
saveFolder <- "Figures/"

parameters <- read.table(paste0(root,path,'/parameters.txt'), header =TRUE)

#extractData() sequentially extracts information from all result files, which takes some time
repDfC <- extractData(paste0(root,path, "/results_file/"), extH = TRUE)
#filter environment
repDfC2 <- ddply(subset(repDfC$stoDf, ho == 6.0) , .variables = c("c","f"), meanh)
repDfC2$loss <- repDfC2$countNoNA/repDfC2$count*100

repDfC2$clus[repDfC2$clus >= 2.5 & !is.na(repDfC2$clus)] <- 2.4999


vegetation <- colorRampPalette(c("#D4D4D4", "darkgreen") )


pdf(paste0(saveFolder,"figA5.pdf"), width = 6.5, height = 6)

layout(matrix(c(1,2,0,3,4,5,6,0,7,8), ncol = 5, byrow = TRUE), widths = c(8,1,1,8,1))
par(oma = c(1,2,0,1))

plotMatrixCF(repDf2,"clus", rev(heat.colors(10)),steps = 10,  range = c(0,2.5))
mtext('A', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('aver. clustering', adj = 0, cex = 1, at = 0, line = 1)
plot_colscale(repDf2$group, 
              colpal = rev(heat.colors(10)), range = c(0,2.5), mar = c(4,0,8,2))

plotMatrixCF(repDf2,"rho", vegetation(5), steps = 5,  range = c(0, 1))
mtext('B', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('aver. vegetation cover', adj = 0, cex = 1, at = 0, line = 1)
plot_colscale(repDf2$h, 
              colpal = vegetation(5), range = c(0,1),mar = c(4,0,8,2))

plotMatrixCF(repDfC2,"clus",rev(heat.colors(10)), steps = 10, range = c(0,2.5))
mtext('C', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('aver. clustering', adj = 0, cex = 1, at = 0, line = 1)
plot_colscale(repDfC2$group, 
              colpal = rev(heat.colors(10)), range = c(0,2.5), mar = c(4,0,8,2))

plotMatrixCF(repDfC2,"rho", vegetation(5), steps = 5, range = c(0, 1))
mtext('D', adj = 0, cex = 1.5, at = -0.3, line = 1)
mtext('aver. vegetation cover', adj = 0, cex = 1, at = 0, line = 1)
plot_colscale(repDfC2$h, 
              colpal = vegetation(5), range = c(0,1),mar = c(4,0,8,2))

dev.off()
