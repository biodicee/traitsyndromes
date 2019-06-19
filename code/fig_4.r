
######################### 
##### Figure 4 bis ######
# Evolutionary dynamics #
#### Climate change #####
#### Facil vs Comp ######
#########################


source("code/functions.R")

# Where do I save my figures ?
if(!dir.exists("figures/")) dir.create("figures/")
saveFolder <- "figures/"


parameters <- read.table('simresults/change/parameters.txt', header =TRUE)



png(paste(saveFolder,"fig4.png",sep = ""),  width = 7, height = 6.5, units = "in", res = 300)

par(oma = c(4.5,0,0,1))
#layout(matrix(c(1, 3, 2, 3, 4, 6, 5, 6, 7 , 9, 8, 9), 6, 2, byrow = TRUE), widths=c(2, 1), heights = c(5, 4, 5, 4, 5, 4)) #plot all
layout(matrix(c(1, 2, 3), 3, 1, byrow = TRUE), widths= 1, heights = 1) #plot only h

par(mar=c(1,6,1,0))
#without facilitation
#-----------------------------------------
load(paste0("simresults/change/results_file/result", parameters$ID[parameters$c_intra == 0.1 & parameters$f == 0][4]) )
plotH()
#ecotype labels
#text(c(rep(200000,4),79500,145500), c(5.2,4.25,3.3,2.1,5.4,4.5), letters[1:6], xpd = TRUE, pos = 4, font = 2)
#label
PlotLabel = 'A'
text(x = -28000, y = 15, PlotLabel, cex = 1.75, xpd = TRUE, pos = 2)
#plotRho()
#plotMatH(result$h_final)

#without competition
#-----------------------------------------
load(paste0("simresults/change/results_file/result", parameters$ID[parameters$c_intra == 0 & parameters$f == 0.4][4]) )
plotH()
#ecotype labels
#text(c(rep(200000,4),79500,145500), c(5.2,4.25,3.3,2.1,5.4,4.5), letters[1:6], xpd = TRUE, pos = 4, font = 2)
#label
PlotLabel = 'B'
text(x = -28000, y = 15, PlotLabel, cex = 1.75, xpd = TRUE, pos = 2)
#plotRho()
#plotMatH(result$h_final)

#with competition and facilitation
#-----------------------------------------
load(paste0("simresults/change/results_file/result", parameters$ID[parameters$c_intra == 0.1 & parameters$f == 0.4][8]) )
plotH(xax = TRUE)
#ecotype labels
#text(c(rep(200000,4),79500,145500), c(5.2,4.25,3.3,2.1,5.4,4.5), letters[1:6], xpd = TRUE, pos = 4, font = 2)
#label
PlotLabel = 'C'
text(x = -28000, y = 15, PlotLabel, cex = 1.75, xpd = TRUE, pos = 2)
#plotRho()
#plotMatH(result$h_final)

dev.off()
