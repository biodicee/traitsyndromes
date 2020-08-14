

######################### 
####### Figure 2 ########
# Evolutionary dynamics #
####### Constant ########
###### Wet vs Dry #######
#########################


source("code/functions.R")

# Where do I save my figures ?
if(!dir.exists("figures/")) dir.create("figures/")
saveFolder <- "figures/"

#Evolutionary dynamics
##################

#pdf(paste(saveFolder,"fig2.pdf",sep = ""),  width = 7, height = 9,  paper = "special")
png(paste(saveFolder,"fig2.png",sep = ""), width = 8, height = 9, units = "in", res = 300)
 
par(oma = c(0,1,0,0))
layout(matrix(c(1, 3, 2, 3, 4, 6, 5, 6, 7,9,8,9), 6, 2, byrow = TRUE), widths=c(2, 0.5), heights = c(5, 4, 5, 4 ,5,4))

#Evolution in constant environment E0 = 1.1
#----------------------------------------


load('simresults/example/result3')

#cat(paste0("$h = ", round(mean(result$h[[401]][result$h[[401]] != 0]),2), " \\pm ", round(sd(result$h[[401]][result$h[[401]] != 0]), 2), "$"))
#eco_c <- result$h[[401]][result$h[[401]] > 1 & result$h[[401]] < 6 ]
#eco_b <- result$h[[401]][result$h[[401]] > 6 & result$h[[401]] < 9 ]
#eco_a <- result$h[[401]][result$h[[401]] > 9]
#cat(paste0("$h = ", round(mean(eco_a),2), " \\pm ", round(sd(eco_a), 2), "$"))
#cat(paste0("$h = ", round(mean(eco_b),2), " \\pm ", round(sd(eco_b), 2), "$"))
#cat(paste0("$h = ", round(mean(eco_c),2), " \\pm ", round(sd(eco_c), 2), "$"))

plotH()
#ecotype labels
text(c(rep(200000,3),78000), c(10,6.7,5.1,5.7), tolower(as.roman(1:4)), xpd = TRUE, pos = 4)
#label
PlotLabel = 'A'
text(x = -32000, y = 15, PlotLabel, cex = 1.75, xpd = TRUE, pos = 2)
plotRho()
par(mar = c(0,0,6,1))
plotMatH(result$h_final)


#Evolution in constant environment E0 = 0.9
#----------------------------------------

load('simresults/example/result124')
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
text(c(180000,rep(200000,3)), c(7.43,4.82,3.03,2.145), tolower(as.roman(5:8)), xpd = TRUE, pos = 4)
#label
PlotLabel = 'B'
text(x = -32000, y = 15, PlotLabel, cex = 1.75, xpd = TRUE, pos = 2)
plotRho()
par(mar = c(0,0,6,1))
plotMatH(result$h_final)

#Evolution in constant environment E0 = 0.6
#----------------------------------------

load('simresults/example/result366')

#cat(paste0("$h = ", round(mean(result$h[[401]][result$h[[401]] != 0]),2), " \\pm ", round(sd(result$h[[401]][result$h[[401]] != 0]), 2), "$"))
mean(result$rho$T[1950:2001])
plotH()
#ecotype labels
text(c(rep(200000,1)), c(2.145), tolower(as.roman(9)), xpd = TRUE, pos = 4)
#label
PlotLabel = 'C'
text(x = -32000, y = 15, PlotLabel, cex = 1.75, xpd = TRUE, pos = 2)
plotRho()
par(mar = c(0,0,6,1))
plotMatH(result$h_final)

dev.off()
