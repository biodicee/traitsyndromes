# code to explore simulation results and derive statistics for text

# Simulation of three climate scenarios at f = 0 and c = 0.1

## Humid climate 

load('simresults/example/result3')
plot(result$h_mean~result$time, type = "l", ylim = c(2,14))
hist(subset(result$h[[401]], result$h[[401]]> 0 ))
summary(subset(result$h[[401]], result$h[[401]]> 0 ))

tail(result$rho$T )

## sub-humid climate 

load('simresults/example/result124')
plot(result$h_mean~result$time, type = "l", ylim = c(2,14))
hist(subset(result$h[[401]], result$h[[401]]> 0 ))
summary(subset(result$h[[401]], result$h[[401]]> 0 ))

tail(result$rho$T )

## semi-arid climate

load('simresults/example/result366')
plot(result$h_mean~result$time, type = "l", ylim = c(2,14))
hist(subset(result$h[[401]], result$h[[401]]> 0 ))
summary(subset(result$h[[401]], result$h[[401]]> 0 ))

tail(result$rho$T )


# simulation of dual gradient

## observations of effect of facilitation

## at low f, clustering along c gradient

summary(repDf2$clus)

plotMatrixCF(repDf2,"clus_cutoff", rev(heat.colors(6)),steps = 6,  range = c(0.9,1.2))
text(rep(seq(0,1,0.1), 11), rep(seq(0,0.5,0.05), each = 11), round(repDf2$clus,2 ) )


plotMatrixCF(repDf2,"h", rainbow(32), range = c(0, 15))
text(rep(seq(0,1,0.1), 11), rep(seq(0,0.5,0.05), each = 11), round(repDf2$h,2 ) )

## with climate change

summary(repDfC2$clus)

plotMatrixCF(repDfC2,"clus_cutoff", rev(heat.colors(6)),steps = 6,  range = c(0.9,1.2))
text(rep(seq(0,1,0.1), 11), rep(seq(0,0.5,0.05), each = 11), round(repDfC2$clus,2 ) )

plotMatrixCF(repDfC2,"h", rainbow(32), range = c(0, 15))
text(rep(seq(0,1,0.1), 11), rep(seq(0,0.5,0.05), each = 11), round(repDfC2$h,2 ) )


## examples under climate change

## f and c active

load(paste0("simresults/change/results_file/result", parameters$ID[parameters$c_intra == 0.1 & parameters$f == 0.4][8]) )
plotH(xax = TRUE)

dev.off()

hist(result$h[[110000/500]], breaks = 100) 


ecotype_x <- result$h[[110000/500]][result$h[[110000/500]] < 9 & result$h[[110000/500]] > 8 ]
mean(ecotype_x)
sd(ecotype_x)

hist(result$h[[90000/500]], breaks = 100) 

ecotype_xi <- result$h[[90000/500]][result$h[[90000/500]] < 5 & result$h[[90000/500]] > 4 ]
mean(ecotype_xi)
sd(ecotype_xi)



hist(result$h_final, breaks = 100) 



ecotype_vi <- result$h_final[result$h_final < 2.5 & result$h_final > 2 ]
mean(ecotype_vi)
sd(ecotype_vi)
ecotype_vii <- result$h_final[result$h_final < 4 & result$h_final > 3 ]
mean(ecotype_vii) 
sd(ecotype_vii)
ecotype_viii <- result$h_final[result$h_final < 5 & result$h_final > 4 ]
mean(ecotype_viii)
sd(ecotype_viii)
ecotype_ix <- result$h_final[result$h_final < 6 & result$h_final > 5 ]
mean(ecotype_ix)
sd(ecotype_ix)

