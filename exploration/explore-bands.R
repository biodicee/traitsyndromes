
source("code/functions.R")


#extractData() sequentially extracts information from all result files, which takes some time
repDf <- extractData('simresults/constant/results_file/', extH = TRUE)
repDf1 <- subset(repDf$stoDf, env == 0.6 & ho == 6.0)
repDf2 <- ddply(repDf1, .variables = c("c","f"), meanh)
repDf2$clus[repDf2$clus >= 2.5 & !is.na(repDf2$clus)] <- 2.4999

parameters <- read.table('simresults/change/parameters.txt', header =TRUE)

#extractData() sequentially extracts information from all result files, which takes some time
repDfC <- extractData('simresults/change/results_file/', extH = TRUE)
#filter environment
repDfC2 <- ddply(subset(repDfC$stoDf, ho == 6.0) , .variables = c("c","f"), meanh)
repDfC2$loss <- repDfC2$countNoNA/repDfC2$count*100

repDfC2$clus[repDfC2$clus >= 2.5 & !is.na(repDfC2$clus)] <- 2.4999

vegetation <- colorRampPalette(c("#D4D4D4", "darkgreen") )


l <- matrix(0, nrow=15, ncol = 11)
l[1:3,1:3] <- 1:9
l[1:3,5:7] <- 10:18
l[1:3,9:11] <- 19:27

l[5:7,1:3] <- 28:36
l[5:7,5:7] <- 37:45
l[5:7,9:11] <- 46:54

l[9:11,1:3] <- 55:63
l[9:11,5:7] <- 64:72
l[9:11,9:11] <- 73:81

l[13:15,1:3] <- 82:90
l[13:15,5:7] <- 91:99
l[13:15,9:11] <- 100:108


pdf("documentation/patterns_constant.pdf", width = 9, height = 12)


layout(l, widths = c(1,1,1,0.1,1,1,1,0.1,1,1,1), heights = c(1,1,1,0.1,1,1,1,0.1,1,1,1,0.1,1,1,1))
par(mar = c(0,0,0,0)+0.01, oma = c(1,6,6,1))  

for(sim in subset(parameters, c_intra == 0.35 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final )
}

for(sim in subset(parameters, c_intra == 0.35 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.35 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.30 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}



for(sim in subset(parameters, c_intra == 0.25 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

mtext("local competition",2,3, outer = TRUE) 
mtext(expression(italic(c[l])~"="~0.35),2,1, outer = TRUE, at = 0.88, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.30),2,1, outer = TRUE, at = 0.63, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.25),2,1, outer = TRUE, at = 0.37, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.20),2,1, outer = TRUE, at = 0.12, cex = 0.8) 

mtext("facilitation",3,3, outer = TRUE)
mtext(expression(italic(f)~"="~0.8),3,1, at = 0.18, outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.9),3,1, at = 0.5,outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~1.0),3,1, at = 0.82, outer = TRUE, cex = 0.8)


#



layout(l, widths = c(1,1,1,0.1,1,1,1,0.1,1,1,1), heights = c(1,1,1,0.1,1,1,1,0.1,1,1,1,0.1,1,1,1))
par(mar = c(0,0,0,0)+0.01, oma = c(1,6,6,1))  

for(sim in subset(parameters, c_intra == 0.35 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final )
}

for(sim in subset(parameters, c_intra == 0.35 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.35 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.30 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}



for(sim in subset(parameters, c_intra == 0.25 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

mtext("local competition",2,3, outer = TRUE) 
mtext(expression(italic(c[l])~"="~0.35),2,1, outer = TRUE, at = 0.88, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.30),2,1, outer = TRUE, at = 0.63, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.25),2,1, outer = TRUE, at = 0.37, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.20),2,1, outer = TRUE, at = 0.12, cex = 0.8) 

mtext("facilitation",3,3, outer = TRUE)
mtext(expression(italic(f)~"="~0.5),3,1, at = 0.18, outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.6),3,1, at = 0.5,outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.7),3,1, at = 0.82, outer = TRUE, cex = 0.8)


#

layout(l, widths = c(1,1,1,0.1,1,1,1,0.1,1,1,1), heights = c(1,1,1,0.1,1,1,1,0.1,1,1,1,0.1,1,1,1))
par(mar = c(0,0,0,0)+0.01, oma = c(1,6,6,1))  

for(sim in subset(parameters, c_intra == 0.15 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final )
}

for(sim in subset(parameters, c_intra == 0.15 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.15 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.10 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.10 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.10 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}



for(sim in subset(parameters, c_intra == 0.05 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.05 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.05 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

mtext("local competition",2,3, outer = TRUE) 
mtext(expression(italic(c[l])~"="~0.15),2,1, outer = TRUE, at = 0.88, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.10),2,1, outer = TRUE, at = 0.63, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.05),2,1, outer = TRUE, at = 0.37, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.00),2,1, outer = TRUE, at = 0.12, cex = 0.8) 

mtext("facilitation",3,3, outer = TRUE)
mtext(expression(italic(f)~"="~0.8),3,1, at = 0.18, outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.9),3,1, at = 0.5,outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~1.0),3,1, at = 0.82, outer = TRUE, cex = 0.8)


#



layout(l, widths = c(1,1,1,0.1,1,1,1,0.1,1,1,1), heights = c(1,1,1,0.1,1,1,1,0.1,1,1,1,0.1,1,1,1))
par(mar = c(0,0,0,0)+0.01, oma = c(1,6,6,1))  

for(sim in subset(parameters, c_intra == 0.15 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final )
}

for(sim in subset(parameters, c_intra == 0.15 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.15 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.10 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.10 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.10 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}



for(sim in subset(parameters, c_intra == 0.05 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.05 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.05 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

mtext("local competition",2,3, outer = TRUE) 
mtext(expression(italic(c[l])~"="~0.15),2,1, outer = TRUE, at = 0.88, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.10),2,1, outer = TRUE, at = 0.63, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.05),2,1, outer = TRUE, at = 0.37, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.00),2,1, outer = TRUE, at = 0.12, cex = 0.8) 

mtext("facilitation",3,3, outer = TRUE)
mtext(expression(italic(f)~"="~0.5),3,1, at = 0.18, outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.6),3,1, at = 0.5,outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.7),3,1, at = 0.82, outer = TRUE, cex = 0.8)



#


layout(l, widths = c(1,1,1,0.1,1,1,1,0.1,1,1,1), heights = c(1,1,1,0.1,1,1,1,0.1,1,1,1,0.1,1,1,1))
par(mar = c(0,0,0,0)+0.01, oma = c(1,6,6,1))  

for(sim in subset(parameters, c_intra == 0.35 & f == 0.0)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final )
}

for(sim in subset(parameters, c_intra == 0.35 & f == 0.1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.35 & f == 0.2)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 0.0)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.30 & f == 0.1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 0.2)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}



for(sim in subset(parameters, c_intra == 0.25 & f == 0.0)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 0.1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 0.2)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.2 & f == 0.0)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.2 & f == 0.1)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.2 & f == 0.2)$ID[1:9] ) {
  load(paste0('simresults/constant/results_file/result', sim) )
  plotMatH(result$h_final)
}

mtext("local competition",2,3, outer = TRUE) 
mtext(expression(italic(c[l])~"="~0.34),2,1, outer = TRUE, at = 0.88, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.30),2,1, outer = TRUE, at = 0.63, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.25),2,1, outer = TRUE, at = 0.37, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.20),2,1, outer = TRUE, at = 0.12, cex = 0.8) 

mtext("facilitation",3,3, outer = TRUE)
mtext(expression(italic(f)~"="~0.0),3,1, at = 0.18, outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.1),3,1, at = 0.5,outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.2),3,1, at = 0.82, outer = TRUE, cex = 0.8)



dev.off()











pdf("documentation/pattern_climatechange.pdf", width = 9, height = 12)


layout(l, widths = c(1,1,1,0.1,1,1,1,0.1,1,1,1), heights = c(1,1,1,0.1,1,1,1,0.1,1,1,1,0.1,1,1,1))
par(mar = c(0,0,0,0)+0.01, oma = c(1,6,6,1))  

for(sim in subset(parameters, c_intra == 0.35 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final )
}

for(sim in subset(parameters, c_intra == 0.35 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.35 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.30 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}



for(sim in subset(parameters, c_intra == 0.25 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

mtext("local competition",2,3, outer = TRUE) 
mtext(expression(italic(c[l])~"="~0.35),2,1, outer = TRUE, at = 0.88, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.30),2,1, outer = TRUE, at = 0.63, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.25),2,1, outer = TRUE, at = 0.37, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.20),2,1, outer = TRUE, at = 0.12, cex = 0.8) 

mtext("facilitation",3,3, outer = TRUE)
mtext(expression(italic(f)~"="~0.8),3,1, at = 0.18, outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.9),3,1, at = 0.5,outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~1.0),3,1, at = 0.82, outer = TRUE, cex = 0.8)


#



layout(l, widths = c(1,1,1,0.1,1,1,1,0.1,1,1,1), heights = c(1,1,1,0.1,1,1,1,0.1,1,1,1,0.1,1,1,1))
par(mar = c(0,0,0,0)+0.01, oma = c(1,6,6,1))  

for(sim in subset(parameters, c_intra == 0.35 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final )
}

for(sim in subset(parameters, c_intra == 0.35 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.35 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.30 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}



for(sim in subset(parameters, c_intra == 0.25 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.20 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

mtext("local competition",2,3, outer = TRUE) 
mtext(expression(italic(c[l])~"="~0.35),2,1, outer = TRUE, at = 0.88, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.30),2,1, outer = TRUE, at = 0.63, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.25),2,1, outer = TRUE, at = 0.37, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.20),2,1, outer = TRUE, at = 0.12, cex = 0.8) 

mtext("facilitation",3,3, outer = TRUE)
mtext(expression(italic(f)~"="~0.5),3,1, at = 0.18, outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.6),3,1, at = 0.5,outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.7),3,1, at = 0.82, outer = TRUE, cex = 0.8)


#

layout(l, widths = c(1,1,1,0.1,1,1,1,0.1,1,1,1), heights = c(1,1,1,0.1,1,1,1,0.1,1,1,1,0.1,1,1,1))
par(mar = c(0,0,0,0)+0.01, oma = c(1,6,6,1))  

for(sim in subset(parameters, c_intra == 0.15 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final )
}

for(sim in subset(parameters, c_intra == 0.15 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.15 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.10 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.10 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.10 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}



for(sim in subset(parameters, c_intra == 0.05 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.05 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.05 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 0.8)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 0.9)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

mtext("local competition",2,3, outer = TRUE) 
mtext(expression(italic(c[l])~"="~0.15),2,1, outer = TRUE, at = 0.88, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.10),2,1, outer = TRUE, at = 0.63, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.05),2,1, outer = TRUE, at = 0.37, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.00),2,1, outer = TRUE, at = 0.12, cex = 0.8) 

mtext("facilitation",3,3, outer = TRUE)
mtext(expression(italic(f)~"="~0.8),3,1, at = 0.18, outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.9),3,1, at = 0.5,outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~1.0),3,1, at = 0.82, outer = TRUE, cex = 0.8)


#



layout(l, widths = c(1,1,1,0.1,1,1,1,0.1,1,1,1), heights = c(1,1,1,0.1,1,1,1,0.1,1,1,1,0.1,1,1,1))
par(mar = c(0,0,0,0)+0.01, oma = c(1,6,6,1))  

for(sim in subset(parameters, c_intra == 0.15 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final )
}

for(sim in subset(parameters, c_intra == 0.15 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.15 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.10 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.10 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.10 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}



for(sim in subset(parameters, c_intra == 0.05 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.05 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.05 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 0.5)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 0.6)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.0 & f == 0.7)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

mtext("local competition",2,3, outer = TRUE) 
mtext(expression(italic(c[l])~"="~0.15),2,1, outer = TRUE, at = 0.88, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.10),2,1, outer = TRUE, at = 0.63, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.05),2,1, outer = TRUE, at = 0.37, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.00),2,1, outer = TRUE, at = 0.12, cex = 0.8) 

mtext("facilitation",3,3, outer = TRUE)
mtext(expression(italic(f)~"="~0.5),3,1, at = 0.18, outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.6),3,1, at = 0.5,outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.7),3,1, at = 0.82, outer = TRUE, cex = 0.8)



#


layout(l, widths = c(1,1,1,0.1,1,1,1,0.1,1,1,1), heights = c(1,1,1,0.1,1,1,1,0.1,1,1,1,0.1,1,1,1))
par(mar = c(0,0,0,0)+0.01, oma = c(1,6,6,1))  

for(sim in subset(parameters, c_intra == 0.35 & f == 0.0)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final )
}

for(sim in subset(parameters, c_intra == 0.35 & f == 0.1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.35 & f == 0.2)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 0.0)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.30 & f == 0.1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}


for(sim in subset(parameters, c_intra == 0.30 & f == 0.2)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}



for(sim in subset(parameters, c_intra == 0.25 & f == 0.0)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 0.1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.25 & f == 0.2)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.2 & f == 0.0)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.2 & f == 0.1)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

for(sim in subset(parameters, c_intra == 0.2 & f == 0.2)$ID[1:9] ) {
  load(paste0('simresults/change/results_file/result', sim) )
  plotMatH(result$h_final)
}

mtext("local competition",2,3, outer = TRUE) 
mtext(expression(italic(c[l])~"="~0.34),2,1, outer = TRUE, at = 0.88, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.30),2,1, outer = TRUE, at = 0.63, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.25),2,1, outer = TRUE, at = 0.37, cex = 0.8) 
mtext(expression(italic(c[l])~"="~0.20),2,1, outer = TRUE, at = 0.12, cex = 0.8) 

mtext("facilitation",3,3, outer = TRUE)
mtext(expression(italic(f)~"="~0.0),3,1, at = 0.18, outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.1),3,1, at = 0.5,outer = TRUE, cex = 0.8)
mtext(expression(italic(f)~"="~0.2),3,1, at = 0.82, outer = TRUE, cex = 0.8)



dev.off()


