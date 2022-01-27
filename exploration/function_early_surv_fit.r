
### Early survival function parametrization

# rm(list=ls())		
library(gplots)

### Data ----------------------------------------------------------------

# Germination percentage from Tertiary and Quaternary plant species 
# assessed along a water deficit gradient (osmotic water potential of the soil in megapascal) between moist (around 0 MPa) and dry (from -0.5 to 1 MPa)
# Environmental quality (E) was then calculated as 1 + osmotic potential, so E = 0 means dry soils and E = 1.2 water saturated soils
# Data Refs: Verdú & Garcia Fayos, 1996; Pérez-Fernandez et al. 2006

# Tertiary trait
tg <- (c(90,87,84,84,60,22,10,5,0,0))/100 # germination percentage
tw <- c(-0,-0.02,-0.05,-0.10,-0.31,-0.39,-0.49,-0.62,-0.78,-0.99) # osmotic water potential of the soil in MPa
te <- (1+tw) # Environmental quality (E)

# Quaternary trait
qg <- (c(90,84,60,66,66))/100  # germination percentage (scaled to 100%) 
qw <- c(0,-0.165,-0.256,-0.368,-0.5)  # osmotic water potential of the soil in MPa
qe <- (1+qw)  # Environmental quality (E)



### Parametrizing the Early Survival function ------------------------------------------

# Function
surv <- function(e, alpha, h) {
  (alpha*(e^h))/(1 + alpha*(e^h)) # Eq. 2 in the manuscript
}

# Data
tdata <- data.frame(tg,te)
qdata <- data.frame(qg,qe)

# Estimating parameter alpha (constant parameter)
fit_Ta <- nls(tg~surv(te, alpha, h), data=data.frame(tdata), start=list(h=1, alpha=1))
fit_Qa <- nls(qg~surv(qe, alpha, h), data=data.frame(qdata), start=list(h=1, alpha=1))
avg_alpha <- (coef(fit_Ta)[2] + coef(fit_Qa)[2])/2 # alpha estimated at 7.5 

# Estimating trait value h for Tertiary and Quaternary plants
fit_T <- nls(tg~surv(te, avg_alpha, h), data=data.frame(tdata), start=list(h=1))
fit_Q <- nls(qg~surv(qe, avg_alpha, h), data=data.frame(qdata), start=list(h=1))
summary(fit_T) # h = 5.7 (T-plant)
summary(fit_Q) # h = 2.4 (Q-plant)



### Plotting Figure 1A ---------------------------------------------------------

Tcol <- "forestgreen"
Qcol <- "orangered2"
  
# Dots
plot(tg~te, ylim=c(0,1), xlim=c(0,1.2), col=Tcol, pch = 16, xlab="Soil water availability (E)", ylab="Early survival S(E,h)")
points(qg~qe, col=Qcol, pch=16)

# Function
lines(seq(0,1.2, length = 120) , predict(fit_T, newdata = list(te = seq(0,1.2, length = 120))), col=Tcol, lwd=2 )
lines(seq(0,1.2, length = 120) , predict(fit_Q, newdata = list(qe = seq(0,1.2, length = 120))), col=Qcol, lwd=2 )

# Labels
text(0.75,0.35, labels = "Tertiary", col=Tcol, cex=1)
text(0.75,0.3, labels = expression(paste("(", h[T], "= 5.7)")), col=Tcol, cex=0.9)

text(0.47,0.35, labels = "Quaternary",  col=Qcol,cex=1)
text(0.47,0.3, labels = expression(paste("(", h[Q], "= 2.4)")), col=Qcol, cex=0.9)

# Other values of h in grey
for (i in 1:2){
  lines(surv(seq(0,1, length = 100),avg_alpha,h=i)~seq(0,1, length = 100), lwd=2, col="grey40")
  #text(0.1,0.92, labels = "h=0", col="grey50", cex=0.9)
  text(0.13,0.6, labels = "h=1", col="grey50", cex=0.9)
  text(0.23,0.4, labels = "h=2", col="grey50", cex=0.9)
}



