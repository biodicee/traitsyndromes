library(compiler)
library(grid)
library(plyr)

#Mode

Mode <- function(x) {
  ux <- unique(x)
  mx <- tabulate(match(x, ux))
  mat <- data.frame(ux,mx)
  mat$ux[mat$mx==max(mat$mx)]
}


#étendue
Eten <- function(x){
  max(x)-min(x)
}

#espace interquartile
EIQ <- function(x){
  quantile(x)[4]-quantile(x)[2]
}

#ecart  moyen

ecmoy <- function(x){
  mean(abs(x-mean(x)))
}


#erreur standard
se <- function(x){
  sd(x)/sqrt(length(x)) #formule de l'erreur standart
}

#intervalle de confiance 95%
ci <- function(x,alpha = 0.05){
  if(length(x) >= 30){
    ci1 <- qnorm(1 -alpha/2)*sd(x)/sqrt(length(x)) #formule de l'intervalle de conf effectif supérieur à 30
    
  } else {
    ci1 <- qt(1 -alpha/2,length(x)-1)*sd(x)/sqrt(length(x)) #formule de l'intervalle de conf effectif inférieur à 30
    
  }
  ci1
}

#intervalle de confiance 95% avec bornes
cib <- function(x,alph = 0.05){
  if(length(x) >= 30){
    ci1 <- qnorm(1 -alph/2)*sd(x)/sqrt(length(x)) #formule de l'intervalle de conf effectif supérieur à 30
    
  } else {
    ci1 <- qt(1 -alph/2,length(x)-1)*sd(x)/sqrt(length(x)) #formule de l'intervalle de conf effectif inférieur à 30
    
  }
  ci2 <- c(mean(x) - ci1, mean(x) + ci1)
  ci2
}

#redéfinir les limites d'un graphe pour axes à 0
lim <- function(x){
  if (min(x)<0){
    c(min(x)-(max(x) - min(x))*0.05,max(x)+(max(x) - min(x))*0.05)
  } else {
    c(0-(max(x) - min(x))*0.05,max(x)+(max(x) - min(x))*0.05)
  }
}

#graph vide
plot0 <- function(xlim,ylim){
  plot(NA,type ="n",ann=FALSE,axes=FALSE,xlim=xlim,ylim=ylim)
}


#graph avec des flèches
plot0a <- function(x,y){
  xLim <- lim(x)
  yLim <- lim(y)
  plot(NA,type ="n",ann=FALSE,axes=FALSE,xlim=xLim,ylim=yLim)
  arrows(xLim[1],0,xLim[2],0,angle=20,length=0.15,lwd=1.5)
  arrows(0,yLim[1],0,yLim[2],angle=20,length=0.15,lwd=1.5)
  points(x,y,pch=3)
}

#coordonnées polaires
pol <- function(x,y,angle = "rad"){
  rho <- sqrt(x^2+y^2)
  if (x < 0 && y >= 0){
    theta = pi + atan(y/x)
  } else if (x < 0 && y < 0){
    theta = -pi + atan(y/x)
  } else {
    theta <- atan(y/x)
  }
  if (angle == "rad"){
    c(rho,theta)
  } else {
    c(rho,theta*180/pi)
  }
}

#norme vecteur

norme <- function(x,y,z){
  sqrt(x^2+y^2+z^2)
}

#produit scalaire
proScal <- function(x,y){
  sum(x*y)
}

# produit vectoriel
proVect<- function(x,y){
  if(length(x)<3){
    x <- c(x,0)
    y <- c(y,0)
    print("vecteurs du plan")
  }
  xcomp <- c(x[2]*y[3] - x[3]*y[2])
  ycomp <- c(x[3]*y[1] - x[1]*y[3])
  zcomp <- c(x[1]*y[2] - x[2]*y[1])
  c(xcomp,ycomp,zcomp)
}

#héron

x = 3
y = 4 ; z = 6
heron <- function(x,y,z){
  p <- (x+y+z)/2
  sqrt(p*(p-x)*(p-y)*(p-z))
}

khi2 <- function(x){
  cont <- x
  
  #faire les totaux
  conttot <- cbind(cont, apply(cont,1,sum))
  conttot <- rbind(conttot, apply(conttot,2,sum))
  colnames(conttot) <-  c(colnames(cont),"Total")
  row.names(conttot) <-  c(row.names(cont),"Total")
  print(conttot)
  
  #effectifs théoriques
  theo <- cont
  for (i in 1:dim(cont)[1]){
    for (j in 1:dim(cont)[2]){
      theo[i,j] <- sum(cont[i,])*sum(cont[,j])/sum(cont)
    }
  }
  print("T0")
  print(theo)
  
  #différence entre effectifs théoriques et observés
  contR <- cont - theo
  print("R")
  print(contR)
  #mise au carré
  contR2 <- contR^2
  print("R2")
  print(contR2)
  #rapport avec effectifs théoriques
  
  contR2T0 <- contR2/theo
  #khi 2 obs
  
  contR2T0tot <- cbind(contR2T0, apply(contR2T0,1,sum))
  contR2T0tot <- rbind(contR2T0tot, apply(contR2T0tot,2,sum))
  colnames(contR2T0tot) <-  c(colnames(cont),"Total")
  row.names(contR2T0tot) <-  c(row.names(cont),"Total")
  print("R2/T0")
  print(contR2T0tot)
  
  khi2obs <- contR2T0tot[dim(contR2T0tot)[1],dim(contR2T0tot)[2]]
  print("khi2obs")
  print(khi2obs)
  #calcul degrés de liberté
  
  k <- (dim(cont)-1)[1]*(dim(cont)-1)[2]
  print("k")
  print(k)
  #Calcul de la probabilité
  print("p val")
  print( 1 - pchisq(khi2obs,k))
}

#plot prob vs prob
plotPvsP = function () {
  plot(NA,xlim = c(0,1), ylim = c(0,1),asp = 1,ann = FALSE,axes = FALSE)
  axis(1,tck = -0.01,label = FALSE,at = seq(0,1,0.5))
  axis(2,las = 2,tck = -0.01,label = FALSE,at = seq(0,1,0.5))
  axis(1,lwd = 0,line = -0.5,at = seq(0,1,0.5))
  axis(2,las = 2,lwd = 0,line = -0.5,at = seq(0,1,0.5))
  rect(1,1,0,0,border="grey60")
}



#number clusters in lattice 
ClusGroup <- function (x){
  mat_group <- x
  mat_group[which(mat_group==0)] <- NA
  mat_group[which(mat_group!=0)] <- 0
  groupVal = 1
  #loop until every cell has a group number
  while(0 %in% mat_group){
    #loop find every cell with 0
    for(i in 1:dim(mat_group)[1]){
      for(j in 1:dim(mat_group)[2]){
        if(!is.na(mat_group[i,j]) && mat_group[i,j] == 0){
          #assignate group value
          mat_group[i,j] <- groupVal
          #find neighboors
          neigVal = 0
          neigEvaluated = c()
          while (0 %in% neigVal){
            #transform vector indices into matrix indices
            celtot <- which(mat_group == groupVal)
            if(length(neigEvaluated)>0) {
              cel <- celtot[-which(celtot %in% neigEvaluated)]
            } else cel <- celtot
            neigEvaluated <- celtot
            row <- floor((cel - 1) %% (dim(mat_group)[1])) + 1
            col <- floor((cel - 1) / (dim(mat_group)[1])) + 1
            #assign values to neighboors
            for(group in 1:length(cel)){
              #define neighboorhood
              if (row[group] == 1) rplus = dim(mat_group)[1] - 1 else rplus = -1
              if (row[group] == dim(mat_group)[1]) rmoins = -(dim(mat_group)[1] - 1) else rmoins = 1
              if (col[group] == 1) cplus = dim(mat_group)[2] - 1 else cplus = -1
              if (col[group] == dim(mat_group)[2]) cmoins = -(dim(mat_group)[2] - 1) else cmoins = 1
              rn <- c(0,rplus,0,rmoins)
              cn <- c(cplus,0,cmoins,0)
              for(neighboor in 1:length(rn)){
                if (!is.na(mat_group[row[group] + rn[neighboor],col[group] + cn[neighboor]])){
                  mat_group[row[group] + rn[neighboor],col[group] + cn[neighboor]] <- groupVal
                }
              }
            }
            #check neighboors
            #transform vector indices into matrix indices
            celtot <- which(mat_group == groupVal)
            if(length(neigEvaluated)>0) {
              cel <- celtot[-which(celtot %in% neigEvaluated)]
            } else cel <- celtot
            row <- floor((cel - 1) %% (dim(mat_group)[1])) + 1
            col <- floor((cel - 1) / (dim(mat_group)[1])) + 1
            #define neighboorhood
            rn <- c(0,1,0,-1)
            cn <- c(1,0,-1,0)
            neigVal=c() #bug
            if (length(cel) > 0 ){
              #check neighboors
              for(group in 1:length(cel)){
                #define neighboorhood
                if (row[group] == 1) rplus = dim(mat_group)[1] - 1 else rplus = -1
                if (row[group] == dim(mat_group)[1]) rmoins = -(dim(mat_group)[1] - 1) else rmoins = 1
                if (col[group] == 1) cplus = dim(mat_group)[2] - 1 else cplus = -1
                if (col[group] == dim(mat_group)[2]) cmoins = -(dim(mat_group)[2] - 1) else cmoins = 1
                rn <- c(0,rplus,0,rmoins)
                cn <- c(cplus,0,cmoins,0)
                for(neighboor in 1:length(rn)){
                  neigVal = c(neigVal,mat_group[row[group] + rn[neighboor],col[group] + cn[neighboor]])
                }
              }
            }
          }
          groupVal = groupVal + 1
        }
      }
    }
  }
  list(
    clustNb = length(table(mat_group)),
    clustMeanSize = mean(table(mat_group)),
    clustMaxSize = max(table(mat_group)),
    clustSdSize = sd(table(mat_group)),
    mat_group = mat_group)
}

ClusGroup <- cmpfun(ClusGroup)

#compare observed and expected value for neighbourhood
findNeigh <- function  (x) {
  mat_group <- x
  mat_group[which(mat_group!=0)] <- 1
  arr <- array(NA,c(dim(x)[1],dim(x)[2],4))
  #array 1
  arr[,,1] <- mat_group
  arr[,,2:4] <- rep(NA,dim(x)[1]*dim(x)[2]*3)#nb vois
  #calculate number of cells without friend
  for (i in 1:dim(x)[1]){
    for (j in 1:dim(x)[2]){
      if (i == 1) {hvm = -dim(x)[1]+1} else {hvm = 1}
      if (j == 1) {vvm = -dim(x)[1]+1} else {vvm = 1}
      if (i == dim(x)[1]) {hvp = -dim(x)[1]+1} else {hvp = 1}
      if (j == dim(x)[2]) {vvp = -dim(x)[1]+1} else {vvp = 1}
      arr[i,j,2] = sum(c(arr[i+hvp,j,1],arr[i,j+vvp,1],arr[i-hvm,j,1],arr[i,j-vvm,1]))
      if (arr[i,j,1] == 0) arr[i,j,3] = sum(c(arr[i+hvp,j,1],arr[i,j+vvp,1],arr[i-hvm,j,1],arr[i,j-vvm,1]))
      if (arr[i,j,1] == 1) arr[i,j,4] = sum(c(arr[i+hvp,j,1],arr[i,j+vvp,1],arr[i-hvm,j,1],arr[i,j-vvm,1]))
    }
  }
  #0 instead of NA
  if (sum(arr[,,1]) != 0) probReal = table(arr[,,1],exclude = 0)/(dim(x)[1]*dim(x)[2]) else probReal = 0
  if (sum(arr[,,1]) != 0) probNoEmp = table(arr[,,2])[1]/(dim(x)[1]*dim(x)[2])
  
  n = 4
  k = 0:n
  p_n = dbinom(k,n,probReal)
  p_n0 = dbinom(k,n,probReal)*(1-probReal)
  p_n1 = dbinom(k,n,probReal)*probReal
  
  sto <- matrix(c(
    probReal,
    length(which(arr[,,2] == 0))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,2] == 1))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,2] == 2))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,2] == 3))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,2] == 4))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,3] == 0))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,3] == 1))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,3] == 2))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,3] == 3))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,3] == 4))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,4] == 0))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,4] == 1))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,4] == 2))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,4] == 3))/(dim(x)[1]*dim(x)[2]),
    length(which(arr[,,4] == 4))/(dim(x)[1]*dim(x)[2]),
    probReal,
    p_n,
    p_n0,
    p_n1),
    ncol = 16,
    byrow = TRUE
  )
  
  
  
  colnames(sto) = c("rho"
                    ,"Neib0","Neib1","Neib2","Neib3","Neib4"
                    ,"Neib00","Neib10","Neib20","Neib30","Neib40"
                    ,"Neib01","Neib11","Neib21","Neib31","Neib41")
  
  sto
  
}


findNeigh <- cmpfun(findNeigh)

#palette de couleurs
ColorsSim <- function(n,alpha = 1){
  pal <- c(rgb(0,0,0,alpha)
           ,rgb(0.90,0.60,0,alpha)
           ,rgb(0.35,0.70,0.90,alpha)
           ,rgb(0,0.60,.50,alpha)
           ,rgb(.95,0.90,.25,alpha)
           ,rgb(0,.45,.70,alpha)
           ,rgb(.80,.40,0,alpha)
           ,rgb(.80,.60,.70,alpha))
  pal[1:n]
}

#cut data in groups (peaks)
function (x, thresh = 0.2) {
  if(mean(x) == 0){
    list(
      h_groupValues = 0
    )
  } else {
    sh_matrix <- sort(x)
    pks <- which(diff(sign(diff(sh_matrix, na.pad = FALSE)), na.pad = FALSE) < 0) + 2
    rem <- rem2 <- c()
    for (i in 1:(length(pks)-1)){
      #i = 1
      if (abs(sh_matrix[pks[i]] - sh_matrix[pks[i+1]]) < thresh){
        rem <- c(rem,i+1)
        rem2 <- c(rem2,i)
      }
    }
    if (is.null(rem)){
      h_groupValues = 0
    } else {
      pks1 <- pks[-rem]
      pks2 <- pks[-rem2]
      h_groupValues <- (sh_matrix[pks2] - sh_matrix[pks1]) / 2 + sh_matrix[pks1]
      h_groupValues
      h_cut <- (h_groupValues[-1] - h_groupValues[-length(h_groupValues)]) / 2 + h_groupValues[-length(h_groupValues)]
      test <- density(x[which(x > 0)],bw = 0.1)
      #plot(test)
      #abline(v=c(h_groupValues), col = 2)
      #abline(v=h_cut, col = 4)
      list(
        h_cut = h_cut,h_groupValues = h_groupValues
      )
    }
  }
}


#summarySE

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N     = length2(xx[[col]], na.rm=na.rm),
                     mean  = mean   (xx[[col]], na.rm=na.rm),
                     var   = var    (xx[[col]], na.rm=na.rm),
                     sd    = sd     (xx[[col]], na.rm=na.rm),
                     max   = max    (xx[[col]], na.rm=na.rm),
                     min   = min    (xx[[col]], na.rm=na.rm),
                     range = max    (xx[[col]], na.rm=na.rm) - min    (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#intraspecific clustering
intraClus <- function(x){
  group <- unique(c(x))
  if (0 %in% group) group <- sort(group)[-1]
  h2 <- x
  
  clusMat <- matrix(NA,nrow = length(group), ncol = length(group))
  colnames(clusMat) <- group
  row.names(clusMat) <- group
  
  
  width <- dim(h2)[1]
  height <- dim(h2)[2]
  
  
  
  X <- matrix(as.integer(1:(width*height)), ncol = width, byrow =TRUE)
  # setting the border of the evaluation matrix X
  X <- cbind(X[,width], X, X[,1] )  
  X <- rbind(X[height,], X, X[1,] ) 
  # transformation vector which adds the border to the lattice:
  x_with_border <- as.integer(t(X))
  
  # from the matrix X (with copied border cells), which cells are the actual cells (reverse transformation vector of the previous lines) 
  x_to_evaluate <- sort(matrix(1:prod(dim(X)), ncol = dim(X)[2], byrow =TRUE)[-c(1, dim(X)[1]), -c(1,dim(X)[2])]	)		
  
  # defining the neighborhood which is to be evaluated	
  # set interaction matrix
  I <- matrix(c(0,1,0,1,NA,1,0,1,0), ncol = 3, byrow = TRUE)	
  # coordinates of neighbours in Interaction matrix I: 
  neighbours_in_I <- which(is.finite(abs(I)/abs(I)), arr.in = TRUE)
  # coordinates relative to the evaluated cell (=  which(is.na(I) ) 
  relrow <- neighbours_in_I[,1]-which(is.na(I), arr.ind = TRUE)[1]
  relcol <- neighbours_in_I[,2]-which(is.na(I), arr.ind = TRUE)[2]
  
  # relative position of the four direct neighbours of a cell
  interact <- (relrow * dim(X)[2] + relcol)
  
  
  for (i in 1:length(group)){
    for (j in 1:length(group)){
      A = i
      B = j
      #  Comparing a and b
      rhob <- length(h2[which(h2==group[B])])/(dim(h2)[1]*(dim(h2)[2]))
      
      b_neighb <- rowSums(sapply(interact, function(j) (h2==group[B])[x_with_border][x_to_evaluate+j] ))/4 # matrix with all neighbours in state b
      Qba <- mean(b_neighb[h2==group[A]]) # mean of b in the neighborhood of a = probability of finding b given a
      #Qba
      clust_ba <- Qba/rhob
      clusMat[i,j] <- round(clust_ba,1)
    }
  }
  clusMat  
  #fclusMat <- cbind(clusMat,apply(clusMat, 1,sum) - diag(clusMat), diag(clusMat))
  
  
  #colnames(fclusMat) <- c(group,"inter","intra")
  #fclusMat
}

#clustering 
clus <- function (x) {
  #for neighbours evaluation
  width <- dim(x)[1]
  height <- dim(x)[2]
  X <- matrix(as.integer(1:(width*height)), ncol = width, byrow =TRUE)
  # setting the border of the evaluation matrix X
  X <- cbind(X[,width], X, X[,1] )  
  X <- rbind(X[height,], X, X[1,] ) 
  # transformation vector which adds the border to the lattice:
  x_with_border <- as.integer(t(X))
  
  # from the matrix X (with copied border cells), which cells are the actual cells (reverse transformation vector of the previous lines) 
  x_to_evaluate <- sort(matrix(1:prod(dim(X)), ncol = dim(X)[2], byrow =TRUE)[-c(1, dim(X)[1]), -c(1,dim(X)[2])]	)		
  
  # defining the neighborhood which is to be evaluated	
  # set interaction matrix
  I <- matrix(c(0,1,0,1,NA,1,0,1,0), ncol = 3, byrow = TRUE)	
  # coordinates of neighbours in Interaction matrix I: 
  neighbours_in_I <- which(is.finite(abs(I)/abs(I)), arr.in = TRUE)
  # coordinates relative to the evaluated cell (=  which(is.na(I) ) 
  relrow <- neighbours_in_I[,1]-which(is.na(I), arr.ind = TRUE)[1]
  relcol <- neighbours_in_I[,2]-which(is.na(I), arr.ind = TRUE)[2]
  
  # relative position of the four direct neighbours of a cell
  interact <- (relrow * dim(X)[2] + relcol)
  
  rho <- mean(x)
  Qtt <- rowSums( sapply(interact,	function(j) (x == 1)[x_with_border][x_to_evaluate+j]  ))/4     
  clust_data <- (Qtt[which(x==1)]/rho)
  cluster <- mean(clust_data)
  round(cluster,3)
}

clus <- cmpfun(clus)

paire11 <- function (x){
  mat <- x
  mat1 <- cbind(mat[,2:dim(mat)[2]],mat[,1])
  mat2 <- rbind(mat[2:dim(mat)[1],],mat[1,])
  col <- c(mat == 1 & mat2 == 1)
  row <- c(mat == 1 & mat1 == 1)
  paires <- sum(col == TRUE) + sum(row == TRUE)
  list(
    obs = paires/(2*prod(dim(mat))),
    exp = (sum(mat)/prod(dim(mat)))^2
  )
}

paire11 <- cmpfun(paire11)


#compare observed and expected value for neighbourhood for each species and for empty cells
findNeighSpe <- function  (x) {
  mat_group <- mat_groupSpe <- x
  mat_group[which(mat_group!=0)] <- 1
  Spe <- sort(unique(c(x)))
  arr <- array(NA,c(dim(x)[1],dim(x)[2],length(Spe)))
  exp <- obs <- matrix(NA,nrow = length(Spe),ncol = 5)
  colnames(obs) <- colnames(exp) <- 0:4
  row.names(obs) <- row.names(exp) <- Spe
  probSpe <- rep(NA, length(Spe))
  #calculate number of cells without friend
  for (h in 1:length(Spe)){
    probSpe[h] = length(c(mat_groupSpe[which(mat_groupSpe == Spe[h])]))/prod(dim(x))
    for (i in 1:dim(x)[1]){
      for (j in 1:dim(x)[2]){
        #define neighbourhood and infinite landscape
        if (i == 1) {hvm = -dim(x)[1] + 1} else {hvm = 1}
        if (j == 1) {vvm = -dim(x)[1] + 1} else {vvm = 1}
        if (i == dim(x)[1]) {hvp = -dim(x)[1] + 1} else {hvp = 1}
        if (j == dim(x)[2]) {vvp = -dim(x)[1] + 1} else {vvp = 1}
        #fill array
        #arr[i,j,1] = sum(c(mat_group[i+hvp,j],mat_group[i,j+vvp],mat_group[i-hvm,j],mat_group[i,j-vvm]))
        if (mat_groupSpe[i,j] == Spe[h]) arr[i,j,h] = sum(c(mat_group[i+hvp,j],mat_group[i,j+vvp],mat_group[i-hvm,j],mat_group[i,j-vvm]))
      }
    }
    #calculate probabilities for each ...
    obs[h,] <- (table(arr[,,h])/prod(dim(x)))[as.character(0:4)]
    exp[h,] <- dbinom(0:4,4,sum(mat_group)/prod(dim(x)))*probSpe[h]
    coord <- barplot(obs[h,],ylim = c(0,max(c(obs[h,],exp[h,]),na.rm = TRUE)))
    points(coord,exp[h,],pch = 16, col = 2)
  }
  obs[which(is.na(obs))] <- 0
  list(obs,exp)
}

findNeighSpe <- cmpfun(findNeighSpe)




#calculate shannon index 
#(need a vector of all of the specices in)
# exemple : species <- c(1,1,1,1,2,2,1,2,1,1,3,3,3,1,2)
#define proportion of species
CalcShannon <- function (x){
  propSp <- table(x)/length(x)
  #calculate shannon
  shann <- -sum(propSp*log(propSp))
  #calculate shannon max
  shannMax <- log(length(propSp))
  return(shann)
}

CalcShannon<- cmpfun(CalcShannon)


plotMatrixR <- function(x, legend = TRUE,axes = FALSE,box = FALSE, col = rev(terrain.colors(100)),gridded = TRUE) {
  library(raster)
  x1 <- raster(x)
  plot(x1, legend = legend,axes = axes,box = box,col = col)
}

plotMatrixR <- cmpfun(plotMatrixR)


#each patch get a label
clusSpecies <- function (x){
  #number clusters in lattice
  mat_group <- x #load matrix (for storage)
  Species <- unique(c(x[x != 0]))
  mat_group[which(mat_group==0)] <- NA
  mat_group[which(mat_group!=0)] <- 0
  groupVal = 1
  #loop until every cell has a group number
  #loop find every cell with 0
  for (sp in Species){
    while (0 %in% mat_group[x == sp])
      for(i in 1:dim(mat_group)[1]){
        for(j in 1:dim(mat_group)[2]){
          if(!is.na(mat_group[i,j]) && x[i,j] == sp && mat_group[i,j] == 0){
            #assignate group value
            mat_group[i,j] <- groupVal
            #find neighboors
            neigVal = 0
            neigEvaluated = c()
            while (0 %in% neigVal){
              #transform vector indices into matrix indices
              celtot <- which(mat_group == groupVal)
              if(length(neigEvaluated)>0) {
                cel <- celtot[-which(celtot %in% neigEvaluated)]
              } else cel <- celtot
              neigEvaluated <- celtot
              row <- floor((cel - 1) %% (dim(mat_group)[1])) + 1
              col <- floor((cel - 1) / (dim(mat_group)[1])) + 1
              #assign values to neighboors
              for(group in 1:length(cel)){
                #define neighboorhood
                if (row[group] == 1) rplus = dim(mat_group)[1] - 1 else rplus = -1
                if (row[group] == dim(mat_group)[1]) rmoins = -(dim(mat_group)[1] - 1) else rmoins = 1
                if (col[group] == 1) cplus = dim(mat_group)[2] - 1 else cplus = -1
                if (col[group] == dim(mat_group)[2]) cmoins = -(dim(mat_group)[2] - 1) else cmoins = 1
                rn <- c(0,rplus,0,rmoins)
                cn <- c(cplus,0,cmoins,0)
                for(neighboor in 1:length(rn)){
                  if (!is.na(mat_group[row[group] + rn[neighboor],col[group] + cn[neighboor]]) && x[row[group] + rn[neighboor],col[group] + cn[neighboor]] == sp){
                    mat_group[row[group] + rn[neighboor],col[group] + cn[neighboor]] <- groupVal
                  }
                }
              }
              #check neighboors
              #transform vector indices into matrix indices
              celtot <- which(mat_group == groupVal)
              if(length(neigEvaluated)>0) {
                cel <- celtot[-which(celtot %in% neigEvaluated)]
              } else cel <- celtot
              row <- floor((cel - 1) %% (dim(mat_group)[1])) + 1
              col <- floor((cel - 1) / (dim(mat_group)[1])) + 1
              #define neighboorhood
              rn <- c(0,1,0,-1)
              cn <- c(1,0,-1,0)
              neigVal = c() #bug
              if (length(cel) > 0 ){
                #check neighboors
                for(group in 1:length(cel)){
                  #define neighboorhood
                  if (row[group] == 1) rplus = dim(mat_group)[1] - 1 else rplus = -1
                  if (row[group] == dim(mat_group)[1]) rmoins = -(dim(mat_group)[1] - 1) else rmoins = 1
                  if (col[group] == 1) cplus = dim(mat_group)[2] - 1 else cplus = -1
                  if (col[group] == dim(mat_group)[2]) cmoins = -(dim(mat_group)[2] - 1) else cmoins = 1
                  rn <- c(0,rplus,0,rmoins)
                  cn <- c(cplus,0,cmoins,0)
                  for(neighboor in 1:length(rn)){
                    neigVal = c(neigVal,mat_group[row[group] + rn[neighboor],col[group] + cn[neighboor]])
                  }
                }
              }
            }
            groupVal = groupVal + 1
          }
        }
      }
  }
  species = c()
  size = c()
  number = c()
  for (i in Species){
    tableCalcul <- table(mat_group[x == Species[i]])
    species <- c(species,rep(Species[i],length(tableCalcul)))
    size <- c(size,tableCalcul)
  }
  
  list(
    clustNb = length(table(mat_group)),
    clustMeanSize = mean(table(mat_group)),
    clustMaxSize = max(table(mat_group)),
    clustSdSize = sd(table(mat_group)),
    mat_group = mat_group,
    mat_species = x,
    clusters = data.frame(species,size))
}

#highlight

highlight <- function(x, colrange = c("black", "red2"), steps = NULL, range = "auto"){
  
  if(is.null(steps)) steps = length(unique(x))
  if(range[1] == "auto") {
    min_val = min(x, na.rm = TRUE)
    max_val = max(x, na.rm = TRUE)
  } else {
    min_val = range[1]
    max_val = range[2]
  }
  
  colorscale <- colorRampPalette(colrange, space = "rgb")
  cols <- colorscale( steps)
  
  cols[as.integer((x-min_val)/(max_val-min_val)*steps+1)]
  
}


plot_colscale <- function(x, # data
                          colpal = v,
                          range = c(min(x, na.rm = TRUE)-0.5, max(x, na.rm = TRUE)+0.5),
                          labels = TRUE, at = NULL, mar = c(0,0,0,4)+0.05) {
  pardefault <- par()
 # call <- format(substitute(v))
  n <- length(colpal)
  par(mar = mar, bty = "n")
  min = range[1]
  max = range[2]
  plot(NA,NA, ylab = "", xlab = "", type = "n", xaxt = "n", yaxt = "n", xlim = c(0,1), ylim = c(min,max))
  
  rect(xleft = c(0,0,0,0), 
       ybottom = seq(min,max,length = 1+n)[1:n],
       xright = c(1,1,1,1),
       ytop = seq(min,max,length = 1+n)[1:n+1],
       col = colpal[1:n],
       border = NA
  )
  axis(4, labels = labels, at = at, las = 1)
}

#plot matrix of h from vector of h
plotMatH <- function (h_new, xlim = NA, ylim = NA,type = "h",border = NA, text = FALSE, range = NA){
  if (is.na(xlim) & is.na(ylim)) xlim = ylim = sqrt(length(h_new))
  if (type == 'h') h_new[h_new == 0] <- NA
  colorPlot <- highlight(h_new, colrange = rainbow(32), steps = 50, range = c(0, 15))
  if (type == "env") colorPlot <- highlight(h_new, colrange = rev(grey.colors(5)), steps = 50, range = c(0.6, 0.8))
  if (type == "prob") colorPlot <- highlight(h_new, colrange = rev(grey.colors(50)), steps = 50, range = c(0, 1))
  if (!is.na(range[1])) colorPlot <- highlight(h_new, colrange = rev(grey.colors(50)), steps = 50, range = range)
  
  plot(NA,xlim = c(0,xlim), ylim = c(ylim,0),type = "n", bty = "n", ann = FALSE, axes = FALSE, asp = 1)
  rect(rep(1:xlim, ylim),rep(1:ylim, each = xlim),rep(0:(xlim-1), ylim),rep(0:(ylim-1), each = xlim), col = colorPlot, border = border)
  if(text == TRUE) text(rep(1:xlim, ylim)-0.5,rep(1:ylim, each = xlim)-0.5, h_new)
}

plotH <- function (xax = FALSE) {
  #get informations from file
  width <- result$lattice$width
  height <- result$lattice$height
  timesteps <- result$lattice$timesteps
  resolution <- result$lattice$resolution
  e_type <- result$parms$e_type 
  
  hmaxi = 15
  #set graphical options
  
  
  par(mar=c(1, 6, 3, 0))
  plot(NA,NA, xlim = c(0,timesteps+5000), ylim = c(1.5,14.5), axes = F, ann = F)
  
  axis(1,labels = NA, lwd = 1.5, cex.axis = 1)
  axis(2, las = 1, at = seq(2,14,4), labels = seq(2,14,4), lwd = 1.5, cex.axis = 1)
  
  mtext(side = 2, line = 2.5, text = expression(trait~values~(italic(h))), cex = 1)
  
  if (xax == TRUE) {
    mtext(side = 1, line = 2.5, text = 'time (years)', cex = 1)
    axis(1, cex = 2, lwd = 1.5, cex.axis = 1)
  }
  
  #add initial environment value
  text(0,13,substitute(list(E[0] ~'='~ list(r)), list(r = result$parms$eo)), pos = 4, cex = 1.5)
  
  h_unique <- lapply(result$h, unique)
  for(i in 1:length(h_unique)) {
    color <- highlight(h_unique[[i]], colrange = rainbow(32), steps = 50, range = c(0,hmaxi))
    color[h_unique[[i]] == 0] <- '#FFFFFF'
    
    try(points(rep(i*resolution, times = length(h_unique[[i]])), h_unique[[i]], col = color, pch = 20, cex = 0.4 ))
  }
  if (e_type == "change") {
    text(200000,14,expression(list(E[0] ~'='~ 0.6)), pos = 2, cex = 1.5)
    segments(c(120000,80000),c(0,0),c(120000,80000),c(14,14),lty = 2)
  }
}

plotRho  <- function () {
  width <- result$lattice$width
  height <- result$lattice$height
  timesteps <- result$lattice$timesteps
  resolution <- result$lattice$resolution
  e_type <- result$parms$e_type 
  hmaxi = 15
  
  h_unique <- lapply(result$h, unique)
  
  par(mar=c(4, 6, 0, 0))
  
  plot(NA,NA,xlim = c(0, timesteps+5000), ylim = c(0, 0.9),
       axes = F, ann = F)#, type = 'n', xaxs = 'i', yaxs = 'i' )
  
  axis(2, at = seq(0, 0.6, by = 0.2), las = 1, lwd = 1.5, cex.axis = 1)
  axis(1, cex = 2, lwd = 1.5, cex.axis = 1)
  
  mtext(side = 2, line = 2.5, adj = 1, text = expression(plant~cover~(rho ['+'])), cex = 1)
  mtext(side = 1, line = 2.5, text = 'time (years)', cex = 1)
  
  time = 2:(length(h_unique)+1)
  
  time_ = result$time[seq(1,length(result$time),length(result$time[-1])/length(result$h[-1]))]
  for(k in time) {
    #k= 2
    h <- result$h[[k-1]]
    h[h == 0] <- NA
    h <- sort(h, na.last = TRUE)
    col <- highlight(h, colrange = rainbow(32), steps = 50, range = c(0,hmaxi))
    
    rect(rep(time_[k-1], times = 2500), seq(0,1-1/2500,length = 2500), rep(time_[k], times = 2500), seq(1/2500,1,length = 2500), col = col, border = NA)
  }
  
  if (e_type == "change") {
    segments(c(120000,80000),c(0,0),c(120000,80000),c(14,14),lty = 2)
  }
}

find_cut <- function (x, thresh = 0.2) {
  sh_matrix <- sort(x)
  if (!(length(unique(sh_matrix))==1 && unique(sh_matrix) == 0)){
    pks <- which(diff(sign(diff(sh_matrix, na.pad = FALSE)), na.pad = FALSE) < 0) + 2
    rem <- rem2 <- c()
    for (i in 1:(length(pks)-1)){
      #i = 1
      if (abs(sh_matrix[pks[i]] - sh_matrix[pks[i+1]]) < thresh){
        rem <- c(rem,i+1)
        rem2 <- c(rem2,i)
      }
    }
    pks1 <- pks[-rem]
    pks2 <- pks[-rem2]
    h_groupValues <- (sh_matrix[pks2] - sh_matrix[pks1]) / 2 + sh_matrix[pks1]
    h_groupValues
    h_cut <- (h_groupValues[-1] - h_groupValues[-length(h_groupValues)]) / 2 + h_groupValues[-length(h_groupValues)]
    test <- density(x[which(x > 0)],bw = 0.1)
    #plot(test)
    #abline(v=c(h_groupValues), col = 2)
    #abline(v=h_cut, col = 4)
    list(
      h_cut = h_cut,h_groupValues = h_groupValues, groupNb = length(h_groupValues)
    )
  } else   list(
    h_cut = NA ,h_groupValues = NA, groupNb = NA
  )
}

#plot (very specific, use with extract plot and meanh)
plotMatrixCF <- function(df, 
                         matrixVar, 
                         range = c(min(df[ ,matrixVar], na.rm = TRUE)-0.5, max(df[ ,matrixVar], na.rm = TRUE)+0.5),
                         colpal = rev(heat.colors(15)),
                         steps = 10
                         ) {
  #matrix plot
  par(mar = c(4,4,4,1))
  plot(NA, xlim = c(-0.02,1.01), ylim = c(-0.01,0.51), ann = FALSE, axes = FALSE)
  mtext(side = 1, line = 2.5, expression(italic(f)))
  mtext(side = 2, line = 2.5, expression(italic(c[l])))
  axis(1,at = seq(0,1,0.2))
  axis(2,at = seq(0,0.5,0.1))
  
  colPlot <- highlight(df[,matrixVar], colrange = colpal, range = range, steps = steps)
 # if (matrixVar == "clus") colPlot <- highlight(log(df[ ,matrixVar]), colrange = colpal, range = )
  # if (matrixVar != "count"){
  #   if (matrixVar == "clus" | matrixVar == "rho" ){ colPlot[is.na(df[ ,"rho"])] <- "#FFFFFF"
  #   } else colPlot[is.na(df[ ,"rho"])] <- "#BBBBBB"
  # }
  if (matrixVar == "countNoNA") colPlot <- highlight(df[ ,matrixVar], colrange = (gray.colors(max(df[ ,matrixVar])+1)), steps = max(df[ ,matrixVar])+1, range = c(0,max(df[ ,matrixVar])))
  
  colPlot[is.na(df[ ,"rho"])] <- "#4D4D4D"
  rect(df$f-0.05,df$c-0.025,df$f+0.05,df$c+0.025, col = colPlot, border = FALSE)
  df[ ,matrixVar][is.nan(df[ ,matrixVar])] <- NA
  #text(df$f,df$c, round(df[ ,matrixVar],2))
  ext <- rep("", length(df$f)*length(df$c))
  if (matrixVar != "count") {
    ext[is.na(df$rho)] <- "C"
    ext[df$extE == 0] <- "E"
    #text(df$f,df$c, ext, col = "#FFFFFF", cex = .75)
  }
  
  
}

#summary of the metrics choosen to describe the simluation results
meanh <- function (x){
  data.frame(group = mean(x[,"group"], na.rm = TRUE),
             groupsd = sd(x[,"group"], na.rm = TRUE),
             h = mean(x[,"hmean"], na.rm = TRUE),
             hsd = mean(x[,"hsd"], na.rm = TRUE),
             clus = mean(x[,"clus"], na.rm = TRUE),
             rho = mean(x[,"rho"][x[,"rho"] != 0], na.rm = TRUE),
             count = length(!is.na(!is.nan(x[,"group"]))),
             countNoNA = length(x[,"group"][!is.na(x[,"group"])]),
             extE = mean(x[,"extE"], na.rm = TRUE)
  )
}

#extract data from different simulation in the selected folder

extractData <- function(folders, extH = FALSE){
  files = c()
  for (j in 1:length(folders)){
    #get the name of all the files
    #setwd(folders[j])
    files <- c(files, paste0(folders[j], sort(dir(folders[j]))))
  }
  interact <- setInteract()
  ini <- rep(NA, length(files))
  ini2 = ini * 2500
  stoDf <- data.frame(hmean = ini, hsd = ini, c = ini, f = ini, env = ini, type = ini, group = ini, rho = ini, clus = ini, ho = ini)
  hDf <- data.frame(c = ini2, f = ini2, env = ini2, type = ini2, ho = ini2, h = ini2)
  for (l in 1:length(files)){
    load(files[l])
    stoDf[l, "c"]     <- result$parms$c_intra
    stoDf[l, "f"]     <- result$parms$f
    stoDf[l, "env"]   <- result$parms$eo
    stoDf[l, "type"]  <- result$parms$e_type
    stoDf[l, "hmean"] <- mean(result$h_final[result$h_final != 0])
    stoDf[l, "hsd"]   <- sd(result$h_final[result$h_final != 0])
    stoDf[l, "group"] <- find_cut(result$h_final)$groupNb
    stoDf[l, "rho"]   <- result$rho$T[2001]
    stoDf[l, "ho"]    <- result$parms$ho
    stoDf[l, "extE"]  <- result$rho$T[700]
    x <- as.numeric(unlist(result$x_final))[1:2500]
    x[x == 2] <- 0
    rho = mean(x)
    Qtt <- rowSums( sapply(interact$interact,	function(j) (x == 1)[interact$x_with_border][interact$x_to_evaluate+j]))/4     
    clust_data <- (Qtt[which(x==1)]/rho)
    stoDf[l, "clus"]  <- mean(clust_data)
    if (extH == TRUE){
    hDf[((l-1)*2500+1):(2500*l), "c"]     <- result$parms$c_intra
    hDf[((l-1)*2500+1):(2500*l), "f"]     <- result$parms$f
    hDf[((l-1)*2500+1):(2500*l), "env"]   <- result$parms$eo
    hDf[((l-1)*2500+1):(2500*l), "type"]  <- result$parms$e_type
    hDf[((l-1)*2500+1):(2500*l), "h"]     <- result$h_final
    hDf[((l-1)*2500+1):(2500*l), "ho"]    <- result$parms$ho
    }
  }
  res <- list(stoDf, hDf)
  names(res) <- c("stoDf","hDf")
  res
}


setInteract <- function(width = 50, height = 50){
  X <- matrix(as.integer(1:(width*height)), ncol = width, byrow =TRUE)
  # setting the border of the evaluation matrix X
  X <- cbind(X[,width], X, X[,1] )  
  X <- rbind(X[height,], X, X[1,] ) 
  # transformation vector which adds the border to the lattice:
  x_with_border <- as.integer(t(X))
  
  # from the matrix X (with copied border cells), which cells are the actual cells (reverse transformation vector of the previous lines) 
  x_to_evaluate <- sort(matrix(1:prod(dim(X)), ncol = dim(X)[2], byrow =TRUE)[-c(1, dim(X)[1]), -c(1,dim(X)[2])]	)		
  
  # defining the neighborhood which is to be evaluated	
  # set interaction matrix
  I <- matrix(c(0,1,0,1,NA,1,0,1,0), ncol = 3, byrow = TRUE)	
  # coordinates of neighbours in Interaction matrix I: 
  neighbours_in_I <- which(is.finite(abs(I)/abs(I)), arr.in = TRUE)
  # coordinates relative to the evaluated cell (=  which(is.na(I) ) 
  relrow <- neighbours_in_I[,1]-which(is.na(I), arr.ind = TRUE)[1]
  relcol <- neighbours_in_I[,2]-which(is.na(I), arr.ind = TRUE)[2]
  
  # relative position of the four direct neighbours of a cell
  interact <- (relrow * dim(X)[2] + relcol)
  neighMatrices <- list(interact,x_with_border,x_to_evaluate)
  names(neighMatrices) <- c('interact', 'x_with_border', 'x_to_evaluate')
  neighMatrices
}
