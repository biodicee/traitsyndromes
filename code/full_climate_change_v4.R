######################################################	## change directory
##											                            		## check number of processors running
##	model: evolutionary dynamics with facilitation!     ## check parameters out commanded
##											          	                  	## check parameters gradients
##	author: Marina & Flo						                  	##
##	date: 16.10.2013					  	                  		##
##										  			                          ##	
######################################################

#PRE SIMULATION
#--------------------
rm(list=ls())

sim_name = "full_climate_change_test"
setwd("/home/sok10hm/Dokumente/FullClimateChangeSimuv4/")
# create and select
#root = "/home/sok10hm/Dokumente/FullClimateChangeSimu/"
#dir.create(paste0(root, "data/", sim_name),  showWarnings = FALSE)
#setwd(paste0(root, "data/", sim_name))
#dir.create("results_file",  showWarnings = FALSE)

#PARAMETERS
#--------------------

#Neigbourhood type
Neigbourhood = "VonNeumann" # VonNeumann = N,E,S,W 
                            #alternative "Moore" Moore = N,NE,E,SE,S,SW,W,NW

#Define gradients
#repetition = 5 #4 
#f_gradient <- 0.4 #seq(0,1,0.1) # intensity of local facilitation (how much neighborhood improves local E) 
#c_intra_gradient <- 0.1 #seq(0,0.5,0.05) # local competition
#eo_gradient = c(0.6) # lattice water availability (Environment)
#h0_init = c(2.5, 6) # range of starting conditions for mean h
#e_type_gradient = c(rep(c("constant", "change"), each = length(f_gradient)*length(c_intra_gradient)*repetition),rep("constant",length(c_intra_gradient)*repetition))

#parameters

parameters <- list(
  ID = NA, #1:((length(c_intra_gradient))*length(f_gradient)*length(eo_gradient)*repetition),
  replicate = 1:10, # sequence of replicates
  c_intra = seq(0,0.5,0.05), #rep(c_intra_gradient, times = length(f_gradient)*length(eo_gradient) * repetition), # lattice water availability (Environment), between 0 and 1, E = 1 + (soil water potential, always negative MPa)
  f = seq(0,1,0.1), # rep(rep(f_gradient, each = length(c_intra_gradient)*repetition),length(eo_gradient)), 
  eo = c(1.1), #rep(eo_gradient, each = length(c_intra_gradient)*length(f_gradient)),
  #e_type = c(rep("constant",length(c_intra_gradient)*length(f_gradient)*length(eo))),
  
  # COLONIZATION
  B = 0.8, # seed production
  del = 0.1, # seeds dispersed globally; (1-del) seeds on nearest neighbourhood
  a = 0.0, # seed bank
  
  # competition
  c_glob = 0.2, # 0.05 # global competition
  #c_intra = 0.15, # 0.1, # 0.05 # strength of local competition (max of local competition, when hdif=0 and exp(-hdif)=1)
  ci = 1, # shape of exponential function of intraspecific competition
  
  # facilitation function parameters (local water availability)
  #eo = 1.2, # OLD R # lattice water availability (Environment), between 0 and 1, E = 1 + (soil water potential, always negative MPa)
  emax = 1.2, # soil water saturation (soil water potential = -1 MPa)
  #f = 0.5, # intensity of local facilitation (how much neighborhood improves local E)
  e_type = "change", # "change", #environment type "change" or "constant"
  eo_2 = 0.6, #environment after climate change
  
  # survival function parameters
  a_s = 7.5, # 2, inclination, maximum survival/2 = 0.5
  g_s = 1, # 1, since 1/g_s = maximum survival = 1
  ho_type = "distri", # "distri" for normal distribution "mono" for single value
  ho = 6, # initial value, h belongs to [2.5(=Q),5.8(=T)], exponent of E, MUTATIONS allowed
  ho_step = 0.05, #sd of h normal distribution
  hmin = 2, #hmax = 6,
  
  # mutation parameters
  Mutation = TRUE, #allow mutation 
  mu = 0.01, #mutation probability
  mu_step = 0.05, # mutation step: sd of the mutation step normal distribution (mean of the normal distribution is the h value)
  
  # MORTALITY and cost function parameters
  mo = 0.01, # intrinsic mortality, initial mortality rate
  a_m = 0.2, # cost
  g_m = 1#, #expoent of survival (1 = linear, <1 decelerating, >1 accelerating)
)

parameters <- expand.grid(parameters)
parameters$ID <- seq_along(parameters$ID)

# saving parameters values in a text file
write.table(parameters,'parameters.txt', col.names=T, row.names=F, append=F)

# specify lattice
width = 50   #default = 50
height = 50  #default = 50

# initial cell states
states = c("T","0")
prob = c(5/10,5/10)

# time and resolution of simulation
timesteps =  200000 #default = 200000
delta = 1/5
collect = 50/delta # saving last collect h_new
resolution = 100/delta # saving simulations every X timestep

# saving lattice values on a text file
write.table(data.frame(width,height,states,prob,timesteps,delta,resolution),'lattice.txt', col.names=T, row.names=F, append=F)


#E_sharp <- c(rep(1.2, (timesteps/delta)/2), rep(0.5, (timesteps/delta)/2+1)) 
changes <- c(1/timesteps,0.4,0.6,1)*(timesteps) #c(1,40000,50000,100000)
E_smooth <- c(rep(parameters$eo[1], changes[2]/delta), seq(parameters$eo[1], parameters$eo_2[1], length = (changes[3]-changes[2])/delta) ,  rep(parameters$eo_2[1], (changes[4]-changes[3])/delta+1 ) ) 
#+ rnorm(timesteps/delta+1, sd = 0.01)


# derive helper vectors for counting: 
# transformation vector for evaluation at the border of the grid
# set evaluation matrix 
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
if (Neigbourhood == "VonNeumann"){
  I <- matrix(c(0,1,0,1,NA,1,0,1,0), ncol = 3, byrow = TRUE)
} else {
  I <- matrix(c(1,1,1,1,NA,1,1,1,1), ncol = 3, byrow = TRUE)
}
# coordinates of neighbours in Interaction matrix I: 
neighbours_in_I <- which(is.finite(abs(I)/abs(I)), arr.in = TRUE)
# coordinates relative to the evaluated cell (=  which(is.na(I) ) 
relrow <- neighbours_in_I[,1]-which(is.na(I), arr.ind = TRUE)[1]
relcol <- neighbours_in_I[,2]-which(is.na(I), arr.ind = TRUE)[2]

# relative position of the four direct neighbours of a cell
interact <- (relrow * dim(X)[2] + relcol)

write.table(data.frame(ID = NA, eo = NA, f=NA, a_m = NA, h_final=NA, h_f_sd=NA, h50 = NA, h50_sd = NA, rho50 = NA, rho50_sd = NA, qTT = NA, qTT_sd = NA, qT0 = NA, qT0_sd = NA, q0T = NA,q0T_sd = NA, q00 = NA, q00_sd = NA, equ_05=NA, equ_01 = NA, equ_001=NA)[-1,], "output.csv", col.names= TRUE, row.names = FALSE)


#allow parallel programming
#install.packages("foreach")
#install.packages("doSNOW")

library(foreach) #required once at the beginning of the programm
library(doSNOW) #required once at the beginning of the programm

nbclusters <- parallel::detectCores(all.tests=FALSE,logical=TRUE)-1
cl <- makeCluster(nbclusters)
registerDoSNOW(cl)


#############################################################################
#############################################################################

run_start <- Sys.time()

foreach(iter = parameters$ID, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {  
  
  # identify worker and set working directory
  #hostname <- system("hostname", intern = TRUE)

  #if(hostname %in% c("kefi-118", "sonia-workstation")) {
  #  root = "~/Documents/MarinaProject/"
  #  dir.create(paste0(root, "data/", sim_name), showWarnings = FALSE)
  #  setwd(paste0(root, "data/", sim_name))
  #  dir.create("results_file", showWarnings = FALSE)
  #}
  
  set.seed(iter)
  
  # sampling the initial random grid into a list object
  initial <- list(  
    dim = c(as.integer(width), as.integer(height)),  # first element contains the dimensions of the landscape 
    cells = sample(factor(1:length(states)), width*height, replace = T, prob = prob ) #second element contains a random row-wise, factorial vector to fill the grid 
  )
  levels(initial$cells) <- states  #assign cell states 
  class(initial) <- c("list","landscape") # set class of object (required for plotting)
  
  # timesteps/(delta*resolution)
  
  #  initialising first iteration 
  x_old <- initial    #old landscape for first timestep   
  parms_temp <- as.list(parameters[iter,]) # copying parms for the simulation and multiplying
  
  #choose h from a normal distribution
  if (parms_temp$ho_type == "distri"){
  ho_ini <- rnorm(length(x_old$cells[x_old$cells == "T"]), parms_temp$ho, parms_temp$ho_step)
  } else ho_ini = parms_temp$ho

  #make vector of h values
  h_initial <- rep(0, width*height)
  #h_initial <- c(parms_temp$ho,0)[x_old$cells] # occupied
  #h_initial <- c(parms_temp$ho,parms_temp$ho)[x_old$cells] # all cells
  h_initial[x_old$cells == "T"] <- ho_ini
  h_old <- h_initial
  
  e_fac <- c(parms_temp$eo,parms_temp$eo)[x_old$cells] # for all cells
  
  
  # initialising result list 
  result <- list()   # create an output list 
  #result$x_initial <- initial
  
  result$parms <- parms_temp
  
  result$lattice <- list()			
  result$lattice$width <- width
  result$lattice$height <- height
  result$lattice$states <- states
  result$lattice$prob_ini <- prob
  result$lattice$timesteps <- timesteps
  result$lattice$delta <- delta
  result$lattice$resolution <- resolution
  
  
  result$time <- seq(0, timesteps, (delta*resolution)) # write simulated timesteps
  
  #save all level global densities: rho_*
  result$rho  <- list()			
  for(i in 1:length(states)) {
    result$rho[[i]] <- numeric(length = timesteps/(delta*resolution)+1) 
    result$rho[[i]][1]  <- sum(initial$cells == states[i])/(width*height) # write initial rho 
  }
  names(result$rho) <- c("T", "0")
  
  
  result$q_  <- list()			
  for(j in 1:(2*length(states))) {
    result$q_[[j]] <- numeric(length = timesteps/(delta*resolution)+1) 
  }
  
  for(j in 1:length(states)) {
    #for each possible cell state do
    neighbors_0 <- numeric(length = length(which(initial$cells == states[j]))) #allocate memory
    neighbors_T <- neighbors_0 #allocate memory
    
    # create a logical vector x(which cells are in state "state"?) 
    # and transfer the vector x to a vector x_with_border: x[x_with_border]
    x_logical_with_border_T <- (initial$cells == "T")[x_with_border]
    x_logical_with_border_0  <- (initial$cells == "0")[x_with_border]
    
    # 3. of those original values which are "state": x_to_evaluate[initial$cells == "state"]
    x_state <- x_to_evaluate[initial$cells == states[j] ]
    
    for(k in interact) {
      neighbors_T <- neighbors_T + x_logical_with_border_T[x_state+k]
      neighbors_0 <- neighbors_0 + x_logical_with_border_0[x_state+k]
      # derive a vector which contains the value to count (also "state") in one of the neighbor cells j
    } 	# cumulate the result for each of the four neighboring cells (for(k in interact))
    
    result$q_[[j]][1]  <- mean(neighbors_T/4)	
    result$q_[[j+2]][1]  <- mean(neighbors_0/4)	
    # write average q values = local densities for each state 
    # devide by 4 to get density, average over all cells
    
  } 
  names(result$q_) <- c("TT", "T0", "0T", "00")
  
  
  result$h  <- list()
  result$h[[1]] <- h_initial # write 'x' as the first entry of the list
  for(i in 1:((timesteps/resolution)+1)) result$h[[i]] <- h_initial	# allocate memory for each timeseries object
  
  result$h_mean <- numeric(length = timesteps/(delta*resolution)+1)
  result$h_mean[1] <- parms_temp$ho
  result$h_sd <- numeric(length = timesteps/(delta*resolution)+1)
  result$h_sd[1] <- 0
  
  #new
  result$h_max <- max(h_initial) # parameter that will save the maximum value of h for the ylim of the final plot
  
  result$h_collect <- c() # higher density of values saved at the end of simulation
  
  result$recol  <- data.frame(time = NA, Qtt = NA, c_local = NA, comp = NA, surv = NA, h = NA, h_parent = NA, inherit_global = NA, mutation = NA, rho = NA)			
  
  #result$timeseries <- list() # create a subordinate list and
  #result$timeseries[[1]] <- initial # write 'x' as the first entry of the list
  #for(i in 1:(timesteps+1)) result$timeseries[[i]] <- initial	# allocate memory for each timeseries object
  
  
  ##############################################################################
  # Real start of the simulation
  for(i in 2:(timesteps/delta)+1) {    #calculation loop 
    # i=2
    
    #parms_temp$eo <-E_sharp[i]
    if(parms_temp$e_type == "change") parms_temp$eo <-E_smooth[i]
    
    
    x_new <- x_old 		# copy x_old into an object x_new to allocate memory
    h_new <- h_old
    
    if(TRUE) {if(all(x_old$cells == "0")) { # Extinction! 
      for(j in i:(timesteps/delta)+1) {
        if(j %in% seq(1,(timesteps/delta)+1, resolution/delta)) result$h[[(j-1)*delta/resolution+1]] <- h_new
        #if(j %in% seq(1,(timesteps/delta)+1, 1/delta)) result$timeseries[[j*delta]] <- x_new 
      }
      print(paste("Extinction",parms_temp$ID, "_Eo",parms_temp$eo,"_f", parms_temp$f))
      break()
    }}
    
    
    
    # model specific part:
    # setting time-step parameters
    
    parms_temp$rho <- sum(x_old$cells == "T")/(width*height) #result$rho[[1]][i-1]   # get old rho plus for this timestep 
    
    parms_temp$Qtt <- rowSums( sapply(interact,	function(j) (x_old$cells == "T")[x_with_border][x_to_evaluate+j]  ))/4     
    
    # finding the survival rate (parameter h) of the parents
    
    rnum1 <- runif(width*height)
    
    # randomly sampling inherited h value from whole grid: 
    # when cell has no neighbors to colonize (parms_temp$Qtt == 0) or global colonization (rnum1<parms_temp$del)
    inherit_global <- (rnum1<parms_temp$del) # true or false, true for global inheritance
    
    # below: creates a fully occupied matrix, which gives the h in case of a global recolonization
    if (length(h_old[x_old$cells == "T"])==1){
      h_new[] <- h_old[x_old$cells == "T"]
    } else {
      h_new[parms_temp$Qtt == 0 & x_old$cells=="0" | inherit_global & x_old$cells=="0"] <- sample(h_old[x_old$cells == "T"], 
            length(which(parms_temp$Qtt == 0 & x_old$cells=="0" | inherit_global & x_old$cells=="0")), replace=T) 
      
      # print(h_new)
      # randomly sampling inherited h value from neighbours:
      # local colonization (rnum1>=parms_temp$del)
      # define the cell that can be locally recolonized
      x_with_local_parent <- which(parms_temp$Qtt > 0 & (!inherit_global) & x_old$cells=="0")
      
      
      if(length(x_with_local_parent) > 0) {
        
        neighb <- sapply(interact,
                         function(x) { h_old[x_with_border][x_to_evaluate[x_with_local_parent]+x]
                         }		
        ) # neighbors of the empty cells that will have local colonization
        
        is_neighb <-sapply(interact,
                           function(x) {  as.integer(x_old$cells == "T")[x_with_border][x_to_evaluate[x_with_local_parent]+x]
                           }		
        ) # transform occupied (parents neighbors) cells in integers (1) for the sampling
        
        # sample inherited h from neighborhood, same probability for each occupied neighbors
        if(length(x_with_local_parent) == 1) { # just because in this case 'neighb' is a vector and not a matrix!
          h_new[x_with_local_parent] = sample(neighb, 1, prob = is_neighb)
        } else { # here 'neighb' is a matrix and thus we can use neighb[x,]
          h_new[x_with_local_parent] = sapply(1:length(x_with_local_parent), function(x) sample(neighb[x,], 1, prob = is_neighb[x,])	)
        }
      }
    }
    
    h_parent <- h_new # inherited h, before mutation
    
    # mutation
    if (parms_temp$Mutation == TRUE){
      
      rnum2 <- runif(width*height)
      
      mutation <- (rnum2<=(parms_temp$mu)) # true or false, true for mutation
      
      for (k in which(mutation & x_old$cells=="0")){ # mutation happens
        h_new[k] <- rnorm(1, mean=h_new[k], sd=(parms_temp$mu_step)) # step/size of the mutation
        if(h_new[k] < 0) h_new[k]= parms_temp$hmin
        if(h_new[k] < parms_temp$hmin) h_new[k]= parms_temp$hmin + (parms_temp$hmin - h_new[k])
        #if(h_new[k] > parms_temp$hmax) h_new[k]= parms_temp$hmax-(h_new[k]- parms_temp$hmax) # if h_new is two times bigger than 6, this becomes negative
        #if(h_new[k] < 0) h_new[k]= parms_temp$hmax # just in case h_new was two times bigger than 6
      }
    }
    
    # applying the rules to fill the cells in x_new
    
    ## Survival probability (intrinsic) 
    so <- with(parms_temp, (a_s*(eo^(h_new))/(1+(g_s*a_s*(eo^(h_new))))))	# NO h+1 anymore
    
    ## Mortality and the cost of having higher survival because of adaptation (before facilitation!!!)
    ## Simon 
    mort <- with(parms_temp, mo + a_m*((so)^(g_m))) 
    #mort <- with(parms_temp, mo + a_m * (1-alpha_m*(h_new-hmin)^gamma/(1 + alpha_m*(h_new-hmin)^gamma)))
    
    ## Local Facilitation
    e_fac <- with(parms_temp,(eo*(1 + (Qtt*(emax-eo)*f))) )
    surv <- with(parms_temp, (a_s*(e_fac^(h_new))/(1+(g_s*a_s*(e_fac^(h_new)))))) 	# NO h+1 anymore
    # facilitation increases survival, but has no cost on mortality
    
    ## Global competition and intraspecific local competition
    h_neighb <-(sapply(interact,	function(j) ((x_old$cells == "T")*h_old)[x_with_border][x_to_evaluate+j] ))	 # matrix with h of the occupied neighbors of each cell	
    occupied_neighbors <-(h_neighb>=parms_temp$hmin) # true or false matrix, true for cells with h values above minimum allowed == occupied cells
    hdif<-abs((h_neighb-h_new)*occupied_neighbors) # differences of cell new h from its neighbors h. Just occupied neighbors!!
    c_neighb <- exp(parms_temp$ci*(-hdif))*(occupied_neighbors) # calculating local competition, again just for occupied neighbors!!
    # ci: parameter of the intraspecific competition exponential function (speed of decay) 
    c_local <- parms_temp$c_intra*(apply(c_neighb, 1, sum)) #c_intra: intraspecific competition parameter
    comp <- with(parms_temp, (c_glob*rho) + c_local)	
    
    ## Transition Rules
    rnum3 <- runif(width*height)
    
    recolonisation <- with(parms_temp, (B*(del*rho+(1-del)*Qtt)+a)*(surv-comp)*delta)
    death <- mort*delta
    # when competition is stronger than survival, no recolonisation:
    # set to 0 to avoid negative terms (possible through competition)
    recolonisation[which(recolonisation <= 0)] <- 0 
    
    # Just double checking...
    if(any(recolonisation > 1)) print(paste("recolonisation larger than one",parms_temp$ID, "_Eo",parms_temp$eo,"_fac", parms_temp$f))
    if(any(recolonisation < 0)) print(paste("NEGATIVE recolonisation",parms_temp$ID, "_Eo",parms_temp$eo,"_fac", parms_temp$f))
    if(any(death > 1)) print(paste("death larger than one",parms_temp$ID, "_Eo",parms_temp$eo,"_fac", parms_temp$f))
    if(any(death < 0)) print(paste("NEGATIVE death",parms_temp$ID, "_Eo",parms_temp$eo,"_fac", parms_temp$f))
    
    recol_event <- (rnum3 <= recolonisation) #TRUE OR FALSE vector
    
    # Applying the rules!!!!
    # Update of the grid
    x_new$cells[which(x_old$cells == "0" & recol_event )] <- "T"
    x_new$cells[which(x_old$cells == "T" & rnum3 <= death)] <- "0"
    
    if(max(h_new) > max(h_old)) result$h_max<-max(h_new) # just for the ylim of the final plot
    
    x_old <- x_new 
    
    h_new[x_new$cells=="0"] <- 0
    h_old <- h_new
    
    # saving the values
    # saving the last 'collect' timesteps valeus
    if(i >= ((timesteps/delta+1)-collect) ) {
      if (any(recol_event)==TRUE){ #new
        result$recol  <- rbind(result$recol, 
                               data.frame(
                                 time = rep(i*delta, times=length(which(recol_event))),
                                 Qtt = parms_temp$Qtt[recol_event], 
                                 c_local = c_local[recol_event], 
                                 comp = comp[recol_event], 
                                 surv = surv[recol_event], 
                                 h = h_new[recol_event], 
                                 h_parent = h_parent[recol_event], 
                                 inherit_global = inherit_global[recol_event], 
                                 mutation = mutation[recol_event], 
                                 rho = parms_temp$rho #[recol_event]
                               )			
        )
        
      }
    }
    
    # saving the last 'collect' timesteps h values of the lattice
    if(i %in% seq( ((timesteps/delta+1)-collect) , (timesteps/delta+1), (1/delta) ) ){
      result$h_collect <- c(result$h_collect, h_new[h_new!=0])
    }
    
    # saving vector h_new each resolution/delta real timesteps
    if(i %in% seq(1,(timesteps/delta+1), resolution/delta)) result$h[[(i-1)/(resolution/delta)+1]] <- h_new 
    # saving vector h_new each resolution/delta real timesteps
    
    # activate to save each single timeseries step
    #if(i %in% seq(1,(timesteps/delta+1), 1/delta)) result$timeseries[[(i-1)*delta+1]] <- x_new  #the whole grid is saved to timeseries
    
    # saving state of the new grid (with the given resolution)
    if( (i-1)/(resolution)+1 == as.integer((i-1)/(resolution)+1) ) {
      #i[which( (i-1)/(resolution)+1 == as.integer((i-1)/(resolution)+1) )]
      
      l <- ((i-1)/(resolution))+1
      
      for(j in 1:length(states)) {
        result$rho[[j]][l]  <- sum(x_new$cells == states[j])/(width*height) # write rhovalues 
      }
      
      for(j in 1:length(states)) {
        #for each possible cell state do
        neighbors_0 <- numeric(length = length(which(x_new$cells == states[j]))) #allocate memory
        neighbors_T <- neighbors_0 #allocate memory
        
        # create a logical vector x(which cells are in state "state"?) 
        # and transfer the vector x to a vector x_with_border: x[x_with_border]
        x_logical_with_border_T <- (x_new$cells == "T")[x_with_border]
        x_logical_with_border_0  <- (x_new$cells == "0")[x_with_border]
        
        # 3. of those original values which are "state": x_to_evaluate[initial$cells == "state"]
        x_state <- x_to_evaluate[x_new$cells == states[j] ]
        
        for(k in interact) {
          neighbors_T <- neighbors_T + x_logical_with_border_T[x_state+k]
          neighbors_0 <- neighbors_0 + x_logical_with_border_0[x_state+k]
          
          # derive a vector which contains the value to count (also "state") in one of the neighbor cells j
          # cumulate the result for each of the four neighboring cells (for(k in interact))
        }
        
        result$q_[[j]][l]  <- mean(neighbors_T/4)	
        result$q_[[j+2]][l]  <- mean(neighbors_0/4)	
        
        # write average q values = local densities for each state 
        # devide by 4 to get density
        # average over all cells
        
      } 
      # h mean and sd
      result$h_mean[l] <- mean(h_new[h_new != 0])
      result$h_sd[l] <- sd(h_new[h_new != 0])
    }
    
    if(i %in% seq(1,(timesteps/delta+1), 50000/delta)[-1])  gc()  #garbage collection each 50.000 real timesteps
    
  } # end of simulation
  
  ###########################################################################
  
  result$x_final <- x_new
  result$h_final <- h_new
  
  #dir.create("/home/marina/Desktop/foreach_two_ssp_TEST/results_file")
  save(result, file = paste("results_file/result", parms_temp$ID, sep = ""))
  
  interval = 50*(resolution*delta) # the last 10.000 real timesteps 
  evaluate <- which(result$time > timesteps-interval) 
  evaluateB <- which(result$time <= timesteps-interval & result$time > timesteps - (2*interval))
  
  h_50 = mean(result$h_mean[evaluate], na.rm = TRUE)
  h_evB = mean(result$h_mean[evaluateB], na.rm = TRUE)
  
  
  #write.table(, "output.csv", col.names= FALSE, row.names = FALSE, append = TRUE)
  gc()  #garbage collection
  return(data.frame(
    ID = parms_temp$ID, 
    Eo = parms_temp$eo, 
    Fac = parms_temp$f, 
    Cost = parms_temp$a_m, 
    h_final = mean(h_new[h_new != 0]),
    h_f_sd = sd(h_new[h_new != 0]), 
    h_50 = h_50, #mean(result$h_mean[evaluate], na.rm = TRUE),
    h_50_sd = mean(result$h_sd[evaluate], na.rm = TRUE), 
    rho50 = mean(result$rho[[1]][evaluate], na.rm = TRUE), 
    rho50_sd = sd(result$rho[[1]][evaluate], na.rm = TRUE),  
    qTT = mean(result$q_$TT[evaluate], na.rm = TRUE),  
    qTT_sd = sd(result$q_$TT[evaluate], na.rm = TRUE),  
    qT0 = mean(result$q_[[2]][evaluate], na.rm = TRUE),
    qT0_sd = sd(result$q_[[2]][evaluate], na.rm = TRUE),  
    q0T = mean(result$q_[[3]][evaluate], na.rm = TRUE), 			
    q0T_sd = sd(result$q_[[3]][evaluate], na.rm = TRUE),  
    q00 = mean(result$q_[[4]][evaluate], na.rm = TRUE),			
    q00_sd = sd(result$q_[[4]][evaluate], na.rm = TRUE),
    equ_05 = abs(h_50-h_evB) < 0.05*parms_temp$mu_step,
    equ_01 = abs(h_50-h_evB) < 0.01*parms_temp$mu_step,
    equ_001 = abs(h_50-h_evB) < 0.001*parms_temp$mu_step
    )
  )
} -> output

write.table(output, "output.csv", col.names = TRUE, row.names= FALSE)

stopCluster(cl)
closeAllConnections()

# get all result files to master
#system(paste0("scp -r sonia@162.38.184.118:/home/sonia/Documents/MarinaProject/data/",
#              sim_name, "/results_file ", root, "data/", sim_name) )

#system(paste0("scp -r sonia@162.38.184.88:/home/sonia/Documents/MarinaProject/data/",
#              sim_name, "/results_file ", root, "data/", sim_name) )

# return runtime
run_end <- Sys.time()
runtime <- difftime(run_end,run_start, units = "mins")
message(paste("runtime:", round(runtime,2), "minutes"))

