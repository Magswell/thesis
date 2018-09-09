#' # Final HELE Model in JAGS
#' 
#' Description: [insert description here XXX]
#' 
#' ### Preamble
#' 
#' Load libraries
#+ libraries, message = F, warning = F
library(knitr) # documentation-related
library(ezknitr) # documentation-related
library(devtools) # documentation-related
library(jagsUI)

#' Clear environment and set seed
#' 
remove(list = ls())
set.seed(2583722)

#' _____________________________________________________________________________
#' ## Load Data
#' 
hele <- read.csv("data/processed_data/hele_wide_JAGS.csv", header=TRUE)

#' Load hele liatris data
hele.liatris.wrongorder <- read.csv("data/processed_data/hele_liatris_obslvl.csv", row.names = 1)

#' remove old liatris values and add in ones matched by plot
#colnames.liatris <- colnames(hele[,grep(pattern = "^liatris", x = colnames(hele))])
hele <- hele[,-grep(pattern = "^liatris", x = colnames(hele))]

#' merge data
all.data <- merge(hele, hele.liatris.wrongorder, by = "plot")

#' _____________________________________________________________________________
#' ## Prepare data for analysis
#' 
hele.liatris <- all.data[,c("liatris1_num.2015A",
                            "liatris2_num.2015A",
                            "liatris3_num.2015A",
                            "liatris1_num.2015B",
                            "liatris2_num.2015B",
                            "liatris3_num.2015B",
                            "liatris1_num.2016A",
                            "liatris2_num.2016A",
                            "liatris3_num.2016A",
                            "liatris1_num.2016B",
                            "liatris2_num.2016B",
                            "liatris3_num.2016B" 
)]
#' 
#' Liatris sum by plot
hele.y <- hele[,grep("hele_", x = colnames(hele))]
hele.plotLiatris <- as.data.frame(matrix(NA, ncol = 5, nrow = nrow(hele.y)))
colnames(hele.plotLiatris) <- c("sum", "mean", "min", "max", "hele.sum")
columns <- grep(pattern = "^liatris", x = colnames(hele.liatris))
for(rr in 1:nrow(hele.y)){
  hele.plotLiatris$sum[rr] <- sum(as.numeric(hele.liatris[rr,columns]),
                                  na.rm = T)
  hele.plotLiatris$mean[rr] <- mean(as.numeric(hele.liatris[rr,columns]), 
                                    na.rm = T)
  hele.plotLiatris$min[rr] <- min(as.numeric(hele.liatris[rr,columns]),
                                  na.rm = T)
  hele.plotLiatris$max[rr] <- max(as.numeric(hele.liatris[rr,columns]),
                                  na.rm = T)
  hele.plotLiatris$hele.sum[rr] <- sum(as.numeric(hele.y[rr,]),
                                       na.rm = T)
}
hele.plotLiatris.mean <- mean(hele.plotLiatris$mean)
hele.plotLiatris.sd <- sd(hele.plotLiatris$mean)
hele.plotLiatris.z <- (hele.plotLiatris$mean-hele.plotLiatris.mean)/hele.plotLiatris.sd
#' 
#' Bundle data for JAGS
hele.data <- list(y = hele[,grep("hele_", x = colnames(hele))], 
                  n.plots = nrow(hele), 
                  n.surveys = 4, 
                  n.reps = 3, 
                  disturbance = hele$DY_pre2015,
                  liatris = hele.plotLiatris.z,
                  bunchgrass = hele$bunchgrass.z)


#' Generate initial values
max <- z.hele <- numeric() # z.hele is 1 if hele ever detected, else 0
N.hele <- matrix(0, nrow=hele.data$n.plots, ncol=hele.data$n.surveys) # max detected per round
for (pp in 1:hele.data$n.plots){
  max[pp] <- max(hele.data$y[pp,], na.rm = TRUE)
  z.hele[pp] <- ifelse(max[pp] >= 1, yes = 1, no = 0)
  for (ss in 1:hele.data$n.surveys){
    N.hele[pp,ss] <- max(hele.data$y[pp,((ss-1)*3+1):((ss-1)*3+3)])
  }
}

#' Bundle initial values with shifted count estimates and occupancy estimates
hele.inits <- function() list(N.est = N.hele+1, 
                   z = z.hele)

#' _____________________________________________________________________________
#' ## Define JAGS model
#' 
#' With syntax formatting
hele.model <- function(){
  ###### Priors and constraints
  # Priors for abundance
  b0.abund ~ dgamma(0.1, 0.01) # Vague prior for mean initial lambda (mean 10, up to 600)
  log.b0.abund <- log(b0.abund) # convert to log scale for covariate modeling
  b1.abund ~ dnorm(0,0.25) # abundance covariate for disturbance, log scale
  b2.abund ~ dnorm(0,0.25) # abundance covariate for liatris, log scale
  sd.abund ~ dunif(0.1,5) # prior for within-plot among-survey variation in abundance
  tau.abund <- pow(sd.abund,-2) # convert to precision for JAGS
  
  # Priors for zero inflation (occupancy)
  b0.psi ~ dnorm(0,0.25)
  b1.psi ~ dnorm(0,0.25)  # disturbance effect on psi 
  b2.psi ~ dnorm(0,0.25) # covariate for liatris cover, logit scale
  
  # Beta-binomial priors for detection
  alpha.p ~ dgamma(.01,.01)   # Vague priors for a and b for beta distribution (to model p)
  beta.p ~ dgamma(.01,.01)
  p.derived <- alpha.p/(alpha.p+beta.p) # mean detection probability
  
  ###### Likelihood
  # Biological model for plot-level occupancy/abundance
  for (pp in 1:n.plots) {
    # Probability of occupancy
    logit(psi[pp]) <- b0.psi + 
      b1.psi * bunchgrass[pp] + 
      b2.psi * liatris[pp] 
    
    # Site occupancy
    z[pp] ~ dbern(psi[pp])   # is site even occupied (z = 1)?
    
    # Model for mean abundance (move into survey loop for HELE)
    logN.mean[pp] <- log.b0.abund + 
      b1.abund * disturbance[pp] +
      b2.abund * liatris[pp] 
 
    for(ss in 1:n.surveys){
      logN.eps[pp,ss] ~ dnorm(0, tau.abund)  # generate random error on log scale
      N.pred[pp,ss] <- exp(logN.mean[pp] + logN.eps[pp,ss]) # backtransform to real scale
      N.est[pp,ss] ~ dpois(N.pred[pp,ss])
      N.true[pp,ss] <- z[pp]*(N.est[pp,ss]+1) # +1 to create hurdle so all zeroes come from z
    } # ss number of survey periods
  } # pp is row number (plot)
  
  # Observation model for replicated counts
  for(pp in 1:n.plots){
    for(ss in 1:n.surveys){
      for(rr in 1:n.reps){
        # allow each replicate to have independent detection probability
        p[pp,ss,rr] ~ dbeta(alpha.p, beta.p)
        # compare observed counts to estimated detection and abundance
        y[pp,((ss-1)*3+rr)] ~ dbin(p[pp,ss,rr], N.true[pp,ss])
      } # n.reps is number of counts per period (3)
    } # close n.surveys
  } # close n.plots
} # end JAGS model

#' Copied to a text file
sink("hele_jags_zip.txt")
cat("
    model {
  ###### Priors and constraints
    # Priors for abundance
    b0.abund ~ dgamma(0.1, 0.01) # Vague prior for mean initial lambda (mean 10, up to 600)
    log.b0.abund <- log(b0.abund) # convert to log scale for covariate modeling
    b1.abund ~ dnorm(0,0.25) # abundance covariate for disturbance, log scale
    b2.abund ~ dnorm(0,0.25) # abundance covariate for liatris, log scale
    sd.abund ~ dunif(0.1,5) # prior for within-plot among-survey variation in abundance
    tau.abund <- pow(sd.abund,-2) # convert to precision for JAGS
    
    # Priors for zero inflation (occupancy)
    b0.psi ~ dnorm(0,0.25)
    b1.psi ~ dnorm(0,0.25)  # disturbance effect on psi 
    b2.psi ~ dnorm(0,0.25) # covariate for liatris cover, logit scale
    
    # Beta-binomial priors for detection
    alpha.p ~ dgamma(.01,.01)   # Vague priors for a and b for beta distribution (to model p)
    beta.p ~ dgamma(.01,.01)
    p.derived <- alpha.p/(alpha.p+beta.p) # mean detection probability
    
    ###### Likelihood
    # Biological model for plot-level occupancy/abundance
    for(pp in 1:n.plots) {
    # Probability of occupancy
    logit(psi[pp]) <- b0.psi + 
    b1.psi * bunchgrass[pp] + 
    b2.psi * liatris[pp] 
    
    # Site occupancy
    z[pp] ~ dbern(psi[pp])   # is site even occupied (z = 1)?
    
    # Model for mean abundance
    logN.mean[pp] <- log.b0.abund + 
    b1.abund * disturbance[pp] +
    b2.abund * liatris[pp]
    
    for (ss in 1:n.surveys){
    logN.eps[pp,ss] ~ dnorm(0, tau.abund)  # generate random error on log scale
    N.pred[pp,ss] <- exp(logN.mean[pp] + logN.eps[pp,ss]) # backtransform to real scale
    N.est[pp,ss] ~ dpois(N.pred[pp,ss])
    N.true[pp,ss] <- z[pp]*(N.est[pp,ss]+1) # +1 to create hurdle so all zeroes come from z
    } # ss number of survey periods
    } # pp is row number (plot)
    
    # Observation model for replicated counts
    for (pp in 1:n.plots){
    for (ss in 1:n.surveys){
    for (rr in 1:n.reps){
    # allow each replicate to have independent detection probability
    p[pp,ss,rr] ~ dbeta(alpha.p, beta.p)
    # compare observed counts to estimated detection and abundance
    y[pp,((ss-1)*3+rr)] ~ dbin(p[pp,ss,rr], N.true[pp,ss])
    } # n.reps is number of counts per period (3)
    } # close n.surveys
    } # close n.plots
    } # end JAGS model
    ",fill = TRUE) #commented for readability, remove to run
sink()

#' Parameters monitored
parms <- c("b0.abund", "b1.abund", "sd.abund", "b2.abund", 
           "b0.psi", "b1.psi", "b2.psi", 
           "alpha.p", "beta.p", "p.derived",
           "z", "N.true") 

#' MCMC settings (settings for final run, reduce 100-fold for exploratory analysis)
na <- 1000 #final has 1000
ni <- 250000 #final has 250000
nt <- 10 #final has 10
nb <- 50000 #final has 50000
nc <- 3 #final has 3

#' Run the model 
hele_JAGS <- jagsUI(data = hele.data, 
                            inits = hele.inits, 
                            parameters.to.save = parms, 
                            model.file = "hele_jags_zip.txt", 
                            n.adapt = na, 
                            n.chains = nc, 
                            n.thin = nt, 
                            n.iter = ni, 
                            n.burnin = nb, 
                            parallel=FALSE, 
                            codaOnly = c("z","N.true"))
#' Model output
print(hele_JAGS, digits=3)

#' _____________________________________________________________________________
#' ## Save files
#' 
#' 
save(hele_JAGS, file="data/output_data/hele_JAGS_out_final20180801.Rdata")

#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/05_abundance_hele_jags.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 
#' *You can run the ezspin() code above to create an html in the "output" folder
#' and any associated figures will get put into the "figures" folder.*