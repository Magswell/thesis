#' # Variable selection for point counts: Northern Barrens Tiger Beetle
#' 
#' Description: XX
#' 
#' 
#' 
#' ### Preamble
#' 
#' Load libraries
#+ libraries, message = F, warning = F
library(knitr) # documentation-related
library(ezknitr) # documentation-related
library(devtools) # documentation-related

library(unmarked) # analysis-related

#' Clear environment and set seed
#' 
remove(list = ls())
set.seed(2583722)

#' _____________________________________________________________________________
#' ### Load Data
#' 
#' Import data and check structure
#' This csv contains all of the CIPA count data and all of the
#' site and observation covariates.
#' 
#' Site level covariates (impact abundance) -> Canopy cover, litter depth, and total grass
#' 
#' Observation covariates (impact detection probability) are Time and 
#' Date (minutes from dawn, days from May 1) and observer skill
#' 
#' Note that the site covariates are averages from 5 habitat survey points 
#' within each plot

#' Load cipa abundance data
cipa.occ <- read.csv("data/processed_data/cipa_abundance_data.csv")

#' Load cipa survey data in wide format (JAGS format)
cipa.wide <- read.csv("data/processed_data/cipa_wide_JAGS.csv", row.names = 1)

#' Load plot-level data
plot.data <- read.csv("data/processed_data/plot_level_data.csv", row.names = 1)

#' ## Merge plot-level data with all.data
merged.data <- merge(cipa.occ, plot.data, by = "plot")

#' 
#' Subset Data to SNWR only
# all.data <- merged.data[merged.data$area == "SNWR",]
# when we are no longer subsetting, make this:
all.data <- merged.data

#' _____________________________________________________________________________
#' ## PART 1: Process data for unmarked
#' 
#' 
#' Pull out count matrix 
cipa.y <- all.data[,c("cipa_1.2014A", "cipa_2.2014A", "cipa_3.2014A",
                      "cipa_1.2014B", "cipa_2.2014B", "cipa_3.2014B",
                      "cipa_1.2015A", "cipa_2.2015A", "cipa_3.2015A",
                      "cipa_1.2015B", "cipa_2.2015B", "cipa_3.2015B",
                      "cipa_1.2015C", "cipa_2.2015C", "cipa_3.2015C",
                      #"cipa_1.2015D", "cipa_2.2015D", "cipa_3.2015D",
                      #"cipa_1.2015E", "cipa_2.2015E", "cipa_3.2015E",
                      #"cipa_1.2015F", "cipa_2.2015F", "cipa_3.2015F",
                      "cipa_1.2016A", "cipa_2.2016A", "cipa_3.2016A",
                      "cipa_1.2016B", "cipa_2.2016B", "cipa_3.2016B",
                      "cipa_1.2016C", "cipa_2.2016C", "cipa_3.2016C"
                      #"cipa_1.2016D", "cipa_2.2016D", "cipa_3.2016D"
                      )]
# Convert to binary
cipa.occ <- as.matrix((cipa.y > 0) + 0)

#' Pull out date (day of year; doy) matrix
cipa.doy <- cipa.wide[,c("doy.2014A", "doy.2014A", "doy.2014A",
                         "doy.2014B", "doy.2014B", "doy.2014B",
                         "doy.2015A", "doy.2015A", "doy.2015A",
                         "doy.2015B", "doy.2015B", "doy.2015B",
                         "doy.2015C", "doy.2015C", "doy.2015C",
                         #"doy.2015D", "doy.2015D", "doy.2015D",
                         #"doy.2015E", "doy.2015E", "doy.2015E",
                         #"doy.2015F", "doy.2015F", "doy.2015F",
                         "doy.2016A", "doy.2016A", "doy.2016A",
                         "doy.2016B", "doy.2016B", "doy.2016B",
                         "doy.2016C", "doy.2016C", "doy.2016C"
                         #"doy.2016D", "doy.2016D", "doy.2016D"
                         )]
cipa.doy <- scale(cipa.doy)

#' Pull out time (hours since sunrise) matrix
cipa.time <- cipa.wide[,c("hr.rise.2014A", "hr.rise.2014A", "hr.rise.2014A",
                          "hr.rise.2014B", "hr.rise.2014B", "hr.rise.2014B",
                          "hr.rise.2015A", "hr.rise.2015A", "hr.rise.2015A",
                          "hr.rise.2015B", "hr.rise.2015B", "hr.rise.2015B",
                          "hr.rise.2015C", "hr.rise.2015C", "hr.rise.2015C",
                          #"hr.rise.2015D", "hr.rise.2015D", "hr.rise.2015D",
                          #"hr.rise.2015E", "hr.rise.2015E", "hr.rise.2015E",
                          #"hr.rise.2015F", "hr.rise.2015F", "hr.rise.2015F",
                          "hr.rise.2016A", "hr.rise.2016A", "hr.rise.2016A",
                          "hr.rise.2016B", "hr.rise.2016B", "hr.rise.2016B",
                          "hr.rise.2016C", "hr.rise.2016C", "hr.rise.2016C"
                          #"hr.rise.2016D", "hr.rise.2016D", "hr.rise.2016D"
                          )]
cipa.time <- scale(cipa.time)

#' Pull out observer skill (binary; was the observer experienced) matrix
cipa.obs.skill <- cipa.wide[,c("obs_skill.2014A", "obs_skill.2014A", "obs_skill.2014A",
                               "obs_skill.2014B", "obs_skill.2014B", "obs_skill.2014B",
                               "obs_skill.2015A", "obs_skill.2015A", "obs_skill.2015A",
                               "obs_skill.2015B", "obs_skill.2015B", "obs_skill.2015B",
                               "obs_skill.2015C", "obs_skill.2015C", "obs_skill.2015C",
                               #"obs_skill.2015D", "obs_skill.2015D", "obs_skill.2015D",
                               #"obs_skill.2015E", "obs_skill.2015E", "obs_skill.2015E",
                               #"obs_skill.2015F", "obs_skill.2015F", "obs_skill.2015F",
                               "obs_skill.2016A", "obs_skill.2016A", "obs_skill.2016A",
                               "obs_skill.2016B", "obs_skill.2016B", "obs_skill.2016B",
                               "obs_skill.2016C", "obs_skill.2016C", "obs_skill.2016C"
                               #"obs_skill.2016D", "obs_skill.2016D", "obs_skill.2016D"
                               )]

#' Pull dry bulb temp C matrix
cipa.temp <- cipa.wide[,c("HOURLYDRYBULBTEMPC.2014A", "HOURLYDRYBULBTEMPC.2014A", "HOURLYDRYBULBTEMPC.2014A",
                          "HOURLYDRYBULBTEMPC.2014B", "HOURLYDRYBULBTEMPC.2014B", "HOURLYDRYBULBTEMPC.2014B",
                          "HOURLYDRYBULBTEMPC.2015A", "HOURLYDRYBULBTEMPC.2015A", "HOURLYDRYBULBTEMPC.2015A",
                          "HOURLYDRYBULBTEMPC.2015B", "HOURLYDRYBULBTEMPC.2015B", "HOURLYDRYBULBTEMPC.2015B",
                          "HOURLYDRYBULBTEMPC.2015C", "HOURLYDRYBULBTEMPC.2015C", "HOURLYDRYBULBTEMPC.2015C",
                          "HOURLYDRYBULBTEMPC.2016A", "HOURLYDRYBULBTEMPC.2016A", "HOURLYDRYBULBTEMPC.2016A",
                          "HOURLYDRYBULBTEMPC.2016B", "HOURLYDRYBULBTEMPC.2016B", "HOURLYDRYBULBTEMPC.2016B",
                          "HOURLYDRYBULBTEMPC.2016C", "HOURLYDRYBULBTEMPC.2016C", "HOURLYDRYBULBTEMPC.2016C"
)]

#' New yearly-site-covariate for whether plots were disturbed in 2015 or 2016
yearly.site.covs <- list(
  year = matrix(c(1,1,
                  2,2,2,2,2,
                  3,3,3,3),
                nrow = nrow(all.data), 
                ncol = 8,
                byrow = T)
)

#' Create unmarkedFrame
# We give it the y-data (counts), site covariates (influence abundance), 
# and observation covariates (influence detection probability)
analysis.umf <- 
  unmarkedMultFrame(y=cipa.occ,
                   # Number of primary survey occasions (number of survey periods)
                   numPrimary = 8,
                   siteCovs=data.frame(canopy=all.data$canopy.z, 
                                       totgrass=all.data$totgrass.z,
                                       litter=all.data$litter.z,
                                       DY_pre2014=all.data$DY_pre2014,
                                       elev.CV=all.data$elev_CV.z), 
                   ## Note that we are using the coefficients of variation here,
                   # so the variation from the mean value
                   obsCovs=list(time=cipa.time,
                                date=cipa.doy,
                                date_sqr = cipa.doy*cipa.doy,
                                obs_skill=cipa.obs.skill,
                                tempC = cipa.temp),
                                
                   yearlySiteCovs = yearly.site.covs
                   )  
#'
#' Standardize covariates after making the UMF
#yearlySiteCovs(analysis.umf) <- scale(yearlySiteCovs(analysis.umf))
# summary(analysis.umf) 

#' View and examine UMF structure
summary(analysis.umf)
#analysis.umf



#' _____________________________________________________________________________
#' ## Final models
#' 
#' 
#' Null model with Negative Binomial distribution
(occ.null.NB <- colext(psiformula = ~1, # occupancy
                            pformula = ~1, # detection model
                            gammaformula = ~1,  # colonization
                            epsilonformula = ~1, # extinction probability
                            data = analysis.umf))
#' Canopy cover model for occupancy with Negative Binomial distribution
(occ.c.NB <- colext(psiformula = ~canopy , # occupancy
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # colonization
                         epsilonformula = ~1, # extinction probability
                         data = analysis.umf))
#' Litter model for occupancy with Negative Binomial distribution
(occ.l.NB <- colext(psiformula = ~litter , # occupancy
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # colonization
                         epsilonformula = ~1, # extinction probability
                         data = analysis.umf))
#' Coefficient of Variation of elevation model for occupancy with Negative Binomial distribution
(occ.e.NB <- colext(psiformula = ~elev.CV, # occupancy
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # colonization
                         epsilonformula = ~1, # extinction probability
                         data = analysis.umf))


#' ### Model selection
#' 
#' Put the fitted models in a "fitList"
#' Fit list is an unmarked tool to organize models for selection or 
#' model-averaged prediction
dfms <- fitList("lam(.)p(.)gamma(.)epsilon(.)-NB"  = occ.null.NB,
                "lam(canopy)p(.)gamma(.)epsilon(.)-NB"  = occ.c.NB,
                "lam(litter)p(.)gamma(.)epsilon(.)-NB"  = occ.l.NB,
                "lam(elev.CV)p(.)gamma(.)epsilon(.)-NB"  = occ.e.NB)
#'
#'
#'
# Rank them by AIC
# modSel is a way to model selection results
(dms <- modSel(dfms)) 

#' _____________________________________________________________________________
#' ## Save files for results table
#' 
save(occ.e.NB, file = "data/output_data/cipa_occ_e_NB.Rdata") #elevCV
save(occ.null.NB, file = "data/output_data/cipa_occ_null_NB.Rdata") #null
save(occ.l.NB, file = "data/output_data/cipa_occ_l_NB.Rdata")#litter
save(occ.c.NB, file = "data/output_data/cipa_occ_c_NB.Rdata") #canopy

#' 
#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/06b_occupancy_cipa_unmarked.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 
#' *You can run the ezspin() code above to create an html in the "output" folder
#' and any associated figures will get put into the "figures" folder.*