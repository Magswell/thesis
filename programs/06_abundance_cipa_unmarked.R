#' # Model selection for point counts: Northern Barrens Tiger Beetle
#' 
#' Description: Use unmarked to test models, 
#' AIC to compare, and select 1 final model for future predictions
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
cipa.abnd <- read.csv("data/processed_data/cipa_abundance_data.csv")

#' Load cipa survey data in wide format (JAGS format)
cipa.wide <- read.csv("data/processed_data/cipa_wide_JAGS.csv", row.names = 1)

#' Load plot-level data
plot.data <- read.csv("data/processed_data/plot_level_data.csv", row.names = 1)

#' ## Merge plot-level data with all.data
merged.data <- merge(cipa.abnd, plot.data, by = "plot")

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
  unmarkedFramePCO(y=cipa.y,
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
#' ## PART 2: Model Fitting
#' 
#' Try ZIP, NB, and regular Poisson (default)
#' 
#' This section of model fitting is just for initial abundance, with 
#' detection and dynamic (gamma, omega) variables not included
#'
#'______________________________________________________________________________
#' ### Initial Abundance models
#' 
#' 
#' Null model with Poisson distribution
(abnd.null.P <- pcountOpen(lambdaformula = ~1, # initial abundance
                           pformula = ~1, # detection model
                           gammaformula = ~1,  # recruitment
                           omegaformula = ~1, # survival probability
                           data = analysis.umf,
                           mixture = "P", 
                           K=50))
#' Null model with Negative Binomial distribution
(abnd.null.NB <- pcountOpen(lambdaformula = ~1, # initial abundance
                            pformula = ~1, # detection model
                            gammaformula = ~1,  # recruitment
                            omegaformula = ~1, # survival probability
                            data = analysis.umf,
                            mixture = "NB", 
                            K=50))
#' Null model with ZIP distribution
(abnd.null.ZIP <- pcountOpen(lambdaformula = ~1, # initial abundance
                            pformula = ~1, # detection model
                            gammaformula = ~1,  # recruitment
                            omegaformula = ~1, # survival probability
                            data = analysis.umf,
                            mixture = "ZIP", 
                            K=50))
#'
#' Canopy cover model for initial abundance with Poisson distribution
(abnd.c.P <- pcountOpen(lambdaformula = ~canopy , # initial abundance
                            pformula = ~1, # detection model
                            gammaformula = ~1,  # recruitment
                            omegaformula = ~1, # survival probability
                            data = analysis.umf,
                            mixture = "P", 
                            K=50))
#' Canopy cover model for initial abundance with Negative Binomial distribution
(abnd.c.NB <- pcountOpen(lambdaformula = ~canopy , # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "NB", 
                         K=50))
#' Canopy cover model for initial abundance with ZIP distribution
(abnd.c.ZIP <- pcountOpen(lambdaformula = ~canopy , # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "ZIP", 
                         K=50))
#'
#'
#' Litter model for initial abundance with Poisson distribution
(abnd.l.P <- pcountOpen(lambdaformula = ~litter , # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "P", 
                         K=50))
#' Litter model for initial abundance with Negative Binomial distribution
(abnd.l.NB <- pcountOpen(lambdaformula = ~litter , # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "NB", 
                         K=50))
#' Litter model for initial abundance with ZIP distribution
(abnd.l.ZIP <- pcountOpen(lambdaformula = ~litter , # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "ZIP", 
                         K=50))
#' Coefficient of Variation of elevation model for initial abundance with POISSON distribution
(abnd.e.P <- pcountOpen(lambdaformula = ~elev.CV, # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "P", 
                         K=50))
#' Coefficient of Variation of elevation model for initial abundance with Negative Binomial distribution
(abnd.e.NB <- pcountOpen(lambdaformula = ~elev.CV, # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "NB", 
                         K=50))
#' Coefficient of Variation of elevation model for initial abundance with ZIP distribution
(abnd.e.ZIP <- pcountOpen(lambdaformula = ~elev.CV, # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "ZIP", 
                         K=50))

#' ### Model selection
#' 
#' Put the fitted models in a "fitList"
#' Fit list is an unmarked tool to organize models for selection or 
#' model-averaged prediction
dfms <- fitList("lam(.)p(.)gamma(.)omega(.)-P"  = abnd.null.P,
                "lam(.)p(.)gamma(.)omega(.)-NB"  = abnd.null.NB,
                "lam(.)p(.)gamma(.)omega(.)-ZIP"  = abnd.null.ZIP,
                # canopy cover models
                "lam(canopy)p(.)gamma(.)omega(.)-P"  = abnd.c.P,
                "lam(canopy)p(.)gamma(.)omega(.)-NB"  = abnd.c.NB,
                "lam(canopy)p(.)gamma(.)omega(.)-ZIP"  = abnd.c.ZIP,
                # litter depth models
                "lam(litter)p(.)gamma(.)omega(.)-P"  = abnd.l.P,
                "lam(litter)p(.)gamma(.)omega(.)-NB"  = abnd.l.NB,
                "lam(litter)p(.)gamma(.)omega(.)-ZIP"  = abnd.l.ZIP,
                # elevation model
                "lam(elev.CV)p(.)gamma(.)omega(.)-P"  = abnd.e.P,
                "lam(elev.CV)p(.)gamma(.)omega(.)-NB"  = abnd.e.NB,
                "lam(elev.CV)p(.)gamma(.)omega(.)-ZIP"  = abnd.e.ZIP
                )
#'
#'
#'
# Rank them by AIC
# modSel is a way to model selection results
(dms <- modSel(dfms)) 

#' _____________________________________________________________________________
#' ## Save files for results table
#' 
save(abnd.e.NB, file = "data/output_data/cipa_abnd_e_NB.Rdata") #elevCV
save(abnd.null.NB, file = "data/output_data/cipa_abnd_null_NB.Rdata") #null
save(abnd.l.NB, file = "data/output_data/cipa_abnd_l_NB.Rdata")#litter
save(abnd.c.NB, file = "data/output_data/cipa_abnd_c_NB.Rdata") #canopy

#' 
#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/06_abundance_cipa_unmarked.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 
#' *You can run the ezspin() code above to create an html in the "output" folder
#' and any associated figures will get put into the "figures" folder.*