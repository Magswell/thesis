#' # Model comparison for point counts: Lark Sparrow
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
#' 
#' Import data and check structure
#' This csv contains all of the LASP count data (v1 through v12) and all of the
#' site and observation covariates.
#' 
#' Site level covariates (impact abundance) -> Canopy cover and number of woody
#' stems
#' 
#' Observation covariates (impact detection probability) are Time and 
#' Date (minutes from dawn, days from May 1).
#' 
#' Note that the site covariates are averages from 5 habitat survey points 
#' within each plot

#' Load lasp data
lasp.data <- read.csv("data/raw_data/20171020_LASP_pc.csv") 

#' Load plot-level data
plot.data <- read.csv("data/processed_data/plot_level_data.csv", row.names = 1)

#' ## Merge plot-level data with all.data
merged.data <- merge(lasp.data, plot.data, by = "plot")

#' 
#' Subset Data to SNWR only
# all.data <- merged.data[merged.data$area == "SNWR",]
# when we are no longer subsetting, make this:
all.data <- merged.data

#' _____________________________________________________________________________
#' ## PART 1: Process data for unmarked
#' 
#' 
# Pull out count matrix 
lasp.y <- all.data[,c("v1", "v2", "v3","v4", "v5", "v6","v7", 
                      "v8", "v9","v10", "v11", "v12")]
# str(lasp.y)

#' New yearly-site-covariate for whether plots were disturbed in 2015 or 2016
yearly.site.covs <- list(
  year = matrix(0:1,
                nrow = nrow(all.data), 
                ncol = 2,
                byrow = T),
  DY_ge2015 = matrix(c(all.data$DY_ge2015, rep(0, nrow(all.data))),
                     nrow = nrow(all.data),
                     ncol = 2,
                     byrow = F)
)

#' Create unmarkedFrame
# We give it the y-data (counts), site covariates (influence abundance), 
# and observation covariates (influence detection probability)
analysis.umf <- 
  unmarkedFramePCO(y=lasp.y,
                   # Number of primary survey occasions (number of years)
                   numPrimary = 2,
                   siteCovs=data.frame(canopy=all.data$canopy.z, 
                                       numwood=all.data$numwood.z,
                                       litter=all.data$litter.z,
                                       DY_pre2015=all.data$DY_pre2015), 
                   ## Note that we are using the coefficients of variation here,
                   # so the variation from the mean value
                   obsCovs=list(time=all.data[,c("t1", "t2", "t3","t4", "t5", 
                                                 "t6","t7", "t8", "t9","t10", 
                                                 "t11", "t12")],
                                date=all.data[,c("d1", "d2", "d3","d4", "d5", 
                                                 "d6","d7", "d8", "d9","d10", 
                                                 "d11", "d12")]),
                   yearlySiteCovs = yearly.site.covs)
                     
#'
#' View and examine UMF structure
summary(analysis.umf)
#analysis.umf

#' Date mean/SD so we can back-transform later
date <- as.matrix(all.data[,c("d1", "d2", "d3","d4", "d5", 
                              "d6","d7", "d8", "d9","d10", 
                              "d11", "d12")])
date.mean.lasp <- mean(c(date), na.rm = T)
date.sd.lasp <- sd(c(date), na.rm = T)

#' Time mean/SD so we can back-transform later
time <- as.matrix(all.data[,c("t1", "t2", "t3","t4", "t5", 
                              "t6","t7", "t8", "t9","t10", 
                              "t11", "t12")])
time.mean.lasp <- mean(c(time), na.rm = T)
time.sd.lasp <- sd(c(time), na.rm = T)


#' Standardize covariates after making the UMF
obsCovs(analysis.umf) <- scale(obsCovs(analysis.umf))
# summary(analysis.umf) 

#' _____________________________________________________________________________
#' ## PART 2: Model Fitting
#' 
#' Try ZIP, NB, and regular Poisson (default)
#' 
#' This section of model fitting is just for initial abundance, with 
#' detection (p) and dynamic (gamma, omega) variables not included
#'
#'______________________________________________________________________________
#' ### Initial Abundance models
#' 
#' Null model with Poisson distribution
(abnd.null.P <- pcountOpen(lambdaformula = ~1, # initial abundance
                           pformula = ~1, # detection model
                           gammaformula = ~1,  # recruitment
                           omegaformula = ~1, # survival probability
                           data = analysis.umf,
                           mixture = "P", 
                           K=20))
#' Null model with ZIP distribution
(abnd.null.ZIP <- pcountOpen(lambdaformula = ~1, # initial abundance
                           pformula = ~1, # detection model
                           gammaformula = ~1,  # recruitment
                           omegaformula = ~1, # survival probability
                           data = analysis.umf,
                           mixture = "ZIP", 
                           K=20))
#' Null model with Negative Binomial distribution
(abnd.null.NB <- pcountOpen(lambdaformula = ~1, # initial abundance
                           pformula = ~1, # detection model
                           gammaformula = ~1,  # recruitment
                           omegaformula = ~1, # survival probability
                           data = analysis.umf,
                           mixture = "NB", 
                           K=20))
#' Canopy cover model for initial abundance with Poisson distribution
(abnd.c.P <- pcountOpen(lambdaformula = ~canopy, # initial abundance
                          pformula = ~1, # detection model
                          gammaformula = ~1,  # recruitment
                          omegaformula = ~1, # survival probability
                          data = analysis.umf,
                          mixture = "P", 
                          K=20))
#' Canopy cover model for initial abundance with ZIP distribution
(abnd.c.ZIP <- pcountOpen(lambdaformula = ~canopy, # initial abundance
                            pformula = ~1, # detection model
                            gammaformula = ~1,  # recruitment
                            omegaformula = ~1, # survival probability
                            data = analysis.umf,
                            mixture = "ZIP", 
                            K=20))
#' Canopy cover model for initial abundance with Negative Binomial distribution
(abnd.c.NB <- pcountOpen(lambdaformula = ~canopy, # initial abundance
                           pformula = ~1, # detection model
                           gammaformula = ~1,  # recruitment
                           omegaformula = ~1, # survival probability
                           data = analysis.umf,
                           mixture = "NB", 
                           K=20))
#' Numwood model for initial abundance with Poisson distribution
(abnd.st.P <- pcountOpen(lambdaformula = ~numwood, # initial abundance
                          pformula = ~1, # detection model
                          gammaformula = ~1,  # recruitment
                          omegaformula = ~1, # survival probability
                          data = analysis.umf,
                          mixture = "P", 
                          K=20))
#' Numwood model for initial abundance with ZIP distribution
(abnd.st.ZIP <- pcountOpen(lambdaformula = ~numwood, # initial abundance
                            pformula = ~1, # detection model
                            gammaformula = ~1,  # recruitment
                            omegaformula = ~1, # survival probability
                            data = analysis.umf,
                            mixture = "ZIP", 
                            K=20))
#' Numwood model for initial abundance with Negative Binomial distribution
(abnd.st.NB <- pcountOpen(lambdaformula = ~numwood, # initial abundance
                           pformula = ~1, # detection model
                           gammaformula = ~1,  # recruitment
                           omegaformula = ~1, # survival probability
                           data = analysis.umf,
                           mixture = "NB", 
                           K=20))
#' Recent disturbance model for initial abundance with Poisson distribution
(abnd.d.P <- pcountOpen(lambdaformula = ~DY_pre2015, # initial abundance
                        pformula = ~1, # detection model
                        gammaformula = ~1,  # recruitment
                        omegaformula = ~1, # survival probability
                        data = analysis.umf,
                        mixture = "P", 
                        K=20))
#' Recent disturbance model for initial abundance with ZIP distribution
(abnd.d.ZIP <- pcountOpen(lambdaformula = ~DY_pre2015, # initial abundance
                          pformula = ~1, # detection model
                          gammaformula = ~1,  # recruitment
                          omegaformula = ~1, # survival probability
                          data = analysis.umf,
                          mixture = "ZIP", 
                          K=20))
#' Recent disturbance model for initial abundance with Negative Binomial distribution
(abnd.d.NB <- pcountOpen(lambdaformula = ~DY_pre2015, # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "NB", 
                         K=20))

#' Litter model for initial abundance with Poisson distribution
(abnd.l.P <- pcountOpen(lambdaformula = ~litter, # initial abundance
                        pformula = ~1, # detection model
                        gammaformula = ~1,  # recruitment
                        omegaformula = ~1, # survival probability
                        data = analysis.umf,
                        mixture = "P", 
                        K=20))
#' Litter model for initial abundance with ZIP distribution
(abnd.l.ZIP <- pcountOpen(lambdaformula = ~litter, # initial abundance
                          pformula = ~1, # detection model
                          gammaformula = ~1,  # recruitment
                          omegaformula = ~1, # survival probability
                          data = analysis.umf,
                          mixture = "ZIP", 
                          K=20))
#' Litter model for initial abundance with Negative Binomial distribution
(abnd.l.NB <- pcountOpen(lambdaformula = ~litter, # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "NB", 
                         K=20))

#' ### Model comparison
#' 
#' Put the fitted models in a "fitList"
#' Fit list is an unmarked tool to organize models for selection or 
#' model-averaged prediction
dfms <- fitList(# null models
                "lam(.)p(.)gamma(.)omega(.)-P"  = abnd.null.P,
                "lam(.)p(.)gamma(.)omega(.)-ZIP"  = abnd.null.ZIP,
                "lam(.)p(.)gamma(.)omega(.)-NB"  = abnd.null.NB,
                # canopy cover models
                "lam(canopy)p(.)gamma(.)omega(.)-P"  = abnd.c.P,
                "lam(canopy)p(.)gamma(.)omega(.)-ZIP"  = abnd.c.ZIP,
                "lam(canopy)p(.)gamma(.)omega(.)-NB"  = abnd.c.NB,
                # numwood models
                "lam(numwood)p(.)gamma(.)omega(.)-P"  = abnd.st.P,
                "lam(numwood)p(.)gamma(.)omega(.)-ZIP"  = abnd.st.ZIP,
                "lam(numwood)p(.)gamma(.)omega(.)-NB"  = abnd.st.NB,
                # recent disturbance models
                "lam(dy_pre2015)p(.)gamma(.)omega(.)-P"  = abnd.d.P,
                "lam(dy_pre2015)p(.)gamma(.)omega(.)-ZIP"  = abnd.d.ZIP,
                "lam(dy_pre2015)p(.)gamma(.)omega(.)-NB"  = abnd.d.NB,
                # litter models
                "lam(litter)p(.)gamma(.)omega(.)-P"  = abnd.l.P,
                "lam(litter)p(.)gamma(.)omega(.)-ZIP"  = abnd.l.ZIP,
                "lam(litter)p(.)gamma(.)omega(.)-NB"  = abnd.l.NB)
#'
#'
#'
# Rank them by AIC
# modSel is a way to model selection results
(dms <- modSel(dfms)) 

#' _____________________________________________________________________________
#' ## Probability of detection covariates
#' 
#' Now model detection based on a Poisson distribution model with disturbance for
#' initial abundance
#' 
#' Disturbance model for initial abundance with NB distribution +
#' date and time for detection
(abnd.d.NB.pDT <- pcountOpen(lambdaformula = ~DY_pre2015, # initial abundance
                             pformula = ~date + time, # detection model
                             gammaformula = ~1,  # recruitment
                             omegaformula = ~1, # survival probability
                             data = analysis.umf,
                             mixture = "NB", 
                             K=20))

#' Disturbance model for initial abundance with NB distribution +
#' time for detection
(abnd.d.NB.pT <- pcountOpen(lambdaformula = ~DY_pre2015, # initial abundance
                            pformula = ~time, # detection model
                            gammaformula = ~1,  # recruitment
                            omegaformula = ~1, # survival probability
                            data = analysis.umf,
                            mixture = "NB", 
                            K=20))

#' Canopy cover model for initial abundance with NB distribution +
#' date for detection
(abnd.d.NB.pD <- pcountOpen(lambdaformula = ~DY_pre2015, # initial abundance
                            pformula = ~date, # detection model
                            gammaformula = ~1,  # recruitment
                            omegaformula = ~1, # survival probability
                            data = analysis.umf,
                            mixture = "NB", 
                            K=20))

#' Model comparison
dfms.p <- fitList("lam(DY_pre2015)p(.)gamma(.)omega(.)-NB"  = abnd.d.NB,
                "lam(DY_pre2015)p(date+time)gamma(.)omega(.)-NB"  = abnd.d.NB.pDT,
                "lam(DY_pre2015)p(date)gamma(.)omega(.)-NB"  = abnd.d.NB.pD,
                "lam(DY_pre2015)p(time)gamma(.)omega).)-NB" = abnd.d.NB.pT)
(dms.p <- modSel(dfms.p)) 

#' ____________________________________________________________________________
#' ## Final models for summary table
#' 
#' Null model with Negative Binomial distribution
(abnd.null.NB.pDT <- pcountOpen(lambdaformula = ~1, # initial abundance
                            pformula = ~date + time, # detection model
                            gammaformula = ~1,  # recruitment
                            omegaformula = ~1, # survival probability
                            data = analysis.umf,
                            mixture = "NB", 
                            K=20))
#' Canopy cover model for initial abundance with Negative Binomial distribution
(abnd.c.NB.pDT <- pcountOpen(lambdaformula = ~canopy, # initial abundance
                         pformula = ~date + time, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "NB", 
                         K=20))
#' Numwood model for initial abundance with Negative Binomial distribution
(abnd.st.NB.pDT <- pcountOpen(lambdaformula = ~numwood, # initial abundance
                          pformula = ~date + time, # detection model
                          gammaformula = ~1,  # recruitment
                          omegaformula = ~1, # survival probability
                          data = analysis.umf,
                          mixture = "NB", 
                          K=20))
#' Recent disturbance model for initial abundance with Negative Binomial distribution
(abnd.d.NB.pDT <- pcountOpen(lambdaformula = ~DY_pre2015, # initial abundance
                         pformula = ~date + time, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "NB", 
                         K=20))
#' litter model for initial abundance with Negative Binomial distribution
(abnd.l.NB.pDT <- pcountOpen(lambdaformula = ~litter, # initial abundance
                             pformula = ~date + time, # detection model
                             gammaformula = ~1,  # recruitment
                             omegaformula = ~1, # survival probability
                             data = analysis.umf,
                             mixture = "NB", 
                             K=20))

#' Fit List
dfms.final <- fitList(
  "lam(canopy)p(date+time)gamma(.)omega(.)-NB"  = abnd.c.NB.pDT,
  "lam(numwood)p(date+time)gamma(.)omega(.)-NB"  = abnd.st.NB.pDT,
  "lam(dy_pre2015)p(date+time)gamma(.)omega(.)-NB"  = abnd.d.NB.pDT,
  "lam(null)p(date+time)gamma(.)omega(.)-NB"  = abnd.null.NB.pDT,
  "lam(litter)p(date+time)gamma(.)omega(.)-NB"  = abnd.l.NB.pDT)

#'
#'
#'
# Rank them by AIC
# modSel is a way to model selection results
(dms <- modSel(dfms.final)) 

#' _____________________________________________________________________________
#' ## Save files for results table
#' 
save(abnd.c.NB.pDT, file = "data/output_data/lasp_abnd_c_NB_pDT.Rdata")
save(abnd.st.NB.pDT, file = "data/output_data/lasp_abnd_st_NB_pDT.Rdata")
save(abnd.d.NB.pDT, file = "data/output_data/lasp_abnd_d_NB_pDT.Rdata")
save(abnd.null.NB.pDT, file = "data/output_data/lasp_abnd_null_NB_pDT.Rdata")
save(abnd.l.NB.pDT, file = "data/output_data/lasp_abnd_l_NB_pDT.Rdata")
#' SD/Mean of date
save(date.sd.lasp, date.mean.lasp, 
     time.sd.lasp, time.mean.lasp,
     file = "data/output_data/lasp_dateTime_meanSD.Rdata")


#' 
#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/02_abundance_lasp.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 
#' *You can run the ezspin() code above to create an html in the "output" folder
#' and any associated figures will get put into the "figures" folder.*