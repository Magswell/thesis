#' # Model comparison for point counts: Eastern Towhee
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
#' This csv contains all of the EATO count data (v1 through v12) and all of the
#' site and observation covariates.
#' 
#' Site level covariates (impact abundance) -> Canopy cover and number of open
#' grown oaks
#' 
#' Observation covariates (impact detection probability) are Time and 
#' Date (minutes from dawn, days from May 1).
#' 
#' Note that the site covariates are averages from 5 habitat survey points 
#' within each plot

#' Load eato data
eato.data <- read.csv("data/raw_data/20171024_EATO_pc.csv") 

#' Load plot-level data
plot.data <- read.csv("data/processed_data/plot_level_data.csv", row.names = 1)

#' ## Merge plot-level data with all.data
merged.data <- merge(eato.data, plot.data, by = "plot")

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
eato.y <- all.data[,c("v1", "v2", "v3","v4", "v5", "v6","v7", 
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
  unmarkedFramePCO(y=eato.y,
                   # Number of primary survey occasions (number of years)
                   numPrimary = 2,
                   siteCovs=data.frame(canopy=all.data$canopy.z, 
                                       oaks=all.data$oaks.z, 
                                       totgrass=all.data$totgrass.z,
                                       numwood=all.data$numwood.z,
                                       DY_pre2015=all.data$DY_pre2015), 
                   ## Note that we are using the coefficients of variation here,
                   # so the variation from the mean value
                   obsCovs=list(time=all.data[,c("t1", "t2", "t3","t4", "t5", 
                                                 "t6","t7", "t8", "t9","t10", 
                                                 "t11", "t12")],
                                date=all.data[,c("d1", "d2", "d3","d4", "d5", 
                                                 "d6","d7", "d8", "d9","d10", 
                                                 "d11", "d12")]),
                   yearlySiteCovs = yearly.site.covs
                   )  

#'
#' View and examine UMF structure
summary(analysis.umf)
#analysis.umf

#' Date mean/SD so we can back-transform later
date <- as.matrix(all.data[,c("d1", "d2", "d3","d4", "d5", 
                 "d6","d7", "d8", "d9","d10", 
                 "d11", "d12")])
date.mean.eato <- mean(c(date), na.rm = T)
date.sd.eato <- sd(c(date), na.rm = T)
date.test <- scale(date)

#' Standardize covariates after making the UMF
obsCovs(analysis.umf) <- scale(obsCovs(analysis.umf))
# summary(analysis.umf) 

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


#' ### Model comparison
#' 
#' Put the fitted models in a "fitList"
#' Fit list is an unmarked tool to organize models for selection or 
#' model-averaged prediction
dfms <- fitList("lam(.)p(.)gamma(.)omega(.)-P"  = abnd.null.P,
                "lam(.)p(.)gamma(.)omega(.)-ZIP"  = abnd.null.ZIP,
                "lam(.)p(.)gamma(.)omega(.)-NB"  = abnd.null.NB,
                # canopy cover models
                "lam(canopy)p(.)gamma(.)omega(.)-P"  = abnd.c.P,
                "lam(canopy)p(.)gamma(.)omega(.)-ZIP"  = abnd.c.ZIP,
                "lam(canopy)p(.)gamma(.)omega(.)-NB"  = abnd.c.NB,
                # recent disturbance models
                "lam(dy_pre2015)p(.)gamma(.)omega(.)-P"  = abnd.d.P,
                "lam(dy_pre2015)p(.)gamma(.)omega(.)-ZIP"  = abnd.d.ZIP,
                "lam(dy_pre2015)p(.)gamma(.)omega(.)-NB"  = abnd.d.NB)
#'
#'
#'
# Rank them by AIC
# modSel is a way to model selection results
(dms <- modSel(dfms)) 

#' _____________________________________________________________________________
#' ## Probability of detection model
#' 
#' Now model detection based on a Poisson distribution model with canopy for
#' initial abundance
#' 
#' Canopy cover model for initial abundance with Poisson distribution +
#' date and time for detection
(abnd.c.P.pDT <- pcountOpen(lambdaformula = ~canopy, # initial abundance
                            pformula = ~date + time, # detection model
                            gammaformula = ~1,  # recruitment
                            omegaformula = ~1, # survival probability
                            data = analysis.umf,
                            mixture = "P", 
                            K=20))
#' Canopy cover model for initial abundance with Poisson distribution +
#' date for detection
(abnd.c.P.pD <- pcountOpen(lambdaformula = ~canopy, # initial abundance
                            pformula = ~date, # detection model
                            gammaformula = ~1,  # recruitment
                            omegaformula = ~1, # survival probability
                            data = analysis.umf,
                            mixture = "P", 
                            K=20))
#' Canopy cover model for initial abundance with Poisson distribution +
#' time for detection
(abnd.c.P.pt <- pcountOpen(lambdaformula = ~canopy, # initial abundance
                           pformula = ~time, # detection model
                           gammaformula = ~1,  # recruitment
                           omegaformula = ~1, # survival probability
                           data = analysis.umf,
                           mixture = "P", 
                           K=20))



#' Model selection
dfms.p <- fitList("lam(canopy)p(.)gamma(.)omega(.)-P"  = abnd.c.P,
                "lam(canopy)p(date+time)gamma(.)omega(.)-P"  = abnd.c.P.pDT,
                "lam(canopy)p(date)gamma(.)omega(.)-P"  = abnd.c.P.pD,
                "lam(canopy)p(time)gamma(.)omega(.)-P" = abnd.c.P.pt)
(dms.p <- modSel(dfms.p)) 

#' _____________________________________________________________________________
#' ## Final models
#' 
#' Canopy cover model for initial abundance with Poisson distribution
(abnd.c.P.pD <- pcountOpen(lambdaformula = ~canopy, # initial abundance
                        pformula = ~date, # detection model
                        gammaformula = ~1,  # recruitment
                        omegaformula = ~1, # survival probability
                        data = analysis.umf,
                        mixture = "P", 
                        K=20))

#' Null model with Poisson distribution
(abnd.null.P.pD <- pcountOpen(lambdaformula = ~1, # initial abundance
                           pformula = ~date, # detection model
                           gammaformula = ~1,  # recruitment
                           omegaformula = ~1, # survival probability
                           data = analysis.umf,
                           mixture = "P", 
                           K=20))

#' Disturbance model for initial abundance with Poisson distribution
(abnd.d.P.pD <- pcountOpen(lambdaformula = ~DY_pre2015, # initial abundance
                           pformula = ~date, # detection model
                           gammaformula = ~1,  # recruitment
                           omegaformula = ~1, # survival probability
                           data = analysis.umf,
                           mixture = "P", 
                           K=20))

#' Model summary
dfms.final <- fitList("lam(canopy)p(date)gamma(.)omega(.)-P"  = abnd.c.P.pD,
                      "lam(null)p(date)gamma(.)omega(.)-P"  = abnd.null.P.pD,
                      "lam(disturbance)p(date)gamma(.)omega(.)-P"  = abnd.d.P.pD)
(dms.final <- modSel(dfms.final)) 


#' _____________________________________________________________________________
#' ## Save files
#' 
save(abnd.c.P.pD, file = "data/output_data/eato_abnd_c_P_pD.Rdata")
save(abnd.null.P.pD, file = "data/output_data/eato_abnd_null_P_pD.Rdata")
save(abnd.d.P.pD, file = "data/output_data/eato_abnd_d_P_pD.Rdata")
#' SD/Mean of date
save(date.sd.eato, date.mean.eato, 
     file = "data/output_data/eato_date_meanSD.Rdata")

#' 
#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/03_abundance_eato.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 
#' *You can run the ezspin() code above to create an html in the "output" folder
#' and any associated figures will get put into the "figures" folder.*