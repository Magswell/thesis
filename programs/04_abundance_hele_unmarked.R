#' # Model selection for plot surveys: Leonard's Skipper
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
#' This csv contains all of the HELE count data and all of the
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

#' Load hele abundance data
hele.abnd <- read.csv("data/processed_data/hele_abundance_data.csv")


#' Load hele liatris data
hele.liatris <- read.csv("data/processed_data/hele_liatris_obslvl.csv", 
                         row.names = 1)

#' Load hele survey data in wide format
hele.wide <- read.csv("data/processed_data/hele_wide_format.csv", row.names = 1)

#' Load plot-level data
plot.data <- read.csv("data/processed_data/plot_level_data.csv", row.names = 1)

#' ## Merge plot-level data with all.data
merged.data <- merge(hele.abnd, plot.data, by = "plot")
merged.data <- merge(merged.data, hele.liatris, by = "plot")

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
hele.y <- all.data[,c("hele_1.2015A", "hele_2.2015A", "hele_3.2015A",
                      "hele_1.2015B", "hele_2.2015B", "hele_3.2015B",
                      "hele_1.2016A", "hele_2.2016A", "hele_3.2016A",
                      "hele_1.2016B", "hele_2.2016B", "hele_3.2016B"
)]

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

#' Liatris sum by plot
hele.plotLiatris <- as.data.frame(matrix(NA, ncol = 5, nrow = nrow(hele.y)))
colnames(hele.plotLiatris) <- c("sum", "mean", "min", "max", "hele.sum")
columns <- grep(pattern = "liatris", x = colnames(hele.liatris))
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



#' New yearly-site-covariate for whether plots were disturbed in 2015 or 2016
yearly.site.covs <- list(
  UDY_ge2015 = matrix(c(all.data$UDY_ge2015, rep(0, nrow(all.data))),
                      nrow = nrow(all.data),
                      ncol = 2,
                      byrow = F)
)

#' Create unmarkedFrame
# We give it the y-data (counts), site covariates (influence abundance), 
# and observation covariates (influence detection probability)
analysis.umf <- 
  unmarkedFramePCO(y=hele.y,
                   # Number of primary survey occasions (number of years)
                   numPrimary = 2,
                   siteCovs=data.frame(canopy=all.data$canopy.z, 
                                       bunchgrass=all.data$bunchgrass.z,
                                       litter=all.data$litter.z,
                                       DY_pre2015=all.data$DY_pre2015,
                                       liatris=hele.plotLiatris.z,
                                       nonbunchgrass=all.data$nonbunch.z), 
                   ## Note that we are using the coefficients of variation here,
                   # so the variation from the mean value
                   obsCovs=NULL,
                   yearlySiteCovs = yearly.site.covs)  
#'
#' Standardize covariates after making the UMF
#obsCovs(analysis.umf) <- scale(obsCovs(analysis.umf))
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
#' Canopy cover for initial abundance with Poisson distribution
(abnd.c.P <- pcountOpen(lambdaformula = ~canopy , # initial abundance
                        pformula = ~1, # detection model
                        gammaformula = ~1,  # recruitment
                        omegaformula = ~1, # survival probability
                        data = analysis.umf,
                        mixture = "P", 
                        K=20))
#' Canopy cover for initial abundance with ZIP distribution
(abnd.c.ZIP <- pcountOpen(lambdaformula = ~canopy , # initial abundance
                          pformula = ~1, # detection model
                          gammaformula = ~1,  # recruitment
                          omegaformula = ~1, # survival probability
                          data = analysis.umf,
                          mixture = "ZIP", 
                          K=20))
#' Canopy cover model for initial abundance with Negative Binomial distribution
(abnd.c.NB <- pcountOpen(lambdaformula = ~canopy , # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "NB", 
                         K=20))

#' Bunchgrass for initial abundance with Poisson distribution
(abnd.b.P <- pcountOpen(lambdaformula = ~bunchgrass , # initial abundance
                        pformula = ~1, # detection model
                        gammaformula = ~1,  # recruitment
                        omegaformula = ~1, # survival probability
                        data = analysis.umf,
                        mixture = "P", 
                        K=20))
#' Bunchgrass for initial abundance with ZIP distribution
(abnd.b.ZIP <- pcountOpen(lambdaformula = ~bunchgrass , # initial abundance
                          pformula = ~1, # detection model
                          gammaformula = ~1,  # recruitment
                          omegaformula = ~1, # survival probability
                          data = analysis.umf,
                          mixture = "ZIP", 
                          K=20))
#' Bunchgrass model for initial abundance with Negative Binomial distribution
(abnd.b.NB <- pcountOpen(lambdaformula = ~bunchgrass , # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "NB", 
                         K=20))

#' Nonbunchgrass for initial abundance with Poisson distribution
(abnd.nb.P <- pcountOpen(lambdaformula = ~nonbunchgrass , # initial abundance
                        pformula = ~1, # detection model
                        gammaformula = ~1,  # recruitment
                        omegaformula = ~1, # survival probability
                        data = analysis.umf,
                        mixture = "P", 
                        K=20))
#' Nonbunchgrass for initial abundance with ZIP distribution
(abnd.nb.ZIP <- pcountOpen(lambdaformula = ~nonbunchgrass , # initial abundance
                          pformula = ~1, # detection model
                          gammaformula = ~1,  # recruitment
                          omegaformula = ~1, # survival probability
                          data = analysis.umf,
                          mixture = "ZIP", 
                          K=20))
#' Nonbunchgrass model for initial abundance with Negative Binomial distribution
(abnd.nb.NB <- pcountOpen(lambdaformula = ~nonbunchgrass , # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "NB", 
                         K=20))

#' Liatris for initial abundance with Poisson distribution
(abnd.t.P <- pcountOpen(lambdaformula = ~liatris , # initial abundance
                        pformula = ~1, # detection model
                        gammaformula = ~1,  # recruitment
                        omegaformula = ~1, # survival probability
                        data = analysis.umf,
                        mixture = "P", 
                        K=20))
#' Liatris for initial abundance with ZIP distribution
(abnd.t.ZIP <- pcountOpen(lambdaformula = ~liatris , # initial abundance
                          pformula = ~1, # detection model
                          gammaformula = ~1,  # recruitment
                          omegaformula = ~1, # survival probability
                          data = analysis.umf,
                          mixture = "ZIP", 
                          K=20))
#' Liatris model for initial abundance with Negative Binomial distribution
(abnd.t.NB <- pcountOpen(lambdaformula = ~liatris , # initial abundance
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # recruitment
                         omegaformula = ~1, # survival probability
                         data = analysis.umf,
                         mixture = "NB", 
                         K=20))

#' Litter depth for initial abundance with Poisson distribution
(abnd.l.P <- pcountOpen(lambdaformula = ~litter , # initial abundance
                        pformula = ~1, # detection model
                        gammaformula = ~1,  # recruitment
                        omegaformula = ~1, # survival probability
                        data = analysis.umf,
                        mixture = "P", 
                        K=20))
#' Litter depth for initial abundance with ZIP distribution
(abnd.l.ZIP <- pcountOpen(lambdaformula = ~litter , # initial abundance
                          pformula = ~1, # detection model
                          gammaformula = ~1,  # recruitment
                          omegaformula = ~1, # survival probability
                          data = analysis.umf,
                          mixture = "ZIP", 
                          K=20))
#' Litter model for initial abundance with Negative Binomial distribution
(abnd.l.NB <- pcountOpen(lambdaformula = ~litter , # initial abundance
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


#' ### Model selection
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
                # bunchgrass models
                "lam(bunchgrass)p(.)gamma(.)omega(.)-P"  = abnd.b.P,
                "lam(bunchgrass)p(.)gamma(.)omega(.)-ZIP"  = abnd.b.ZIP,
                "lam(bunchgrass)p(.)gamma(.)omega(.)-NB"  = abnd.b.NB,
                # bunchgrass models
                "lam(nonbunchgrass)p(.)gamma(.)omega(.)-P"  = abnd.nb.P,
                "lam(nonbunchgrass)p(.)gamma(.)omega(.)-ZIP"  = abnd.nb.ZIP,
                "lam(nonbunchgrass)p(.)gamma(.)omega(.)-NB"  = abnd.nb.NB,
                # bunchgrass models
                "lam(liatris)p(.)gamma(.)omega(.)-P"  = abnd.t.P,
                "lam(liatris)p(.)gamma(.)omega(.)-ZIP"  = abnd.t.ZIP,
                "lam(liatris)p(.)gamma(.)omega(.)-NB"  = abnd.t.NB,
                # litter depth models
                "lam(litter)p(.)gamma(.)omega(.)-P"  = abnd.l.P,
                "lam(litter)p(.)gamma(.)omega(.)-ZIP"  = abnd.l.ZIP,
                "lam(litter)p(.)gamma(.)omega(.)-NB"  = abnd.l.NB,
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
#' ## Save files for results table
#' 
save(abnd.d.NB, file = "data/output_data/hele_abnd_d_NB.Rdata") #disturbance
save(abnd.null.NB, file = "data/output_data/hele_abnd_null_NB.Rdata") #null
save(abnd.t.NB, file = "data/output_data/hele_abnd_t_NB.Rdata") #liatris
save(abnd.nb.NB, file = "data/output_data/hele_abnd_nb_NB.Rdata")#nonbunchgrass
save(abnd.b.NB, file = "data/output_data/hele_abnd_b_NB.Rdata") #bunchgrass
save(abnd.l.NB, file = "data/output_data/hele_abnd_l_NB.Rdata")#litter
save(abnd.c.NB, file = "data/output_data/hele_abnd_c_NB.Rdata") #canopy

#' 
#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/04_abundance_hele_unmarked.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 
#' *You can run the ezspin() code above to create an html in the "output" folder
#' and any associated figures will get put into the "figures" folder.*