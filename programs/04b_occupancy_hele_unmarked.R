#' # Variable selection for plot surveys: Leonard's Skipper
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
hele.occ <- read.csv("data/processed_data/hele_abundance_data.csv")


#' Load hele liatris data
hele.liatris <- read.csv("data/processed_data/hele_liatris_obslvl.csv", 
                         row.names = 1)

#' Load hele survey data in wide format
hele.wide <- read.csv("data/processed_data/hele_wide_format.csv", row.names = 1)

#' Load plot-level data
plot.data <- read.csv("data/processed_data/plot_level_data.csv", row.names = 1)

#' ## Merge plot-level data with all.data
merged.data <- merge(hele.occ, plot.data, by = "plot")
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

#' Convert counts to binary
hele.occ <- as.matrix((hele.y > 0) + 0)

#' Liatris data
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
  unmarkedMultFrame(y=hele.occ,
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
#' ## Final models
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
#' Bunchgrass model for occupancy with Negative Binomial distribution
(occ.b.NB <- colext(psiformula = ~bunchgrass , # occupancy
                         pformula = ~1, # detection model
                         gammaformula = ~1,  # colonization
                         epsilonformula = ~1, # extinction probability
                         data = analysis.umf))
#' Nonbunchgrass model for occupancy with Negative Binomial distribution
(occ.nb.NB <- colext(psiformula = ~nonbunchgrass , # occupancy
                          pformula = ~1, # detection model
                          gammaformula = ~1,  # colonization
                          epsilonformula = ~1, # extinction probability
                          data = analysis.umf))
#' Liatris model for occupancy with Negative Binomial distribution
(occ.t.NB <- colext(psiformula = ~liatris , # occupancy
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
#' Recent disturbance model for occupancy with Negative Binomial distribution
(occ.d.NB <- colext(psiformula = ~DY_pre2015, # occupancy
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
                "lam(bunchgrass)p(.)gamma(.)epsilon(.)-NB"  = occ.b.NB,
                "lam(nonbunchgrass)p(.)gamma(.)epsilon(.)-NB"  = occ.nb.NB,
                "lam(liatris)p(.)gamma(.)epsilon(.)-NB"  = occ.t.NB,
                "lam(litter)p(.)gamma(.)epsilon(.)-NB"  = occ.l.NB,
                "lam(dy_pre2015)p(.)gamma(.)epsilon(.)-NB"  = occ.d.NB)
#'
#'
#'
# Rank them by AIC
# modSel is a way to model selection results
(dms <- modSel(dfms)) 


#' _____________________________________________________________________________
#' ## Save files for results table
#' 
save(occ.d.NB, file = "data/output_data/hele_occ_d_NB.Rdata") #disturbance
save(occ.null.NB, file = "data/output_data/hele_occ_null_NB.Rdata") #null
save(occ.t.NB, file = "data/output_data/hele_occ_t_NB.Rdata") #liatris
save(occ.nb.NB, file = "data/output_data/hele_occ_nb_NB.Rdata")#nonbunchgrass
save(occ.b.NB, file = "data/output_data/hele_occ_b_NB.Rdata") #bunchgrass
save(occ.l.NB, file = "data/output_data/hele_occ_l_NB.Rdata")#litter
save(occ.c.NB, file = "data/output_data/hele_occ_c_NB.Rdata") #canopy

#' 
#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/04b_occupancy_hele_unmarked.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 
#' *You can run the ezspin() code above to create an html in the "output" folder
#' and any associated figures will get put into the "figures" folder.*