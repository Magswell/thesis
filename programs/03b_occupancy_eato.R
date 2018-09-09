#' # Model comparison for occupancy data: Eastern Towhee
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
# Convert to binary
eato.occ <- as.matrix((eato.y > 0) + 0)

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
  unmarkedMultFrame(y=eato.occ,
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
#' ## Final models
#' 
#' Canopy cover model for occupancy 
(occ.c.P.pD <- colext(psiformula = ~canopy, # occupancy
                        pformula = ~date, # detection model
                        gammaformula = ~1,  # colonization
                        epsilonformula = ~1, # extinction probability
                        data = analysis.umf))

#' Null model 
(occ.null.P.pD <- colext(psiformula = ~1, # occupancy
                           pformula = ~date, # detection model
                           gammaformula = ~1,  # colonization
                           epsilonformula = ~1, # extinction probability
                           data = analysis.umf))

#' Disturbance model for occupancy 
(occ.d.P.pD <- colext(psiformula = ~DY_pre2015, # occupancy
                           pformula = ~date, # detection model
                           gammaformula = ~1,  # colonization
                           epsilonformula = ~1, # extinction probability
                           data = analysis.umf))

#' Model summary
dfms.final <- fitList("lam(canopy)p(date)gamma(.)omega(.)-P"  = occ.c.P.pD,
                      "lam(null)p(date)gamma(.)omega(.)-P"  = occ.null.P.pD,
                      "lam(disturbance)p(date)gamma(.)omega(.)-P"  = occ.d.P.pD)
(dms.final <- modSel(dfms.final)) 


#' _____________________________________________________________________________
#' ## Save files
#' 
save(occ.c.P.pD, file = "data/output_data/eato_occ_c_P_pD.Rdata")
save(occ.null.P.pD, file = "data/output_data/eato_occ_null_P_pD.Rdata")
save(occ.d.P.pD, file = "data/output_data/eato_occ_d_P_pD.Rdata")
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
#' ezspin(file = "programs/00_thesis_programs/03b_occupancy_eato.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 
#' *You can run the ezspin() code above to create an html in the "output" folder
#' and any associated figures will get put into the "figures" folder.*