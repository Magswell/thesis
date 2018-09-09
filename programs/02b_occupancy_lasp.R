#' # Model comparison for occupancy: Lark Sparrow
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

# Convert to binary
lasp.occ <- as.matrix((lasp.y > 0) + 0)

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
  unmarkedMultFrame(y=lasp.occ,
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

#' ____________________________________________________________________________
#' ## Final models for summary table
#' 
#' Null model
(occ.null.NB.pDT <- colext(psiformula = ~1, # occupancy
                            pformula = ~date + time, # detection model
                            gammaformula = ~1,  # colonization
                            epsilonformula = ~1, # extinction probability
                            data = analysis.umf))

#' Canopy cover model for occupancy
(occ.c.NB.pDT <- colext(psiformula = ~canopy, # occupancy
                         pformula = ~date + time, # detection model
                         gammaformula = ~1,  # colonization
                         epsilonformula = ~1, # extinction probability
                         data = analysis.umf))

#' Numwood model for occupancy
(occ.st.NB.pDT <- colext(psiformula = ~numwood, # occupancy
                          pformula = ~date + time, # detection model
                          gammaformula = ~1,  # colonization
                          epsilonformula = ~1, # extinction probability
                          data = analysis.umf))

#' Recent disturbance model for occupancy
(occ.d.NB.pDT <- colext(psiformula = ~DY_pre2015, # occupancy
                         pformula = ~date + time, # detection model
                         gammaformula = ~1,  # colonization
                         epsilonformula = ~1, # extinction probability
                         data = analysis.umf))
#' litter model for occupancy
(occ.l.NB.pDT <- colext(psiformula = ~litter, # occupancy
                             pformula = ~date + time, # detection model
                             gammaformula = ~1,  # colonization
                             epsilonformula = ~1, # extinction probability
                             data = analysis.umf))

#' Fit List
dfms.final <- fitList(
  "lam(canopy)p(date+time)gamma(.)epsilon(.)-NB"  = occ.c.NB.pDT,
  "lam(numwood)p(date+time)gamma(.)epsilon(.)-NB"  = occ.st.NB.pDT,
  "lam(dy_pre2015)p(date+time)gamma(.)epsilon(.)-NB"  = occ.d.NB.pDT,
  "lam(null)p(date+time)gamma(.)epsilon(.)-NB"  = occ.null.NB.pDT,
  "lam(litter)p(date+time)gamma(.)epsilon(.)-NB"  = occ.l.NB.pDT)

#'
#'
#'
# Rank them by AIC
# modSel is a way to model selection results
(dms <- modSel(dfms.final)) 

#' _____________________________________________________________________________
#' ## Save files for results table
#' 
save(occ.c.NB.pDT, file = "data/output_data/lasp_occ_c_NB_pDT.Rdata")
save(occ.st.NB.pDT, file = "data/output_data/lasp_occ_st_NB_pDT.Rdata")
save(occ.d.NB.pDT, file = "data/output_data/lasp_occ_d_NB_pDT.Rdata")
save(occ.null.NB.pDT, file = "data/output_data/lasp_occ_null_NB_pDT.Rdata")
save(occ.l.NB.pDT, file = "data/output_data/lasp_occ_l_NB_pDT.Rdata")
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