#' # Processing Plot-Level Data
#' 

#' 
#' ### Preamble
#' 
#' Load libraries
#+ libraries, message = F, warning = F
library(knitr) # documentation-related
library(ezknitr) # documentation-related
library(devtools) # documentation-related

# analysis-related

#' Clear environment and set seed
#' 
remove(list = ls())
set.seed(2583722)

#' _____________________________________________________________________________
#' ## Load Data
#' 
#' Plot-level data
plot.data <- read.csv("data/processed_data/plot_level_data.csv")

#' _____________________________________________________________________________
#' ## Run correlation analysis for plot-level habitat covariates
#' 
#' New data frame with just the habitat variables
hab.covs <- plot.data[,c("pct_canopy", "num_woody_stems", "pct_bunchgrass",
                         "num_mounds", "pct_bluestem", "num_blowouts", 
                         "num_liatris_stems", "num_open_oaks", "num_milkweed_stems",
                         "litter_depth", "pct_nonbunchgrass", "tot_grass", "elev_CV.z")]
# Full correlation table (not-run)
#round(cor(hab.covs),2)

#' **Correlation coefficients and associated p-values**
#' 
#' Column 1; percent canopy
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$num_woody_stems)$estimate,3),
        round(cor.test(hab.covs$pct_canopy, hab.covs$num_woody_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$num_open_oaks)$estimate,3),
        round(cor.test(hab.covs$pct_canopy, hab.covs$num_open_oaks)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$tot_grass)$estimate,3),
        round(cor.test(hab.covs$pct_canopy, hab.covs$tot_grass)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$pct_bluestem)$estimate,3),
        round(cor.test(hab.covs$pct_canopy, hab.covs$pct_bluestem)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$pct_bunchgrass)$estimate,3),
        round(cor.test(hab.covs$pct_canopy, hab.covs$pct_bunchgrass)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$pct_nonbunchgrass)$estimate,2),
        round(cor.test(hab.covs$pct_canopy, hab.covs$pct_nonbunchgrass)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$num_liatris_stems)$estimate,3),
        round(cor.test(hab.covs$pct_canopy, hab.covs$num_liatris_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$num_milkweed_stems)$estimate,3),
        round(cor.test(hab.covs$pct_canopy, hab.covs$num_milkweed_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$litter_depth)$estimate,3),
        round(cor.test(hab.covs$pct_canopy, hab.covs$litter_depth)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$num_mounds)$estimate,3),
        round(cor.test(hab.covs$pct_canopy, hab.covs$num_mounds)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$num_blowouts)$estimate,3),
        round(cor.test(hab.covs$pct_canopy, hab.covs$num_blowouts)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_canopy, hab.covs$elev_CV.z)$estimate,3),
        round(cor.test(hab.covs$pct_canopy, hab.covs$elev_CV.z)$p.value,3)))

#' Column 2; number of woody stems
print(c(round(cor.test(hab.covs$num_woody_stems, hab.covs$num_open_oaks)$estimate,3),
        round(cor.test(hab.covs$num_woody_stems, hab.covs$num_open_oaks)$p.value,3)))
print(c(round(cor.test(hab.covs$num_woody_stems, hab.covs$tot_grass)$estimate,3),
        round(cor.test(hab.covs$num_woody_stems, hab.covs$tot_grass)$p.value,3)))
print(c(round(cor.test(hab.covs$num_woody_stems, hab.covs$pct_bluestem)$estimate,3),
        round(cor.test(hab.covs$num_woody_stems, hab.covs$pct_bluestem)$p.value,3)))
print(c(round(cor.test(hab.covs$num_woody_stems, hab.covs$pct_bunchgrass)$estimate,3),
        round(cor.test(hab.covs$num_woody_stems, hab.covs$pct_bunchgrass)$p.value,3)))
print(c(round(cor.test(hab.covs$num_woody_stems, hab.covs$pct_nonbunchgrass)$estimate,2),
        round(cor.test(hab.covs$num_woody_stems, hab.covs$pct_nonbunchgrass)$p.value,3)))
print(c(round(cor.test(hab.covs$num_woody_stems, hab.covs$num_liatris_stems)$estimate,3),
        round(cor.test(hab.covs$num_woody_stems, hab.covs$num_liatris_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$num_woody_stems, hab.covs$num_milkweed_stems)$estimate,3),
        round(cor.test(hab.covs$num_woody_stems, hab.covs$num_milkweed_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$num_woody_stems, hab.covs$litter_depth)$estimate,3),
        round(cor.test(hab.covs$num_woody_stems, hab.covs$litter_depth)$p.value,3)))
print(c(round(cor.test(hab.covs$num_woody_stems, hab.covs$num_mounds)$estimate,3),
        round(cor.test(hab.covs$num_woody_stems, hab.covs$num_mounds)$p.value,3)))
print(c(round(cor.test(hab.covs$num_woody_stems, hab.covs$num_blowouts)$estimate,3),
        round(cor.test(hab.covs$num_woody_stems, hab.covs$num_blowouts)$p.value,3)))
print(c(round(cor.test(hab.covs$num_woody_stems, hab.covs$elev_CV.z)$estimate,3),
        round(cor.test(hab.covs$num_woody_stems, hab.covs$elev_CV.z)$p.value,3)))

#' Column 3; number of open oaks
print(c(round(cor.test(hab.covs$num_open_oaks, hab.covs$tot_grass)$estimate,3),
        round(cor.test(hab.covs$num_open_oaks, hab.covs$tot_grass)$p.value,3)))
print(c(round(cor.test(hab.covs$num_open_oaks, hab.covs$pct_bluestem)$estimate,3),
        round(cor.test(hab.covs$num_open_oaks, hab.covs$pct_bluestem)$p.value,3)))
print(c(round(cor.test(hab.covs$num_open_oaks, hab.covs$pct_bunchgrass)$estimate,3),
        round(cor.test(hab.covs$num_open_oaks, hab.covs$pct_bunchgrass)$p.value,3)))
print(c(round(cor.test(hab.covs$num_open_oaks, hab.covs$pct_nonbunchgrass)$estimate,2),
        round(cor.test(hab.covs$num_open_oaks, hab.covs$pct_nonbunchgrass)$p.value,3)))
print(c(round(cor.test(hab.covs$num_open_oaks, hab.covs$num_liatris_stems)$estimate,3),
        round(cor.test(hab.covs$num_open_oaks, hab.covs$num_liatris_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$num_open_oaks, hab.covs$num_milkweed_stems)$estimate,3),
        round(cor.test(hab.covs$num_open_oaks, hab.covs$num_milkweed_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$num_open_oaks, hab.covs$litter_depth)$estimate,3),
        round(cor.test(hab.covs$num_open_oaks, hab.covs$litter_depth)$p.value,3)))
print(c(round(cor.test(hab.covs$num_open_oaks, hab.covs$num_mounds)$estimate,3),
        round(cor.test(hab.covs$num_open_oaks, hab.covs$num_mounds)$p.value,3)))
print(c(round(cor.test(hab.covs$num_open_oaks, hab.covs$num_blowouts)$estimate,3),
        round(cor.test(hab.covs$num_open_oaks, hab.covs$num_blowouts)$p.value,3)))
print(c(round(cor.test(hab.covs$num_open_oaks, hab.covs$elev_CV.z)$estimate,3),
        round(cor.test(hab.covs$num_open_oaks, hab.covs$elev_CV.z)$p.value,3)))

#' Column 4: total grass

print(c(round(cor.test(hab.covs$tot_grass, hab.covs$pct_bluestem)$estimate,3),
        round(cor.test(hab.covs$tot_grass, hab.covs$pct_bluestem)$p.value,3)))
print(c(round(cor.test(hab.covs$tot_grass, hab.covs$pct_bunchgrass)$estimate,3),
        round(cor.test(hab.covs$tot_grass, hab.covs$pct_bunchgrass)$p.value,3)))
print(c(round(cor.test(hab.covs$tot_grass, hab.covs$pct_nonbunchgrass)$estimate,2),
        round(cor.test(hab.covs$tot_grass, hab.covs$pct_nonbunchgrass)$p.value,3)))
print(c(round(cor.test(hab.covs$tot_grass, hab.covs$num_liatris_stems)$estimate,3),
        round(cor.test(hab.covs$tot_grass, hab.covs$num_liatris_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$tot_grass, hab.covs$num_milkweed_stems)$estimate,3),
        round(cor.test(hab.covs$tot_grass, hab.covs$num_milkweed_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$tot_grass, hab.covs$litter_depth)$estimate,3),
        round(cor.test(hab.covs$tot_grass, hab.covs$litter_depth)$p.value,3)))
print(c(round(cor.test(hab.covs$tot_grass, hab.covs$num_mounds)$estimate,3),
        round(cor.test(hab.covs$tot_grass, hab.covs$num_mounds)$p.value,3)))
print(c(round(cor.test(hab.covs$tot_grass, hab.covs$num_blowouts)$estimate,3),
        round(cor.test(hab.covs$tot_grass, hab.covs$num_blowouts)$p.value,3)))
print(c(round(cor.test(hab.covs$tot_grass, hab.covs$elev_CV.z)$estimate,3),
        round(cor.test(hab.covs$tot_grass, hab.covs$elev_CV.z)$p.value,3)))

#' Column 5; percent bluestem

print(c(round(cor.test(hab.covs$pct_bluestem, hab.covs$pct_bunchgrass)$estimate,3),
        round(cor.test(hab.covs$pct_bluestem, hab.covs$pct_bunchgrass)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bluestem, hab.covs$pct_nonbunchgrass)$estimate,2),
        round(cor.test(hab.covs$pct_bluestem, hab.covs$pct_nonbunchgrass)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bluestem, hab.covs$num_liatris_stems)$estimate,3),
        round(cor.test(hab.covs$pct_bluestem, hab.covs$num_liatris_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bluestem, hab.covs$num_milkweed_stems)$estimate,3),
        round(cor.test(hab.covs$pct_bluestem, hab.covs$num_milkweed_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bluestem, hab.covs$litter_depth)$estimate,3),
        round(cor.test(hab.covs$pct_bluestem, hab.covs$litter_depth)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bluestem, hab.covs$num_mounds)$estimate,3),
        round(cor.test(hab.covs$pct_bluestem, hab.covs$num_mounds)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bluestem, hab.covs$num_blowouts)$estimate,3),
        round(cor.test(hab.covs$pct_bluestem, hab.covs$num_blowouts)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bluestem, hab.covs$elev_CV.z)$estimate,3),
        round(cor.test(hab.covs$pct_bluestem, hab.covs$elev_CV.z)$p.value,3)))

#' Column 6; percent bunch grass
print(c(round(cor.test(hab.covs$pct_bunchgrass, hab.covs$pct_nonbunchgrass)$estimate,2),
        round(cor.test(hab.covs$pct_bunchgrass, hab.covs$pct_nonbunchgrass)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bunchgrass, hab.covs$num_liatris_stems)$estimate,3),
        round(cor.test(hab.covs$pct_bunchgrass, hab.covs$num_liatris_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bunchgrass, hab.covs$num_milkweed_stems)$estimate,3),
        round(cor.test(hab.covs$pct_bunchgrass, hab.covs$num_milkweed_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bunchgrass, hab.covs$litter_depth)$estimate,3),
        round(cor.test(hab.covs$pct_bunchgrass, hab.covs$litter_depth)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bunchgrass, hab.covs$num_mounds)$estimate,3),
        round(cor.test(hab.covs$pct_bunchgrass, hab.covs$num_mounds)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bunchgrass, hab.covs$num_blowouts)$estimate,3),
        round(cor.test(hab.covs$pct_bunchgrass, hab.covs$num_blowouts)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_bunchgrass, hab.covs$elev_CV.z)$estimate,3),
        round(cor.test(hab.covs$pct_bunchgrass, hab.covs$elev_CV.z)$p.value,3)))

#' Column 7; percent non-bunchgrass
print(c(round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$num_liatris_stems)$estimate,3),
        round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$num_liatris_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$num_milkweed_stems)$estimate,3),
        round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$num_milkweed_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$litter_depth)$estimate,3),
        round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$litter_depth)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$num_mounds)$estimate,3),
        round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$num_mounds)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$num_blowouts)$estimate,3),
        round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$num_blowouts)$p.value,3)))
print(c(round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$elev_CV.z)$estimate,3),
        round(cor.test(hab.covs$pct_nonbunchgrass, hab.covs$elev_CV.z)$p.value,3)))

#' Column 8; number of liatris stems
print(c(round(cor.test(hab.covs$num_liatris_stems, hab.covs$num_milkweed_stems)$estimate,3),
        round(cor.test(hab.covs$num_liatris_stems, hab.covs$num_milkweed_stems)$p.value,3)))
print(c(round(cor.test(hab.covs$num_liatris_stems, hab.covs$litter_depth)$estimate,3),
        round(cor.test(hab.covs$num_liatris_stems, hab.covs$litter_depth)$p.value,3)))
print(c(round(cor.test(hab.covs$num_liatris_stems, hab.covs$num_mounds)$estimate,3),
        round(cor.test(hab.covs$num_liatris_stems, hab.covs$num_mounds)$p.value,3)))
print(c(round(cor.test(hab.covs$num_liatris_stems, hab.covs$num_blowouts)$estimate,3),
        round(cor.test(hab.covs$num_liatris_stems, hab.covs$num_blowouts)$p.value,3)))
print(c(round(cor.test(hab.covs$num_liatris_stems, hab.covs$elev_CV.z)$estimate,3),
        round(cor.test(hab.covs$num_liatris_stems, hab.covs$elev_CV.z)$p.value,3)))

#' Column 9; number of milkweed stems
print(c(round(cor.test(hab.covs$num_milkweed_stems, hab.covs$litter_depth)$estimate,3),
        round(cor.test(hab.covs$num_milkweed_stems, hab.covs$litter_depth)$p.value,3)))
print(c(round(cor.test(hab.covs$num_milkweed_stems, hab.covs$num_mounds)$estimate,3),
        round(cor.test(hab.covs$num_milkweed_stems, hab.covs$num_mounds)$p.value,3)))
print(c(round(cor.test(hab.covs$num_milkweed_stems, hab.covs$num_blowouts)$estimate,3),
        round(cor.test(hab.covs$num_milkweed_stems, hab.covs$num_blowouts)$p.value,3)))
print(c(round(cor.test(hab.covs$num_milkweed_stems, hab.covs$elev_CV.z)$estimate,3),
        round(cor.test(hab.covs$num_milkweed_stems, hab.covs$elev_CV.z)$p.value,3)))

#' Column 10; litter depth
print(c(round(cor.test(hab.covs$litter_depth, hab.covs$num_mounds)$estimate,3),
        round(cor.test(hab.covs$litter_depth, hab.covs$num_mounds)$p.value,3)))
print(c(round(cor.test(hab.covs$litter_depth, hab.covs$num_blowouts)$estimate,3),
        round(cor.test(hab.covs$litter_depth, hab.covs$num_blowouts)$p.value,3)))
print(c(round(cor.test(hab.covs$litter_depth, hab.covs$elev_CV.z)$estimate,3),
        round(cor.test(hab.covs$litter_depth, hab.covs$elev_CV.z)$p.value,3)))

#' Column 11: number of gopher mounds
print(c(round(cor.test(hab.covs$num_mounds, hab.covs$num_blowouts)$estimate,3),
        round(cor.test(hab.covs$num_mounds, hab.covs$num_blowouts)$p.value,3)))
print(c(round(cor.test(hab.covs$num_mounds, hab.covs$elev_CV.z)$estimate,3),
        round(cor.test(hab.covs$num_mounds, hab.covs$elev_CV.z)$p.value,3)))


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/08_tables_thesis.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)