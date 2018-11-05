#' # Invert results - Figures & Tables
#' 
#' Description: Model comparison tables and figures
#' 
#' ### Preamble
#' 
#' Load libraries
#+ libraries, message = F, warning = F
library(knitr) # documentation-related
library(ezknitr) # documentation-related
library(devtools) # documentation-related

library(unmarked) # analysis-related
library(ggplot2)
library(ggthemes)
library(gridExtra)

#' Clear environment and set seed
#' 
#' *Note: for reproducibility, it is important to include these. Clearing the
#' environment ensures that you have specified all pertinent files that need
#' to be loaded, and setting the seed ensures that your analysis is 
#' repeatable*
remove(list = ls())
set.seed(2583722)

#' _____________________________________________________________________________
#' ## Load Data
#' 
#' **JAGS Models**
#' 
#' CIPA model
load("data/output_data/cipa_JAGS_out_final.Rdata")

#' HELE model
load("data/output_data/hele_JAGS_out_20181102.Rdata")

#' **Unmarked models**
#' 
#' HELE
load(file = "data/output_data/hele_abnd_d_NB.Rdata") #disturbance
hele.abnd.d.NB <- abnd.d.NB
load(file = "data/output_data/hele_abnd_null_NB.Rdata") #null
hele.abnd.null.NB <- abnd.null.NB
load(file = "data/output_data/hele_abnd_t_NB.Rdata") #liatris
hele.abnd.t.NB <- abnd.t.NB
load(file = "data/output_data/hele_abnd_nb_NB.Rdata")#nonbunchgrass
hele.abnd.nb.NB <- abnd.nb.NB
load(file = "data/output_data/hele_abnd_b_NB.Rdata") #bunchgrass
hele.abnd.b.NB <- abnd.b.NB
load(file = "data/output_data/hele_abnd_l_NB.Rdata")#litter
hele.abnd.l.NB <- abnd.l.NB
load(file = "data/output_data/hele_abnd_c_NB.Rdata") #canopy
hele.abnd.c.NB <- abnd.c.NB
#' CIPA 
load(file = "data/output_data/cipa_abnd_e_NB.Rdata") #elevCV
cipa.abnd.e.NB <- abnd.e.NB
load(file = "data/output_data/cipa_abnd_null_NB.Rdata") #null
cipa.abnd.null.NB <- abnd.null.NB
load(file = "data/output_data/cipa_abnd_l_NB.Rdata")#litter
cipa.abnd.l.NB <- abnd.l.NB
load(file = "data/output_data/cipa_abnd_c_NB.Rdata") #canopy
cipa.abnd.c.NB <- abnd.c.NB

#' ____________________________________________________________________________
#' ### Unmarked results
#' 
#' **CIPA**
#' 
#' Fit list
dfms.cipa <- fitList("lam(.)p(.)gamma(.)omega(.)-NB"  = cipa.abnd.null.NB,
                     "lam(canopy)p(.)gamma(.)omega(.)-NB"  = cipa.abnd.c.NB,
                     "lam(litter)p(.)gamma(.)omega(.)-NB"  = cipa.abnd.l.NB,
                     "lam(elev.CV)p(.)gamma(.)omega(.)-NB"  = cipa.abnd.e.NB
)
(dms <- modSel(dfms.cipa)) 

#'  Model 1 (elevation)
coef(cipa.abnd.e.NB, type="lambda")
confint(cipa.abnd.e.NB, type="lambda", level=0.85)
coef(cipa.abnd.e.NB, type="det")
confint(cipa.abnd.e.NB, type="det", level=0.85)
coef(cipa.abnd.e.NB, type="gamma")
confint(cipa.abnd.e.NB, type="gamma", level=0.85)
coef(cipa.abnd.e.NB, type="omega")
confint(cipa.abnd.e.NB, type="omega", level=0.85)
cipa.abnd.e.NB

#'  Model 2 (canopy)
coef(cipa.abnd.c.NB, type="lambda")
confint(cipa.abnd.c.NB, type="lambda", level=0.85)
coef(cipa.abnd.c.NB, type="det")
confint(cipa.abnd.c.NB, type="det", level=0.85)
coef(cipa.abnd.c.NB, type="gamma")
confint(cipa.abnd.c.NB, type="gamma", level=0.85)
coef(cipa.abnd.c.NB, type="omega")
confint(cipa.abnd.c.NB, type="omega", level=0.85)
cipa.abnd.c.NB

#'  Model 3 (null)
coef(cipa.abnd.null.NB, type="lambda")
confint(cipa.abnd.null.NB, type="lambda", level=0.85)
coef(cipa.abnd.null.NB, type="det")
confint(cipa.abnd.null.NB, type="det", level=0.85)
coef(cipa.abnd.null.NB, type="gamma")
confint(cipa.abnd.null.NB, type="gamma", level=0.85)
coef(cipa.abnd.null.NB, type="omega")
confint(cipa.abnd.null.NB, type="omega", level=0.85)
cipa.abnd.null.NB

#'  Model 4 (litter)
coef(cipa.abnd.l.NB, type="lambda")
confint(cipa.abnd.l.NB, type="lambda", level=0.85)
coef(cipa.abnd.l.NB, type="det")
confint(cipa.abnd.l.NB, type="det", level=0.85)
coef(cipa.abnd.l.NB, type="gamma")
confint(cipa.abnd.l.NB, type="gamma", level=0.85)
coef(cipa.abnd.l.NB, type="omega")
confint(cipa.abnd.l.NB, type="omega", level=0.85)
cipa.abnd.l.NB

#' 
#' **HELE**
#' 
#' Fit list
dfms.hele <- fitList("lam(.)p(.)gamma(.)omega(.)-NB"  = hele.abnd.null.NB,
                "lam(canopy)p(.)gamma(.)omega(.)-NB"  = hele.abnd.c.NB,
                "lam(bunchgrass)p(.)gamma(.)omega(.)-NB"  = hele.abnd.b.NB,
                "lam(nonbunchgrass)p(.)gamma(.)omega(.)-NB"  = hele.abnd.nb.NB,
                "lam(liatris)p(.)gamma(.)omega(.)-NB"  = hele.abnd.t.NB,
                "lam(litter)p(.)gamma(.)omega(.)-NB"  = hele.abnd.l.NB,
                "lam(dy_pre2015)p(.)gamma(.)omega(.)-NB"  = hele.abnd.d.NB)
(dms <- modSel(dfms.hele)) 

#'  Model 1 (liatris)
coef(hele.abnd.t.NB, type="lambda")
confint(hele.abnd.t.NB, type="lambda", level=0.85)
coef(hele.abnd.t.NB, type="det")
confint(hele.abnd.t.NB, type="det", level=0.85)
coef(hele.abnd.t.NB, type="gamma")
confint(hele.abnd.t.NB, type="gamma", level=0.85)
coef(hele.abnd.t.NB, type="omega")
confint(hele.abnd.t.NB, type="omega", level=0.85)
hele.abnd.t.NB

#'  Model 2 (distrubance)
coef(hele.abnd.d.NB, type="lambda")
confint(hele.abnd.d.NB, type="lambda", level=0.85)
coef(hele.abnd.d.NB, type="det")
confint(hele.abnd.d.NB, type="det", level=0.85)
coef(hele.abnd.d.NB, type="gamma")
confint(hele.abnd.d.NB, type="gamma", level=0.85)
coef(hele.abnd.d.NB, type="omega")
confint(hele.abnd.d.NB, type="omega", level=0.85)
hele.abnd.d.NB

#'  Model 3 (null)
coef(hele.abnd.null.NB, type="lambda")
confint(hele.abnd.null.NB, type="lambda", level=0.85)
coef(hele.abnd.null.NB, type="det")
confint(hele.abnd.null.NB, type="det", level=0.85)
coef(hele.abnd.null.NB, type="gamma")
confint(hele.abnd.null.NB, type="gamma", level=0.85)
coef(hele.abnd.null.NB, type="omega")
confint(hele.abnd.null.NB, type="omega", level=0.85)
hele.abnd.null.NB

#'  Model 4 (nonbunchgrass)
coef(hele.abnd.nb.NB, type="lambda")
confint(hele.abnd.nb.NB, type="lambda", level=0.85)
coef(hele.abnd.nb.NB, type="det")
confint(hele.abnd.nb.NB, type="det", level=0.85)
coef(hele.abnd.nb.NB, type="gamma")
confint(hele.abnd.nb.NB, type="gamma", level=0.85)
coef(hele.abnd.nb.NB, type="omega")
confint(hele.abnd.nb.NB, type="omega", level=0.85)
hele.abnd.nb.NB

#'  Model 5 (bunchgrass)
coef(hele.abnd.b.NB, type="lambda")
confint(hele.abnd.b.NB, type="lambda", level=0.85)
coef(hele.abnd.b.NB, type="det")
confint(hele.abnd.b.NB, type="det", level=0.85)
coef(hele.abnd.b.NB, type="gamma")
confint(hele.abnd.b.NB, type="gamma", level=0.85)
coef(hele.abnd.b.NB, type="omega")
confint(hele.abnd.b.NB, type="omega", level=0.85)
hele.abnd.b.NB

#'  Model 6 (litter)
coef(hele.abnd.l.NB, type="lambda")
confint(hele.abnd.l.NB, type="lambda", level=0.85)
coef(hele.abnd.l.NB, type="det")
confint(hele.abnd.l.NB, type="det", level=0.85)
coef(hele.abnd.l.NB, type="gamma")
confint(hele.abnd.l.NB, type="gamma", level=0.85)
coef(hele.abnd.l.NB, type="omega")
confint(hele.abnd.l.NB, type="omega", level=0.85)
hele.abnd.l.NB

#'  Model 7 (canopy)
coef(hele.abnd.c.NB, type="lambda")
confint(hele.abnd.c.NB, type="lambda", level=0.85)
coef(hele.abnd.c.NB, type="det")
confint(hele.abnd.c.NB, type="det", level=0.85)
coef(hele.abnd.c.NB, type="gamma")
confint(hele.abnd.c.NB, type="gamma", level=0.85)
coef(hele.abnd.c.NB, type="omega")
confint(hele.abnd.c.NB, type="omega", level=0.85)
hele.abnd.c.NB

#' ____________________________________________________________________________
#' ### JAGS results
#' 
#' CIPA model results
cipa_JAGS$summary

#' CIPA CRIs for model effect sizes
#' 
#' Abundance intercept
b0.abnd <- cipa_JAGS$sims.list$b0.abund
quantile(b0.abnd, probs = c(0.075, 0.925))

#' Abundance covariate: elevation
b1.abnd <- cipa_JAGS$sims.list$b1.abund
quantile(b1.abnd, probs = c(0.075, 0.925))

#' Abundance covariate: canopy
b2.abnd <- cipa_JAGS$sims.list$b2.abund
quantile(b2.abnd, probs = c(0.075, 0.925))

#' Occupancy covariate: elevation
b1.psi <- cipa_JAGS$sims.list$b1.psi
quantile(b1.psi, probs = c(0.075, 0.925))

#' Occupancy covariate: canopy
b2.psi <- cipa_JAGS$sims.list$b2.psi
quantile(b2.psi, probs = c(0.075, 0.925))

#' ### Violin Plot for CIPA
#' 
#+ cipa_violin
b0.abnd <- as.data.frame(b0.abnd)
b0.abnd$parameter <- "b0.abnd"

b1.abnd <- as.data.frame(b1.abnd)
b1.abnd$parameter <- "b1.abnd"
b1.abnd$covariate <- "Elevation"
b1.abnd$model <- "Abundance"

b2.abnd <- as.data.frame(b2.abnd)
b2.abnd$parameter <- "b2.abnd"
b2.abnd$covariate <- "Canopy"
b2.abnd$model <- "Abundance"

b1.psi <- as.data.frame(b1.psi)
b1.psi$parameter <- "b1.psi"
b1.psi$covariate <- "Elevation"
b1.psi$model <- "Occupancy"

b2.psi <- as.data.frame(b2.psi)
b2.psi$parameter <- "b2.psi"
b2.psi$covariate <- "Canopy"
b2.psi$model <- "Occupancy"

colnames(b1.abnd) <-
  colnames(b2.abnd) <- 
  colnames(b1.psi) <-
  colnames(b2.psi) <-
  c("Posteriors", "Parameter", "Covariate", "Model")
posteriors.cipa <- rbind(b1.abnd, b2.abnd, b1.psi, b2.psi)
  
#' Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- median(x)
  ymin <- quantile(x, probs = .075)
  ymax <- quantile(x, probs = .925)
  return(c(y=m,ymin=as.numeric(ymin),ymax=as.numeric(ymax)))
}

#' Violin plots for CIPA
#+ resultsCIPA_violin
ggplot(aes(y = Posteriors, x = Covariate), data = posteriors.cipa) + 
  geom_violin() +
  facet_wrap(facets = "Model") +
  stat_summary(fun.data=data_summary, 
               geom="pointrange", color="red") +
  geom_hline(yintercept = 0)
# Commented out line below so that the plot is only created 
#   when the file viewer is set up to appropriate size
#ggsave(filename = "output/CIPA_violin_thesis.png",device = "png")

#' HELE CRIs for model effect sizes
#' 
#' Abundance intercept
b0.abnd <- hele_JAGS$sims.list$b0.abund
quantile(b0.abnd, probs = c(0.075, 0.925))

#' Abundance covariate: elevation
b1.abnd <- hele_JAGS$sims.list$b1.abund
quantile(b1.abnd, probs = c(0.075, 0.925))

#' Abundance covariate: canopy
b2.abnd <- hele_JAGS$sims.list$b2.abund
quantile(b2.abnd, probs = c(0.075, 0.925))

#' Occupancy covariate: liatris
b1.psi <- hele_JAGS$sims.list$b1.psi
quantile(b1.psi, probs = c(0.075, 0.925))


#' ### Violin Plot for HELE
#' 
#+ hele_violin
b0.abnd <- as.data.frame(b0.abnd)
b0.abnd$parameter <- "b0.abnd"

b1.abnd <- as.data.frame(b1.abnd)
b1.abnd$parameter <- "b1.abnd"
b1.abnd$covariate <- "Disturbance"
b1.abnd$model <- "Abundance"

b2.abnd <- as.data.frame(b2.abnd)
b2.abnd$parameter <- "b2.abnd"
b2.abnd$covariate <- "Liatris"
b2.abnd$model <- "Abundance"

b1.psi <- as.data.frame(b1.psi)
b1.psi$parameter <- "b1.psi"
b1.psi$covariate <- "Liatris"
b1.psi$model <- "Occupancy"

b2.psi <- as.data.frame(b2.psi)
b2.psi$parameter <- "b2.psi"
b2.psi$covariate <- "Liatris"
b2.psi$model <- "Occupancy"

colnames(b1.abnd) <-
  colnames(b2.abnd) <- 
  colnames(b1.psi) <-
  colnames(b2.psi) <-
  c("Posteriors", "Parameter", "Covariate", "Model")
posteriors.hele <- rbind(b1.abnd, b2.abnd, b1.psi)

#' Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- median(x)
  ymin <- quantile(x, probs = .075)
  ymax <- quantile(x, probs = .925)
  return(c(y=m,ymin=as.numeric(ymin),ymax=as.numeric(ymax)))
}

#' Violin plot for HELE
#+ resultsHELE_violin
ggplot(aes(y = Posteriors, x = Covariate), data = posteriors.hele) + 
  geom_violin() +
  facet_grid(facets = .~Model, scales = "free_x", space = "free_x") +
  stat_summary(fun.data=data_summary, 
               geom="pointrange", color="red") +
  geom_hline(yintercept = 0)
# Commented out line below so that the plot is only created 
#   when the file viewer is set up to appropriate size
#ggsave(filename = "output/HELE_violin_thesis.png",device = "png")



#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/10_abundance_results_inverts.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 
#' *You can run the ezspin() code above to create an html in the "output" folder
#' and any associated figures will get put into the "figures" folder.*