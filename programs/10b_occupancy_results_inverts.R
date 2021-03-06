#' # Invert results - Figures & Tables
#' 
#' Description: Model comparison tables
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


#' **Unmarked models**
#' 
#' HELE
load(file = "data/output_data/hele_occ_d_NB.Rdata") #disturbance
hele.occ.d.NB <- occ.d.NB
load(file = "data/output_data/hele_occ_null_NB.Rdata") #null
hele.occ.null.NB <- occ.null.NB
load(file = "data/output_data/hele_occ_t_NB.Rdata") #liatris
hele.occ.t.NB <- occ.t.NB
load(file = "data/output_data/hele_occ_nb_NB.Rdata")#nonbunchgrass
hele.occ.nb.NB <- occ.nb.NB
load(file = "data/output_data/hele_occ_b_NB.Rdata") #bunchgrass
hele.occ.b.NB <- occ.b.NB
load(file = "data/output_data/hele_occ_l_NB.Rdata")#litter
hele.occ.l.NB <- occ.l.NB
load(file = "data/output_data/hele_occ_c_NB.Rdata") #canopy
hele.occ.c.NB <- occ.c.NB
#' CIPA 
load(file = "data/output_data/cipa_occ_e_NB.Rdata") #elevCV
cipa.occ.e.NB <- occ.e.NB
load(file = "data/output_data/cipa_occ_null_NB.Rdata") #null
cipa.occ.null.NB <- occ.null.NB
load(file = "data/output_data/cipa_occ_l_NB.Rdata")#litter
cipa.occ.l.NB <- occ.l.NB
load(file = "data/output_data/cipa_occ_c_NB.Rdata") #canopy
cipa.occ.c.NB <- occ.c.NB

#' ____________________________________________________________________________
#' ### Unmarked results
#' 
#' **CIPA**
#' 
#' Fit list
dfms.cipa <- fitList("lam(.)p(.)gamma(.)omega(.)-NB"  = cipa.occ.null.NB,
                     "lam(canopy)p(.)gamma(.)omega(.)-NB"  = cipa.occ.c.NB,
                     "lam(litter)p(.)gamma(.)omega(.)-NB"  = cipa.occ.l.NB,
                     "lam(elev.CV)p(.)gamma(.)omega(.)-NB"  = cipa.occ.e.NB
)
(dms <- modSel(dfms.cipa)) 

#'  Model 1 (elevation)
coef(cipa.occ.e.NB, type="psi")
confint(cipa.occ.e.NB, type="psi", level=0.85)
coef(cipa.occ.e.NB, type="det")
confint(cipa.occ.e.NB, type="det", level=0.85)
coef(cipa.occ.e.NB, type="col")
confint(cipa.occ.e.NB, type="col", level=0.85)
coef(cipa.occ.e.NB, type="ext")
confint(cipa.occ.e.NB, type="ext", level=0.85)
cipa.occ.e.NB

#'  Model 2 (canopy)
coef(cipa.occ.c.NB, type="psi")
confint(cipa.occ.c.NB, type="psi", level=0.85)
coef(cipa.occ.c.NB, type="det")
confint(cipa.occ.c.NB, type="det", level=0.85)
coef(cipa.occ.c.NB, type="col")
confint(cipa.occ.c.NB, type="col", level=0.85)
coef(cipa.occ.c.NB, type="ext")
confint(cipa.occ.c.NB, type="ext", level=0.85)
cipa.occ.c.NB

#'  Model 3 (null)
coef(cipa.occ.null.NB, type="psi")
confint(cipa.occ.null.NB, type="psi", level=0.85)
coef(cipa.occ.null.NB, type="det")
confint(cipa.occ.null.NB, type="det", level=0.85)
coef(cipa.occ.null.NB, type="col")
confint(cipa.occ.null.NB, type="col", level=0.85)
coef(cipa.occ.null.NB, type="ext")
confint(cipa.occ.null.NB, type="ext", level=0.85)
cipa.occ.null.NB

#'  Model 4 (litter)
coef(cipa.occ.l.NB, type="psi")
confint(cipa.occ.l.NB, type="psi", level=0.85)
coef(cipa.occ.l.NB, type="det")
confint(cipa.occ.l.NB, type="det", level=0.85)
coef(cipa.occ.l.NB, type="col")
confint(cipa.occ.l.NB, type="col", level=0.85)
coef(cipa.occ.l.NB, type="ext")
confint(cipa.occ.l.NB, type="ext", level=0.85)
cipa.occ.l.NB

#' 
#' **HELE**
#' 
#' Fit list
dfms.hele <- fitList("lam(.)p(.)gamma(.)omega(.)-NB"  = hele.occ.null.NB,
                     "lam(canopy)p(.)gamma(.)omega(.)-NB"  = hele.occ.c.NB,
                     "lam(bunchgrass)p(.)gamma(.)omega(.)-NB"  = hele.occ.b.NB,
                     "lam(nonbunchgrass)p(.)gamma(.)omega(.)-NB"  = hele.occ.nb.NB,
                     "lam(liatris)p(.)gamma(.)omega(.)-NB"  = hele.occ.t.NB,
                     "lam(litter)p(.)gamma(.)omega(.)-NB"  = hele.occ.l.NB,
                     "lam(dy_pre2015)p(.)gamma(.)omega(.)-NB"  = hele.occ.d.NB)
(dms <- modSel(dfms.hele)) 

#'  Model 1 (liatris)
coef(hele.occ.t.NB, type="psi")
confint(hele.occ.t.NB, type="psi", level=0.85)
coef(hele.occ.t.NB, type="det")
confint(hele.occ.t.NB, type="det", level=0.85)
coef(hele.occ.t.NB, type="col")
confint(hele.occ.t.NB, type="col", level=0.85)
coef(hele.occ.t.NB, type="ext")
confint(hele.occ.t.NB, type="ext", level=0.85)
hele.occ.t.NB

#'  Model 2 (null)
coef(hele.occ.null.NB, type="psi")
confint(hele.occ.null.NB, type="psi", level=0.85)
coef(hele.occ.null.NB, type="det")
confint(hele.occ.null.NB, type="det", level=0.85)
coef(hele.occ.null.NB, type="col")
confint(hele.occ.null.NB, type="col", level=0.85)
coef(hele.occ.null.NB, type="ext")
confint(hele.occ.null.NB, type="ext", level=0.85)
hele.occ.null.NB

#'  Model 3 (nonbunchgrass)
coef(hele.occ.nb.NB, type="psi")
confint(hele.occ.nb.NB, type="psi", level=0.85)
coef(hele.occ.nb.NB, type="det")
confint(hele.occ.nb.NB, type="det", level=0.85)
coef(hele.occ.nb.NB, type="col")
confint(hele.occ.nb.NB, type="col", level=0.85)
coef(hele.occ.nb.NB, type="ext")
confint(hele.occ.nb.NB, type="ext", level=0.85)
hele.occ.nb.NB

#'  Model 4 (litter)
coef(hele.occ.l.NB, type="psi")
confint(hele.occ.l.NB, type="psi", level=0.85)
coef(hele.occ.l.NB, type="det")
confint(hele.occ.l.NB, type="det", level=0.85)
coef(hele.occ.l.NB, type="col")
confint(hele.occ.l.NB, type="col", level=0.85)
coef(hele.occ.l.NB, type="ext")
confint(hele.occ.l.NB, type="ext", level=0.85)
hele.occ.l.NB

#'  Model 5 (bunchgrass)
coef(hele.occ.b.NB, type="psi")
confint(hele.occ.b.NB, type="psi", level=0.85)
coef(hele.occ.b.NB, type="det")
confint(hele.occ.b.NB, type="det", level=0.85)
coef(hele.occ.b.NB, type="col")
confint(hele.occ.b.NB, type="col", level=0.85)
coef(hele.occ.b.NB, type="ext")
confint(hele.occ.b.NB, type="ext", level=0.85)
hele.occ.b.NB

#'  Model 6 (canopy)
coef(hele.occ.c.NB, type="psi")
confint(hele.occ.c.NB, type="psi", level=0.85)
coef(hele.occ.c.NB, type="det")
confint(hele.occ.c.NB, type="det", level=0.85)
coef(hele.occ.c.NB, type="col")
confint(hele.occ.c.NB, type="col", level=0.85)
coef(hele.occ.c.NB, type="ext")
confint(hele.occ.c.NB, type="ext", level=0.85)
hele.occ.c.NB

#'  Model 7 (disturbance)
coef(hele.occ.d.NB, type="psi")
confint(hele.occ.d.NB, type="psi", level=0.85)
coef(hele.occ.d.NB, type="det")
confint(hele.occ.d.NB, type="det", level=0.85)
coef(hele.occ.d.NB, type="col")
confint(hele.occ.d.NB, type="col", level=0.85)
coef(hele.occ.d.NB, type="ext")
confint(hele.occ.d.NB, type="ext", level=0.85)
hele.occ.d.NB



#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/10b_occupancy_results_inverts.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 
#' *You can run the ezspin() code above to create an html in the "output" folder
#' and any associated figures will get put into the "figures" folder.*