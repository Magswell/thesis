#' # Bird results - Figures & Tables
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
#' Plot level means
load(file = "data/output_data/plot_level_meanSD.Rdata")

#' 
#' EATO models to compare
load(file = "data/output_data/eato_abnd_c_P_pD.Rdata")
eato.abnd.c.P.pD <- abnd.c.P.pD
load(file = "data/output_data/eato_abnd_null_P_pD.Rdata")
eato.abnd.null.P.pD <- abnd.null.P.pD
load(file = "data/output_data/eato_abnd_d_P_pD.Rdata")
eato.abnd.d.P.pD <- abnd.d.P.pD
load(file = "data/output_data/eato_date_meanSD.Rdata")

#' LASP models to compare
load(file = "data/output_data/lasp_abnd_c_NB_pDT.Rdata")
lasp.abnd.c.NB.pDT <- abnd.c.NB.pDT
load(file = "data/output_data/lasp_abnd_st_NB_pDT.Rdata")
lasp.abnd.st.NB.pDT <- abnd.st.NB.pDT
load(file = "data/output_data/lasp_abnd_d_NB_pDT.Rdata")
lasp.abnd.d.NB.pDT <- abnd.d.NB.pDT
load(file = "data/output_data/lasp_abnd_null_NB_pDT.Rdata")
lasp.abnd.null.NB.pDT <- abnd.null.NB.pDT
load(file = "data/output_data/lasp_abnd_l_NB_pDT.Rdata")
lasp.abnd.l.NB.pDT <- abnd.l.NB.pDT
#' SD/Mean of date
load(file = "data/output_data/lasp_dateTime_meanSD.Rdata")


#' _____________________________________________________________________________
#' ## Fit list EATO
dfms.eato <- fitList(
  "lam(canopy)p(date)gamma(.)omega(.)-P"  = eato.abnd.c.P.pD,
  "lam(null)p(date)gamma(.)omega(.)-P"  = eato.abnd.null.P.pD,
  "lam(disturbance)p(date)gamma(.)omega(.)-P"  = eato.abnd.d.P.pD)

(dms.eato <- modSel(dfms.eato))

#' Transform parameters
#' 
#'  Model 1
coef(eato.abnd.c.P.pD, type="lambda")
confint(eato.abnd.c.P.pD, type="lambda", level=0.85)
coef(eato.abnd.c.P.pD, type="det")
confint(eato.abnd.c.P.pD, type="det", level=0.85)
coef(eato.abnd.c.P.pD, type="gamma")
confint(eato.abnd.c.P.pD, type="gamma", level=0.85)
coef(eato.abnd.c.P.pD, type="omega")
confint(eato.abnd.c.P.pD, type="omega", level=0.85)

#'  Model 2
coef(eato.abnd.null.P.pD, type="lambda")
confint(eato.abnd.null.P.pD, type="lambda", level=0.85)
coef(eato.abnd.null.P.pD, type="det")
confint(eato.abnd.null.P.pD, type="det", level=0.85)
coef(eato.abnd.null.P.pD, type="gamma")
confint(eato.abnd.null.P.pD, type="gamma", level=0.85)
coef(eato.abnd.null.P.pD, type="omega")
confint(eato.abnd.null.P.pD, type="omega", level=0.85)

#'  Model 3
coef(eato.abnd.d.P.pD, type="lambda")
confint(eato.abnd.d.P.pD, type="lambda", level=0.85)
coef(eato.abnd.d.P.pD, type="det")
confint(eato.abnd.d.P.pD, type="det", level=0.85)
coef(eato.abnd.d.P.pD, type="gamma")
confint(eato.abnd.d.P.pD, type="gamma", level=0.85)
coef(eato.abnd.d.P.pD, type="omega")
confint(eato.abnd.d.P.pD, type="omega", level=0.85)

#' _____________________________________________________________________________
#' ### Plot EATO
#' 
#' New dataset for predictions
nd.eato <- data.frame(canopy = seq(-2, 2.5, length = 50),
                      date = seq(-2,2.5, length = 50))
#' Prediction of lambda and prediction intervals
nd.eato$lam.predict <- predict(eato.abnd.c.P.pD, type="lambda", newdata = nd.eato, appendData = F)
#' Back-transform canopy for plotting
nd.eato$canopyOrig <- nd.eato$canopy*canopy.sd + canopy.mean
#' Create a variable for predicted lambda
nd.eato$lam.predict.mean <- nd.eato$lam.predict$Predicted
#' Calculate 85% Confidence intervals
nd.eato$lam.predict.85LL <- nd.eato$lam.predict$Predicted - 1.44*nd.eato$lam.predict$SE
nd.eato$lam.predict.85UL <- nd.eato$lam.predict$Predicted + 1.44*nd.eato$lam.predict$SE

#' Base graphics version
plot(y = nd.eato$lam.predict$Predicted, x =nd.eato$canopyOrig, ylim=c(0,5), xlim=c(0,100))

#' Prediction of detection probability (p) 
nd.eato$p.predict <- predict(abnd.c.P.pD, type="det", newdata = nd.eato, appendData = F)
#' Create a variable for predicted (p)
nd.eato$p.predict.mean <- nd.eato$p.predict$Predicted
#' Back-transform date for plotting
nd.eato$dateOrig <- nd.eato$date*date.sd.eato + date.mean.eato
#' Calculate 85% Confidence intervals
nd.eato$p.predict.85LL <- nd.eato$p.predict$Predicted - 1.44*nd.eato$p.predict$SE
nd.eato$p.predict.85UL <- nd.eato$p.predict$Predicted + 1.44*nd.eato$p.predict$SE

#' Base graphics version
plot(y = nd.eato$p.predict$Predicted, x =nd.eato$dateOrig, ylim=c(0,1))


#' _____________________________________________________________________________
#' ## EATO Plots
#' 
#' Abundance plot
abnd.eato.plot <- ggplot(data = nd.eato, aes(x=canopyOrig, y=lam.predict.mean)) +
  geom_line()+
  geom_ribbon(aes(ymin=lam.predict.85LL, ymax=lam.predict.85UL), alpha=0.3) +
  ylim(c(0,5)) +
  xlim(c(0,100)) +
  ylab(expression(paste("Mean Abundance ( ",lambda," )"))) +
  xlab("Canopy Cover") +
  annotate("text", label = "A", x=10, y=5) +
  theme_classic()

#' Detection plot
det.eato.plot <- ggplot(data = nd.eato, aes(x=dateOrig, y=p.predict.mean)) +
  geom_line()+
  geom_ribbon(aes(ymin=p.predict.85LL, ymax=p.predict.85UL), alpha=0.3) +
  ylim(c(0,1)) +
  ylab("Probability of Detection (p)") +
  xlab("Days past May 1") +
  annotate("text", label = "B", x=22, y=1) +
  theme_classic()

#' Organize abundance and detection plots side-by-side in one graphic
#+ resultsEATO
grid.arrange(abnd.eato.plot, det.eato.plot, ncol = 2)

#' Figure for DNR report
#+ resultsEATO_DNRreport
ggplot(data = nd.eato, aes(x=canopyOrig, y=lam.predict.mean)) +
  geom_line()+
  geom_ribbon(aes(ymin=lam.predict.85LL, ymax=lam.predict.85UL), alpha=0.3) +
  ylim(c(0,5)) +
  xlim(c(0,100)) +
  ylab(expression("Mean Abundance")) +
  xlab("Canopy Cover") +
  theme_classic()



#' _____________________________________________________________________________
#' ## Lasp results
#' 
#' 
#' 
#' Fit List
dfms.lasp <- fitList(
  "lam(canopy)p(date+time)gamma(.)omega(.)-NB"  = lasp.abnd.c.NB.pDT,
  "lam(numwood)p(date+time)gamma(.)omega(.)-NB"  = lasp.abnd.st.NB.pDT,
  "lam(dy_pre2015)p(date+time)gamma(.)omega(.)-NB"  = lasp.abnd.d.NB.pDT,
  "lam(null)p(date+time)gamma(.)omega(.)-NB"  = lasp.abnd.null.NB.pDT,
  "lam(litter)p(date+time)gamma(.)omega(.)-NB"  = lasp.abnd.l.NB.pDT)
#'
#'
#'
# Rank them by AIC
# modSel is a way to model selection results
(dms <- modSel(dfms.lasp)) 

#' Transform parameters
#' 
#'  Model 1
coef(lasp.abnd.d.NB.pDT, type="lambda")
confint(lasp.abnd.d.NB.pDT, type="lambda", level=0.85)
coef(lasp.abnd.d.NB.pDT, type="det")
confint(lasp.abnd.d.NB.pDT, type="det", level=0.85)
coef(lasp.abnd.d.NB.pDT, type="gamma")
confint(lasp.abnd.d.NB.pDT, type="gamma", level=0.85)
coef(lasp.abnd.d.NB.pDT, type="omega")
confint(lasp.abnd.d.NB.pDT, type="omega", level=0.85)

#'  Model 2
coef(lasp.abnd.c.NB.pDT, type="lambda")
confint(lasp.abnd.c.NB.pDT, type="lambda", level=0.85)
coef(lasp.abnd.c.NB.pDT, type="det")
confint(lasp.abnd.c.NB.pDT, type="det", level=0.85)
coef(lasp.abnd.c.NB.pDT, type="gamma")
confint(lasp.abnd.c.NB.pDT, type="gamma", level=0.85)
coef(lasp.abnd.c.NB.pDT, type="omega")
confint(lasp.abnd.c.NB.pDT, type="omega", level=0.85)

#'  Model 3
coef(lasp.abnd.st.NB.pDT, type="lambda")
confint(lasp.abnd.st.NB.pDT, type="lambda", level=0.85)
coef(lasp.abnd.st.NB.pDT, type="det")
confint(lasp.abnd.st.NB.pDT, type="det", level=0.85)
coef(lasp.abnd.st.NB.pDT, type="gamma")
confint(lasp.abnd.st.NB.pDT, type="gamma", level=0.85)
coef(lasp.abnd.st.NB.pDT, type="omega")
confint(lasp.abnd.st.NB.pDT, type="omega", level=0.85)

#'  Model 4
coef(lasp.abnd.null.NB.pDT, type="lambda")
confint(lasp.abnd.null.NB.pDT, type="lambda", level=0.85)
coef(lasp.abnd.null.NB.pDT, type="det")
confint(lasp.abnd.null.NB.pDT, type="det", level=0.85)
coef(lasp.abnd.null.NB.pDT, type="gamma")
confint(lasp.abnd.null.NB.pDT, type="gamma", level=0.85)
coef(lasp.abnd.null.NB.pDT, type="omega")
confint(lasp.abnd.null.NB.pDT, type="omega", level=0.85)

#'  Model 5
coef(lasp.abnd.l.NB.pDT, type="lambda")
confint(lasp.abnd.l.NB.pDT, type="lambda", level=0.85)
coef(lasp.abnd.l.NB.pDT, type="det")
confint(lasp.abnd.l.NB.pDT, type="det", level=0.85)
coef(lasp.abnd.l.NB.pDT, type="gamma")
confint(lasp.abnd.l.NB.pDT, type="gamma", level=0.85)
coef(lasp.abnd.l.NB.pDT, type="omega")
confint(lasp.abnd.l.NB.pDT, type="omega", level=0.85)


#' _____________________________________________________________________________
#' ### Plot LASP
#' 
#' New dataset for predictions
nd.lasp <- data.frame(canopy = seq(-2, 2.5, length = 50),
                      numwood = seq(-2, 3, length = 50),
                      date = seq(-2,2.5, length = 50),
                      time = seq(-2,2.5, length = 50))
#' Canopy cover
#' Prediction of lambda and prediction intervals
nd.lasp$lam.predict.canopy <- predict(lasp.abnd.c.NB.pDT, type="lambda", newdata = nd.lasp, appendData = F)
#' Back-transform canopy for plotting
nd.lasp$canopyOrig <- nd.lasp$canopy*canopy.sd + canopy.mean
#' Create a variable for predicted lambda
nd.lasp$lam.predict.mean.canopy <- nd.lasp$lam.predict.canopy$Predicted
#' Calculate 85% Confidence intervals
nd.lasp$lam.predict.85LL.canopy <- nd.lasp$lam.predict.canopy$Predicted - 1.44*nd.lasp$lam.predict.canopy$SE
nd.lasp$lam.predict.85UL.canopy <- nd.lasp$lam.predict.canopy$Predicted + 1.44*nd.lasp$lam.predict.canopy$SE
nd.lasp$lam.predict.85LL.canopy <- ifelse(nd.lasp$lam.predict.85LL.canopy >= 0, nd.lasp$lam.predict.85LL.canopy, 0)

#' Base graphics version
plot(y = nd.lasp$lam.predict.canopy$Predicted, x =nd.lasp$canopyOrig, ylim=c(0,5), xlim=c(0,100))


#' Numwood
#' Prediction of lambda and prediction intervals
nd.lasp$lam.predict.numwood <- predict(lasp.abnd.st.NB.pDT, type="lambda", newdata = nd.lasp, appendData = F)
#' Back-transform numwood for plotting
nd.lasp$numwoodOrig <- nd.lasp$numwood*numwood.sd + numwood.mean
#' Create a variable for predicted lambda
nd.lasp$lam.predict.mean.numwood <- nd.lasp$lam.predict.numwood$Predicted
#' Calculate 85% Confidence intervals
nd.lasp$lam.predict.85LL.numwood <- nd.lasp$lam.predict.numwood$Predicted - 1.44*nd.lasp$lam.predict.numwood$SE
nd.lasp$lam.predict.85UL.numwood <- nd.lasp$lam.predict.numwood$Predicted + 1.44*nd.lasp$lam.predict.numwood$SE

#' Base graphics version
plot(y = nd.lasp$lam.predict.numwood$Predicted, x =nd.lasp$numwoodOrig, ylim=c(0,5), xlim=c(0,30))

#' _____________________________________________________________________________
#' ## LASP Plots
#' 
#' Abundance Canopy plot (for thesis)
#+ resultsLASP
abnd.lasp.c.plot <- ggplot(data = nd.lasp, aes(x=canopyOrig, y=lam.predict.mean.canopy)) +
  geom_line()+
  geom_ribbon(aes(ymin=lam.predict.85LL.canopy, ymax=lam.predict.85UL.canopy), alpha=0.3) +
  ylim(c(0,6)) +
  xlim(c(0,100)) +
  ylab(expression(lambda)) +
  xlab("Canopy Cover") +
  annotate("text", label = "A", x=10, y=5) +
  theme_classic()

abnd.lasp.ns.plot <- ggplot(data = nd.lasp, aes(x=numwoodOrig, y=lam.predict.mean.numwood)) +
  geom_line()+
  geom_ribbon(aes(ymin=lam.predict.85LL.numwood, ymax=lam.predict.85UL.numwood), alpha=0.3) +
  ylim(c(0,5)) +
  xlim(c(0,20)) +
  ylab(expression(lambda)) +
  xlab("Number of Woody Stems") +
  annotate("text", label = "B", x=1, y=5) +
  theme_classic()

grid.arrange(abnd.lasp.c.plot, abnd.lasp.ns.plot, ncol = 2)

#' Abundance Canopy plot (for dnr report)
#+ resultsLASP_DNRreport
abnd.lasp.c.plot.dnr <- ggplot(data = nd.lasp, aes(x=canopyOrig, y=lam.predict.mean.canopy)) +
geom_line()+
  geom_ribbon(aes(ymin=lam.predict.85LL.canopy, ymax=lam.predict.85UL.canopy), alpha=0.3) +
  ylim(c(0,6)) +
  xlim(c(0,100)) +
  ylab("Mean Abundance") +
  xlab("Canopy Cover") +
  annotate("text", label = "A", x=10, y=6) +
  theme_classic()

abnd.lasp.ns.plot.dnr <- ggplot(data = nd.lasp, aes(x=numwoodOrig, y=lam.predict.mean.numwood)) +
  geom_line()+
  geom_ribbon(aes(ymin=lam.predict.85LL.numwood, ymax=lam.predict.85UL.numwood), alpha=0.3) +
  ylim(c(0,5)) +
  xlim(c(0,20)) +
  ylab("Mean Abundance") +
  xlab("Number of Woody Stems") +
  annotate("text", label = "B", x=1, y=5) +
  theme_classic()

grid.arrange(abnd.lasp.c.plot.dnr, abnd.lasp.ns.plot.dnr, ncol = 2)
#' _____________________________________________________________________________
#' ## Save files
#' 
#' 

#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/09_abundance_results_birds.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 
#' *You can run the ezspin() code above to create an html in the "output" folder
#' and any associated figures will get put into the "figures" folder.*