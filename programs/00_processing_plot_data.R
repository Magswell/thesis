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
#' Using lark sparrow (lasp) data to pull off plot-level variables
#' 
lasp.data <- read.csv("data/raw_data/20171020_LASP_occ.csv")

#' Pull in elevation data
#'
elev.sdsf <- read.delim("data/shapefile_data/sdsf_zone_stat.txt", sep = ",")
elev.snwr <- read.delim("data/shapefile_data/snwr_zone_stat.txt", sep = ",")

#' Load Average Liatris by plot
load("data/processed_data/avg_liatris.Rdata")

#' ## Process Data - Create plot-level dataset
#' 
#' Pull off habitat variables 
colnames(lasp.data)
plot.data <- lasp.data[,c("plot", 
                          "area", 
                          "pct_canopy",
                          "num_woody_stems",
                          "pct_bunchgrass" ,
                          "num_mounds" ,
                          "pct_bluestem",
                          "num_blowouts" ,
                          "num_liatris_stems",
                          "num_open_oaks",
                          "num_milkweed_stems",
                          "litter_depth" ,
                          "pct_nonbunchgrass",
                          "tot_grass" )]

#' Add average liatris to plot-level data
plot.data <- merge(x = plot.data, y = avg_liatris, by = "plot")

#' Calculate elevation coefficient of variation for each plot
#' 
elev.sdsf$elev_cv <- elev.sdsf$STD/elev.sdsf$MEAN
elev.sdsf <- elev.sdsf[,c("PLOT__", "elev_cv")]
elev.sdsf$PLOT_NO <- elev.sdsf$PLOT__
elev.sdsf <- elev.sdsf[,c("PLOT_NO", "elev_cv")]
elev.snwr$elev_cv <- elev.snwr$STD/elev.snwr$MEAN
elev.snwr <- elev.snwr[,c("PLOT_NO", "elev_cv")]
elev.all <- rbind(elev.sdsf, elev.snwr)

#' Add Coef of variation to plot-level data
plot.data <- merge(x = plot.data, y = elev.all, by.x = "plot", by.y = "PLOT_NO", all.x = T)

#' 
#' Add disturbance variables
#' 
#' BY00, BY03, BY07, BY10, BY12, BY13, BY15 : Years of recorded burns (Y/N)
plot.data$BY00 <- ifelse(test = plot.data$plot==4, yes = 1, no = 0)
plot.data$BY03 <- ifelse(test = plot.data$plot==2 |
                           plot.data$plot==4, yes = 1, no = 0)
plot.data$BY07 <- ifelse(test = plot.data$plot==3 |
                           plot.data$plot==4 |
                           plot.data$plot==10 |
                           plot.data$plot==11 |
                           plot.data$plot==12 |
                           plot.data$plot==13, yes = 1, no = 0)
plot.data$BY10 <- ifelse(test = plot.data$plot==5 |
                           plot.data$plot==6 |
                           plot.data$plot==7 |
                           plot.data$plot==8 |
                           plot.data$plot==9 |
                           plot.data$plot==10|
                           plot.data$plot==11|
                           plot.data$plot==12|
                           plot.data$plot==13, yes = 1, no = 0)
plot.data$BY12 <- ifelse(test = plot.data$plot==3 |
                           plot.data$plot==4 |
                           plot.data$plot==5 |
                           plot.data$plot==6 |
                           plot.data$plot==14, yes = 1, no = 0)
plot.data$BY13 <- ifelse(test = plot.data$plot==7, yes = 1, no = 0)
plot.data$BY15 <- ifelse(test = plot.data$plot==6 |
                           plot.data$plot==10 |
                           plot.data$plot==11 |
                           plot.data$plot==12 |
                           plot.data$plot==13 |
                           plot.data$plot==14, yes = 1, no = 0)
#' Grazing
#' 
plot.data$GY15 <- ifelse(test = plot.data$plot==7 |
                           plot.data$plot==8 |
                           plot.data$plot==9|
                           plot.data$plot==10 |
                           plot.data$plot==11 |
                           plot.data$plot==12 |
                           plot.data$plot==13, yes = 1, no = 0)
plot.data$GY16 <- ifelse(test = plot.data$plot==10 |
                           plot.data$plot==11 |
                           plot.data$plot==12|
                           plot.data$plot==13, yes = 1, no = 0)
#' Thinning
#' 
plot.data$TY04 <- ifelse(test = plot.data$plot==50 |
                           plot.data$plot==51 |
                           plot.data$plot==58, yes = 1, no = 0)
plot.data$TY05 <- ifelse(test = plot.data$plot==42 |
                           plot.data$plot==58, yes = 1, no = 0)
plot.data$TY06 <- ifelse(test = plot.data$plot==58, yes = 1, no = 0)
plot.data$TY07 <- ifelse(test = plot.data$plot==29, yes = 1, no = 0)
plot.data$TY08 <- ifelse(test = plot.data$plot==27, yes = 1, no = 0)
plot.data$TY09 <- ifelse(test = plot.data$plot==49 |
                           plot.data$plot==53, yes = 1, no = 0)
plot.data$TY10 <- ifelse(test = plot.data$plot==26 |
                           plot.data$plot==30, yes = 1, no = 0)
plot.data$TY15 <- ifelse(test = plot.data$plot==17 |
                           plot.data$plot==18 |
                           plot.data$plot==52 |
                           plot.data$plot==54 |
                           plot.data$plot==55, yes = 1, no = 0)
plot.data$TY16 <- ifelse(test = plot.data$plot==19 |
                           plot.data$plot==20 |
                           plot.data$plot==21, yes = 1, no = 0)

#' Clearcut
#' 
plot.data$CC04 <- ifelse(test = plot.data$plot==55 |
                           plot.data$plot==56 |
                           plot.data$plot==57, yes = 1, no = 0)
plot.data$CC05 <- ifelse(test = plot.data$plot==47, yes = 1, no = 0)
plot.data$CC07 <- ifelse(test = plot.data$plot==31, yes = 1, no = 0)
plot.data$CC08 <- ifelse(test = plot.data$plot==60, yes = 1, no = 0)
plot.data$CC11 <- ifelse(test = plot.data$plot==27 |
                           plot.data$plot==42 |
                           plot.data$plot==43, yes = 1, no = 0)
plot.data$CC12 <- ifelse(test = plot.data$plot==23, yes = 1, no = 0)
plot.data$CC13 <- ifelse(test = plot.data$plot==34 |
                           plot.data$plot==35, yes = 1, no = 0)
plot.data$CC14 <- ifelse(test = plot.data$plot==24, yes = 1, no = 0)
plot.data$CC15 <- ifelse(test = plot.data$plot==39|
                         plot.data$plot==33 , yes = 1, no = 0)
plot.data$CC16 <- ifelse(test = plot.data$plot==45, yes = 1, no = 0)

#' Planted Pines
#' 
plot.data$PP11 <- ifelse(test = plot.data$plot==31 |
                           plot.data$plot==36 |
                           plot.data$plot==37 |
                           plot.data$plot==48, yes = 1, no = 0)


#' Create new Boolean variables for pre-2015 disturbance (burned, thinned, or grazed 
#' between 2010 and 2014) as "DY_pre2015" and disturbances since 2014 as "DY_ge2015"
plot.data$DY_pre2015 <- ifelse(test = plot.data$BY10==1 |
                                 plot.data$BY12==1|
                                 plot.data$BY13==1|
                                 plot.data$CC11==1|
                                 plot.data$CC12==1|
                                 plot.data$CC13==1|
                                 plot.data$CC14==1|
                                 plot.data$PP11==1|
                                 plot.data$TY10==1, yes = 1, no = 0)
plot.data$DY_ge2015 <- ifelse(test = plot.data$BY15==1 |
                                plot.data$GY15==1|
                                plot.data$GY16==1|
                                plot.data$TY15==1|
                                plot.data$TY16==1|
                                plot.data$CC15==1|
                                plot.data$CC16==1, yes = 1, no = 0)


#' Create new Boolean variables for pre-2014 disturbance (burned, thinned, or grazed 
#' between 2010 and 2013) as "DY_pre2014" and disturbances since 2013 as "DY_ge2014"
plot.data$DY_pre2014 <- ifelse(test = plot.data$BY10==1 |
                                 plot.data$BY12==1|
                                 plot.data$BY13==1|
                                 plot.data$CC11==1|
                                 plot.data$CC12==1|
                                 plot.data$CC13==1|
                                 plot.data$PP11==1|
                                 plot.data$TY10==1, yes = 1, no = 0)
plot.data$DY_ge2014 <- ifelse(test = plot.data$BY15==1 |
                                plot.data$GY15==1|
                                plot.data$GY16==1|
                                plot.data$TY15==1|
                                plot.data$TY16==1|
                                plot.data$CC14==1|
                                plot.data$CC15==1|
                                plot.data$CC16==1, yes = 1, no = 0)

#' This variable identifies whether a plot had any understory-specific 
#' disturbances between seasons.
plot.data$UDY_ge2015 <- ifelse(test = plot.data$BY15==1 |
                                 plot.data$GY15==1|
                                 plot.data$GY16==1, yes = 1, no = 0)

#' Specify the types of disturbance
#' 
plot.data$burned_pre2015 <- ifelse(test = plot.data$BY10==1 |
                                 plot.data$BY12==1|
                                 plot.data$BY13==1, yes = 1, no = 0)
plot.data$harvest_pre2015 <- ifelse(test = plot.data$CC11==1|
                                      plot.data$CC12==1|
                                       plot.data$CC13==1|
                                       plot.data$CC14==1|
                                       plot.data$TY10==1, yes = 1, no = 0)
plot.data$type_pre2015 <- ifelse(test = plot.data$burned_pre2015==1, 
                                 yes = "burned",
                                 no = ifelse(test = plot.data$CC11==1|
                                               plot.data$CC12==1|
                                               plot.data$CC13==1|
                                               plot.data$CC14==1, 
                                  yes = "clearcut",
                                  no = ifelse(test = plot.data$TY10==1, 
                                              yes = "thinned", 
                                              no = ifelse(test = plot.data$PP11==1, "pines planted", "none"))))

plot.data$burnGraze_ge2015 <- ifelse(test = plot.data$BY15==1 |
                                plot.data$GY15==1|
                                plot.data$GY16==1, yes = 1, no = 0)
plot.data$harvest_ge2015 <- ifelse(test = plot.data$TY15==1|
                                plot.data$TY16==1|
                                plot.data$CC15==1|
                                plot.data$CC16==1, yes = 1, no = 0)
plot.data$type_ge2015 <- ifelse(test = plot.data$burnGraze_ge2015==1, 
                                yes = "burnGraze",
                                no = ifelse(test = plot.data$harvest_ge2015,
                                            yes = "harvest",
                                            no = "none"))

#' Standardize site-covariates, so they are centered on zero and can be 
#' compared on similar scales (as opposed to having canopy cover 0-100, 
#' oaks 0-40 or so) This is more important in situations where the scales 
#' are really different.
#' 
#' Structured so that for each site we are describing difference from the mean
#' 
#' Total grass
totgrass.mean <- mean(plot.data$tot_grass)
totgrass.sd <- sd(plot.data$tot_grass)
totgrass.z <- (plot.data$tot_grass-totgrass.mean)/totgrass.sd
#'
#' Non-bunch grass
nonbunch.mean <- mean(plot.data$pct_nonbunchgrass)
nonbunch.sd <- sd(plot.data$pct_nonbunchgrass)
nonbunch.z <- (plot.data$pct_nonbunchgrass-nonbunch.mean)/nonbunch.sd 
#'
#' Bunch grass
bunch.mean <- mean(plot.data$pct_bunchgrass)
bunch.sd <- sd(plot.data$pct_bunchgrass)
bunch.z <- (plot.data$pct_bunchgrass-bunch.mean)/bunch.sd
#' 
#' Number of woody stems
numwood.mean <- mean(plot.data$num_woody_stems)
numwood.sd <- sd(plot.data$num_woody_stems)
numwood.z <- (plot.data$num_woody_stems-numwood.mean)/numwood.sd
#'
#' Canopy cover
canopy.mean <- mean(plot.data$pct_canopy)
canopy.sd <- sd(plot.data$pct_canopy)
canopy.z <- (plot.data$pct_canopy-canopy.mean)/canopy.sd 
#'
#' Number of open oaks
oaks.mean <- mean(plot.data$num_open_oaks)
oaks.sd <- sd(plot.data$num_open_oaks)
oaks.z <- (plot.data$num_open_oaks-oaks.mean)/oaks.sd
#'
#' Number of gopher mounds
mounds.mean <- mean(plot.data$num_mounds)
mounds.sd <- sd(plot.data$num_mounds)
mounds.z <- (plot.data$num_mounds-mounds.mean)/mounds.sd
#' 
#' Number of blowouts
blowouts.mean <- mean(plot.data$num_blowouts)
blowouts.sd <- sd(plot.data$num_blowouts)
blowouts.z <- (plot.data$num_blowouts-blowouts.mean)/blowouts.sd
#'
#' Number of liatris stems
liatris.mean <- mean(plot.data$avg_liatris)
liatris.sd <- sd(plot.data$avg_liatris)
liatris.z <- (plot.data$avg_liatris-liatris.mean)/liatris.sd
#' 
#' Number of milkweed
milkweed.mean <- mean(plot.data$num_milkweed_stems)
milkweed.sd <- sd(plot.data$num_milkweed_stems)
milkweed.z <- (plot.data$num_milkweed_stems-milkweed.mean)/milkweed.sd
#' 
#' Litter depth
litter.mean <- mean(plot.data$litter_depth)
litter.sd <- sd(plot.data$litter_depth)
litter.z <- (plot.data$litter_depth-litter.mean)/litter.sd

#' Elevation coefficient of variation
#' 
elev_CV.mean <- mean(plot.data$elev_cv)
elev_CV.sd <- sd(plot.data$elev_cv)
elev_CV.z <- (plot.data$elev_cv-elev_CV.mean)/elev_CV.sd

#' Append standardized data to plot data set
#' 
plot.data$totgrass.z <- totgrass.z
plot.data$bunchgrass.z <- bunch.z
plot.data$oaks.z <- oaks.z
plot.data$canopy.z <- canopy.z
plot.data$numwood.z <- numwood.z
plot.data$mounds.z <- mounds.z
plot.data$blowouts.z <- blowouts.z
plot.data$plotlvl_liatris.z <- liatris.z
plot.data$plotlvl_milkweed.z <- milkweed.z
plot.data$litter.z <- litter.z
plot.data$elev_CV.z <- elev_CV.z
plot.data$nonbunch.z <- nonbunch.z

#' _____________________________________________________________________________
#' ## Save files
#' 
#' 
#'  
#' Save plot-level data
write.csv(x = plot.data, file = "data/processed_data/plot_level_data.csv")
#' Mean/SD of standardized covariates
save(canopy.mean, canopy.sd, 
     numwood.mean, numwood.sd,
     elev_CV.mean, elev_CV.sd,
     bunch.mean, bunch.sd,
     file = "data/output_data/plot_level_meanSD.Rdata")

#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/00_thesis_programs/00_processing_plot_data.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)