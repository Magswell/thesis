#' # Snakes results - Figures & Tables
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
survey.data <- read.csv("data/raw_data/20180208_plot_survey_data.csv",
                        stringsAsFactors = F)

#' Load cipa data
cipa.abnd <- read.csv("data/processed_data/cipa_abundance_data.csv")
colnameskeep.cipa <- c("plot",colnames(cipa.abnd)[grep("^cipa",colnames(cipa.abnd))])
cipa.abnd <- cipa.abnd[,colnameskeep.cipa]

#' Load hele abundance data
hele.abnd <- read.csv("data/processed_data/hele_abundance_data.csv") 
colnameskeep.hele <- c("plot",colnames(hele.abnd)[grep("^hele",colnames(hele.abnd))])
hele.abnd <- hele.abnd[,colnameskeep.hele]

#' _____________________________________________________________________________
#' ## Process Data
#' 
#' PICA
pica.data <- 
  survey.data[,c("plot",
                 colnames(survey.data)[grep("^pica",colnames(survey.data))])]
pica.data$pica_1 <- as.numeric(pica.data$pica_1)
pica.data$pica_2 <- as.numeric(pica.data$pica_2)
pica.data$pica_3 <- ifelse(pica.data$pica_3 =="x", 
                           yes = NA, 
                           no = as.numeric(pica.data$pica_3))
str(pica.data)
counts <- NULL
for(plot in 1:60){
  pica.plot.data <- pica.data[pica.data$plot==plot,]
  for(row in 1:nrow(pica.plot.data)){
    sumrow <- sum(rowSums(pica.plot.data[,2:4]), na.rm = T) }
  print(paste0(
    "the total # observations in plot ",
    plot,
    " is ", sumrow))
  counts <- c(counts, sumrow)
}
length(counts[counts!=0])
mean(counts[counts!=0])
sd(counts[counts!=0])/sqrt(length(counts[counts!=0]))


#' HENA
hena.data <- 
  survey.data[,c("plot",
                 colnames(survey.data)[grep("^hena",colnames(survey.data))])]
str(hena.data)
hena.data$hena_1 <- as.numeric(hena.data$hena_1)
hena.data$hena_2 <- as.numeric(hena.data$hena_2)
hena.data$hena_3 <- ifelse(hena.data$hena_3 =="x", 
                           yes = NA, 
                           no = as.numeric(hena.data$hena_3))
table(hena.data$hena_3)
str(hena.data)
counts <- NULL
for(plot in 1:60){
  hena.plot.data <- hena.data[hena.data$plot==plot,]
  for(row in 1:nrow(hena.plot.data)){
    sumrow <- sum(rowSums(hena.plot.data[,2:4]), na.rm = T) }
  print(paste0(
    "the total # observations in plot ",
    plot,
    " is ", sumrow))
  counts <- c(counts, sumrow)
}
length(counts[counts!=0])
mean(counts[counts!=0])
sd(counts[counts!=0])/sqrt(length(counts[counts!=0]))

#' _____________________________________________________________________________
#' ## Function to calculate statistics
#' 
#' Create function to get descriptive results
description <- function(dataset, pattern){
  datacolumns <- colnames(dataset)[grep(pattern,colnames(dataset))]
  number.unoccupied <- number.occupied <- 0
  mean.count.each.occupied.plots <- NA
  for(row in 1:nrow(dataset)){
    data.subset <- dataset[row,!is.na(dataset[row,])]
    data.subset <- data.subset[,2:ncol(data.subset)]
    test.all.unoccupied <- 
      ifelse(test = as.numeric(rowSums(data.subset))>=1,
             yes = FALSE,
             no = TRUE)
    if(test.all.unoccupied==T){
      print(paste0("plot ", dataset$plot[row]," was unoccupied"))
      number.unoccupied <- number.unoccupied + 1
    }else{
      print(paste0("plot ", dataset$plot[row]," was occupied"))
      number.occupied <- number.occupied + 1
      data.occupied <- data.subset[,data.subset[,]>=1]
      mean.count.each.occupied.plots <- c(
        mean.count.each.occupied.plots,
        mean(as.numeric(data.occupied))
      )
    }
  }
  print(paste0(
    number.occupied, " out of ", number.unoccupied + number.occupied, " (",
    round(number.occupied/(number.unoccupied + number.occupied)*100, digits = 2),
    "%) plots were occupied"))
  print(paste0("Mean abundance in occupied plots was ",
               round(mean(mean.count.each.occupied.plots, na.rm = T),digits=2),
               " +/- ",
               round(sd(mean.count.each.occupied.plots, na.rm = T)/
                       sqrt(length(mean.count.each.occupied.plots)-1),digits=2),
               " SE"))
}

#' ### Descriptive results for CIPA
description(dataset = cipa.abnd, pattern = "^cipa")

#' ### Descriptive results for HELE
description(dataset = hele.abnd, pattern = "^hele")

#' ### Descriptive results for HENA
description(dataset = hena.data, pattern = "^hena")



#' Plot for defense
plotting.data <- as.data.frame(matrix(data = NA,nrow = 1,ncol = 4))
colnames(plotting.data) <- c("variable", "est.mean", "ll85", "ul85")
plotting.data$variable<- "Disturbance"
plotting.data$est.mean <- 4.13
plotting.data$ll85 <- 2.58
plotting.data$ul85 <- 5.68

plotting.data$Species<-"Lark Sparrow"

ggplot(plotting.data,aes(x=variable,y=est.mean))+
  geom_point(size=4)+
  geom_linerange(aes(ymin=ll85,ymax=ul85))+
  geom_hline(yintercept=0, col="red")+
  #theme_bw(base_size = 20)+
  xlab(NULL)+
  ylab("Relative Abundance")+
  ylim(0,6)+
  theme_tufte()+
  theme(axis.text=element_text(size=20, colour="black"),
        axis.title=element_text(size=20,face="bold"))
  #ggtitle("Response to Oak Savanna Restoration Techniques")
#ggsave(filename = "output/LASP_disturbance_lineplot_defense.png",device = "png")








