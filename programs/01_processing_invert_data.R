#' # Processing the invertebrate survey data
#' 
#' 
#' ### Preamble
#' 
#' Load libraries
#+ libraries, message = F, warning = F
library(knitr) # documentation-related
library(ezknitr) # documentation-related
library(devtools) # documentation-related
library(StreamMetabolism) # Includes sunrise / sunset function sunrise.set
library(lubridate) # Date / time functions
library(dplyr)

#' Clear environment and set seed
#' 
remove(list = ls())
set.seed(2583722)

#' _____________________________________________________________________________
#' ## Load Data
#' 
survey.data <- read.csv("data/raw_data/20180208_plot_survey_data.csv",
                        stringsAsFactors = F)

#' ## Remove duplicate species records
#' 
#' Ids to delete from obs_nongame.plot_survey_final
duplicate.ids <- c(924, 925, 926, 927, 928, 929, 930, 
                   931, 932, 933, 934, 935, 1093, 1094, 
                   1095, 1096, 1097, 1098, 1099, 1092, 
                   1103, 1104, 1105, 1106, 1107, 1108, 
                   1109, 1110, 1111, 1112, 1120, 1121)
#' Take out duplicate rows
survey.data <- survey.data[!survey.data$id %in% duplicate.ids,]

#' _____________________________________________________________________________
#'  ## Clean up Date & time
#'  
survey.data$date <- as.Date(with(survey.data, 
                               paste(year, month, day, sep="-")), 
                          format = "%Y-%m-%d")
#' Time
#' Fix problem times
survey.data$survey_start[survey.data$uuid==
                           "5398cb6e-bc65-4c1b-8199-a1f7fecf7e39"] <- 1506
survey.data$survey_end[survey.data$uuid==
                         "5398cb6e-bc65-4c1b-8199-a1f7fecf7e39"] <- 1630
survey.data$survey_start[survey.data$uuid==
                           "53ab42a1-fbce-4737-8017-bf494672527b"] <- 1612
survey.data$survey_end[survey.data$uuid==
                         "53ab42a1-fbce-4737-8017-bf494672527b"] <- 1642
survey.data$survey_start[survey.data$uuid==
                           "54233cbd-0386-4ed3-b3e3-4e8138dd422a"] <- 1620
survey.data$survey_end[survey.data$uuid==
                         "54233cbd-0386-4ed3-b3e3-4e8138dd422a"] <- 1650

#' Other Time Tidying
survey.data$survey_start <- as.numeric(
  gsub(pattern = ":",
       replacement = "",
       x = survey.data$survey_start))
survey.data$survey_start_char <- as.character(survey.data$survey_start)

survey.data$survey_start_char[survey.data$survey_start<1000] <- 
  paste0("0", survey.data$survey_start_char[survey.data$survey_start<1000])

survey.data$survey_start <- as.POSIXlt(with(survey.data, 
                                         paste(year, month, day, survey_start_char,
                                               sep="-")), 
                                    format = "%Y-%m-%d-%H%M")
survey.data <- survey.data[with(survey.data, order(year, plot, survey_start)),]





#' _____________________________________________________________________________
#'  ## Get time from sunrise information
#' Zimmerman MN lat/long
#' 45.4432986 -93.589962
#'
#' Add a field with start times in type POSIXct converted to POSIXlt
survey.data$start.poslt <- as.POSIXlt(survey.data$survey_start)

#' Add field for day of year (1-365)
survey.data$doy <- survey.data$start.poslt$yday

#' Create sunrise field, initially filled with survey_start times to force 
#' POSIXlt format
survey.data$sunrise <- as.POSIXlt(survey.data$survey_start)

#' Get sunrise info for all of the dates that had plot searches
#' 
#' Sunrise.set function returns both sunrise and sunset, so we just want col 1
for (rr in 1:nrow(survey.data)){
  survey.data$sunrise[rr] <- sunrise.set(lat = 45.44,long = -93.59,
                                         date = survey.data$survey_start[rr],
                                         timezone = "UTC+6",num.days = 1)[,1]
} 

#' Calculate time between sunrise and survey start (start.poslt)
#' 
#' Create field hours.from.rise
survey.data$hr.rise <- difftime(time1 = survey.data$start.poslt,
                                       time2 = survey.data$sunrise,
                                       units = "hours")

#' Round survey start time to nearest hour
survey.data$start.posct <- as.POSIXct(survey.data$start.poslt)
survey.data <- subset(survey.data, select=-c(sunrise, start.poslt, survey_start))
survey.data$sd.hour.rnd <- as.POSIXct(round(x = survey.data$start.posct, "hour"))

#' _____________________________________________________________________________
#'  ## Get hourly temp data from St Cloud Airport station WBAN:14926
climate.data <- read.csv("data/raw_data/20180220_climate_data.csv",
                         stringsAsFactors = F)

str(climate.data)

#' DATE field is character, so we need to convert it
#' 
#' Force POSIXlt format by conversion, but this only grabs the date (not time)
climate.data$cl.date <- as.POSIXlt(climate.data$DATE)
climate.data$cl.hour <- as.POSIXlt(climate.data$DATE)
climate.data$cl.hour.rnd <- as.POSIXlt(climate.data$DATE)

#' Now that field is formatted correctly, get the whole date with hour and minutes
climate.data$cl.date <- strptime(climate.data$DATE, format = "%Y-%m-%d %H:%M")
#' Now that field is formatted correctly, get the whole date, cut off at hour (not rounded)
climate.data$cl.hour <- strptime(climate.data$DATE, format = "%Y-%m-%d %H")

#' Now that field is formatted correctly, get the whole date, rounded to nearest hour
climate.data$cl.hour.rnd <- strptime(climate.data$DATE, format = "%Y-%m-%d %H:%M")
climate.data$cl.hour.rnd <- round(climate.data$cl.hour.rnd, "hours")


#' Line below does not work. For some reason I can't do it all in one step.
#climate.data$cl.date <- as.POSIXlt(strptime(climate.data$DATE, format = "%Y-%m-%d %h:%M"))



#' _____________________________________________________________________________
#'  ## Join hourly climate data (dry bulb temp) to survey data
climate_clip <- climate.data[,c("cl.hour.rnd","HOURLYDRYBULBTEMPC",
                                "HOURLYWindSpeed","REPORTTPYE")]
climate_clip$cl.hour.rnd <- as.POSIXct(climate_clip$cl.hour.rnd)
#test <- merge(x = survey.data, y = climate_clip, 
#              by.x = "sd.hour.rnd", by.y = "cl.hour.rnd", 
#              all.x = T, all.y=F)
survey.data2 <- left_join(x = survey.data, y = climate_clip, by=c("sd.hour.rnd"="cl.hour.rnd"))

#' There are duplicate climate records with report type "FM-16" that I will delete here
survey.data <- survey.data2[survey.data2$REPORTTPYE!="FM-16",]

#' Fill in a few NA dry bulb temps. There are a bunch of missing temps for 
#' 2014_09_09

survey.data$HOURLYDRYBULBTEMPC <- ifelse(survey.data$sd.hour.rnd == "2014-09-09 14:00:00" |
                                           survey.data$sd.hour.rnd == "2014-09-09 12:00:00" , 
                                   yes = ifelse(survey.data$plot == "34" |
                                                  survey.data$plot == "37" |
                                                  survey.data$plot == "41" |
                                                  survey.data$plot == "47" , 
                                                yes = 21,
                                                no = survey.data$HOURLYDRYBULBTEMPC),
                                   no = survey.data$HOURLYDRYBULBTEMPC)

#' Fill in a few NA wind speeds. There are a bunch of missing values for 
#' 2014_09_09
survey.data$HOURLYWindSpeed <- ifelse(survey.data$sd.hour.rnd == "2014-09-09 14:00:00" |
                                        survey.data$sd.hour.rnd == "2014-09-09 12:00:00" , 
                                      yes = ifelse(survey.data$plot == "34" |
                                                     survey.data$plot == "37" |
                                                     survey.data$plot == "41" |
                                                     survey.data$plot == "47" ,
                                                   yes = 9, 
                                                   no = survey.data$HOURLYWindSpeed),
                                      no = survey.data$HOURLYWindSpeed)

#' _____________________________________________________________________________
#' ## Remove fields that we don't need
#' 
#' Columns to delete from survey.data because I won't use them
survey.data <- subset(survey.data, select = -c(username, apikey, uuid, gps, start_point, 
                                               end_point, created_at, updated_at, 
                                               athi_1, athi_2, athi_3, 
                                               img_name, img_path,
                                               origin_table, origin_id,
                                               pica_1, pica_2, pica_3,
                                               hena_1, hena_2, hena_3,
                                               cisc_1, cisc_2, cisc_3,
                                               cifo_1, cifo_2, cifo_3,
                                               cipu_1, cipu_3, cipu_3,
                                               survey_start_char, 
                                               other_spp, start.posct,
                                               REPORTTPYE
                                               ))
colnames(survey.data)

#' _____________________________________________________________________________
#' ## Populate "area" when it's missing
#' 
#' How many are missing?
summary(survey.data$area)
table(survey.data$plot[survey.data$area=="SNWR"])
table(survey.data$plot[survey.data$area=="SDSF"])

survey.data$area <- ifelse(test = survey.data$plot <= 20 |
                             survey.data$plot == 61, 
                           yes = "SNWR", 
                           no = "SDSF")
#' Check numbers 
table(survey.data$area)
table(survey.data$plot[survey.data$area=="SNWR"])
table(survey.data$plot[survey.data$area=="SDSF"])

#' _____________________________________________________________________________
#' ## Survey Effort
#' 
#' Defined as number of observers & duration
#' 
#' Duration was either 30 or 60 minutes. Have to clean up discrepancies. (Durations
#' verified by start and end time)
#' 
#' **Also, if duration was blank, should be 30 minutes**
summary(survey.data$survey_length)
survey.data$survey_length[is.na(survey.data$survey_length)] <- 30
survey.data$survey_length[survey.data$survey_length==20|
                            survey.data$survey_length==40|
                            survey.data$survey_length==31|
                            survey.data$survey_length==32] <- 30
table(survey.data$survey_length)

#' **If number of observers was blank, populate num_obs**
#' 
#' Summary of number of observations (i.e., how many NAs?)
summary(survey.data$num_obs)
#' What does observer column look like for num_obs==NA?
survey.data$observer[is.na(survey.data$num_obs)]
#' When two observers named, change num_obs to 2
survey.data$num_obs[grep(pattern = ",", x = survey.data$observer)] <- 2
# Double check that all "two-observers" in observer column are fixed
survey.data$observer[is.na(survey.data$num_obs)]
#' Now, all the rest of missing num_obs should be 1
survey.data$num_obs[is.na(survey.data$num_obs)] <- 1
# Double check
summary(survey.data$num_obs)
#' Finally, record with num_obs == 344 is a mis-placed waypoint; change to 1
survey.data$num_obs[survey.data$num_obs==344] <- 1
# Double check
summary(survey.data$num_obs)

#' **Calculate survey effort (may be used as an offset in models)**
#' 
#' 
survey.data$survey_effort <- survey.data$survey_length*survey.data$num_obs
summary(survey.data$survey_effort)



#' _____________________________________________________________________________
#' ## Set values for liatris fields
#'
#' Fix liatris 'NA' values that don't belong there. 
#' For details, see "corrections to HELE searches.csv"
#' 
#' Change liatris to 0 for all survey periods, plot 16, 2016A. survey.data$id = 1215
#' 
survey.data$liatris_1 <- ifelse(survey.data$id == 1215 & is.na(survey.data$liatris_1),
                                   yes = "0",
                                   no =survey.data$liatris_1)
survey.data$liatris_2 <- ifelse(survey.data$id == 1215 & is.na(survey.data$liatris_2),
                                   yes = "0",
                                   no =survey.data$liatris_2)
survey.data$liatris_3 <- ifelse(survey.data$id == 1215 & is.na(survey.data$liatris_3),
                                yes = "0",
                                no =survey.data$liatris_3)

#' Change liatris to 0 for survey periods 2 and 3, plot 25, 2016A. survey.data$id = 1222

survey.data$liatris_2 <- ifelse(survey.data$id == 1222 & is.na(survey.data$liatris_2),
                                yes = "0",
                                no =survey.data$liatris_2)
survey.data$liatris_3 <- ifelse(survey.data$id == 1222 & is.na(survey.data$liatris_3),
                                yes = "0",
                                no =survey.data$liatris_3)

#' Change liatris to 0 for survey periods 2 and 3, plot 30, 2016B. survey.data$id = 1226

survey.data$liatris_2 <- ifelse(survey.data$id == 1226 & is.na(survey.data$liatris_2),
                                yes = "0",
                                no =survey.data$liatris_2)
survey.data$liatris_3 <- ifelse(survey.data$id == 1226 & is.na(survey.data$liatris_3),
                                yes = "0",
                                no =survey.data$liatris_3)

#' Change liatris to 0 for survey period 3, plot 37, 2016A and 2016B. survey.data$id = 1237, 1238
 
survey.data$liatris_3 <- ifelse(survey.data$id == 1237 & is.na(survey.data$liatris_3),
                                yes = "0",
                                no =survey.data$liatris_3)
survey.data$liatris_3 <- ifelse(survey.data$id == 1238 & is.na(survey.data$liatris_3),
                                yes = "0",
                                no =survey.data$liatris_3)

#' Change liatris to 0 for survey period 2, plot 49, 2016A. survey.data$id = 1251
 
survey.data$liatris_2 <- ifelse(survey.data$id == 1251 & is.na(survey.data$liatris_2),
                                yes = "0",
                                no =survey.data$liatris_2)

#' Change liatris to 0 for survey periods 1 and 3, plot 58, 2015B. survey.data$id = 998
#' 
survey.data$liatris_1 <- ifelse(survey.data$id == 998 & survey.data$liatris_1=="Not Recorded",
                                yes = "0",
                                no =survey.data$liatris_1)

survey.data$liatris_3 <- ifelse(survey.data$id == 998 & survey.data$liatris_3=="Not Recorded",
                                yes = "0",
                                no =survey.data$liatris_3)



#' _____________________________________________________________________________
#' ## Fix inconsistent values
#' 
survey.data$liatris1_num <- ifelse(survey.data$liatris_1 == "?ëÑ 16 - Many" |
                                    survey.data$liatris_1 == ">15" , 
                                   yes = 20, 
                                   no = ifelse(survey.data$liatris_1 == "6 to 15 - Some" |
                                                 survey.data$liatris_1 == "15-Jun", 
                                               yes = 10.5,
                                               no = ifelse(survey.data$liatris_1 == "1 to 5 - Few" |
                                                             survey.data$liatris_1 == "5-Jan",
                                                           yes = 3,
                                                           no = ifelse(survey.data$liatris_1 == "0 - None" |
                                                                         survey.data$liatris_1 == "0",
                                                                       yes = 0,
                                                                       no = ifelse(survey.data$liatris_1 == "Not Recorded",
                                                                                   yes= NA,
                                                                                   no= NA
                                                                       )))))

survey.data$liatris2_num <- ifelse(survey.data$liatris_2 == "?ëÑ 16 - Many" |
                                     survey.data$liatris_2 == ">15" , 
                                   yes = 20, 
                                   no = ifelse(survey.data$liatris_2 == "6 to 15 - Some", 
                                               yes = 10.5,
                                               no = ifelse(survey.data$liatris_2 == "1 to 5 - Few" |
                                                             survey.data$liatris_2 == "5-Jan",
                                                           yes = 3,
                                                           no = ifelse(survey.data$liatris_2 == "0 - None" |
                                                                         survey.data$liatris_2 == "0",
                                                                       yes = 0,
                                                                       no = ifelse(survey.data$liatris_2 == "Not Recorded",
                                                                                   yes= NA,
                                                                                   no= NA
                                                                       )))))

survey.data$liatris3_num <- ifelse(survey.data$liatris_3 == "?ëÑ 16 - Many" |
                                     survey.data$liatris_3 == ">15" , 
                                   yes = 20, 
                                   no = ifelse(survey.data$liatris_3 == "6 to 15 - Some" |
                                                 survey.data$liatris_3 == "15-Jun", 
                                               yes = 10.5,
                                               no = ifelse(survey.data$liatris_3 == "1 to 5 - Few" |
                                                             survey.data$liatris_3 == "5-Jan",
                                                           yes = 3,
                                                           no = ifelse(survey.data$liatris_3 == "0 - None" |
                                                                         survey.data$liatris_3 == "0",
                                                                       yes = 0,
                                                                       no = ifelse(survey.data$liatris_3 == "Not Recorded",
                                                                                   yes= NA,
                                                                                   no= NA
                                                                       )))))



                                   
                                   
                                   
#' 
#' _____________________________________________________________________________
#'  ## Add variable for observer skill level
table(survey.data$observer)
skilled.observers <- c("CEs", "CES", "CEX", "CS, ME", "eph", "Eph", "EPh", "EPH", "Mre")
survey.data$obs_skill <- ifelse(survey.data$observer %in% skilled.observers, 
                                yes = 1,
                                no = 0)
#' Check we got the right number of rows, expecting 1 x 389 and 0 x 76
table(survey.data$obs_skill)

#' _____________________________________________________________________________
#' ## Remove plot 44 & 61
survey.data <- survey.data[survey.data$plot != 44 &
                             survey.data$plot != 61,]
#'
#' Separate out for each species of interest
#' 
#' _____________________________________________________________________________
#' ## Tiger Beetles (CIPA)
#'
#' Tiger beetles only surveyed May 1 through June 30; when air temp was > 50
#' There is another flight of tiger beetles later in the season, so include all 
#' surveys from May - Autumn
cipa.data <- survey.data[survey.data$month>=5,]
#' Also, remove columns no longer needed
cipa.data <- cipa.data[,c("id", "area", "date", "year",
                          "plot", 
                          "cipa_1", "cipa_2", "cipa_3",
                          "survey_effort", "hr.rise",
                          "obs_skill", "doy",
                          "HOURLYWindSpeed", "HOURLYDRYBULBTEMPC")]

#' Everything is stored as character because of the Xs, so remove 
#' Xs and convert to numeric
cipa.data$cipa_1 <- ifelse(cipa.data$cipa_1=="x" | cipa.data$cipa_1=="X", NA,
                           cipa.data$cipa_1)

cipa.data$cipa_2 <- ifelse(cipa.data$cipa_2=="x" | cipa.data$cipa_2=="X", NA,
                           cipa.data$cipa_2)    

cipa.data$cipa_3 <- ifelse(cipa.data$cipa_3=="x" | cipa.data$cipa_3=="X", NA,
                           cipa.data$cipa_3)


#' Add column to exclude surveys with NA in all three survey periods
cipa.data$exclude <- ifelse(is.na(cipa.data$cipa_1) & is.na(cipa.data$cipa_2) &
                            is.na(cipa.data$cipa_3), yes = "T", no =  "F")

#' Fix wonky NA in field cipa_3. Should be 0
cipa.data$cipa_3 <- ifelse(cipa.data$id=="784", yes="0",no = cipa.data$cipa_3)

#' 

#' **Reshape data from "long-format" to "wide-format"**
#' 
survey.letters <- c("A", "B", "C", "D", "E", "F", "G")
cipa.long <- NULL


#' 

for(yy in 2014:2016){       # cycle through years
  for(pp in c(1:43,45:60)){ # and cycle through plots
    # Extract out only survey data for that year yy and plot pp
    plot.data <- cipa.data[cipa.data$year==yy & 
                             cipa.data$plot==pp &
                             cipa.data$exclude=="F",]
    # Create new column that is empty
    plot.data$survey.yrID <- NULL
    # how many visits in that plot in that year?
    n.visits <- nrow(plot.data)
    if(n.visits!=0){
      for(nn in 1:n.visits){
        survey.ID <- survey.letters[nn]
        temp.surveyyrid <- paste0(yy,survey.ID)
        plot.data$survey.yrID[nn] <- temp.surveyyrid
      }
    }else{
      print(paste0("plot ",pp," year", yy))
    }
    cipa.long <- rbind(cipa.long, plot.data)
  }
}
table(cipa.long$survey.yrID)

#' Set exclude = T for fields that had insufficient surveys (2015D, 2015E, 2015F, 2016D)
cipa.long$exclude <- ifelse(cipa.long$survey.yrID=="2015D" | cipa.long$survey.yrID=="2015E"
                            | cipa.long$survey.yrID=="2015F" | cipa.long$survey.yrID=="2016D", "T",
                            cipa.long$exclude)

# Date range for cipa surveys
summary(cipa.long$date)
#
#'
#'
#'
#'
#'
#'

#' _____________________________________________________________________________
#' ## Leonards Skippers (HELE)
#' 
#' Leonard skippers only surveyed in August and September when it was not raining
hele.data <- survey.data[survey.data$month>=8&
                           survey.data$month<=9,]

#' Remove 2014 data because liatris data was not collected that year
hele.data <- hele.data[hele.data$year!=2014,]

#' Everything is stored as character because of the Xs, so remove 
#' Xs and convert to numeric
hele.data$hele_1 <- ifelse(hele.data$hele_1=="x" | hele.data$hele_1=="X", NA,
                           hele.data$hele_1)

hele.data$hele_2 <- ifelse(hele.data$hele_2=="x" | hele.data$hele_2=="X", NA,
                           hele.data$hele_2)    

hele.data$hele_3 <- ifelse(hele.data$hele_3=="x" | hele.data$hele_3=="X", NA,
                           hele.data$hele_3)


#' Add column to exclude surveys with NA in all three survey periods
hele.data$exclude <- ifelse(is.na(hele.data$hele_1) & is.na(hele.data$hele_2) &
                              is.na(hele.data$hele_3), yes = "T", no =  "F")

#' Fix wonky value ',0'  in field hele_1. Should be 0
hele.data$hele_1 <- ifelse(hele.data$hele_1==",0", yes="0",no = hele.data$hele_1)

#' There are some issues with 'NA' values and other data entry problems
#' For full details, see my published works
#' or "corrections to HELE searches.csv" vol 7

#' Plot 10 only had 1 survey in 2015, and all the liatris values were NA. Exclude it.
#' 
hele.data <- hele.data[hele.data$id !=982,]

#' Survey for plot 11 on 2016-08-18 was 'practice.' Exclude it.
#'
hele.data <- hele.data[hele.data$id !=1212,]











# Also, remove columns no longer needed
hele.data <- hele.data[,c("id", "area", "date", "year",
                          "plot", "hele_1", "hele_2", "hele_3",
                          "liatris1_num", "liatris2_num", "liatris3_num",
                          "survey_effort","hr.rise","obs_skill", "doy",
                          "HOURLYWindSpeed"
                          )]

#' **Reshape data from "long-format" to "wide-format"**
#' 
survey.letters <- c("A", "B", "C", "D", "E", "F", "G")
hele.long <- NULL

for(yy in 2015:2016){       # cycle through years
  for(pp in c(1:43,45:60)){ # and cycle through plots
    # Extract out only survey data for that year yy and plot pp
    plot.data <- hele.data[hele.data$year==yy & 
                             hele.data$plot==pp,]
    # Create new column that is empty
    plot.data$survey.yrID <- NULL
    # how many visits in that plot in that year?
    n.visits <- nrow(plot.data)
    if(n.visits!=0){
      for(nn in 1:n.visits){
        survey.ID <- survey.letters[nn]
        temp.surveyyrid <- paste0(yy,survey.ID)
        plot.data$survey.yrID[nn] <- temp.surveyyrid
      }
    }else{
      print(paste0("plot ",pp,", year ", yy))
    }
    hele.long <- rbind(hele.long, plot.data)
  }
}
table(hele.long$survey.yrID)

# Date range for hele surveys
summary(hele.long$date)

#' **Reshape**
#'
cipa.wide <- reshape(data = cipa.long, 
                     timevar="survey.yrID", 
                     idvar = "plot", 
                     direction = "wide")
hele.wide <- reshape(data = hele.long, 
                     timevar="survey.yrID", 
                     idvar = "plot", 
                     direction = "wide")





#' _____________________________________________________________________________
#' ## Calculate average liatris for HELE data
for(rr in 1:nrow(hele.wide)){
  hele.wide$avg_liatris[rr] <- 
    mean(as.numeric(hele.wide[rr,grep(pattern = "liatris",x =colnames(hele.wide))]),
         na.rm = T)
  
}

#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' _____________________________________________________________________________
#' ## Get data ready for analysis
#' 
#' ### Tiger beetles data formatted for analysis
#' 
#' Extract columns of survey results
colnames <- colnames(cipa.wide)
(colnames <- colnames[grep("cipa_", x = colnames)])
cipa.abnd <- cipa.wide[,c("plot",colnames)]
summary(cipa.abnd)

#' Everything is stored as character because of the Xs, so 
#' convert to numeric
for(cc in 1:ncol(cipa.abnd)){
  cipa.abnd[,cc] <- as.numeric(cipa.abnd[,cc])
}

#' Convert to binary
cipa.occ <- cipa.abnd
for(cc in 2:ncol(cipa.occ)){
  for(rr in 1:nrow(cipa.occ)){
    cipa.occ[rr,cc] <- ifelse(cipa.abnd[rr,cc]>=1, 
                              yes = 1, 
                              no = ifelse(cipa.abnd[rr,cc]==0,
                                          yes = 0,
                                          no = cipa.abnd[rr,cc]))
  }
}


#' Create a version of cipa.abnd that has only the targeted survey dates
cipa.abnd <- subset(cipa.abnd, 
                    select = c("plot", "cipa_1.2014A","cipa_2.2014A","cipa_3.2014A",
                               "cipa_1.2014B","cipa_2.2014B","cipa_3.2014B",
                               "cipa_1.2015A","cipa_2.2015A","cipa_3.2015A",
                               "cipa_1.2015B","cipa_2.2015B","cipa_3.2015B",
                               "cipa_1.2015C","cipa_2.2015C","cipa_3.2015C",
                               "cipa_1.2016A","cipa_2.2016A","cipa_3.2016A",
                               "cipa_1.2016B","cipa_2.2016B","cipa_3.2016B",
                               "cipa_1.2016C","cipa_2.2016C","cipa_3.2016C"))
colnames(cipa.abnd)

#' Create 'clean' version of cipa.wide to make life easier for JAGS
#' 
colnames(cipa.wide)

cipa.wide.clean<- subset(cipa.wide,
                         select = c("plot", "cipa_1.2014A","cipa_2.2014A","cipa_3.2014A",
                                    "cipa_1.2014B","cipa_2.2014B","cipa_3.2014B",
                                    "cipa_1.2015A","cipa_2.2015A","cipa_3.2015A",
                                    "cipa_1.2015B","cipa_2.2015B","cipa_3.2015B",
                                    "cipa_1.2015C","cipa_2.2015C","cipa_3.2015C",
                                    "cipa_1.2016A","cipa_2.2016A","cipa_3.2016A",
                                    "cipa_1.2016B","cipa_2.2016B","cipa_3.2016B",
                                    "cipa_1.2016C","cipa_2.2016C","cipa_3.2016C",
                                    "obs_skill.2014A","obs_skill.2014B","obs_skill.2015A",
                                    "obs_skill.2015B","obs_skill.2015C","obs_skill.2016A",
                                    "obs_skill.2016B","obs_skill.2016C",
                                    "doy.2014A","doy.2014B","doy.2015A","doy.2015B",
                                    "doy.2015C","doy.2016A","doy.2016B","doy.2016C",
                                    "hr.rise.2014A", "hr.rise.2014B","hr.rise.2015A",
                                    "hr.rise.2015B", "hr.rise.2015C","hr.rise.2016A",
                                    "hr.rise.2016B", "hr.rise.2016C",
                                    "HOURLYDRYBULBTEMPC.2014A", 
                                    "HOURLYDRYBULBTEMPC.2014B",
                                    "HOURLYDRYBULBTEMPC.2015A",
                                    "HOURLYDRYBULBTEMPC.2015B", 
                                    "HOURLYDRYBULBTEMPC.2015C",
                                    "HOURLYDRYBULBTEMPC.2016A",
                                    "HOURLYDRYBULBTEMPC.2016B", 
                                    "HOURLYDRYBULBTEMPC.2016C",
                                    "HOURLYWindSpeed.2014A", "HOURLYWindSpeed.2014B",
                                    "HOURLYWindSpeed.2015A",
                                    "HOURLYWindSpeed.2015B", "HOURLYWindSpeed.2015C",
                                    "HOURLYWindSpeed.2016A",
                                    "HOURLYWindSpeed.2016B", "HOURLYWindSpeed.2016C"
                                    #'"exclude.2014A","exclude.2014B","exclude.2015A",
                                    #'"exclude.2015B","exclude.2015C","exclude.2016A",
                                    #'"exclude.2016B","exclude.2016C"
                                    ))
#'
#'
#'
#' ### Skipper data formatted for analysis
#' 
#' Extract columns of survey results
(colnames <- colnames(hele.wide))
(colnames <- colnames[grep("hele_", x = colnames)])
hele.abnd <- hele.wide[,c("plot",colnames)]
str(hele.abnd)

#' Everything is stored as character because of the Xs, so
#' convert to numeric
for(cc in 1:ncol(hele.abnd)){
  hele.abnd[,cc] <- as.numeric(hele.abnd[,cc])
}
str(hele.abnd)

#' Convert to binary
hele.occ <- hele.abnd
for(cc in 2:ncol(hele.occ)){
  for(rr in 1:nrow(hele.occ)){
    hele.occ[rr,cc] <- ifelse(hele.abnd[rr,cc]>=1, 
                              yes = 1, 
                              no = ifelse(hele.abnd[rr,cc]==0,
                                          yes = 0,
                                          no = hele.abnd[rr,cc]))
  }
}

#' Extract columns of liatris
colnames <- colnames(hele.wide)
(colnames <- colnames[grep("liatris", x = colnames)])
#' Remove avg_liatris 
colnames <- colnames[colnames!="avg_liatris"]
liatris_obslvl <- hele.wide[,c("plot",colnames)]

#' _____________________________________________________________________________
#' Clean up data for JAGS (this used to be in cipa_jags.R)
#' 
#' CIPA
#' 
#' First, which columns are we querying for each year?
colnames2014 <- colnames(cipa.wide.clean[grepl("^cipa.*2014", colnames(cipa.wide.clean))])
colnames2015 <- colnames(cipa.wide.clean[grepl("^cipa.*2015", colnames(cipa.wide.clean))])
colnames2016 <- colnames(cipa.wide.clean[grepl("^cipa.*2016", colnames(cipa.wide.clean))])

#' Now, sum up non-NA records of those columns
cipa.wide.clean$surv_ct14 <- rowSums(!is.na(cipa.wide.clean[,colnames2014]))
cipa.wide.clean$surv_ct15 <- rowSums(!is.na(cipa.wide.clean[,colnames2015]))
cipa.wide.clean$surv_ct16 <- rowSums(!is.na(cipa.wide.clean[,colnames2016]))

#' Check counts, make sure they look correct
cipa.subset <- subset(cipa.wide.clean,select=c("cipa_1.2014A","cipa_2.2014A","cipa_3.2014A",
                                          "cipa_1.2014B","cipa_2.2014B","cipa_3.2014B",
                                          "surv_ct14",
                                          "cipa_1.2015A", "cipa_2.2015A", "cipa_3.2015A",
                                          "cipa_1.2015B", "cipa_2.2015B", "cipa_3.2015B",
                                          "cipa_1.2015C", "cipa_2.2015C", "cipa_3.2015C",
                                          "surv_ct15",
                                          "cipa_1.2016A", "cipa_2.2016A", "cipa_3.2016A",
                                          "cipa_1.2016B", "cipa_2.2016B", "cipa_3.2016B",
                                          "cipa_1.2016C", "cipa_2.2016C", "cipa_3.2016C",
                                          "surv_ct16"))



#' Load plot-level data
plot.data <- read.csv("data/processed_data/plot_level_data.csv", row.names = 1)

#' Subset plot data, eliminating fields that we don't want
subset.plot <- subset(plot.data, select = c("plot","area","pct_canopy","num_woody_stems",
                                            "pct_bunchgrass", "num_mounds", "pct_bluestem",
                                            "num_blowouts", "num_liatris_stems",
                                            "num_open_oaks","num_milkweed_stems","litter_depth",
                                            "pct_nonbunchgrass","tot_grass","avg_liatris",
                                            "totgrass.z","bunchgrass.z","oaks.z","canopy.z",
                                            "numwood.z","mounds.z","blowouts.z","plotlvl_liatris.z",
                                            "plotlvl_milkweed.z","litter.z","DY_pre2015",
                                            "DY_ge2015","DY_pre2014","DY_ge2014","UDY_ge2015",
                                            "burned_pre2015","harvest_pre2015","type_pre2015",
                                            "burnGraze_ge2015","harvest_ge2015","type_ge2015",
                                            "elev_CV.z"))


#' ## Merge plot-level data with all.data
cipa.merged.data <- merge(cipa.wide.clean, subset.plot, by = "plot")
#'
#' HELE
#'
hele.wide.clean <- subset(hele.wide,
                          select = c("plot",
                                     "hele_1.2015A","hele_2.2015A","hele_3.2015A",
                                     "hele_1.2015B","hele_2.2015B","hele_3.2015B",
                                     "hele_1.2016A","hele_2.2016A","hele_3.2016A",
                                     "hele_1.2016B","hele_2.2016B","hele_3.2016B",
                                     "doy.2015A","doy.2015B",
                                     "doy.2016A","doy.2016B",
                                     "liatris1_num.2015A","liatris2_num.2015A","liatris3_num.2015A",
                                     "liatris1_num.2015B","liatris2_num.2015B","liatris3_num.2015B",
                                     "liatris1_num.2016A","liatris2_num.2016A","liatris3_num.2016A",
                                     "liatris1_num.2016B","liatris2_num.2016B","liatris3_num.2016B"))

#' Next, which columns are we querying for each year?
colnames2015 <- colnames(hele.wide.clean[grepl("^hele.*2015", colnames(hele.wide.clean))])
colnames2016 <- colnames(hele.wide.clean[grepl("^hele.*2016", colnames(hele.wide.clean))])

#' Now, sum up non-NA records of those columns
hele.wide.clean$surv_ct15 <- rowSums(!is.na(hele.wide.clean[,colnames2015]))
hele.wide.clean$surv_ct16 <- rowSums(!is.na(hele.wide.clean[,colnames2016]))

#' Subset plot data for HELE JAGS
subset.plot <- subset(plot.data, select = c("plot",
                                            "totgrass.z","bunchgrass.z","oaks.z","canopy.z",
                                            "numwood.z","mounds.z","blowouts.z","plotlvl_liatris.z",
                                            "plotlvl_milkweed.z","litter.z",
                                            "DY_pre2015",
                                            "DY_ge2015","DY_pre2014","DY_ge2014","UDY_ge2015",
                                            "burned_pre2015","harvest_pre2015","type_pre2015",
                                            "burnGraze_ge2015","harvest_ge2015","type_ge2015",
                                            "elev_CV.z"))


#' ## Merge plot-level data with all.data
hele.merged.data <- merge(hele.wide.clean, subset.plot, by = "plot")

#'
#'
#'
#'
#' _____________________________________________________________________________
#' ## Save files
#' 
#' Full, processed survey data
write.csv(x = survey.data, "data/processed_data/survey_data_full.csv")

write.csv(x = hele.occ, "data/processed_data/hele_occupancy_data.csv")
write.csv(x = hele.abnd, "data/processed_data/hele_abundance_data.csv")
write.csv(x = liatris_obslvl, "data/processed_data/hele_liatris_obslvl.csv")
write.csv(x = hele.wide, "data/processed_data/hele_wide_format.csv")
write.csv(x = hele.long, "data/processed_data/hele_long_format.csv")
write.csv(x = cipa.occ, "data/processed_data/cipa_occupancy_data.csv")
write.csv(x = cipa.abnd, "data/processed_data/cipa_abundance_data.csv")
write.csv(x = cipa.wide, "data/processed_data/cipa_wide_format.csv")
write.csv(x = cipa.long, "data/processed_data/cipa_long_format.csv")
write.csv(x = cipa.wide.clean, "data/processed_data/cipa_wide_format_clean.csv")

#' Export csv of data in JAGS format 
write.csv(x = cipa.merged.data, "data/processed_data/cipa_wide_JAGS.csv")
write.csv(x = hele.merged.data, "data/processed_data/hele_wide_JAGS.csv")

#' Save for the plot-level processing file
avg_liatris <- hele.wide[,c("plot","avg_liatris")]
save(avg_liatris,file = "data/processed_data/avg_liatris.Rdata")

#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/01_processing_invert_data.R", out_dir = "output/thesis_results_spun", fig_dir = "figures", keep_md = F)
#' 