rm(list=ls())
setwd('/Users/fabianyii/Desktop/UKM.RA/UKM/dims')
install.packages('dplyr')
library(dplyr)

############ SORTING OUT 2018 DATA #################
d18 <- read.csv('r.data/2018.csv')
d18 <- d18[,-8] #remove redundant column
d18 <- d18[-which(d18$race != 'M'),] #exclude non-malays
d18 <- d18[-c(1,2,9,15,17,18,20,21,22,23,24,26,35,37,38,39,41,43,49,50,51,56,66,
          67,68,70,75,77,82,83,87,88,96,100,101,103,104,110,111,112,122,123,
          127,131,132,134,135,140,141,142,143,144,153,160,162,163,168,172,173,174,
          182,187,188,190,191,192,199,201,202,203,204,206,209,210,213,214,215,218,219,
          221,225,226,239,240,243,245,252,253,254,255,256,258,260,269,272,276,277,279,280,
          286,289,290,292,294,296,297,298,299,300,301,302,303,304,312,313,322,323,324,325,326,
          327,328,329,332,337,339,343,346,347,348,350,365,366,367,374,376,377,378,386,391,419,420,421,
          32,34,198,379,33,34,92,94,95,185,195,205,334,402),]
############ FINISHED SORTING OUT 2018 DATA #################


############ SORTING OUT 2019 DATA #################
d19 <- read.csv('r.data/2019.csv')
d19 <- d19[-which(d19$race != 'M'),] #exclude non-malays

#use str_detect() to find if a string contains certains characters (e.g. amblyopia, tropia)
d19 <-d19[str_detect (d19$diag, "amblyopia", negate=TRUE),] #exclude amblyopes
d19 <-d19[str_detect (d19$diag, "trop", negate=TRUE),] #exclude px with any forms of tropia
############ FINISHED SORTING OUT 2019 DATA #################



############ SORTING OUT 2020 DATA #################
d20 <- read.csv('r.data/2020.csv')
d20 <- d20[-which(d20$race != 'M'),] #exclude non-malays

#use str_detect() to find if a string contains certains characters (e.g. amblyopia, tropia)
d20 <-d20[str_detect (d20$diag, "ambly", negate=TRUE),] #exclude amblyopes
d20 <-d20[str_detect (d20$diag, "AMBLY", negate=TRUE),] #exclude amblyopes
d20 <-d20[str_detect (d20$diag, "trop", negate=TRUE),] #exclude px with any forms of tropia

d20 <- d20[-c(10,102,177),] # cleaning remaining dataset
############ FINISHED SORTING OUT 2020 DATA #################


#### COMBINE all three cleaned datasets ###
d18$age = NA
d18 <- d18[,c(1:6,8,7)] ## add a dummy age column to d18 so that it matches other datasets

d <- rbind(d18,d19,d20) # combine all three datasets

d[duplicated(d$ic),1:7] # display duplicate px

for(i in d[duplicated(d$ic),1:7]$ic) {
    d <- d[-which(d$ic == i)[1],] }     #### REMOVE duplicate px!!

## isolate the last three digits of IC number
d$ic <- substr(d$ic, 12,14)

## exclude all other columns but px name and last three digits of IC number
dat <- data.frame(px=d$px, ic=d$ic)

#order IC from small to large
dat$ic <- as.numeric(dat$ic)
dat <- dat[order(dat$ic),]

#export dataframe as excel
write.csv(dat, "ProspectiveSubjects.csv")












