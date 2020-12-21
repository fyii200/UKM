rm(list=ls())
setwd('/Users/fabianyii/Desktop/UKM RA/dims')
d <- read.csv('data/haize.fatin.csv')
d$assessment.date <- as.Date(d$assessment.date, '%d/%m/%y')

### set up TWILLIO
# install.packages("twilio")
# library(twilio)
# Sys.setenv(TWILIO_SID = "ACdb7f6fcc7719f83a8fb008a4aa71e690")
# Sys.setenv(TWILIO_TOKEN = "db92d3065f4f14f66b7548f1263be167")

# tw_send_message(to= '+601125164569', from= '+12518108929', body=paste('Trying this out') ) ## DONT ACTIVATE THIS LINE

# nrow(data.frame(unique(d$px)))

for (i in 1:1 ) {
a <- d[which(d$px == unique(d$px)[i]),c(1,3,5,78)]
if( a[nrow (a[which(is.na(a$assessment.date)=='FALSE'),]),]$visit != 'MRI') {
  
  if(a[nrow (a[which(is.na(a$assessment.date)=='FALSE'),]),]$visit == 1) 
  {print(paste0('Contact ',a$px[1], ' at ', a$contact.no[1], ' for MRI') )
    tw_send_message(to= '+601125164569', from= '+12518108929', 
                    body=paste0('Dear ',a$px[1],', ','This is just to remind you that your next visit (MRI) is due soon. Kindly contact us to make an appointment')  ) }
    
  if(a[nrow (a[which(is.na(a$assessment.date)=='FALSE'),]),]$visit == 2) {
    if(as.numeric(as.Date('21/12/2020', '%d/%m/%y') - a[nrow (a[which(is.na(a$assessment.date)=='FALSE'),]),]$assessment.date)/30 >= 1.75) #1 week before due
    { print(paste0('Contact ',a$px[1], ' at ', a$contact.no[1], ' for 3-month follow-up (due in 1 week)') ) 
      tw_send_message(to= '+601125164569', from= '+12518108929', 
                      body=paste0('Dear ',a$px[1],', ','This is just to remind you that your next visit (3-month follow-up) is due in 1 week. Kindly contact us to make an appointment')  ) }    
  }
    
  if(a[nrow (a[which(is.na(a$assessment.date)=='FALSE'),]),]$visit == 3) {
    if(as.numeric(as.Date('21/12/2020', '%d/%m/%y') - a[nrow (a[which(is.na(a$assessment.date)=='FALSE'),]),]$assessment.date)/30 >= 2.75) #contact 1 week before due
    { print(paste0('Contact ',a$px[1], ' at ', a$contact.no[1], ' for 6-month follow-up (due in 1 week)') ) 
      tw_send_message(to= '+601125164569', from= '+12518108929', 
                      body=paste0('Dear ',a$px[1],', ','This is just to remind you that your next visit (6-month follow-up) is due in 1 week. Kindly contact us to make an appointment')  ) }    
  }
    
  if(a[nrow (a[which(is.na(a$assessment.date)=='FALSE'),]),]$visit == 4) {
    if(as.numeric(as.Date('21/12/2020', '%d/%m/%y') - a[nrow (a[which(is.na(a$assessment.date)=='FALSE'),]),]$assessment.date)/30 >= 5.75) # contact 1 week before due
    { print(paste0('Contact ',a$px[1], ' at ', a$contact.no[1], ' for 12-month follow-up (due in 1 week)') ) 
      tw_send_message(to= '+601125164569', from= '+12518108929', 
                      body=paste0('Dear ',a$px[1],', ','This is just to remind you that your next visit (12-month follow-up) is due in 1 week. Kindly contact us to make an appointment')  ) }    
  }
    
  if(a[nrow (a[which(is.na(a$assessment.date)=='FALSE'),]),]$visit == 5) {
    if(as.numeric(as.Date('21/12/2020', '%d/%m/%y') - a[nrow (a[which(is.na(a$assessment.date)=='FALSE'),]),]$assessment.date)/30 >= 5.75) #contact 1 week before due
    { print(paste0('Contact ',a$px[1], ' at ', a$contact.no[1], ' for 18-month follow-up (due in 1 week)') ) 
      tw_send_message(to= '+601125164569', from= '+12518108929', 
                      body=paste0('Dear ',a$px[1],', ','This is just to remind you that your next visit (18-month follow-up) is due in 1 week. Kindly contact us to make an appointment')  ) }    
  }
    
  if(a[nrow (a[which(is.na(a$assessment.date)=='FALSE'),]),]$visit == 6)
    { print(paste0(a$px[1], ' has attanded all measurement sessions; no need to contact') ) }
} 
else{ if(as.numeric(as.Date('21/12/2020', '%d/%m/%y') - a[which(a$visit==1),]$assessment.date)/30 >= 0.75) 
{print(paste0('Contact ',a$px[1], ' at ', a$contact.no[1], ' for 1-month follow-up') )
  tw_send_message(to= '+601125164569', from= '+12518108929', 
                  body=paste0('Dear ',a$px[1],', ','This is just to remind you that your next visit (1-month follow-up) is due in 1 week. Kindly contact us to make an appointment')  )} }
}


