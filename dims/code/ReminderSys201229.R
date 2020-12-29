rm(list=ls())
setwd('/Users/fabianyii/Desktop/UKM.RA/UKM/dims')
d <- read.csv('data/haize.fatin.csv')
for(i in 3:10) { d[,i] <- as.Date(d[,i], '%d/%m/%y') }

# install.packages('blastula')
# install.packages('keyring')
require(lubridate)
library("blastula")
library(keyring)

mes <- data.frame(id=1:100,mes='mes')


for (i in 1:nrow(data.frame(unique(d$px))) ) {
  a <- d[which(d$px == unique(d$px)[i]),]
  if( names(a) [length(which(is.na(a)=='FALSE'))] != 'd2') {
    if(names(a) [length(which(is.na(a)=='FALSE'))] == 'd1') 
    {mes$mes[i] <- print(paste0('PLEASE contact ',a$px, ' at 0', a$contact, ' for the first MRI (due before 1-month follow-up on ', 
                                ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(1), ')' ) )
    mes$id[i] <- a$px}
    
    if( names(a) [length(which(is.na(a)=='FALSE'))] == 'd3') {
      if(as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 >= 1.75) #1 week before due
      {mes$mes[i] <- print(paste0('PLEASE contact ',a$px, ' at 0', a$contact, ' for 3-month follow-up (due on ', 
                                  ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(2), ')' ) )
      mes$id[i] <- a$px }
      else {if(as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 >= 1.50 &
               as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 < 1.75) #2 week before due
      {mes$mes[i] <- print(paste0(a$px, "'s 3-month follow-up will be due in 2 weeks (on ", 
                                  ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(2), ')' ) )
      mes$id[i] <- a$px}
        
        else {mes$mes[i] <- print(paste0('RELAX! No need to contact ',a$px, ' now; ', '3-month follow-up is only due on ', 
                                         ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(2) ) ) 
        mes$id[i] <- a$px} }   
    }
    
    if( names(a) [length(which(is.na(a)=='FALSE'))] == 'd4') {
      if(as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 >= 2.75) #1 week before due
      {mes$mes[i] <- print(paste0('PLEASE contact ',a$px, ' at 0', a$contact, ' for 6-month follow-up (due on ', 
                                  ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(3), ')' ) ) 
      mes$id[i] <- a$px }
      else {if(as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 >= 2.50 &
               as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 < 2.75) #2 week before due
      {mes$mes[i] <- print(paste0(a$px, "'s 6-month follow-up will be due in 2 weeks (on ", 
                                  ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(3), ')' ) )
      mes$id[i] <- a$px}
        
        else {mes$mes[i] <- print(paste0('RELAX! No need to contact ',a$px, ' now; ', '6-month follow-up is only due on ', 
                                         ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(3) ) ) 
        mes$id[i] <- a$px} }   
    }
    
    if( names(a) [length(which(is.na(a)=='FALSE'))] == 'd5') {
      if(as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 >= 5.75) #1 week before due
      {mes$mes[i] <- print(paste0('PLEASE contact ',a$px, ' at 0', a$contact, ' for 12-month follow-up (due on ', 
                                  ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(6), ')' ) ) 
      mes$id[i] <- a$px }
      else {if(as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 >= 5.50 &
               as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 < 5.75) #2 week before due
      {mes$mes[i] <- print(paste0(a$px, "'s 12-month follow-up will be due in 2 weeks (on ", 
                                  ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(6), ')' ) )
      mes$id[i] <- a$px} 
        
        else {mes$mes[i] <- print(paste0('RELAX! No need to contact ',a$px, ' now; ', '12-month follow-up is only due on ', 
                                         ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(6) ) ) 
        mes$id[i] <- a$px} }    
    }
    
    if( names(a) [length(which(is.na(a)=='FALSE'))] == 'd6') {
      if(as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 >= 5.75) #1 week before due
      {mes$mes[i] <- print(paste0('PLEASE contact ',a$px, ' at 0', a$contact, ' for 18-month follow-up (due on ', 
                                  ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(6), ')' ) ) 
      mes$id[i] <- a$px }
      else {if(as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 >= 5.50 &
               as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 < 5.75) #2 week before due
      {mes$mes[i] <- print(paste0(a$px, "'s 18-month follow-up will be due in 2 weeks (on ", 
                                  ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(6), ')' ) )
      mes$id[i] <- a$px }
        
        else {mes$mes[i] <- print(paste0('RELAX! No need to contact ',a$px, ' now; ', '18-month follow-up is only due on ', 
                                         ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(6) ) ) 
        mes$id[i] <- a$px}    } 
    }
    
    if( names(a) [length(which(is.na(a)=='FALSE'))] == 'd7')
    {mes$mes[i] <- print(paste0('PLEASE contact ',a$px, ' at 0', a$contact, ' for the last MRI (px attended their 18-month follow-up on ', 
                                a[,length(which(is.na(a)=='FALSE'))], ')' ) )
    mes$id[i] <- a$px
    }
    
    
    if( names(a) [length(which(is.na(a)=='FALSE'))] == 'd8')
    {mes$mes[i] <- print(paste0('GOOD NEWS! ' ,a$px, ' has attanded all measurement sessions; no need to contact') ) 
    mes$id[i] <- a$px}
  }
  
  else{ if(as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 >= 0.75) #1 week before due
  {mes$mes[i] <- print(paste0('PLEASE contact ',a$px, ' at 0', a$contact, ' for 1-month follow-up') )
  mes$id[i] <- a$px }
    
    else {if(as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 >= 0.50 &
             as.numeric(Sys.Date( ) - a[,length(which(is.na(a)=='FALSE'))])/30 < 0.75) #2 week before due
    {mes$mes[i] <- print(paste0(a$px, "'s 1-month follow-up will be due in 2 weeks (on ", 
                                ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(1), ')' ) )
    mes$id[i] <- a$px}
      
      else {mes$mes[i] <- print(paste0('RELAX! No need to contact ',a$px, ' now; ', '1-month follow-up is only due on ', 
                                       ymd(a[,length(which(is.na(a)=='FALSE'))]) %m+% months(2) ) ) 
      mes$id[i] <- a$px} }    
  }
}

mes <- mes[which(mes$mes != 'mes'),] 
# as.character(cat(paste0(mes[1:length(mes)],sep="\n")) )

create_smtp_creds_key(
  id = "gmail",
  user = "kajianrabunjauh@gmail.com",
  host = "kajianrabunjauh.gmail.com",
  port = 465,
  use_ssl = TRUE,
  overwrite=TRUE)

for (i in 1:nrow(mes)){
  email <- compose_email(body = mes[i,2],
                         footer = 'Weekly Reminder')
  
  smtp_send(
    email,
    from = "kajianrabunjauh@gmail.com",
    to = "kajianrabunjauh@gmail.com",
    subject = toupper(mes$id[i]),
    credentials = creds(
      user = "kajianrabunjauh@gmail.com",
      provider = "gmail") )
}