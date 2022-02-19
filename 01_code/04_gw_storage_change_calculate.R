#This code calculates the groundwater storage change (%) for each subregion
#The code is written by sarfaraz alam; email: szalam@ucla.edu

rm(list=ls())

#import library
library(readxl)
library(writexl)
library(lubridate)
library(ggplot2)
library(reshape2)

wd=list()
wd$data='**data directory here**'
wd$output='**output directory here**'

#set directory to data directory 
setwd(wd$data)

#data to be imported. The following line includes importing R90_2ft and R80_2ft
#this has to be changed to R90_10ft and R80_10ft to get outputs for these scenarios
#this can be changed to R90_2ft_WT to get output for water transfer scenario
files=c('CVground_R90_2ft.xlsx',
        'CVground_R80_2ft.xlsx',
        'CVground_base.xlsx')

#initiating variables
base=R90=R80=0

#looping through subregions. i is subregion number
for(i in 1:21){
  tmp=read_excel(files[[3]],sheet=paste0('Sheet',i))
  base=cbind(base,tmp[109:nrow(tmp),3]*1.23348e-6) # 1 acre-ft = 1.23348e-6 km3
  
  tmp=read_excel(files[[1]],sheet=paste0('Sheet',i))
  R90=cbind(R90,tmp[109:nrow(tmp),3]*1.23348e-6)
  
  tmp=read_excel(files[[2]],sheet=paste0('Sheet',i))
  R80=cbind(R80,tmp[109:nrow(tmp),3]*1.23348e-6)
}

#importing base file
tmp=read_excel(files[[3]],sheet=paste0('Sheet',i))
tmp=tmp[109:nrow(tmp),1]

base[,1]=R90[,1]=R80[,1]=tmp

#range for which statistics needed
range_consd=c(1:21)

#base gw change
base_gw_change=base[nrow(base),2:ncol(base)]-base[1,2:ncol(base)]
base_all=sum(base_gw_change[range_consd])

#scenario gw change
r90_change=R90[1:nrow(R90),2:ncol(R90)]-base[1:nrow(base),2:ncol(base)]
recover_90=r90_change[nrow(r90_change),]
recover_90_tot=sum(recover_90[range_consd])

r80_change=R80[1:nrow(R80),2:ncol(R80)]-base[1:nrow(base),2:ncol(base)]
recover_80=r80_change[nrow(r80_change),]
recover_80_tot=sum(recover_80[range_consd])

#fraction recovery or GWC (%)
frac_90=recover_90*100/abs(base_gw_change)
# frac_90
frac_90_tot=recover_90_tot*100/abs(base_all)
frac_90_tot

frac_80=recover_80*100/abs(base_gw_change)
# frac_80
frac_80_tot=recover_80_tot*100/abs(base_all)
frac_80_tot

df=data.frame(1:21,t(base_gw_change),t(frac_90),t(frac_80))
colnames(df)=c('sub','base_change','R90_frac_recovery','R80_frac_recovery')

#set directory to output location
setwd(wd$output)

#rename the output file according to input dataset
write.csv(df,'gw_change_for_R90_R80_2ft.csv')
