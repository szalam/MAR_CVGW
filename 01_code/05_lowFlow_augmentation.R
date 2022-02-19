#calculate percentage low flow augmentation
#The code is written by sarfaraz alam. email: szalam@ucla.edu


rm(list=ls())

library(readxl)
library(writexl)
library(lubridate)
library(ggplot2)
library(reshape2)

wd=list()
wd$data='Add data directory here'
wd$output='add output directory here'

setwd(wd$data)

#function
summ_calc=function(data_list_gwbud,subreg){
  tmp=read_excel(data_list_gwbud,sheet=paste0('Sheet',subreg))
  df.tmp=data.frame(df.dt,tmp[,6])[109:nrow(df.dt),]
  
  
  #only low flow months (July, aug, sept)
  df.tmp=df.tmp[which(df.tmp[,2]==7 | df.tmp[,2]==8 | df.tmp[,2]==9),]
  
  
  #Input needed below
  #***************************************
  #comment out the lines 37 and 40 if interested to know low flow change for the entire
  #study period. Otherwise comment out the drought years below to get low flow change
  #for those years
  
  # 2007-2009 drought
  # df.tmp=df.tmp[which(df.tmp[,1]==2007 | df.tmp[,1]==2008 | df.tmp[,1]==2009),]
  
  # 2012-2015 drought
  # df.tmp=df.tmp[which(df.tmp[,1]==2012 | df.tmp[,1]==2013 | df.tmp[,1]==2014 |
  #                       df.tmp[,1]==2015),]
  #***************************************
  
  #unit conversion from acre-ft to km3
  strm2gw=sum(df.tmp[,3])/810714.402*(-1) 
  return(strm2gw)
}


#intput. Start row number which corresponds to 10/31/1959
selec_strt_date=109

#month years
dt.mn=c(10:12,rep(1:12,times=64),1:9)
dt.yr=c(rep(1950,times=3),rep(1951:2014,each=12),rep(2015, times=9))
df.dt=data.frame(dt.yr,dt.mn)

#intiate variables to be used
gw2strm.diff=gw2strm.diff1=gw2strm.diff2=t1_diff=t2_diff=t1_abs=t2_abs=0

for(subreg in 1:22)
{
  data_list_gwbud=c('CVground_C2VSimFG_base_1950_2015.xlsx','CVground_C2VSimFG_90th_N2M_monthly_elemEMG_distCalc_upLim2ft_fracdl01_2ftLim_fixedThreshold.xlsx','CVground_C2VSimFG_80th_N2M_monthly_elemEMG_distCalc_upLim2ft_fracdl01_2ftLim_fixedThreshold.xlsx')
  
  #percent change
  gw2strm.diff1=c(gw2strm.diff1,
                 (summ_calc(data_list_gwbud[[2]],subreg)-
                                 summ_calc(data_list_gwbud[[1]],subreg))*100/
                  abs(summ_calc(data_list_gwbud[[1]],subreg)))
  
  gw2strm.diff2=c(gw2strm.diff2,
                 (summ_calc(data_list_gwbud[[3]],subreg)-
                    summ_calc(data_list_gwbud[[1]],subreg))*100/
                   abs(summ_calc(data_list_gwbud[[1]],subreg)))
  
 
  #storing values for regionwide calculation
  t1_diff=c(t1_diff,
                  (summ_calc(data_list_gwbud[[2]],subreg)-
                     summ_calc(data_list_gwbud[[1]],subreg)))
  
  t1_abs=c(t1_abs,abs(summ_calc(data_list_gwbud[[1]],subreg)))
  
  t2_diff=c(t2_diff,
                  (summ_calc(data_list_gwbud[[3]],subreg)-
                     summ_calc(data_list_gwbud[[1]],subreg)))
  
  t2_abs=c(t2_abs,abs(summ_calc(data_list_gwbud[[1]],subreg)))
  
}

t1_abs=t1_abs[-1]
t2_abs=t2_abs[-1]
t1_diff=t1_diff[-1]
t2_diff=t2_diff[-1]

#calculating percentage
#**************************************************
sc_per_ch90=sum(t1_diff[1:7])*100/sum(t1_abs[1:7])
sc_per_ch80=sum(t2_diff[1:7])*100/sum(t2_abs[1:7])

ez_per_ch90=sum(t1_diff[8])*100/sum(t1_abs[8])
ez_per_ch80=sum(t2_diff[8])*100/sum(t2_abs[8])

sc_ez_per_ch90=sum(t1_diff[1:8])*100/sum(t1_abs[1:8])
sc_ez_per_ch80=sum(t2_diff[1:8])*100/sum(t2_abs[1:8])

sj_per_ch90=sum(t1_diff[10:13])*100/sum(t1_abs[10:13])
sj_per_ch80=sum(t2_diff[10:13])*100/sum(t2_abs[10:13])

tl_per_ch90=sum(t1_diff[14:21])*100/sum(t1_abs[14:21])
tl_per_ch80=sum(t2_diff[14:21])*100/sum(t2_abs[14:21])

cv_per_ch90=sum(t1_diff[22])*100/sum(t1_abs[22])
cv_per_ch80=sum(t2_diff[22])*100/sum(t2_abs[22])
#**************************************************

#creating dataframe with variables created 
tmp=data.frame(HR=c('SC_EZ','SJ','TL','CV'),R90_2ft=c(sc_ez_per_ch90,sj_per_ch90,tl_per_ch90,cv_per_ch90),
               R80_2ft=c(sc_ez_per_ch80,sj_per_ch80,tl_per_ch80,cv_per_ch80))

#melting to prepare for ggplot
mdat=melt(tmp,id.vars = c('HR'))

#adding region names
mdat[,1]=factor(mdat[,1],levels = c('SC_EZ','SJ','TL','CV'))

#plot using ggplot 
p=ggplot(mdat,aes(x=HR,
                  y=value,fill=variable))+
  geom_bar(position="dodge", stat="identity", color='black')+theme_bw()+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  # ylab(expression(paste('Change in baseflow (',km^{3},')')))+ xlab('Subregions')+
  ylab(expression(paste('Change in baseflow (%)')))+ xlab(' ')+
  # scale_x_discrete(limits = as.character(1:22))+
  scale_fill_manual(values=c("lightseagreen",'yellow3'),
                    name="Scenarios")
  # ylim(c(0,100))#

p

