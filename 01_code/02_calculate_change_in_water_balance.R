# This code calculates average difference in water balance components between MAR scenario
# R90_2ft and base scenario
# The code is written by sarfaraz alam. email: szalam@ucla.edu
#*******************************************************************

# clear work environment
rm(list=ls())

# load library
library(readxl)
library(writexl)
library(lubridate)
library(ggplot2)
library(reshape2)
library(viridis)
library(wesanderson)

#***********************************************
# storing useful directory
wd=list()
#location of all input data
wd$data='add the directory of the data folder'

#output locations
wd$output='add the directory of output folder'
#***********************************************

# row number of the start date. In this case it is 10/31/1969
selec_strt_date=109

#set working directory
setwd(wd$data)

#Importing C2VSIM reach ids used for MAR for each subregion
connec_file=read_excel('sub_reachOut_connections_columnNumber.xlsx')

# number of diversion location for each subregion. Putting temporary 1 for subregion 9 and 14, which don't have diversion location
stm_numberrs=c(1,4,1,1,2,2,1,5,2,1,1,2,4,1,1,1,1,4,1,1,1)

#intial value of variables to be used later
rch.div= div_all_stm_combine=0
mdat_list=list()

#creating function for later use
wat_budget_calc=function(data_list,subreg,type.var,var.colm, sheet,selec_strt_date=selec_strt_date){
  
  #reading and storing data for variables that is diversion and non-diversion
  #****************************************************************************
  if(type.var !='Diversion'){
  base_gw=read_excel(data_list[1],sheet = paste0('Sheet',subreg))
  rech_elemEMG_tmp=read_excel(data_list[2],sheet = paste0('Sheet',subreg))
  }
  
  if(type.var =='Diversion'){
    rech_elemEMG_tmp=read_excel(data_list[2],sheet = paste0('Sheet',subreg))
  }
  
  if(type.var !='Diversion'){
  gw_diff=data.frame(date=selec_strt_date:nrow(base_gw),
                     base_75th_N2M_elemEMG=(rech_elemEMG_tmp[selec_strt_date:nrow(base_gw),var.colm]-base_gw[selec_strt_date:nrow(base_gw),var.colm]))
  }
  if(type.var=='Diversion'){
    gw_diff=data.frame(date=selec_strt_date:nrow(rech_elemEMG_tmp),
                       base_75th_N2M_elemEMG=(rech_elemEMG_tmp[selec_strt_date:nrow(rech_elemEMG_tmp),var.colm]))
  }            
  #*********************************************
  
  #Type
  gw_diff[,1]=type.var

  #calculating sum of all values
  temp_gw_diff=aggregate(gw_diff[,2:ncol(gw_diff)],by=list(gw_diff[,1]),FUN = sum)
  
  #converting unit from acre ft to million cubic meter (MCM)
  temp_gw_diff[1,2]=temp_gw_diff[1,2]*1233.48/1000000 # *1233.48/1000000 to convert to million cubic meter (MCM)
  
  return(temp_gw_diff)
}

#initiating diversion id. diversion id for MAR nodes start from column 442 in C2VSIM input file
div_id_tmp=441
data2=data=NULL

#intial value of variables to be used later
wat_bal_stor_all=base_gw_chng=diver_all=pump_all=percol_all=gainFrmStrm_all=diver_tmp=return_all1=return_all2=0
gainFrmStrm_tmp=pump_tmp=recharge_tmp=net_sub_in_tmp=dp_perc_tmp=return_tmp=0
wat_bal_stor=list()

#looping through 21 subregions. i=22 will calculate the total for all subregions
for(subreg in 1:22)
{
  #storing name of input variables
  data_list_landwat=c('CVlandwater_base.xlsx','CVlandwater_R90_2ft.xlsx')
  data_list_rootzn=c('CVrootzn_base.xlsx','CVrootzn_R90_2ft.xlsx')
  data_list_gwbud=c('CVground_base.xlsx','CVground_R90_2ft.xlsx')
  data_list_stream=c('NAN','CVstream_diversion_R90_2ft.xlsx')

  # for subregions 1 to 21, calculation for water balance components (except diversion)
  #*****************************************************
  if(subreg<22){
  gainFrmStrm_all=(wat_budget_calc(data_list_gwbud,subreg,type.var = 'GW Storage',var.colm=6,sheet='Sheet',selec_strt_date)[,2])
  gainFrmStrm_tmp=sum(gainFrmStrm_tmp,gainFrmStrm_all)
 
  pump_all=(wat_budget_calc(data_list_gwbud,subreg,type.var = 'GW Storage',var.colm=13,sheet='Sheet',selec_strt_date)[,2])
  pump_tmp=sum(pump_tmp,pump_all)
  
  recharge_all=(wat_budget_calc(data_list_gwbud,subreg,type.var = 'GW Storage',var.colm=7,sheet='Sheet',selec_strt_date)[,2])
  recharge_tmp=sum(recharge_tmp,recharge_all)
  
  return_all1=(wat_budget_calc(data_list_rootzn,subreg,type.var = 'GW Storage',var.colm=9,sheet='Sheet',selec_strt_date)[,2])
  return_all2=(wat_budget_calc(data_list_rootzn,subreg,type.var = 'GW Storage',var.colm=27,sheet='Sheet',selec_strt_date)[,2])
  return_tmp=sum(return_tmp,(return_all1+return_all2))
  
  net_sub_in_all=(wat_budget_calc(data_list_gwbud,subreg,type.var = 'GW Storage',var.colm=15,sheet='Sheet',selec_strt_date)[,2])
  net_sub_in_tmp=sum(net_sub_in_tmp,net_sub_in_all)
  
  dp_perc_all=(wat_budget_calc(data_list_gwbud,subreg,type.var = 'GW Storage',var.colm=5,sheet='Sheet',selec_strt_date)[,2])
  dp_perc_tmp=sum(dp_perc_tmp,dp_perc_all)
  }
  #*****************************************************
  
  # for entire Central valley, calculation for water balance components (except diversion)
  #*****************************************************
  if(subreg==22){
    gainFrmStrm_all=gainFrmStrm_tmp
    pump_all=pump_tmp
    recharge_all=recharge_tmp
    return_all=return_tmp
    net_sub_in_all=net_sub_in_tmp
    dp_perc_all=dp_perc_tmp
  }
  #*****************************************************
  
  # for subregions 1 to 21, calculation for diversion change
  #*****************************************************
  if(subreg<=21){
    stm.count=stm_numberrs[subreg]
    diver_tmp2=0
    for (nkl in 1:stm.count) {
      div_id_tmp=div_id_tmp+1#nkl
      print(div_id_tmp)
      diver_all1=(wat_budget_calc(data_list_stream,div_id_tmp,type.var = 'Diversion',var.colm=4,sheet='Sheet',selec_strt_date)[,2])
      diver_all2=(wat_budget_calc(data_list_stream,div_id_tmp,type.var = 'Diversion',var.colm=5,sheet='Sheet',selec_strt_date)[,2])
      diver_all3=(wat_budget_calc(data_list_stream,div_id_tmp,type.var = 'Diversion',var.colm=6,sheet='Sheet',selec_strt_date)[,2])
      diver_all=diver_all1+diver_all2+diver_all3
      
      div_all_stm_combine=cbind(div_all_stm_combine,diver_all)
      diver_tmp2=sum(diver_tmp2,diver_all)
      diver_tmp=sum(diver_tmp,diver_all)
    }
    diver_all=diver_tmp2
  }

  # for entire Central Valley, calculation for diversion change
  #*****************************************************
  if(subreg==22){
    diver_all=diver_tmp
  }
  
  
  
  data=data.frame(type='Water Balance',Diversion=diver_all,Recharge=recharge_all,GW2Stream=gainFrmStrm_all*(-1),Return_flow=(return_all1+return_all2),net_sub_in=net_sub_in_all,Pump=pump_all,
                  deep_perc=dp_perc_all)
  
  
 #preparing data for ggplot using melt function
  mdat=melt(data,id.vars = c('type'))
  
  #convert unit MCM to km3
  mdat[,3]=mdat[,3]*0.001/56 #56 years
  
  #storing subregion-wide mdat in a list
  mdat_list[[subreg]]=mdat
}


#the following section is used to aggregate subregion data for hydrologic regions for
#plotting. I am using flag to define which region to process
# flag = 1 is SC_EZ, 2 is SJ and 3 is TL

flag=1

if(flag==1){
  st_sub=1
  end_sub=8
  name_reg='SC_EZ'
}
if(flag==2){
  st_sub=10
  end_sub=13
  name_reg='SJ'
}
if(flag==3){
  st_sub=14
  end_sub=21
  name_reg='TL'
}

#read initial subregion value
mdat_comb=mdat_list[[st_sub]]

#add all subregion value for a major region
for (i in (st_sub+1):end_sub) {
  mdat_comb[,3]=mdat_comb[,3]+mdat_list[[i]][,3]
}

#creating plot and storing in variable p
p=ggplot(mdat_comb,aes(x=type,
                  y=value,fill=variable))+
  geom_bar(position="dodge", stat="identity", color='black')+theme_classic()+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab(expression(paste('Change during 1960-2015 ( ', km^{3},'/yr)')))+ xlab(' ')+
  ylim(c(-.03,1.1))+
  scale_fill_manual(values=col,
                    name="Scenarios",
                    labels=c("Diversion", "Recharge", "GW2Stream",'Return Flow','Net Sub. Inflow', "Pump",
                             'Deep Percolation'))

#exporting the plot
ggsave(p,filename=paste0(wd$output,"Water_balance_change_',name_reg,'_R90_2ft.png",sep=""),
       width = 15, height = 15, units = "cm")

