#The code calculates cumulative groundwater to stream flow for base and MAR scenarios
#it is written by sarfaraz alam. email: szalam@ucla.edu


rm(list=ls())

wd=list()
wd$data= 'add data directory here'
wd$figure='add output figure directory here'

#reading all GW to stream values
#-------------------------------------

#list variables
gw2stream = list()

#set directory
setwd(wd$data)

#in loop read the gw-stream interaction variables
for (i in 1:21) {
  Base = read.csv(paste0('wat_balance_components_Base_',i,'.csv'))[,5]
  scen_R90_2ft = read.csv(paste0('wat_balance_components_R90_2ft_',i,'.csv'))[,5]
  scen_R90_10ft = read.csv(paste0('wat_balance_components_R90_10ft_',i,'.csv'))[,5]
  scen_R80_2ft = read.csv(paste0('wat_balance_components_R80_2ft_',i,'.csv'))[,5]
  scen_R80_10ft = read.csv(paste0('wat_balance_components_R80_10ft_',i,'.csv'))[,5]
  
  #cumulative sum
  Base = cumsum(Base)
  scen_R90_2ft = cumsum(scen_R90_2ft)
  scen_R90_10ft = cumsum(scen_R90_10ft)
  scen_R80_2ft = cumsum(scen_R80_2ft)
  scen_R80_10ft = cumsum(scen_R80_10ft)
  
  #storing in dataframe and in list
  tmp = data.frame(Base = Base, R90_2ft = scen_R90_2ft, R90_10ft = scen_R90_10ft,
                   R80_2ft = scen_R80_2ft, R80_10ft = scen_R80_10ft)
  
  #subtracting the first value of cumsum with all and making first row = 0
  for(k in 1:ncol(tmp)){
    tmp[2:nrow(tmp),k] = tmp[2:nrow(tmp),k] - tmp[1,k]
  }
  
  tmp[1,] = 0
  
  #storing in list
  gw2stream[[i]] = tmp
  
}


# sum values Based on major regions
#--------------------------------------
sc_ez = gw2stream[[1]]
for(i in 2:8)
{
  sc_ez = sc_ez+ gw2stream[[i]]
}

sj = gw2stream[[10]]
for(i in 11:13)
{
  sj = sj+ gw2stream[[i]]
}

tl = gw2stream[[14]]
for(i in 15:21)
{
  tl = tl+ gw2stream[[i]]
}

cv = gw2stream[[1]]
for(i in 2:21)
{
  cv = cv+ gw2stream[[i]]
}


# plot cumulative for different scenario
#------------------------------------------

#date in water year (calendar year is 10/31/1959)
date = seq(as.Date("1960/01/15"), by = "month", length.out = 672)
sc_ez = cbind(date, sc_ez)
sj = cbind(date, sj)
tl = cbind(date, tl)  
cv = cbind(date, cv)

mdat_sc_ez = melt(sc_ez, id.vars = c('date'))
mdat_sj = melt(sj, id.vars = c('date'))
mdat_tl = melt(tl, id.vars = c('date'))
mdat_cv = melt(cv, id.vars = c('date'))


#The following section provides plots for SC_EZ, SJ, TL and CV. Each done separately and exported
#separately

p = ggplot(mdat_sc_ez, aes(x=date, y = value, group = variable)) + 
  theme_bw()+
  geom_line(aes(color = variable ), size=1.2) + 
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab(expression(paste('[ ',km^3,']')))+ xlab('Year')+
  scale_x_date(limits = as.Date(c("1960-01-15","2020-12-15")),date_breaks = "10 year",
               date_labels = "%Y")+
  scale_linetype_manual(values=c("solid", "solid",'dashed',"solid",'dashed'))+
  scale_color_manual(values=c("black", "red", "darkgreen", 'blue', 'orange'), 
                     name="Scenarios",
                     breaks=c("Base", "R90_2ft", "R90_10ft","R80_2ft", "R80_10ft"),
                     labels=c("Base", "R90_2ft", "R90_10ft","R80_2ft", "R80_10ft"))


ggsave(p,filename=paste0(wd$figure,"sc_ez_gw2stream.png",sep=""),
       width = 20, height = 15, units = "cm")

p = ggplot(mdat_sj, aes(x=date, y = value, group = variable)) + 
  theme_bw()+
  geom_line(aes(color = variable ), size=1.2) + 
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab(expression(paste('[ ',km^3,']')))+ xlab('Year')+
  scale_x_date(limits = as.Date(c("1960-01-15","2020-12-15")),date_breaks = "10 year",
               date_labels = "%Y")+
  scale_linetype_manual(values=c("solid", "solid",'dashed',"solid",'dashed'))+
  scale_color_manual(values=c("black", "red", "darkgreen", 'blue', 'orange'), 
                     name="Scenarios",
                     breaks=c("Base", "R90_2ft", "R90_10ft","R80_2ft", "R80_10ft"),
                     labels=c("Base", "R90_2ft", "R90_10ft","R80_2ft", "R80_10ft"))
ggsave(p,filename=paste0(wd$figure,"sj_gw2stream.png",sep=""),
       width = 20, height = 15, units = "cm")

p = ggplot(mdat_tl, aes(x=date, y = value, group = variable)) + 
  theme_bw()+
  geom_line(aes(color = variable ), size=1.2) + 
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab(expression(paste('[ ',km^3,']')))+ xlab('Year')+
  scale_x_date(limits = as.Date(c("1960-01-15","2020-12-15")),date_breaks = "10 year",
               date_labels = "%Y")+
  scale_linetype_manual(values=c("solid", "solid",'dashed',"solid",'dashed'))+
  scale_color_manual(values=c("black", "red", "darkgreen", 'blue', 'orange'), 
                     name="Scenarios",
                     breaks=c("Base", "R90_2ft", "R90_10ft","R80_2ft", "R80_10ft"),
                     labels=c("Base", "R90_2ft", "R90_10ft","R80_2ft", "R80_10ft"))
  

ggsave(p,filename=paste0(wd$figure,"tl_gw2stream.png",sep=""),
       width = 20, height = 15, units = "cm")

p = ggplot(mdat_cv, aes(x=date, y = value, group = variable)) + 
  theme_bw()+
  geom_line(aes(color = variable ), size=1.2) + 
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab(expression(paste('[ ',km^3,']')))+ xlab('Year')+
  scale_x_date(limits = as.Date(c("1960-01-15","2020-12-15")),date_breaks = "10 year",
               date_labels = "%Y")+
  scale_linetype_manual(values=c("solid", "solid",'dashed',"solid",'dashed'))+
  scale_color_manual(values=c("black", "red", "darkgreen", 'blue', 'orange'), 
                     name="Scenarios",
                     breaks=c("Base", "R90_2ft", "R90_10ft","R80_2ft", "R80_10ft"),
                     labels=c("Base", "R90_2ft", "R90_10ft","R80_2ft", "R80_10ft"))


ggsave(p,filename=paste0(wd$figure,"cv_gw2stream.png",sep=""),
       width = 20, height = 15, units = "cm")
