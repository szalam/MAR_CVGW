#This code is written to find node on C2VSIM stream to divert water to recharge elements
#The goal is to find stream node that has minimum distance to recharge sites
#When there are more than 1 stream to divert, 1 group of recharge sites get water from one stream
#and the other group gets from the other recharge site
#This code is written by Sarfaraz Alam. email: szalam@ucla.edu

rm(list=ls())

#import library
library(rowr)

wd=list()
wd$data='add data directory here'
wd$output='add output directory here'
setwd(wd$data)

#list of files
files=c('element_Centroid_with_subreg_SAGBI_coordinate.txt','stream_gw_node_with_strmId_coordinate.txt','sub_reachOut_connections.xlsx')

#import file 1
df1=read.table(files[1],header=T, sep=",")[,c(2,5,6,8,9,21)]

#separating required SAGBI types
df1=df1[which(df1[,6]=='Good' | df1[,6]=='Moderately Good' | df1[,6]=='Excellent'),]
head(df1)

#import file 2
df2=read.table(files[2],header=T, sep=",")[,c(3,4,7,8,9)]
head(df2)

#reach connection file import
rch.conn=read_excel(paste0(wd$data,files[3]))

#initiating variabls to be used later
selec.all.nodes=selec.all.elements=list()
sub.strm.node=0
selec.all.elements.all=data.frame(matrix(0,nrow = 1000,ncol=40))
gk=1

#calculate distance in loop
for (subreg in 1:21) {
  
  #reaches
  rch.num=which(rch.conn[subreg,]==-999)[1]-2
  rch.tmp=unlist(rch.conn[subreg,(c(1:rch.num)+1)])
  
  #separate nodes with select stream ids in df2.tmp
  df2.tmp=df2
  df2.tmp=df2.tmp[df2.tmp$StreamID %in% rch.tmp,]
  head(df2.tmp)
  
  #separate sagbi elements in given subregions
  df1.tmp=df1
  df1.tmp=df1.tmp[which(df1.tmp$SubRegion==subreg),]
  head(df1.tmp)
  
  #selected streams and nodes store
  selec.all.nodes[[subreg]]=0
  selec.all.elements[[subreg]]=0
  
  #when there is only 1 stream
  if(rch.num==1){
    tmp=0
    for (i in 1:nrow(df2.tmp)) {
      tmp1=0
      for (j in 1:nrow(df1.tmp)) {
        dist=sqrt((df1.tmp[j,4]-df2.tmp[i,4])^2+(df1.tmp[j,5]-df2.tmp[i,5])^2)
        tmp1=tmp1+dist
      }
      tmp=c(tmp,tmp1)
    }
    tmp=tmp[-1]
    
    strm.nod.id.minDist=which(tmp==min(tmp))
    select_strm_node=df2.tmp[strm.nod.id.minDist,]
    selec.all.nodes[[subreg]]=select_strm_node
    selec.all.elements[[subreg]]=df1.tmp[,2]
    selec.all.elements.all[1:length(selec.all.elements[[subreg]]),gk]=selec.all.elements[[subreg]]
    gk=gk+1
  }
  
  
  
  #when there more than 1 stream
  dist.elem=list()
  if(rch.num>1){
    select.strm=0
    #first find which stream is closest to which node
    #looping through each stream
      for (i in 1:nrow(df1.tmp)) {
        dist.elem[[i]]=0
        for (j in 1:nrow(df2.tmp)) {
          dist=sqrt((df1.tmp[i,4]-df2.tmp[j,4])^2+(df1.tmp[i,5]-df2.tmp[j,5])^2)
          dist.elem[[i]]=c(dist.elem[[i]],dist)
        }
        dist.elem[[i]]=dist.elem[[i]][-1]
        tmp.min=which(dist.elem[[i]]==min(dist.elem[[i]]))[1]
        select.strm=c(select.strm,df2.tmp[tmp.min,2])
      }
    select.strm=select.strm[-1]

      #selected reach number is added in a new column
      df1.tmp=data.frame(df1.tmp,selec_stream=select.strm)
      
      #---------------------------------------------------------------
      # upto this we found which which node should get water from which reach. now have to
      # find which point in that reach gives minimum distance
      #---------------------------------------------------------------
      strms=unique(df1.tmp$selec_stream)
      
      k=1
      for (n in strms) {
        
        df2.tmp.tmp=df2.tmp[which(df2.tmp$StreamID==n),]
        df1.tmp.tmp=df1.tmp[which(df1.tmp$selec_stream==n),]
        print(length(df1.tmp.tmp[,2]))
        selec.all.elements[[subreg]]=cbind.fill(selec.all.elements[[subreg]],df1.tmp.tmp[,2],fill=NA)
        selec.all.elements.all[1:length(df1.tmp.tmp[,2]),gk]=df1.tmp.tmp[,2]
        gk=gk+1
        tmp=0
        for (i in 1:nrow(df2.tmp.tmp)) {
          tmp1=0
          for (j in 1:nrow(df1.tmp.tmp)) {
            dist=sqrt((df1.tmp.tmp[j,4]-df2.tmp.tmp[i,4])^2+(df1.tmp.tmp[j,5]-df2.tmp.tmp[i,5])^2)
            tmp1=tmp1+dist
          }
          tmp=c(tmp,tmp1)
        }
        tmp=tmp[-1]
        
        strm.nod.id.minDist=which(tmp==min(tmp))
        select_strm_node=df2.tmp.tmp[strm.nod.id.minDist,]
        selec.all.nodes[[subreg]]=rbind(selec.all.nodes[[subreg]],select_strm_node)
      }

      selec.all.nodes[[subreg]]=selec.all.nodes[[subreg]][-1,]
      selec.all.elements[[subreg]]=selec.all.elements[[subreg]][,-1]
  }
  
  if(subreg ==14){
  selec.all.elements.all[,gk]=0
  gk=gk+1
  }
  
  if(subreg!=14){
    sub.strm.node=rbind(sub.strm.node,data.frame(subreg,StreamID=selec.all.nodes[[subreg]][,2],nodeID=selec.all.nodes[[subreg]][,3]))
  }
  if(subreg ==14){
    sub.strm.node=rbind(sub.strm.node,data.frame(subreg,StreamID=0,nodeID=0))
  }

}

sub.strm.node=sub.strm.node[-1,]
colnames(selec.all.elements.all)=c(paste0('Target_for_rch_',1:nrow(sub.strm.node)))


#exporting stream node and target recharge elements
write.csv(selec.all.elements.all,paste0(wd$output,'target_elements_based_on_distance.csv'))
write.csv(sub.strm.node,paste0(wd$output,'diversion_streams_based_on_distance.csv'))

