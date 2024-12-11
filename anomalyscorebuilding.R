
### Functions for the energy usage clustering dashboard


library(dplyr)       # data wrangling
library(httr)
library(jsonlite)
library(DT)
library(AnomalyScore)
library(lubridate)  
library(ggplot2)
library(plotly)
library(rsample)     # data splitting 
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees

###  Auxiliary variables and data ####
hierarchytable=fromJSON(
  txt="./data/buildings.json")

#### unlist the hierarchy table and create a metadata table  #####
itrows=   dim(hierarchytable)[1]
metatable=NULL
for(i in 1:itrows){
    if(  dim(hierarchytable$meters[[i]])[1]>0 ){
        tempdf= hierarchytable$meters[[i]][,c("building","meter_id_clean",
                                              "meter_type","serving_revised","units_after_conversion", "class")]
        rowhierarchy= hierarchytable[i,c("building_code","building_name","floor_area","usage","year_built","zone")]
    names( rowhierarchy )[1]="building"
    temptable= tempdf %>% left_join(rowhierarchy, join_by(building))
    if(is.null(metatable)  ){
      metatable=temptable
    }else{
      metatable=rbind(metatable, temptable)
    }
  }else{next}
  
}

# unique buildings
metabuilding=unique(metatable[,c("building","building_name","floor_area","usage","year_built","zone")])

source<-unique( metatable$meter_type) #unique source types 
# list of functions available to compute the scores
dist_names<-c(
  'Cort',
  'Wasserstein',
  'Mahalanobis',
  'CortNorm',
  'Coherence',
  'PDC',
  'CGCI',
  'RGPDC',
  'PMIME',
  'mvLWS',
  'Banddepth'
)
functions=list(
  distance_matrix_cort,
  distance_matrix_wasserstein,
  distance_matrix_mahalanobis,
  distance_matrix_cortNorm,
  distance_matrix_coherence,
  distance_matrix_PDC,
  distance_matrix_CGCI,
  distance_matrix_RGPDC,
  distance_matrix_PMIME,
  distance_matrix_mvLWS,
  distance_matrix_banddepth
)

###  Auxiliary functions ####

# function to clean the dataset by row deleting NA assuming column is time the rest are column series ####
cleanrows= function(table ){
  ldata=  dim(table)[1]
  table2=table
  rowstoclean=c()
  for(i in 1:ldata){
    if( any(is.na(table[i,]) ) ){rowstoclean=c(rowstoclean,i)   }
  } 
  if(length(rowstoclean)>0){ table2=table[-rowstoclean,] }
  return(table2)
}

#### Auxialiry functions find meters #
# Function to find building names via regular expressions ####
findmeters=function(namestosearch, meters ){
  meterloc=c()
  for(i in 1: length(meters)){
    meterloc[i]<- grep(paste0("^",  meters[i],"$"  )  ,namestosearch)
  }
  return(meterloc)
}

# building information and making the classification/regression tree #
findbuildings=function(namestosearch, buildings ){
  meterloc=c()
  for(i in 1: length(buildings)){
    meterloc<-c(meterloc, grep(buildings[i],namestosearch))
  }
  return(meterloc)
}
 
# function to query data from the API given a data range ####
# in the public version we query the sampled public data
energyframe=function(metype, from, to,metatable  ){
  
  metasub=metatable[which(metatable$meter_type==metype),]
  metinst<- metasub$meter_id_clean
  obsfinaltable=NULL
    namesdf=metasub$meter_id_clean 
    buildingcodes=  sub(pattern="_.*","", x=namesdf )
    buildingcodes=  sub(pattern="-.*","", x=buildingcodes )
    
    ### summarize by building
    Lquery= length(buildingcodes)
    for(k in 1:Lquery){ 
      samplemeter=fromJSON(
        txt=paste0( "./data/sample/", namesdf[k], ".json" ) )
      timeformat=strptime( samplemeter$time ,format='%Y-%m-%dT%H:%M:%S+0000')
      samplemeter=samplemeter[which(   timeformat>= from & timeformat<= (as.Date( to )+1)  ),    ]
      
        if(is.null(obsfinaltable)  ){
          obsfinaltable=  samplemeter  #TSquery[[k]]$obs
          names(obsfinaltable)[2]=buildingcodes[k]
        }else{

          if( names(obsfinaltable)[dim(obsfinaltable)[2]] == buildingcodes[k]  ){
            # substitute NA with 0 before summing
            samplemeter$value[which(is.na(samplemeter$value )) ]=0 
            middletab=  obsfinaltable[,c(1,dim(obsfinaltable)[2])]
            middletab=middletab %>% left_join(samplemeter,join_by(time) )
            middlevec=apply( middletab[,-1],1,sum   )
            #join separately then sum then add
            obsfinaltable[,dim(obsfinaltable)[2]]=middlevec
            names(obsfinaltable)[dim(obsfinaltable)[2]]=buildingcodes[k]

          }else{
            obsfinaltable=obsfinaltable%>% left_join(samplemeter,join_by(time) )
            #also check for the second iteration because the indexes for the names change
            names(obsfinaltable)[dim(obsfinaltable)[2]]=buildingcodes[k]
          }
        }
    } #end for k
    obsfinaltable$time=strptime( obsfinaltable$time ,format='%Y-%m-%dT%H:%M:%S+0000')
  #discard zero columns
  chekcdev<- apply(obsfinaltable[,-1],2,sum, na.rm=T )
  zerodevices<- as.vector(which(chekcdev<.0001))
  if(length(zerodevices)>0){ obsfinaltable<-obsfinaltable[,-(zerodevices+1)]}
  #discard with NA more than 10% of the column size
  sumvert<- apply(obsfinaltable[-1]  ,2, function(x) sum(is.na(x)))
  devsout<- as.vector(which(sumvert> (dim(obsfinaltable)[1]/10) ))
  if(length(devsout)>0){obsfinaltable=obsfinaltable[,-(devsout+1)]}
  #delete the rows with at least one NA
  obsfinaltable=cleanrows(obsfinaltable)
  return(obsfinaltable)
}


# Function to compute the mnomaly scores for a single table and add the metadata #####
Anomaly_score_LUbuilding=function(sum_series,knn, distance,dparams, standvec, metabuilding ){
  
  if( dim(sum_series)[1]!=0 ){
    
    mtk<- which(names(sum_series)=="time"|names(sum_series)=="timechar" )
    unittest<-as.matrix( sum_series[,-mtk ] )
    totalenergy<-apply(unittest, 2, sum, na.rm=T  )#total energy
    
    standvec= as.numeric(standvec)
    if(is.null(standvec)){lstand=0 }else{
    lstand=length(standvec)}
    
    cs_true=c(T,T)
    cs_default=c(F,F)
    if(lstand>0){cs_default[standvec]=cs_true[standvec] }  
    unittest<-scale(as.matrix(unittest), center = cs_default[2], scale = cs_default[1])
    dparams$unit<-unittest
    # application of the score function    
    AScat1<-kneighbors_distance_docall(knn, distance, dparams )
    # summarize important variables
    
    #order the results to identify highest scores
    names(AScat1$anomalyscore)<-names(totalenergy)
    orderscore<-order(AScat1$anomalyscore, decreasing = T )
    totalenergy<-totalenergy[   orderscore   ]
    
    AScat1$anomalyscore<-AScat1$anomalyscore[orderscore]
    AScat1$dmat<-AScat1$dmat[orderscore,orderscore]
    
    framescore=data.frame(building=names(AScat1$anomalyscore), Ascore=AScat1$anomalyscore,totalenergy=totalenergy )
    # Adds metadata 
    framescore=framescore %>% left_join(metabuilding, join_by(building) )
    # final dataframe with the results     
    return(list(dmat=AScat1$dmat,  framescore=framescore) )
  }else{
    return("no data")
  }
}

#### function for making a daily anomaly score computation #####
# This function perform a longitudinal analysis by computing the Anomaly scores
# for sequential days and adding metadata (owner, zone, residential indicator of the building)
# then join all the computed scores vertically
# metertype is the source type selected in the dashboard
# starday: initial time of the analysis
# endday: final time of the analysis
# knn: number of nearest neighbors to compute the score 
# distance:  name of the distance matrix function to use
# dparams parameters of the distance function
# center and scale are booleans to standardize the data

Anomalyscore_periods_LU=function(full_series, startday,endday, knn,distance,dparams, standvec ){
  # sequence of days to compute the scores  
  daysequence=seq(as.Date(startday),as.Date(  endday),1 ) 
  for(h in 2: length(daysequence) ){ 
    
    newend<-as.Date(daysequence[h])
    newstart<-newend-1
    sum_series<-full_series[which(full_series$time>= as.character(newstart) &full_series$time<=as.character(newend)),]
    ## check again for potential zero colummns
    chekcdev<- apply(sum_series[,-1],2,sum, na.rm=T )
    zerodevices<- as.vector(which(chekcdev<.0001))
    if(length(zerodevices)>0){ sum_series<-sum_series[,-(zerodevices+1)]}
    # error handling
    an.error.occured <- FALSE
    tryCatch( {ASLU<-Anomaly_score_LUbuilding(sum_series, knn,distance,dparams, standvec,metabuilding ) }
              , error = function(e) {an.error.occured <<- TRUE;
              })
    if(an.error.occured){print(paste0( "An error occured at h=", h," Date=",newstart)  ); next}
    # it can skip iterations where for a particular day all data is NA or zeros    
    if(is.character(ASLU) ){next}
    ASLU$framescore$date<-newend
    # adding all data together    
    if(h ==2 ){
      finalframe<-ASLU$framescore
    }else{
      finalframe<-rbind(finalframe,ASLU$framescore)
    }
  }
  finalframe$weekend<- as.factor(  ifelse(wday(finalframe$date)<=5,0,1) )
  finalframe$usage<-as.factor(finalframe$usage)
  finalframe$building_name<-as.factor(finalframe$building_name)
  #finalframe$owner<-as.factor(finalframe$owner)
  finalframe$zone<-as.factor(finalframe$zone)
  #finalframe$year_built<-as.factor(finalframe$year_built)
  return(finalframe)
}


path.to.root <- function(node){
  if(node == 1){ # root?
    node}
  else{ # recurse, %/% 2 gives the parent of node
    c(node, path.to.root(node %/% 2))}
}


# return the given node and all its ancestors (a vector of node numbers)
# from: Plotting rpart trees with the rpart.plot package (Stephen Milborrow, 2021)
plottreenodes=function(mod, nodeindex, distaname){
  if(nodeindex==0){
    rpart.plot(mod, branch.lwd=5,roundint=FALSE, type=5,cex=1)  
  }else{
    nodeslabel = as.numeric(  row.names(mod$frame)[  which ( mod$frame$var=="<leaf>" ) ] )
    nodes <- as.numeric(row.names(mod$frame))
    node <- nodeslabel[nodeindex]  # 11 is our chosen node, arbitrary for this example
    cols.text <- ifelse(nodes %in% path.to.root(node), "black", "gray")
    cols.branch <- ifelse(nodes %in% path.to.root(node), "red", "gray")
    #plotting the tree
    rpart.plot(mod,col = cols.text, branch.col = cols.branch,branch.lwd=5, split.col = cols.text,roundint=FALSE, type=5,cex=1)
  }
}


# function to highlight the nodes of the tree #####
# plot the TS corresponding to a node, in case of  node=0 then all the series are plotted

highlightplot= function(optimal_tree,unit_train,fulldata ,node,datestart,dateend,standvec){
  standvec= as.numeric(standvec)
  lstand=length(standvec)
  cs_true=c(T,T)
  cs_default=c(F,F)
  if(lstand>0){cs_default[standvec]=cs_true[standvec] }  
  startday<-paste0(datestart, " 00:00:00")
  endday<-paste0(dateend, " 23:59:59" )
  
if(node==0){
  Bdhs<- as.character( unique(unit_train$building_name ) )
  codesbuilding<- unique(unit_train$building )
  
  for(i in 1:length(Bdhs) ){
    
    ts_hs<-fulldata[which(fulldata$time>= startday &fulldata$time<=endday) , c(1, findbuildings(   names(fulldata) ,codesbuilding[i] )  )]
    names(ts_hs)[2]<-"Power"
    ts_hs$day_hour<- paste0(yday(ts_hs$time),"_", hour(ts_hs$time))
    ts_hs<-ts_hs[,-c(1)]%>% group_by(day_hour) %>% summarise(across(everything(), \(x) sum(x, na.rm = TRUE) ) )
    ts_hs$Building<-Bdhs[i]
    
    
    ts_hs$Power<-scale(ts_hs$Power, center = cs_default[2], scale = cs_default[1]) # set center and scale according to inputs
    if(i==1 ){
      sumframe<-ts_hs
    }else{
      sumframe<-rbind(sumframe,ts_hs)
    }
    
  }
  
  #formating for ggplot
  dayh<-strsplit( sumframe$day_hour,"_" )
  sumdays<- unlist(dayh)[seq(1,(2*dim(sumframe)[1]-1), by=2 ) ]
  sumhours<-unlist(dayh)[seq(2,(2*dim(sumframe)[1]), by=2 ) ]
  date_sum<-  as.Date( paste0(year(startday),"-01-01" ) )+ as.numeric(sumdays) -1
  sumframe$time<-as.POSIXct( paste0(date_sum, " ",sumhours ), format="%Y-%m-%d %H" )
  sumframe<-sumframe[order(sumframe$time),]
  names(sumframe)[2]<-"Power"
  #visualization
  ggplot(sumframe, aes(x = time, y = Power)) +
  geom_line(aes(color = Building))+
    scale_x_datetime(
      #breaks = scales::date_breaks("1 day"),           # Major breaks at start of each day
      date_breaks="1 day",
      #   minor_breaks = scales::date_breaks("1 hour"),    # Minor breaks for every hour
      date_minor_breaks="1 hour",
      labels =scales::date_format("%b %d")               # Show day name
    ) +
  theme(panel.background = element_rect(fill='transparent'), 
        axis.minor.ticks.length.x.bottom = unit(2, "pt"),
        axis.minor.ticks.x.bottom  = element_line(color = "red"),  # Red color for minor ticks
  plot.background = element_rect(fill='transparent', color=NA),legend.position="none",
  axis.title.y.left=element_blank(),
  axis.text.x=element_text(angle = 45)
  )
  
}else{  

wherenodes<- unique(  optimal_tree$where )
wherenodes=wherenodes[order(wherenodes)]

indlocmaxleaf<- which(optimal_tree$where== wherenodes[node]    ) # max(optimal_tree$where) as.numeric(input$node)
high_scores<- unit_train[indlocmaxleaf,]
Bdhs<- as.character( unique(high_scores$building_name ) )
codesbuilding<- unique(high_scores$building )

#selection of days and buildings from the main data set
for(i in 1:length(Bdhs) ){
  
  ts_hs<-fulldata[which(fulldata$time>= startday &fulldata$time<=endday) , c(1, findbuildings(   names(fulldata) ,codesbuilding[i] )  )]
  filterhs<- high_scores[which(high_scores$building_name==Bdhs[i]  ),]
  ts_hs$highlight<-ifelse(as.Date( ts_hs$time) %in%  filterhs$date ,1,0  )
  names(ts_hs)[2]<-"Power"
  ts_hs$day_hour<- paste0(yday(ts_hs$time),"_", hour(ts_hs$time))
  ts_hs<-ts_hs[,-c(1)]%>% group_by(day_hour) %>% summarise(across(everything(), \(x) sum(x, na.rm = TRUE) ) )
  ts_hs$Building<-Bdhs[i]
  
  ts_hs$Power<-scale(ts_hs$Power, center = cs_default[2], scale = cs_default[1]) # set center and scale according to inputs
  
  
  if(i==1 ){
    sumframe<-ts_hs
  }else{
    sumframe<-rbind(sumframe,ts_hs)
  }
  
}

#formating for ggplot
dayh<-strsplit( sumframe$day_hour,"_" )
sumdays<- unlist(dayh)[seq(1,(2*dim(sumframe)[1]-1), by=2 ) ]
sumhours<-unlist(dayh)[seq(2,(2*dim(sumframe)[1]), by=2 ) ]
date_sum<-  as.Date( paste0(year(startday),"-01-01" ) )+ as.numeric(sumdays) -1
sumframe$time<-as.POSIXct( paste0(date_sum, " ",sumhours ), format="%Y-%m-%d %H" )
sumframe<-sumframe[order(sumframe$time),]

# Visualization
ggplot(sumframe, aes(x = time, y = Power)) +
geom_line(aes(color = Building))+
  scale_x_datetime(
    #breaks = scales::date_breaks("1 day"),           # Major breaks at start of each day
    date_breaks="1 day",
 #   minor_breaks = scales::date_breaks("1 hour"),    # Minor breaks for every hour
    date_minor_breaks="2 hour",
    labels = scales::date_format("%b %d")               # Show day name
  ) +
theme(panel.background = element_rect(fill='transparent'),
      axis.minor.ticks.length.x.bottom = unit(2, "pt"),
      axis.minor.ticks.x.bottom  = element_line(color = "red"),  # Red color for minor ticks
      plot.background = element_rect(fill='transparent', color=NA),legend.position="bottom",
axis.title.y.left=element_blank(),
axis.text.x=element_text(angle = 45)
)
}
  
}


## function to determine the depth of a tree given data and a set of variables####

target_tree=function(datascores, variables, targetdepth, maxiter ){
  finalmod=rpart( # initial tree
    formula = Ascore ~ . ,
    data    = datascores[,variables],
    method  = "anova",
    control = list(cp=0,minsplit = 1, minbucket = 1)
  )
  nodes <- as.numeric(rownames(finalmod$frame))
  maxdepth= max(rpart:::tree.depth(nodes))
  
  if(maxdepth>targetdepth ){ # start the process to find a smaller tree
    cpit=0.00000000000000001
    Dfactor=1.079
    for(i in 1:maxiter){
      cpit=cpit*Dfactor   
      modtemp=rpart(
        formula = Ascore ~ . ,
        data    = datascores[,variables],
        method  = "anova",
        control = list(cp=cpit,minsplit = 1, minbucket = 1)
      )
      nodes <- as.numeric(rownames(modtemp$frame))
      depthmod= max(rpart:::tree.depth(nodes))
      if(depthmod<maxdepth){
        finalmod=modtemp
        maxdepth=depthmod
      }
      if(depthmod<=targetdepth){break}
    } # 
  }
  return( finalmod  )   
}


#get the risk levels (ordering by mean value)
# my mod is the regression tree as an rpart object
risklevels<-function(tree,scores){
  #tree is the regression tree 
  # scores is the data frame containing the scores used to create the tree
  wherenodes<- unique(  tree$where )
  wherenodes=wherenodes[order(wherenodes)] # terminal nodes
  #mean values
  mval=c()
  for(i in 1:length(wherenodes)){
    indlocmaxleaf<- which(tree$where== wherenodes[i]    ) 
    selected_scores<- scores[indlocmaxleaf,]
    mval[i]= mean(selected_scores$Ascore)
  }
  
  indexframe= data.frame(index=1:length(wherenodes),nodeval=wherenodes, mval=mval )
  indexframe=indexframe[order(indexframe$mval, decreasing = T),]
  # RL  list creation
  listindex= as.list(c(0, indexframe$index) )
  #vector of risk level labels
  labelsrisk=c("All",paste0("RL ",length(wherenodes):1 ) )
  names(listindex)=labelsrisk
  return(listindex)
}


#tests
#  testdata=energyframe(metype ="Electricity",from="2023-08-01", to="2023-08-04", metatable = metatable)
# 
# TESTscore_singleday= Anomaly_score_LUbuilding(sum_series=testdata,knn=4, distance=distance_matrix_banddepth,dparams=list(), standvec=c(1,2), metabuilding )
# 
# TESTscore=Anomalyscore_periods_LU(full_series=testdata, startday="2023-08-01",endday="2023-08-04", knn=4,distance=distance_matrix_cort,dparams=list(k=3),  standvec=c() )
# 
# mytree= target_tree(datascores=TESTscore, variables=c(2,5:8), targetdepth=4, maxiter=1000 )
# 
# plottreenodes(mod=mytree, nodeindex=5, distaname="Hi")
# 
# 
# highlightplot(optimal_tree=mytree,unit_train=TESTscore,fulldata=testdata ,node=7,datestart="2023-08-01",dateend="2023-08-04",standvec=c())

  
  
  