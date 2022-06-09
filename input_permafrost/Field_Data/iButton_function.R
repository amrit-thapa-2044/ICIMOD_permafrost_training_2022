# PURPOSE:
#	extracts mean annual ground surface temperature (MAGST), basal ripening date (RD) and melt out date (MD)
# Author: schmid marc-olivier
###############################################################################
f_iButton<-function(in_path,in.matrix,out_path,out.matrix){	
  
  "&" <- function(...) UseMethod("&")
  "&.default" <- .Primitive("&")
  "&.character" <- function(...) paste(...,sep="")
  
  data.ib		<-	read.csv(paste(in_path,in.matrix,sep = ''),header=T) 
  
  year		<-	substring(data.ib$date[length(data.ib$date)],1,4)					#year of end date
  
  max.mis		<-	24		#max of NA values per ibutton for interpolation, otherwise ibutton gets removed
  
  ###snow
  m1	<-	3		#max. temperature for snow
  m2	<-	0.5		#min. temperature snow can disappear
  t1	<-	24		#Number of measurements per day
  v1	<-	0.1		#max. daily sd indicating snow for POSITIVE GST
  v2	<-	0.3		#max. daily sd indicating snow for NEGATIVE GST
  
  ###degree of reliance for snow
  
  time1	<-	paste(year,'-01-01',sep = '') #year&"-01-01"		#start of relevant time period for degree of reliance for snow
  time2	<-	paste(year,'-03-01',sep = '') #year&"-03-01"		#end of relevant time period for degree of reliance for snow
  
  MDr.sd	<-	MDr.sd #0.2				#min. mean daily standart deviation with no snow cover for specified time period
  
  ###zero curtain
  v	<- 	0.25	#treshold zero curtain (if dailymax & dailymin is within +/- v, this day is a zero curtian day)
  t.FI<-	-50		#min FI during longest snow cover period to extract spring zero curtain period
  
  ###############################################################################
  
  ### ibutton data
  #date<-read.csv(paste(in_path,in.matrix,sep = ''))[1]
  ###if more than one day is missing ibutton get removed
  for(ib in names(data.ib)){
    if(length(subset(data.ib[,ib],is.na(data.ib[,ib])))>max.mis){data.ib[,ib]<-NULL}
  }
  
  # ##linear interpolation of missing values
  # for (ib in names(data.ib)){
  # 	data.ib[,ib]<-na.approx(data.ib[,ib]) #is.na
  # }
  #data.ib$date<-date
  
  ###iButton names
  names.ib<-names(data.ib[,2:length(data.ib)])
  #	names.ib<- names(data.ib)#names(data.ib[,2:dim(data.ib)[2]])
  
  
  ###output table
  stat.melt<-as.data.frame(matrix(ncol = 5, nrow = length(data.ib)-1))#stat.melt<-as.data.frame(matrix(ncol = 5, nrow = dim(data.ib)[2]))#stat.melt<-as.data.frame(matrix(ncol = 5, nrow = length(data.ib)-1))
  names(stat.melt) <- c("ib","FI","RD","MD","MAGST2014")
  stat.melt$ib<-names.ib
  
  ###daily aggregations
  
  a.sd	<-aggregate(data.ib[2:length(names(data.ib))],by=list(substr(data.ib$date,1,10)),FUN=sd)
  a.mean	<-aggregate(data.ib[2:length(names(data.ib))],by=list(substr(data.ib$date,1,10)),FUN=mean)
  a.max	<-aggregate(data.ib[2:length(names(data.ib))],by=list(substr(data.ib$date,1,10)),FUN=max)
  a.min	<-aggregate(data.ib[2:length(names(data.ib))],by=list(substr(data.ib$date,1,10)),FUN=min)
  
  #aa=aggregate(data.ib[2:length(names(data.ib))],by=list(substring(data.ib$date,1,10)),FUN=min)
  
  
  ########### CALCULATION
  i<-1
  
  for(ib in names.ib){
    # ib="D16"
    x<-a.sd[,ib]
    z<-a.min[,ib]
    y<-a.max[,ib]
    w<-a.mean[,ib]
    tt<-data.ib[,ib]
    
    ###MAGST2014
    stat.melt$MAGST2014[i]<-mean(w)
    
    ### freezing index (FI)	
    stat.melt$FI[i]<-sum(w[which(w<0)])
    
    #temporary index
    melt.index <- data.frame(date=a.sd$Group.1 , snow.ind = rep(0,length(z)),zc.ind = rep(0,length(z)))
    
    ####SNOW	
    #melt.index[z$x < v1, "snow.ind"] <- 1
    melt.index[x < ifelse(y<0,yes=v2,no=v1),"snow.ind"] <- 1
    melt.index[y>m1,"snow.ind"]<-0
    index <- melt.index$snow.ind
    
    #positive and negative temperatures
    temp.index<-data.frame( date=a.sd$Group.1 , ind = rep(0,length(z)))
    temp.index[y<m2,"ind"]<-1
    temp<-temp.index$ind
    
    index.new <- index
    for(ii in 1:(length(index)-1)){
      if(index.new[ii] == 1 & index.new[ii + 1] == 0 & temp[ii + 1] == 1) index.new[ii + 1] <- 1
    }
    
    #ibuttons with to short snow cover length dont get filled up just due to negative temps
    
    index<-index.new
    melt.index$snow.ind <- index
    
    #end of snow
    j <- 0
    k <- 0
    for(ii in 1:(length(index)-1)){
      if(index[ii] == 1 & index[ii + 1] != 0){j <- j + 1}
      if(index[ii] == 1 & index[ii + 1] == 0 & k > j){j <- 0}
      if(index[ii] == 1 & index[ii + 1] == 0 & k <= j){ k <- j; 
      ind.end <- ii; j <- 0}
    }   
    
    stat.melt$MD[i] <-as.character(melt.index$date[ind.end])
    
    #start of snow
    j <- 0
    k <- 0
    for(ii in length(index):2){
      if(index[ii] == 1 & index[ii - 1] != 0){j <- j + 1}
      if(index[ii] == 1 & index[ii - 1] == 0 & k > j){j <- 0}
      if(index[ii] == 1 & index[ii - 1] == 0 & k <= j){ k <- j; 
      ind.start <- ii; j <- 0}
    }
    
    if(sum(index)!=0){
      index.snow<-index*0
      index.snow[ind.start:ind.end]<-1
    }else{
      index.snow<-index
    }
    
    ### freezing index (FDD) during snow period	
    w2<-index.snow*w
    stat.melt$FDD[i]<-sum(w2[which(w2<0)])
    
    ###sd from Dez-March
    stat.melt$sd.day[i]<-mean(x[which(a.sd$Group.1==time1):which(a.sd$Group.1==time2)])
    
    ###BASAL RIPENING
    #######################################################################################
    #all zero curtain days
    melt.index[z <= v&z>-v&y <= v&y>-v, "zc.ind"] <- 1
    
    #index for negative temps
    index.freez<-index*0
    index.freez[w<(-v)]<-1
    
    if(sum(index.freez)!=0){
      j <- 0
      k <- 0
      for(ii in 1:(length(index.freez)-1)){
        if(index.freez[ii] == 1 & index.freez[ii + 1] != 0){j <- j + 1}
        if(index.freez[ii] == 1 & index.freez[ii + 1] == 0 & k > j){j <- 0}
        if(index.freez[ii] == 1 & index.freez[ii + 1] == 0 & k <= j){ k <- j; 
        freez.end <- ii; j <- 0}
      }   
      
      
      melt.index$zc.ind<-melt.index$zc.ind*index.snow
      melt.index$zc.ind[1:freez.end]<-0
      
    }else{
      melt.index$zc.ind<-melt.index$zc.ind*index.snow
    }
    
    index.new<-melt.index
    index.RD<-melt.index$zc.ind
    
    #onset of melting period
    j <- 0
    k <- 0
    for(a in 1:(length(index.RD)-1)){
      if(index.RD[a] == 1){j <- j + 1}
      if(index.RD[a+1] == 0 & k <= j){ k <- j; ind.end <- a; j <- 0}
    }      
    ind.start <-  ind.end -k + 1
    
    ss<-julian(strptime(index.new$date[ind.start], "%Y-%m-%d"))
    ee<-julian(strptime(stat.melt$MD[i], "%Y-%m-%d"))
    
    
    stat.melt$RD[i]	<-	as.character(index.new$date[ind.start])
    
    stat.melt$RD[i]<-ifelse(sum(index.new$zc.ind)==0,yes="NA",no=stat.melt$RD[i]) #no date if no zero curtain
    stat.melt$RD[i]<-ifelse(stat.melt$FI[i]>t.FI,yes="NA",no=stat.melt$RD[i]) #date only if FI is big enough
    stat.melt$RD[i]<-ifelse(ss>ee,yes="NA",no=stat.melt$RD[i]) #date only if basal ripening is before melt out date
    
    
    i=i+1
  }
  
  ###"Propability of insulating snowcover
  stat.melt$MDr	<-MDr.sd-stat.melt$sd.day
  
  stat.melt$MD<-ifelse(stat.melt$MDr<0,NA,stat.melt$MD)
  stat.melt$RD<-ifelse(stat.melt$MDr<0,NA,stat.melt$RD)
  
  #Remove columns
  stat.melt$sd.day<-NULL
  stat.melt$FI<-NULL
  stat.melt$FDD<-NULL
  stat.melt$MDr<-NULL
  
  write.csv(stat.melt, out_path&out.matrix, quote =F, row.names =F)
  
}