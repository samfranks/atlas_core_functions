#### functions for processing grid references of varying types
#### Simon Gillings
#### Created December 2013


cat('Functions for processing grid references...\n')


#####################################################################################################################################################
# convert 1km reference to 10km reference -----------------------------------------------------------------------
#####################################################################################################################################################
gridconv1km.to.10km<-function(df,invar) 
  #df = the dataframe to process
  #invar = the name of the variable holding the grid reference
  {
  invar.index<-which(names(df)== invar)
  print(invar.index)
  df$tenkm<-paste(substr(df[,invar.index],1,3),substr(df[,invar.index],5,5),sep=''  )
  return(df)
}




#####################################################################################################################################################
# convert 10km reference to 50km reference -----------------------------------------------------------------------
#####################################################################################################################################################
gridconv10km.to.50km<-function(df,invar) 
  #df = the dataframe to process
  #invar = the name of the variable holding the grid reference
  {
  #which column to process
  invar.index<-which(names(df)== invar)
  #force name to be tenkm to make easier processing
  names(df)[invar.index]<-'tenkm'
  letters<-substr(df$tenkm,1,2)
  east<-as.numeric(substr(df$tenkm,3,3))
  north<-as.numeric(substr(df$tenkm,4,4))
  df$quadref<-NA
  df$quadref<-ifelse(east<=4 & north<=4, paste(letters,'SW',sep=''),df$quadref)
  df$quadref<-ifelse(east<=4 & north>4, paste(letters,'NW',sep=''),df$quadref)
  df$quadref<-ifelse(east>4 & north>4, paste(letters,'NE',sep=''),df$quadref)
  df$quadref<-ifelse(east>4 & north<=4, paste(letters,'SE',sep=''),df$quadref)
  return(df)
}



#####################################################################################################################################################
# convert 1km reference to tetrad reference -----------------------------------------------------------------------
#####################################################################################################################################################
gridconv01km.to.02km<-function(df,invar) 
  #df = the dataframe to process
  #invar = the name of the variable holding the grid reference
  {
  #which column to process
  invar.index<-which(names(df)== invar)
  #force name to be gridref to make easier processing
  names(df)[invar.index]<-'gridref'
  df$let<-substr(df$gridref,1,2)
  df$e10<-substr(df$gridref,3,3)
  df$e1<-substr(df$gridref,4,4)
  df$n10<-substr(df$gridref,5,5)
  df$n1<-substr(df$gridref,6,6)
  df$tet<-'o'
  df$tet<-ifelse(df$e1==0 & df$n1==0,'A',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==1,'A',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==0,'A',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==1,'A',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==0,'F',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==1,'F',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==0,'F',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==1,'F',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==0,'K',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==1,'K',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==0,'K',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==1,'K',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==0,'Q',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==1,'Q',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==0,'Q',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==1,'Q',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==0,'V',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==1,'V',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==0,'V',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==1,'V',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==2,'B',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==3,'B',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==2,'B',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==3,'B',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==2,'G',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==3,'G',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==2,'G',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==3,'G',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==2,'L',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==3,'L',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==2,'L',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==3,'L',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==2,'R',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==3,'R',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==2,'R',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==3,'R',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==2,'W',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==3,'W',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==2,'W',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==3,'W',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==4,'C',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==5,'C',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==4,'C',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==5,'C',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==4,'H',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==5,'H',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==4,'H',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==5,'H',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==4,'M',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==5,'M',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==4,'M',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==5,'M',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==4,'S',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==5,'S',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==4,'S',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==5,'S',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==4,'X',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==5,'X',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==4,'X',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==5,'X',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==6,'D',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==7,'D',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==6,'D',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==7,'D',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==6,'I',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==7,'I',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==6,'I',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==7,'I',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==6,'N',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==7,'N',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==6,'N',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==7,'N',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==6,'T',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==7,'T',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==6,'T',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==7,'T',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==6,'Y',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==7,'Y',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==6,'Y',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==7,'Y',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==8,'E',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==9,'E',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==8,'E',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==9,'E',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==8,'J',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==9,'J',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==8,'J',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==9,'J',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==8,'P',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==9,'P',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==8,'P',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==9,'P',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==8,'U',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==9,'U',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==8,'U',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==9,'U',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==8,'Z',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==9,'Z',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==8,'Z',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==9,'Z',df$tet)
  df$tetrad_id<-paste(df$let,df$e10,df$n10,df$tet,sep='')
  return(df)
}
