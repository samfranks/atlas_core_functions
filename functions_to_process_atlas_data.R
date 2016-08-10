#### functions for manipulating atlas data including species selections, imposing chequerboard
#### Simon Gillings
#### Created December 2013

cat('Functions for processing atlas data...\n')

exclude.seabirds.on.speccode<-function(indata) {
  data<-subset(indata, !speccode %in% c(12,18,22,23,25,26,27,222,224,242,232,226,235,229,234,236,237,1282,1283,238,239,240,256,258,248,250,251,249,260,262,264,266))
  return(data)  
}
exclude.seabirds.on.cbc_code<-function(indata) {
  data<-subset(indata, !cbc_code %in% c('F.','MX','TM','TL','GX','CA','SA','AC','NX','KI','BH','MU','CM','LU','IN','LB','HG','YG','YC','IG','GZ','GB','AF','BJ','TE','CN','AE','RS','GU','RA','TY','PU','F_'))
  return(data)  
}

#exclude subspp, hybrids, domestics from a dataset using speccode
exclude.hybrids<-function(indata) {
  data<-subset(indata, !speccode %in% c(873,874,875,876,911,1021,1053,1152,1165,1247,1278,1279))
  return(data)  
}

#limit a dataset to the chequerboard in Ireland
limit2chequerboard.ireland<-function(indata) {
  indata$en<-as.numeric(substr(indata$tenkm,3,3))+as.numeric(substr(indata$tenkm,4,4))
  indata$en<-ifelse(substr(indata$tenkm,1,1)=='I',indata$en,NA)
  indata<-subset(indata, !en %in% c(0,2,4,6,8,10,12,14,16,18) )
  indata$en<-NULL
  return(indata)
}

#limit a dataset to the chequerboard in Britain
limit2chequerboard.britain<-function(indata) {
  indata$en<-as.numeric(substr(indata$tenkm,3,3))+as.numeric(substr(indata$tenkm,4,4))
  indata$en<-ifelse(substr(indata$tenkm,1,1) %in% c('H','N','T','S'),indata$en,NA)
  indata<-subset(indata, !en %in% c(0,2,4,6,8,10,12,14,16,18) )
  indata$en<-NULL
  return(indata)}
  
  
  ##remove channel islands and Ireland
  
  just.Britain<-function(indata,Ireland=c("yes","no")){
    ##select channel islands
  CI<-grep("WA",indata$tenkm)
  CI1<-grep("WV",indata$tenkm)
  ##remove these from the list
  indata<-indata[-c(CI,CI1),]
  if(Ireland=="no"){
  ##select Irish records and remove them
  indata<-indata[!substr(indata$tenkm,1,1)=="I",]
  } 
  return(indata)
 }
  
  
  ###remove all species with no confirmed or no probable or confirmed breeding evidence in the UK
  
  breeding.evidence<-function(indata,cat=c("cat","cat3"),level=c("probable","confirmed","B")){
    if(level=="probable"){
      if(cat=="cat"){species_list<-subset(indata, indata$cat>1)
      } else {
        species_list<-subset(indata, indata$cat70>1|indata$cat90>1|indata$cat2010b>1)
      }
    }
    if(level=="confirmed"){
      if(cat=="cat"){species_list<-subset(indata, indata$cat>2)
      } else {
        species_list<-subset(indata, indata$cat70>2|indata$cat90>2|indata$cat2010b>2)
      }
    }
    if(level=="B"){
      species_list<-subset(indata, indata$cat)
    }
    species_list<-unique(indata$speccode)
    indata<-subset(indata, indata$speccode %in% species_list)
    return(indata)
  }
 

