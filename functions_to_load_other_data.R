#### functions for loading additional useful datasets
#### Simon Gillings
#### Created December 2013

cat('Functions for loading related data...\n')

#####################################################################################################################################################
# load the centroid coordinates on British National Grid (includes Ireland on same projection)
#####################################################################################################################################################
  load.coords<-function(res) 
  #res = the resolution of coordinates needed (002, 010, 020, 050, 100)
  {
  res<-ifelse(res==2,'002',res)
  res<-ifelse(res==10,'010',res)
  res<-ifelse(res==20,'020',res)
  res<-ifelse(res==50,'050',res)
  res<-ifelse(res==100,'100',res)
  if(res!='002') {
    data<-read.table(paste(path.unix.archive,'birdatlas2007-11/data/lookups/sas-staticdata/',res,'km_xy_coords.csv',sep=''),
                     sep=',',
                     header=T,
                     colClasses=c('character','numeric','numeric'))
  }
  if(res=='002') {
    data<-read.table(paste(path.unix.archive,'birdatlas2007-11/data/lookups/sas-staticdata/',res,'km_xy_coords.csv',sep=''),
                     sep=',',
                     header=T,
                     colClasses=c('character','character','numeric','numeric'))
    data<-data[,c(1,3,4)]
  }
  data$easting<-round(data$easting,digits=0)
  data$northing<-round(data$northing,digits=0)
  return(data)
}


#####################################################################################################################################################
# load dataset containing the land area per 10-km square ---------------------------------------------------------------
# currently two versions for BI low tide or GB high tide (high tide line not available for Ireland)
#####################################################################################################################################################
load.areas<-function(type='BIlow') {
  if(type=='BIlow') {
    data<-read.table(paste(path.unix.archive,'birdatlas2007-11/data/lookups/sas-staticdata/BI010_area_above_low_tide_line.csv',sep=''),
                     sep=',',
                     header=T,
                     colClasses=c('character','numeric'))
  }
  if(type=='GBhigh') {
    data<-read.table(paste(path.unix.archive,'birdatlas2007-11/data/lookups/sas-staticdata/GB010_area_above_mean_high_water.csv',sep=''),
                     sep=',',
                     header=T,
                     colClasses=c('character','numeric'))
  }
  return(data)
}


#####################################################################################################################################################
# load rule8 targets --------------------------------------------------------------------------------------------------------------------------------
# this is the number of tetrads that needed to be covered, and the number needed for analysis based on land area
# rule8 = number of squares needing coverage (8, or all with land if fewer than 8 have centre on land) 
# rule8adj = number of squares needed for analysis based on proportion of 10-km with land
#####################################################################################################################################################
load.targets<-function() {
  data<-read.csv(paste(path.unix.archive,'birdatlas2007-11/data/lookups/rule8target.csv', sep='')
                 ,header=T
                 ,colClasses=c('character','character','numeric','numeric')
  )
  return(data)
}

  
#####################################################################################################################################################
# load non-native definitions -----------------------------------------------------------------------------------------------------------------------
# highlights species as category C (established) or E (exotic), with separate coding for breeding and winter due to vagrancy of some species in winter
#####################################################################################################################################################
load.non.native.definitions<-function() {
  cat('speccode = numeric species code\n')
  cat('non_native_b = species definition for breeding data = C=established, E=exotic\n')
  cat('non_native_w = species definition for winter data = C=established, E=exotic\n')
  cat('tenkm = 10-km square\n')
  data<-read.csv(paste0(path.unix.archive,'birdatlas2007-11/data/lookups/non_native_categories.csv')
                 ,header=T
                 ,colClasses=c('numeric','character','character')
  )
  return(data)
}