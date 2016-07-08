#### functions for loading different atlas datasets
#### Simon Gillings
#### Created December 2013

cat('Functions for loading atlas data...\n')


#####################################################################################################################################################
# read full tetrad visit by species matrix of TTVs - first hour count only --------------------------------------------------------------------------
#####################################################################################################################################################
load.ttv.matrix.firsthour<-function() {
  cat('user_id = observer username\n')
  cat('tenkm = 10-km square\n')
  cat('tetrad_id = tetrad A:Z\n')
  cat('obsyear = year of visit\n')
  cat('obsmonth = month of visit\n')
  cat('obsday = day of visit\n')
  cat('season = B or W\n')
  cat('334 columns, one per species, containing 1st hour count. Names are numeric species codes with prefix _\n')
  data<-read.table(paste0(path.unix.archive,'birdatlas2007-11/data/processed/ttv_count_first_hour_only.csv'),
                   header=T,sep=",", 
                   colClasses=c(rep('character',3),rep('numeric',3),'character',rep('numeric',334) )
  )
  return(data)
}


#####################################################################################################################################################
# read full tetrad visit by species matrix of TTVs - mean count -------------------------------------------------------------------------------------
#####################################################################################################################################################
load.ttv.matrix.meancount<-function() {
  cat('user_id = observer username\n')
  cat('tenkm = 10-km square\n')
  cat('tetrad_id = tetrad A:Z\n')
  cat('obsyear = year of visit\n')
  cat('obsmonth = month of visit\n')
  cat('obsday = day of visit\n')
  cat('season = B or W\n')
  cat('346 columns, one per species, containing mean count across hours. Names are numeric species codes with prefix _\n')
  cat('duration = 1 or 2 hours\n')
  data<-read.table(paste0(path.unix.archive,'birdatlas2007-11/data/processed/ttv_mean_count_per_hour.csv'),
                   header=T,sep=",", 
                   colClasses=c(rep('character',3),rep('numeric',3),'character',rep('numeric',347) )
  )
  return(data)
}



#####################################################################################################################################################
# read the list of completed TTVs with date and duration info ---------------------------------------------------------------------------------------
#####################################################################################################################################################
load.ttv.details<-function() {
  cat('btoreg = 4-letter code for Atlas region\n')
  cat('user_id = observer username\n')
  cat('tenkm = 10-km square\n')
  cat('tetlet = tetrad A:Z\n')
  cat('datestring = visit date as a string, e.g. 19122007\n')
  cat('source = W=web submission; P=Paper submission\n')
  cat('duration = 1 or 2 hours\n')
  cat('obsdt = observation date as a Date object\n')
  data<-read.fwf(paste0(path.unix.archive,'birdatlas2007-11/data/raw/tetrads_and_duration.lst')
                 ,header=F
                 ,widths=c(4,-1,32,-1,4,-3,1,-1,8,-1,1,-1,-2,1)
                 ,col.names=c('btoreg','user_id','tenkm','tetlet','datestring','source','duration')
                 ,colClasses=c(rep('character',times=6),'numeric'))
  #convert datestring to date value and remove late data from local atlases
  data$obsdt <- as.Date(data$datestring, "%d%m%Y")
  data<-data[data$obsdt<as.Date("01/11/2011", format='%d/%m/%Y'),]
  return(data)
}


#####################################################################################################################################################
# load the processed maxcat file --------------------------------------------------------------------------------------------------------------------
#####################################################################################################################################################
load.maxcat<-function() {
  cat('tenkm = 10-km square\n')
  cat('season = Breeding or Winter\n')
  cat('speccode = numeric species code\n')
  cat('cat = highest breeding evidence, c(0,0.01,0.1,0.2,1,2,3) - see documentation\n')
  data<-read.table(paste0(path.unix.archive,'birdatlas2007-11/data/processed/maxcat.csv'),
                   header=T,sep=",", 
                   colClasses=c('character','character','numeric','numeric'))
  return(data)
}



#####################################################################################################################################################
# load the all maxcat file - processed distributions for all atlases --------------------------------------------------------------------------------
# (excludes out-of-season breeding records and early winter records for 2007-11 data)
#####################################################################################################################################################
load.allmaxcats<-function() {
  cat('tenkm = 10-km square\n')
  cat('speccode = numeric species code\n')
  cat('cat70 = breeding evidence 1968-72\n')
  cat('cat80 = present winter 1980s\n')
  cat('cat90 = breeding evidence 1988-91\n')
  cat('cat2010b = breeding evidence 2008-11\n')
  cat('cat2010w = present winter 2007-11\n')
  cat('File excludes out-of-season breeding records and early winter records for 2007-11 data')
  data<-read.table(paste0(path.unix.archive,'birdatlas2007-11/data/processed/allmaxcats4change.csv'),
                   header=T,sep=",", 
                   colClasses=c('character',rep('numeric',6)))
  return(data)
}



#####################################################################################################################################################
# load the processed maxcat file for change analyses ------------------------------------------------------------------------------------------------
# (excludes out-of-season breeding records and early winter records)
#####################################################################################################################################################
load.maxcat.change<-function() {
  cat('tenkm = 10-km square\n')
  cat('season = Breeding or Winter\n')
  cat('speccode = numeric species code\n')
  cat('cat = highest breeding evidence, c(1,2,3) - see documentation\n')
  cat('File excludes out-of-season breeding records and early winter records for 2007-11 data')
  data<-read.table(paste0(path.unix.archive,'birdatlas2007-11/data/processed/maxcat4change.csv'),
                   header=T,sep=",", 
                   colClasses=c('character','character','numeric','numeric'))
  return(data)
}



#####################################################################################################################################################
# load dataset abundance data on 10-km grid ---------------------------------------------------------------------------------------------------------
#####################################################################################################################################################
load.abundance.10km.data<-function() {
  cat('tenkm = 10-km square\n')
  cat('season = Breeding or Winter\n')
  cat('pocc = proportion of tetrads in 10-km occupied\n')
  cat('speccode = numeric species code\n')
  cat('meancount = mean of counts (averaged across hours, then across visits)\n')
  cat('cat = maximum breeding evidence recorded in 10-km (1:3; fixed at 3 for winter records)\n')
  cat('ttv = dummy variable, should be deleted\n')
  cat('Not, for breeding season data, this file has had 10-km squares removed if there was no breeding evidence\n')
  data<-read.table(paste0(path.unix.archive,'birdatlas2007-11/data/processed/results_abundbysp2010_10km.csv'),
                   header=T,sep=",", 
                   colClasses=c('character','character','numeric','numeric','numeric','numeric','numeric' ))
  return(data)
}


#####################################################################################################################################################
# load dataset abundance data on 20-km grid ---------------------------------------------------------------------------------------------------------
#####################################################################################################################################################
load.abundance.20km.data<-function() {
  cat('segref = 20-km square\n')
  cat('season = Breeding or Winter\n')
  cat('speccode = numeric species code\n')
  cat('ntets = number of tetrads surveyed in 20-km\n')
  cat('meancount = mean of counts (averaged across hours, then across visits)\n')
  cat('freqindex = proportion of tetrads in 20-km occupied\n')
  data<-read.table(paste0(path.unix.archive,'birdatlas2007-11/data/processed/results_abundbysp2010_20km.csv'),
                   header=T,sep=",", 
                   colClasses=c('character','character','numeric','numeric','numeric','numeric' ))
  return(data)
}



#####################################################################################################################################################
# load dataset of breeding abundance changes on 20-km grid ------------------------------------------------------------------------------------------
#####################################################################################################################################################
load.abundance.change.20km.data<-function() {
  cat('segref = 20-km square\n')
  cat('cbc_code = 2-letter species code\n')
  cat('index90 = proportion of tetrads occupied in 1988-91\n')
  cat('index10 = proportion of tetrads occupied in 2008-11\n')
  cat('diff = arithmetic difference in proportion of tetrads occupuied between atlases\n')
  cat('cat = classification of change (-5 to +5) for producing map\n')
  cat('easting = British National grid easting\n')
  cat('northing = British National grid northing\n')
  data<-read.table(paste0(path.unix.archive,'birdatlas2007-11/data/processed/results_abchange_by_spp_20km.csv'),
                   header=T,sep=",", 
                   colClasses=c('character','character',rep('numeric',6 )))
  return(data)
}



#####################################################################################################################################################
# load change data ----------------------------------------------------------------------------------------------------------------------------------
#####################################################################################################################################################
load.change.data<-function() {
  cat('tenkm = 10-km square\n')
  cat('speccode = numeric species code\n')
  cat('change = class of change: STA, INC, DEC, MIDDEC, OLDDEC etc\n')
  cat('interval = interval between atlases over which calculated\n')
  cat('season = Breeding or Winter\n')
  cat('cbc_code = 2-letter species code\n')  
  data<-read.table(paste0(path.unix.archive,'birdatlas2007-11/data/processed/distribution_changes10km.csv'),
                   header=T,sep=",", 
                   colClasses=c('character','numeric','character','character','character','character' ))
  names(data)[6]<-'cbc_code'
  return(data)
}



#####################################################################################################################################################
# load the fixed effort distributions for a species -------------------------------------------------------------------------------------------------
#####################################################################################################################################################
# load.fe.distributions<-function(cbc_code) {
#   data<-read.table(paste(path.unix.working,'fixed_effort_distributions/csv/',cbc_code,'.csv', sep=""),header=T,sep=",", colClasses=c('character','character','numeric','numeric','numeric'))
#   #data<-read.table(paste(path.unix.working,'my_perms/fedistributions/',cbc_code,'.csv', sep=""),header=T,sep=",", colClasses=c('character','character','numeric','numeric','numeric'))
#   return(data)
# }



#####################################################################################################################################################
# load the fixed effort range sizes and changes -----------------------------------------------------------------------------------------------------
#####################################################################################################################################################
# load.fe.rangesizes<-function() {
#   data<-read.table(paste(path.unix.working,'fixed_effort_distributions/csv/contents.csv', sep=""),header=T,sep=",", colClasses=c('character','character','numeric','numeric'))
#   #data<-read.table(paste(path.unix.working,'my_perms/fedistributions/contents.csv', sep=""),header=T,sep=",", colClasses=c('character','character','numeric','numeric'))
#   return(data)
# }


#####################################################################################################################################################
#load info on which tetrads were surveyed in 1988-91 and 2008-11 for fixed effort calculations ------------------------------------------------------
#####################################################################################################################################################
load.fixed.effort.ttv.coverage<-function()
  {
  cat('tenkm = 10-km square\n')
  cat('tetrad_id = tetrad letter\n')
  cat('eligible = centre on land (Y/N)\n')
  cat('hrs2010early = number of hours for early breeding season 2008-11 visit\n')
  cat('hrs2010late = number of hours for late breeding season 2008-11 visit\n')
  cat('num_forms_1990 = number of forms containing data for 1988-91\n')
  cat('sum_visits_1990 = total number of visits to tetrad in 1988-91\n')
  cat('cov2010 = was tetrad visited in 2008-11 (1/NA)\n')
  cat('cov1990 = was tetrad visited in 1988-91 (1/NA)\n')
  data<-read.table(paste0(path.unix.archive,'birdatlas2007-11/data/processed/fixed_effort_coverage_1990_2010.csv'),
                   header=T,sep=",", 
                   colClasses=c('character','character','character',rep('numeric',6) ))
  return(data)
}



#####################################################################################################################################################
#load the safe-for-fixed-effort-calcs TTV data for 88-91 -------------------------------------------------------------------------------------------------------------------
#####################################################################################################################################################
load.ttv.safe8891<-function(ruleset) {
  cat('tenkm = 10-km\n')
  cat('tetrad_id = tetrad letter\n')
  cat('n.visits = number of visits\n')
  cat('cbc_code = 2-letter species code\n')
  cat('present0 = species was recorded (1/NA)\n')
  cat('count = number of individuals for count species (numeric/NA)\n')
  data<-read.table(paste0(path.unix.archive,'birdatlas1988-91breed/data2km_safe/excess_coverage_removed_ruleset',ruleset,'.csv'),
                   header=T,sep=",", 
                   colClasses=c('character','character','numeric','character','numeric','numeric' ))
  return(data)
}



#####################################################################################################################################################
# species lookup-------------------------------------------------------------------------------------------------------------------------------------
#####################################################################################################################################################
load.specnames<-function() {
  cat('speccode = numeric species code\n')
  cat('cbc_code = 2-letter species code\n')  
  cat('engname = species common name\n')
  cat('sciname = species scientific name\n')
  cat('bou_order = species order at time of Atlas\n')
  data<-read.table(paste0(path.unix.archive,'/birdatlas2007-11/data/lookups/specnames.csv'),
                   sep=',',
                   quote = "",
                   header=T,
                   colClasses=c('numeric','character','character','character','numeric')
  )
  return(data)
}



#####################################################################################################################################################
# across-atlas coverage information at 10-km --------------------------------------------------------------------------------------------------------
#####################################################################################################################################################
load.coverage.data.10km<-function() {
  cat('tenkm = 10-km\n')
  cat('breed1970to1990 = 1 = 10-km covered in both atlases\n')
  cat('breed1970to2010 = 1 = 10-km covered in both atlases\n')
  cat('breed1990to2010 = 1 = 10-km covered in both atlases\n')
  cat('breed197019902010 = 1 = 10-km covered in all three atlases\n')
  cat('winter1980to2010 = 1 = 10-km covered in both atlases\n')
  data<-read.table(paste0(path.unix.archive,'birdatlas2007-11/data/processed/coverage4change.csv'),
                   sep=',',
                   header=T,
                   colClasses=c('character',rep('numeric',5))
  )
  return(data)
}