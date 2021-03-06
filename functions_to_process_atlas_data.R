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
      } 
      if(cat=="cat3"){
        species_list<-subset(indata, indata$cat70>1|indata$cat90>1|indata$cat2010b>1)
      }
    }
    if(level=="confirmed"){
      if(cat=="cat"){species_list<-subset(indata, indata$cat>2)
      } 
      if(cat=="cat3"){
        species_list<-subset(indata, indata$cat70>2|indata$cat90>2|indata$cat2010b>2)
      }
    }
    if(level=="B"){
      species_list<-subset(indata, indata$cat)
    }
    species_list<-unique(species_list$speccode)
    indata<-subset(indata, indata$speccode %in% species_list)
    return(indata)
  }
 

###function to remove exotic species
  ##currently needs specode, but should work with cbc code too
  remove.exotics<-function(indata,season=c("b","w")){
  non_native<-read.csv(file=paste0(path.unix.archive,'birdatlas2007-11/data/lookups/non_native_categories.csv'),header=T)
  ##subset just to get exotics
  if(season=="b"){
  exotic<-subset(non_native,non_native_b=="E")
  }
  if(season=="w"){
    exotic<-subset(non_native,non_native_w=="E")
  }
  specnames<-load.specnames()
  exotic<-merge(exotic, specnames[,c(1,3)])
  ##want to remove all the species which get E for the non native breeding season
  indata<-subset(indata, !indata$speccode %in% exotic$speccode)
  return(indata)
  }
  
  
  ##function to plot a map of surveyed tetrads/tetrads selected for a certain habitat type
  ##specify appropriate colour vector
  ##specify point size for the plotted points (this will depend on how many points you have you want to plot)
  ##specify filename for saved plot, needs to end in .tiff or .png or other pciture file type
  
  plot.map<-function(indata, country=c("britain", "ireland","channel_isles"), colours,point_size, savename){
    library(ggplot2)
    library(rgdal)
    ##might be useful to adapt this so it'll plot all Britain and Ireland at once?
    map<-readOGR(paste0(path.unix.archive,"birdatlas2007-11/data/lookups/needed/gis_outlines"),paste0("atlas_outline_",country,"_only"))
    map<-fortify(map)
    
  p<-ggplot() +
    geom_point(data=indata, aes(x =easting, y = northing), colour=colours, size=point_size) +
    geom_polygon(data=map, aes(x=long, y=lat, Group=group),colour="black", fill=NA)+
    coord_fixed(ratio=1)+
    theme(text = element_text(family = "serif")) +
    theme(plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text =element_blank(), legend.title=element_blank(), legend.key=element_blank())
  print(p)
  ggsave(savename, units = "cm", dpi=800)
  }
  
  

  ###function to calculate species range changes and create boxplots of the results
 ##the downside is this function si so long its really difficult to check properly
  ##it returns a result but its hard to check its doing exactly what I want it to do!
  
  ##next step- calculate range change for each species in the squares for each habitat
  ##indata is a dataframe with tetrad/tenkm id in the first column
  #speccode in the second column
  ##the rest of the columns are years of the atlas in order earlier first
  ##for abundance data the column after the abundance values for each year is the difference column
  
   range_change<-function(indata,type=c("presence","abundance"),years=c(1,2,3), restrict_range=FALSE,restricted_range_limit=NULL){
    
    if(years==1){
      hab<-indata[indata[,3]>0,]
      
      ##get unique list of species
      sp<-unique(hab[,2])
      
       #create a matrix for store the information in
      sp_range<-matrix(nrow=length(sp), ncol=3)
      ##first column contains the list of sp
      sp_range[,1]<-sp
      ##assign column names
      colnames(sp_range)<-c("speccode","PA")
      
      ##sum number of occupied tenkm squares for each species for each year
      for (i in 1:length(sp)){
        sp_hab<-subset(hab, hab[,2]==sp[i])
        sp_range[i,2]<-sum(sp_hab[,3])
      }
      ##change to data frame
      sp_range<-as.data.frame(sp_range)
      
      ##option to exclude rare species
      if (restrict_range==TRUE){
       sp_range<-sp_range[sp_range$PA>restricted_range_limit,]}
 
      return(sp_range)
    }
    
    if(years==2){
    
      if (type=="presence"){
        
        hab<-indata[indata[,3]>0|indata[,4]>0,]
        ##get unique list of species
        sp<-unique(hab[,2])
         #create a matrix for store the information in
        sp_range<-matrix(nrow=length(sp), ncol=3)
        ##first column contains the list of sp
        sp_range[,1]<-sp
      ##assign column names
      colnames(sp_range)<-c("speccode","PA90","PA2010")
      
      ##sum number of occupied tenkm squares for each species for each year
      for (i in 1:length(sp)){
        sp_hab<-subset(hab, hab[,2]==sp[i])
        sp_range[i,2]<-sum(sp_hab[,3])
        sp_range[i,3]<-sum(sp_hab[,4])
      }
      ##change to data frame
      sp_range<-as.data.frame(sp_range)
      
      ##option to exclude rare species
     if (restrict_range==TRUE){
      sp_range<-sp_range[sp_range$PA90>restricted_range_limit|sp_range$PA2010>restricted_range_limit,]
     }   
      ##difference
      sp_range$diff90to2010<-sp_range$PA2010-sp_range$PA90
   
      
      ##percentage change
      sp_range$change90to2010<-(sp_range$PA2010-sp_range$PA90)/sp_range$PA90
      
      ##log ratio
      sp_range$logR90to2010<-rep(NA, length(sp_range[,1]))
      
      ##calculate the log ratio
      for (p in 1:length(sp_range[,1])){
        if (sp_range$PA90[p]>0){
          sp_range$logR90to2010[p]<-log10(sp_range$PA2010[p]/sp_range$PA90[p])
        }
      }}
      if(type=="abundance"){
        
        hab<-indata[indata[,3]>0|indata[,4]>0,]
        
        ##get unique list of species
        sp<-unique(hab[,2])
        ##and look at aggregation
        library(ineq)
    
          #create a matrix for store the information in
          sp_range<-matrix(nrow=length(sp), ncol=10)
          
          ##assign column names
          colnames(sp_range)<-c("cbc_code","mean_index_90","mean_index_10","total_abund_1990","total_abund_2010","mean_diff","sum_diff", "Gini_1990", "Gini_2010", "no._squares")
          
          for (i in 1:length(sp)){
            sp_hab<-subset(hab, hab[,2]==sp[i])
            ##look at mean abundance diff
            sp_range[i,2]<-mean(sp_hab[,3])
            sp_range[i,3]<-mean(sp_hab[,4])
            ##and total abundance diff
            sp_range[i,4]<-sum(sp_hab[,3])
            sp_range[i,5]<-sum(sp_hab[,4])
            ##look at mean difference across sqs and total difference
            sp_range[i,6]<-mean(sp_hab[,5])
            ##look at total difference
            sp_range[i,7]<-sum(sp_hab[,5])
            ##it'd also be interesting to look at distribution here somehow
            ##get Gini coefficient for every species
            sp_range[i,8]<-ineq(sp_hab[,3], type="Gini")
            sp_range[i,9]<-ineq(sp_hab[,4], type="Gini")
            sp_range[i,10]<-length(sp_hab[,1])
          }
          ##change to data frame
          sp_range<-as.data.frame(sp_range)
          
          ##first column contains the list of sp
          ##for some reason if I add this first all the numeric columns become factors
          ##don't know why.....
          sp_range[,1]<-sp
          
          ##option to exclude rare species
          if (restrict_range==TRUE){
            sp_range<-sp_range[sp_range$no._squares>restricted_range_limit|sp_range$no._squares>restricted_range_limit,]
          } 
          
          ##percentage change
          sp_range$change90to2010<-(sp_range$total_abund_2010-sp_range$total_abund_1990)/sp_range$total_abund_1990
          
          ##log ratio change
          sp_range$logR90to2010<-rep(NA, length(sp_range[,1]))
          
          ##calculate the log ratio
          for (p in 1:length(sp_range[,1])){
            if (sp_range$total_abund_1990[p]>0){
              sp_range$logR90to2010[p]<-log10(sp_range$total_abund_2010[p]/sp_range$total_abund_1990[p])
            }
          }
      
      }
      return(sp_range)
    }
          
    if(years==3){
    hab<-indata[indata[,3]>0|indata[,4]>0|indata[,5]>0,]
  
    ##get unique list of species
    sp<-unique(hab[,2])
    
    #create a matrix for store the information in
    sp_range<-matrix(nrow=length(sp), ncol=4)
    ##first column contains the list of sp
    sp_range[,1]<-sp
    ##assign column names
    colnames(sp_range)<-c("speccode","PA70","PA90","PA2010")
    
    ##sum number of occupied tenkm squares for each species for each year
    for (i in 1:length(sp)){
      sp_hab<-subset(hab, hab[,2]==sp[i])
      sp_range[i,2]<-sum(sp_hab[,3])
      sp_range[i,3]<-sum(sp_hab[,4])
      sp_range[i,4]<-sum(sp_hab[,5])
    }
    ##change to data frame
    sp_range<-as.data.frame(sp_range)
    
    ##option to exclude rare species
    if (restrict_range==TRUE){
    sp_range<-sp_range[sp_range$PA70>restricted_range_limit|sp_range$PA90>restricted_range_limit|sp_range$PA2010>restricted_range_limit,]
    }
    
    ##this is difference
    sp_range$diff70to2010<-(sp_range$PA2010-sp_range$PA70)
    sp_range$diff90to2010<-(sp_range$PA2010-sp_range$PA90)
  
    
    ##percentage change
    sp_range$change70to2010<-(sp_range$PA2010-sp_range$PA70)/sp_range$PA70
    sp_range$change90to2010<-(sp_range$PA2010-sp_range$PA90)/sp_range$PA90
    
    sp_range$logR70to2010<-rep(NA, length(sp_range[,1]))
    sp_range$logR90to2010<-rep(NA, length(sp_range[,1]))
    
    ##calculate the log ratio
    for (p in 1:length(sp_range[,1])){
      if (sp_range$PA70[p]>0){
        
        sp_range$logR70to2010[p]<-log10(sp_range$PA2010[p]/sp_range$PA70[p])
        
      }
      if (sp_range$PA90[p]>0){
        sp_range$logR90to2010[p]<-log10(sp_range$PA2010[p]/sp_range$PA90[p])
      }
    }
    
  return(sp_range)
    
    }}
   

   ##function to stack list data and add names from a specified vector
   
   stack<-function(indata, habitats, write.csv=FALSE, savename=NULL){
     library(taRifx)
     sp_trends<-stack.list(indata, label=T)
     
     for (i in 1:length(habitats)){
       sp_trends$from[sp_trends$from==i]<-habitats[i]
     }
     
     n<-ncol(sp_trends)
     
     colnames(sp_trends)[n]<-"Habitat"
     
     if(write.csv==TRUE){
       write.csv(sp_trends, file=savename)
     }
     
     return(sp_trends)
   }
   

   ###function to take species ranges and display the data in boxplots
  #need in data
   ##habitats is a vector of the different habitat types/ or other variable that was used to group the species 
   ##colours- specify colours for each habitat/group in the order in which they appear in the habitats vector
   #plotvar = name of the variable you want to plot
   #x column with data from var want to plot
   ##could also change this to plot multiple boxplots at once?
   
   boxplot_range_change<-function(indata, habitats,x, plotvar,savename=NULL, colours, y1, y2){
     
     
     library(ggplot2)
     library(gridExtra)
     library(grid)
     library(lattice)
     
     
     p<-ggplot(indata, aes(x=Habitat, y=indata[,x], fill=Habitat)) + geom_boxplot(notch=F, outlier.shape =NA)+
       scale_fill_manual(name="Habitat", values=colours)+
       ylab(plotvar)+
       theme(plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
       scale_y_continuous(limits=c(y1,y2))+
       theme_bw() +
       theme(text = element_text(family = "serif"))+
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             axis.text.x=element_text(angle=90, hjust=1),
             legend.background = element_rect(fill = NA), legend.key = element_blank())
     print(p)
     return(p)
     
   }
  
  #######function to plot two plots side by side and make them share a legend
  ##took this from the internet but it seems to work well
  ##need to specify arrangement of graphs and legend
  grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
    library(gridExtra)
    library(grid)
    library(ggplot2)
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
    gl <- c(gl, nrow = nrow, ncol = ncol)
    
    combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                              legend,
                                              ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend,
                                             ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
    grid.newpage()
    grid.draw(combined)
    
  }
  
  ##########################################################################################
  ############read in atlas data and process to appropriate form for most  analyses####
  #############so separate Britain and Ireland, remove channel islands, remove exotic and hybrid species
  ####remove species that are not confirmed UK breeders for presence data- I think this isn't a problem for abundance data..
  ####does Britain abundance data also need to be limited to a chequerboard?
  ####################################################################################
  
  data_to_use<-function(country=c("B","I"), type=c("abund","pres")){
    if(country=="B" & type=="pres"){
      country<-"B"
      data<-load.allmaxcats()
      head(data)
      ##but only want breeding atlas
      ##so get rid of wintering columns
      data<-data[,-c(4,7)]
      ##get rid of hybrids
      data<-exclude.hybrids(data)
      data<-just.Britain(data,Ireland="no")
      ##na's to zeros
      data[is.na(data)]<-0
      ##SUBSET CONFIRMED BREEDING
      sp_confirm<-subset(data, data$cat70>2|data$cat90>2|data$cat2010b>2)
      head(sp_confirm)
      ##unique list
      sp_confirm<-unique(sp_confirm$speccode)
      data<-subset(data, data$speccode %in% sp_confirm)
      ##select the right part of the dataframe
      data_cat<-data[,3:5]
      ##replace with 1's
      data_cat[data_cat>0.2]<-1
      ##now recombine
      data<-cbind(data[,1:2],data_cat)
      ##change column titles to PA for each atlas
      colnames(data)<-c("tenkm","speccode","PA70", "PA90", "PA2010")
      ##remove exotics
      data<-remove.exotics(data, season="b")
      ##load species names and codes
      specnames<-load.specnames()
      head(specnames)
      data<-merge(data,specnames[,c(1,3)])
    }
    if(country=="I" & type=="pres"){
      country<-"I"
      data<-load.allmaxcats()
      head(data)
      ##but only want breeding atlas
      ##so get rid of wintering columns
      data<-data[,-c(4,7)]
      ##get rid of hybrids
      data<-exclude.hybrids(data)
      data<-indata[!substr(data$tenkm,1,1)=="I",]
      ##na's to zeros
      data[is.na(data)]<-0
      ##SUBSET CONFIRMED BREEDING
      sp_confirm<-subset(data, data$cat70>2|data$cat90>2|data$cat2010b>2)
      head(sp_confirm)
      ##unique list
      sp_confirm<-unique(sp_confirm$speccode)
      data<-subset(data, data$speccode %in% sp_confirm)
      ##select the right part of the dataframe
      data_cat<-data[,3:5]
      ##replace with 1's
      data_cat[data_cat>0.2]<-1
      ##now recombine
      data<-cbind(data[,1:2],data_cat)
      ##change column titles to PA for each atlas
      colnames(data)<-c("tenkm","speccode","PA70", "PA90", "PA2010")
      ##remove exotics
      data<-remove.exotics(data, season = "b")
      ##load species names and codes
      specnames<-load.specnames()
      head(specnames)
      data<-merge(data,specnames[,c(1,3)])
    }
    if(country=="B" & type=="abund"){   
      ##load abundance data
      data<-read.csv(file=paste0(hpc.path,"data_files/results_abchange_by_spp_10km.csv"),header=T)
      data<-just.Britain(data,Ireland="no")
      ##exclude seabirds
      data<-exclude.seabirds.on.cbc_code(data)
      ##load species names and codes
      specnames<-load.specnames()
      head(specnames)
      data<-merge(data,specnames[,c(1:3)])
      ##remove exotics
      data<-remove.exotics(data, season="b")
    }
    if(country=="I" & type=="abund") {
      ##load abundance data
      data<-read.csv(file=paste0(hpc.path,"data_files/results_abchange_by_spp_10km.csv"),header=T)
      ##just ireland 
      data<-subset(data,substr(data$tenkm,1,1)=="I")
      ##chequerboard
      data<-limit2chequerboard.ireland(data)
      ##exclude seabirds
      data<-exclude.seabirds.on.cbc_code(data)
      ##load species names and codes
      specnames<-load.specnames()
      head(specnames)
      data<-merge(data,specnames[,c(1:3)])
      ##remove exotics
      data<-remove.exotics(data, season="b")
    }
    return(data)
  }