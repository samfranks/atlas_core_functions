#### constants and settings used in various calculations and mapping
#### Simon Gillings
#### Created December 2013


cat('Create ATLAS object holding defaults and constants...\n')


#create list to hold all constants and defaults
ATLAS<-list()

# Set some constants
#number of 10-km squares in Britain, Ireland and B&I combined
ATLAS$constants$n10km.b<-2876
ATLAS$constants$n10km.b<-1018
ATLAS$constants$n10km.bi<-2876+1018


#Set some defaults
#colours as used in Atlas maps
ATLAS$colours$dist.breed<-rgb(221,89,46,max=255)
ATLAS$colours$dist.present<-'#363636'
ATLAS$colours$dist.winter<-rgb(51,166,217,max=255)
