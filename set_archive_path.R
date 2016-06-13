#### functions for processing grid references of varying types
#### Simon Gillings
#### Created December 2013


# set path to the BTO archives ---------------------------------------------------------------------------

#line of code needed for running on desktop pc
if(.Platform$OS =='windows') {
  cat('Archive path set for desktop use...\n')
  path.unix.archive<-'\\\\btodomain\\FILES\\UNIXArchive\\'
}

#line of code needed for running on BTO HPC
if(.Platform$OS=='unix') {
  cat('Archive path set for HPC use...\n')
  path.unix.archive<-'/archive/'
}

