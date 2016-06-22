########################################
# Get Google Travel Times: Combine Data
# John A. Graves
# June 22, 2016
#########################################
rm(list=ls())
pkg = list("Rcpp","haven","ggmap","plyr","RJSONIO","dplyr","doParallel")
invisible(lapply(pkg, require, character.only = TRUE))

setwd("~/Dropbox/Projects/hospital-quality-ambulance/analysis/")

N.Iter = 133

GoogleDistances <-  foreach (iter=1:N.Iter,.combine=rbind)  %do% 
{
  load(paste0("../data/google-distance/v1.0/google-distance-file-",iter,"-of-",N.Iter,".Rdata"))
  GoogleDistances50.H25
}


