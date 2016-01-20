rm(list=ls())
setwd("~/CT_Registration/Segmentation/Segmentation_Paper/")

x = load("Reseg_Aggregate_models_Rigid_rf.Rda")

rf_modlist = modlist
rm(list= x)

save(rf_modlist, 
    file = "rf_modlist.rda",
    compress = "xz")

x = load("Reseg_Aggregate_models_Rigid_rf_smoothed.Rda")

smoothed_rf_cutoffs = modlist
rm(list= x)

save(smoothed_rf_cutoffs, 
    file = "smoothed_rf_cutoffs.rda",
    compress = "xz")

x = load("Reseg_Aggregate_models_Rigid_logistic.Rda")

logistic_modlist = modlist

save(logistic_modlist, 
    file = "logistic_modlist.rda",
    compress = "xz")


x = load("Reseg_Aggregate_models_Rigid_logistic_smoothed.Rda")

smoothed_logistic_cutoffs = modlist
rm(list= x)

save(smoothed_logistic_cutoffs, 
    file = "smoothed_logistic_cutoffs.rda",
    compress = "xz")


x = load("Reseg_Aggregate_data_cutoffs_Rigid.Rda")

rm(list= x[ x != "est.cutoffs"])


save(est.cutoffs, 
    file = "est.cutoffs.rda",
    compress = "xz")

