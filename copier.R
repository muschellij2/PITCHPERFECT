outdir = path.expand("~/Dropbox/PhD_Thesis/ich_chapter")
fnames = c("ich_chapter.Rnw",
           "ich_chapter.bib",
           "Reseg_Result_Formats_Rigid.Rda",
           "Scanning_Parameters.Rda",
           "Reseg_Aggregate_data_cutoffs_Rigid.Rda",
           "Reseg_111_Filenames_with_Exclusions.Rda",
           "Reseg_Results.Rda",
           "Reseg_Correlation_Results.Rda",
           "logistic_modlist.rda",
           "Patient_Demographics.csv",
           "loncaric_data.txt",
           "Imaging_Pipeline_Flowchart_with_Rigid.pdf",
           "figures/Reseg_Dice_Comparison.png",
           "figures/Reseg_Figure_DSI_Quantile_000_native.png",
           "figures/Reseg_Figure_DSI_Quantile_025_native.png",
           "figures/Reseg_Figure_DSI_Quantile_050_native.png",
           "figures/Reseg_Figure_DSI_Quantile_075_native.png",
           "figures/Reseg_Figure_DSI_Quantile_100_native.png",
           "figures/Reseg_Volume_Comparison.png")

fnames = c(fnames, 
           list.files( path = getwd(),
                       pattern = "Reseg_161-413_20110710_1619_CT_2_HEAD_Head_.*", 
                       recursive = TRUE)
)
stopifnot(all(file.exists(fnames)))
file.copy(fnames, to = file.path(outdir, fnames), overwrite = TRUE)
