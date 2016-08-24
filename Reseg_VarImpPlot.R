rm(list=ls())
library(ichseg)
library(randomForest)
library(broom)
i = 1
lthresh = 40
uthresh = 80
rename_vec = c(
  "moment1" = "Neighborhood mean",
  "moment2" = "Neighborhood sd",
  "skew" = "Neighborhood skew",
  "kurtosis" = "Neighborhood kurtosis",
  "value" = "Image intensity (HU)",
  "thresh" = paste0("Threshold (≥ ", lthresh, " and ≤ ", uthresh, ")"),
  "zscore1" = "Within-plane coronal",
  "zscore2" = "Within-plane sagittal",
  "zscore3" = "Within-plane axial",
  "win_z" = "Winsorized standardized (20% trim)",
  "pct_thresh" = "Percentage thresholded neighbors",
  "prob_img" = "Atropos probability image",
  "pct_zero_neighbor" = "Percent of zero neighbors",
  "any_zero_neighbor" = "Indicator of any zero neighbors",
  "dist_centroid" = "Distance to image centroid",
  "smooth5" = "Gaussian smooth (σ = 5mm^3)",
  "smooth10" = "Gaussian smooth (σ = 10mm^3)",
  "smooth20" = "Gaussian smooth (σ = 20mm^3)",
  "zscore_template" = "Standardized-to-template intensity",
  "flipped_value" = "Contralateral difference"
)

imp = importance(rf_modlist$mod,
                 class = NULL,
                 scale = TRUE, type = NULL)

n.var = nrow(imp)
ord = rev(order(imp[, i], decreasing = TRUE)[1:n.var])
imp = imp[ord, , drop = FALSE]

coefs = broom::tidy(imp)
colnames(coefs)[1] = c("term")
coefs$term = plyr::revalue(coefs$term, rename_vec)
rownames(coefs) = coefs$term
coefs$term = NULL
coefs = as.matrix(coefs)
xmin = 0
pngname = ""
height = 7
width =7
pngname = "figures/Reseg_VarImpPlot.png"

png(pngname,
    height = height,
    width = width,
    units = "in",
    type = "cairo",
    res = 600)


dotchart(coefs[,1],
         xlab = "Mean Decrease in Gini Score",
         ylab = "",
         main = NULL,
         xlim = c(xmin, max(coefs[,1]))
)

dev.off()
plot_crop(pngname)
