## ----label=opts, results='hide', echo=FALSE, message = FALSE, warning=FALSE----
library(knitr)
# knit_hooks$set(webgl = hook_webgl)
opts_chunk$set(echo=FALSE, prompt=FALSE, message=FALSE, warning=FALSE, comment="", results='hide')

## ----load_res, eval = TRUE, echo = FALSE---------------------------------
#rda = file.path(resdir, "Result_Formats.Rda")
rda = "Reseg_Result_Formats_Rigid.Rda"
xx = load(rda)
Nmods = non.aggmods
rm(list=xx)

## ----echo = FALSE--------------------------------------------------------
loader = function(x, varnames, replacer = " ") {
	L = vector(mode = "list", length = length(varnames))
	names(L) = varnames
	for (i in varnames) {
		L[[i]] = replacer
	}
	if (file.exists(x)) {
		rr = load(x)
		for (i in varnames) {
			xx = get(i)
			if (is.null(xx)){
				xx = replacer
			}
			L[[i]] = xx
		}
	}
	return(L)
}
L = loader("Number_of_Non_Nmods_Abstract.rda", c("non_nmods", "total_N"))
non_nmods = L$non_nmods
total_N = L$total_N
med_dice = loader("Median_Dice_Abstract.rda", "med_dice")$med_dice
wt_pvals = loader("Wilcoxon_Rank_pvalues_Abstract.rda", "wt_pvals")$wt_pvals
corrdata = loader("Correlation_data_Abstract.rda", "corrdata", replacer = NULL)$corrdata

## ----label=setup_dir, echo=FALSE-----------------------------------------
ll = ls()
ll = ll[ !ll %in% c("encoding", "Nmods")]
rm(list = ll)
library(tidyr)
library(cttools)
library(fslr)
library(plyr)
library(dplyr)
library(reshape2)
library(broom)
library(xtable)
options(matlab.path = '/Applications/MATLAB_R2014b.app/bin')

# username <- Sys.info()["user"][[1]]
rootdir = path.expand("~/CT_Registration")


# basedir = file.path(rootdir, "Segmentation")
# resdir = file.path(basedir, "results")
# paperdir = file.path(basedir, "Segmentation_Paper")
# figdir = file.path(paperdir, "figure")

# total_rda = file.path(basedir, "111_Filenames_with_volumes_stats.Rda")
# load(total_rda)
# fdf$patientName = as.numeric(gsub("-", "", fdf$id))

# csvname = file.path(rootdir, "data", "Imaging_Information.csv")
# imag = read.csv(csvname, stringsAsFactors = FALSE)
# imag$patientName = imag$id
# imag$id = NULL
#
# setdiff(fdf$patientName, imag$patientName)
xxx = load("Reseg_111_Filenames_with_Exclusions.Rda")
groups = fdf[, c("id", "group")]

rda = "Scanning_Parameters.Rda"
load(rda)

num_pid_to_id = function(x){
  site_id = floor(x/1000)
  id = x %% 1000
  paste0(site_id, "-", id)
}

fdf$site_number = sapply(strsplit(fdf$id, "-"), `[[`, 1)
fdf$pid = as.numeric(gsub("-", "", fdf$id))

man = lapply(alltabs, function(x) {
  unique(x[["0008-0070-Manufacturer"]])
})
stopifnot(all(sapply(man, length) == 1))
man = unlist(man)
models = lapply(alltabs, function(x) {
  unique(x[[ "0008-1090-ManufacturerModelName"]])
})
stopifnot(all(sapply(models, length) == 1))
models = unlist(models)

fdf$model = models
fdf$model[fdf$id == "131-354"] = ""
fdf$model[fdf$id == "179-402"] = ""

fdf$man = man
fdf$man[fdf$man == 'TOSHIBA'] = "Toshiba"
fdf$man[fdf$man == 'SIEMENS'] = "Siemens"
man_fdf = fdf[, c("id", "pid", "man", "model")]
man_fdf = left_join(man_fdf, groups, by = "id")
# man = sapply(man, unique)

stopifnot(nrow(fdf) == 112)
stopifnot(length(unique(fdf$id)) == 112)
#####################
# Running manufacturers
#####################
man.tab = sort(table(man), decreasing=TRUE)
stopifnot(length(man.tab) == 4)
manu = names(man.tab)
manu[manu == 'TOSHIBA'] = "Toshiba"
manu[manu == 'SIEMENS'] = "Siemens"

man.tab = paste0(manu, " ($N=", man.tab, "$)")
man.tab[length(man.tab)] = paste0('and ', man.tab[length(man.tab)] )
man.tab[seq(length(man.tab)-1)] = paste0(man.tab[seq(length(man.tab)-1)], ", " )
man.tab = paste(man.tab, collapse= " ")

train.man.tab = table(man_fdf$man[ man_fdf$group == "Train"])
train.man.tab = paste0(names(train.man.tab), " (N = ", train.man.tab, "),")
train.man.tab[length(train.man.tab)] = paste0("and ", train.man.tab[length(train.man.tab)])
train.man.tab[length(train.man.tab)] = gsub(",$", "", train.man.tab[length(train.man.tab)])
train.man.tab = paste(train.man.tab, collapse = " ")
man_fdf$group = NULL



#####################
# Running slice thickness
#####################
slices = lapply(alltabs, `[[`, "0018-0050-SliceThickness")
slices = sapply(slices, function(x) {
	stopifnot(all(!is.na(x)))
	stopifnot(all(x != ""))
	length(unique(x))
})
alltabs.id = sapply(alltabs, function(x) {
    x = rownames(x)[1]
    gsub(".*/Registration/(.*)/Sorted/.*", "\\1", x)
  })
n.slices = sum(slices > 1)
slice_df = data.frame(id = alltabs.id,
                      var_slice = slices > 1,
                      stringsAsFactors = FALSE)

tilt = lapply(alltabs, `[[`, "0018-1120-GantryDetectorTilt")
tilt = sapply(tilt, unique)
tilt = as.numeric(tilt)

check.na = function(x){
  stopifnot(all(!is.na(x)))
}
check.na(tilt)
n.gant = sum(tilt != 0)
#"102-323" added over the 111 pts

all_and_ices = load("All_IncludingICES_Patients.Rda")
stopifnot(all(all_and_ices == "all.alldat"))
ivh = all.alldat
rm(list= "all.alldat")
# ivh = read.csv("All_Patients.csv", stringsAsFactors = FALSE)
ivh = ivh[, c("Pre_Rand_IVHvol", "Pre_Rand_ICHvol", "patientName")]
ivh$id = num_pid_to_id(ivh$patientName)
ivh$patientName = NULL

demog = read.csv("Patient_Demographics.csv",
                 stringsAsFactors = FALSE)
demog = demog[,c("Age", "Gender", "Ethnicity", "patientName")]
demog_icc = demog[ demog$patientName %in% unique(fdf$pid), ]
mean.age = round(mean(demog_icc$Age), 1)
sd.age = round(sd(demog_icc$Age), 1)

mean.male = round(prop.table(table(demog_icc$Gender))['Male'] * 100, 1)
race = sort(round(prop.table(table(demog_icc$Ethnicity)) * 100, 1), decreasing = TRUE)

race = paste0(race, "\\% ", names(race))
race[length(race)] = paste0("and ", race[length(race)])
race = gsub(" not Hispanic", "", race)
race = gsub("Islander", "islander", race)
race = paste0(race, collapse = ", ")

pvaller = function(x, min.pval = 0.05){
	mp = signif(min.pval, 1)
	ifelse(x < min.pval, paste0("< ", mp),
	paste0("= ", signif(x, 2)))
}

## ------------------------------------------------------------------------
xx = load("Reseg_Aggregate_data_cutoffs_Rigid.Rda")

## ----threshes------------------------------------------------------------
lthresh = 40
uthresh = 80

## ------------------------------------------------------------------------
nsub_voxels = 1e5
nsub_voxels = formatC(nsub_voxels, digits=7, big.mark="{,}")
prop = 0.25
pct_prop = sprintf("%02.2f", prop * 100)

## ------------------------------------------------------------------------
reset = FALSE
if ("fdf" %in% ls()){
	xfdf = fdf
}
total_N = nrow(fdf)
load("Reseg_111_Filenames_with_Exclusions.Rda")
voldf = fdf
if (reset) {
  stopifnot(all.equal(sort(voldf$id), sort(xfdf$id)))
	fdf = xfdf
}
sp_pct = function(x, digits = 1, addpct = FALSE){
  x = sprintf(paste0("%02.", digits, "f"), x * 100)
  if (addpct){
    x = paste0(x, "%")
  }
  x
}
vol_res = ddply(voldf, .(group), summarise,
                mean_includeY1 = sp_pct(mean(includeY1)),
                min_includeY1 = sp_pct(min(includeY1)),
                max_includeY1 = sp_pct(max(includeY1)),
                mean_excludeY0 = sp_pct(mean(1-includeY0)),
                min_excludeY0 = sp_pct(min(1-includeY0)),
                max_excludeY0 = sp_pct(max(1-includeY0))
                )
rownames(vol_res) = vol_res$group
vol_res$group = NULL
rn = rownames(est.cutoffs)
rn = gsub("%", "", rn)
groups = table(voldf$group)
groups = groups[ names(groups) %in% c("Validation", "Test")]
non_nmods = total_N - Nmods
save(non_nmods, total_N, file = "Number_of_Non_Nmods_Abstract.rda")

## ----dice_res------------------------------------------------------------
load("Reseg_111_Filenames_with_volumes.Rda")
hu_df = fdf[, c("id", "mean", "median", "sd")]

load("Reseg_Results.Rda")
run_group = c("Test", "Validation")


long = filter(long,
    cutoff %in% c("cc", "scc"))
long$cutoff = revalue(long$cutoff,
    c("cc"= "Unsmoothed",
    "scc" = "Smoothed")
    )
long = mutate(long,
    mean = (tvol + evol) /2,
    diff = tvol - evol
    )
all_long = long
long = filter(long,
    group %in% c("Test", "Validation"))
slong = filter(long,
    cutoff %in% c("Smoothed"))


nlong = filter(slong, app %in% "Native")
llong = select(nlong, mod,
    dice, sens, accur,
    spec, iimg, group)
llong = melt(llong,
    id.vars = c("iimg", "group", "mod"))
relev2 = c("dice" = "Dice Similarity Index",
        "accur" = "Accuracy",
        "sens" = "Sensitivity",
        "spec" = "Specificity")
llong$variable = revalue(llong$variable,
    relev2
    )
llong$variable = factor(llong$variable,
    levels = relev2)
native = filter(slong, app %in% "Native")


dice = filter(native, mod %in% "rf")
all_dice = filter(all_long,
    cutoff %in% c("Smoothed"),
    app %in% "Native",
    mod %in% "rf")    

check_ids = function(ids) {
  stopifnot(all(all_dice$id %in% ids))
}

check_ids(man_fdf$id)
check_ids(hu_df$id)
check_ids(ivh$id)
check_ids(slice_df$id)


man_dice = left_join(
  all_dice %>% 
    select(iimg, id, dice, group, truevol, estvol) %>% 
    mutate(truevol = truevol / 1000,
           estvol = estvol / 1000), 
  man_fdf, by = "id")
man_dice = left_join(
  man_dice,
  hu_df,
  by = "id")
man_dice = left_join(
  man_dice,
  ivh,
  by = "id")
man_dice = left_join(
  man_dice,
  slice_df,
  by = "id")

train_man = filter(man_dice, group %in% "Train") 
man_dice = filter(man_dice, !group %in% "Train")
man_dice$ich_cat = cut(man_dice$truevol, 
                       breaks = c(0, 30, 60, max(man_dice$truevol)), 
                       include.lowest = TRUE)
man_dice$pre_ich_cat = cut(man_dice$Pre_Rand_ICHvol, 
                       breaks = c(0, 30, 60, max(man_dice$truevol)), 
                       include.lowest = TRUE)
stopifnot(all(!is.na(man_dice$ich_cat)))
stopifnot(all(!is.na(man_dice$pre_ich_cat)))

large_ivh = man_dice %>% arrange(-Pre_Rand_IVHvol) %>% head(2)
man_dice %>% filter(truevol > 100)

n_under_50 = sum(dice$dice < 0.5)
L = length(dice$dice)
stopifnot(L == non_nmods)
fail_rate = sprintf("%3.1f",
	n_under_50/non_nmods*100)
qs = quantile(dice$dice)
ranks = rank(dice$dice)
inds = floor(quantile(1:nrow(dice)))
pick = which(ranks %in% inds)
pick = pick[ order(ranks[pick])]

qs = round(qs, 2)

dice = dice[pick, , drop = FALSE]
dice$quantile = names(qs)


med_dice = group_by(native, mod) %>% summarise(med = median(dice))
med_dice = as.data.frame(med_dice)
nn = as.character(med_dice$mod)
med_dice = med_dice$med
names(med_dice) = nn
med_dice = round(med_dice, 3)
save(med_dice, file = "Median_Dice_Abstract.rda")

## ----wt_pvals------------------------------------------------------------
#######################################
# P-values for median tests
#######################################
ktest = kruskal.test( dice ~ mod, data = native)
eg = t(
  combn(as.character(unique(native$mod)),
  2))
eg = as.data.frame(eg, stringsAsFactors = FALSE)
colnames(eg) = c("x", "y")

wt_pvals = mdply(eg, function(x, y){
  nat = filter(native, mod %in% c(x,y))
  wt = wilcox.test(dice ~ mod, data = nat, paired = TRUE)
  c(pvalue = wt$p.value)
})
wt_pvals$adj = p.adjust(wt_pvals$pvalue,
method = "bonferroni")
sig_rows = wt_pvals[wt_pvals$adj < 0.05,]
stopifnot(all("rf" %in% sig_rows$x | "rf" %in% sig_rows$y))

wt_pvals = sapply(c("logistic", "lasso", "gam"), function(x){
	row = sig_rows[ sig_rows$x %in% x | sig_rows$y %in% x,, drop = FALSE]
	row$adj
	})
wt_pvals = pvaller(wt_pvals, 0.001)
save(wt_pvals, file = "Wilcoxon_Rank_pvalues_Abstract.rda")

## ----vol_wt_pvals--------------------------------------------------------
native$abs_diff = abs(native$diff)
native$abs_pct = abs(native$diff / native$tvol)

# vol_ktest = kruskal.test( diff ~ mod, data = native)
pct_ktest = kruskal.test( abs_pct ~ mod, data = native)
vol_ktest = kruskal.test( abs_diff ~ mod, data = native)

#
# vol_wt_pvals = mdply(eg, function(x, y){
#   nat = filter(native, mod %in% c(x,y))
#   wt = wilcox.test(abs_diff ~ mod, data = nat, paired = TRUE)
#   c(pvalue = wt$p.value)
# })
# vol_wt_pvals$adj = p.adjust(vol_wt_pvals$pvalue,
# method = "bonferroni")
# vol_sig_rows = vol_wt_pvals[vol_wt_pvals$adj < 0.05,]
# stopifnot(all("rf" %in% vol_sig_rows$x | "rf" %in% vol_sig_rows$y))
#
# runs = unique(c(vol_sig_rows$x, vol_sig_rows$y))
# runs = runs[ !runs %in% "rf"]
#
# vol_wt_pvals = sapply(runs, function(x){
#     row = vol_sig_rows[ vol_sig_rows$x %in% x | vol_sig_rows$y %in% x,, drop = FALSE]
#     row$adj
#     })
# vol_wt_pvals = pvaller(vol_wt_pvals, 0.001)

## ----corrs---------------------------------------------------------------
rda = "Reseg_Correlation_Results.Rda"
load(rda)
rmse = corrs[, "rmse", drop = FALSE]
rmse = round(rmse, 1)
nn = rownames(rmse);
rmse = unlist(rmse)
names(rmse) = nn
rm(list = "nn")
corrdata = corrs %>% select(cor, cor.lower, cor.upper)
corrdata = round(corrdata, 2)
save(corrdata, file = "Correlation_data_Abstract.rda")

## ----dice_by_man---------------------------------------------------------
# testing that no training data got in here somehow
stopifnot(!any(man_dice$group %in% "Train"))
# Getting the median/mean/sd for each manufacturer
vals = man_dice %>% group_by(man) %>% 
  summarise(mean = mean(dice),
            sd = sd(dice),
            median = median(dice)) %>% 
  as.data.frame
rownames(vals) = vals$man
vals$median = round(vals$median, 2)
# testing medians across manufacturers
kt = kruskal.test(dice ~ factor(man), data = man_dice)
eg = t(combn(unique(man_dice$man), 2))
eg = data.frame(eg, stringsAsFactors = FALSE)
colnames(eg) = c("Var1", "Var2")
eg$p.value = eg$statistic = NA

# Getting the combination of tests for wilcox
if (kt$p.value < 0.05) {
  ieg = 1 
  for (ieg in seq(nrow(eg))) {
    man1 = eg$Var1[ieg]
    man2 = eg$Var2[ieg]
    wt = wilcox.test(dice ~ factor(man), 
                     data = man_dice %>% filter(man %in% c(man1, man2)))
    eg$p.value[ieg] = wt$p.value
    eg$statistic[ieg] = wt$statistic
  }
}
# adjusting p-values
eg$adj = p.adjust(eg$p.value, method = "bonferroni")
sig = eg[ eg$adj < 0.05, , drop = FALSE]

## ------------------------------------------------------------------------
failed_avg_hu = man_dice %>% filter(dice < 0.01) %>% select(mean)
failed_avg_hu = round(failed_avg_hu, 1)

## ----varslice_comp-------------------------------------------------------
# Getting the median/mean/sd for each manufacturer
ivals = man_dice %>% group_by(var_slice) %>% 
  summarise(mean = mean(dice),
            sd = sd(dice),
            median = median(dice)) %>% 
  as.data.frame
rownames(ivals) = ivals$var_slice
ivals$median = round(ivals$median, 2)

# testing medians across manufacturers
kt = kruskal.test(dice ~ var_slice, data = man_dice)
eg = t(combn(unique(as.character(man_dice$var_slice)), 2))
eg = data.frame(eg, stringsAsFactors = FALSE)
colnames(eg) = c("Var1", "Var2")
eg$p.value = eg$statistic = NA

# Getting the combination of tests for wilcox
if (kt$p.value < 0.05) {
  ieg = 1 
  for (ieg in seq(nrow(eg))) {
    man1 = eg$Var1[ieg]
    man2 = eg$Var2[ieg]
    wt = wilcox.test(dice ~ ich_cat, 
                     data = man_dice %>% filter(ich_cat %in% c(man1, man2)))
    eg$p.value[ieg] = wt$p.value
    eg$statistic[ieg] = wt$statistic
  }
}
# adjusting p-values
eg$adj = p.adjust(eg$p.value, method = "bonferroni")
sig = eg[ eg$adj < 0.06, , drop = FALSE]

## ----cat_tab-------------------------------------------------------------
tab = table(man_dice$ich_cat)
ptab = round(prop.table(tab) * 100, 1)
# Getting the median/mean/sd for each manufacturer
ivals = man_dice %>% group_by(ich_cat) %>% 
  summarise(mean = mean(dice),
            sd = sd(dice),
            median = median(dice)) %>% 
  as.data.frame
rownames(ivals) = ivals$ich_cat
ivals$median = round(ivals$median, 2)

# testing medians across manufacturers
kt = kruskal.test(dice ~ ich_cat, data = man_dice)
eg = t(combn(unique(as.character(man_dice$ich_cat)), 2))
eg = data.frame(eg, stringsAsFactors = FALSE)
colnames(eg) = c("Var1", "Var2")
eg$p.value = eg$statistic = NA

# Getting the combination of tests for wilcox
if (kt$p.value < 0.05) {
  ieg = 1 
  for (ieg in seq(nrow(eg))) {
    man1 = eg$Var1[ieg]
    man2 = eg$Var2[ieg]
    wt = wilcox.test(dice ~ ich_cat, 
                     data = man_dice %>% filter(ich_cat %in% c(man1, man2)))
    eg$p.value[ieg] = wt$p.value
    eg$statistic[ieg] = wt$statistic
  }
}
# adjusting p-values
eg$adj = p.adjust(eg$p.value, method = "bonferroni")
sig = eg[ eg$adj < 0.06, , drop = FALSE]

## ----loncaric------------------------------------------------------------
library(reshape2)
df = read.table("loncaric_data.txt")
colnames(df) = c(paste0("manual_", 1:3), paste0("auto_", 1:3))
df$id = 1:5
df = melt(df, id.vars = "id")
df = tidyr::separate(df, variable, sep = "_", into = c("type", "visit"))
df = spread(df, type, value)
lonc_cor = cor(df$manual, df$auto)
lonc_cor1 = with(df[ df$visit==1,], cor(manual, auto))
lonc_cor2 = with(df[ df$visit==2,], cor(manual, auto))
lonc_cor3 = with(df[ df$visit==3,], cor(manual, auto))

## ----results = 'asis'----------------------------------------------------
vals = c(0, 25, 75, 100)
zero = sprintf("%03.0f", vals)
qqs = qs[ paste0(vals, "%")]
names = c("Lowest", "25$^{\\text{th}}$ Quantile", "75$^{\\text{th}}$ Quantile", "Highest")
lnames = tolower(names)

figstr = paste0('\\begin{figure}
\\centering
\\includegraphics[width=\\linewidth,keepaspectratio]{figures/Long/Reseg_Figure_DSI_Quantile_', zero, '_native.png}
\\caption{{\\bf Patient with  ', names, ' Dice Similarity Index}. We present the patient with the ', lnames, ' Dice Similarity Index (DSI), a measure of spatial overlap, from the chosen predictor model fit with a random forest.  The ', lnames, ' DSI was ', qqs, '. The green indicates a correct classification of ICH from the model, blue indicates a false negative, where the manual segmentation denoted the area to be ICH but the predicted one did not, and red indicates a false positive, where the predicted segmentation denoted the area to be ICH but the manual one did not. }
\\label{fig:dice_img', vals, '}
\\end{figure}

')
cat(figstr)

## ----mod_list------------------------------------------------------------
load("smoothed_logistic_cutoffs.rda")
cutoff = smoothed_logistic_cutoffs$mod.dice.coef[, "cutoff"]

x = load("Reseg_Aggregate_models_Rigid_logistic.Rda")
logistic_summary = smod
n_sum_pred = nrow(coef(logistic_summary))-1
stopifnot(n_sum_pred == 20)
rm(list = x)

load("logistic_modlist.rda")
# mod = logistic_modlist$mod
mod = logistic_summary
npred = NROW(coef(mod)) - 1
# npred for intercept
stopifnot(npred == 20)
rename_vec = c("(Intercept)" = "Intercept",
"moment1" = "Neighborhood mean",
"moment2" = "Neighborhood sd",
"skew" = "Neighborhood skew",
"kurtosis" = "Neighborhood kurtosis",
"value" = "Image intensity (HU)",
"thresh" = paste0("Threshold ($\\geq$ ", lthresh, " and $\\leq$ ", uthresh, ")"),
"zscore1" = "Within-plane coronal",
"zscore2" = "Within-plane sagittal",
"zscore3" = "Within-plane axial",
"win_z" = "Winsorized standardized (20\\% trim)",
"pct_thresh" = "Percentage thresholded neighbors",
"prob_img" = "Atropos probability image",
"pct_zero_neighbor" = "Percent of zero neighbors",
"any_zero_neighbor" = "Indicator of any zero neighbors",
"dist_centroid" = "Distance to image centroid",
"smooth5" = "Gaussian smooth ($\\sigma = 5$mm$^3$)",
"smooth10" = "Gaussian smooth ($\\sigma = 10$mm$^3$)",
"smooth20" = "Gaussian smooth ($\\sigma = 20$mm$^3$)",
"zscore_template" = "Standardized-to-template intensity",
"flipped_value" = "Contralateral difference"
)

if (inherits(mod, "summary.glm")) {
  is_sum = TRUE
  mod = coef(mod)
}
coefs = broom::tidy(mod, quick = TRUE)
if (is_sum) {
  colnames(coefs)[1] = "term"
}
coefs$term = plyr::revalue(coefs$term,
	rename_vec
)
add = NULL
if (is_sum) {
  add = c("SE", "Z", "p.value")
}
colnames(coefs) = c("Predictor", "Beta", add)
if (is_sum) {
  coefs = coefs[, c("Predictor", "Beta", "SE", "Z")]
}
coefcap = paste0( "Beta coefficients (log odds ratio) for the logistic regression model for all coefficients.  ",
"Combining these for each voxel value and using the inverse logit transformation yields the probability that ",
"voxel is ICH. ",
"After smoothing by 1 voxel in all 3 directions, the probability cutoff for thresholding was ",
round(cutoff, 4), ".  We note the standardized-to-template intensity and the neighborhood mean appear to be the strongest predictors.")
xtab = xtable(coefs, digits = 3, caption = coefcap, label = "tab:modspec")

## ----results = "asis"----------------------------------------------------
print.xtable(xtab, include.rownames = FALSE, sanitize.text.function = identity)

## ----cutoff_rf-----------------------------------------------------------
library(ichseg)
cutoff = smoothed_rf_cutoffs$mod.dice.coef[1,"cutoff"]

## ----plotting_dice_over_manu---------------------------------------------
library(ggplot2)
g = man_dice %>% 
  ggplot(aes(x = man, y = dice)) + 
  geom_boxplot(outlier.size = NA, fill = NA) 
g = g + geom_point(position = position_jitter(width = 0.2))
g = g + 
  xlab("Manufacturer") +
  ylab("Dice Similarity Index") +
  theme(text = element_text(size = 24))

g = g + scale_y_continuous(
  breaks = c(0, 0.25, 0.5, 0.75, 1),
  limits = c(0, 1))
pngname = "DSI_Box_By_Manufacturer.png"
png(pngname, height = 5, width = 7, res = 600, units = "in")
  print(g)
dev.off()
pngname = "DSI_Box_By_Manufacturer_A.png"
png(pngname, height = 5, width = 7, res = 600, units = "in")
  print({g + geom_text(aes(x = 4, y = 0.125, label = "A"), size = 30)})
dev.off()

##################################
# Throwing out bad point
##################################
g = g + scale_y_continuous(
  breaks = c(0.5, 0.75, 1),
  limits = c(0.5, 1))
pngname = "DSI_Box_By_Manufacturer_Thresh.png"
png(pngname, height = 5, width = 7, res = 600, units = "in")
  print({g })
dev.off()

g = g + geom_text(aes(x = 4, y = 0.5625, label = "B"), size = 30)
pngname = "DSI_Box_By_Manufacturer_B.png"
png(pngname, height = 5, width = 7, res = 600, units = "in")
  print({g})
dev.off()

## ----dice_vs_hu----------------------------------------------------------
g = man_dice %>% 
  ggplot(aes(y = dice)) + 
  geom_point() + geom_smooth(se = FALSE)
g = g + 
  ylab("Dice Similarity Index") +
  theme(text = element_text(size = 24))
g = g + scale_y_continuous(
  breaks = c(0, 0.25, 0.5, 0.75, 1),
  limits = c(0, 1))
g_mean = g + aes(x = mean) + xlab("Hemorrhage Mean Hounsfield Unit")
g_med = g + aes(x = median) + xlab("Hemorrhage Median Hounsfield Unit")

pngname = "DSI_By_Mean_HU.png"
png(pngname, height = 5, width = 7, res = 600, units = "in")
  print(g_mean)
dev.off()

pngname = "DSI_By_Mean_HU_A.png"
png(pngname, height = 5, width = 7, res = 600, units = "in")
  print({g_mean + geom_text(aes(x = 67.5, y = 0.125, label = "A"), size = 30)})
dev.off()

g_mean = g_mean + scale_y_continuous(
  breaks = c(0.5, 0.75, 1),
  limits = c(0.5, 1))
g_mean = g_mean + geom_text(aes(x = 67.5, y = 0.5625, label = "B"), size = 30)
pngname = "DSI_By_Mean_HU_B.png"
png(pngname, height = 5, width = 7, res = 600, units = "in")
  print({g_mean})
dev.off()

# pngname = "DSI_By_Median_HU.png"
# png(pngname, height = 5, width = 7, res = 600, units = "in")
#   print(g_med)
# dev.off()

## ----dice_vs_ich_cat-----------------------------------------------------
stopifnot(all(!is.na(man_dice$ich_cat)))
# levels(man_dice$ich_cat)
g = man_dice %>% 
  ggplot(aes(x = ich_cat, y = dice)) + 
  geom_boxplot(outlier.size = NA, fill = NA) 
g = g + geom_point(position = position_jitter(width = 0.2))
g = g + 
  xlab("Hemorrhage Size Category") +
  ylab("Dice Similarity Index") +
  theme(text = element_text(size = 24))
g = g + scale_y_continuous(
  breaks = c(0, 0.25, 0.5, 0.75, 1),
  limits = c(NA, 1))
pngname = "DSI_Box_By_ICH_Cat_A.png"
png(pngname, height = 5, width = 7, res = 600, units = "in")
  print({g + geom_text(aes(x = 3, y = 0.125, label = "A"), size = 30)})
dev.off()

g = g + scale_y_continuous(
  breaks = c(0.5, 0.75, 1),
  limits = c(0.5, 1))
g = g + geom_text(aes(x = 3, y = 0.5625, label = "B"), size = 30)
pngname = "DSI_Box_By_ICH_Cat_B.png"
png(pngname, height = 5, width = 7, res = 600, units = "in")
  print(g)
dev.off()

## ----pre_cat_tab---------------------------------------------------------
tab = table(man_dice$pre_ich_cat)
ptab = round(prop.table(tab) * 100, 1)
# Getting the median/mean/sd for each manufacturer
ivals = man_dice %>% group_by(pre_ich_cat) %>% 
  summarise(mean = mean(dice),
            sd = sd(dice),
            median = median(dice)) %>% 
  as.data.frame
rownames(ivals) = ivals$pre_ich_cat
ivals$median = round(ivals$median, 2)

# testing medians across manufacturers
kt = kruskal.test(dice ~ pre_ich_cat, data = man_dice)
eg = t(combn(unique(as.character(man_dice$pre_ich_cat)), 2))
eg = data.frame(eg, stringsAsFactors = FALSE)
colnames(eg) = c("Var1", "Var2")
eg$p.value = eg$statistic = NA

# Getting the combination of tests for wilcox
if (kt$p.value < 0.05) {
  ieg = 1 
  for (ieg in seq(nrow(eg))) {
    man1 = eg$Var1[ieg]
    man2 = eg$Var2[ieg]
    wt = wilcox.test(dice ~ pre_ich_cat, 
                     data = man_dice %>% filter(pre_ich_cat %in% c(man1, man2)))
    eg$p.value[ieg] = wt$p.value
    eg$statistic[ieg] = wt$statistic
  }
}
# adjusting p-values
eg$adj = p.adjust(eg$p.value, method = "bonferroni")
sig = eg[ eg$adj < 0.06, , drop = FALSE]

