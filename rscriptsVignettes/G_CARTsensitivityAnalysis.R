library(tidyverse)
library(predictSuiattPsyCoLaus)

# Custom function to compute harm-to-benefit ratio (hbr)
hbr <- function(x) {
    return(x/(1-x))
}
# FPwt = false positive weight; 1/199 = 0.005025126, etc.
(FPwt <- hbr(c(.005, .0075, .01, .0125, .015, .0175, .02)))
# Outcome incidence in each of the 700 test subsets
inc <- 10/810
# Compute net benefit for the treat all scenario
(nbTreatAll <- inc - FPwt*(1-inc))

# Custom function to compute net benefit for the prediction model.
# tp = true positives, fp = false positives, FPwt = false positives weight.
nbModFun <- function(tp=NULL, fp=NULL, FPwt=NULL) {
    return((tp - FPwt*fp)/810)
}

# Custom function to compute the prediction performance results.
# Argument confMatDf = confusion matrix data.frame. That is, confMatDf contains 10 columns, 8 of which comprise the confusion matrix or are directly computed on the basis of the confusion matrix.
computeResults <- function(confMatDf=NULL) {
    # Append a distinct column that shows the seven unique levels of threshold probabilities (logreg) or harm-to-benefit ratios (CART).
    confMatDf$level <- rep(1:7, times=100)
    # Append the FPwt to confMatDf.
    confMatDf$FPwt <- rep(FPwt, times=100)
    # Append the treat all net benefit to confMatDf.
    confMatDf$nbTreatAll <- rep(nbTreatAll, times=100)
    # Append the treat none net benefit to confMatDf.
    confMatDf$nbTreatNone <- rep(0, times=nrow(confMatDf))
    # Compute and append the prediction model net benefit to confMat. Use custom function nbModFun for this.
    confMatDf$nbModel <- nbModFun(tp=confMatDf$TP, fp=confMatDf$FP, FPwt=confMatDf$FPwt)
    # deltaNb: Difference between net benefit of the prediction model and net benefit of the next best contender (e.g., treat all or treat none).
    confMatDf$deltaNb <-
        # Column 'level' in confMatDf represents the thresholds (for logreg) or FPwt (for CART).
        dplyr::if_else(confMatDf$level <= 3,
                       confMatDf$nbModel - confMatDf$nbTreatAll,
                       confMatDf$nbModel)
    return(confMatDf)
}

# Apply custom function computeResults to the logistic regression output.
logregCompleted <- computeResults(predictSuiattPsyCoLaus::logreg700)

# Apply custom function computeResults to the CART output.
CARTcompleted <- computeResults(predictSuiattPsyCoLaus::CART700)

# CART sensitivity analysis.
# -------------------------
# CARTPrunedLs <- predictSuiattPsyCoLaus::CART700Pruned
CARTPrunedLs <- lapply(predictSuiattPsyCoLaus::CART700Pruned,
                       FUN = computeResults)

# Custom function 'correspond'. Since the median net benefit is computed across all 100 cross-validations for each of the seven threshold probabilities, this computed median net benefit may not precisely correspond to any of the 100 cross-validated net benefits. Therefore, this 'correspond' function extracts the closest net benefit values. Maximum and minimum net benefits must precisely correspond.
correspond <- function(data=NULL) {
    
    # For the given 100 cross-validations (logistic regression or CART), compute median, minimum, and maximum net benefit for each of the seven levels (level = threshold probability for logistic regression or harm-to-benefit ratio for CART).
    medMaxMinDf <-
        data %>% group_by(level) %>%
        summarise(
            Median=median(nbModel),
            Max=max(nbModel),
            Min=min(nbModel)
        )
    # outVec: Vector that is used in the inner for-loop, to assign the correct name, either Median, Minimum, or Maximum.
    outVec <- c(NA, "Median", "Maximum", "Minimum")
    # Empty vectors to collect the results that will be returned at the end of the outer for-loop.
    name <- level <- medMaxMinIdx <- c()
    # Outer for-loop. lvl = column 'level' in function argument data.
    for(lvl in 1:7) {
        # Inner for-loop. It iterates across the columns 2, 3, and 4, of medMaxMinDf (see above).
        for(dfCols in 2:4) {
            
            # Column 2 of medMaxMinDf contains the median net benefit.
            if(dfCols==2) {
                
                # If there is a precise match across the 100 cross-validated net benefits, use that matching net benefit.
                if(any(data[data$level==lvl,"nbModel"][[1]] == medMaxMinDf[medMaxMinDf$level==lvl,"Median"][[1]])) {
                    # Find exact match.
                    idxMedian <- which(data[data$level==lvl,"nbModel"][[1]] == medMaxMinDf[medMaxMinDf$level==lvl,"Median"][[1]])
                    # Assign name "Median" as often as there are exact matches.
                    name <- c(name, rep(outVec[dfCols], times=length(idxMedian)))
                    # Assign level (a value between 1 and 7) as often as there are exact matches.
                    level <- c(level, rep(lvl, times=length(idxMedian)))
                    # Collect the exact matching net benefit(s).
                    medMaxMinIdx <- c(medMaxMinIdx, which(data$level==lvl)[idxMedian])
                    
                    # Else: Use the next closest net benefit across the 100 cross-validated net benefits.
                } else {
                    # Store vector of 100 net benefits; multiply by 100, to get higher precision in variable absDiffVec (below).
                    nb100 <- data[data$level==lvl,"nbModel"][[1]] * 100
                    # Store single median net benefit from medMaxMinDf (reference net benefit), also multiply by 100.
                    nbReference <- medMaxMinDf[medMaxMinDf$level==lvl,"Median"][[1]] * 100
                    # Find closest value across the 100 cross-validated net benefits.
                    absDiffVec <- abs(nb100 - nbReference) # abs = absolute values.
                    # Find all occurrences across the 100 cross-validated net benefits that are near this closest value.
                    idxMedian <- which(absDiffVec == min(absDiffVec))
                    # Assign name "Median" as often as there are closest matches.
                    name <- c(name, rep(outVec[dfCols], times=length(idxMedian)))
                    # Assign level (a value between 1 and 7) as often as there are closest matches.
                    level <- c(level, rep(lvl, times=length(idxMedian)))
                    # Collect the closest matching net benefit(s).
                    medMaxMinIdx <- c(medMaxMinIdx, which(data$level==lvl)[idxMedian])
                }
                # Remaining two columns 3 and 4: Max and Min in medMaxMinDf
            } else {
                # Find exact match.
                idxMaxMin <- which(data[data$level==lvl,"nbModel"][[1]] == medMaxMinDf[medMaxMinDf$level==lvl, dfCols][[1]])
                # Assign column name as often as there are exact matches.
                name <- c(name, rep(outVec[dfCols], times=length(idxMaxMin)))
                # Assign level (a value between 1 and 7) as often as there are exact matches.
                level <- c(level, rep(lvl, times=length(idxMaxMin)))
                # Collect the exact matching net benefit(s).
                medMaxMinIdx <- c(medMaxMinIdx, which(data$level==lvl)[idxMaxMin])
            }
        }
    }
    return(tibble(name, level, medMaxMinIdx))
}

# It may be possible to have more than one net benefit that matches the median, the maximum, or the minimum net benefit across all 100 cross-validations. Therefore, extract unique median, maximum, and minimum net benefit with this custom function.
correspondUnique <- function(correspondDf=NULL) {
    idx_i <- c()
    for(i in 1:7) {
        correspondDf_i <- correspondDf[correspondDf[,"level"]==i,]
        idx_i <- c(idx_i, !duplicated(correspondDf_i[,"name"]))
    }
    return(correspondDf[idx_i,])
}

# Logistic regression
logregCorrespondDf <- correspond(data=logregCompleted)
print(logregCorrespondDf, n=nrow(logregCorrespondDf))

logregCompletedMedMaxMin <- logregCompleted[logregCorrespondDf$medMaxMinIdx,]
logregCompletedMedMaxMin$name <- logregCorrespondDf$name
print(logregCompletedMedMaxMin[,c("name", "iter", "FP", "TP", "level", "FPwt", "nbModel", "deltaNb")], n=nrow(logregCompletedMedMaxMin))

logregCompletedMedMaxMinUnique <- correspondUnique(logregCompletedMedMaxMin)
print(logregCompletedMedMaxMinUnique[,c("name", "iter", "FP", "TP", "level", "FPwt", "nbModel", "deltaNb")], n=21)

# CART
cartCorrespondDf <- correspond(data=CARTcompleted)
print(cartCorrespondDf, n=nrow(cartCorrespondDf))

CARTcompletedMedMaxMin <- CARTcompleted[cartCorrespondDf$medMaxMinIdx,]
CARTcompletedMedMaxMin$name <- cartCorrespondDf$name
print(CARTcompletedMedMaxMin[,c("name", "iter", "FP", "TP", "level", "FPwt", "nbModel", "deltaNb")], n=nrow(CARTcompletedMedMaxMin))

CARTcompletedMedMaxMinUnique <- correspondUnique(CARTcompletedMedMaxMin)
print(CARTcompletedMedMaxMinUnique[,c("name", "iter", "FP", "TP", "level", "FPwt", "nbModel", "deltaNb")], n=21)

# CART (pruning: No pruning, cp = .01, cp = .02, cp = .03, cp = .04, cp = .05)

cartCorrespondPrunedDf <- lapply(CARTPrunedLs, FUN=correspond)

CARTcompletedMedMaxMinPrunedLs <- list()
for(i in 1:length(CARTPrunedLs)) {
    
    CARTPrunedMedMaxMin_i <- CARTPrunedLs[[i]][cartCorrespondPrunedDf[[i]]$medMaxMinIdx,]
    CARTPrunedMedMaxMin_i$name <- cartCorrespondPrunedDf[[i]]$name
    
    CARTcompletedMedMaxMinPrunedLs[[i]] <- CARTPrunedMedMaxMin_i
    
}

CARTcompletedMedMaxMinPrunedLsUnique <- lapply(CARTcompletedMedMaxMinPrunedLs,
                                               FUN=correspondUnique)

print(CARTcompletedMedMaxMinPrunedLsUnique[[3]][,c("name", "iter", "FP", "TP", "level", "FPwt", "nbModel", "deltaNb")], n=21)

# -----------------------------------------------
# Summarise: Median, maximum, and minimum NB

# logregSmry to be used in CART sensitivity analysis plot of all decision curves.
(logregSmry <- logregCompleted %>%
    group_by(level) %>%
    summarise(
        Median=median(nbModel),
        Max=max(nbModel),
        Min=min(nbModel)
    ))

# CARTsmry to be used in CART sensitivity analysis plot of all decision curves.
(CARTsmry <- CARTcompleted %>%
    group_by(level) %>%
    summarise(
        Median=median(nbModel),
        Max=max(nbModel),
        Min=min(nbModel)
    ))

# cartCorrespondPrunedSmryDf to be used in CART sensitivity analysis plot of all decision curves.
(cartCorrespondPrunedSmryDf <- lapply(CARTPrunedLs, function(x) {
    
    x %>% group_by(level) %>%
        summarise(
            Median=median(nbModel),
            Max=max(nbModel),
            Min=min(nbModel)
        )
    
}))

dcaLs <- list(lgrg=logregSmry,
              cart=CARTsmry,
              cart02=cartCorrespondPrunedSmryDf[["cp_0.02"]],
              cart03=cartCorrespondPrunedSmryDf[["cp_0.03"]],
              cart04=cartCorrespondPrunedSmryDf[["cp_0.04"]],
              cart05=cartCorrespondPrunedSmryDf[["cp_0.05"]])
names(dcaLs) <- c("Logistic regression", "CART", "CARTcp02", "CARTcp03", "CARTcp04", "CARTcp05")

# hbr: Custom function to compute the harm-benefit ratio; x = a single threshold probability.
hbr <- function(x) {
    if(any(x < 0) | any(x > 1)) {
        stop("Threshold probabilities must lie between 0 and 1.")
    }
    return(x/(1-x))
}

# Custom function to generate the data that is needed to plot the decision curves.
makeDCADf <- function(dataLs=NULL, colName="Median") {
    # FPwt = false positive weight; 1/199 = 0.005025126, etc.
    FPwt <- hbr(c(.005, .0075, .01, .0125, .015, .0175, .02))
    # Outcome incidence in each of the 700 test subsets
    inc <- 10/810
    # Seven threshold probabilities, in addition to the threshold 0.
    thrsh <- c(0, .005, .0075, .01, .0125, .015, .0175, .02)
    # Net benefit of treat all
    nbTreatAll <- c(inc, inc - FPwt*(1-inc))
    # Net benefit of treat none
    nbTreatNone <- rep(0, times=length(nbTreatAll))
    # Labels of the clinical decision scenarios.
    label <- rep(c("Treat all", "Treat none"), each=8)
    # Empty vector to collect the net benefit of the prediction models.
    nbModel <- c()
    for(i in 1:length(dataLs)) {
        # Collect net benefit from the summary table.
        nbModel <- c(nbModel, inc, dataLs[[i]][,colName][[1]])
        # Add the current model's name to the label vector.
        label <- c(label, rep(names(dataLs)[i], times=8))
    }
    # Make vector with the overall eight threshold probabilities.
    threshold <- rep(thrsh, times=sum(2, length(dataLs)))
    
    return(
        tibble(
            label=forcats::as_factor(label),
            threshold=threshold,
            net_benefit=c(nbTreatAll, nbTreatNone, nbModel)
        )
    )
}

# Use either Min (minimum), Max (maximum), or Median for argument colName.
# The selected colName will then be plotted in the decision curves plot.
dcaDf <- makeDCADf(dataLs = dcaLs, colName = "Median")

# Select colors, for instance, see these URLs:
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# https://r-graph-gallery.com/42-colors-names.html
useColor <- c("Treat all" = "black", "Treat none" = "grey",
              "Logistic regression" = "red", "CART" = "blue",
              "CARTcp02" = "cyan3", "CARTcp03" = "deepskyblue2",
              "CARTcp04" = "darkorchid", "CARTcp05" = "aquamarine3")

# Custom function to generate the harm-to-benefit second x-axis
htbPlot <- function(x) paste0("1:", round((1-x)/x, digits=1))

# Plot the decision curves
ggplot(data=dcaDf, aes(x=threshold, y=net_benefit, colour=label)) +
    geom_line(aes(colour=label, linetype=label, linewidth=label)) +
    
    labs(color=NULL) +
    coord_cartesian(ylim=c(-.0025, .015)) +
    
    # # Without 0% and 1:Inf
    # scale_x_continuous(
    #     breaks = seq(0.005, 0.02, by=.0025),
    #     labels = c("0.5%", "0.75%", "1%", "1.25%", "1.5%", "1.75%", "2%"),
    #     sec.axis = dup_axis(name="Harm-to-benefit ratio", labels=htbPlot)) +
    
    # With 0% and 1:Inf
    scale_x_continuous(
        breaks = seq(0, 0.02, by=.0025)[-2],
        labels = c("0%", "0.5%", "0.75%", "1%", "1.25%", "1.5%", "1.75%", "2%"),
        sec.axis = dup_axis(name="Harm-to-benefit ratio", labels=htbPlot)) +
    
    scale_colour_manual(values = useColor) +
    
    scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "dashed",
                                     "dashed", "dashed", "solid")) +
    
    scale_linewidth_manual(values = c(1, 1, 1, 1, 0.5, 0.5, 0.5, 1)) +

    guides(linetype="none") +
    
    guides(linewidth="none") +
    
    ylab(label="Net benefit") +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=16),
        axis.title.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title.y = element_text(size=16),
        panel.border = element_rect(color="black", fill=NA),
        legend.text = element_text(size=14),
        legend.position = "top",
        legend.title = element_blank()) +
    labs(x="Threshold probability")
