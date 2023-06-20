# Other conventional performance metrics, that are relevant for the model analyst, not for the clinical practitioner.

# Code in this script is applied only to the logistic regression model.
#
# Reason why only logistic regression (not CART): For each single run of the logistic regression model, CART was run seven times (each time using a different harm-to-benefit ratio as case weights). CART was used only as an alternative model to compete against logistic regression in terms of net benefit, not in terms of other performance indicators, such as discrimination or calibration.
#
# Display the discrimination performance measures.
# -----------------------------------------------
# auroc: Area under the receiver operating characteristic curve.
# prauc: Precision-recall curve.

# Put the results in a data.frame with 2 columns:
# Column 1: auroc, column 2: prauc.
aucDf <- data.frame(matrix(data=AUCs, ncol=2, byrow = TRUE))
colnames(aucDf) <- c("auroc", "prauc")

# Two custom functions to compute the 25th and the 75th quantile.
quant1 <- function(x) {as.numeric(quantile(x)["25%"])}
quant3 <- function(x) {as.numeric(quantile(x)["75%"])}
# For the auroc and the prauc, compute Tukey's five number summary statistics (minimum, lower-hinge, median, upper hinge, maximum) and the mean.
aucDf %>% summarise(
    across(everything(),
           list(min=min,
                qut1=quant1,
                mean=mean,
                median=median,
                qut3=quant3,
                max=max))
)
#
# -----------------------------------------------------------------
# These are the results for the original data that was used in this study.
#
# auroc_min auroc_qut1 auroc_mean auroc_median auroc_qut3 auroc_max
#  0.643625  0.7709375  0.8053044    0.7995625  0.8456563  0.916375

#  prauc_min prauc_qut1 prauc_mean prauc_median prauc_qut3 prauc_max
# 0.02793485 0.07438186  0.1321855    0.1063175  0.1630145 0.4361084
# -----------------------------------------------------------------
#
# Display the calibration performance measures.
# ---------------------------------------------
#
# Use the 100 cross-validated predictions, i.e., the test data which contains the observed outcome and the predicted outcome probability (the original 100 test data cannot be published because they contain the observed outcome of the study participants).
#
# Exemplary overview of the first six rows of the first of the 100 cross-validated predictions (bogus output).
# Column truth = observed outcome (0 = no suicide attempt, 1 = suicide attempt).
# Column prob = predicted probability that the outcome occurred.
#   truth        prob
# 1     0 0.002193025
# 2     0 0.001325673
# 3     0 0.002738367
# 4     0 0.009410226
# 5     0 0.007172676
# 6     0 0.012636668
#
# Custom function to compute several calibration summary results.
compCalib <- function(data=NULL, loe=TRUE) {
    
    # Code (part 1). See documentation of the R package rms, there see the documentation for the function val.prob (short for 'validate predicted probabilities').
    p <- data[,"prob"]
    y <- data[,"truth"]
    # Apply the function val.prob. Save the results in variable v.
    v <- rms::val.prob(p, y, pl=FALSE)
    
    # Extract three results from the results saved in variable v.
    # bis = first letter from: brier, intercept, slope.
    bis <- c(as.numeric(v["Brier"]), as.numeric(v["Intercept"]), as.numeric(v["Slope"]))
    
    # Code (part 2)
    # 
    if(loe) {
        # This part of the code has been copy-pasted from the appendix in:
        # ---------------------------------------------------------------
        # Austin PC, Steyerberg EW. The Integrated Calibration Index (ICI) and related metrics for quantifying the calibration of logistic regression models. Statistics in Medicine. 2019;38:4051–4065. https://doi.org/10.1002/sim.8281
        loess.calibrate <- loess(y ~ p)
        p.calibrate <- predict(loess.calibrate, newdata=p)
        ici <- mean(abs(p.calibrate - p))
        e50 <- median(abs(p.calibrate - p))
        e90 <- as.numeric(quantile(abs(p.calibrate - p), probs=.9))
        emax <- max(abs(p.calibrate - p))
        dfLoess <- data.frame(truth=y, prob=p.calibrate)
    } else {
        # Results without loess calibration. That is, direct use of predicted probabilities, as estimated by the data-analysis model; beware: data-analysis models that are not rooted in probability theory, such as logistic regression, do NOT directly return predicted probabilities. This is important to remember, because some software output and some textbooks use the term 'predicted probabilities' whenever a data-analysis model returns values that range between 0 and 1, given the prediction of a binary outcome (so-called classification task.)
        ici <- mean(p)
        e50 <- median(p)
        e90 <- as.numeric(quantile(p, probs=.9))
        emax <- max(p)
        dfLoess <- NA
    }
    
    return(list(
        brier=bis[1],
        intercept=bis[2],
        slope=bis[3],
        ici=ici, e50=e50,
        e90=e90, emax=emax)
    )
    
}
#
# otherPerf: other performance measures (a list object).
otherPerf <- lapply(testSubsampleResults, compCalib)
# Put into a single data.frame (df).
otherPerfDf <- dplyr::bind_rows(otherPerf)
#
# Show results rounded to 4 digits after the decimal point.
# Rows of the matrix (from top to bottom):
# Brier score, calibration intercept, calibration slope, integrated calibration index (ICI), E50, E90, and Emax.
# Columns of the matrix (from left to right):
# Minimum, 25th quantile, mean, median, 75th quantile, maximum.
matrix(
    data = round(
        otherPerfDf %>% summarise(
            across(everything(),
                   list(min=min,
                        qut1=quant1,
                        mean=mean,
                        median=median,
                        qut3=quant3,
                        max=max))
        ), digits=4),
    ncol = 6, byrow = TRUE)

# Results of the original study data.
# ----------------------------------
# Rows 1-7 of the matrix:
# ----------------------
# Brier score, calibration intercept, calibration slope, integrated calibration index (ICI), E50, E90, and Emax (e.g., see Austin and Steyerberg, 2019).
# Austin PC, Steyerberg EW. The Integrated Calibration Index (ICI) and related metrics for quantifying the calibration of logistic regression models. Statistics in Medicine. 2019;38:4051–4065. https://doi.org/10.1002/sim.8281
#
# Columns 1-6 of the matrix:
# -------------------------
# Minimum, 25% quantile, mean, median, 75% quantile, maximum.
#
#        [,1]    [,2]    [,3]    [,4]    [,5]   [,6]  
# [1,] 0.0106  0.0113  0.0116  0.0116  0.0118 0.0126
# [2,] -2.6576 -0.6973 -0.1554 -0.3017 0.3485 2.5935
# [3,] 0.3223  0.7834  0.9487  0.8976  1.0617 1.7481
# [4,] 0.003   0.0061  0.0083  0.008   0.01   0.0184
# [5,] 0.0012  0.0023  0.0032  0.0031  0.0041 0.0071
# [6,] 0.0041  0.0089  0.0126  0.0115  0.0156 0.0243
# [7,] 0.015   0.1166  0.1948  0.1611  0.2588 0.7011
# --------------------------------------------------
#
# Visualize calibration:
# ---------------------
#
# Use the 100 cross-validated test datasets, which contain the observed outcome in one column and the predicted probability (based on the logistic regression model) in the second column.
# Note. The original list with the 100 cross-validated datasets may not be published due to protection of data privacy (in this case, the observed outcome of a subset of the study participants).
#
# Nonetheless, the simulated data can be used to run the following custom function, which visualizes calibration performance across all 100 cross-validations.
#
# Custom function to compute the relevant data for the calibration plot.
probBreak <- function(data=NULL, iter=NULL) {
    
    # Should there be any negative probability, stop.
    negProb <- length(which(data[,"prob"]<0))
    if(negProb > 0) {
        stop("Negative probabilities: ", negProb, ".\n")
    }
    # Extract the maximum predicted probability from column prob in the current test subset of the data.
    maxTmp <- max(data[,"prob"])
    # If predicted probabilities exceed .2, use three intervals.
    if(maxTmp > .2) {
        breaksTmp <- c(0, .1, .2, .3)
        # Else (not exceed .2) use two intervals (0-.1, >.1-.2).
    } else if(maxTmp <= .2) {
        breaksTmp <- c(0, .1, .2)
    }
    
    # ct = cut (= cut the predicted probabilities in 'breaks' many pieces)
    ct <- cut(data[,"prob"], breaks = breaksTmp, include.lowest = TRUE)
    # Extract values from the selected levels.
    ctNumLs <- stringr::str_extract_all(levels(ct), "-?[0-9.]+")
    # Check values, if there is something weird, print it.
    # Also: Make sure there are always exactly two values (lower, upper).
    if(length(ctNumLs[[1]])>2 | ctNumLs[[1]][1]<0) {
        if(is.null(iter)) {
            warning("Something is weird, check.")
        } else {
            warning("Something in iteration ", iter, " is weird, check.")
        }
        upper <- ctNumLs[[1]][length(ctNumLs[[1]])]
        lower <- 0
        ctNumLs[[1]] <- c(lower, upper)
    }
    # Compute mean value of each lower-upper pair of values.
    ctMean <- sapply(ctNumLs, function(x) mean(as.numeric(x)))
    
    # Number of participants in each of the probability bins.
    rtTbl <- as.numeric(table(ct))
    # rt = rate (observed outcome rate for each probability bin)
    rt <- c()
    for(i in 1:nlevels(ct)) {
        # compute rate and add to the collecting variable rt.
        rt <- c(rt, sum(data[ct==levels(ct)[i],"truth"])/rtTbl[i])
    }
    # Return the relevant data for plotting the calibration.
    return(data.frame(enum=1:nlevels(ct), ct=levels(ct), ctMean, rt))
}

# Apply custom function 'probBreak' to all 100 cross-validated predictions, in order to plot the function output.
for(i in 1:100) {
    if(i == 1) {
        # Use the list of cross-validated predictions, produced by the logistic regression model (see script C_LogisticRegression.R)
        dataTestset <- dataTestsetLs[[i]]
        # Apply custom function 'probBreak'.
        calib_1 <- probBreak(dataTestset, iter = i)
        # Plot the output of custom function 'probBreak'.
        plotStart <- 
            ggplot(data=calib_1, aes(x=ctMean, y=rt)) +
            geom_point(size=3) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
            # Set limits of x and of y axis, e.g., c(0,1) or c(0,.4).
            xlim(c(0,1)) + ylim(c(0,1)) +
            xlab(label="Predicted probability") +
            ylab(label="Observed proportion") +
            theme(
                panel.background = element_blank(),
                axis.text.x=element_text(size=16),
                axis.title.x=element_text(size=16),
                axis.text.y=element_text(size=16),
                axis.title.y = element_text(size=16),
                panel.border = element_rect(color="grey", fill=NA))
        plotContinue <- plotStart
        # Else: Add the other 99 cross-validated calibration estimates.
    } else {
        # Use the list of cross-validated predictions, produced by the logistic regression model (see script C_LogisticRegression.R)
        dataTestset <- dataTestsetLs[[i]]
        # Apply custom function 'probBreak'.
        calib_i <- probBreak(dataTestset, iter = i)
        # Plot the output of custom function 'probBreak'; however simply add the output to the existing plot.
        plotContinue <-
            plotContinue +
            geom_point(data=calib_i, aes(x=ctMean, y=rt), colour = "grey", size=3)
    }
}

# How many and which values exceeded the x and y limits of 0.4?
calibLs <- list()
for(i in 1:100) {
    dataTestset <- dataTestsetLs[[i]]
    # Execute custom function 'probBreak'
    calibLs[[i]] <- probBreak(dataTestset)
}
# Transform the list into a data.frame.
calibDf <- dplyr::bind_rows(calibLs)

# Exemplary output from using the original study data:
#     enum        ct ctMean          rt
# 1      1   [0,0.1]   0.05 0.006426735
# 2      2 (0.1,0.2]   0.15 0.156250000
# 3      1   [0,0.1]   0.05 0.011464968
# 4      2 (0.1,0.2]   0.15 0.000000000
# 5      3 (0.2,0.3]   0.25 0.111111111
# 6      1   [0,0.1]   0.05 0.010230179
#
# ct is the interval of predicted probabilities.
# ctMean is the expected rate of the outcome within this interval.
# rt is the observed rate of the outcome within this interval.

# 9 outliers (when using the original study data) > .4
idxOutlier <- calibDf$rt > .4
calibDf[idxOutlier,]
# 9 outliers: The observed rate of the outcome (in this case either 0.5 or 1.0) far exceeded the expected rate (in this case: 0.25 (ctMean)).
#     enum        ct ctMean  rt
# 19     3 (0.2,0.3]   0.25 0.5
# 24     3 (0.2,0.3]   0.25 1.0
# 45     3 (0.2,0.3]   0.25 0.5
# 53     3 (0.2,0.3]   0.25 1.0
# 92     3 (0.2,0.3]   0.25 1.0
# 137    3 (0.2,0.3]   0.25 0.5
# 142    3 (0.2,0.3]   0.25 0.5
# 166    3 (0.2,0.3]   0.25 1.0
# 219    3 (0.2,0.3]   0.25 0.5
# --------------------------------------------------