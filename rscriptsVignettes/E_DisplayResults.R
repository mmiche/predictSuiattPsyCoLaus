# Load packages (install if not yet installed)
library(predictSuiattPsyCoLaus)
library(tidyverse)
# library(cowplot)

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

# The seven threshold probabilities correspond to these harm-to-benefit ratios.
# probThresh = probability thresholds; htb = harm-to-benefit ratio (rounded to three decimals); FPwt = weight by which false positives get weighted.
data.frame(
    probThresh=c(.005, .0075, .01, .0125, .015, .0175, .02),
    htb=c("1:199", "1:132.333", "1:99", "1:79", "1:65.667", "1:56.143", "1:49"),
    FPwt=FPwt)

# Custom function to compute net benefit for the prediction model.
# tp = true positives, fp = false positives, FPwt = false positives weight.
nbModFun <- function(tp=NULL, fp=NULL, FPwt=NULL) {
    return((tp - FPwt*fp)/810)
}

# Custom function to compute the clinical utility results for a classification prediction model.
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


# Display the final results
# -------------------------

# First, net benefit (minimum, median, maximum).
# -----

# Logistic regression (results shown in figure 1 of the main document)
(logregNB <- logregCompleted %>%
    group_by(level) %>%
    summarise(Min=min(nbModel),
              Median=median(nbModel),
              Max=max(nbModel)))

# CART (results shown in figure 1 of the main document)
(cartNB <- CARTcompleted %>%
    group_by(level) %>%
    summarise(Min=min(nbModel),
              Median=median(nbModel),
              Max=max(nbModel)))

dcaDf <- tibble(
    label = factor(
        rep(c("Treat all", "Treat none", "Logistic regression", "CART"), each=8),
        levels = c("Logistic regression", "CART", "Treat all", "Treat none")),
    threshold = rep(c(0, .005, .0075, .01, .0125, .015, .0175, .02), times=4),
    # Net benefit of:
    # Treat all, treat none, logistic regression model, CART model.
    # inc = outcome incidence (10/810)
    net_benefit = c(inc, nbTreatAll,
                    rep(0, times=8),
                    inc, logregNB$Median,
                    inc, cartNB$Median)
)

print(dcaDf, n=nrow(dcaDf))

# Custom function to convert threshold probability to harm-to-benefit ratio.
htbPlot <- function(x) paste0("1:", round((1-x)/x, digits=1))

# Generate figure 1 of the main document:
useColor <- c("Treat all" = "black", "Treat none" = "black",
              "Logistic regression" = "black", "CART" = "black")

ggplot(data=dcaDf, aes(x=threshold, y=net_benefit, colour=label)) +
    geom_line(aes(linetype=label), linewidth=1) +
    labs(color=NULL) +
    coord_cartesian(ylim=c(-.0025, .015)) +
    # With 0% and 1:Inf
    scale_x_continuous(
        breaks = seq(0, 0.02, by=.0025)[-2],
        labels = c("0%", "0.5%", "0.75%", "1%", "1.25%", "1.5%", "1.75%", "2%"),
        sec.axis = dup_axis(name="Harm-to-benefit ratio", labels=htbPlot)) +
    
    scale_colour_manual(values = useColor) +
    
    scale_linetype_manual(values = c("solid", "dotdash", "dashed", "dotted")) +
    
    guides(colour="none") +
    
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


# Second, delta net benefit:
# ------

# Delta net benefit (minimum, median, maximum):

# Logistic regression
# First, remove the 18 models with a negative delta net benefit.
(logregDeltaNB <-
logregCompleted[logregCompleted$deltaNb>=0,] %>%
    group_by(level) %>%
    summarise(Min=min(deltaNb),
              Median=median(deltaNb),
              Max=max(deltaNb)))

# CART
# First, remove the 119 models with a negative delta net benefit.
(cartDeltaNB <-
CARTcompleted[CARTcompleted$deltaNb>=0,] %>%
    group_by(level) %>%
    summarise(Min=min(deltaNb),
              Median=median(deltaNb),
              Max=max(deltaNb)))

# Display the potentially harmful models (i.e., models with a negative delta net benefit)

# There were 7 selected threshold probabilities (= 7 levels).
# Levels 1-7 correspond to the threshold probabilities 0.5%, 0.75%, 1%, 1.25%, 1.5%, 1.75%, 2%.
# Levels 1-7 correspond to the harm-to-benefit ratios 1:199, 1:132.33, 1:99, 1:79, 1:65.67, 1:56.14, 1:49.

# Logistic regression (results shown in figure 1 of the main document)
addmargins(table(logregCompleted[logregCompleted$deltaNb<0,"level"]))

# CART (results shown in figure 1 of the main document)
addmargins(table(CARTcompleted[CARTcompleted$deltaNb<0,"level"]))
# -----------------------------------------------------