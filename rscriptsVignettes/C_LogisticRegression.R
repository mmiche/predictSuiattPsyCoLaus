# Load packages (install if not yet installed)
library(tidyverse)
library(precrec)
#
# ------------------------------------------
# BEWARE: R scripts that need to be executed, prior to running this script:
# B_PsyCoLausSimulateData.R
# ------------------------------------------
# 
# Holdout proportion of full sample, use for training: 80%
holdoutTrain <- .8
# Remaining proportion of full sample, use for testing: 20%
(holdout <- c(holdoutTrain, 1-holdoutTrain))

# Custom function to split the full data into a training and a test subset.
twoSplit <- function(nonEventID=NULL, eventID=NULL,
                     tr=holdout[1], te=holdout[2], seed=NULL) {
    
    if(sum(tr, te)!=1) {
        stop("tr (training) and te (test) must sum up to 1.")
    }
    
    # Ensure that each training and each test subset across all resamplings resemble the full dataset. That is, the outcome in each training and test subset pair should have roughly 1.2% many outcome cases.
    
    # Size of training subset (80%) from participants without the outcome.
    trainN.non <- round(length(nonEventID)*tr, digits=0)
    # Size of test subset (20%) from participants without the outcome.
    testN.non <- round(length(nonEventID)*te, digits=0)
    
    # Size of training subset (80%) from participants with the outcome.
    trainN <- round(length(eventID)*tr, digits=0)
    # Size of test subset (20%) from participants with the outcome.
    testN <- round(length(eventID)*te, digits=0)
    
    # Make vector with training and test cases among participants without the ouctome (N = 4002). This vector has a neat order of 1s and 2s.
    nonEventSelect <- rep(1:2, times=c(trainN.non,
                                       testN.non))
    # Make vector with training and test cases among participants with the ouctome (N = 48). This vector has a neat order of 1s and 2s.
    eventSelect <- rep(1:2, times=c(trainN,
                                    testN))
    # Use the current seed to ensure perfect reproducibility.
    set.seed(seed)
    # Shake the neat vector nonEventSelect up. The 1s and 2s will then be scattered all over the place.
    nonEventSample <- sample(nonEventSelect)
    
    # Do the same thing to the vector eventSelect, what you just did to the vector nonEventSelect.
    set.seed(seed)
    eventSample <- sample(eventSelect)
    
    # Use the shaken up variables to randomly select the training cases.
    train <- c(nonEventID[nonEventSample==1],
               eventID[eventSample==1])
    
    # Use the shaken up variables to randomly select the test cases.
    test <- c(nonEventID[nonEventSample==2],
              eventID[eventSample==2])
    
    # Return as list the training and the test cases.
    return(list(train=train, test=test))
}

# Extract which participants have not reported the outcome.
idNoCase <- which(data1$fsa==0)
# Extract which participants reported the outcome.
idCase <- which(data1$fsa==1)

# Ensure reproducibility
set.seed(1)
# Sample 100 different seeds. Each seed will be used for one resampling iteration.
seeds20230324 <- sample(x=2:1984657, size=100)

# AUCs: AUROC and PRAUC (use precrec package)
AUCs <- c()
# confMatLs: confusion matrices collected in a list,
confMatLs <- list()
# testSubsampleResults: A list which will contain 100 data.frames, each data.frame with 2 columns:
# Column 1: Observed outcome, column 2: Predicted outcome probability.
testSubsampleResults <- list()
# Start for-loop (with 100 iterations).
for(i in 1:length(seeds20230324)) {
    
    print(i)
    
    # Make the resampling into training and test subsample
    testLs <- twoSplit(nonEventID = idNoCase,
                       eventID = idCase,
                       seed = seeds20230324[i])
    
    # train dataset 
    train <- data1[testLs$train,]
    # logistic regression applied to training dataset
    log_mod <- glm(fsa ~ ., family=binomial(link='logit'),data=train)
    # test dataset
    test <- data1[testLs$test,]
    # Apply coefficients from logistic regression model to test dataset.
    log_pred <- as.data.frame(predict(log_mod, test[,-length(test)], type='response'))
    # Result: Test dataset with observed outcome (column truth) and predicted probability (column prob) that the outcome was observed.
    dataTestset <- data.frame(truth=test$fsa, prob=log_pred[,1])
    # Append dataTestset to testSubsampleResults (for later purposes).
    testSubsampleResults[[i]] <- dataTestset
    # Make the column truth a numeric variable, not a factor.
    testSubsampleResults[[i]]$truth <-
        as.numeric(as.character(testSubsampleResults[[i]]$truth))
    
    # ----------------------------------
    # This section computes TP, FP, ..., sens, spec, ... prauc
    # ----------------------------------
    selectedThresholds <- c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)/100
    trueClass <- factor(dataTestset$truth, levels = c("0", "1"))
    # Empty list, used to collect the confusion matrices in the for-loop.
    confMatLs_i <- list()
    # Inner for-loop which generates the confusion matrix for the current iteration AND for each of the seven thresholds.
    for(j in 1:length(selectedThresholds)) {
        predClass <-
            factor(dplyr::if_else(dataTestset$prob<selectedThresholds[j],0,1),
                   levels = c("0", "1"))
        tbl_j <- table(predClass, trueClass)
        
        sens <- tbl_j[2,2]/sum(tbl_j[,2]) # sensitivity
        spec <- tbl_j[1,1]/sum(tbl_j[,1]) # specificity
        ppv <- tbl_j[2,2]/sum(tbl_j[2,]) # positive predictive value
        npv <- tbl_j[1,1]/sum(tbl_j[1,]) # negative predictive value
        
        # Transform matrix to vector.
        tblVec_j <- as.vector(tbl_j)
        # Collect values in a tibble (= a data.frame)
        confMatLs_i[[j]] <- tibble::tibble(
            # TN True Negative, FP False Positive
            TN=tblVec_j[1], FP=tblVec_j[2],
            # FN False Negative, TP True Positive
            FN=tblVec_j[3], TP=tblVec_j[4],
            sens=sens, spec=spec, ppv=ppv, npv=npv
        )
    }
    # Bind the seven tibbles together
    confMatDf_i <- dplyr::bind_rows(confMatLs_i)
    # Add column thrsh (= threshold) in percent.
    confMatDf_i$thrsh <- selectedThresholds*100
    # Add column iter (iteration)
    confMatDf_i$iter <- i
    # Collect output for iteration i in list confMatLs.
    confMatLs[[i]] <- confMatDf_i
    
    # Compute auroc and prauc (use R package precrec):
    aucs_i <- precrec::evalmod(scores = dataTestset$prob, labels = dataTestset$truth)
    # Add result to collecting vector AUCs.
    AUCs <- c(AUCs, precrec::auc(aucs_i)$aucs)
}

# Put all results from the confusion matrices into one data frame. The confusion matrices contained the true positives, false positives, ...
confMatDf <- dplyr::bind_rows(confMatLs)
confMatDf[1:10,]
# --------------------------------------------------