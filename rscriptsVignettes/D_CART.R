library(rpart)
library(tidyverse)

# ------------------------------------------
# BEWARE: R scripts that need to be executed, prior to running this script:
# B_PsyCoLausSimulateData.R
# ------------------------------------------

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
    # Randomly sample from the vector nonEventSelect. The 1s and 2s will then be scattered all over the place.
    nonEventSample <- sample(nonEventSelect)
    
    set.seed(seed)
    # Randomly sample from the vector eventSelect, same as before with vector nonEventSelect.
    eventSample <- sample(eventSelect)
    
    # From the random samples, pick the training cases.
    train <- c(nonEventID[nonEventSample==1],
               eventID[eventSample==1])
    
    # From the random samples, pick the test cases.
    test <- c(nonEventID[nonEventSample==2],
              eventID[eventSample==2])
    
    # Return as list the training and the test cases.
    return(list(train=train, test=test))
}

idNoCase <- which(data1$fsa==0)
idCase <- which(data1$fsa==1)
# 

# Ensure reproducibility
set.seed(1)
# Sample 100 different seeds. Each seed will be used for one resampling iteration.
seeds20230324 <- sample(x=2:1984657, size=100)
# Set up the same 100 distinct pairs of training and test subsets of the total dataset. Setting up means that the row numbers of these distinct pairs of subsets are saved in a list (name of this list: testLs).
testLs <- list()
for(i in 1:length(seeds20230324)) {
    
    # Apply the custom function twoSplit to obtain one pair of training and test data. This is itself a list with two named vectors of row indices.
    testLs_i <- twoSplit(nonEventID = idNoCase,
                         eventID = idCase,
                         seed = seeds20230324[i])
    # Add this list to the overall list, which finally will contain 100 lists.
    testLs[[i]] <- testLs_i
}
#
# Custom function to compute for any given risk thresholds the corresponding harm-to-benefit ratio.
# Correspondance between risk threshold and harm-to-benefit ratio.
# Example: 5% threshold = odds(5) = 95/5 = 19:1.
rtHbr <- function(rt=NULL) {
    # Risk thresholds (rt) can never be below zero or above 100.
    if(any(rt < 0) | any(rt > 100)) {
        stop("Risk thresholds can never be below zero or above 100.")
    }
    benefit <- rt
    harm <- 100 - rt
    # hb: result after dividing harm by benefit
    # If hb = 19, that means the ratio is 1:19 (same with any other hb).
    hb <- harm/benefit
    return(tibble::tibble(harm, benefit, hb, rt))
}
# ------------------
# rts7: Seven predefined risk thresholds (in percent).
rts7 <- seq(.5,2, by=.25)
(rtDf7 <- rtHbr(rts7))
# Want to see numbers in more detail, e.g., column hb?
data.frame(rtDf7)
# 
# Custom function to run the classification and regression tree model, using the seven pre-specified weights. The weights are the part of the harm-to-benefit ratio which in this study is always higher than 1.
# Argument 'prune_cp' = 0 means to not apply pruning. In this study, we applied sensitivity analyses for CART by a posteriori pruning the decision tree, using the values 0.1, 0.2, 0.3, 0.4, and 0.5 as the pruning parameter 'cp' of the function prune (from the package rpart); see supplementary material 1, CART sensitivity analysis, as well as this supplementary material 2 (R package), vignette and R script G_CARTsensitivityAnalysis.
rpartFun <- function(FPwt=1, data=NULL, trainIds=NULL, prune_cp=0) {
    
    # Extract the row indices for the training subset.
    testIds <- (1:nrow(data))[-trainIds]
    # The other part of the harm-to-benefit ratio is constantly 1.
    wt1 <- rep(1, times=length(FPwt))
    
    # Extract name of the outcome column.
    outcome <- names(data)[ncol(data)]
    # Extract names of the predictor columns.
    preds <- names(data)[-ncol(data)]
    
    # Make the model formula.
    fmla <- formula(paste0(outcome, " ~ ", paste0(preds, collapse = "+")))
    
    # weightsVec: For the currently used weight, weightsVec (vev = vector) will collect the number of true negatives, false negatives, false positives, and true positives.
    weightsVec <- c()
    # For each of the weights, run the following code inside the loop.
    for(i in 1:length(FPwt)) {
        
        # Extract weights from the above vectors.
        positiveWeight <- FPwt[i]
        negativeWeight <- wt1[i]
        
        # Set up model weights for the training subset, by using the current weights.
        modelWeights <- ifelse(data[trainIds,outcome] == 1, positiveWeight, negativeWeight)
        
        # Run the classification and regression tree model with the training data, using the case weights.
        decTreeMod <- rpart::rpart(fmla, data = data[trainIds,],
                                   weights = modelWeights)
        # If argument 'prune_cp' is > 0, then prune the decision tree.
        if(prune_cp > 0) {
            decTreeMod <- rpart::prune(decTreeMod, cp = prune_cp)
        }
        
        # Cross-validate the training model in the test subset, extract the predicted class (event no versus yes) for each test subject.
        initNA <- rep(NA, times=810)
        decTreePred <- data.frame(response=initNA, truth=initNA)
        decTreePredClass <- as.numeric(as.character(predict(decTreeMod, data[testIds,], type="class")))
        decTreePred$response <- decTreePredClass
        # The [[1]] is needed to get the values from the tibble column, not the tibble column (that is, a numeric vector and the column of a tibble are different data objects).
        decTreePred$truth <- data[testIds,outcome][[1]]
        
        # If all predicted outcomes are equal (either 0 or 1), then stop and return a message.
        if(all(decTreePredClass[1]==decTreePredClass[2:length(decTreePredClass)])) {
            stop("Predicted outcome is constant, either only 0 or only 1.")
        }
        
        # Make confusion matrix, transform to a vector. Order of the cell counts in each vector: true negative, false positive, false negative, true positive.
        weightsVec_i <-
            as.vector(table(decTreePred[,c("response", "truth")]))
        
        # Collect the current 4 numbers.
        weightsVec <- c(weightsVec, weightsVec_i)
    }
    return(data.frame(wts=weightsVec))
}

# Custom function that uses the cell counts from the confusion matrices to compute sensitivity, specificity, positive predictive value, and negative predictive value.
quatPerf <- function(treeDf=NULL) {
    
    # perfs = performances. Vector that will collect the results.
    perfs <- c()
    # iters = iterations
    iters <- nrow(treeDf)/4
    # iiters = each number in iters is repeated four times.
    iiters <- rep(1:iters, each=4)
    for(i in 1:iters) {
        # quatCells: quat = four. Sorry, not every variable name I think of makes me proud (but then again, who cares, certainly not R).
        quatCells <- treeDf[iiters==i,"wts"]
        # sens = sensitivity (TP/(TP+FN))
        sens <- quatCells[4]/sum(quatCells[3:4])
        # spec = specificity (TN/(TN+FP))
        spec <- quatCells[1]/sum(quatCells[1:2])
        # npv = negative predictive value (TN/(TN+FN))
        npv <- quatCells[1]/sum(quatCells[c(1,3)])
        # ppv = positive predictive value (TP/(TP+FP))
        ppv <- quatCells[4]/sum(quatCells[c(2,4)])
        # Collect results in vector perfs.
        perfs <- c(perfs, sens, spec, ppv, npv)
    }
    # Put results in data.frame.
    out <- data.frame(matrix(perfs, nrow=iters, byrow = TRUE))
    # Set correct column names.
    colnames(out) <- c("sens", "spec", "ppv", "npv")
    return(out)
}

FPwts <- rtDf7$hb
# Collect results in a list
treeResLs <- list()
for(i in 1:length(testLs)) {
    
    print(i)
    
    # Apply custom function rpartFun.
    # Output is a data frame with only one column.
    checkTreeDf <- rpartFun(FPwt = FPwts, data=data1, trainIds = testLs[[i]]$train)
    
    # Add one column which shows the cell's name of the confusion matrix.
    # checkTreeDf has two columns: cell counts (column 1), cell names (column 2)
    checkTreeDf$cells <- c("TN", "FP", "FN", "TP")
    
    # Put checkTreeDf to the list.
    treeResLs[[paste0("checkTreeDf",i)]] <- checkTreeDf
    
    # Apply custom function quatPerf to checkTreeDf.
    # Output is a data.frame with 7 rows and 4 columns. The columns contain sensitivity, specificity, positive predictive value, and negative predictive value.
    perfsDf <- quatPerf(checkTreeDf)
    # Add a column (name: cost), which will be used later on.
    perfsDf$cost <- 1:nrow(perfsDf)
    # Make index vector that will put the added column cost to be the first column.
    rearrangedCols <- c(ncol(perfsDf), 1:(ncol(perfsDf)-1))
    # Apply this index vector to perfsDf.
    perfsDf <- perfsDf[,rearrangedCols]
    
    # Put perfsDf (perfs = performances) to the list.
    treeResLs[[paste0("perfDf",i)]] <- perfsDf
    
}

# Final computation, use list cartLs to collect the results.
cartLs <- list()
for(i in 1:(length(treeResLs)/2)) {
    
    # Extract results (part 1) from the list treeResLs.
    treeCheck1 <- treeResLs[[paste0("checkTreeDf",i)]]
    # Transform to a data.frame object.
    matCheck1 <- as.data.frame(matrix(treeCheck1$wts, nrow=7, byrow=TRUE))
    # Set the correct column names.
    colnames(matCheck1) <- treeCheck1$cells[1:4]
    
    # Extract results (part 2) from the list treeResLs.
    treeCheck2 <- treeResLs[[paste0("perfDf",i)]]
    # Take only the columns sensitivity (sens), specificity (spec), positive predictive value (ppv), and negative predictive value (npv).
    matCheck2 <- treeCheck2[,c("sens", "spec", "ppv", "npv")]
    
    # Put both extracted results into a single data.frame.
    combine <- cbind(matCheck1, matCheck2)
    
    # Add a column that contains the harm/benefit weights.
    combine$FPwt <- rtDf7$hb
    # Add a column that shows the iteration (between 1 and 100)
    combine$iter <- i
    # Add the combined results to the list cartLs.
    cartLs[[i]] <- combine
}

# Put the results from all 100 cross-validations into a single data.frame. Since seven threshold probabilities were selected, this yields a data.frame with 700 rows.
# The function 'as_tibble' from the package tibble makes sense only, if this cartDf (based on the simulated data) shall be used with some of the code in scripts E_DisplayResults or G_CARTsensitivityAnalysis (if cartDf is not of class tibble, other code won't work properly).
# However, if the code in scripts E_DisplayResults or G_CARTsensitivityAnalysis shall be used on the results that are based on the original data (which cannot be published), then there is no problem because these results are part of this predictSuiattPsyCoLaus package, already possessing the class tibble.
cartDf <- tibble::as_tibble(dplyr::bind_rows(cartLs))

# Display rows 1-10 of cartDf
cartDf[1:10,]
# --------------------------------------------------