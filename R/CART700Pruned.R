#' A list, each list element is a data set, output of 700 cross-validated, and optimized, CART-based predictions of a future suicide attempt.
#'
#' CART = Classification and regression trees (Breiman et al., 1984) algorithm. Optimization argument cp (for pruning the decision tree), see \strong{Details} below.
#'
#' @format Each of the six data.frame in the list contains 700 rows and 10 columns:
#' \itemize{
#'   \item TN = True Negatives; correctly predicted that the outcome is absent.
#'   \item FP = False Positives; falsely predicted the outcome to be present.
#'   \item FN = False Negatives; falsely predicted the outcome to be absent.
#'   \item TP = True Positives; correctly predicted the outcome to be present.
#'   \item sens = Sensitivity; true positive rate; rate of subjects with the outcome who were predicted to have developed the outcome; TP/(TP+FN).
#'   \item spec = Specificity; true negative rate; rate of subjects without the outcome who were predicted not to have developed the outcome; TN/(TN+FP).
#'   \item ppv = Positive Predictive Value; TP/(TP+FP).
#'   \item npv = Negative Predictive Value; TN/(TN+FN).
#'   \item FPwt, see \strong{Details} below).
#'   \item iter = iteration. Values between 1 and 700 (there were 100 cross-validated predictions for each of the 7 selected threshold probabilities, yielding 700 overall cross-validated predictions).
#' }
#'
#' @details The argument cp in the function rpart (of the rpart package) has been set to: 0 (no pruning), 0.01, 0.02, 0.03, 0.04, and 0.05. For each of these values, we ran the same 700 cross-validations as in the main analyses. Depending on the value of cp, the least important splits of the fully grown decision tree get snipped off (see R package documentation of rpart, see function name prune.rpart).
#' 
#' FPwt = False Positive weight. This is the one half part of the harm-to-benefit ratio, which is used to weigh the false positives (FP).
#' 
#' For instance, take row 1 of the CART results (the first of the six data sets in the list of CART700Pruned), where there are 204 FP subjects and the FPwt is 199 (which stands for 1:199). Therefore, 204 is multiplied by 1/199, which yields 1.025126 (which is the weighted FP). This weighted FP is subtracted from the TP (6), which yields 4.974874. Finally, this result is divided by the sample size (which in all 700 cross-validation subsets is 810). Therefore, 4.974874/810 yields the net benefit of 0.00614182.
#' 
#' Note. Vignette E in this predictSuiattPsyCoLaus package show how net benefit and other metrics are computed, based on this CART700Pruned output.
#'
#' @docType data
#' @keywords datasets
#' @name CART700Pruned
#'
#' @references
#' 
#' \insertRef{breiman1984classification}{predictSuiattPsyCoLaus}
#' 
#' \insertRef{tibble2021}{predictSuiattPsyCoLaus}
#'
#' @usage data(CART700Pruned)
#' @examples
#' # Display the structure of the first data set in the console
#' str(CART700Pruned[[1]])
#' # Console output:
#' # tibble [700 × 10] (S3: tbl_df/tbl/data.frame)
#' # $ TN  : num [1:700] 596 603 606 659 684 684 696 577 574 574 ...
#' # $ FP  : num [1:700] 204 197 194 141 116 116 104 223 226 226 ...
#' # $ FN  : num [1:700] 4 4 4 5 5 5 5 5 4 4 ...
#' # $ TP  : num [1:700] 6 6 6 5 5 5 5 5 6 6 ...
#' # $ sens: num [1:700] 0.6 0.6 0.6 0.5 0.5 0.5 0.5 0.5 0.6 0.6 ...
#' # $ spec: num [1:700] 0.745 0.754 0.757 0.824 0.855 ...
#' # $ ppv : num [1:700] 0.0286 0.0296 0.03 0.0342 0.0413 ...
#' # $ npv : num [1:700] 0.993 0.993 0.993 0.992 0.993 ...
#' # $ FPwt: num [1:700] 199 132.3 99 79 65.7 ...
#' # $ iter: num [1:700] 1 1 1 1 1 1 1 2 2 2 ...
#'
#' @source R package predictSuiattPsyCoLaus
"CART700Pruned"
