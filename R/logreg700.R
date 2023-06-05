#' A data set, output of 700 cross-validated logreg-based predictions of a future suicide attempt.
#'
#' logreg = Logistic regression algorithm.
#'
#' @format A data.frame with 700 rows and 10 columns:
#' \itemize{
#'   \item TN = True Negatives; correctly predicted that the outcome is absent.
#'   \item FP = False Positives; falsely predicted the outcome to be present.
#'   \item FN = False Negatives; falsely predicted the outcome to be absent.
#'   \item TP = True Positives; correctly predicted the outcome to be present.
#'   \item sens = Sensitivity; true positive rate; rate of subjects with the outcome who were predicted to have developed the outcome; TP/(TP+FN).
#'   \item spec = Specificity; true negative rate; rate of subjects without the outcome who were predicted not to have developed the outcome; TN/(TN+FP).
#'   \item ppv = Positive Predictive Value; TP/(TP+FP).
#'   \item npv = Negative Predictive Value; TN/(TN+FN).
#'   \item thrsh = threshold, see \strong{Details} below).
#'   \item iter = iteration. Values between 1 and 700 (there were 100 cross-validated predictions for each of the 7 selected threshold probabilities, yielding 700 overall cross-validated predictions).
#' }
#'
#' @details thrsh = threshold probability. The 7 selected threshold probabilities (0.5%, 0.75%, 1%, 1.25%, 1.5%, 1.75%, and 2%) are used to weigh the False Positives, after having transformed the thresholds to the harm-to-benefit ratio.
#' 
#' For instance, take row 1 of the logistic regression results, where there are 440 FP subjects and the threshold is 0.5% (which transforms to a harm-to-benefit ratio of 1:199). Therefore, 440 is multiplied by 1/199, which yields 2.211055 (which is the weighted FP). This weighted FP is subtracted from the TP (9), which yields 6.788945. Finally, this result is divided by the sample size (which in all 700 cross-validation subsets is 810). Therefore, 6.788945/810 yields the net benefit of 0.008381414.
#' 
#' Note. Vignette E in this predictSuiattPsyCoLaus package show how net benefit and other metrics are computed, based on this logreg700 output.
#'
#' @docType data
#' @keywords datasets
#' @name logreg700
#'
#' @references
#' 
#' \insertRef{tibble2021}{predictSuiattPsyCoLaus}
#'
#' @usage data(logreg700)
#' @examples
#' # Display the structure of the data set in the console
#' str(logreg700)
#' # Console output:
#' # tibble [700 × 10] (S3: tbl_df/tbl/data.frame)
#' # $ TN   : num [1:700] 360 410 502 608 688 755 760 406 497 570 ...
#' # $ FP   : num [1:700] 440 390 298 192 112 45 40 394 303 230 ...
#' # $ FN   : num [1:700] 1 1 2 3 5 5 5 2 2 4 ...
#' # $ TP   : num [1:700] 9 9 8 7 5 5 5 8 8 6 ...
#' # $ sens : num [1:700] 0.9 0.9 0.8 0.7 0.5 0.5 0.5 0.8 0.8 0.6 ...
#' # $ spec : num [1:700] 0.45 0.512 0.627 0.76 0.86 ...
#' # $ ppv  : num [1:700] 0.02 0.0226 0.0261 0.0352 0.0427 ...
#' # $ npv  : num [1:700] 0.997 0.998 0.996 0.995 0.993 ...
#' # $ thrsh: num [1:700] 0.5 0.75 1 1.25 1.5 1.75 2 0.5 0.75 1 ...
#' # $ iter : num [1:700] 1 1 1 1 1 1 1 2 2 2 ...
#'
#' @source R package predictSuiattPsyCoLaus
"logreg700"
