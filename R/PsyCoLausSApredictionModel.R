#' Prediction model to predict future suicide attempt.
#'
#' This prediction model was developed by using the full dataset (N = 4050) in the publication, of which this R package is the supplementary material.
#' 
#' @format A list with 23 elements
#' 
#' This list is an S3 object of the classes "glm" and "lm". Of the original fitted generalized linear model (glm), every original raw data has been removed, due to data protection rules. That is, this prediction model can still be used with the convenient predict function, which is supplied by the R stats package. Indeed, this is the whole point why we provide our prediction model, namely to externally cross-validate it with other longitudinal data (Steyerberg and Harrell, 2016).
#' 
#' Instruction of how to use our prediction model in R:
#' 
#' \itemize{
#'   \item Make sure your data stems from a general community adult sample. If not, consider recalibration (Van Calster et al., 2019).
#'   \item Make sure your data contains the four predictors lifetime suicide attempt (any prior suicide attempt), any lifetime mental disorder diagnosis (any of the disorder classes affective/mood, anxiety, alcohol abuse/dependence, other substance abuse/dependence), sex (male, female), age.
#'   \item Make sure that the coding of the four predictors are 0 (for "no" and "male") and 1 (for "yes" and "female"), "no" meaning absence of prior suicide attempt or mental disorder diagnosis. The variable age must be a continous numeric variable (unit: years).
#'   \item Make sure the outcome in your data is future suicide attempt.
#'   \item Before applying our prediction model to your data, rename the predictor columns to lifetimeSA (= any prior suicide attempt), lifetimeMentalDx (any prior mental disorder), sex, and age.
#' }
#' 
#' @examples
#' # Simulated external dataset with four predictors to predict the probability
#' # of a future suicide attempt.
#' simData <- data.frame(lifetimeSA=c(0,1,0),
#'                       lifetimeMentalDx=c(0,0,1),
#'                       sex=c(1,0,1),
#'                       age=c(45, 51, 67))
#'  # The prediction model gets loaded automatically when loading this package.
#'  futureSAprobs <- predict(object = PsyCoLausSApredictionModel,
#'                           newdata = simData, type = "response")
#'  
#' 
#' @docType data
#' @keywords model
#' @importFrom Rdpack reprompt
#' @name PsyCoLausSApredictionModel
#' 
#' @references
#' \insertRef{steyerberg2016prediction}{predictSuiattPsyCoLaus}
#' 
#' \insertRef{van2019calibration}{predictSuiattPsyCoLaus}
#' 
NULL