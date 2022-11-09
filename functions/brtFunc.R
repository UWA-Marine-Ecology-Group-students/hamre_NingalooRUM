
#####' gbm.bootstrap.functions ####
#'
#' Function to calculate bootstrap estimates of 95% confidence intervals for partial plots of a brt model fitted with gbm.step. See e.g. Buston and Elith 2011 (J. Anim. Ecol). 'gbm.bootstrap.functions', 'plot.gbm.4list' and 'plot.gbm.boot' were originally written by Jane Elith and John Leathwick, but not released publicly. We thank them for providing this code.
#'
#' @param gbm.object a gbm object describing sample intensity
#' @param list.predictors a list of data from a run of plot.gbm.4list
#' @param n.reps number of bootstrap samples
#' @param n.divisions the number of used in continuous resolution of plot.gbm
#' @param verbose control reporting
#'
#' @export
gbm.bootstrap.functions <- function(
  gbm.object,          # a gbm object describing sample intensity
  list.predictors,  			  # a list of data from a run of plot.gbm.4list
  n.reps = 200,               # number of bootstrap samples
  n.divisions = 100,          # the number of used in continuous resolution of plot.gbm
  verbose = T)                # control reporting
{
  
  # based version 2.9 - J. Leathwick/J. Elith - June 2007
  # updated by Jane Elith 2008 to 2010, to deal with asin data and other various fixes
  # updated by JE Nov 2010 to cut it down just for partial plots, and use the plot.gbm code within that
  # updated by J-B Jouffray March 2017 to replace the use of eval(parse(text=))
  
  require(gbm)
  
  # first get the original analysis details..
  
  gbm.call <- gbm.object$gbm.call
  train.data <- gbm.call$dataframe
  n.obs <- nrow(train.data)
  gbm.x <- gbm.call$gbm.x
  gbm.y <- gbm.call$gbm.y
  family <- gbm.call$family
  lr <- gbm.call$learning.rate
  tc <- gbm.call$tree.complexity
  response.name <- gbm.call$response.name
  predictor.names <- gbm.call$predictor.names
  n.preds <- length(gbm.x)
  n.trees <- gbm.call$best.trees
  weights <- gbm.object$weights
  
  function.preds <- array(0, dim=c(n.divisions, n.preds,n.reps))
  
  cat("gbm.pred.bootstrap - version 2.9","\n\n")
  cat("bootstrap resampling gbm.step model for ",response.name,"\n",sep="")
  cat("with ",n.trees," trees and ",n.obs," observations\n\n",sep="")
  
  # initiate timing call
  
  z1 <- unclass(Sys.time())
  
  # create gbm.fixed function call
  # Note: the models fitted within the bootstrap all have the same number of
  # trees, and are not optimised for each bootstrap sample. This is a shortcut
  # that saves computation time; the effect on the outcome has not been explored
  # but intuitively it will be conservative in the sense that it will introduce
  # more noise if anything.
  
  gbm.call.string <- paste("gbm.fixed(data=boot.data,gbm.x=gbm.x,gbm.y=gbm.y,",sep="")
  gbm.call.string <- paste(gbm.call.string,"family=family,learning.rate=lr,tree.complexity=tc,",sep="")
  gbm.call.string <- paste(gbm.call.string,"n.trees = ",n.trees,", site.weights = weights,verbose=FALSE)",sep="")
  
  # now start the main bootstrap loop
  
  for (i in 1:n.reps) {
    
    if (i == 6 & verbose) {
      
      z2 <- unclass(Sys.time())
      est.time <- (z2 - z1)/60  # time for five reps
      est.time <- est.time * (n.reps/5)
      cat("five bootstrap samples processed \n"," estimated time for completion is ",
          round(est.time,1)," minutes \n",sep="")
    }
    else {
      if (verbose) cat(i,"\n")
    }
    
    # create a vector with which to select the bootstrap sample
    
    i.select <- sample(1:n.obs, n.obs, replace = T)
    boot.data <- train.data[i.select,]
    
    #fit the model
    boot.model.gbm <- eval(parse(text=gbm.call.string))
    #make the predictions
    function.preds[,,i] <- plot.gbm.boot(boot.model.gbm, list.4.preds = list.predictors, continuous.resolution = n.divisions)
  }
  
  # final timing call
  z2 <- unclass(Sys.time())
  elapsed.time <- round((z2 - z1)/60,2)
  cat("analysis took ",round(elapsed.time,1)," minutes \n\n")
  gbm.call$n.bootstrap.reps <- n.reps
  gbm.call$bootstrap.time <- elapsed.time
  final.object <- list(gbm.call = gbm.call)
  final.object$function.preds <- function.preds
  
  return(final.object)
}

#### gbmStepFunc ####
gbm.step <- function (
  data,                                     # the input dataframe
  gbm.x,                                    # the predictors
  gbm.y,                                    # and response
  offset = NULL,                            # allows an offset to be specified
  fold.vector = NULL,                       # allows a fold vector to be read in for CV with offsets,
  tree.complexity = 1,                      # sets the complexity of individual trees
  learning.rate = 0.01,                     # sets the weight applied to inidivudal trees
  bag.fraction = 0.75,                      # sets the proportion of observations used in selecting variables
  site.weights = rep(1, nrow(data)),        # allows varying weighting for sites
  var.monotone = rep(0, length(gbm.x)),     # restricts responses to individual predictors to monotone 
  n.folds = 10,                             # number of folds
  prev.stratify = TRUE,                     # prevalence stratify the folds - only for p/a data
  family = "bernoulli",                     # family - bernoulli (=binomial), poisson, laplace or gaussian
  n.trees = 50,                             # number of initial trees to fit
  step.size = n.trees,                      # numbers of trees to add at each cycle
  max.trees = 10000,                        # max number of trees to fit before stopping
  tolerance.method = "auto",                # method to use in deciding to stop - "fixed" or "auto"
  tolerance = 0.001,                        # tolerance value to use - if method == fixed is absolute, 
  # if auto is multiplier * total mean deviance
  plot.main = TRUE,                         # plot hold-out deviance curve
  plot.folds = FALSE,                       # plot the individual folds as well
  verbose = TRUE,                           # control amount of screen reporting
  silent = FALSE,                           # to allow running with no output for simplifying model)
  keep.fold.models = FALSE,                 # keep the fold models from cross valiation
  keep.fold.vector = FALSE,                 # allows the vector defining fold membership to be kept
  keep.fold.fit = FALSE,                    # allows the predicted values for observations from CV to be kept
  ...)                                      # allows for any additional plotting parameters
{
  
  if (! requireNamespace('gbm') ) { stop ('you need to install the gbm package to run this function') }
  requireNamespace('splines')
  
  if (silent) verbose <- FALSE
  
  # initiate timing call
  
  z1 <- Sys.time()
  
  # setup input data and assign to position one
  
  #  dataframe.name <- deparse(substitute(data))   # get the dataframe name
  #  data <- eval(data)
  x.data <- data[, gbm.x, drop=FALSE]                 #form the temporary datasets
  #names(x.data) <- names(data)[gbm.x]
  y.data <- data[, gbm.y]
  sp.name <- names(data)[gbm.y]
  if (family == "bernoulli") {
    prevalence <- mean(y.data)
  }
  
  #  assign("x.data", x.data, env = globalenv())               #and assign them for later use
  #  assign("y.data", y.data, env = globalenv())
  
  #offset.name <- deparse(substitute(offset))   # get the dataframe name
  #offset <- eval(offset)
  
  n.cases <- nrow(data)
  n.preds <- length(gbm.x)
  
  if (!silent) {
    cat("\n","\n","GBM STEP - version 2.9","\n","\n")
    cat("Performing cross-validation optimisation of a boosted regression tree model \n")
    cat("for", sp.name, "and using a family of",family,"\n")
    cat("Using",n.cases,"observations and",n.preds,"predictors \n")
  }
  
  # set up the selector variable either with or without prevalence stratification
  
  if (is.null(fold.vector)) {
    
    if (prev.stratify & family == "bernoulli") {
      presence.mask <- data[,gbm.y] == 1
      absence.mask <- data[,gbm.y] == 0
      n.pres <- sum(presence.mask)
      n.abs <- sum(absence.mask)
      
      # create a vector of randomised numbers and feed into presences
      selector <- rep(0, n.cases)
      temp <- rep(seq(1, n.folds, by = 1), length = n.pres)
      temp <- temp[order(runif(n.pres, 1, 100))]
      selector[presence.mask] <- temp
      
      # and then do the same for absences
      temp <- rep(seq(1, n.folds, by = 1), length = n.abs)
      temp <- temp[order(runif(n.abs, 1, 100))]
      selector[absence.mask] <- temp
    } else {  #otherwise make them random with respect to presence/absence
      selector <- rep(seq(1, n.folds, by = 1), length = n.cases)
      selector <- selector[order(runif(n.cases, 1, 100))]
    }
  } else {
    if (length(fold.vector) != n.cases) {
      stop("supplied fold vector is of wrong length")
    }
    cat("loading user-supplied fold vector \n")
    selector <- fold.vector
  }
  
  # set up the storage space for results
  
  pred.values <- rep(0, n.cases)
  
  cv.loss.matrix <- matrix(0, nrow = n.folds, ncol = 1)
  training.loss.matrix <- matrix(0, nrow = n.folds, ncol = 1)
  trees.fitted <- n.trees
  
  model.list <- list(paste("model", 1:n.folds, sep=""))     # dummy list for the tree models
  
  # set up the initial call to gbm
  
  if (is.null(offset)) {
    gbm.call <- paste("gbm::gbm(y.subset ~ .,data=x.subset, n.trees = n.trees, interaction.depth = tree.complexity, shrinkage = learning.rate, bag.fraction = bag.fraction, weights = weight.subset, distribution = as.character(family), var.monotone = var.monotone, verbose = FALSE)", sep="")
  } else {
    gbm.call <- paste("gbm::gbm(y.subset ~ . + offset(offset.subset), data=x.subset, n.trees = n.trees, interaction.depth = tree.complexity, shrinkage = learning.rate, bag.fraction = bag.fraction, weights = weight.subset, distribution = as.character(family), var.monotone = var.monotone, verbose = FALSE)", sep="")
  } 
  
  n.fitted <- n.trees
  
  # calculate the total deviance
  
  y_i <- y.data
  
  u_i <- sum(y.data * site.weights) / sum(site.weights)
  u_i <- rep(u_i,length(y_i))
  
  total.deviance <- calc.deviance(y_i, u_i, weights = site.weights, family = family, calc.mean = FALSE)
  
  mean.total.deviance <- total.deviance/n.cases
  
  tolerance.test <- tolerance
  
  if (tolerance.method == "auto") {
    tolerance.test <- mean.total.deviance * tolerance
  }
  
  # now step through the folds setting up the initial call
  
  if (!silent){ 
    cat("creating",n.folds,"initial models of",n.trees,"trees","\n")
    if (prev.stratify & family == "bernoulli") {
      cat("\n","folds are stratified by prevalence","\n")
    } else {
      cat("\n","folds are unstratified","\n")
    }
    cat ("total mean deviance = ",round(mean.total.deviance,4),"\n")
    cat("tolerance is fixed at ",round(tolerance.test,4),"\n")
    if (tolerance.method != "fixed" & tolerance.method != "auto") {
      stop("invalid argument for tolerance method - should be auto or fixed")
    }
  }
  
  if (verbose) {
    cat("ntrees resid. dev.","\n")
  }
  
  for (i in 1:n.folds) {
    
    model.mask <- selector != i  #used to fit model on majority of data
    pred.mask <- selector == i   #used to identify the with-held subset
    
    y.subset <- y.data[model.mask]
    x.subset <- x.data[model.mask, ,drop=FALSE]
    weight.subset <- site.weights[model.mask]
    
    if (!is.null(offset)) {
      offset.subset <- offset[model.mask] 
    } else {
      offset.subset <- NULL
    }
    
    model.list[[i]] <- eval(parse(text = gbm.call))
    
    fitted.values <- model.list[[i]]$fit  #predict.gbm(model.list[[i]], x.subset, type = "response", n.trees = n.trees)
    if (!is.null(offset)) {
      fitted.values <- fitted.values + offset[model.mask]
    }
    if (family == "bernoulli") {
      fitted.values <- exp(fitted.values)/(1 + exp(fitted.values))
    } else if (family == "poisson") {
      fitted.values <- exp(fitted.values)
    }
    
    pred.values[pred.mask] <- gbm::predict.gbm(model.list[[i]], x.data[pred.mask, ,drop=FALSE], n.trees = n.trees)
    
    if (!is.null(offset)) {
      pred.values[pred.mask] <- pred.values[pred.mask] + offset[pred.mask]
    }
    if (family == "bernoulli") {
      pred.values[pred.mask] <- exp(pred.values[pred.mask])/(1 + exp(pred.values[pred.mask]))
    } else if (family == "poisson") {
      pred.values[pred.mask] <- exp(pred.values[pred.mask])
    }
    
    # calc training deviance
    
    y_i <- y.subset
    u_i <- fitted.values
    weight.fitted <- site.weights[model.mask]
    training.loss.matrix[i,1] <- calc.deviance(y_i, u_i, weight.fitted, family = family)
    
    # calc holdout deviance
    
    y_i <- y.data[pred.mask]
    u_i <- pred.values[pred.mask]
    weight.preds <- site.weights[pred.mask]
    cv.loss.matrix[i,1] <- calc.deviance(y_i, u_i, weight.preds, family = family)
    
  } # end of first loop
  
  # now process until the change in mean deviance is =< tolerance or max.trees is exceeded
  
  delta.deviance <- 1
  
  cv.loss.values <- apply(cv.loss.matrix,2,mean)
  if (verbose) {
    cat(n.fitted,"  ",round(cv.loss.values,4),"\n")
  }
  if (!silent) {
    cat("now adding trees...","\n")
  }
  
  j <- 1
  
  while (delta.deviance > tolerance.test & n.fitted < max.trees) { 
    # beginning of inner loop
    
    # add a new column to the results matrice..
    
    training.loss.matrix <- cbind(training.loss.matrix,rep(0,n.folds))
    cv.loss.matrix <- cbind(cv.loss.matrix,rep(0,n.folds))
    
    n.fitted <- n.fitted + step.size
    trees.fitted <- c(trees.fitted,n.fitted)
    
    j <- j + 1
    
    for (i in 1:n.folds) {
      
      model.mask <- selector != i  #used to fit model on majority of data
      pred.mask <- selector == i   #used to identify the with-held subset
      
      y.subset <- y.data[model.mask]
      x.subset <- x.data[model.mask, ,drop=FALSE]
      weight.subset <- site.weights[model.mask]
      if (!is.null(offset)) {
        offset.subset <- offset[model.mask] 
      }
      model.list[[i]] <- gbm::gbm.more(model.list[[i]], weights = weight.subset, step.size)
      
      fitted.values <- model.list[[i]]$fit # predict.gbm(model.list[[i]],x.subset, type = "response", n.trees = n.fitted) 
      if (!is.null(offset)) {
        fitted.values <- fitted.values + offset[model.mask]
      }
      if (family == "bernoulli") {
        fitted.values <- exp(fitted.values)/(1 + exp(fitted.values))
      } else if (family == "poisson") {
        fitted.values <- exp(fitted.values)
      }
      pred.values[pred.mask] <- gbm::predict.gbm(model.list[[i]], x.data[pred.mask, ,drop=FALSE], n.trees = n.fitted)
      
      if (!is.null(offset)) {
        pred.values[pred.mask] <- pred.values[pred.mask] + offset[pred.mask]
      }
      
      if (family == "bernoulli") {
        pred.values[pred.mask] <- exp(pred.values[pred.mask])/(1 + exp(pred.values[pred.mask]))
      } else if (family == "poisson") {
        pred.values[pred.mask] <- exp(pred.values[pred.mask])
      }
      # calculate training deviance
      
      y_i <- y.subset
      u_i <- fitted.values
      weight.fitted <- site.weights[model.mask]
      training.loss.matrix[i,j] <- calc.deviance(y_i, u_i, weight.fitted, family = family)
      
      # calc holdout deviance
      
      u_i <- pred.values[pred.mask]
      y_i <- y.data[pred.mask]
      weight.preds <- site.weights[pred.mask]
      cv.loss.matrix[i,j] <- calc.deviance(y_i, u_i, weight.preds, family = family)
      
    }  # end of inner loop
    
    cv.loss.values <- apply(cv.loss.matrix,2,mean)
    
    if (j < 5) {
      if (cv.loss.values[j] > cv.loss.values[j-1]) {
        if (!silent) {
          cat("restart model with a smaller learning rate or smaller step size...")
        }
        return()
      }
    }
    
    if (j >= 20) {   #calculate stopping rule value
      test1 <- mean(cv.loss.values[(j-9):j])
      test2 <- mean(cv.loss.values[(j-19):(j-9)])
      delta.deviance <- test2 - test1
    }
    
    if (verbose) {
      cat(n.fitted," ",round(cv.loss.values[j],4),"\n") 
      flush.console()
    }
  } # end of while loop
  
  # now begin process of calculating optimal number of trees
  
  training.loss.values <- apply(training.loss.matrix,2,mean)
  
  cv.loss.ses <- rep(0,length(cv.loss.values))
  cv.loss.ses <- sqrt(apply(cv.loss.matrix,2,var)) / sqrt(n.folds)
  
  # find the target holdout deviance
  
  y.bar <- min(cv.loss.values) 
  
  
  # identify the optimal number of trees 
  
  target.trees <- trees.fitted[match(TRUE, cv.loss.values == y.bar)]
  
  # plot out the resulting curve of holdout deviance 
  if (plot.main) {
    
    y.min <- min(cv.loss.values - cv.loss.ses)  #je added multiplier 10/8/05
    y.max <- max(cv.loss.values + cv.loss.ses)  #je added multiplier 10/8/05 }
    
    if (plot.folds) {
      y.min <- min(cv.loss.matrix)
      y.max <- max(cv.loss.matrix) 
    }
    
    plot(trees.fitted, cv.loss.values, type = 'l', ylab = "holdout deviance", xlab = "no. of trees", ylim = c(y.min,y.max), ...)
    abline(h = y.bar, col = 2)
    
    lines(trees.fitted, cv.loss.values + cv.loss.ses, lty=2)  
    lines(trees.fitted, cv.loss.values - cv.loss.ses, lty=2)  
    
    if (plot.folds) {
      for (i in 1:n.folds) {
        lines(trees.fitted, cv.loss.matrix[i,],lty = 3)
      }
    }
    abline(v = target.trees, col=3)
    title(paste(sp.name,", d - ",tree.complexity,", lr - ",learning.rate, sep=""))
  }
  
  # estimate the cv deviance and test statistics
  # includes estimates of the standard error of the fitted values added 2nd may 2005
  
  cv.deviance.stats <- rep(0, n.folds)
  cv.roc.stats <- rep(0, n.folds)
  cv.cor.stats <- rep(0, n.folds)
  cv.calibration.stats <- matrix(0, ncol=5, nrow = n.folds)
  if (family == "bernoulli") {
    threshold.stats <- rep(0, n.folds)
  }
  fitted.matrix <- matrix(NA, nrow = n.cases, ncol = n.folds)  # used to calculate se's
  fold.fit <- rep(0, n.cases)
  
  for (i in 1:n.folds) {
    
    pred.mask <- selector == i   #used to identify the with-held subset
    model.mask <- selector != i  #used to fit model on majority of data
    
    fits <- gbm::predict.gbm(model.list[[i]], x.data[model.mask, ,drop=FALSE], n.trees = target.trees)
    if (!is.null(offset)) {
      fits <- fits + offset[model.mask]
    }
    if (family == "bernoulli") {
      fits <- exp(fits)/(1 + exp(fits))
    } else if (family == "poisson") {
      fits <- exp(fits)
    }
    fitted.matrix[model.mask,i] <- fits
    
    fits <- gbm::predict.gbm(model.list[[i]], x.data[pred.mask, ,drop=FALSE], n.trees = target.trees)
    if (!is.null(offset)) fits <- fits + offset[pred.mask]
    fold.fit[pred.mask] <- fits  # store the linear predictor values
    if (family == "bernoulli") {
      fits <- exp(fits)/(1 + exp(fits))
    } else if (family == "poisson") {
      fits <- exp(fits)
    }
    fitted.matrix[pred.mask,i] <- fits
    
    y_i <- y.data[pred.mask] 
    u_i <- fitted.matrix[pred.mask,i]  #pred.values[pred.mask]
    weight.preds <- site.weights[pred.mask]
    
    cv.deviance.stats[i] <- calc.deviance(y_i, u_i, weight.preds, family = family)
    
    cv.cor.stats[i] <- cor(y_i,u_i)
    
    if (family == "bernoulli") {
      cv.roc.stats[i] <- .roc(y_i,u_i)
      cv.calibration.stats[i,] <- .calibration(y_i,u_i,"binomial")
      threshold.stats[i] <- approx(ppoints(u_i), sort(u_i,decreasing = T), prevalence)$y
    }
    
    if (family == "poisson") {
      cv.calibration.stats[i,] <- .calibration(y_i,u_i,"poisson")
    }
  }
  
  fitted.vars <- apply(fitted.matrix,1, var, na.rm = TRUE)
  
  # now calculate the mean and se's for the folds
  
  cv.dev <- mean(cv.deviance.stats, na.rm = TRUE)
  cv.dev.se <- sqrt(var(cv.deviance.stats)) / sqrt(n.folds)
  
  cv.cor <- mean(cv.cor.stats, na.rm = TRUE)
  cv.cor.se <- sqrt(var(cv.cor.stats, use = "complete.obs")) / sqrt(n.folds)
  
  cv.roc <- 0.0
  cv.roc.se <- 0.0 
  
  if (family == "bernoulli") {
    cv.roc <- mean(cv.roc.stats,na.rm=TRUE)
    cv.roc.se <- sqrt(var(cv.roc.stats, use = "complete.obs")) / sqrt(n.folds)  
    cv.threshold <- mean(threshold.stats, na.rm = T)
    cv.threshold.se <- sqrt(var(threshold.stats, use = "complete.obs")) / sqrt(n.folds)  
  }
  
  cv.calibration <- 0.0
  cv.calibration.se <- 0.0
  
  if (family == "poisson" | family == "bernoulli") {
    cv.calibration <- apply(cv.calibration.stats,2,mean)
    cv.calibration.se <- apply(cv.calibration.stats,2,var)
    cv.calibration.se <- sqrt(cv.calibration.se) / sqrt(n.folds) 
  }
  
  # fit the final model
  
  if (is.null(offset)) {
    gbm.call <- paste("gbm::gbm(y.data ~ .,data=x.data, n.trees = target.trees, interaction.depth = tree.complexity, shrinkage = learning.rate, bag.fraction = bag.fraction, weights = site.weights, distribution = as.character(family), var.monotone = var.monotone, verbose = FALSE)", sep="")
  } else {
    gbm.call <- paste("gbm::gbm(y.data ~ . + offset(offset),data=x.data, n.trees = target.trees, interaction.depth = tree.complexity, shrinkage = learning.rate, bag.fraction = bag.fraction, weights = site.weights, distribution = as.character(family), var.monotone = var.monotone,  verbose = FALSE)", sep="")
  } 
  
  if (!silent) {
    message("fitting final gbm model with a fixed number of ", target.trees, " trees for ", sp.name) 
  }
  gbm.object <- eval(parse(text = gbm.call))
  
  best.trees <- target.trees
  
  #extract fitted values and summary table
  
  gbm.summary <- summary(gbm.object,n.trees = target.trees, plotit = FALSE)
  
  fits <- gbm::predict.gbm(gbm.object, x.data, n.trees = target.trees)
  if (!is.null(offset)) fits <- fits + offset
  if (family == "bernoulli") {
    fits <- exp(fits)/(1 + exp(fits))
  } else if (family == "poisson") {
    fits <- exp(fits)
  }
  fitted.values <- fits
  
  y_i <- y.data
  u_i <- fitted.values
  resid.deviance <- calc.deviance(y_i, u_i, weights = site.weights, family = family, calc.mean = FALSE)
  
  self.cor <- cor(y_i,u_i)
  self.calibration <- 0.0
  self.roc <- 0.0
  
  if (family == "bernoulli") {  # do this manually as we need the residuals
    deviance.contribs <- (y_i * log(u_i)) + ((1-y_i) * log(1 - u_i))
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
    self.roc <- .roc(y_i,u_i)
    self.calibration <- .calibration(y_i,u_i,"binomial")
  }
  
  if (family == "poisson") {   # do this manually as we need the residuals
    deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - (y_i - u_i)
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
    self.calibration <- .calibration(y_i,u_i,"poisson")
  }
  
  if (family == "gaussian" | family == "laplace") {
    residuals <- y_i - u_i
  }
  
  mean.resid.deviance <- resid.deviance/n.cases
  
  z2 <- Sys.time()
  elapsed.time.minutes <- round(as.numeric(z2 - z1)/ 60, 2)  #calculate the total elapsed time
  
  if (verbose) {
    cat("\n")
    cat("mean total deviance =", round(mean.total.deviance,3),"\n")
    cat("mean residual deviance =", round(mean.resid.deviance,3),"\n","\n")
    cat("estimated cv deviance =", round(cv.dev,3),"; se =", round(cv.dev.se,3),"\n","\n")
    cat("training data correlation =",round(self.cor,3),"\n")
    cat("cv correlation = ",round(cv.cor,3),"; se =",round(cv.cor.se,3),"\n","\n")
    if (family == "bernoulli") {
      cat("training data AUC score =",round(self.roc,3),"\n")
      cat("cv AUC score =",round(cv.roc,3),"; se =",round(cv.roc.se,3),"\n","\n")
    }
    cat("elapsed time - ",round(elapsed.time.minutes,2),"minutes","\n")
  }
  
  if (n.fitted == max.trees & !silent) {
    cat("\n","########### warning ##########","\n","\n")
    cat("maximum tree limit reached - results may not be optimal","\n")
    cat("  - refit with faster learning rate or increase maximum number of trees","\n") 
  }
  
  # now assemble data to be returned
  
  gbm.detail <- list(dataframe = data, gbm.x = gbm.x, predictor.names = names(x.data), 
                     gbm.y = gbm.y, response.name = sp.name, offset = offset, family = family, tree.complexity = tree.complexity, 
                     learning.rate = learning.rate, bag.fraction = bag.fraction, cv.folds = n.folds, 
                     prev.stratification = prev.stratify, max.fitted = n.fitted, n.trees = target.trees, 
                     best.trees = target.trees, train.fraction = 1.0, tolerance.method = tolerance.method, 
                     tolerance = tolerance, var.monotone = var.monotone, date = date(), 
                     elapsed.time.minutes = elapsed.time.minutes)
  
  training.stats <- list(null = total.deviance, mean.null = mean.total.deviance, 
                         resid = resid.deviance, mean.resid = mean.resid.deviance, correlation = self.cor, 
                         discrimination = self.roc, calibration = self.calibration)
  
  cv.stats <- list(deviance.mean = cv.dev, deviance.se = cv.dev.se, 
                   correlation.mean = cv.cor, correlation.se = cv.cor.se,
                   discrimination.mean = cv.roc, discrimination.se = cv.roc.se,
                   calibration.mean = cv.calibration, calibration.se = cv.calibration.se)
  
  if (family == "bernoulli") {
    cv.stats$cv.threshold <- cv.threshold
    cv.stats$cv.threshold.se <- cv.threshold.se
  }
  
  #  rm(x.data,y.data, envir = globalenv())           #finally, clean up the temporary dataframes
  
  # and assemble results for return
  
  gbm.object$gbm.call <- gbm.detail
  gbm.object$fitted <- fitted.values
  gbm.object$fitted.vars <- fitted.vars
  gbm.object$residuals <- residuals
  gbm.object$contributions <- gbm.summary
  gbm.object$self.statistics <- training.stats
  gbm.object$cv.statistics <- cv.stats
  gbm.object$weights <- site.weights
  gbm.object$trees.fitted <- trees.fitted
  gbm.object$training.loss.values <- training.loss.values
  gbm.object$cv.values <- cv.loss.values
  gbm.object$cv.loss.ses <- cv.loss.ses
  gbm.object$cv.loss.matrix <- cv.loss.matrix
  gbm.object$cv.roc.matrix <- cv.roc.stats
  
  if (keep.fold.models) {
    gbm.object$fold.models <- model.list
  } else {
    gbm.object$fold.models <- NULL
  }
  
  if (keep.fold.vector) {
    gbm.object$fold.vector <- selector
  } else {
    gbm.object$fold.vector <- NULL
  }
  if (keep.fold.fit) {
    gbm.object$fold.fit <- fold.fit
  } else {
    gbm.object$fold.fit <- NULL
  }
  
  return(gbm.object)  
}

#### ggPDBoot ####
ggPD_boot<-function (gbm.object,predictor = NULL,n.plots = length(pred.names),list.4.preds=NULL,booted.preds=NULL, nrow=NULL,ncol=NULL,
                     col.line="darkorange",cex.line=0.5, type.ci="lines",col.ci= "grey80",cex.ci=0.3,lty.ci=2, alpha.ci=0.5,smooth = FALSE,
                     col.smooth="blue",cex.smooth=0.3,span=0.3,rug = FALSE,rug.pos="t",common.scale = TRUE,cis=c(0.025, 0.975),
                     y.label = "Fitted function",x.label=NULL,...){
  
  gbm.call <- gbm.object$gbm.call
  pred.names <- gbm.call$predictor.names
  
  ggPD_boot.plots<-function (gbm.object) {
    if (!requireNamespace("gbm")) {
      stop("you need to install the gbm package to run this function")
    }
    
    if (is.null(booted.preds)) {
      stop("you need to set booted.preds as the array from the bootstrap run
           (eg testboot$function.preds using testboot<-gbm.bootstrap.functions())")
    }
    
    if (is.null(list.4.preds)) {
      stop("you need to set list.4.preds as the result of plot.gbm.4list()")
    }
    requireNamespace("splines")
    gbm.x <- gbm.call$gbm.x
    response.name <- gbm.call$response.name
    nt<-gbm.object$n.trees
    data <- gbm.call$dataframe
    
    max.vars <- length(gbm.object$contributions$var)
    if (n.plots > max.vars) {
      n.plots <- max.vars
      warning("reducing no of plotted predictors to maximum available (",
              max.vars, ")")
    }
    predictors <- list(rep(NA, n.plots))
    responses <- list(rep(NA, n.plots))
    responses.lower <- list(rep(NA, n.plots))
    responses.upper <- list(rep(NA, n.plots))
    
    for (j in c(1:max.vars)) {
      k <- match(gbm.object$contributions$var[j], pred.names)
      
      if (is.null(x.label)) {
        var.name <- gbm.call$predictor.names[k]
      }
      else {
        var.name <- x.label
      }
      pred.data <- data[, gbm.call$gbm.x[k]]
      response.matrix <- gbm::plot.gbm(gbm.object, i.var = k, n.trees = nt, return.grid = TRUE,...)
      predictors[[j]] <- response.matrix[, 1]
      if (is.factor(data[, gbm.call$gbm.x[k]])) {
        predictors[[j]] <- factor(predictors[[j]], levels = levels(data[,gbm.call$gbm.x[k]]))
      }
      
      #responses[[j]] <- response.matrix[, 2] - mean(response.matrix[, 2])
      #taking away the mean mucks up the real values.
      
      num.values <- nrow(response.matrix)
      responses[[j]] <- response.matrix[,2]
      
      temp <-apply(booted.preds[,k,], 1, function(x){quantile(x, cis[1],na.rm=T)})
      responses.lower[[j]] <- temp[1:num.values]
      
      temp <-apply(booted.preds[,k,], 1, function(x){quantile(x, cis[2],na.rm=T)})
      responses.upper[[j]] <- temp[1:num.values]
      
      if(j == 1) {
        ymin = min(responses.lower[[j]])
        ymax = max(responses.upper[[j]])
        dat<-data.frame(pred.data)
      }
      else {
        ymin = min(ymin,min(responses.lower[[j]]))
        ymax = max(ymax,max(responses.upper[[j]]))
        dat<-data.frame(dat,pred.data)
      }
    }
    
    if (is.null(predictor)){
      
      fittedFunc<-list()
      fittedFunc.lower<-list()
      fittedFunc.upper<-list()
      fittedVal<-list()
      ribbon<-list()
      ggPD<-list()
      
      for (i in 1:n.plots) {
        k <- match(gbm.object$contributions$var[i], pred.names)
        var.name <- gbm.call$predictor.names[k]
        
        fittedFunc[[i]]<-data.frame(predictors[i],responses[i])
        colnames(fittedFunc[[i]])<-c("x","y")
        
        fittedFunc.lower[[i]]<-data.frame(predictors[i],responses.lower[i])
        colnames(fittedFunc.lower[[i]])<-c("x","y")
        
        fittedFunc.upper[[i]]<-data.frame(predictors[i],responses.upper[i])
        colnames(fittedFunc.upper[[i]])<-c("x","y")
        
        fittedVal[[i]]<-data.frame(gbm.object$fitted,dat[i])
        colnames(fittedVal[[i]])<-c("y","x")
        
        ribbon[[i]]<-data.frame("x"=fittedFunc.lower[[i]]$x,"ylow"=fittedFunc.lower[[i]]$y,"yup"=fittedFunc.upper[[i]]$y)
        
        if (is.factor(fittedFunc[[i]]$x)) {
          ggPD[[i]]<- ggplot(fittedFunc[[i]], aes(x=x,y=y))+
            geom_boxplot(color=col.line,size=cex.line)+
            geom_boxplot(data=fittedFunc.lower[[i]], aes(x=x,y=y),color=col.ci)+
            geom_boxplot(data=fittedFunc.upper[[i]], aes(x=x,y=y),color=col.ci)+
            ylab(y.label)+
            xlab(paste(var.name, "  (", round(gbm.object$contributions[i,2], 1), "%)", sep = ""))+
            theme_bw()+
            theme(panel.grid.minor = element_line(linetype = "blank"),
                  panel.grid.major = element_line(linetype = "blank"),
                  axis.text.x  = element_text(size=6),
                  axis.title.x  = element_text(size=10),
                  axis.line.y = element_line(size=0.1),
                  axis.line.x=element_line(size=0.1))
          
          if (common.scale==T){
            ggPD[[i]]<-ggPD[[i]]+ylim(c(ymin,ymax))}
        }
        
        if (type.ci=="lines"){
          ggPD[[i]]<- ggplot(fittedFunc[[i]], aes(x=x,y=y))+
            geom_line(color=col.line,size=cex.line)+
            geom_line(data=fittedFunc.lower[[i]],aes(x=x,y=y),size=cex.ci,color=col.ci,linetype=lty.ci)+
            geom_line(data=fittedFunc.upper[[i]],aes(x=x,y=y),size=cex.ci,color=col.ci,linetype=lty.ci)+
            ylab(y.label)+
            xlab(paste(var.name, "  (", round(gbm.object$contributions[i,2], 1), "%)", sep = ""))+
            theme_bw()+
            theme(panel.grid.minor = element_line(linetype = "blank"),
                  panel.grid.major = element_line(linetype = "blank"),
                  axis.title.x  = element_text(size=10),
                  axis.line.y = element_line(size=0.1),
                  axis.line.x=element_line(size=0.1))
          
          if (smooth==T){
            ggPD[[i]]<-ggPD[[i]]+geom_smooth(span=span,size=0.3,color=col.smooth,se=F,linetype=2)
          }
          
          if (rug==T){
            ggPD[[i]]<-ggPD[[i]]+geom_rug(data=fittedVal[[i]],aes(x=x,y=y),sides=rug.pos,position="jitter",color="#EBEBEB")
          }
          
          if (common.scale==T){
            ggPD[[i]]<-ggPD[[i]]+ylim(c(ymin,ymax))
          }
        }
        
        if (type.ci=="ribbon"){
          ggPD[[i]]<- ggplot()+
            geom_ribbon(data=ribbon[[i]],aes(x=x,ymin=ylow,ymax=yup),fill=col.ci,alpha=alpha.ci)+
            geom_line(data=fittedFunc[[i]], aes(x=x,y=y),color=col.line,size=cex.line)+
            ylab(y.label)+
            xlab(paste(var.name, "  (", round(gbm.object$contributions[i,2], 1), "%)", sep = ""))+
            theme_bw()+
            theme(panel.grid.minor = element_line(linetype = "blank"),
                  panel.grid.major = element_line(linetype = "blank"),
                  axis.title.x  = element_text(size=10),
                  axis.line.y = element_line(size=0.1),
                  axis.line.x=element_line(size=0.1))
          
          if (smooth==T){
            ggPD[[i]]<-ggPD[[i]]+geom_smooth(data=fittedFunc[[i]],aes(x=x,y=y),span=span,size=0.3,color=col.smooth,se=F,linetype=2)
          }
          
          if (rug==T){
            ggPD[[i]]<-ggPD[[i]]+geom_rug(data=fittedVal[[i]],aes(x=x,y=y),sides=rug.pos,position="jitter",color="#EBEBEB")
          }
          
          if (common.scale==T){
            ggPD[[i]]<-ggPD[[i]]+ylim(c(ymin,ymax))
          }
        }
      }
      list(ggPD=ggPD)
    }
    
    else{
      
      if (is.character(predictor)){
        predictor<-match(predictor,gbm.object$contributions$var)}
      
      k <- match(gbm.object$contributions$var[predictor], pred.names)
      var.name <- gbm.call$predictor.names[k]
      
      fittedFunc<-data.frame(predictors[predictor],responses[predictor])
      colnames(fittedFunc)<-c("x","y")
      
      fittedFunc.lower<-data.frame(predictors[predictor],responses.lower[predictor])
      colnames(fittedFunc.lower)<-c("x","y")
      
      fittedFunc.upper<-data.frame(predictors[predictor],responses.upper[predictor])
      colnames(fittedFunc.upper)<-c("x","y")
      
      ribbon<-data.frame("x"=fittedFunc.lower$x,"ylow"=fittedFunc.lower$y,"yup"=fittedFunc.upper$y)
      
      fittedVal<-data.frame(gbm.object$fitted,dat[predictor])
      colnames(fittedVal)<-c("y","x")
      
      if (is.factor(fittedFunc$x)) {
        ggPD<- ggplot(fittedFunc, aes(x=x,y=y))+
          geom_boxplot(color=col.line,size=cex.line)+
          geom_boxplot(data=fittedFunc.lower, aes(x=x,y=y),color=col.ci)+
          geom_boxplot(data=fittedFunc.upper, aes(x=x,y=y),color=col.ci)+
          ylab(y.label)+
          xlab(paste(var.name, "  (", round(gbm.object$contributions[predictor,2], 1), "%)", sep = ""))+
          theme_bw()+
          theme(panel.grid.minor = element_line(linetype = "blank"),
                panel.grid.major = element_line(linetype = "blank"),
                axis.text.x  = element_text(size=6),
                axis.title.x  = element_text(size=10),
                axis.line.y = element_line(size=0.1),
                axis.line.x=element_line(size=0.1))
        
        if (common.scale==T){
          ggPD<-ggPD+ylim(c(ymin,ymax))}
      }
      
      if (type.ci=="lines"){
        ggPD<- ggplot(fittedFunc, aes(x=x,y=y))+
          geom_line(color=col.line,size=cex.line)+
          geom_line(data=fittedFunc.lower,aes(x=x,y=y),size=cex.ci,color=col.ci,linetype=lty.ci)+
          geom_line(data=fittedFunc.upper,aes(x=x,y=y),size=cex.ci,color=col.ci,linetype=lty.ci)+
          ylab(y.label)+
          xlab(paste(var.name, "  (", round(gbm.object$contributions[predictor,2], 1), "%)", sep = ""))+
          theme_bw()+
          theme(panel.grid.minor = element_line(linetype = "blank"),
                panel.grid.major = element_line(linetype = "blank"),
                axis.title.x  = element_text(size=10),
                axis.line.y = element_line(size=0.1),
                axis.line.x=element_line(size=0.1))
        
        if (smooth==T){
          ggPD<-ggPD+geom_smooth(span=span,size=0.3,color=col.smooth,se=F,linetype=2)
        }
        
        if (rug==T){
          ggPD<-ggPD+geom_rug(data=fittedVal,aes(x=x,y=y),sides=rug.pos,position="jitter",color="#EBEBEB")
        }
        
        if (common.scale==T){
          ggPD<-ggPD+ylim(c(ymin,ymax))
        }
      }
      
      if (type.ci=="ribbon"){
        ggPD<- ggplot()+
          geom_ribbon(data=ribbon,aes(x=x,ymin=ylow,ymax=yup),fill=col.ci,alpha=alpha.ci)+
          geom_line(data=fittedFunc,aes(x=x,y=y),color=col.line,size=cex.line)+
          ylab(y.label)+
          xlab(paste(var.name, "  (", round(gbm.object$contributions[predictor,2], 1), "%)", sep = ""))+
          theme_bw()+
          theme(panel.grid.minor = element_line(linetype = "blank"),
                panel.grid.major = element_line(linetype = "blank"),
                axis.title.x  = element_text(size=10),
                axis.line.y = element_line(size=0.1),
                axis.line.x=element_line(size=0.1))
        
        if (smooth==T){
          ggPD<-ggPD+geom_smooth(data=fittedFunc,aes(x=x,y=y),span=span,size=0.3,color=col.smooth,se=F,linetype=2)
        }
        
        if (rug==T){
          ggPD<-ggPD+geom_rug(data=fittedVal,aes(x=x,y=y),sides=rug.pos,position="jitter",color="#EBEBEB")
        }
        
        if (common.scale==T){
          ggPD<-ggPD+ylim(c(ymin,ymax))
        }
      }
      list(ggPD=ggPD)
    }
  }
  
  plot<-ggPD_boot.plots(gbm.object)
  
  if(is.null(predictor)){
    do.call(grid.arrange,c(plot$ggPD,list(nrow=nrow,ncol=ncol)))}
  else grid.draw(plot$ggPD)
}
