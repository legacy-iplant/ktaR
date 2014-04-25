#########################################################
#####           Validate!                           #####
#####           By Dustin A. Landers                #####
#####           Contact: (770) 289-8830             #####
#####           dustin.landers@gmail.com            #####
#########################################################

###### H measure stuff below!!! Not my code until you see the the indicator ############

# Returns confusion matrix
misclassCounts <- function (predicted.class, true.class) 
{
  
  true.class <- as.array(true.class)
  predicted.class <- as.array(predicted.class)
  TP <- sum(predicted.class == 1 & true.class == 1)
  FP <- sum(predicted.class == 1 & true.class == 0)
  TN <- sum(predicted.class == 0 & true.class == 0)
  FN <- sum(predicted.class == 0 & true.class == 1)
  conf.matrix <- data.frame(pred.1 = c(TP, FP), pred.0 = c(FN, 
                                                           TN))
  row.names(conf.matrix) <- c("actual.1", "actual.0")
  ER <- (FP + FN)/(TP + FP + TN + FN)
  Sens <- TP/(TP + FN)
  Spec <- TN/(TN + FP)
  Precision <- TP/(TP + FP)
  Recall <- Sens
  TPR <- Recall
  FPR <- 1 - Spec
  F <- 2/(1/Precision + 1/Sens)
  Youden <- Sens + Spec - 1
  metrics <- data.frame(ER = ER, Sens = Sens, Spec = Spec, 
                        Precision = Precision, Recall = Recall, TPR = TPR, FPR = FPR, 
                        F = F, Youden = Youden)
  return(list(conf.matrix = conf.matrix, metrics = metrics))
}

# Relabels TRUE/FALSES to 0's and 1's
relabel <-  function (labels) 
{
  if (length(levels(as.factor(labels))) == 1) {
    stop("Only one class is present in the dataset. Need both classes to be represented.")
  }
  
  if (length(levels(as.factor(labels))) > 2) {
    stop("More than two classes present, but code can only handle binary classification.")
  }
  
  labels <- as.factor(as.character(labels))
  input.labels <- levels(labels)
  cond.temp <- (identical(input.labels, c("case", "non-case")) | 
                  identical(input.labels, c("Case", "Non-case")) | identical(input.labels, 
                                                                             c("case", "noncase")) | identical(input.labels, c("Case", 
                                                                                                                               "Non-case")))
  if (cond.temp) {
    levels(labels) <- c("1", "0")
    message("\nClass labels have been switched from (", paste(input.labels[1], 
                                                            input.labels[2], sep = ","), ") to (", paste("1", 
                                                                                                         "0", sep = ","), ")\n")
    labels <- as.factor(labels)
    labels <- 2 - as.numeric(labels)
  }
  
  else {
    levels(labels) <- c("0", "1")
    if (!(identical(input.labels, c("0", "1")))) {
      message("Class labels have been switched from (", 
              paste(input.labels[1], input.labels[2], sep = ","), 
              ") to (", paste("0", "1", sep = ","), ")")
    }
    
    labels <- as.factor(labels)
    labels <- as.numeric(labels) - 1
  }
  
  return(labels)
}
# Returns hmeasure and a number of other statistics
HMeasure <-  function (true.class, scores, severity.ratio = NA, threshold = 0.5, 
                       level = 0.95) 
{
  if (is.matrix(true.class) || is.data.frame(true.class)) {
    stop("True class should be a vector, not a matrix / data frame.  Consider the order of the arguments.")
  }
  
  if (any(is.na(true.class))) {
    stop("Missing values in class labels are not allowed.")
  }
  
  true.class <- relabel(true.class)
  rownames(scores) <- NULL
  rownames(true.class) <- NULL
  if (is.vector(scores)) {
    scores <- as.data.frame(scores)
  }
  
  if (is.matrix(scores)) {
    n <- dim(scores)[1]
    k <- dim(scores)[2]
    if (n < k) {
      warning(gettextf("Consider transposing score matrix: number of classifiers (columns) = %d exceeds number %d of datapoints (rows)", 
                       k, n), domain = NA)
    }
    
    scores <- as.data.frame(scores)
  }
  
  if (dim(scores)[1] != length(true.class)) {
    stop("Label vector provided has different length than respective classifier scores")
  }
  
  if (any(is.na(scores))) {
    warning("Missing entries detected in matrix of scores. Respective entries will be disregarded")
  }
  
  complete.rows <- complete.cases(scores)
  scores <- subset(scores, subset = complete.rows)
  true.class <- subset(true.class, subset = complete.rows)
  rownames(scores) <- NULL
  rownames(true.class) <- NULL
  n <- dim(scores)[1]
  k <- dim(scores)[2]
  if (length(threshold) == 1) {
    threshold <- rep(threshold, k)
  }
  
  else {
    if (length(threshold) < k) {
      warning("Threshold must either be a single value, or a vector of length equal to the number of classifiers employed. The default value of 0.5 will be used.")
    }
    
  }
  
  HMeasure.single <- function(y, s, classifier.name = NULL, 
                              severity.ratio = severity.ratio, threshold = threshold, 
                              level = level) {
    n <- length(s)
    n1 <- sum(y)
    n0 <- n - n1
    pi0 <- n0/n
    pi1 <- n1/n
    if (is.na(severity.ratio)) {
      severity.ratio <- pi1/pi0
    }
    
    zord <- order(s)
    sc <- s[zord]
    
    Get.Score.Distributions <- function(y, s, n1, n0) {
      s1 <- unname(tapply(y, s, sum))/n1
      s1 <- c(0, s1, 1 - sum(s1))
      s0 <- unname(tapply(1 - y, s, sum))/n0
      s0 <- c(0, s0, 1 - sum(s0))
      S <- length(s1)
      F1 <- cumsum(s1)
      F0 <- cumsum(s0)
      return(list(F1 = F1, F0 = F0, s1 = s1, s0 = s0, S = S))
    }
    
    out.scores <- Get.Score.Distributions(y = y, s = s, n1 = n1, 
                                          n0 = n0)
    AUC <- 1 - sum(out.scores$s0 * (out.scores$F1 - 0.5 * 
                                      out.scores$s1))
    switched <- FALSE
    the.criterion <- AUC < 0.5
    if (the.criterion) {
      switched <- TRUE
      s <- 1 - s
      out.scores <- Get.Score.Distributions(y, s, n1, n0)
      if (is.null(classifier.name)) {
        warning("ROC curve mostly lying under the diagonal. Switching scores.", 
                domain = NA)
      }
      
      else {
        warning(gettextf("ROC curve of %s mostly lying under the diagonal. Switching scores.", 
                         classifier.name), domain = NA)
      }
      
    }
    
    F1 <- out.scores$F1
    F0 <- out.scores$F0
    s0 <- out.scores$s0
    s1 <- out.scores$s1
    S <- out.scores$S
    misclass.out <- misclassCounts(as.numeric(s > threshold), 
                                   true.class)
    misclass.metrics <- misclass.out$metrics
    temp <- misclass.out$conf.matrix
    misclass.conf <- data.frame(TP = temp[1, 1], FP = temp[2, 
                                                           1], TN = temp[2, 2], FN = temp[1, 2])
    AUC <- 1 - sum(s0 * (F1 - 0.5 * s1))
    Gini <- 2 * AUC - 1
    KS <- max(abs(F0 - F1))
    cost.parameter <- severity.ratio/(1 + severity.ratio)
    MER <- min(pi0 * (1 - F0) + pi1 * F1)
    MWL <- 2 * min(cost.parameter * pi0 * (1 - F0) + (1 - 
                                                        cost.parameter) * pi1 * F1)
    
    Look.Up.AUC <- function(xcurve, ycurve, x = 0) {
      result <- NA
      if (all(diff(xcurve) >= 0)) {
        ind <- which(xcurve - x > 0)[1]
        x1 <- xcurve[ind - 1]
        x2 <- xcurve[ind]
        y1 <- ycurve[ind - 1]
        y2 <- ycurve[ind]
        if (x2 - x1 > 0) {
          pos <- (x2 - x)/(x2 - x1)
          result <- (1 - pos) * y1 + pos * y2
        }
        
        else {
          result <- y2
        }
        
      }
      
      return(result)
    }
    
    SensFixed <- matrix(NA, 1, length(level))
    SpecFixed <- matrix(NA, 1, length(level))
    temp <- array(NA, length(level))
    for (l in 1:length(level)) {
      SensFixed[l] <- c(Look.Up.AUC(F0, 1 - F1, x = level[l]))
      temp[l] <- paste("Sens.Spec", floor(level[l] * 100), 
                       sep = "")
    }
    
    SensFixed <- as.data.frame(SensFixed)
    colnames(SensFixed) <- temp
    for (l in 1:length(level)) {
      SpecFixed[l] <- Look.Up.AUC(F1, F0, x = 1 - level[l])
      temp[l] <- paste("Spec.Sens", floor(level[l] * 100), 
                       sep = "")
    }
    
    SpecFixed <- as.data.frame(SpecFixed)
    colnames(SpecFixed) <- temp
    chull.points <- chull(1 - F0, pmax(1 - F1, 1 - F0))
    G0 <- 1 - F0[chull.points]
    G1 <- 1 - F1[chull.points]
    hc <- length(chull.points)
    sG0 <- c(0, G0[c(2:length(G0))] - G0[c(1:(length(G0) - 
                                                1))])
    sG1 <- c(0, G1[c(2:length(G1))] - G1[c(1:(length(G1) - 
                                                1))])
    AUCH <- sum(sG0 * (G1 - 0.5 * sG1))
    s.class0 <- sort(s[y == 0])
    s.class1 <- sort(s[y == 1])
    cost <- c(1:(hc + 1))
    b0 <- c(1:hc + 1)
    b1 <- c(1:hc + 1)
    if (severity.ratio > 0) {
      shape1 <- 2
      shape2 <- 1 + (shape1 - 1) * 1/severity.ratio
    }
    
    if (severity.ratio < 0) {
      shape1 <- pi1 + 1
      shape2 <- pi0 + 1
    }
    
    cost[1] <- 0
    cost[hc + 1] <- 1
    b00 <- beta(shape1, shape2)
    b10 <- beta(1 + shape1, shape2)
    b01 <- beta(shape1, 1 + shape2)
    b0[1] <- pbeta(cost[1], shape1 = (1 + shape1), shape2 = shape2) * 
      b10/b00
    b1[1] <- pbeta(cost[1], shape1 = shape1, shape2 = (1 + 
                                                         shape2)) * b01/b00
    b0[hc + 1] <- pbeta(cost[hc + 1], shape1 = (1 + shape1), 
                        shape2 = shape2) * b10/b00
    b1[hc + 1] <- pbeta(cost[hc + 1], shape1 = shape1, shape2 = (1 + 
                                                                   shape2)) * b01/b00
    for (i in 2:hc) {
      cost[i] <- pi1 * (G1[i] - G1[i - 1])/(pi0 * (G0[i] - 
                                                     G0[i - 1]) + pi1 * (G1[i] - G1[i - 1]))
      b0[i] <- pbeta(cost[i], shape1 = (1 + shape1), shape2 = shape2) * 
        b10/b00
      b1[i] <- pbeta(cost[i], shape1 = shape1, shape2 = (1 + 
                                                           shape2)) * b01/b00
    }
    
    LHshape1 <- 0
    for (i in 1:hc) {
      LHshape1 <- LHshape1 + pi0 * (1 - G0[i]) * (b0[(i + 
                                                        1)] - b0[i]) + pi1 * G1[i] * (b1[(i + 1)] - b1[i])
    }
    
    B0 <- pbeta(pi1, shape1 = (1 + shape1), shape2 = shape2) * 
      b10/b00
    B1 <- pbeta(1, shape1 = shape1, shape2 = (1 + shape2)) * 
      b01/b00 - pbeta(pi1, shape1 = shape1, shape2 = (1 + 
                                                        shape2)) * b01/b00
    H <- 1 - LHshape1/(pi0 * B0 + pi1 * B1)
    data <- list(F0 = F0, F1 = F1, G0 = G0, G1 = G1, cost = cost, 
                 pi1 = pi1, pi0 = pi0, n0 = n0, n1 = n1, n = n, hc = hc, 
                 s.class0 = s.class0, s.class1 = s.class1, severity.ratio = severity.ratio)
    metrics <- data.frame(H = H, Gini = Gini, AUC = AUC, 
                          AUCH = AUCH, KS = KS, MER = MER, MWL = MWL)
    metrics <- cbind(metrics, SpecFixed, SensFixed)
    metrics <- cbind(metrics, misclass.metrics, misclass.conf)
    return(list(data = data, metrics = metrics))
  }
  
  data <- list()
  for (count in 1:k) {
    name.now <- colnames(scores)[count]
    s <- scores[, count]
    threshold.now <- threshold[count]
    output <- HMeasure.single(y = true.class, s = s, classifier.name = name.now, 
                              severity.ratio = severity.ratio, threshold = threshold.now, 
                              level = level)
    if (count == 1) {
      metrics <- output$metrics
    }
    
    if (count > 1) {
      metrics <- rbind(metrics, output$metrics)
    }
    
    data[[count]] <- output$data
  }
  
  rownames(metrics) <- colnames(scores)
  names(data) <- colnames(data)
  hmeasure <- list(metrics = metrics)
  attr(hmeasure, "data") <- data
  class(hmeasure) <- "hmeasure"
  return(hmeasure)
}

######################## Begin my code ###############################

options(warn=-1)  

# Dependencies
require(methods)
require(getopt)

# Inputs
args <- commandArgs(trailingOnly = TRUE)

options <- matrix(c("folder","f",1,"character",
                    "class","c",0,"character",
                    "snp","g",0,"character",
                    "score","s",0,"character",
                    "effect","e",0,"character",
                    "beta","b",0,"character",
                    "severity","sr",0,"character",
                    "filename","fn",0,"character",
                    "threshold","th",0,"character",
                    "seper","se",0,"character",
                    "kttype","kt",0,"character",
                    "kttypeseper","kts",0,"character"),
                  ncol=4,byrow=TRUE)

ret.opts <- getopt(options,args)

app.output.dir <- ret.opts$folder
truth.file <- ret.opts$class
SNP.col <- ret.opts$snp
threshold.col <- ret.opts$score
file.name <- ret.opts$filename

## Make default delimination tab-delimited
if (is.null(ret.opts$seper)) {
  seper <- "whitespace"
} else {
  seper <- ret.opts$seper
}

## Make default kt input type as OTE
if (is.null(ret.opts$kttype)) {
  kttype <- "OTE"
} else {
  kttype <- ret.opts$kttype
}

## Make default kt type seper whitespace-delimited
if (is.null(ret.opts$kttypeseper)) {
  kttypeseper <- "whitespace"
} else {
  kttypeseper <- ret.opts$kttypeseper
}

#app.output.dir <- "~/Desktop/results"
#truth.file <- "/users/dustin/documents/ktar/truth/PlinkStd10.txt"
#SNP.col <- "SNP"
#threshold.col <- "P"
#file.name <- "heythere.txt"
#severity <- 1
#threshold <- 0.05

if (is.null(ret.opts$effect)) {
  do.effect <- FALSE
} else {
  do.effect <- TRUE
  beta.file <- ret.opts$effect
  #  beta.file <- "/users/dustin/documents/ktar/betas.txt"
  beta.col <- ret.opts$beta
  #  beta.col <- "BETA"
}

do.truth <- TRUE

# Begin
app.output.list <- list.files(app.output.dir) # List all files in the app output dir

## KT File Type OTE, OTE2, and other
if (kttypeseper=="whitespace") {
    if (kttype=="OTE1") {
      my.truth <- as.character(
        read.table(file=truth.file,header=FALSE,stringsAsFactor=FALSE)
        )
      
      if (do.effect) {
        my.betas <- as.numeric(
          read.table(file=beta.file,header=FALSE,stringsAsFactor=FALSE)
          )
      }
      
    } else if (kttype=="OTE2") {
      temp.file <- read.table(file=truth.file,header=FALSE,stringsAsFactor=FALSE)
      
      if (is.character(temp.file[1,])) {
        
        temp.file <- t(temp.file)
        
      }
      
      my.truth <- temp.file[,1]
      my.betas <- temp.file[,2]
      
    } else if (kttype=="FGS") {
      temp.file <- read.table(file=truth.file,header=FALSE,stringsAsFactor=FALSE)
      
      if (is.character(temp.file[1,])) {
        
        temp.file <- t(temp.file)
        
      }
      
      my.truth <- temp.file[,1]
      my.betas <- temp.file[,2]
      
      my.truth <- my.truth[which(my.betas!=0)]
      my.betas <- my.betas[which(my.betas!=0)]
      
    }
    
}

if (kttypeseper=="comma") {
  if (kttype=="OTE1") {
    my.truth <- as.character(
      read.csv(file=truth.file,header=FALSE,stringsAsFactor=FALSE)
    )
    
    if (do.effect) {
      my.betas <- as.numeric(
        read.csv(file=beta.file,header=FALSE,stringsAsFactor=FALSE)
      )
    }
    
  } else if (kttype=="OTE2") {
    temp.file <- read.csv(file=truth.file,header=FALSE,stringsAsFactor=FALSE)
    
    if (is.character(temp.file[1,])) {
      
      temp.file <- t(temp.file)
      
    }
    
    my.truth <- temp.file[,1]
    my.betas <- temp.file[,2]
    
  } else if (kttype=="FGS") {
    temp.file <- read.csv(file=truth.file,header=FALSE,stringsAsFactor=FALSE)
    
    if (is.character(temp.file[1,])) {
      
      temp.file <- t(temp.file)
      
    }
    
    my.truth <- temp.file[,1]
    my.betas <- temp.file[,2]
    
    my.truth <- my.truth[which(my.betas!=0)]
    my.betas <- my.betas[which(my.betas!=0)]
    
  }
  
}

# Returns list of file locations
file.locations <- function(x) {
  return(
    paste(app.output.dir,x,sep="/")
  )
}

# Loads in an application output
if (seper=="whitespace") {
  
      my.read.table <- function(x) {
        return(
          read.table(file=x,header=TRUE,stringsAsFactor=FALSE)
        )
      }
      
    } else if (seper=="comma") {
    
      my.read.table <- function(x) {
        return(
          read.csv(file=x,header=TRUE,stringsAsFactor=FALSE)
        )
      }
      
    } 


# Returns the SNP vector
my.SNP.eval <- function(x="mydata") {
  return(
    eval(parse(text=paste(x,SNP.col,sep="$")))
  )
}

# Returns the score vector
my.P.eval <- function(x="mydata") {
  return(
    eval(parse(text=paste(x,threshold.col,sep="$")))
  )
}

# Returns the beta (or estimated effect size) vector
my.beta.eval <- function(x="mydata") {
  return(
    eval(parse(text=paste(x,beta.col,sep="$")))
  )
}

# Returns a vector of 0"s and 1"s (negatives and positives)
in.truth <- function(x) {
  ifelse(x %in% my.truth, TRUE, FALSE)
}

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2, na.rm=TRUE))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error), na.rm=TRUE)
}

# Functions for returning folders
unlist.vector.eval <- function(x) {
  return(
    eval(parse(text=paste("unlist(",x,")")))
  )
}

vector.eval <- function(x) {
  return(
    eval(parse(text=paste(x)))
  )
}

# Function for returning column names in final output
make.col.name <- function(x) {
  return(
    unlist(strsplit(x,".",fixed=TRUE))[2]
  )
}

# Creating locations to be used in for loop of classification and error estimates
locs <- lapply(app.output.list, file.locations)

# Returns Area Under the ROC Curve
auc.dustin <- function(x,y) {
  x1 <- x[y==TRUE]
  n1 <- length(x1) 
  x2 <- x[y==FALSE] 
  n2 <- length(x2)
  r <- rank(c(x1,x2))  
  auc <- (sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n2) 
  return(1-auc)
}

return.AUC <- list()
return.Names <- list()
return.RMSE <- list()
return.MAE <- list()
return.r <- list()
return.R2 <- list()
return.H <- list()
return.Gini <- list()
return.KS <- list() 
return.Sens <- list() 
return.Spec <- list()
return.Precision <- list()
return.TPR <- list() 
return.FPR <- list() 
return.Youden <- list() 
return.TP <- list()
return.FP <- list() 
return.TN <- list() 
return.FN <- list()

# Begin for-loop through every simulated output to create descriptive statistics --
#     of classification performance

for (i in 1:length(locs)) {
  
  cat("\nLoop",i,"for evaluation metrics on",locs[[i]],"\n")
  
  mydata <- my.read.table(locs[[i]])
  this.truth <- sapply(my.SNP.eval("mydata"), in.truth)
  
  if (do.effect) {
    cat("\nPerforming effect size evaluation...\n")
    Beta <- matrix(0,nrow=nrow(mydata),ncol=1)
    for (j in 1:length(my.truth)) {
      Beta[which(this.truth==TRUE)[which(names(which(this.truth==TRUE))==my.truth[j])]] <- my.betas[j]
    }
    error <- Beta - my.beta.eval()
    return.RMSE <- append(return.RMSE, rmse(error))
    return.MAE <- append(return.MAE, mae(error))
    return.r <- append(return.r, cor(Beta, my.beta.eval()))
    return.R2 <- append(return.R2, cor(Beta, my.beta.eval())**2)
  }
  
  if (do.truth) {
    
    return.AUC <- append(return.AUC, auc.dustin(my.P.eval("mydata"), this.truth))
    
    metrics <- HMeasure(this.truth, my.P.eval("mydata"))$metrics
    return.H <- append(return.H, metrics["H"])
    return.Gini <- append(return.Gini, metrics["Gini"])
    return.KS <- append(return.KS, metrics["KS"])
    return.Sens <- append(return.Sens, metrics["Sens"])
    return.Spec <- append(return.Spec, metrics["Spec"])
    return.Precision <- append(return.Precision, metrics["Precision"])
    return.TPR <- append(return.TPR, metrics["TPR"])
    return.FPR <- append(return.FPR, metrics["FPR"])
    return.Youden <- append(return.Youden, metrics["Youden"])
    return.TP <- append(return.TP, metrics["TP"])
    return.FP <- append(return.FP, metrics["FP"])
    return.TN <- append(return.TN, metrics["TN"])
    return.FN <- append(return.FN, metrics["FN"])
  }
  
  return.Names <- append(return.Names, app.output.list[i])
  
}

use.these <- list()
possibles <- c("return.Names","return.AUC","return.RMSE","return.MAE", "return.r", "return.R2",
               "return.H", "return.Gini", "return.KS", "return.Sens", "return.Spec", 
               "return.Precision", "return.TPR", "return.FPR", "return.Youden", "return.TP",
               "return.FP", "return.TN", "return.FN")
for (i in 1:length(possibles)) {
  if (length(vector.eval(possibles[i])) > 0) {
    use.these <- append(use.these, possibles[i])
  }
}

if (length(app.output.list) > 1) {
  to.print <- data.frame(sapply(use.these, unlist.vector.eval))
  names(to.print) <- sapply(use.these, make.col.name)
  write.table(x=to.print,file=file.name,quote=FALSE,row.names=FALSE,sep="\t")
}

if (length(app.output.list)==1) {
  to.print <- sapply(use.these, unlist.vector.eval)
  names <- sapply(use.these, make.col.name)
  write.table(x=t(data.frame(names, to.print)),file=file.name,quote=FALSE,
              row.names=FALSE,col.names=FALSE,sep="\t")
}

# End