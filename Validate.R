#########################################################
#####           Validate!                           #####
#####           By Dustin A. Landers                #####
#####           Contact: (770) 289-8830             #####
#####           dustin.landers@gmail.com            #####
#########################################################

options(warn=-1)  

# Dependencies
require(methods)
require(getopt)

# Inputs
args <- commandArgs(trailingOnly = TRUE)

options <- matrix(c('folder','f',1,"character",
					'class','c',0,"character",
					'snp','g',0,"character",
					'score','s',0,"character",
					'effect','e',0,"character",
					'beta','b',0,"character",
					'severity','sr',0,"character",
					'severity2','sr2',0,"character",
					'severity3','sr3',0,"character",
					'delim','d',0,"character"),
		ncol=4,byrow=TRUE)

ret.opts <- getopt(options,args)

app_output_dir <- ret.opts$folder
truth_file <- ret.opts$class
SNP_col <- ret.opts$snp
threshold_col <- ret.opts$score

# Can do up to three seperate H statistics using different severity ratios --
# 		This is equivalent with testing under different priors

if (is.null(ret.opts$severity)) {
	sr <- NA
} else {sr <- as.numeric(ret.opts$severity)}

if (is.null(ret.opts$severity2)) {
	sr2 <- NA
	sr2_do <- FALSE
} else {
	sr2 <- as.numeric(ret.opts$severity2)
	sr2_do <- TRUE
}

if (is.null(ret.opts$severity3)) {
	sr3 <- NA
	sr3_do <- FALSE
} else {
	sr3 <- as.numeric(ret.opts$severity3)
	sr3_do <- TRUE
}

if (is.null(ret.opts$delim)) {
	delim <- '\t'
} else {
	delim <- ret.opts$delim
}

do_effect <- TRUE
if (is.null(ret.opts$effect)) 
	do_effect <- FALSE

do_truth <- TRUE

# Begin
app_output_list <- list.files(app_output_dir) # List all files in the app output dir
my_truth <- as.character(read.table(file=truth_file,header=FALSE,stringsAsFactor=FALSE))

if (do_effect)
	my_betas <- as.numeric(read.table(file=beta_file,header=FALSE,stringsAsFactor=FALSE))

# Returns Area Under the ROC Curve
auc <- function(x,y) {
 	x1 <- x[y==TRUE]
 	n1 <- length(x1) 
 	x2 <- x[y==FALSE] 
 	n2 <- length(x2)
	r <- rank(c(x1,x2))  
	auc <- (sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n2) 
	return(1-auc)
}

# Returns list of file locations
file_locations <- function(x) {
	return(
		paste(app_output_dir,x,sep='/')
		)
}

# Loads in an application output
my_read_table <- function(x) {
	return(
		read.table(file=x,header=TRUE,stringsAsFactor=FALSE,sep=delim)
		)
}

# Returns the SNP vector
my_SNP_eval <- function(x='mydata') {
	return(
		eval(parse(text=paste(x,SNP_col,sep='$')))
		)
}

# Returns the score vector
my_P_eval <- function(x='mydata') {
	return(
		eval(parse(text=paste(x,threshold_col,sep='$')))
		)
}

# Returns the beta (or estimated effect size) vector
my_beta_eval <- function(x='mydata') {
	return(
		eval(parse(text=paste(x,beta_col,sep='$')))
		)
}

# Returns a vector of 0's and 1's (negatives and positives)
in_truth <- function(x) {
	ifelse(x %in% my_truth, 1, 0)
}

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
    sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
    mean(abs(error))
}

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
relabel <-	function (labels) 
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
	        message("Class labels have been switched from (", paste(input.labels[1], 
	            input.labels[2], sep = ","), ") to (", paste("1", 
	            "0", sep = ","), ")")
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
HMeasure <-	function (true.class, scores, severity.ratio = NA, threshold = 0.5, 
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

# Functions for returning folders
unlist_vector_eval <- function(x) {
	return(
		eval(parse(text=paste('unlist(',x,')')))
		)
}

vector_eval <- function(x) {
	return(
		eval(parse(text=paste(x)))
		)
}

# Function for returning column names in final output
make_col_name <- function(x) {
	return(
		unlist(strsplit(x,'_',fixed=TRUE))[2]
		)
}

# Creating locations to be used in for loop of classification and error estimates
locs <- lapply(app_output_list, file_locations)

# Create list for all statistics/measures to be returned
return_AUC <- list()
return_Names <- list()
return_RMSE <- list()
return_MAE <- list()
return_H <- list()
return_Gini <- list()
return_AUCH <- list()
return_KS <- list()
return_H2 <- list()
return_H3 <- list()

# Begin for-loop through every simulated output to create descriptive statistics --
# 		of classification performance

for (i in 1:length(locs)) {

	mydata <- my_read_table(locs[[i]])
	this_truth <- sapply(my_SNP_eval('mydata'), in_truth)

	if (do_effect) {
		Beta <- matrix(0,nrow=nrow(mydata),ncol=1)
		for (j in 1:length(my_truth)) {
			Beta[which(this_truth==TRUE)[which(names(which(this_truth==TRUE))==my_truth[j])]] <- my_betas[j]
		}
		error <- Beta - my_beta_eval()
		return_RMSE <- append(return_RMSE, rmse(error))
		return_MAE <- append(return_MAE, mae(error))
	}

	if (do_truth) {
		myh <- HMeasure(this_truth, my_P_eval('mydata'), severity.ratio=sr)
			return_AUC <- append(return_AUC, myh$metrics[3])
			return_H <- append(return_H, myh$metrics[1])
			return_Gini <- append(return_Gini, myh$metrics[2])
			return_AUCH <- append(return_AUCH, myh$metrics[4])
			return_KS <- append(return_KS, myh$metrics[5])
		if (sr2_do==TRUE) {
			myh <- HMeasure(this_truth, my_P_eval('mydata'), severity.ratio=sr2)
			return_H2 <- append(return_H2, myh$metrics[1])
		}
		if (sr3_do==TRUE) {
			myh <- HMeasure(this_truth, my_P_eval('mydata'), severity.ratio=sr3)
			return_H3 <- append(return_H3, myh$metrics[1])
		}
	}

	return_Names <- append(return_Names, app_output_list[i])

}

use_these <- list()
possibles <- c('return_Names','return_AUC','return_RMSE','return_MAE','return_H','return_H2','return_H3','return_Gini','return_AUCH','return_KS')
for (i in 1:length(possibles)) {
	if (length(vector_eval(possibles[i])) > 0) {
		use_these <- append(use_these, possibles[i])
	}
}

to_print <- data.frame(sapply(use_these, unlist_vector_eval))
names(to_print) <- sapply(use_these, make_col_name)
write.table(x=to_print,file='Results.txt',quote=FALSE,row.names=FALSE,sep='\t')

# End