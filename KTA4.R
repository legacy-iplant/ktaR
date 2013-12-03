#########################################################
#####           Known-Truth Analysis App            #####
#####           By Dustin A. Landers                #####
#####           Contact: (770) 289-8830             #####
#####           dustin.landers@gmail.com            #####
#########################################################

options(warn=-1)  

# Dependencies
require(pROC)
require(methods)
require(getopt)

# Inputs
args <- commandArgs(trailingOnly = TRUE)
app_output_dir <- args[1]
truth_file <- args[2]
beta_file <- args[3]
threshold_col <- args[4]
SNP_col <- args[5]
beta_col <- args[6]

# Begin
app_output_list <- list.files(app_output_dir) # List all files in the app output dir
my_truth <- as.character(read.table(file=truth_file,header=FALSE,stringsAsFactor=FALSE))
my_betas <- as.numeric(read.table(file=beta_file,header=FALSE,stringsAsFactor=FALSE))

gini <- function(x, unbiased = TRUE, na.rm = FALSE){
    if (!is.numeric(x)){
        warning("'x' is not numeric; returning NA")
        return(NA)
    }
    if (!na.rm && any(na.ind <- is.na(x)))
        stop("'x' contain NAs")
    if (na.rm)
        x <- x[!na.ind]
    n <- length(x)
    mu <- mean(x)
    N <- if (unbiased) n * (n - 1) else n * n
    ox <- x[order(x)]
    dsum <- drop(crossprod(2 * 1:n - n - 1,  ox))
    return(dsum / (mu * N))
}

file_locations <- function(x) {
	return(
		paste(app_output_dir,x,sep='/')
		)
}

my_read_table <- function(x) {
	return(
		read.table(file=x,header=TRUE,stringsAsFactor=FALSE)
		)
}

my_SNP_eval <- function(x='mydata') {
	return(
		eval(parse(text=paste(x,SNP_col,sep='$')))
		)
}

my_P_eval <- function(x='mydata') {
	return(
		eval(parse(text=paste(x,threshold_col,sep='$')))
		)
}

my_beta_eval <- function(x='mydata') {
	return(
		eval(parse(text=paste(x,beta_col,sep='$')))
		)
}

in_truth <- function(x) {
	x %in% my_truth
}

locs <- lapply(app_output_list, file_locations)

return_gini <- list()
return_auc <- list()
return_names <- list()
return_RMSE <- list()

for (i in 1:length(locs)) {
	mydata <- my_read_table(locs[[i]])
	this_truth <- sapply(my_SNP_eval('mydata'), in_truth)

	Beta <- matrix(0,nrow=nrow(mydata),ncol=1)
	for (j in 1:length(my_truth)) {
		Beta[which(this_truth==TRUE)[which(names(which(this_truth==TRUE))==my_truth[j])]] <- my_betas[j]
	}

	#return_gini <- append(return_gini, gini())
	return_RMSE <- append(return_RMSE, sqrt(mean((my_beta_eval('mydata')-Beta)**2)))
	my_roc <- roc(this_truth ~ my_P_eval('mydata'))
	return_auc <- append(return_auc, my_roc$auc)
	return_names <- append(return_names, paste(locs[[i]]))
	return_gini <- append(return_gini, (gini(my_roc$sens)+1)/2)

}

write(paste(unlist(return_names), unlist(return_auc), unlist(return_RMSE), unlist(return_gini), sep='\t'), file='Results.txt')
# End