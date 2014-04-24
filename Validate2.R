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
#args <- commandArgs(trailingOnly = TRUE)

#options <- matrix(c("folder","f",1,"character",
#                    "class","c",0,"character",
#                    "snp","g",0,"character",
#                    "score","s",0,"character",
#                    "effect","e",0,"character",
#                    "beta","b",0,"character",
#                    "severity","sr",0,"character"),
#                  ncol=4,byrow=TRUE)

#ret.opts <- getopt(options,args)

#app_output_dir <- ret.opts$folder
#truth_file <- ret.opts$class
#SNP_col <- ret.opts$snp
#threshold_col <- ret.opts$score

app_output_dir <- "~/Desktop/results"
truth_file <- "/users/dustin/documents/ktar/truth/PlinkStd10.txt"
SNP_col <- "SNP"
threshold_col <- "P"
file_name <- "hey_there.txt"

#if (is.null(ret.opts$effect)) {
#  do_effect <- FALSE
#} else {
  do_effect <- TRUE
#  beta_file <- ret.opts$effect
  beta_file <- "/users/dustin/documents/ktar/betas.txt"
#  beta_col <- ret.opts$beta
  beta_col <- "BETA"
#}

do_truth <- TRUE

# Begin
app_output_list <- list.files(app_output_dir) # List all files in the app output dir
my_truth <- as.character(read.table(file=truth_file,header=FALSE,stringsAsFactor=FALSE))

if (do_effect)
  my_betas <- as.numeric(read.table(file=beta_file,header=FALSE,stringsAsFactor=FALSE))

# Returns list of file locations
file_locations <- function(x) {
  return(
    paste(app_output_dir,x,sep="/")
  )
}

# Loads in an application output
my_read_table <- function(x) {
  return(
    read.table(file=x,header=TRUE,stringsAsFactor=FALSE)
  )
}

# Returns the SNP vector
my_SNP_eval <- function(x="mydata") {
  return(
    eval(parse(text=paste(x,SNP_col,sep="$")))
  )
}

# Returns the score vector
my_P_eval <- function(x="mydata") {
  return(
    eval(parse(text=paste(x,threshold_col,sep="$")))
  )
}

# Returns the beta (or estimated effect size) vector
my_beta_eval <- function(x="mydata") {
  return(
    eval(parse(text=paste(x,beta_col,sep="$")))
  )
}

# Returns a vector of 0"s and 1"s (negatives and positives)
in_truth <- function(x) {
  ifelse(x %in% my_truth, TRUE, FALSE)
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
unlist_vector_eval <- function(x) {
  return(
    eval(parse(text=paste("unlist(",x,")")))
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
    unlist(strsplit(x,"_",fixed=TRUE))[2]
  )
}

# Creating locations to be used in for loop of classification and error estimates
locs <- lapply(app_output_list, file_locations)

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

return_AUC <- list()
return_Names <- list()
return_RMSE <- list()
return_MAE <- list()

# Begin for-loop through every simulated output to create descriptive statistics --
#   	of classification performance

for (i in 1:length(locs)) {
  
  cat("\nLoop",i,"for evaluation metrics on",locs[[i]])
  
  mydata <- my_read_table(locs[[i]])
  this_truth <- sapply(my_SNP_eval("mydata"), in_truth)
  
  if (do_effect) {
    cat("\nPerforming effect size evaluation...")
    Beta <- matrix(0,nrow=nrow(mydata),ncol=1)
    for (j in 1:length(my_truth)) {
      Beta[which(this_truth==TRUE)[which(names(which(this_truth==TRUE))==my_truth[j])]] <- my_betas[j]
    }
    error <- Beta - my_beta_eval()
    return_RMSE <- append(return_RMSE, rmse(error))
    return_MAE <- append(return_MAE, mae(error))
  }
  
  if (do_truth) {
    return_AUC <- append(return_AUC, auc.dustin(my_P_eval("mydata"), this_truth))
  }
  
  return_Names <- append(return_Names, app_output_list[i])
  
}

use_these <- list()
possibles <- c("return_Names","return_AUC","return_RMSE","return_MAE")
for (i in 1:length(possibles)) {
  if (length(vector_eval(possibles[i])) > 0) {
    use_these <- append(use_these, possibles[i])
  }
}

if (length(app_output_list) > 1) {
  to_print <- data.frame(sapply(use_these, unlist_vector_eval))
  names(to_print) <- sapply(use_these, make_col_name)
  write.table(x=to_print,file=file_name,quote=FALSE,row.names=FALSE,sep="\t")
}

if (length(app_output_list)==1) {
  to_print <- sapply(use_these, unlist_vector_eval)
  names <- sapply(use_these, make_col_name)
  write.table(x=t(data.frame(names, to_print)),file=file_name,quote=FALSE,
              row.names=FALSE,col.names=FALSE,sep="\t")
}
  
# End