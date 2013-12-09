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

#options <- matrix(c('folder','f',1,"character",
#					'truth','t',2,"character",
#					'effect','e',2,"character",
#					'thres','p',2,"character",
#					'snp','s',2,"character",
#					'beta','b',2,"character"),
#		ncol=4,byrow=TRUE)

#ret.opts <- getopt(options,args)

#app_output_dir <- ret.opts$folder
#truth_file <- ret.opts$truth
#beta_file <- ret.opts$effect
#threshold_col <- ret.opts$thres
#SNP_col <- ret.opts$snp
#beta_col <- ret.opts$beta

app_output_dir <- '~/desktop/mytest/test'
truth_file <- '~/desktop/mytest/syntruth.txt'
beta_file <- '~/desktop/mytest/synbetas.txt'
threshold_col <- 'P'
SNP_col <- 'SNP'
beta_col <- 'BETA'

# Begin
app_output_list <- list.files(app_output_dir) # List all files in the app output dir
my_truth <- as.character(read.table(file=truth_file,header=FALSE,stringsAsFactor=FALSE))
my_betas <- as.numeric(read.table(file=beta_file,header=FALSE,stringsAsFactor=FALSE))

auc <- function(x,y) {
 	x1 <- x[y==TRUE]
 	n1 <- length(x1) 
 	x2 <- x[y==FALSE] 
 	n2 <- length(x2)
	r <- rank(c(x1,x2))  
	auc <- (sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n2) 
	return(1-auc)
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

locs <- lapply(app_output_list, file_locations)

return_auc <- list()
return_names <- list()
return_RMSE <- list()
return_MAE <- list()

for (i in 1:length(locs)) {
	mydata <- my_read_table(locs[[i]])
	this_truth <- sapply(my_SNP_eval('mydata'), in_truth)

	Beta <- matrix(0,nrow=nrow(mydata),ncol=1)
	for (j in 1:length(my_truth)) {
		Beta[which(this_truth==TRUE)[which(names(which(this_truth==TRUE))==my_truth[j])]] <- my_betas[j]
	}

	error <- Beta - my_beta_eval()
	return_RMSE <- append(return_RMSE, rmse(error))
	return_MAE <- append(return_MAE, mae(error))
	return_names <- append(return_names, paste(locs[[i]]))
	return_auc <- append(return_auc, auc(my_P_eval('mydata'),this_truth))

}

write(paste(unlist(return_names), unlist(return_auc), unlist(return_RMSE), unlist(return_MAE), sep='\t'), file='Results.txt')
# End