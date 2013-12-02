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

args <- commandArgs(trailingOnly = TRUE)
app_output_dir <- args[1]
truth_file <- args[2]
beta_file <- args[3]
threshold_col <- args[4]
SNP_col <- args[5]
beta_col <- args[6]

#app_output_dir <- '/Users/dustin/Desktop/DongWangOutputs'
#truth_file <- '/Users/dustin/Desktop/dongwang_truth.txt'
#beta_file <- '/Users/dustin/Desktop/dongwang_betas.txt'
#threshold_col <- 'p'
#SNP_col <- 'SNP'
#beta_col <- 'predicted'
					 
app_output_list <- list.files(app_output_dir) # List all files in the app output dir
my_truth <- as.character(read.table(file=truth_file,header=FALSE,stringsAsFactor=FALSE))
my_betas <- as.numeric(read.table(file=beta_file,header=FALSE,stringsAsFactor=FALSE))

filename <- function(loc) {
	parts <- unlist(
		strsplit(loc,'/',fixed=TRUE)
		)
	file <- parts[length(parts)]
	parts <- unlist(
		strsplit(file,'.',fixed=TRUE)
		)
	name <- parts[1:(length(parts)-1)]
	if (length(name) > 1) {
		name <- paste(name,collapse='.')
	}
	return(name)
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

	return_RMSE <- append(return_RMSE, sqrt(mean((my_beta_eval('mydata')-Beta)**2)))
	return_auc <- append(return_auc, roc(this_truth ~ my_P_eval('mydata'))$auc)
	return_names <- append(return_names, filename(locs[[i]]))

}

write(paste(unlist(return_names), unlist(return_auc), unlist(return_RMSE), sep='\t'), file='Results.txt')

#which(this_truth==TRUE)[which(names(which(this_truth==TRUE))==my_truth[1])]