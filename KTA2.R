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
threshold_col <- args[3]
SNP_col <- args[4]

#app_output_dir <- '/Users/dustin/Desktop/DongWangOutputs'
#truth_file <- '/Users/dustin/Desktop/dongwang_truth.txt'
#threshold_col <- 'p'
#SNP_col <- 'SNP'
					 
app_output_list <- list.files(app_output_dir) # List all files in the app output dir
my_truth <- as.character(read.table(file=truth_file,header=FALSE,stringsAsFactor=FALSE))

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

my_SNP_eval <- function(x) {
	return(
		eval(parse(text=paste('mydata',SNP_col,sep='$')))
		)
}

my_P_eval <- function(x) {
	return(
		eval(parse(text=paste('mydata',threshold_col,sep='$')))
		)
}

in_truth <- function(x) {
	x %in% my_truth
}

locs <- lapply(app_output_list, file_locations)

return_data <- list()
return_data_names <- list()

for (i in 1:length(locs)) {
	mydata <- my_read_table(locs[[i]])
	this_truth <- sapply(my_SNP_eval(locs[[i]]), in_truth)
	return_data <- append(return_data, roc(this_truth ~ my_P_eval(locs[[i]]))$auc)
	return_data_names <- append(return_data_names, filename(locs[[i]]))
}

Final_Data <- as.data.frame(matrix(c(unlist(return_data_names), unlist(return_data)), ncol=2))
write(paste(Final_Data[,1], Final_Data[,2], sep='\t'), file='AUC.txt')