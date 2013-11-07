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

app_output_dir <- 'C:\\users\\stapletonlab\\documents\\github\\ktar\\outputplink'
truth_file <- 'C:\\users\\stapletonlab\\documents\\github\\ktar\\truth\\plinkstd10.txt'
threshold_col <- 'P'
SNP_col <- 'SNP'
					 
# Get list of files for app outputs and 
# known-truth text files
app_output_list <- list.files(app_output_dir) # List all files in the app output dir
my_truth <- as.character(read.table(file=truth_file,header=FALSE,stringsAsFactor=FALSE))

# Make function for filenames for app outputs
filename <- function(loc) {
	parts <- unlist(strsplit(loc,'\\',fixed=TRUE))
	file <- parts[length(parts)]
	parts <- unlist(strsplit(file,'.',fixed=TRUE))
	name <- parts[1:(length(parts)-1)]
	if (length(name) > 1) {
		name <- paste(name,collapse='.')
	}
	return(name)
}

# Load files as lists
file_locations <- function(x) {
	return(paste(app_output_dir,x,sep='\\'))
}

my_read_table <- function(x) {
	return(read.table(file=x,header=TRUE,stringsAsFactor=FALSE))
}

locs <- lapply(app_output_list,file_locations)

# Generate AUC
my_SNP_eval <- function(x) {
	return(eval(parse(text=paste('mydata',SNP_col,sep='$'))))
}

my_P_eval <- function(x) {
	return(eval(parse(text=paste('mydata',threshold_col,sep='$'))))
}

in_truth <- function(x) {
	x %in% my_truth
}

do_analyze <- function(x) {
	mydata <- my_read_table(x)
	truth <- sapply(my_SNP_eval(mydata),in_truth)
	print(roc(truth~my_P_eval(mydata))$auc)
	rm(mydata)
}

print(lapply(locs,do_analyze))