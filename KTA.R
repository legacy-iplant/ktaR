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

# Command-line arguments
args <- commandArgs(trailingOnly = TRUE)
app_output_dir <- args[1] # First argument is the directory of app outputs (string)
truth_file_or_dir <- args[2] # Second argument is 1 if there is only one truth file
							 # (numeric/integer)

if (truth_file_or_dir == 0) {
	truth_dir <- args[3] # The third argument is either directory or file of 
						 # known-truth SNPs (string); if this is a directory, they
						 # must have the same names as their output counterparts
}

if (truth_file_or_dir == 1) {
	truth_file <- args[3]
}

threshold_col <- args[4] # Fourth argument is a string name of the threshold column
						 # name, for example if it's the P-value you want to vary,
						 # and the name of the column is the string here (string)
SNP_col <- args[5] 		 # Fifth argument is a string of the SNP column (string)
include_best <- args[6]  # If 1, includes output with recommended threshold; else 0
include_all <- args[7]   # If 1, includes file for each output with sens, spec, and 
						 #    threshold; else 0
						 
# Get list of files for app outputs and 
# known-truth text files
app_output_list <- list.files(app_output_dir) # List all files in the app output dir

if (truth_file_or_dir == 0) {
	truth_list <- list.files(truth_dir)
}

# Get filenames for app outputs and truth strings
filenames <- matrix(nrow=length(app_output_list),ncol=1)
for (output in 1:length(app_output_list)) {

	file <- app_output_list[output]
	parts <- unlist(strsplit(file,'.',fixed=TRUE))
	name <- parts[1:(length(parts)-1)]
	
	if (length(name) > 1) {
		name <- paste(name,collapse='.')
	}

	filenames[output] <- name
}

# Generate ROC curves
AUC <- matrix(nrow=length(filenames),ncol=6)
AUC[,1] <- filenames

for (file in 1:length(filenames)) {

	for (output in 1:length(app_output_list)) {

		if (filenames[file] %in% unlist(strsplit(app_output_list[output],'.',fixed=TRUE))) {
			my_output <- read.table(file=paste(app_output_dir,app_output_list[output],sep='/'),
				header=TRUE,stringsAsFactor=FALSE)
		}

		if (truth_file_or_dir == 0) {

			for (truth in 1:length(truth_list)) {

				if (filenames[file] %in% unlist(strsplit(truth_list[truth],'.',fixed=TRUE))) {
					my_truth <- as.character(read.table(file=paste(truth_dir,truth_list[truth],sep='/'),
						header=FALSE,stringsAsFactor=FALSE))
				}
			}
		}

		if (truth_file_or_dir == 1) {
			my_truth <- as.character(read.table(file=truth_file,header=FALSE,stringsAsFactor=FALSE))
		}

		my_output$truth <- ifelse(eval(parse(text=paste('my_output',SNP_col,sep='$'))) %in% my_truth, 1, 0)
		rocobj <- roc(my_output$truth~eval(parse(text=paste('my_output',threshold_col,sep='$'))))
		AUC[file,2] <- as.numeric(rocobj$auc)

		if (include_best == 1) {
			AUC[file,3] <- coords(rocobj,'best',ret=c('threshold'))
			AUC[file,4] <- coords(rocobj,'best',ret=c('specificity'))
			AUC[file,5] <- coords(rocobj,'best',ret=c('sensitivity'))
			AUC[file,6] <- coords(rocobj,'best',ret=c('ppv'))
		}

		if (include_all == 1) {
			df <- as.data.frame(cbind(rocobj$thresholds,rocobj$sensitivities,rocobj$specificities,))
			names(df) <- c('Threshold','TPR','TNR')
			write.table(df,file=paste(filenames[file],'_ROCValues.txt',sep=''),
				quote=FALSE,row.names=FALSE,sep='\t')
			rm(df)
		}
	}
}

# Write the AUC's to a file
AUC <- as.data.frame(AUC)
names(AUC) <- c('Output','Area Under Curve','Best Threshold','Specificity (TNR)','Sensitivity (TPR)',
	'Precision (PPV)')
write(paste(AUC[,1],AUC[,2],sep='\t'),file='AUC.txt')
write.table(AUC,file='allCSV.txt',quote=FALSE,row.names=FALSE,sep='\t')