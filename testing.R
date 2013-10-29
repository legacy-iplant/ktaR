#########################################################
#####			Known-Truth Analysis App			#####
#####			By Dustin A. Landers				#####
#####			Contact: (770) 289-8830				#####
#####			dustin.landers@gmail.com			#####
#########################################################

options(warn=-1)  

# Dependencies
require(pROC)
require(methods)

app_output_dir <- '/users/dustin/desktop/output'
truth_dir <- '/users/dustin/desktop/truth'
threshold_col <- 'P'
SNP_col <- 'SNP'
truth_file_or_dir <- 0
						 
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
AUC <- matrix(nrow=length(filenames),ncol=5)
AUC[,1] <- filenames

for (file in 1:length(filenames)) {
	for (output in 1:length(app_output_list)) {
		if (filenames[file] %in% unlist(strsplit(app_output_list[output],'.',fixed=TRUE))) {
			my_output <- read.table(file=paste(app_output_dir,app_output_list[output],sep='/'),header=TRUE,stringsAsFactor=FALSE)
		}
	if (truth_file_or_dir == 0) {
		for (truth in 1:length(truth_list)) {
			if (filenames[file] %in% unlist(strsplit(truth_list[truth],'.',fixed=TRUE))) {
				my_truth <- as.character(read.table(file=paste(truth_dir,truth_list[truth],sep='/'),header=FALSE,stringsAsFactor=FALSE))
			}
		}
	}
	if (truth_file_or_dir == 1) {
		my_truth <- as.character(read.table(file=truth_file,header=FALSE,stringsAsFactor=FALSE))
	}
	my_output$truth <- ifelse(eval(parse(text=paste('my_output',SNP_col,sep='$'))) %in% my_truth, 1, 0)
	rocobj <- roc(my_output$truth~eval(parse(text=paste('my_output',threshold_col,sep='$'))))
	AUC[file,2] <- as.numeric(rocobj$auc)
	AUC[file,3] <- coords(rocobj,'best', ret=c('threshold'))
	AUC[file,4] <- coords(rocobj,'best', ret=c('specificity'))
	AUC[file,5] <- coords(rocobj,'best', ret=c('sensitivity'))
	}
}

# Write the AUC's to a file
AUC <- as.data.frame(AUC)
names(AUC) <- c('Output','Area Under Curve','Best Threshold','Specificity','Sensitivity')
write.csv(AUC,file='alltheAUC.txt',quote=FALSE, row.names=FALSE)