## Finding out about standard error computations
## Using the DeLong approach

test <- read.table(file='/Users/dustin/Desktop/PheHasStruct_002_Trait_H2_04_GenotypeData_8pctMissing_--assoc.qassoc',header=T)
my_truth <- as.character(read.table(file='~/desktop/mytest/syntruth.txt',header=FALSE,stringsAsFactor=FALSE))

in_truth <- function(x) {
	x %in% my_truth
}

this_truth <- sapply(test$SNP, in_truth)

x1 <- test$P[this_truth==TRUE]
x2 <- test$P[this_truth==FALSE]

