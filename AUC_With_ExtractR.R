options(digits=999)

my.tab <- read.table(file='kurt.txt', header=T)

extract.auc <- function(my.tab=my.tab) {
	n1 <- 35
	n2 <- 780158
	x1 <- my.tab$RANK
	x2 <- 1:n2
	x2 <- subset(x=x2,subset=ifelse(x2 %in% x1, FALSE, TRUE))
	r <- c(x1,x2)
	auc <- (sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n2)
	return(1-auc)
}

print(extract.auc(my.tab))