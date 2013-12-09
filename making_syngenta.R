# Functions for making simulation data sets

NoPopStructure <- function(my.mu=0.5, my.s2=0.3, places=c(1212, 2479, 1048, 417, 1527, 1018, 2675, 1734), 
	effects=c(2, 2, 3, 3, 5, 5, 7, 7), pedfile='/Users/dustin/Desktop/simulation10.ped') {

	ped <- read.table(file=pedfile, header=FALSE)

	n <- nrow(ped)
	snps <- length(places)

	mu <- my.mu
	e <- rnorm(n, 0, my.s2)
	g <- matrix(rep(NA, snps), nrow=snps, ncol=n)

	for (i in 1:n) {
		associated.SNPS <- ped[i, places]
		associated.SNPS <- ifelse(associated.SNPS == 'A', 1, 0)
		g[,i] <- associated.SNPS * effects
	}

	y <- matrix(nrow=n,ncol=1)

	for (j in 1:n) {
		y[j] <- mu + e[j] + sum(g[,j])
	}

	return(y)

}