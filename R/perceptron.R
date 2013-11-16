#calculates perceptro for a binomial distribution
#n is equals the number of samples to perform the 
#algorithm over
perceptron <- function(n=10) {
	
	y <- vector("numeric")
	h_of_x <- vector("numeric")
	
	end <- F
	rep <- 1L
	
	#model weigths
	w <- c(0, 0, 0)
	
	training_set <- matrix(ncol=n)
	#training set each column a s a vector
	training_set <- apply(training_set, 2, function(x) { x <- c( 1L,runif(2,-1,1) ) })
	
	#random vectors for build up the model
	vector1 <- c( runif(2,-1,1) )
	vector2 <- c( runif(2,-1,1) )
	
	while(!end) {
		#the random line is actually the target function
		#gets y values as from training dataset
		y <- apply(training_set, 2, function(x) { target_function(x, vector1, vector2) })
		
		#yields the h of x for every training point in the set
		h <- apply(training_set, 2, function(x) { hypothesis(x, w) })

		#yields a vector of either classified (if 0) or mis (if 1)
		h_of_x <- are.classified(h, y)

		#yields the positions of non classified points
		non_classified <- which( h_of_x %in% -1 )

		#if all points are not classified
		if( sum(h_of_x == 0) < 10 ) {
			
			pos_of_misclassified <- sample(c(non_classified, non_classified), 1,replace=T)
			random_m_point <- training_set[ , pos_of_misclassified]
			
			rep <- rep + 1
			w <- update.weigths(w, random_m_point, y[pos_of_misclassified])
		} else end = T
		
		#if( sum(h_of_x == 0) == 10 ) end = T
	}
	return(rep)
}

##
## yields the cross product between the model function
## and a vector given
##
regression_line <- function(x = NA, vector1, vector2) {
	#define two randomly points
	r_a <- vector1
	r_b <- vector2
	
	cross_product <- 0L
	#cross product to find if is either over or under the line
	cross_product <- ( (r_b[1] - r_a[1]) * (x[2] - r_a[2]) - (r_b[2] - r_a[2]) * (x[1] - r_a[1]) )
	return(cross_product)
}

## target function, yields 1 if the value is positive
## and -1 if is negative
target_function <- function(x, vector1, vector2) {
	sign(regression_line(x, vector1, vector2))
}

## yields the hypothesis with the passed weights
hypothesis <- function(x, w) {
	h <- sign(w %*% x)
	return(h)
}

## passes both h of x and y of x and compares
## if happen to be equals yields a 0 (classified)
## if not, yields a 1
are.classified <- function(h, y) {
	classified <- ifelse( h == y, 0, -1)
}

#update the weigths vector whith a cross product
update.weigths <- function(w, x, y) {
	w <- ( w + (x * y))
	return(w)
}

plot_regression <- function(training_set){
	### TODO
}

#number of samples to the model
main <- function(rep = 10, n=10) {
	average <- c()
	for(i in 1:as.numeric(rep)) {
		average <- c(average ,perceptron(n))
	}
	print("mean")
	mean(average)
}
