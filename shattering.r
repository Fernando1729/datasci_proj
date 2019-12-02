
shattering <- function(sample=1:15, R=2, iter=20) {

   S = NULL
   for (n in sample) {
	cat("Sample of size ", n, "\n")

	# dataset
	D = NULL
	for (i in 1:R) {
		D = cbind(D, rnorm(mean=0,sd=1,n=n))
	}

	hash = list()
	for (i in 1:(iter*n^3)) {
		# hyperplane
		coeffs = runif(min=-3, max=3, n=R+1)
		r = apply(D, 1, function(row) { coeffs %*% c(row, 1)} )
		classification = sign(r)
		key = paste(as.character(classification),
			    sep="", collapse="#")
		hash[[key]]=1
	}

	S = rbind(S, c(n, length(hash)))
   }

   return (S)
}

growth <- function(sample=1:15, R=2, iter=20) {

   S = NULL
   for (n in sample) {
	cat("Sample of size ", n, "\n")

	# dataset
	D = NULL
	for (i in 1:R) {
		D = cbind(D, rnorm(mean=0,sd=1,n=n))
	}

	Y = round(runif(-1, 1, n=n))
	Y[Y <  0] = -1
	Y[Y >= 0] = 1

	hash = list()
	for (i in 1:(iter*n^3)) {
		# hyperplane
		coeffs = runif(min=-3, max=3, n=R+1)
		r = apply(D, 1, function(row) { coeffs %*% c(row, 1)} )
		classification = sign(r)

		# Apply loss function
		loss = as.numeric(classification == Y)

		key = paste(as.character(loss),
			    sep="", collapse="#")
		hash[[key]]=1
	}

	S = rbind(S, c(n, length(hash)))
   }

   return (S)
}

growth2 <- function(sample=1:15, R=2, iter=20) {

   S = NULL
   for (n in sample) {
	cat("Sample of size ", n, "\n")

	# dataset
	D = NULL
	for (i in 1:R) {
		D = cbind(D, rnorm(mean=0,sd=1,n=n))
	}

	Y = round(runif(-1, 1, n=n))
	Y[Y <  0] = -1
	Y[Y >= 0] = 1

	hash = list()
	for (i in 1:(iter*n^3)) {
		# hyperplane
		coeffs = runif(min=-3, max=3, n=R+1)
		r = apply(D, 1, function(row) { coeffs %*% c(row, 1)} )

		classification = rep(0, length(r))
		classification[r <  -3] = -1
		classification[r >= -3] = 1

		# Apply loss function
		loss = as.numeric(classification == Y)

		key = paste(as.character(loss),
			    sep="", collapse="#")
		hash[[key]]=1
	}

	S = rbind(S, c(n, length(hash)))
   }

   return (S)
}
