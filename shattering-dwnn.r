require(tseriesChaos)
require(rgl)

dwnn = function(dataset, query, sigma){
    dataset = as.matrix(dataset)
    if(!is.matrix(query)) # assume it is a vector
        query = matrix(query, nrow=1)

    X = as.matrix(dataset[,1:(ncol(dataset)-1)], ncol=(ncol(dataset)-1))
    Y = dataset[,ncol(dataset)]

    Z = apply(query, 1, function(queryRow){
        dists = apply(X, 1, function(row){
            sqrt(sum(
                (row - queryRow)**2
            ))
        })
        activations = exp(-dists**2 / (2*sigma**2))
        return( (activations %*% Y) / sum(activations) )
    })

    if(is.matrix(query))
        return(Z)
    else
        return(as.numeric(Z))
}

test.r1 = function(samps=1:15, sigma){
	result = NULL
	for(n in samps){
		cat("n = ", n, "\n");
		maxHash = 0;
		for(i in 1:100){
			# Generate the sample to classify
			samp = matrix(runif(-10, 10, n=2*n), ncol=2)

			hash = list();
			for(j in 1:(100*n^2)){
				# x has 1 dimension
				dataset = matrix(runif(-10, 10, n=2*100), nrow=100);
				r = dwnn(dataset, matrix(samp[,1], ncol=1), sigma);
				loss = floor(abs(r - samp[,2]) / 2);
				loss[loss > 10] = 10;
				key = paste(as.character(loss), sep="", collapse="#");
				hash[[key]] = 1;
			}
			maxHash = max(maxHash, length(hash));
		}
		result = rbind(result, c(n, maxHash));
	}
	return(result);
}

test.rn = function(samps=1:15, sigma, rn=6){
	result = NULL
	for(n in samps){
		cat("n = ", n, "\n");
		maxHash = 0;
		for(i in 1:100){
			# Generate the sample to classify
			samp = matrix(runif(-10, 10, n=(rn+1)*n), nrow=n)

			hash = list();
			for(j in 1:(100*n^2)){
				dataset = matrix(runif(-10, 10, n=(rn+1)*100), nrow=100);
				r = dwnn(dataset, samp[,1:rn], sigma);
				loss = floor(abs(r - samp[,rn]) / 2);
				loss[loss > 10] = 10;
				# print(loss);
				key = paste(as.character(loss), sep="", collapse="#");
				hash[[key]] = 1;
			}
			maxHash = max(maxHash, length(hash));
		}
		result = rbind(result, c(n, maxHash));
	}
	return(result);
}

result1 = test.r1(1:7, sigma=4.25);
result2 = test.r1(1:7, sigma=2);
print(result1);
print(result2);

result1 = test.rn(1:7, sigma=4.25, rn=7);
result2 = test.rn(1:7, sigma=2.9375, rn=5);
print(result1);
print(result2);
