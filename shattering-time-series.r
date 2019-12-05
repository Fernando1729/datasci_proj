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

# Create time series
N = 10000;
A = 20
alpha = 0.8
series = alpha*(A/2)*sin(2*pi*seq(0, 10, length=N)) + (1-alpha)*rnorm(n=N, sd=A/2);

tune_sigma = function(emb, testSize = 200, sigmaValues = seq(0.5, 5, length=25)){
	begin = nrow(emb) - testSize;
	result = NULL;
	for(sigma in sigmaValues){
		sum = 0;
		cat("Sigma = ", sigma, "\n");
		for(i in (begin+1):nrow(emb)){
			query = emb[i,1:(ncol(emb)-1)];
			gold  = emb[i,ncol(emb)];
			ans   = dwnn(emb[1:(i-1),], query, sigma);

			loss = (gold - ans)**2;
			sum  = sum + loss
		}
		result = rbind(result, c(sigma, sum / testSize))
	}
	colnames(result) = c("Sigma", "Squared Error");
	return(result);
}

#emb = embedd(series, m=7, d=250);
#result = tune_sigma(emb);
#print(result)
#
#       Sigma Squared Error
# [1,] 0.5000      7.783496
# [2,] 0.6875      6.398268
# [3,] 0.8750      5.357476
# [4,] 1.0625      4.723024
# [5,] 1.2500      4.332752
# [6,] 1.4375      4.100297
# [7,] 1.6250      3.976603
# [8,] 1.8125      3.917173
# [9,] 2.0000      3.888941
#[10,] 2.1875      3.874013
#[11,] 2.3750      3.864396
#[12,] 2.5625      3.856886
#[13,] 2.7500      3.850319
#[14,] 2.9375      3.844333
#[15,] 3.1250      3.838874
#[16,] 3.3125      3.833995
#[17,] 3.5000      3.829783
#[18,] 3.6875      3.826337
#[19,] 3.8750      3.823761
#[20,] 4.0625      3.822162
#[21,] 4.2500      3.821656 <----
#[22,] 4.4375      3.822366
#[23,] 4.6250      3.824426
#[24,] 4.8125      3.827985
#[25,] 5.0000      3.833205

#emb = embedd(series, m=5, d=250);
#result = tune_sigma(emb);
#print(result)
#
#       Sigma Squared Error
# [1,] 0.5000      6.662904
# [2,] 0.6875      5.526579
# [3,] 0.8750      4.981710
# [4,] 1.0625      4.665227
# [5,] 1.2500      4.469616
# [6,] 1.4375      4.362518
# [7,] 1.6250      4.309654
# [8,] 1.8125      4.284485
# [9,] 2.0000      4.271936
#[10,] 2.1875      4.264806
#[11,] 2.3750      4.260093
#[12,] 2.5625      4.256736
#[13,] 2.7500      4.254495
#[14,] 2.9375      4.253455 <----
#[15,] 3.1250      4.253827
#[16,] 3.3125      4.255876
#[17,] 3.5000      4.259904
#[18,] 3.6875      4.266244
#[19,] 3.8750      4.275264
#[20,] 4.0625      4.287374
#[21,] 4.2500      4.303031
#[22,] 4.4375      4.322746
#[23,] 4.6250      4.347082
#[24,] 4.8125      4.376656
#[25,] 5.0000      4.412141


# First for m=7

result.m7 = NULL
for(nSamples in 1:15){
	cat("nSamples = ", nSamples, "\n");
	hash = list()
	for(alpha in seq(1, 0, length=10)){
		# cat("Iterations: ", length(1:(20*ncol(emb)**2)), "\n")
		# for(i in 1:(5*ncol(emb)**2)){
		for(i in 1:20){
			A = 20;
			series = alpha*(A/2)*sin(2*pi*seq(0, 10, length=N)) + (1-alpha)*rnorm(n=N, mean=0, sd=A/2);
			D = embedd(series, m=7, d=250);

			trainSize = nrow(D) - 500;
			train = D[1:trainSize,];
			test  = D[(trainSize+1):nrow(D),];

			queryIds = sample(1:nrow(test))[1:nSamples];
			query = test[queryIds,1:(ncol(test)-1)];
			gold  = test[queryIds,ncol(test)];

			ans   = dwnn(train, query, 4.25);
			# print(cbind(ans, gold))

			loss = floor(abs(gold - ans) / (A/10));
			# print(loss)
			loss[loss > 10] = 10
			loss = paste(as.character(loss), sep="", collapse="#");
			# print(loss)
			hash[[loss]] = 1;
		}
	}
	result.m7 = rbind(result.m7, c(nSamples, length(hash)));
	cat("Shattering: ", length(hash), "\n");
}

# Now m=5

result.m5 = NULL
for(nSamples in 1:15){
	cat("nSamples = ", nSamples, "\n");
	hash = list()
	for(alpha in seq(1, 0, length=10)){
		# cat("Iterations: ", length(1:(20*ncol(emb)**2)), "\n")
		# for(i in 1:(5*ncol(emb)**2)){
		for(i in 1:20){
			A = 20;
			series = alpha*(A/2)*sin(2*pi*seq(0, 10, length=N)) + (1-alpha)*rnorm(n=N, mean=0, sd=A/2);
			D = embedd(series, m=7, d=250);

			trainSize = nrow(D) - 500;
			train = D[1:trainSize,];
			test  = D[(trainSize+1):nrow(D),];

			queryIds = sample(1:nrow(test))[1:nSamples];
			query = test[queryIds,1:(ncol(test)-1)];
			gold  = test[queryIds,ncol(test)];

			ans   = dwnn(train, query, 2.9375);
			# print(cbind(ans, gold))

			loss = floor(abs(gold - ans) / (A/10));
			# print(loss)
			loss[loss > 10] = 10
			loss = paste(as.character(loss), sep="", collapse="#");
			# print(loss)
			hash[[loss]] = 1;
		}
	}
	result.m5 = rbind(result.m5, c(nSamples, length(hash)));
	cat("Shattering: ", length(hash), "\n");
}

print("m = 7")
print(result.m5)

print("m = 5")
print(result.m7)
