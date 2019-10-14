require(tseriesChaos)
require(rgl)
require(EMD)

data = read.table("lor63.dat", header=F)[,1]
emb  = embedd(data, m=3, d=4)

plot3d(emb)

# Reading Paris.csv
data = read.csv("Paris.csv")
id = which(data[,"NAME"] == "ORLY, FR")
tmax = data[id, "TMAX"]
tmax = tmax[2600:(2600 + 25345 - 1)]

# Embedding
res = emd(tmax, boundary="wave")
stochastic = rowSums(res$imf[,1:3])
deterministic = rowSums(res$imf[,4:res$nimf]) + res$residue
emb = embedd(deterministic, m=7, d=74)

nrow(emb)
plot3d(emb[1:5000,1:3], type="l", col=1)
lines3d(emb[5001:10000,1:3], col=2)
lines3d(emb[10001:15000,1:3], col=3)
lines3d(emb[15001:20000,1:3], col=4)
