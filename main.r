require(tseriesChaos)
require(rgl)

data = read.table("lor63.dat", header=F)[,1]
emb  = embedd(data, m=3, d=4)

plot3d(emb)
