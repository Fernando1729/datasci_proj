# datasci_proj
Data Science Course Project

## Possibilities

- Attempt to find the best space reconstruction, which can be done by:
	1. taking more values from the Auto Mutual Information Graph
	2. tuning the parameters `m` and `d` to minimize training error

- Modify the DWNN to give more weight to more recent examples

- Verify whether we are not falling in the DWNN weakness, where the query point is not "surrounded" by other points
	- How can we check this in multidimensional space? We could take a matrix of the vector differences (q - x\_i) for all i and analyze it. Taking the dot product between lines will give us the angle, which could be used.
	- If we are falling in the weakness, we could modify the DWNN to overcome this problem.


## Related articles

- https://www.quora.com/How-can-we-use-artificial-neural-networks-to-approximate-a-piecewise-continuous-function

- https://link.springer.com/content/pdf/10.1007%2Fs11063-007-9070-9.pdf
