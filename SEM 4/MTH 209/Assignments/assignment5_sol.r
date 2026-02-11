MY_gauss_convert <- function(x, mu, Sigma)
{
	inv.Sigma <- solve(Sigma)

	transform <- as.numeric(mu + inv.Sigma%*% (x - mu))
	return(transform)
}

