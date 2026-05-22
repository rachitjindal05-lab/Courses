# Consider the following R function
addR <- function(x, y)
{
  return(x + y)
}
library(Rcpp)
library(rbenchmark)
# cppFunction() is a function that takes a bunch of C++ codes (all in quotes)
# and then saves and compiles it.
# In C++, we have to declare the type of every object.
# The function we are making is addC() which accepts two integers
# and returns the sum of the two integers.
cppFunction('int addC(int x, int y) {
int sum = x + y;
return sum;
}')
addR(3, 4)
addC(3, 4)
# In R
EucR <- function(x, y)
{
  rtn <- sqrt(sum( (x-y)^2 ))
  return(rtn)
}
# In C++ using Rcpp
cppFunction('double EucC(NumericVector x, NumericVector y) {
double track = 0;
int n = x.size();
for(int i = 0; i < n; i++){
track = track + pow( (x[i] - y[i]), 2);
}
track = sqrt(track);
return track;
}
')
x <- runif(1e4)
y <- runif(1e4)
# all.equal checks whether result is the same
all.equal(EucR(x, y),EucC(x, y))

#Problem1
benchmark(EucR(x, y),EucC(x, y),replications = 1000)

#Problem2
cppFunction('NumericVector funcC(NumericVector vec)
{
  int n = vec.size();
  double slog = 0;
  NumericVector frac (n);
  for(int i=0;i<n;i++)
  {
    frac[i]=log(vec[i]);
    slog=slog+frac[i];
  }
  for(int i=0;i<n;i++)
  {
    frac[i]=frac[i]/slog;
  }
  return frac;
}')
benchmark(func(1:1e5), func2(1:1e5),funcC(1:1e5))

#Problem3
cppFunction('NumericMatrix funcC(NumericMatrix mat1,NumericMatrix mat2)
{
  int m = mat1.nrow();
  int n = mat2.ncol();
  if(m!= mat2.nrow() || n!=mat2.ncol())
  {
    printf("addition not possible");
    exit(0);
  }
  NumericMatrix sum (m,n);
  for(int i=0;i<m;i++)
  {
     for(int j=0;j<n;j++)
     {
        sum(i,j)=mat1(i,j)+mat2(i,j);
     }
  }
  return sum;
}')
mat1 <- matrix(runif(4),nrow = 2)
mat2 <- matrix(runif(4),nrow = 2)
funcC(mat1,mat2)

#Problem4
cppFunction ('NumericVector colsumC(NumericMatrix mat)
{
  int m=mat.nrow();
  int n=mat.ncol();
  NumericVector sum(n);
  for(int i=0;i<n;i++)
  {
    sum(i) = 0;
    for (int j=0;j<m;j++)
    {
      sum(i) = sum(i) + mat(j,i);
    }
  }
  return(sum);
}')
mat <- matrix(runif(1e7),nrow = 1e3)
benchmark(colSums(mat),colsumC(mat),replications = 25)
#C is not fast in this case

#Problem 5
cppFunction ('LogicalVector pm(NumericVector vec)
{
  int n = vec.size();
  LogicalVector ans(n);
  for(int i=0;i<n;i++)
  {
     if(vec(i)>0)
     {
       ans(i)=TRUE;
     }
     else
     {
       ans(i)=FALSE;
     }
  }
  return ans;
}')
