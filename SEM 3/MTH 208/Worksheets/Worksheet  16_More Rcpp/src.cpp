#include <Rcpp.h>
using namespace Rcpp;
//PROBLEM 1
//WORKSHEET18 PROBLEM 1
// [[Rcpp::export]]
double EucC(NumericVector x, NumericVector y) {
  double track = 0;
  int n = x.size();
  for(int i = 0; i < n; i++){
    track = track + pow( (x[i] - y[i]), 2);
  }
  track = sqrt(track);
  return track;
}
//WORKSHEET18 PROBLEM 2
// [[Rcpp::export]]
NumericVector funcC(NumericVector vec)
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
}
//WORKSHEET18 PROBLEM 3
// [[Rcpp::export]]
NumericMatrix funcCmat(NumericMatrix mat1,NumericMatrix mat2)
{
  int m = mat1.nrow();
  int n = mat1.ncol();
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
}
//PROBLEM 2
int experiment()
{
  double sum=0;
  int count=0;
  while(sum<=1)
  {
    sum = sum + R::runif(0,1);
    count++;
  }
  return count;
}
// [[Rcpp::export]]
double approxEXP()
{
  double avg =0;
  for(int i=0;i<1000;i++)
  {
    avg = avg + experiment();
  }
  avg= avg/1000;
  return avg;
}

//PROBLEM 3
int blows(int age)
{
  int bls = 0;
  int remain = age;
  while(0<remain)
  {
    IntegerVector x(remain);
    x =  seq(1,remain);
    IntegerVector temp = sample(x,1);
    remain = remain - temp(0);
    bls++;
  }
  return bls;
}
// [[Rcpp::export]]
double avgBlow(int age,int n)
{
  double avg = 0;
  for(int i=0;i< n;i++)
  {
    avg = avg+ blows(age);
  }
  avg=avg/n;
  return avg;
}
//PROBLEM 4


