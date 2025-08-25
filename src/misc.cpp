#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]] 

// build the loss matrix (psi)

// [[Rcpp::export]]
arma::mat build_psi(
    arma::vec const& lambda, unsigned int const& nd
) {
  arma::mat L(nd,nd); 
  const unsigned int nl = lambda.n_elem;
  unsigned int cur = 0;
  for (size_t s=0; s<nd; s++) {
    for (size_t i=0; i<nd-s; i++ ) {
      if (cur>=nl) break;
      L(i+s,i) = lambda(cur);
      cur ++;
    }
  }
  return L*L.t();
}

// compute Euclidean distances
arma::mat neg_distances(
    arma::vec const& lambda, arma::mat const& P, arma::mat const& Q
){
  const unsigned int nd = Q.n_cols;
  const unsigned int np = Q.n_rows; 
  const unsigned int nv = P.n_rows;
  arma::mat psi = build_psi(lambda, nd);
  arma::rowvec a(nd);
  arma::mat dist(np, nv);
  for (size_t i=0; i<np; i++ ) { 
    for (size_t j=0; j<nv; j++) {
      a = P.row(j) - Q.row(i);
      dist(i,j) -= std::sqrt(arma::as_scalar(a*psi*a.t())); 
    }
  }
  return dist;
}

// multinomial logit neg log likelihood
double nll_cl( 
    arma::mat const& U, arma::uvec const& y, arma::vec const& wt
){
  arma::rowvec adj = arma::max(U, 0);
  arma::rowvec denom = adj + arma::log(arma::sum(arma::exp(U.each_row() - adj), 0));
  double nll = 0;
  for (size_t i=0; i<y.n_elem; i++) {
    nll += (denom(i) - U(y(i), i))*wt(i);
  }
  return nll; 
}

// negative log-likelihood

// [[Rcpp::export]]
double rcpp_nll_base(
    arma::vec const& lambda, arma::mat const& P, arma::mat const& Q, 
    arma::uvec const& y, arma::vec const& wt
){
  arma::mat U = neg_distances(lambda, P, Q);
  return nll_cl(U, y, wt);
}

// [[Rcpp::export]]
double rcpp_nll_x(
    arma::vec const& lambda, arma::mat const& P, arma::mat const& Q, 
    arma::uvec const& y, arma::vec const& wt,
    arma::vec const& beta, arma::cube const& X
){
  arma::mat U = X.each_slice()*beta;
  U = U + neg_distances(lambda, P, Q);
  return nll_cl(U, y, wt);
}
  
// [[Rcpp::export]]
double rcpp_nll_xd110(
    arma::vec const& lambda, arma::mat const& P, arma::mat const& Q, 
    arma::uvec const& y, arma::vec const& wt,
    arma::vec const& beta, arma::cube const& X,
    double const& alpha
){
  arma::mat Qd = Q; 
  double delta = 1/(1+std::exp(-alpha));
  Qd.col(0) = Qd.col(0)*delta;
  Qd.col(1) = Qd.col(1)*delta;
  arma::mat U = X.each_slice()*beta;
  U = U + neg_distances(lambda, P, Qd);  
  return nll_cl(U, y, wt);
}

// predictions

// [[Rcpp::export]]
arma::urowvec rcpp_pred_base(
    arma::vec const& lambda, arma::mat const& P, arma::mat const& Q
){
  arma::mat U = neg_distances(lambda, P, Q);
  return arma::index_max(U);
}

// [[Rcpp::export]]
arma::urowvec rcpp_pred_x(
    arma::vec const& lambda, arma::mat const& P, arma::mat const& Q,
    arma::vec const& beta, arma::cube const& X
){
  arma::mat U = X.each_slice()*beta;
  U = U + neg_distances(lambda, P, Q);
  return arma::index_max(U);
}

// [[Rcpp::export]]
arma::urowvec rcpp_pred_xd110(
    arma::vec const& lambda, arma::mat const& P, arma::mat const& Q,
    arma::vec const& beta, arma::cube const& X,
    double const& alpha
){
  arma::mat Qd = Q; 
  double delta = 1/(1+std::exp(-alpha));
  Qd.col(0) = Qd.col(0)*delta;
  Qd.col(1) = Qd.col(1)*delta;
  arma::mat U = X.each_slice()*beta;
  U = U + neg_distances(lambda, P, Qd);
  return arma::index_max(U);
}
