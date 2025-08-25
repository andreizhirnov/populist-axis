
library(Rcpp)
library(dplyr)
library(tidyr) 
library(abind)

set.seed(123)
rm(list=ls())

### load the functions needed to compute negative log likelihood
Rcpp::sourceCpp("src/misc.cpp")

### subdirectories
in_dir <- "./data"
outdir <- "./output"

### load data
(loaded <-  load(file.path(in_dir, "data_for_vcp_EVES2.RData")))
t0 <- openxlsx::read.xlsx(file.path(in_dir, "template4tables_EVES2.xlsx"), sheet='core')

### constants
countries <- c("CZ","DE","ES","FR","HU","IT","NL","SE","GB")
names(countries) <- countries

countries.lab <- countrycode::countrycode(countries, 'iso2c', 'country.name')
names(countries.lab) <- countries
countries.lab['GB'] <- 'England'
countries.lab <- sort(countries.lab)

ucov <- c("female", "university", "age_under30", "age_over50")
pcov <- c("time_ingov")
dims <- c("econ","cult","populism","ant","ppl","man")
dims.lab <- c("Economic Left-Right","Cultural Conservatism","Populism","Anti-elitism","People-centrism","Manichean outlook")
mdims <- c('econ', 'cult')
sdims <- c('econ', 'cult', 'populism') 
popvars <- c('populism','ant','ppl','man')

## prepare sample pieces for vote choice models
sam <- lapply(countries, function(cn) {
  v <- vdata[[cn]] |> tibble::rowid_to_column("rowid")
  p <- subset(pdata, cntry==cn)
  menu <- v |> count(cntry_party_ivc) |> filter(n>10) |> 
    pull(cntry_party_ivc) |> intersect(p$cntry_party) |> sort() 
  v <- subset(v, cntry_party_ivc %in% menu, select=c('rowid','wt','cntry_party_ivc', dims, ucov))
  p <- subset(p, cntry_party %in% menu, select=c('cntry_party',pcov, dims))
  
# party-level intercepts and control variables
  tobin <- setdiff(menu, v |> count(cntry_party_ivc) |> slice_max(n) |> pull(cntry_party_ivc)) 
  dummies <- outer(menu, tobin, function(x,y) as.numeric(x==y))
  xval1 <- xval0 <- replicate(nrow(v), dummies)
  xlab1 <- xlab0 <- paste0("j_",make.names(tobin))
  for (uv in ucov) { 
    xval1 <- abind(xval1, outer(dummies, v[[uv]]), along = 2L)
    xlab1 <- c(xlab1, paste0("i_",make.names(tobin),"_",uv))
  }
# coordinates in the political space
  Q.df <- as.matrix(p[match(menu, p$cntry_party), sdims])
  P.df <- as.matrix(v[, sdims])
  Qx.df <- Q.df %*% t(sca.simple[[cn]])
  Px.df <- P.df %*% t(sca.simple[[cn]])
# compile pieces into a list
  list(
    n = nrow(P.df),
    wt=v$wt/mean(v$wt),
    Q0.M=Q.df[,mdims],
    P0.M=P.df[,mdims],    
    Q.M=Q.df[,sdims],
    P.M=P.df[,sdims],
    Q0.N=Qx.df[,mdims],
    P0.N=Px.df[,mdims],    
    Q.N=Qx.df,
    P.N=Px.df,
    X0=xval0,
    X1=xval1,
    y = match(v$cntry_party_ivc, menu),
    X0lab = xlab0,
    X1lab = xlab1
  )
})

make_nll <- function(separable=TRUE, discount=TRUE, controls=FALSE, env) { 
  ndim <- ncol(env[['Q']])

  if (controls) {
    if (discount) {
      f <- function(alpha, lambda, beta) rcpp_nll_xd110(lambda, P, Q, y-1L, wt, beta, X, alpha) 
      formals(f)[['alpha']] <- 1 
      } else {
      f <- function(lambda, beta) rcpp_nll_x(lambda, P, Q, y-1L, wt, beta, X)
      }
    formals(f)[['beta']] <- rep(0, dim(env[['X']])[2L])
  }
  else {
    f <- function(lambda) rcpp_nll_base(lambda, P, Q, y-1L, wt)
  }
  
  if (separable) {
    formals(f)[['lambda']] <- rep(1, ndim) 
  } else {
    formals(f)[['lambda']] <- c(rep(1, ndim), rep(0, (ndim*(ndim-1L)) %/% 2L))
  }
  
  list2env(env,environment(f))
  return(f)
}

### models with 3 dimensions
### A1 - separable
### A2 - nonseparable
### A3 - nonseparable + intercepts
### A4 - nonseparable + controls
### A5 - nonseparable + controls + discounting
## C are 2-dimensional alternatives

est <- list()
for (cn in countries) {
  sam_int <- sam_con <- sam[[cn]][c('y','wt')]
  sam_int[['X']] <- sam[[cn]][['X0']] # intercepts only
  sam_con[['X']] <- sam[[cn]][['X1']] # intercepts and controls
  n <- sam[[cn]][['n']]
  for (t in c('M','N')) {
    two <- list(Q = sam[[cn]][[paste0('Q0.',t)]], P= sam[[cn]][[paste0('P0.',t)]])
    three <- list(Q =sam[[cn]][[paste0('Q.',t)]], P = sam[[cn]][[paste0('P.',t)]])
    
    nlls <- list()
    # 2-dimensional
    nlls[['C1']] <- make_nll(separable=TRUE, discount=FALSE, controls=FALSE, env=c(two, sam_int))
    nlls[['C2']] <- make_nll(separable=FALSE, discount=FALSE, controls=FALSE, env=c(two, sam_int))   
    nlls[['C3']] <- make_nll(separable=FALSE, discount=FALSE, controls=TRUE, env=c(two, sam_int))   
    nlls[['C4']] <- make_nll(separable=FALSE, discount=FALSE, controls=TRUE, env=c(two, sam_con))    
    nlls[['C5']] <- make_nll(separable=FALSE, discount=TRUE, controls=TRUE, env=c(two, sam_con))
    # 3-dimensional
    nlls[['A1']] <- make_nll(separable=TRUE, discount=FALSE, controls=FALSE, env=c(three, sam_int))
    nlls[['A2']] <- make_nll(separable=FALSE, discount=FALSE, controls=FALSE, env=c(three, sam_int))   
    nlls[['A3']] <- make_nll(separable=FALSE, discount=FALSE, controls=TRUE, env=c(three, sam_int))   
    nlls[['A4']] <- make_nll(separable=FALSE, discount=FALSE, controls=TRUE, env=c(three, sam_con))    
    nlls[['A5']] <- make_nll(separable=FALSE, discount=TRUE, controls=TRUE, env=c(three, sam_con))
    
    for (mod in names(nlls)) {
      est[[paste0(t, mod, '.',cn)]] <- tryCatch(stats4::mle(minuslogl=nlls[[mod]], 
                                                            nobs=n, 
                                                            control=list(maxit=250)), 
                                                error=function(e) return(NULL))
    }
  }
}
saveRDS(est, file.path(outdir, "estimates_w_intercepts_r.rds"))