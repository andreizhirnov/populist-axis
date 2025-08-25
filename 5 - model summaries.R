library(Rcpp)
library(dplyr)
library(tidyr) 
library(abind)

set.seed(123)
rm(list=ls())

### load the functions needed to compute negative log likelihood
Rcpp::sourceCpp("src/misc.cpp")

### set subfolders
in_dir <- "./data"
aux_dir <- "./misc_inputs"
outdir <- "./output"

### load data and estimates
(loaded <-  load(file.path(in_dir, "data_for_vcp.RData")))
t0 <- openxlsx::read.xlsx(file.path(aux_dir, "template4tables.xlsx"), sheet='core')
est <- readRDS(file.path(outdir, "estimates_w_intercepts_r.rds"))

### set constants
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
fdims <- c("econ","cult", "ant","ppl","man")
popvars <- c('populism','ant','ppl','man')

### inventorization
inventory <- within(data.frame(nam=names(est)), {
  coord <- ifelse(substr(nam, 1L, 1L)=='N','Projected','Unprojected')
  cntry <- substring(nam, 5L)
  model <- substr(nam, 2L, 3L)
  model_no <- substr(nam, 3L, 3L)
  conv <- sapply(nam, function(u) est[[u]]@details$convergence)
  ndim <- ifelse(substring(model,1L,1L)=='A', "3D", "2D")
})
inventory <- subset(inventory, conv==0)
table(inventory$cntry,inventory$model)
table(inventory$model, inventory$coord)

### model fit statistics
inventory$nobs <- inventory$BIC <- inventory$ll <- inventory$AIC <- NA
for (j in seq_along(inventory$nam)) {
  inventory[j,'nobs'] <- stats4::nobs(est[[inventory$nam[j]]])  
  inventory[j,'ll'] <- stats4::logLik(est[[inventory$nam[j]]])  
  inventory[j,'AIC'] <- stats4::AIC(est[[inventory$nam[j]]])  
  inventory[j,'BIC'] <- stats4::BIC(est[[inventory$nam[j]]])  
}

write.csv(inventory[c("model","cntry", "ndim",'coord', "nobs","ll", "AIC", "BIC")], 
          file.path(outdir, "vc_model_fit.csv"), row.names=FALSE)

### extract parameter estimates
params <- lapply(est, function(e) {
  co <- e@coef
  names(nams) <- nams <- c("alpha","beta","lambda")
  lapply(nams, function(n) {
    text <- grep(paste0("^", n), names(co), val=TRUE)
    matches <- regmatches(text, gregexpr("\\d+\\.?\\d*", text))
    sorter <- sapply(matches, function(m) {
      if (length(m)==0) return(0)
      else return(as.integer(m))
    })
    if (length(text)==0) return(0)
    else return(co[text[order(sorter)]]) 
  })
})
 
### re-create the pieces of data used for estimation
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
  xlab1 <- xlab0 <- paste0(make.names(tobin), ": constant")
  for (uv in ucov) { 
    xval1 <- abind(xval1, outer(dummies, v[[uv]]), along = 2L)
    xlab1 <- c(xlab1, paste0(make.names(tobin),": ",uv))
  }
  # coordinates in the political space
  Q.df <- as.matrix(p[match(menu, p$cntry_party), sdims])
  P.df <- as.matrix(v[, sdims])
  Qx.df <- Q.df %*% t(sca.simple[[cn]])
  Px.df <- P.df %*% t(sca.simple[[cn]])
  # collect data pieces into a list
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

### models with 3 dimensions
### A1 - separable
### A2 - nonseparable
### A3 - nonseparable + intercepts
### A4 - nonseparable + controls
### A5 - nonseparable + controls + discounting
## C are 2-dimensional alternatives

acc <- list()
for (cn in countries) { 
  gt <- sam[[cn]][['y']]-1L
  wt <- sam[[cn]][['wt']]
  sam_int <- list(X=sam[[cn]][['X0']]) # intercepts only
  sam_con <- list(X=sam[[cn]][['X1']]) # intercepts and controls
  for (t in c('M','N')) {
    two <- list(Q = sam[[cn]][[paste0('Q0.',t)]], P= sam[[cn]][[paste0('P0.',t)]])
    three <- list(Q =sam[[cn]][[paste0('Q.',t)]], P = sam[[cn]][[paste0('P.',t)]])
    
    preds <- list()
    # 2-dimensional
    preds[['C1']] <- with(c(two, sam_int, params[[paste0(t, 'C1', '.',cn)]]),
                          tryCatch(rcpp_pred_base(lambda, P, Q), error=function(e) NA))
    preds[['C2']] <- with(c(two, sam_int, params[[paste0(t, 'C2', '.',cn)]]),
                          tryCatch(rcpp_pred_base(lambda, P, Q), error=function(e) NA))
    preds[['C3']] <- with(c(two, sam_int, params[[paste0(t, 'C3', '.',cn)]]),
                          tryCatch(rcpp_pred_x(lambda, P, Q, beta, X), error=function(e) NA))
    preds[['C4']] <- with(c(two, sam_con, params[[paste0(t, 'C4', '.',cn)]]),
                          tryCatch(rcpp_pred_x(lambda, P, Q, beta, X), error=function(e) NA))
    preds[['C5']] <- with(c(two, sam_con, params[[paste0(t, 'C5', '.',cn)]]),
                          tryCatch(rcpp_pred_xd110(lambda, P, Q, beta, X, alpha), error=function(e) NA))

    # 3-dimensional
    preds[['A1']] <- with(c(three, sam_int, params[[paste0(t, 'A1', '.',cn)]]),
                          tryCatch(rcpp_pred_base(lambda, P, Q), error=function(e) NA))
    preds[['A2']] <- with(c(three, sam_int, params[[paste0(t, 'A2', '.',cn)]]),
                          tryCatch(rcpp_pred_base(lambda, P, Q), error=function(e) NA))
    preds[['A3']] <- with(c(three, sam_int, params[[paste0(t, 'A3', '.',cn)]]),
                          tryCatch(rcpp_pred_x(lambda, P, Q, beta, X), error=function(e) NA))
    preds[['A4']] <- with(c(three, sam_con, params[[paste0(t, 'A4', '.',cn)]]),
                          tryCatch(rcpp_pred_x(lambda, P, Q, beta, X), error=function(e) NA))
    preds[['A5']] <- with(c(three, sam_con, params[[paste0(t, 'A5', '.',cn)]]),
                          tryCatch(rcpp_pred_xd110(lambda, P, Q, beta, X, alpha), error=function(e) NA))
    # ground truth
    acc_this <- sapply(preds, function(pf) sum((pf==gt)*wt)/sum(wt))
    acc[[paste0(t,'.',cn)]] <- data.frame(cntry=cn, t= t, model = names(acc_this), accuracy = acc_this) 
  }
}

## export the accuracy information
forout <- acc |> bind_rows() |>
  filter(t=='M') |>
  mutate(
    column = paste0('c', substring(model,2L)),
    ndim= case_match(substring(model,1L, 1L),
                     'A' ~ '3-dim.',
                     'C' ~ '2-dim.',),
    country = countries.lab[cntry],
    val = paste0(formatC(accuracy*100, digits=1, format='f'), "%"),
  ) |>
  select(country, ndim, column,  val) |>
  pivot_wider(names_from=column, values_from=val) |>
  arrange(country, ndim)

print(xtable::xtable(forout, type = "latex"), 
      file = file.path(outdir, "vc_model_acc_short_r.tex"), 
      include.rownames=FALSE, floating = FALSE, tabular.environment="longtable") 

## export summaries of the parameter estimates
fest <- lapply(countries, function(cn) {
  sel <- inventory |>  filter(cntry == cn) |> filter(grepl("^M", nam))
  e <- lapply(seq_len(nrow(sel)), function(r) {
    m <- sel$nam[r]
    mod <- paste0("c", sel$model_no[r], '.', tolower(sel$ndim[r]))
    tab <- as.data.frame(stats4::summary(est[[m]])@coef)
    colnames(tab) <- c("val","set") 
    main <- tab |> mutate(term = rownames(tab)) |>
      mutate(
        term = rownames(tab),
        p = pnorm(-abs(val)/set),
        stars = case_when(p <0.01 ~ "***",  p <0.05 ~ "**",  p <0.1 ~ "*", TRUE ~ ""),
        val1 = paste0(format(round(val, digits=2), trim=TRUE), stars),
        val2 = paste0("(", format(round(set, digits=2), trim=TRUE), ")")
      ) |> select(term, val1, val2) |>
      pivot_longer(cols=starts_with("val"), names_to="subrow", values_to=mod)
    modsta <- data.frame(term = c("AIC","BIC","nobs"),
                         subrow = 'val1',
                         val = c(
                           format(round(sel$AIC[r], digits=1), trim=TRUE),
                           format(round(sel$BIC[r], digits=1), trim=TRUE),
                           format(round(sel$nobs[r], digits=0), trim=TRUE)
                         ))
    colnames(modsta)[3L] <- mod
    rbind(main, modsta)
  }) 
  t <- Reduce(function(x,y) merge(x,y, all=TRUE), e)
  t <- replace(t, is.na(t), "") |> left_join(t0)
  left <- sam[[cn]][["X1lab"]] 
  names(left) <- paste0('beta', seq_along(left))
  sorter2 <- numeric(nrow(t))
  sorter2[grep("^beta", t$term)] <- as.numeric(substring(t$term[grep("^beta", t$term)], 5L))
  t <- t |> mutate(
    label = coalesce(label, left[term]),
    sorter = coalesce(sorter, 200 + sorter2),
    sorter = sorter + as.numeric(substring(subrow,4L))/10) |>
    arrange(sorter) |>
    mutate(
      label = ifelse(subrow=="val2", "", label),
      end = ifelse(subrow=="val1", "\\\\*", "\\\\")
    )
  cnams <- sort(grep("^c", colnames(t),val=TRUE))
  list(rows = paste0(apply(t[,c("label", cnams)], 1L, paste, collapse="&"), t$end), 
       header = paste0("Parameter&", paste(cnams, collapse="&"), "\\\\\\hline"),
       ncol = length(cnams))
})

write("\n", file = file.path(outdir, "est_rums.txt"))
for (cn in countries) {
  write(
    paste0("\n\n\\begin{longtable}{l", paste(rep("c", fest[[cn]]$ncol), collapse=""), "} \\caption{Covariates of vote choice, ",
           countries.lab[cn], "}\\\\
\\hline ", fest[[cn]]$header, "
\\endfirsthead
\\hline ", fest[[cn]]$header, "
\\endhead
\\hline\\multicolumn{",(fest[[cn]]$ncol+1L),"}{r}{Continued on next page}\\\\
\\endfoot
\\hline\\hline
\\hline \\multicolumn{",(fest[[cn]]$ncol+1L),"}{r}{*p<0.1, **p<0.05, ***p<0.01. Standard errors in parentheses.}\\\\
\\endlastfoot
"), file = file.path(outdir, "est_rums.txt"), append=TRUE)
  write(fest[[cn]]$rows, file = file.path(outdir, "est_rums.txt"), append=TRUE)
  write("\\end{longtable}", file = file.path(outdir, "est_rums.txt"), append=TRUE)
}
