library(dplyr)
library(tidyr)
library(lavaan)
library(mice)

set.seed(123) 

rm(list=ls())

in_dir <- "./data"
out_dir <- "./output"

countries <- c("CZ","DE","ES","FR","HU","IT","NL","SE","GB")
names(countries) <- countries

(loaded <- load(file.path(in_dir, "prepared_for_vcp_EVES2.RData")))
 
pol.mod <- openxlsx::read.xlsx(file.path(in_dir, "variables with policies.xlsx"), sheet="model3")
udims <- c('econ','cult','populism')
dims <- unique(pol.mod$dim)

target <- pol.mod[['vabbr']]

models <- list()
models[c('CZ')] <- "
econ =~ E4 + E3; 
cult =~ C1 + C2 + C3; 
ant =~ A1 + A2 + A3; 
ppl =~ P1 + P2 + P3; 
man =~ M1 + M2 + M3;
"
models[c('HU','DE','GB','SE','IT')] <- "
econ =~ E1 + E4 + E3; 
cult =~ C1 + C2 + C3; 
ant =~ A1 + A2 + A3; 
ppl =~ P1 + P2 + P3; 
man =~ M1 + M2 + M3;
"
models[setdiff(countries, c('CZ','HU','DE','GB','SE','IT'))] <- "
econ =~ E2 + E4 + E3; 
cult =~ C1 + C2 + C3; 
ant =~ A1 + A2 + A3; 
ppl =~ P1 + P2 + P3; 
man =~ M1 + M2 + M3;
"

dims.lab <- c(econ="Economic", cult="Cultural",
              ant="Anti-elitism", ppl="People-centrism", man="Manichean", populism="Populism")
countries.lab <- countrycode::countrycode(countries, 'iso2c', 'country.name')
names(countries.lab) <- countries
countries.lab['GB'] <- 'England'
countries.lab <- sort(countries.lab)

## add party data with negligible weight
psams <- pbulk[,c('cntry','row',target)]
psams$wt <- 0.001
psams$type <- 'P'
psams <- split(psams, psams$cntry)

nimp <- vbulk.i[[1L]]$m 
sams <- lapply(countries, function(cn) {
  lapply(seq_len(nimp), function(it) { 
    i <- mice::complete(vbulk.i[[cn]], action=it)[,target]
    d <- vtemp[[cn]][c('row','cntry','wt')]
    rbind(data.frame(d,i, type='R'), psams[[cn]])
  })
})

### estimate
est <- lapply(countries, function(cn) {
  lapply(sams[[cn]], function(u) {
    cfa(models[[cn]], data=u, sampling.weights='wt', parameterization='delta')
  })
})
saveRDS(est, file.path(out_dir, "meas_estimates.rds"))

## export model fit statistics and estimates for the first imputation
est <- readRDS(file.path(out_dir, "meas_estimates.rds"))
first_models <- lapply(est, `[[`, 1L)

lapply(first_models, fitMeasures, fit.measures = c("cfi","rmsea","srmr")) |> 
  bind_rows(.id="cntry") |>
  write.csv(file=file.path(out_dir,paste0("mfit_bycountry.csv")), na='')

# type_s = c('Standardized Loadings'=1,"Latent Variable Correlations"=2,'Thresholds'=3, 'Residual Variances'=4)[type],

## export standardized solutions
sols <- lapply(first_models, standardizedSolution) |>
  bind_rows(.id="cntry") |> 
  filter(se>0) |>
  mutate(
    val1 = formatC(est.std, digits=2L),
    val2 = paste0("[",formatC(se, digits=2L), "]"),
    type = case_when(
      op =='=~' ~ 'SL',
      op =='|' ~ 'TR',
      op =='~~' & lhs %in% dims ~ "LVC",
      op =='~~' ~ 'RV'
    ),
    type = factor(type, levels=c('SL','LVC','TR','RV')),
    lab = ifelse(type=='SL', 
                 paste0(rhs, ' on ', dims.lab[lhs]), 
                 ifelse(type=='TR', 
                        paste0(lhs, ": ", rhs), 
                        ifelse(type=='RV', 
                               lhs, 
                               ifelse(
                                 type=="LVC",
                                 paste0(dims.lab[lhs], ' - ', dims.lab[rhs]),
                                 "placeholder")
                               )))
  ) |> pivot_longer(cols=starts_with("val"))|>
  select(cntry,type,lab,name,value) |>
  pivot_wider(names_from=cntry, values_from=value) |>
  arrange(type, lab, name) |>
  mutate(
    lab = ifelse(name=="val2",NA,lab) 
  )
print(xtable::xtable(sols[c("lab", countries)], type = "latex"), 
      file = file.path(out_dir, "mestimates_bycountry.tex"), 
      include.rownames=FALSE, floating = FALSE, tabular.environment="longtable")

# write.csv(sols, file=file.path(out_dir,paste0("mestimates_bycountry.csv")), row.names=FALSE, na='')

# generate predicted scores
pred <- lapply(countries, function(cn) {
  lapply(est[[cn]], lavPredict, type = "lv")
})
saveRDS(pred, file.path(out_dir, "meas_pred.rds"))

pred <- readRDS(file.path(out_dir, "meas_pred.rds"))
# lapply(pred, function(u) summary(u[[1L]]))

scored <- lapply(countries, function(cn) {
# aggregate over imputations
  good <- !is.na(sapply(pred[[cn]], '[', 1L, 1L))
  pr <- Reduce('+', pred[[cn]][good])/sum(good)
# normalize  
  wt <- sams[[cn]][[1L]]$wt
  wmeans <- colSums(pr*wt)/sum(wt)
  pr <- sweep(pr, 2L, wmeans)
  wsd <- sqrt(colSums((pr^2)*wt)/sum(wt))
  pr <- sweep(pr, 2L, wsd, "/")
# add information from the grid file
  u <- data.frame(sams[[cn]][[1L]][c('cntry','row','wt','type')], pr)  
  pop <- apply(u[,c('ant','ppl','man')], 1L, min)
  wmeans <- sum(pop*wt)/sum(wt)
  pop <- pop-wmeans
  wsd <- sqrt(sum(wt*(pop^2))/sum(wt)) 
  u$populism <- pop/wsd
  u
})
scored <- do.call('rbind', scored)
rownames(scored) <- NULL
sdims <- c(dims, 'populism')

# observed correlations
cor(scored[,sdims])

# party data
pdata <- scored |> filter(type=='P') |> 
  inner_join(pbulk, by = join_by(cntry, row)) |>
  group_by(cntry_party) |>
  summarize(across(any_of(sdims), mean)) |>
  left_join(ptemp, by=join_by(cntry_party))

# sanity check 
subset(pdata, cntry_party=='DE:AfD', select=sdims)
subset(pdata, cntry_party=='DE:Linke', select=sdims)

# user data
vdata <- scored |> 
  filter(type=="R") |> 
  select(-c(type, wt)) |>
  inner_join(bind_rows(vtemp), by = c('cntry','row')) 
vdata <- split(vdata, vdata$cntry)

# covariance between dimensions and populism
covs <- lapply(countries, function(cn) {
  d <- vdata[[cn]]
  cov.wt(d[,udims], wt = d$wt, cor = TRUE, center = TRUE,
                 method = c("unbiased", "ML"))$cor 
})
 
## make scaling matrices
sca.simple <- lapply(countries, function(cn) {  
  solve(t(chol(covs[[cn]][udims,udims])))
})

save(list=c("vdata", "pdata", "sca.simple", "sq"), file= file.path(in_dir, "data_for_vcp_EVES2.RData"))




