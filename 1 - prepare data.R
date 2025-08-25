library("lavaan")
library("survey")
library("mice")
library(dplyr)
library(tidyr)

set.seed(123)

rm(list=ls())
in_dir <- "./data"
out_dir <- "./output"
n_imp <- 20L  ##  change this to 20 in the final run

countries <- c("CZ","DE","ES","FR","HU","IT","NL","RO","SE","GB")
names(countries) <- countries

### aux functions
az_rake <- function(x, id, by, on, wf.ds, pop.mg) {
  dli <- split(x, x[[by]])
  over <- names(dli)
  weights <- lapply(over, function(cn) {
    wd <- subset(wf.ds, wf.ds[[by]]==cn & wf.ds[[id]] %in% dli[[cn]][[id]])
    wt.template <- lapply(on, function(j) { 
      ini <- pop.mg[[cn]][[j]]
      ini <- ini[ini[[j]]!="UNK" & ini[[j]] %in% wd[[j]],]
      unk <- mean(wd[[j]]=="UNK")
      unk.df <- data.frame(k="UNK",value=unk)
      colnames(unk.df)[1] <- j
      ini$value <- (1-unk)*ini$value/sum(ini$value)
      if (unk>0) ini <- rbind(ini, unk.df)
      ini
    })
    sur <- survey::svydesign(ids=as.formula(paste0('~',id)), probs= ~1, data= wd)
    sur.r <- survey::rake(sur, sample.margins = lapply(paste0("~",on), as.formula), 
                          population.margins = wt.template, 
                          control = list(maxit = 200))
    wt <- stats::weights(sur.r)
    wt <- wt/mean(wt)
    wt[wt>3] <- 3
    s <- data.frame(row=sur.r$cluster[[id]], wt=wt/mean(wt))
    merge(x, s, by=id)
  })
  weights$make.row.names <- FALSE
  do.call('rbind', weights)
}

## load data
dt0 <- readRDS(file.path(in_dir, "EVES2_ww_nolabs.rds"))
vc0 <- readRDS(file.path(in_dir, "PRECEDE3_vc.rds"))
bmks <- readRDS(file.path(in_dir, "Census benchmarks.rds"))
pol.mod <- openxlsx::read.xlsx(file.path(in_dir, "variables with policies.xlsx"), sheet="model3") |> na.omit()
pol.trans <- with(pol.mod, setNames(vnam, vnam_peps))
peps <- readRDS(file.path(in_dir, 'PEPS.rds')) |> filter(cntry %in% countries)
pg.cab0 <- openxlsx::read.xlsx(file.path(in_dir, "parlgov.xlsx"), sheet="cabinet")
pg.ele0 <- openxlsx::read.xlsx(file.path(in_dir, "parlgov.xlsx"), sheet="election")
popul0 <- openxlsx::read.xlsx(file.path(in_dir, "parties_information.xlsx"))
poppa0 <- foreign::read.dta(file.path(in_dir, "party_means_forR.dta"))
wf.recall <- openxlsx::read.xlsx(file.path(in_dir, "vote distribution.xlsx"), sheet="margins")

## cleaning
dt <- subset(dt0, 
             cntry %in% countries & 
               !wf.region %in% c('UKL','UKM','UKN','ES11','ES13','ES21','ES22','ES24','ES51','ES52','ES64','ES70'))

for (cn in countries) {
  bmks[[cn]][['wf.recall']] <- subset(wf.recall, cntry==cn, select=c('wf.recall','value'))
} 

#### policy positions
dims <- unique(pol.mod$dim)
qq <- tapply(pol.mod$vnam, INDEX=pol.mod$dim, FUN=paste, collapse=' + ')
mod <- paste(paste0(names(qq), " =~ ", qq , ";"), collapse = ' ') 
              
target <- with(pol.mod, setNames(vnam, vabbr))
reverse <- subset(pol.mod, reverse=='Y', select="vabbr", drop=TRUE)

### vote choice data
vc <- vc0 |> filter(!is.na(cntry_party_ivc)) |> select(cntry, row, cntry_party_ivc) |>
  mutate(
    cntry_party_ivc = case_match(cntry_party_ivc,
                                 "CZ:Spolu" ~ "CZ:ODS",
                                 .default = cntry_party_ivc))

# for (cn in countries) {
#   ivc <- vbulk |> filter(cntry==cn) |> count(cntry_party_ivc) |> filter(n>=10) |> pull(cntry_party_ivc)
#   pd <- subset(pdata, cntry==cn, select='cntry_party', drop=TRUE)
#   cat(cn, "\n")
#   cat('ivc without parties: ', setdiff(ivc, pd), '\n') 
#   cat('parties without ivc: ', setdiff(pd, ivc), '\n') 
#   #   cat('rvc without parties: ', setdiff(rvc, pd), '\n') 
#   #   cat('parties without rvc: ', setdiff(pd, rvc), '\n') 
# }

### prepare other voter data
vbulk <- within(dt, {
  female <- ifelse(wf.gender=='F',1,0)
  age_under30 <- ifelse(wf.age=='lt30',1,0)
  age_over50 <- ifelse(wf.age=='ge50',1,0)
  university <- ifelse(wf.edu=='T',1,0)
  }) |> select(cntry, row, female, university, age_under30, age_over50, all_of(target)) |> 
  mutate(across(all_of(reverse), ~ 6L-.x)) |> 
  full_join(vc)

ivc_nmi <- ifelse(is.na(vbulk$cntry_party_ivc),0,1)
pol_nmi <- ifelse(complete.cases(vbulk[,names(target)]),1,0)
# table(ivc_nmi, pol_nmi) # I can save 742 observations if using imputations
sam <- subset(vbulk, ivc_nmi+pol_nmi>0) |> 
  mutate(cntry_party_ivc = coalesce(cntry_party_ivc, 'UNK'))
 
## compute weights, pre-sample, and impute
wei.vars <- c("wf.gender","wf.age","wf.edu","wf.region",'wf.recall' )
wei.factors <- subset(dt, select=c('row','cntry',grep("^wf.", colnames(dt), val=TRUE)))
wei.factors <- merge(wei.factors, vc0[c('row','wf.recall')], all.x=TRUE )
wei.factors$wf.recall[which(is.na(wei.factors$wf.recall))] <- 'UNK' 
sam <- az_rake(sam, id='row', by='cntry', on=wei.vars, wf.ds=wei.factors, pop.mg=bmks) 
sams <- split(sam, sam$cntry)

vtemp <- lapply(sams, function(u) {
  for (v in names(target)) {
    u[[v]] <- NULL
  }
  u
})

vbulk.i <- lapply(sams, function(u) { 
  w <- u
  w$cntry <- w$row <- w$wt <-NULL 
  for (v in names(target)) {
    w[[v]] <- ordered(w[[v]], levels=1:5) 
  }
  mice::mice(w, m=n_imp, seed=123, method='rf')
}) 
 
### add party-level data to the party data 

## add parties
ptran <- c(
  'IT:Azione'='IT:Azione+Italia Viva',
  'IT:Italia Viva'='IT:Azione+Italia Viva',
  'IT:Sinistra'='IT:Verdi+Sinistra',
  'IT:Verdi'= 'IT:Verdi+Sinistra',
  'ES:SMR' = 'ES:SMR',
  'ES:UP' = 'ES:SMR',
  'ES:Compromis' = 'ES:SMR'
)

pdata <- peps |> mutate(
  nam = pol.trans[item],
  row = party)|> 
  group_by(cntry, row, nam) |>
  summarize(val.m = median(val, na.rm=TRUE), 
            val.a = mean(val, na.rm=TRUE)) |> mutate(
              val = ifelse((val.m %% 1) > -0.2 & (val.m %% 1) < 0.2, val.m, 
                           ifelse(val.a>val.m, ceiling(val.m), 
                                  ifelse(val.a<val.m, floor(val.m),
                                         ifelse(runif(1L)>=1/2, ceiling(val.m), floor(val.m))))), 
              val = 3L+as.integer(round(val)),
              val.m = NULL,
              val.a = NULL
            ) |> 
  ungroup() |>
  filter(nam %in% target) |> 
  pivot_wider(names_from=nam, values_from=val) |> 
  mutate(
    cntry_party = paste0(cntry,":",row),
    cntry_party = coalesce(ptran[cntry_party], cntry_party)) |>
  select(cntry, row, cntry_party, all_of(target)) |>
  mutate(across(all_of(reverse), ~ 6L-.x)) |>
  mutate(across(all_of(names(target)), ~ordered(.x, levels=1:5)))

pbulk <- subset(pdata, cntry_party %in% vbulk$cntry_party_ivc)

eldate <- as.Date("2023-2-1")
ptran <- c('CZ:Pi'='CZ:Pirati',  
           'CZ:SZ'='CZ:Zeleni',
           'DE:CDU'='DE:CDU/CSU',
           'DE:CDU+CSU'='DE:CDU/CSU',
           'DE:CSU'='DE:CDU/CSU',
           'DE:Greens'='DE:B90/Gru',
           'DE:PDS|Li'='DE:Linke',
           'DE:FW'="DE:Free Voters",
           "DE:PARTEI"="DE:Partei",
           'ES:AP-P'='ES:PP',
           'ES:Vox'='ES:VOX',
           'ES:UP'='ES:SMR',
           'ES:IU'='ES:SMR',
           'ES:P'='ES:SMR',
           'ES:C|AV'='ES:SMR',
           'ES:PCE|IU'='ES:SMR',
           "ES:PNV"="ES:EAJ-PNV",
           'FR:PE'='FR:EELV',
           'FR:PRG'='FR:Rad',
           'FR:PR'='FR:Rad',
           'FR:V'='FR:EELV',
           'FR:RPR'='FR:LR',
           'FR:UMP|LR'='FR:LR',
           'FR:UDF|MD'='FR:MoDem',
           'FR:DLR|DLF'='FR:DLF',
           'FR:REM'='FR:LREM',           
           'FR:NC'='FR:UDI',
           'FR:FI'='FR:LFI',
           'FR:FN'='FR:RN',
           "FR:REM|R"="FR:LREM",
           'GB:Lib'='GB:LD',
           'GB:GP'='GB:GPEW',           
           'HU:Fi+KDNP'='HU:Fidesz-KDNP',
           'HU:Fi-MPSz'='HU:Fidesz-KDNP',
           'HU:KDNP'='HU:Fidesz-KDNP',
           'HU:Jobbik'='HU:JOBBIK',
           'IT:AN'='IT:FI',
           'IT:FI-PdL'='IT:FI',
           'IT:NPSI'='IT:FI',
           'IT:LN'='IT:Lega',
           'IT:CCD'='IT:NcI',
           'IT:CCD+CDU'='IT:NcI',
           'IT:SC'='IT:NcI',
           'IT:UC'='IT:NcI',
           'IT:UDEUR'='IT:NcI',
           'IT:CeS'='IT:PD',
           'IT:DINI-RI'='IT:PD',
           'IT:DS'='IT:PD',
           'IT:PdCI'='IT:PD',
           'IT:PpP'='IT:PD',
           'IT:R'='IT:PD',
           'IT:+EU'='IT:PlusEuropa',
           'IT:PRI'='IT:PRI',
           'IT:UDR'='IT:PRI',
           'IT:FdV'='IT:Verdi+Sinistra',
           'IT:Verdi'='IT:Verdi+Sinistra',           
           'IT:S'='IT:Verdi+Sinistra',
           'IT:IV'='IT:Azione+Italia Viva',
           'NL:50+'='NL:50Plus',
           'RO:ALDE'='RO:PNL',
           'RO:LRP'='RO:PNL',
           'RO:PC'='RO:PNL',
           'RO:PD'='RO:PNL',
           'RO:PD-L'='RO:PNL',
           'RO:PSDR'='RO:PSD',
           'RO:USR'='RO:USRPlus',
           'RO:UP'='RO:USRPlus',
           'RO:PROR'='RO:PRO',
           'SE:FP'='SE:L')
 
yrm20 <- as.Date(paste0(as.integer(format(eldate, format='%Y'))-20L, format(eldate, format='-%m-%d'))) 
denom <- as.numeric(difftime(eldate, yrm20, units="days"))

pg.cab <- pg.cab0 |>
  mutate(
    cntry = countrycode::countrycode(country_name_short,'iso3c','iso2c'),
    cntry_party = paste0(cntry,":", party_name_short),
    cntry_party = coalesce(ptran[cntry_party], cntry_party), 
    start = as.Date(start_date),
  ) |> 
  filter(country_name_short %in% countrycode::countrycode(countries,'iso2c','iso3c')) |>
  filter(start<=eldate & start >= yrm20) |>
  mutate(across(ends_with("cabinet_id"), ~as.integer(.x))) |>
  select(cntry, cabinet_name, start, cabinet_party, cntry_party, ends_with("cabinet_id")) |> 
  distinct()

# keeper <- aggregate(start ~ cntry, data=subset(pg.cab, ), FUN=max) 
# sta.inc <- unique(subset(merge(pg.cab, keeper), select=c('cntry_party','cabinet_party')))
# sta.inc <- setNames(sta.inc$cabinet_party, sta.inc$cntry_party)
# 
# keeper <-  unique(as.numeric(as.character(
#   subset(pg.cab, start <= eldate & start >= yrm20 & !is.na(previous_cabinet_id), select="previous_cabinet_id", drop=TRUE  )
# )))
# pg.cab <- subset(pg.cab, (cabinet_id %in% keeper | start >= yrm20) & start <= eldate)
# 
# qq <- setdiff(unique(subset(pg.cab, cntry=='RO', select='cntry_party', drop=TRUE)), subset(pbulk, select='cntry_party', drop=TRUE))
# unique(subset(pg.cab, cntry_party %in% qq , select=c("cntry_party", "party_name_english")))


temp0 <- pg.cab |> select(cntry, start, cabinet_id, previous_cabinet_id) |>  distinct()
incab <- temp0 |> 
  left_join(temp0, by = join_by(cntry==cntry, cabinet_id ==previous_cabinet_id)) |>
  mutate(
    start.y = coalesce(start.y, eldate),
    time_ingov = as.numeric(difftime(as.Date(start.y), pmax(start.x, yrm20), units="days"))/denom,
  ) |> select(cntry, cabinet_id, time_ingov) |>
  inner_join( pg.cab  |> filter(cabinet_party>0) |> select(cntry, cabinet_id, cntry_party) ) |>
  group_by(cntry_party) |>
  summarize(time_ingov = sum(time_ingov, na.rm=TRUE))

popul.v <- popul0 |>
  mutate(type = coalesce(type, timbro_type)) |> with(setNames(type, paste0(cntry, ':', party)))
    
### election weights 
pg.ele  <- pg.ele0 |>
  mutate(
    cntry = countrycode::countrycode(country_name_short,'iso3c','iso2c'),
    cntry_party = paste0(cntry,":", party_name_short),
    cntry_party = coalesce(ptran[cntry_party], cntry_party),
    start = as.Date(election_date),
    seats_share = as.numeric(seats)/as.numeric(seats_total),
  ) |> filter(cntry %in% countries & election_type=='parliament' & start < eldate) |> 
  group_by(cntry) |> slice_max(start) |> 
  filter(seats_share>0) |> 
  # select(any_of(c('cntry',"cntry_party","party_name_english","seats_share")))
  group_by(cntry_party) |> summarize(ss=sum(seats_share, na.rm=TRUE)) |> ungroup()

## POPPA populism 
pg.grid  <- pg.ele0 |>
  mutate(
    cntry = countrycode::countrycode(country_name_short,'iso3c','iso2c'),
    cntry_party = paste0(cntry,":", party_name_short),
    cntry_party = coalesce(ptran[cntry_party], cntry_party),
    start = as.Date(election_date), 
  ) |> filter(cntry %in% countries & election_type=='parliament' & start < eldate) |> 
  group_by(cntry) |> slice_max(start) |> ungroup()

poppa1 <- poppa0 |> mutate(
  cntry = case_match(as.character(country_id),
                     'GE' ~ 'DE', 'UK' ~ 'GB', 'SP' ~ 'ES', .default = as.character(country_id))) |> 
  filter(cntry %in% countries) |>  
  select(cntry, party, populism, parlgov_id) |>
  left_join(pg.grid[c('cntry',"cntry_party","party_name_english", "party_id","vote_share")],
                by = join_by(cntry==cntry, parlgov_id==party_id))|>
  mutate(
    cntry_party = case_when(
      cntry=='CZ' & party=='KDUCSL' ~ 'CZ:KDU-CSL', 
      cntry=='ES' & party=='EHB' ~ 'ES:EH Bildu',
      cntry=='ES' & party=='IU' ~ 'ES:SMR', 
      cntry=='FR' & party=='Rad' ~ 'FR:PRG',  
      cntry=="GB" & party=="Plaid" ~ "GB:PC",
      cntry=='HU' & party=='Parbeszed' ~ 'HU:PM',   
      cntry=='IT' & party=='MDP' ~ 'IT:A1',
      cntry=='IT' & party=='SI' ~ "IT:Verdi+Sinistra",      
      cntry=='RO' & party=='USR' ~ 'RO:USRPlus',
      .default = coalesce(cntry_party, paste0(cntry, ':', party))),
      vote_share = coalesce(as.numeric(as.character(vote_share)), 0.01),
    ) |> group_by(cntry_party) |>
  summarize(poppa_populism = sum(populism*vote_share, na.rm=TRUE)/sum(vote_share, na.rm=TRUE)) 

# xx = pbulk[c('cntry','cntry_party')] |> distinct()
# setdiff(xx$cntry_party, poppa1$cntry_party)
# setdiff(poppa1$cntry_party,xx$cntry_party)

ptemp <- pbulk[c('cntry','cntry_party')] |> distinct() |>
  left_join(poppa1, by='cntry_party') |>
  left_join(incab, by='cntry_party') |> 
  left_join(pg.ele, by='cntry_party') |>  
  mutate( 
    time_ingov = coalesce(time_ingov, 0),
    popul = popul.v[cntry_party],
    party = substring(cntry_party, 4L),
    ss = coalesce(ss, 0),
    ptype = factor(popul, 
                  levels=c("PR","PL","PC","OTH"), 
                  labels=c("Populist Right", "Populist Left","Other Populist","Other"))) 

save(list=c("vbulk.i","vtemp","pbulk","ptemp"), file= file.path(in_dir, "prepared_for_vcp_EVES2.RData"))
