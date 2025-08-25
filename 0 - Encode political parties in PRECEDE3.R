library(dplyr)
library(tidyr)

rm(list=ls())

## define the subdirectories
in_dir <- "./data"
aux_dir <- "./misc_inputs"

## load the data
dt <- readRDS(file.path(in_dir, "PRECEDE3_ww_nolabs.rds"))
pcodes <- openxlsx::read.xlsx(file.path(aux_dir, "party_codes_PRECEDE3.xlsx"), sheet="party_codes") |> 
  subset(type %in% c('rvc','ivc'), select=c('type','vnam','val','lab'))
pencodes <- openxlsx::read.xlsx(file.path(aux_dir, "party_codes_PRECEDE3.xlsx"), sheet="party_encoding") 

## extract vote choice variables from the dataset
ivc.vars <- unique(subset(pcodes, type=='ivc', select='vnam', drop=TRUE)) ## vote intention
rvc.vars <- unique(subset(pcodes, type=='rvc', select='vnam', drop=TRUE)) ## recalled vote choice

vc <- dt |> select(row, cntry, any_of(rvc.vars), any_of(ivc.vars)) |>
  pivot_longer(cols=any_of(c(rvc.vars, ivc.vars)), names_to='vnam', values_to='val', values_drop_na = TRUE) |>
  inner_join(pcodes) |>
  select(cntry, row, type, lab)

## encode the data stored as text
rec.vars <- c(ivc_text="Q9.vote.int.TEXT", rvc_text="Q8.vote.rec.TEXT")  
toencode <- dt |> select(row, cntry, any_of(rec.vars)) |>
  pivot_longer(cols=any_of(names(rec.vars)), names_to='vnam', values_to='val', values_drop_na = TRUE)
toencode <- split(toencode, toencode$cntry)
encoder <- split(pencodes, pencodes$cntry)
encoded <- lapply(names(toencode), function(cn) {
  d <- toencode[[cn]]
  fromval <- encoder[[cn]][['pattern']]
  toval <- encoder[[cn]][['lab']]
  d$newval <- ifelse(d$val %in% toval, d$val, NA)
  for (r in 1:nrow(encoder[[cn]])) {
    d$newval[which(grepl(fromval[r], trimws(d$val), ignore.case = TRUE))] <- toval[r]
  }
  d
}) |> bind_rows()
### check <- subset(encoded, is.na(newval))
encoded.w <- encoded |> filter(!is.na(newval)) |> select(cntry, row, vnam, newval) |>
  pivot_wider(names_from=vnam, values_from=newval)

## combine parties as necessary
ptran.li <- list(
  'CZ:ANO'=c('ANO'), 
  'CZ:PirSTAN'=c('PirSTAN','Pirati','STAN'),
  'CZ:Spolu'=c('Spolu','ODS','KDU-CSL','TOP09'),
  'DE:AfD'=c('AfD'),
  'DE:B90/Gru'=c('B90/Gru'),
  'DE:CDU/CSU'=c('CDU/CSU','CDU','CSU'),
  'DE:FDP'=c('FDP'), 
  'DE:SPD'=c('SPD'),
  'ES:SMR'=c('UP','IU','Podemos','AV','VQ','Compromis','MM','Mas Pais'),
  'ES:PP'=c('PP'),  
  'ES:PSOE'=c('PSOE'), 
  'ES:VOX'=c('VOX'),
  'FR:Ensemble'=c('Ensemble','LREM','MoDem','Horizons'),
  'FR:NUPES'=c('NUPES','LFI','EELV','PS','PCF'), 
  'FR:RN'=c('RN'),
  'FR:UDC'=c('UDC','UDI','LR'),
  'GB:Con'=c('Con'),
  'GB:Lab'=c('Lab'),
  'GB:LD'=c('LD'), 
  'HU:EM'=c('EM','DK','JOBBIK','MSZP','Parbedez', 'Momentum','LMP','MMM','99M'),
  'HU:Fidesz-KDNP'=c('Fidesz-KDNP','Fidesz','KDNP'), 
  'IT:CL'=c('PD','Verdi+Sinistra','Verdi','Sinistra','PlusEuropa','IC'),
  'IT:CR'=c('FdI','FI','Lega','Noi Moderati'),
  'IT:M5S'=c('M5S'), 
  'NL:D66'=c('D66'), 
  'NL:PVV'=c('PVV'),
  'NL:VVD'=c('VVD'), 
  'RO:PNL'=c('PNL'),
  'RO:PSD'=c('PSD'),
  'RO:USRPlus'=c('USRPlus'),
  'SE:M'=c('M'), 
  'SE:SAP'=c('SAP'),
  'SE:SD'=c('SD')
)
ptran.cn <- setNames(substring(names(ptran.li), 1L,2L),names(ptran.li)) 
ptran <- c(
  'IT:Azione'='IT:Azione+Italia Viva',
  'IT:Italia Viva'='IT:Azione+Italia Viva',
  'IT:Sinistra'='IT:Verdi+Sinistra',
  'IT:Verdi'= 'IT:Verdi+Sinistra',
  'ES:SMR' = 'ES:SMR',
  'ES:UP' = 'ES:SMR',
  'ES:Compromis' = 'ES:SMR'
)

vcw <- vc |> pivot_wider(names_from=type, values_from=lab) |>
  full_join(encoded.w) |>
  mutate(
    rvc = ifelse(rvc=="other" & !is.na(rvc_text), rvc_text, rvc),
    ivc = ifelse(ivc=="other" & !is.na(ivc_text), ivc_text, ivc),
    wf.recall = NA
  )
for (p in names(ptran.li)) {
  vcw$wf.recall[which(vcw$cntry==ptran.cn[p] & vcw$rvc %in% ptran.li[[p]])] <- p
}

vcw <- vcw |>
  mutate(
    wf.recall = coalesce(wf.recall, 
                          ifelse(rvc %in% c("invalid","exclude","nonvote"), "UNK", paste0(cntry, ":OTH"))
                          ),
    cntry_party_ivc = ptran[paste0(cntry, ':', ivc)],
    cntry_party_ivc = ifelse(is.na(ivc) | ivc %in% c("other","exclude","invalid","nonvote"), NA, 
                             coalesce(paste0(cntry, ":", ivc), cntry_party_ivc )),
    ivc = substring(cntry_party_ivc, 4L),
    cntry_party_rvc = ptran[paste0(cntry, ':', rvc)],
    cntry_party_rvc = ifelse(is.na(rvc) | rvc %in% c("other","exclude","invalid","nonvote"), NA, 
                             coalesce(paste0(cntry, ":", rvc), cntry_party_rvc)),
    rvc = substring(cntry_party_rvc, 4L),
    ivc_text = NULL,
    rvc_text = NULL
  )

saveRDS(vcw, file.path(in_dir, "PRECEDE3_vc.rds"))
 
