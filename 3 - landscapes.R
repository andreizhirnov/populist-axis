library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

theme_set(theme_bw() + theme(axis.title=element_text(size=12), strip.text=element_text(size=12),
                             axis.text=element_text(size=11), legend.text=element_text(size=11)))

set.seed(123) 

rm(list=ls())

in_dir <- "./data"
out_dir <- "./output"

loaded <-  load(file= file.path(in_dir, "data_for_vcp_EVES2.RData"))

countries <- c("CZ","DE","ES","FR","HU","IT","NL","SE","GB")
names(countries) <- countries

udims <- c('econ','cult','populism')



dims.lab <- c(econ="Economic", cult="Cultural",
              ant="Anti-elitism", ppl="People-centrism", man="Manichean", populism="Populism")
countries.lab <- countrycode::countrycode(countries, 'iso2c', 'country.name')
names(countries.lab) <- countries
countries.lab['GB'] <- 'England'
countries.lab <- sort(countries.lab)
### figure 2
temp <- pdata |>
  mutate(
    populist = case_when(
      is.na(popul) ~ NA,
      grepl("^P",popul) ~ 'Populist',
      TRUE ~ 'Non-populist'
    )) 
pic <- temp |> 
  drop_na(populism, poppa_populism, populist) |>
  ggplot(aes(x=poppa_populism, y=populism, shape=populist)) +
  geom_point() +
  scale_shape_manual(values=c(1,16)) +
  labs(x="Populism (POPPA)", y='Populism (our estimates)', shape="Classified by\nPopuList as") 
ggsave(filename = file.path(out_dir, "figure2.pdf"), plot = pic, width=7, height=4)
 
cat("correlation between POPPA measure and current measure: ", 
    with(temp, cor(poppa_populism, populism, use='complete.obs')),"\n")

cat("excluding Hungary: ", 
    with(subset(temp, cntry != 'HU'), cor(poppa_populism, populism, use='complete.obs')),"\n")

cat("by country:\n")  
vs <- sapply(split(temp,temp$cntry), with,  cor(poppa_populism, populism, use='complete.obs'))
for (cn in names(vs)) cat(cn, ": ", vs[cn], "\n")

## correlations
cors <- lapply(countries, function(cn) {
  d <- vdata[[cn]]
  cov.wt(d[,udims], wt = d$wt, cor = TRUE, center = TRUE,
         method = c("unbiased", "ML"))$cor 
})

pcors <- lapply(cors, function(v) {
  prec <- solve(v)
  d <- sqrt(diag(prec))
  -prec / outer(d,d)
})

## plot with correlations
stretch <- function(m) {
  m |> as.data.frame() |>
    tibble::rownames_to_column("l") |>
    pivot_longer(cols = any_of(udims), names_to="r") 
}
long_cors <- lapply(cors, stretch) |> bind_rows(.id="cntry") |> mutate(name="Bivariate")
long_pcors <- lapply(pcors, stretch) |> bind_rows(.id="cntry") |> mutate(name="Partial")

qlabs <- c('cor_econ_cult'='Economic & Cultural','cor_econ_populism'='Economic & Populist','cor_cult_populism'='Cultural & Populist')

pic <- rbind(long_cors, long_pcors) |>
  mutate(
    qty = paste0('cor_', l, "_", r),
    qty.f = factor(qty, levels=names(qlabs), labels=qlabs),
    cntry.f = factor(cntry, levels=rev(names(countries.lab)), labels=rev(countries.lab))
  ) |>
  filter(qty %in% names(qlabs)) |>
  ggplot(aes(x=value, y=cntry.f, shape=name)) +
  geom_point() +
  scale_shape_manual(breaks=c("Bivariate","Partial"), values=c(0,16)) +
  geom_vline(xintercept=0, color='red')+
  facet_wrap(vars(qty.f)) +
  labs(shape=element_blank(), x='Correlation', y=element_blank())+
  expand_limits(x=c(-1,1)) +
  theme(legend.position='bottom')
ggsave(file.path(out_dir, "figure5.pdf"), pic, width=7, height=4)
 
## landscapes
kde2d.weighted <- function(dat, w, n) {
  nx <- nrow(dat)
  gs <- lapply(dat, function(v) seq(min(v, na.rm=TRUE), max(v, na.rm=TRUE), length = n))
  hs <- sapply(dat, MASS::bandwidth.nrd) /4
  lik <- lapply(1:2, function(i) {
    a <- outer(gs[[i]], dat[[i]], "-")/hs[i]
    matrix(dnorm(a), n, nx)
  })
  if (missing(w)) w <- numeric(nx)+1
  z <- lik[[1L]] %*% diag(w) %*% t(lik[[2L]])/(sum(w) * hs[1L] * hs[2L])
  return(
    data.frame(
      x = rep(gs[[1L]], times = n),
      y = rep(gs[[1L]], each = n),
      den = as.vector(z) 
    ))
}

axes.template <- 
  list(c('x'='econ', 'y'='cult', 'z'='populism'),
       c('x'='econ', 'y'='populism', 'z'='cult'),
       c('x'='cult', 'y'='populism', 'z'='econ'))
axis.labs <- c('econ'='Economic Left-Right', 'cult'='Cultural Left-Right', 'populism'='Populist Orientation')
sdims <- names(axis.labs)
nb <- 100 

coll <- list()
for (cn in countries) {
  temp <- subset(vdata[[cn]], select=c('row','wt',sdims))
  ptemp <- subset(pdata, cntry==cn, select=c('cntry_party',sdims))
  ptemp$party <- sapply(strsplit(ptemp$cntry_party,":"), '[', 2L)
  pics <- list()
  for (a in seq_along(axes.template)) { 
    axes <- axes.template[[a]]
    kd.df <- kde2d.weighted(dat=temp[axes[c('x','y')]], n=nb, w=temp[['wt']])
    colnames(kd.df)[1:2] <- axes[c('x','y')]
    
    pics[[a]] <- ggplot(kd.df, mapping=aes(x=.data[[axes.template[[a]][['x']]]],
                                           y=.data[[axes.template[[a]][['y']]]])) +
      stat_contour(geom="polygon",aes(z = den, fill=after_stat(level)), alpha=0.8) +
      scale_fill_distiller(palette = "YlOrBr", direction = 1) +
      geom_point(data=ptemp, color="darkblue", size=3) +   
      geom_text_repel(data=ptemp, aes(label=party), color="darkblue", size=5) + 
      labs(x=axis.labs[axes.template[[a]]['x']], y=axis.labs[axes.template[[a]]['y']]) +
      theme_bw() + theme(legend.position = "none",
                         plot.caption=element_text(size=14),
                         axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
                         axis.ticks=element_blank(),  axis.text.x=element_text(size=14),  
                         axis.text.y=element_text(size=14, angle=90),
                         panel.grid.minor=element_line(), panel.grid.major=element_blank())
  } 
  text_grob <- grid::textGrob(countries.lab[cn], rot = 90, gp = grid::gpar(fontsize = 18))
  tit <- ggplotify::as.ggplot(text_grob)
  coll[[cn]] <- wrap_plots(pics[[1L]], pics[[2L]], pics[[3L]], tit, nrow=1L) +
    plot_layout(widths = c(4, 4, 4, 0.7))
  ggsave(file.path(out_dir, paste0("lscp_", cn,".pdf")), coll[[cn]], width=14, height=5) 
} 

combo <- list(
  coll$CZ / coll$GB / coll$FR / coll$DE / coll$HU,
  coll$IT / coll$NL / coll$ES / coll$SE
)

ggsave(file.path(out_dir, paste0("figure3.pdf")), combo[[1L]], width=11, height=33*length(combo[[1L]])/9) 
ggsave(file.path(out_dir, paste0("figure4.pdf")), combo[[2L]], width=11, height=33*length(combo[[2L]])/9) 





