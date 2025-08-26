This code can be used to replicate the analysis presented in the paper "Populist Axis: Mapping European Voters and Parties in a Political Space with Populist, Economic and Cultural Dimensions."

The analysis relies on the following data (along with the data in the _misc_inputs_ folder):
* __PRECEDE3_ww_nolabs.rds__: Cross-national survey by PRECEDE3
* __PEPS.rds__: Expert survey on party positions by PRECEDE3
* __parlgov.xlsx__: Döring, Holger, Alexandra Quaas, Maike Hesse, and Philip Manow. 2023. Parliaments and governments database (ParlGov): Information on parties, elections and cabinets in established democracies.
https://www.parlgov.org/data-info/
* __POPPA_party_means_forR.dta__: Zaslove, Andrej, Maurits Meijers, and Robert Huber. 2024. Populism and Political Parties Expert Survey 2023 (POPPA). https://doi.org/10.7910/DVN/RMQREQ
 
The analysis was conducted using __R 4.4.0__,  __RTools 4.4__, and the following R packages:
* Data wrangling: dplyr (1.1.4), tidyr (1.3.1), tibble (3.2.1), openxlsx (4.2.5.2), countrycode (1.6.0)
* Survey data analysis: survey (4.4-2)
* Multiple imputations: mice (3.16.0)
* Data visualization: ggplot2 (3.5.1), ggrepel (0.9.5), patchwork (1.2.0), ggplotify (0.1.2)
* Modelling: lavaan (0.6-19), Rcpp (1.0.12), RcppArmadillo (0.12.8.4.0), abind (1.4-8)

The replication materials includes the following R files:
* __0 - Encode political parties in PRECEDE3.R__: combines party codes with the information about intended/recalled vote choice in the write-in fields
* __1 - prepare data.R__: combines data from multiple sources, calculates sampling weights and runs multiple imputations
* __2 - measurement model estimation.R__: estimates CFA models and outputs models statistics and scores for voters and parties
* __3 - landscapes.R__ and __3_landscapes.Rmd__: describe and visualize the political landscapes
  * The output can be viewed [here](3_landscapes.md)
* __4 - regression models.R__: estimate vote choice models
* __5 - model summaries.R__: export model summaries
* __6 - vote model interpretation.R__ and __6_vote_model_interpretation.Rmd__: interpret the estimates of vote choice models
  * The output can be viewed [here](6_vote_model_interpretation.md)



