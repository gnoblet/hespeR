# hespeR

 R Handle of The Humanitarian Emergency Settings Perceived Needs Scale (HESPER)

## To install

The development version can be installed from GitHub with:

```r
devtools::install_github("gnoblet/hespeR")
```

Ultimately, the goal is to publish on CRAN.

 ## Roadmap

 - [ ] Composition of needed columns
 - [ ] Cleaning data (if needed)
 - [ ] Analyses
 - [ ] Viz
 - [ ] Publication (reports, pres)

 All steps must contain documnetation and tests.

 ## Construction

 - [ ] Use `checkmate` or `asserthat` for parameters checks
 - [ ] Use a consistent way to check for datasets details, e.g. do columns exist in df, is colA of type X, Y, Z
 - [ ] Document return outputs, document the process


## Data workflow

Below is an initial wrap-up of the data workflow we have in place.

1. `add_hesper_main()`: creates binaries for all hesper items, collapse top three priorities into one top three column. Needs helper functions `add_val_in_set_binaries()`, `add_top_three()`, `expand_bin()`, `replace_na_subset()`, `is_not_empty()`

2. `add_hesper_cat()`: function that creates different composite indicators from hesper items for each type of hesper items [no helper function, just rowSums]
 
3. `clean_top_priorities_subset()`: clean top one/two/three priorities child binary columns with NA for subset of data. Needs helper functions: `replace_na_subset()`, `is_not_empty()`


## Handling subsets and missing values

There are 3 metrics we are interested in. The two first metrics are applied on the data as is. In case a specific item is only asked to a subset of respondents (skip logic), the metrics are as is, meaning calculated on the subset.
1) Prevalence of serious problems for each Hesper item, i.e. % of individuals/households that reported a serious problem 
2) Prevalence of undefined, i.e. % of individuals/households that reported not applicable (not_applicable), prefer not to answer (pnta) or do not know (dnk)

THe last metric is applied to all households

3) (complex) Prevalence of serious problems for each Hesper item over all respondents (regardless of subsets)

### Details on initial wrangling

For metrics (1) and (2):
- ensure that items asked only to a subset of respondents are cleaned, i.e. if `subset_var` is in a set of values `subset_vals`, then `hesper_var` is set to `NA_character_`
- if all hesper items are NA, then `hesper_var` is set to `NA_character_`
- add binaries for each metric

For metrics 3:
- add new binaries (`hesper_var_overall`) for all hesper items regardless of subsets that take 1 if `hesper_var` is "serious_problem" and 0 otherwise
- if all hesper items are NA, then `hesper_var` is set to `NA_character_`

