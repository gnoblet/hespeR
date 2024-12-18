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