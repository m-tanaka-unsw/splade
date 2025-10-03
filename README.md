# splade: Stable Phylogenetic Leaf ADdress Encoding

This R package supports Stable Phylogenetic Leaf Address Encoding (splade). It provides functions to convert a phylogenetic tree to a phylogenetic leaf address encoding (folio). This encoding allows taxa to be added to an existing tree in a stable manner such that the previously defined leaf addresses do not change. A set of addresses (a folio) is extended with weights on edges to form a foliox object. The package contains utilities for manipulating foliox objects. 


## Install package 


The devtools packages is needed to install splade from github:

``` r
install.packages("devtools")
```

splade uses the R packages ape, stringr, purrr: 

``` r
install.packages(c("ape","stringr", "purrr"))
```

and treeio:

``` r
devtools::install_github("YuLab-SMU/treeio")
```

Now install the splade package:

``` r
devtools::install_github("m-tanaka-unsw/splade")
```

## Authors and contributors 

Conceived and designed by Mark Tanaka, Ruiting Lan and Andrew Francis. Initial implementation by Mark Tanaka. Additional contributions by Monique Vith and Eleanor Chadwick. 

### Citation

Tanaka MM, Lan R and Francis AR (2026) Stably encoding phylogenetic trees with folios of leaf addresses. *Journal of Theoretical Biology*, 616, 112265. 

