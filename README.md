# ags 

The R package `ags` helps to construct time series of statistics for Germany's municipalities (Gemeinden) and districts (Kreise) using crosswalks provided by the _Bundesinstitut für Bau-, Stadt- und Raumforschung_ (BBSR). The same crosswalks are used to assemble the [INKAR database](https://www.inkar.de/) which can be accessed using the package [bonn](https://github.com/sumtxt/bonn).


### Installation 

You can install the package directly from Github using:  

```R
remotes::install_github("sumtxt/ags", force=TRUE)
```

### Usage

The [Getting Started](https://sumtxt.github.io/ags/articles/ags.html) vignette provides details on how to use this package. 



### Complementary Packages 

* The R package `wiesbaden` [github.com/sumtxt/wiesbaden](https://github.com/sumtxt/wiesbaden) provides functions to retrieve data from the databases maintained by the Federal Statistical Office of Germany (DESTATIS) in Wiesbaden.

* The R package `bonn` [github.com/sumtxt/bonn](https://github.com/sumtxt/bonn) provides functions to retrieve data from the [INKAR](https://www.inkar.de/) database maintained by the Federal Office for Building and Regional Planning (BBSR) in Bonn.


