# panlemhelpers

Package R permettant une meilleure visualisation des données du Panel Lémanique et facilitant ainsi leur analyse

## Installation

``` r
install.packages("remotes")
remotes::install_github("EPFL-LASUR/panlemhelpers")
```

## Chargement

`library(panlemhelpers)`

## Exemple

``` r
wave1_data <- opal.table_get(o, "Panel Lémanique", "wave1")
get_participants_wave1(wave1_data)
get_documentation_wave1(wave1_data)
```

## Documentation

``` r
library(panlemhelpers)
devtools::help(get_answer_wave1)
```

Pour une documentation plus détaillée, voir la [Documentation Panel Lémanique](https://epfl-lasur.github.io/Doc-Panel-L-manique/fonction-helpers/).

## License

GPL-3
