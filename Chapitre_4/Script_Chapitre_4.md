Ressources complémentaires - Chapitre 4
================
Rémi Anselme
2022-12-02 18:18:13

  - [Chapitre 4](#chapitre-4)
      - [Préparation des données](#préparation-des-données)
  - [Références](#références)
      - [Packages utilisés](#packages-utilisés)
      - [Version de RStudio](#version-de-rstudio)

# Chapitre 4

## Préparation des données

Les données ont été préparés dans le fichier `data_processing.R` (*pas
totalement mis au propre*). Nous avons récupéré des fichiers `.csv`. De
cette préparation des données, nous obtenons systématiquement des
données en `.RData` prête à être importées sur R.

# Références

## Packages utilisés

``` r
ins.pack <- installed.packages()[names(sessionInfo()$otherPkgs), "Version"] %>% 
  as.data.frame() %>% tibble::rownames_to_column()
names(ins.pack)[1] <- "Packages"
names(ins.pack)[2] <- "Version"

ins.pack %>% knitr::kable()
```

| Packages      | Version |
| :------------ | :------ |
| stringr       | 1.4.1   |
| readr         | 2.1.2   |
| ggplot2       | 3.4.0   |
| dplyr         | 1.0.10  |
| rstatix       | 0.7.0   |
| phonfieldwork | 0.0.11  |
| speakr        | 3.2.1   |
| knitr         | 1.41    |

## Version de RStudio

``` r
rstudioapi::versionInfo()
```

    ## $citation
    ## 
    ## To cite RStudio in publications use:
    ## 
    ##   RStudio Team (2020). RStudio: Integrated Development Environment for
    ##   R. RStudio, PBC, Boston, MA URL http://www.rstudio.com/.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {RStudio: Integrated Development Environment for R},
    ##     author = {{RStudio Team}},
    ##     organization = {RStudio, PBC},
    ##     address = {Boston, MA},
    ##     year = {2020},
    ##     url = {http://www.rstudio.com/},
    ##   }
    ## 
    ## 
    ## $mode
    ## [1] "desktop"
    ## 
    ## $version
    ## [1] '1.3.1073'
    ## 
    ## $release_name
    ## [1] "Giant Goldenrod"
