Ressources complémentaires - Chapitre 6
================
Rémi Anselme
2023-03-31 17:22:24

  - [Chapitre 6](#chapitre-6)
      - [TRILL vs. OTHER dans les
        grammaires](#trill-vs-other-dans-les-grammaires)
          - [Méthodologie](#méthodologie)
          - [Visualisation et résultats](#visualisation-et-résultats)
          - [Exemples de processus phonologiques associés aux
            rhotiques](#exemples-de-processus-phonologiques-associés-aux-rhotiques)
              - [Exemple n°1 : Rhotiques et
                palatalisation](#exemple-n1--rhotiques-et-palatalisation)

# Chapitre 6

**Les ressources supplémentaires du Chapitre 6 sont en cours de mise au
propre.**

## TRILL vs. OTHER dans les grammaires

Pour répondre à cette question, cette section s’intéresse à ce que sont
le TRILL et OTHER dans les grammaires. Il s’agit de comprendre comment
les différents segments sont décrits. Pour cela, nous avons utilisé le
corpus d’ouvrages de descriptions linguistiques que nous avions récoltés
pour la réplication de l’étude de Winter et al. (2022) en chapitre 5.

``` r
rough_r_data <- readr::read_csv("../Chapitre_5/data_replication/rough_r_data.csv")
```

    ## Rows: 2729 Columns: 19
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (13): Language, ISO_code, Phoible_code, Meaning, Form, Trill, Dataset, F...
    ## dbl  (3): Latitude, Longitude, Rough.M
    ## lgl  (3): rough, r, l
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### Méthodologie

Le fichier CSV data\_4\_terminology\_graph a été obtenu en parcourant
les notes prises dans les ressources complémentaires du Chapitre 5 en
cherchant pour des mots-clefs pour les domaines de la phonétique et de
la phonologie.

``` r
dataTerm <- readr::read_delim("data_4_terminology_graph.csv", 
    "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  dplyr::mutate(revision = ifelse(stringr::str_detect(Phonology,"trill") & is.na(revision),"trilled",revision)) %>% 
  dplyr::filter(revision=="trilled" | revision=="other") %>% 
  dplyr::filter(!is.na(Phonology)) %>% 
  dplyr::mutate(Phonology = ifelse(revision=="trilled",paste0(Phonology,"; TRILL"),paste0(Phonology,"; OTHER")))
```

    ## New names:
    ## Rows: 332 Columns: 7
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "\t" chr
    ## (6): Language, ISO_code, Phoible_code, R_type, revision, Phonology dbl (1):
    ## ...1
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
names(dataTerm)[1] <- "X1"

dataTerm %>% 
  dplyr::group_by(X1) %>% 
  dplyr::mutate(Phonology = stringr::str_split(Phonology,";{1,2}[:space:]"),
                Length = length(unlist(Phonology))) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(Length>1)-> dataTerm_2


data4graph <- NULL

for(i in 2:nrow(dataTerm_2)){
  
  (dataTerm_2[i,] %>% 
  dplyr::mutate(cart = list(combn(unlist(Phonology),2))))$cart[[1]] %>% t() %>% 
  as.data.frame() %>% 
  dplyr::mutate(X1 = dataTerm_2$X1[i],
                Language = dataTerm_2$Language[i],
                ISO_code = dataTerm_2$ISO_code[i],
                Phoible_code = dataTerm_2$Phoible_code[i],
                R_type = dataTerm_2$R_type[i],
                revision = dataTerm_2$revision[i]) -> data2add
  
  data4graph <- dplyr::bind_rows(data4graph,data2add)
}

data4graph <- dplyr::mutate(data4graph, V1 = as.character(V1),
                                        V2 = as.character(V2))
```

``` r
concepts <- (stringr::str_c(stringr::str_replace_na(dataTerm$Phonology),collapse = "; ") %>% 
  stringr::str_split(";{1,2}[:space:]") %>% 
  table() %>% as.data.frame() %>% dplyr::rename(Freq = Freq,
                                                Phonology = 1) %>% 
  dplyr::filter(Freq>1))$Phonology
 
dataTerm_trill <- dataTerm %>% 
                    dplyr::filter(revision=="trilled")

dataTerm_other <- dataTerm %>% 
                    dplyr::filter(revision=="other")

stringr::str_c(stringr::str_replace_na(dataTerm_trill$Phonology),collapse = "; ") %>% 
  stringr::str_split(";[:space:]") %>% 
  table() %>% as.data.frame() %>% dplyr::rename(Freq_trill = Freq,
                                                Phonology = 1) %>% 
  dplyr::full_join(
    stringr::str_c(stringr::str_replace_na(dataTerm_other$Phonology),collapse = "; ") %>% 
  stringr::str_split(";[:space:]") %>% 
  table() %>% as.data.frame() %>% dplyr::rename(Freq_other = Freq,
                                                Phonology = 1),
  by="Phonology"
  ) %>% 
  dplyr::filter(Phonology %in% concepts) %>%
  dplyr::filter(Phonology != 'NA') %>% 
  dplyr::mutate(Freq_trill = ifelse(is.na(Freq_trill),0,Freq_trill),
                Freq_other = ifelse(is.na(Freq_other),0,Freq_other),
                sum_tot = log(abs(Freq_trill-Freq_other))) %>% 
  dplyr::filter(sum_tot>1) %>% 
  dplyr::mutate(Phonology = as.character(Phonology))-> data_norma
```

### Visualisation et résultats

``` r
data4graph %>% 
  dplyr::mutate(V1 = ifelse(V1=="devoising","voiceless",V1),
                V2 = ifelse(V2=="devoising","voiceless",V2),
                V1 = ifelse(V1=="hardening","fortition",V1),
                V2 = ifelse(V2=="hardening","fortition",V2)) %>% 
  dplyr::filter(V1 %in% c(data_norma$Phonology, "TRILL", "OTHER","voiceless","fortition") &
                  V2 %in% c(data_norma$Phonology, "TRILL", "OTHER","voiceless","fortition")) %>%
  dplyr::count(V1,V2) %>% 
  dplyr::rename(source = V1,
                target = V2,
                weight = n) %>% 
  dplyr::filter(weight > 10) %>% 
  igraph::graph.data.frame(directed = FALSE) -> data2plotgraph_1
  #igraph::graph.data.frame(directed = FALSE,vertices = nodes_1) -> data2plotgraph_1

nodes_1 <- data.frame(name=igraph::V(data2plotgraph_1)$name,
                    cat=c("phono","phone","phono","phono","phone","phono","phono","phono",
      "phone","phono","phono","phono","phono","phono","phone","phono",
      "phone","phono","phone","phone","phone","phono","phone","phone",
      "class","class"),
                    audience.size=c((dplyr::filter(data_norma,Phonology%in%c(igraph::V(data2plotgraph_1)$name,"devoising","hardening") &
                                                     !Phonology%in%c("TRILL","OTHER")))$sum_tot,5,5))

data4graph %>% 
  dplyr::mutate(V1 = ifelse(V1=="devoising","voiceless",V1),
                V2 = ifelse(V2=="devoising","voiceless",V2),
                V1 = ifelse(V1=="hardening","fortition",V1),
                V2 = ifelse(V2=="hardening","fortition",V2)) %>% 
  dplyr::filter(V1 %in% c(data_norma$Phonology, "TRILL", "OTHER","voiceless","fortition") &
                  V2 %in% c(data_norma$Phonology, "TRILL", "OTHER","voiceless","fortition")) %>%
  dplyr::count(V1,V2) %>% 
  dplyr::rename(source = V1,
                target = V2,
                weight = n) %>% 
  dplyr::filter(weight > 10) %>% 
  igraph::graph.data.frame(directed = FALSE,vertices = nodes_1) -> data2plotgraph_1



# normalize the edge weights between 0-1
igraph::E(data2plotgraph_1)$weight <- igraph::E(data2plotgraph_1)$weight / max(igraph::E(data2plotgraph_1)$weight)


colrs <- c("gray50", "tomato", "gold")

igraph::V(data2plotgraph_1)$color <- colrs[as.factor(igraph::V(data2plotgraph_1)$cat)]

igraph::V(data2plotgraph_1)$size <- igraph::V(data2plotgraph_1)$audience.size*4.5

# play with different values of `k` until you get a reasonable looking graph
k = 15
```

``` r
set.seed(2022)
#cairo_pdf("graph_results.pdf",width = 20, height = 20, pointsize = 12)
data2plotgraph_1 %>%  
  plot(#layout=igraph::layout_with_graphopt(data2plotgraph_1, charge=0.02),
       layout=igraph::layout_with_lgl,
       #vertex.label.color="black",
       vertex.shape="circle",
       vertex.label.cex=3.75,
        edge.width = igraph::E(data2plotgraph_1)$weight * k,
       margin=c(-0.1,-0.1,-0.1,-0.1))
```

![Graphe des associations des différents concepts de phonétique et de
phonologie de grammaires décrivant des langues possédant un /r/ trillé «
TRILL » ou un autre segment rhotique « OTHER ». Seuls les concepts qui
sont présent plus de dix fois dans les grammaires sont représentés dans
ce graphe. Les cercles jaunes correspondent à des concepts de
phonologie, alors que ceux en rouge correspondant à des concepts de
phonétique. La taille des cercles jaunes et rouges correspondent à la
fréquence des concepts, et l’épaisseur des liens à la fréquence de
l’association entre le concept et « TRILL » ou « OTHER ». Les
différentes épaisseurs des liens et tailles des cercles ont été
normalisées. Le graphe est obtenu graĉe au package igraph sur R (Csardi
et Nepusz
2006)](Script_Chapitre_6_files/figure-gfm/unnamed-chunk-5-1.png)

``` r
#dev.off()
```

``` r
# data_reg <- readr::read_delim("data_4_terminology_graph.csv", 
#     "\t", escape_double = FALSE, trim_ws = TRUE) %>%
#   dplyr::mutate(revision = ifelse(stringr::str_detect(Phonology,"trill") & is.na(revision),"trilled",revision)) %>% 
#   dplyr::filter(revision=="trilled" | revision=="other") %>% 
#   dplyr::filter(!is.na(Phonology))
# 
# 
# names(data_reg)[1] <- "X1"
# 
# data_reg %>% 
#   dplyr::group_by(X1) %>% 
#   dplyr::mutate(Phonology = stringr::str_split(Phonology,"; "),
#                 lenght = length(unlist(Phonology))) -> max_sample
# 
# max(data_reg$lenght)
# 
# data_reg %>% 
#   dplyr::group_by(X1) %>% 
#   dplyr::mutate(Phonology = stringr::str_split(Phonology,"; "),
#                 Length = length(unlist(Phonology)),
#                 con_1=NA,
#                 con_2=NA,
#                 con_3=NA,
#                 con_4=NA,
#                 con_5=NA,
#                 con_6=NA,
#                 con_7=NA,
#                 con_8=NA,
#                 con_9=NA,
#                 con_10=NA,
#                 con_11=NA,
#                 con_12=NA,
#                 con_13=NA,
#                 con_14=NA) %>% 
#   dplyr::ungroup() -> data_reg
# 
# 
# for(i in 1:nrow(data_reg)){
#   for(j in 1:max(data_reg$Length)){
#     data_reg[[as.factor(paste("con_",as.character(j),sep=""))]][i] =
#       data_reg$Phonology[[i]][j]
#   }
# }

#save(data_reg, file = "data/data_reg.RData")
```

``` r
load("data/data_reg.RData")

data_reg %>%
  tidyr::gather(key=para,value=valeur,c(con_1:con_14)) %>% 
  dplyr::mutate(para = 1) %>% 
  dplyr::group_by(Language) %>% 
  dplyr::select(-Phonology) %>% 
  dplyr::distinct() %>% 
  dplyr::left_join(rough_r_data,by=c("Language","ISO_code","Phoible_code","revision","R_type")) %>% 
  dplyr::select(-Meaning,-Form,-Trill,-Dataset) %>% 
  dplyr::distinct() %>% 
  tidyr::spread(valeur,para,fill=0) -> data_regression_para

data_regression_para$revision <- as.factor(data_regression_para$revision)
```

``` r
lme4::glmer(revision ~ trill + flap + tap + (1|Continent) + (1|Family),
            data=data_regression_para,
            family = binomial(logit)) -> f_all_full

summary(f_all_full)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: revision ~ trill + flap + tap + (1 | Continent) + (1 | Family)
    ##    Data: data_regression_para
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    227.6    255.9   -107.8    215.6      810 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -15.5099  -0.0025   0.0020   0.0245   1.9145 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  Family    (Intercept) 29.19    5.402   
    ##  Continent (Intercept) 12.11    3.480   
    ## Number of obs: 816, groups:  Family, 70; Continent, 9
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -2.411      1.641  -1.469    0.142    
    ## trill         12.494      1.834   6.813 9.58e-12 ***
    ## flap          -5.669      1.120  -5.061 4.16e-07 ***
    ## tap           -6.017      1.182  -5.093 3.53e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr) trill  flap  
    ## trill -0.061              
    ## flap  -0.075 -0.805       
    ## tap   -0.062 -0.840  0.772

``` r
lme4::glmer(revision ~ trill + (1|Continent) + (1|Family),
            data=data_regression_para,
            family = binomial(logit)) -> f_trill

summary(f_trill) 
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: revision ~ trill + (1 | Continent) + (1 | Family)
    ##    Data: data_regression_para
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    302.6    321.4   -147.3    294.6      812 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.4655 -0.0352  0.0159  0.0846  4.0936 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  Family    (Intercept) 16.703   4.087   
    ##  Continent (Intercept)  6.679   2.584   
    ## Number of obs: 816, groups:  Family, 70; Continent, 9
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -4.3173     1.2304  -3.509  0.00045 ***
    ## trill         7.0908     0.8055   8.803  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr)
    ## trill -0.324

``` r
lme4::glmer(revision ~ flap + (1|Continent) + (1|Family),
            data=data_regression_para,
            family = binomial(logit)) -> f_flap
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0963571 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(f_flap) 
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: revision ~ flap + (1 | Continent) + (1 | Family)
    ##    Data: data_regression_para
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    585.3    604.2   -288.7    577.3      812 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -7.5170 -0.1471  0.0970  0.2193  2.1958 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  Family    (Intercept) 13.95    3.735   
    ##  Continent (Intercept) 10.22    3.197   
    ## Number of obs: 816, groups:  Family, 70; Continent, 9
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.9391594  0.0007342 -1279.2   <2e-16 ***
    ## flap         0.3965380  0.0007342   540.1   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## flap 0.000 
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.0963571 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
lme4::glmer(revision ~ tap + (1|Continent)+ (1|Family),
            data=data_regression_para,
            family = binomial(logit)) -> f_tap
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge with max|grad| = 0.0939773 (tol = 0.002, component 1)
    
    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
summary(f_tap)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: revision ~ tap + (1 | Continent) + (1 | Family)
    ##    Data: data_regression_para
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    573.9    592.8   -283.0    565.9      812 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.1799 -0.1595  0.0967  0.2143  2.7513 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  Family    (Intercept) 13.73    3.705   
    ##  Continent (Intercept) 10.05    3.171   
    ## Number of obs: 816, groups:  Family, 70; Continent, 9
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.4310186  0.0007529  -572.5   <2e-16 ***
    ## tap         -0.9515387  0.0007529 -1263.8   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##     (Intr)
    ## tap 0.000 
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.0939773 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
anova(f_all_full,f_flap)
```

    ## Data: data_regression_para
    ## Models:
    ## f_flap: revision ~ flap + (1 | Continent) + (1 | Family)
    ## f_all_full: revision ~ trill + flap + tap + (1 | Continent) + (1 | Family)
    ##            npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## f_flap        4 585.34 604.16 -288.67   577.34                         
    ## f_all_full    6 227.63 255.86 -107.81   215.63 361.71  2  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(f_all_full,f_trill)
```

    ## Data: data_regression_para
    ## Models:
    ## f_trill: revision ~ trill + (1 | Continent) + (1 | Family)
    ## f_all_full: revision ~ trill + flap + tap + (1 | Continent) + (1 | Family)
    ##            npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## f_trill       4 302.61 321.42 -147.30   294.61                         
    ## f_all_full    6 227.63 255.86 -107.81   215.63 78.976  2  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(f_all_full,f_tap)
```

    ## Data: data_regression_para
    ## Models:
    ## f_tap: revision ~ tap + (1 | Continent) + (1 | Family)
    ## f_all_full: revision ~ trill + flap + tap + (1 | Continent) + (1 | Family)
    ##            npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## f_tap         4 573.95 592.77 -282.97   565.95                         
    ## f_all_full    6 227.63 255.86 -107.81   215.63 350.32  2  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
abs(AIC(f_trill) - AIC(f_flap))
```

    ## [1] 282.7377

``` r
abs(AIC(f_trill) - AIC(f_tap))
```

    ## [1] 271.342

Les différences des AIC son systématiquement largement supérieures à 5.

### Exemples de processus phonologiques associés aux rhotiques

``` r
load("../Chapitre_3/phoible.RData") 

languoid <- readr::read_delim("../Chapitre_3/languoid.csv", ",") %>% 
            dplyr::select(id,family_id,
                          parent_id,name,
                          level,latitude,
                          longitude,
                          iso639P3code)
```

    ## Rows: 24438 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (7): id, family_id, parent_id, name, level, iso639P3code, country_ids
    ## dbl (5): latitude, longitude, child_family_count, child_language_count, chil...
    ## lgl (3): bookkeeping, description, markup_description
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
languoid <- languoid %>% 
               dplyr::rename(Glottocode = id)

phoible <- phoible %>% 
               dplyr::left_join(languoid,
                                by="Glottocode")

data4graph %>% 
  dplyr::mutate(V1 = as.character(V1),
                V2 = as.character(V2)) %>% 
  dplyr::mutate(V1 = ifelse(V1=="devoising","voiceless",V1),
                V2 = ifelse(V2=="devoising","voiceless",V2)) %>% 
  dplyr::filter(V1 %in% c(data_norma$Phonology, "TRILL", "OTHER","voiceless") &
                  V2 %in% c(data_norma$Phonology, "TRILL", "OTHER","voiceless")) -> data_word_trill
```

#### Exemple n°1 : Rhotiques et palatalisation

``` r
data_word_trill_palatalization <- (dplyr::filter(data_word_trill,stringr::str_detect(V1,"palatal") |
                                                  stringr::str_detect(V2,"palatal")))$Language %>% unique()

phoible %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ʲ")|stringr::str_detect(Allophones,"ʲ")) %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ɽ|ɾ|r|ɻ")) %>% 
  dplyr::mutate(revision = ifelse(stringr::str_detect(Phoneme,"r"),"trilled","other"))%>% 
  dplyr::select(Glottocode,latitude,longitude,revision) %>% 
  dplyr::rename(Latitude = latitude,
                Longitude = longitude) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(Value_1 = "phoible_rough") -> phoible_palatalization

rough_r_data %>%
  dplyr::filter(Language %in% data_word_trill_palatalization) %>% 
  dplyr::select(Language,Latitude,Longitude,revision) %>% 
  dplyr::mutate(Value = "gramm_rough") %>% 
  dplyr::distinct() -> gramm_palatalization

dplyr::full_join(phoible_palatalization,gramm_palatalization) %>% 
  dplyr::mutate(Value = ifelse(is.na(Value),"Phoible","Grammars"),
                Language = ifelse(is.na(Language),Glottocode,Language)) %>% 
  dplyr::select(-Glottocode,-Value_1) -> full_data_palatalization
```

    ## Joining, by = c("Latitude", "Longitude", "revision")

``` r
phoible %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ʲ")|stringr::str_detect(Allophones,"ʲ")) %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ɽ|ɾ|r|ɻ")) %>% 
  dplyr::mutate(revision = ifelse(stringr::str_detect(Phoneme,"r"),"trilled","other")) %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"rʲ|r̥ʲ")==TRUE) %>% 
  dplyr::filter(family_id != "indo1319") -> table_pala_trill

length(unique(table_pala_trill$InventoryID))
```

    ## [1] 25

``` r
phoible %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ʲ")|stringr::str_detect(Allophones,"ʲ")) %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ɽ|ɾ|r|ɻ")) %>% 
  dplyr::mutate(revision = ifelse(stringr::str_detect(Phoneme,"r"),"trilled","other")) %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ʲ")==TRUE & stringr::str_detect(Phoneme,"r")==FALSE)  %>% 
  dplyr::filter(family_id != "indo1319") %>% 
  #dplyr::select(family_id,InventoryID) %>% dplyr::distinct() %>% 
  #dplyr::select(family_id) %>%  table()
  dplyr::filter(family_id == "atla1278")
```

    ##   InventoryID Glottocode ISO6393 LanguageName SpecificDialect
    ## 1         143   nucl1417     ibo         Igbo            <NA>
    ## 2         365   nucl1417     ibo         IGBO            <NA>
    ## 3         852   fwee1238     fwe          Fwe            <NA>
    ## 4         853   yeyi1239     yey         Yeyi            <NA>
    ## 5        1202   subi1246     sbs       Subiya            <NA>
    ## 6        1203   tote1238     ttl       Totela            <NA>
    ## 7        1533   eten1239     etx         Iten            <NA>
    ## 8        1533   eten1239     etx         Iten            <NA>
    ## 9        1537   irig1241     iri        Rigwe            <NA>
    ##                         GlyphID Phoneme Allophones Marginal SegmentClass Source
    ## 1                     027E+02B2      ɾʲ      ɾʲ ɾ̃ʲ     <NA>    consonant    spa
    ## 2 027E+032A+02B2+007C+027E+02B2   ɾ̪ʲ|ɾʲ       <NA>    FALSE    consonant  upsid
    ## 3                     027B+02B2      ɻʲ         ɻʲ    FALSE    consonant     ph
    ## 4                     027B+02B2      ɻʲ         ɻʲ    FALSE    consonant     ph
    ## 5                     027B+02B2      ɻʲ         ɻʲ    FALSE    consonant     ph
    ## 6                     027B+02B2      ɻʲ         ɻʲ    FALSE    consonant     ph
    ## 7                     027E+02B2      ɾʲ         ɾʲ    FALSE    consonant     gm
    ## 8                027E+02B7+02B2     ɾʷʲ        ɾʷʲ    FALSE    consonant     gm
    ## 9                     027E+02B2      ɾʲ         ɾʲ    FALSE    consonant     gm
    ##   tone stress syllabic short long consonantal sonorant continuant
    ## 1    0      -        -     -    -           +        +          +
    ## 2    0      -        -     -    -           +        +          +
    ## 3    0      -        -     -    -           +        +          +
    ## 4    0      -        -     -    -           +        +          +
    ## 5    0      -        -     -    -           +        +          +
    ## 6    0      -        -     -    -           +        +          +
    ## 7    0      -        -     -    -           +        +          +
    ## 8    0      -        -     -    -           +        +          +
    ## 9    0      -        -     -    -           +        +          +
    ##   delayedRelease approximant tap trill nasal lateral labial round labiodental
    ## 1              0           +   +     -     -       -      -     0           0
    ## 2              0           +   +     -     -       -      -     0           0
    ## 3              0           +   -     -     -       -      -     0           0
    ## 4              0           +   -     -     -       -      -     0           0
    ## 5              0           +   -     -     -       -      -     0           0
    ## 6              0           +   -     -     -       -      -     0           0
    ## 7              0           +   +     -     -       -      -     0           0
    ## 8              0           +   +     -     -       -      +     +           -
    ## 9              0           +   +     -     -       -      -     0           0
    ##   coronal anterior distributed strident dorsal high low front back tense
    ## 1       +        +           -        -      +    +   -     +    -     0
    ## 2       +        +           0        -      +    +   -     +    -     0
    ## 3       +        -           -        -      +    +   -     +    -     0
    ## 4       +        -           -        -      +    +   -     +    -     0
    ## 5       +        -           -        -      +    +   -     +    -     0
    ## 6       +        -           -        -      +    +   -     +    -     0
    ## 7       +        +           -        -      +    +   -     +    -     0
    ## 8       +        +           -        -      +    +   -     +    -     0
    ## 9       +        +           -        -      +    +   -     +    -     0
    ##   retractedTongueRoot advancedTongueRoot periodicGlottalSource
    ## 1                   0                  0                     +
    ## 2                   0                  0                     +
    ## 3                   0                  0                     +
    ## 4                   0                  0                     +
    ## 5                   0                  0                     +
    ## 6                   0                  0                     +
    ## 7                   0                  0                     +
    ## 8                   0                  0                     +
    ## 9                   0                  0                     +
    ##   epilaryngealSource spreadGlottis constrictedGlottis fortis
    ## 1                  -             -                  -      -
    ## 2                  -             -                  -      -
    ## 3                  -             -                  -      -
    ## 4                  -             -                  -      -
    ## 5                  -             -                  -      -
    ## 6                  -             -                  -      -
    ## 7                  -             -                  -      -
    ## 8                  -             -                  -      -
    ## 9                  -             -                  -      -
    ##   raisedLarynxEjective loweredLarynxImplosive click family_id parent_id   name
    ## 1                    -                      -     -  atla1278  igbo1259   Igbo
    ## 2                    -                      -     -  atla1278  igbo1259   Igbo
    ## 3                    -                      -     -  atla1278  zamb1244    Fwe
    ## 4                    -                      -     -  atla1278  cent2260   Yeyi
    ## 5                    -                      -     -  atla1278  mach1269 Subiya
    ## 6                    -                      -     -  atla1278  mach1269 Totela
    ## 7                    -                      -     -  atla1278  iten1244   Eten
    ## 8                    -                      -     -  atla1278  iten1244   Eten
    ## 9                    -                      -     -  atla1278  sout3163 Irigwe
    ##      level  latitude longitude iso639P3code revision
    ## 1 language   4.62705   7.23441          ibo    other
    ## 2 language   4.62705   7.23441          ibo    other
    ## 3 language -17.72372  24.43151          fwe    other
    ## 4 language -18.91780  23.60880          yey    other
    ## 5 language -17.56960  24.88670          sbs    other
    ## 6 language -16.95750  24.37510          ttl    other
    ## 7 language   9.73073   8.61443          etx    other
    ## 8 language   9.73073   8.61443          etx    other
    ## 9 language   9.85197   8.65250          iri    other
