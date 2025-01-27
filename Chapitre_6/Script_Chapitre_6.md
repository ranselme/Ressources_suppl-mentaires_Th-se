Ressources complémentaires - Chapitre 6
================
Rémi Anselme
2023-04-04 18:26:31

  - [Chapitre 6](#chapitre-6)
      - [TRILL vs. OTHER dans les
        grammaires](#trill-vs-other-dans-les-grammaires)
          - [Méthodologie](#méthodologie)
          - [Visualisation et résultats](#visualisation-et-résultats)
          - [Exemples de processus phonologiques associés aux
            rhotiques](#exemples-de-processus-phonologiques-associés-aux-rhotiques)
              - [Exemple n°1 : Rhotiques et
                palatalisation](#exemple-n1--rhotiques-et-palatalisation)
              - [Exemple n°2 : Rhotiques et
                nasalisation](#exemple-n2--rhotiques-et-nasalisation)
              - [Exemple n°3 : Rhotiques et contraste de
                longueur](#exemple-n3--rhotiques-et-contraste-de-longueur)

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

# play with different values of k until you get a reasonable looking graph
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
full_data_palatalization$Value <- factor(full_data_palatalization$Value,levels = c("Phoible","Grammars"))

phoible %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ʲ")|stringr::str_detect(Allophones,"ʲ")) %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ɽ|ɾ|r|ɻ")) %>% 
  dplyr::mutate(revision = ifelse(stringr::str_detect(Phoneme,"r"),"trilled","other")) %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ʲ")==TRUE & stringr::str_detect(Phoneme,"r")==TRUE)  %>% 
  dplyr::filter(family_id != "indo1319")  -> table_pala_trill

phoible %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ʲ")|stringr::str_detect(Allophones,"ʲ")) %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ɽ|ɾ|r|ɻ")) %>% 
  dplyr::mutate(revision = ifelse(stringr::str_detect(Phoneme,"r"),"trilled","other")) %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ʲ")==TRUE & stringr::str_detect(Phoneme,"r")==FALSE)  %>% 
  dplyr::filter(family_id != "indo1319")  -> table_pala_other
```

En s’interéssant aux données de PHOIBLE, on retrouve dans 25 langues
non-indo-européennes pour 29 inventaires un segment représenté par /rʲ/
(et ses dérivés non-voisés ou rétractés). Des 29 inventaires, seulement
8 ont des allophones reportés et il s’agit systématiquement du \[rʲ\].

Pour les autres segments « simil-r » (ɽ, ɾ, ɻ), PHOIBLE comprend 22
langues pour 24 inventaires.

Pour les trills palatalisés, on a les familles de langues suivantes :

``` r
table_pala_trill %>% 
  dplyr::select(family_id,InventoryID) %>% dplyr::distinct() %>% 
  dplyr::select(family_id) %>%  table() %>% 
  as.data.frame() -> compte_trill_fam
```

1.  1.  La famille linguistique ouralienne (Glotto : `ural1272`) (16
        inventaires)
    2.  La famille linguistique austronésienne (Glotto : `aust1305`) (1
        inventaire)
    3.  La famille linguistique austroasiatique (Glotto : `aust1305`) (1
        inventaire)
    4.  La famille linguistique atlantico-congolaise (Glotto :
        `atla1278`) (6 inventaires)
    5.  La famille linguistique mongolique (Glotto : `mong1329`) (3
        inventaires)
    6.  La famille linguistique dravidienne (Glotto : `drav1251`) (2
        inventaires)

Les familles où les autres segments « simil-r » palatalisés se
retrouvent sont :

``` r
table_pala_other %>% 
  dplyr::select(family_id,InventoryID) %>% dplyr::distinct() %>% 
  dplyr::select(family_id) %>%  table() %>% 
  as.data.frame() -> compte_other_fam
```

2.  1.  La famille linguistique atlantico-congolaise (Glotto :
        `atla1278`) (8 inventaires)
    2.  La famille linguistique ouralienne (Glotto : `ural1272`) (3
        inventaires)
    3.  La famille linguistique oto-mangue (Glotto : `otom1299`) (1
        inventaire)
    4.  La famille linguistique soudanique centrale (Glotto :
        `cent2225`) (1 inventaire)
    5.  La famille linguistique arawakienne (Glotto : `araw1281`) (7
        inventaires)
    6.  La famille linguistique arawane (Glotto : `araw1282`) (1
        inventaire)
    7.  La famille linguistique borane (Glotto : `bora1262`) (1
        inventaire)
    8.  La famille linguistique zaparoane (Glotto : `zapa1251`) (1
        inventaire)
    9.  La famille linguistique turcique (Glotto : `turk1311`) (1
        inventaire)

<!-- end list -->

``` r
base_world + 
  ggplot2::geom_point(data = full_data_palatalization %>% 
                        dplyr::select(Language,Latitude,Longitude,Value,revision) %>% 
                        dplyr::mutate(revision = ifelse(revision == "trilled","TRILL","OTHER")) %>% 
                        dplyr::distinct(), ggplot2::aes(x=Longitude, y=Latitude, fill=Value),
             pch=21,size=2, alpha=1)+
  ggplot2::scale_fill_viridis_d(name = "Variétés") +
  ggplot2::ggtitle(NULL) + 
  ggplot2::theme(legend.position="bottom") +
  ggplot2::facet_grid(revision~.)
```

    ## Warning: Removed 3 rows containing missing values (`geom_point()`).

![Distribution dans les langues du monde des rhotiques palatalisées.
Dans PHOIBLE seuls les symboles suivants sont pris en compte : ɽ, ɾ et ɻ
pour les langues classifiées comme OTHER (en haut) et r pour celles
classifiées comme TRILL (en bas). Nous n’avons pas exclu les langues
indoeuropéennes de PHOIBLE pour avoir un meilleur aperçu
typologique.](Script_Chapitre_6_files/figure-gfm/unnamed-chunk-16-1.png)

#### Exemple n°2 : Rhotiques et nasalisation

``` r
data_word_trill_nasalization <- (dplyr::filter(data_word_trill,stringr::str_detect(V1,"nasal") |
                                                  stringr::str_detect(V2,"nasal")))$Language %>% unique()

phoible %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"̃")|stringr::str_detect(Allophones,"̃")) %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"ɽ|ɾ|r|ɻ")) %>% 
  dplyr::mutate(revision = ifelse(stringr::str_detect(Phoneme,"r"),"trilled","other"))%>% 
  dplyr::select(Glottocode,latitude,longitude,revision) %>% 
  dplyr::rename(Latitude = latitude,
                Longitude = longitude) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(Value_1 = "phoible_rough") -> phoible_nasalization

rough_r_data %>%
  dplyr::filter(Language %in% data_word_trill_nasalization) %>% 
  dplyr::select(Language,Latitude,Longitude,revision) %>% 
  dplyr::mutate(Value = "gramm_rough") %>% 
  dplyr::distinct() -> gramm_nasalization

dplyr::full_join(phoible_nasalization,gramm_nasalization) %>% 
  dplyr::mutate(Value = ifelse(is.na(Value),"Phoible","Grammars"),
                Language = ifelse(is.na(Language),Glottocode,Language)) %>% 
  dplyr::select(-Glottocode,-Value_1) -> full_data_nasalization
```

    ## Joining, by = c("Latitude", "Longitude", "revision")

``` r
full_data_nasalization$Value <- factor(full_data_nasalization$Value,levels = c("Phoible","Grammars"))
```

``` r
base_world + 
  ggplot2::geom_point(data = full_data_nasalization %>% 
                        dplyr::select(Language,Latitude,Longitude,Value,revision) %>% 
                        dplyr::mutate(revision = ifelse(revision == "trilled","TRILL","OTHER")) %>% 
                        dplyr::distinct(), ggplot2::aes(x=Longitude, y=Latitude, fill=Value),
             pch=21,size=2, alpha=1)+
  ggplot2::scale_fill_viridis_d(name = "Variétés") +
  ggplot2::ggtitle(NULL) + 
  ggplot2::theme(legend.position="bottom") +
  ggplot2::facet_grid(revision~.)
```

    ## Warning: Removed 3 rows containing missing values (`geom_point()`).

![Distribution dans les langues du monde des rhotiques nasalisées. Dans
PHOIBLE seuls les symboles suivants sont pris en compte : ɽ, ɾ et ɻ pour
les langues classifiées comme OTHER (en haut) et r pour celles
classifiées comme TRILL (en bas). Nous n’avons pas exclu les langues
indoeuropéennes de PHOIBLE pour avoir un meilleur aperçu
typologique.](Script_Chapitre_6_files/figure-gfm/unnamed-chunk-18-1.png)

#### Exemple n°3 : Rhotiques et contraste de longueur

``` r
phoible %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"r|ɾ|ɽ")) %>% 
  dplyr::group_by(InventoryID,Glottocode) %>% 
  dplyr::count(LanguageName) %>% 
  dplyr::filter(n > 1 & n < 10) -> language4contrast
  
phoible %>% 
  dplyr::filter(stringr::str_detect(Phoneme,"r|ɾ|ɽ"))  %>% 
  dplyr::filter(InventoryID %in% language4contrast$InventoryID) %>% 
  dplyr::select(1:7,49:55)-> data4contrast

data4contrast %>% 
  dplyr::group_by(InventoryID,longitude,latitude) %>% 
  dplyr::mutate(row_id = dplyr::row_number()) %>% 
  dplyr::summarise(Phoneme = list(Phoneme)) ->  data4contrast
```

    ## `summarise()` has grouped output by 'InventoryID', 'longitude'. You can
    ## override using the `.groups` argument.

``` r
data4contrast$Phoneme[[4]]
```

    ## [1] "ɽ" "ɾ"

``` r
# data4contrast %>% 
#   dplyr::mutate(tap_long = dplyr::case_when(stringr::str_detect(Phoneme,"ɾ(?!ː)") & stringr::str_detect(Phoneme,"ɾː") ~ 1,
#                                               T ~ 0),
#                 trill_long = dplyr::case_when(stringr::str_detect(Phoneme,"r(?!ː)") & stringr::str_detect(Phoneme,"rː") ~ 1,
#                                               T ~ 0),
#                 flap_long = dplyr::case_when(stringr::str_detect(Phoneme,"ɽ(?!ː)") & stringr::str_detect(Phoneme,"ɽː") ~ 1,
#                                               T ~ 0),
#                 tap_flap = dplyr::case_when(stringr::str_detect(Phoneme,"ɾ") & stringr::str_detect(Phoneme,"ɽ") ~ 1,
#                                               T ~ 0),
#                 trill_tap = dplyr::case_when(stringr::str_detect(Phoneme,"r") & stringr::str_detect(Phoneme,"ɾ") ~ 1,
#                                               T ~ 0),
#                 trill_flap = dplyr::case_when(stringr::str_detect(Phoneme,"r") & stringr::str_detect(Phoneme,"ɽ") ~ 1,
#                                                T ~ 0)) -> data2contrast
 
# save(data2contrast, file = "data/data2contrast.RData")
load("data/data2contrast.RData")
```

``` r
data2contrast_nonindo <- data2contrast %>% 
   dplyr::filter(!(InventoryID %in% dplyr::filter(phoible,family_id == "indo1319")$InventoryID %>% unique()))
 
data2contrast_indo <- data2contrast %>% 
   dplyr::filter(InventoryID %in% dplyr::filter(phoible,family_id == "indo1319")$InventoryID %>% unique())

data2contrast_nonindo %>% 
  dplyr::mutate(sum_cols = tap_long+trill_long+flap_long+tap_flap+trill_tap+trill_flap) %>% 
  dplyr::filter(sum_cols > 1) -> more_trills_nonindo

data2contrast_indo %>% 
  dplyr::mutate(sum_cols = tap_long+trill_long+flap_long+tap_flap+trill_tap+trill_flap) %>% 
  dplyr::filter(sum_cols > 1) -> more_trills_indo
```

3.  1.  r contraste avec ɾ → 113 inventaires (auxquels il faudrait
        ajouter 32 inventaires pour les langues indo-européennes)
    2.  r contraste avec rː → 40 inventaires (auxquels il faudrait
        ajouter 7 inventaires pour les langues indo-européennes)
    3.  r contraste avec ɽ → 81 inventaires (auxquels il faudrait
        ajouter 29 inventaires pour les langues indo-européennes)
    4.  ɾ contraste avec ɽ → 41 inventaires (auxquels il faudrait
        ajouter 21 inventaires pour les langues indo-européennes)
    5.  ɾ contraste avec ɾː → 1 inventaire
    6.  ɽ contraste avec ɽː → Non attesté (0)

Dans 18 inventaires (auxquels il faudrait ajouter 5 inventaires pour les
langues indo-européennes), on a un un contraste de longueur mettant en
jeu au moins plus de trois rhotiques. Il peut s’agir des triplets /r ɽ
ɾ/ et /r rː ɽ/.

``` r
data_word_trill_gemination <- (dplyr::filter(data_word_trill,stringr::str_detect(V1,"gemination") |
                                                  stringr::str_detect(V2,"gemination")))$Language %>% unique()

phoible %>% 
  dplyr::select(InventoryID,Glottocode) %>% dplyr::distinct() %>% 
  dplyr::right_join(data2contrast) %>% 
  dplyr::mutate(revision = ifelse(stringr::str_detect(Phoneme,"r"),"trilled","other"))%>% 
  dplyr::select(Glottocode,latitude,longitude,revision) %>% 
  dplyr::rename(Latitude = latitude,
                Longitude = longitude) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(Value_1 = "phoible_rough") -> phoible_gemination
```

    ## Joining, by = "InventoryID"

    ## Warning in stri_detect_regex(string, pattern, negate = negate, opts_regex =
    ## opts(pattern)): argument is not an atomic vector; coercing

``` r
rough_r_data %>%
  dplyr::filter(Language %in% data_word_trill_gemination) %>% 
  dplyr::select(Language,Latitude,Longitude,revision) %>% 
  dplyr::mutate(Value = "gramm_rough") %>% 
  dplyr::distinct() -> gramm_gemination

dplyr::full_join(phoible_gemination,gramm_gemination) %>% 
  dplyr::mutate(Value = ifelse(is.na(Value),"Phoible","Grammars"),
                Language = ifelse(is.na(Language),Glottocode,Language)) %>% 
  dplyr::select(-Glottocode,-Value_1) -> full_data_gemination
```

    ## Joining, by = c("Latitude", "Longitude", "revision")

``` r
full_data_gemination$Value <- factor(full_data_gemination$Value,levels = c("Phoible","Grammars"))
```

``` r
base_world + 
  ggplot2::geom_point(data = full_data_gemination %>% 
                        dplyr::select(Language,Latitude,Longitude,Value,revision) %>% 
                        dplyr::mutate(revision = ifelse(revision == "trilled","TRILL","OTHER")) %>% 
                        dplyr::distinct(), ggplot2::aes(x=Longitude, y=Latitude, fill=Value),
             pch=21,size=2, alpha=1)+
  ggplot2::scale_fill_viridis_d(name = "Variétés") +
  ggplot2::ggtitle(NULL) + 
  ggplot2::theme(legend.position="bottom") +
  ggplot2::facet_grid(revision~.)
```

    ## Warning: Removed 23 rows containing missing values (`geom_point()`).

![Distribution dans les langues du monde des contrastes de longueurs
pour les rhotiques. Dans PHOIBLE seuls les symboles suivants sont pris
en compte : ɽ et ɾ pour les langues classifiées comme OTHER (en haut) et
r pour celles classifiées comme TRILL (en bas). Nous n’avons pas exclu
les langues indo-européennes de PHOIBLE pour avoir un meilleur aperçu
typologique.](Script_Chapitre_6_files/figure-gfm/unnamed-chunk-22-1.png)

Pour ne pas avoir à travailler sur toutes les données de manière
exhaustive, nous avons préféré échantillonner nos données. Nous avons
obtenu un échantillon aléatoire de six langues pour le contraste entre
/r/ et /rː/.

``` r
# J'avais oublié de mettre un set.seed()
# data2contrast_nonindo %>% 
#   dplyr::mutate(sum_cols = tap_long+trill_long+flap_long+tap_flap+trill_tap+trill_flap) %>% 
#   dplyr::filter(trill_long > 0) %>% 
#   dplyr::ungroup() %>%
#   dplyr::sample_n(6)

echant_long_trill <- c(1442, 879, 1552,1419,2156,1418)


phoible %>%
  dplyr::filter(InventoryID %in% echant_long_trill) %>%
  dplyr::select(Glottocode,LanguageName) %>% dplyr::distinct()
```

    ##   Glottocode   LanguageName
    ## 1   kara1476      Garadjari
    ## 2   kron1241         Krongo
    ## 3   kang1288  Kanga (Kanga)
    ## 4   jams1239  Jamsay, Dogon
    ## 5   east2652 Oromo, Eastern
    ## 6   amha1245        Amharic

``` r
# J'avais oublié de mettre un set.seed()
# data2contrast_nonindo %>% 
# dplyr::mutate(sum_cols = tap_long+trill_long+flap_long+tap_flap+trill_tap+trill_flap) %>% 
# dplyr::filter(tap_flap > 0) %>% 
# dplyr::ungroup() %>%
# dplyr::sample_n(8)

echant_tap_flap <- c(1729   , 1777, 1849,1788,11, 2481,1787,1749)

phoible %>%
  dplyr::filter(InventoryID %in% echant_tap_flap) %>%
  dplyr::select(Glottocode,LanguageName) %>% dplyr::distinct()
```

    ##   Glottocode LanguageName
    ## 1   khar1287       Kharia
    ## 2   hooo1248           Ho
    ## 3   kork1243        Korku
    ## 4   duru1236        Parji
    ## 5   sora1254        Soːra
    ## 6   tami1289        Tamil
    ## 7   paum1247      Paumarí
    ## 8   kond1303        Konda

Enfin, nous nous intéressons au contraste entre /ɾ/ et /ɽ/. Nous
échantillonnons pour huit langues.

``` r
base_world + 
  ggplot2::geom_point(data = data4contrast, ggplot2::aes(x=longitude, y=latitude),
             pch=21,size=2, alpha=1,fill="#FDE725FF")+
  ggplot2::ggtitle(NULL) 
```

    ## Warning: Removed 23 rows containing missing values (`geom_point()`).

![Distribution de tous les contrastes de longueurs des rhotiques dans
les langues de PHOIBLE selon les six critères de sélection de présence
des segments dans les inventaires : /r/ en opposition à /ɾ/, /r/ en
opposition à /rː/, /r/ en opposition à /ɽ/, /ɾ/ en opposition à /ɽ/, /ɾ/
en opposition à /ɾː/, /ɽ/ en opposition à /ɽː/. Il s’agit de langues où
la présence des oppositions dans les rhotiques peut influencer
positivement la présence des trills phonétiques. Chaque point jaune
serait donc idéalement une langue avec un trill phonétique (ce qui n’est
pas le cas dans les
faits).](Script_Chapitre_6_files/figure-gfm/unnamed-chunk-25-1.png)
