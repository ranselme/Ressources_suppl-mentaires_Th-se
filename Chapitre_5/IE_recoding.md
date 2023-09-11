Ressources complémentaires - Chapitre 5
================
Rémi Anselme
2023-09-11 14:53:54

- <a href="#re-codage-des-langues-indo-européennes"
  id="toc-re-codage-des-langues-indo-européennes">Re-codage des langues
  Indo-Européennes</a>
  - <a href="#langue---exemple" id="toc-langue---exemple">Langue -
    Exemple</a>
  - <a href="#afrikaans" id="toc-afrikaans">Afrikaans</a>
  - <a href="#belarusian" id="toc-belarusian">Belarusian</a>
  - <a href="#bulgarian" id="toc-bulgarian">Bulgarian</a>
  - <a href="#bosnian" id="toc-bosnian">Bosnian</a>
  - <a href="#catalan" id="toc-catalan">Catalan</a>
  - <a href="#czech" id="toc-czech">Czech</a>
  - <a href="#welsh" id="toc-welsh">Welsh</a>
  - <a href="#danish" id="toc-danish">Danish</a>
  - <a href="#german" id="toc-german">German</a>
  - <a href="#greek" id="toc-greek">Greek</a>
  - <a href="#spanish" id="toc-spanish">Spanish</a>
  - <a href="#persian" id="toc-persian">Persian</a>
  - <a href="#french" id="toc-french">French</a>
  - <a href="#frisian" id="toc-frisian">Frisian</a>
  - <a href="#irish" id="toc-irish">Irish</a>
  - <a href="#armenian" id="toc-armenian">Armenian</a>
  - <a href="#icelandic" id="toc-icelandic">Icelandic</a>
  - <a href="#italian" id="toc-italian">Italian</a>
  - <a href="#luxembourgish" id="toc-luxembourgish">Luxembourgish</a>
  - <a href="#lithuanian" id="toc-lithuanian">Lithuanian</a>
  - <a href="#latvian" id="toc-latvian">Latvian</a>
  - <a href="#macedonian" id="toc-macedonian">Macedonian</a>
  - <a href="#dutch" id="toc-dutch">Dutch</a>
  - <a href="#norwegian" id="toc-norwegian">Norwegian</a>
  - <a href="#sinhala-sinhalese" id="toc-sinhala-sinhalese">Sinhala
    (Sinhalese)</a>
  - <a href="#slovak" id="toc-slovak">Slovak</a>
  - <a href="#slovenian" id="toc-slovenian">Slovenian</a>
  - <a href="#albanian" id="toc-albanian">Albanian</a>
  - <a href="#swedish" id="toc-swedish">Swedish</a>
  - <a href="#ukrainian" id="toc-ukrainian">Ukrainian</a>
  - <a href="#yiddish" id="toc-yiddish">Yiddish</a>
  - <a href="#nepali" id="toc-nepali">Nepali</a>
  - <a href="#judeo-tat" id="toc-judeo-tat">Judeo-Tat</a>
  - <a href="#portuguese" id="toc-portuguese">Portuguese</a>
  - <a href="#romanian" id="toc-romanian">Romanian</a>
  - <a href="#breton" id="toc-breton">Breton</a>
  - <a href="#polish" id="toc-polish">Polish</a>
  - <a href="#russian" id="toc-russian">Russian</a>
  - <a href="#romani" id="toc-romani">Romani</a>
  - <a href="#bengali" id="toc-bengali">Bengali</a>
  - <a href="#marathi" id="toc-marathi">Marathi</a>
  - <a href="#punjabi" id="toc-punjabi">Punjabi</a>
  - <a href="#romagnol-std" id="toc-romagnol-std">Romagnol-std</a>
  - <a href="#lower-sorbian" id="toc-lower-sorbian">Lower Sorbian</a>
- <a href="#si-on-ajoute-plus-de-données-par-langue-pas-pour-le-moment"
  id="toc-si-on-ajoute-plus-de-données-par-langue-pas-pour-le-moment">Si
  on ajoute plus de données par langue (pas pour le moment)</a>
- <a href="#résultat-du-recodage" id="toc-résultat-du-recodage">Résultat
  du recodage</a>

``` r
rough_r_data <- readr::read_csv("data_replication/rough_r_data.csv")
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

``` r
#Petites fonctions pour faciliter le travail de recodage des données et les commantaires :

data_comments <- dplyr::bind_rows(
lang_info %>%
  dplyr::select(Language,Dataset,Trill,Comments),
clics_info %>% 
  dplyr::select(Language,Dataset,R_type,Comments)) 

change_data <- function(LANGUE,OUTPUT){
  rough_r_data <- rough_r_data %>% 
    dplyr::mutate(revision = ifelse(Language==LANGUE,OUTPUT,revision))}

show_lang <- function(LANGUAGE){
dplyr::filter(rough_r_data,Language==LANGUAGE)  %>%
  dplyr::filter(Meaning %in% c("rough", "smooth")) %>% 
  dplyr::select(Language,Dataset,Meaning,Form,Trill,R_type)}

show_comments <- function(LANGUAGE){
  dplyr::filter(data_comments,Language==LANGUAGE) %>% 
   dplyr::select(Language,Dataset,Comments)}

show_iso <- function(LANGUAGE){
dplyr::filter(rough_r_data,Language==LANGUAGE)  %>%
  dplyr::filter(Meaning %in% c("rough", "smooth")) %>% 
  dplyr::select(Language,ISO_code,Phoible_code) %>% 
  dplyr::distinct()}
```

# Re-codage des langues Indo-Européennes

``` r
rough_r_data %>%
  dplyr::filter(Family=="Indo-European") %>% 
  dplyr::filter(Meaning %in% c("rough", "smooth")) -> nb_langue
```

Nous travaillons sur 44 indo-européennes auxquelles nous rajouterons
l’anglais et le hongrois.

Nous nous basons sur la méthodologie précédement adoptée et ne nous
focusons pas sur : “This is in line with the strategy used for /r/,
where phonetically different segments (e.g., uvular fricatives in French
and the alveolar trill in Spanish) are grouped together based on their
phonological behavior.” (Winter et al. 2022: 9)

``` r
rough_r_data %>%
  dplyr::filter(Family=="Indo-European") %>% 
  dplyr::filter(Meaning %in% c("rough", "smooth")) %>% 
  dplyr::select(Language) %>%  table() 
```

    ## .
    ##           Afrikaans            Albanian            Armenian          Belarusian 
    ##                   1                   1                   2                   2 
    ##             Bengali             Bosnian              Breton           Bulgarian 
    ##                   3                   1                   4                   5 
    ##             Catalan               Czech              Danish               Dutch 
    ##                   3                   3                   4                   3 
    ##              French             Frisian              German               Greek 
    ##                   3                   2                   5                   2 
    ##           Icelandic               Irish             Italian           Judeo-Tat 
    ##                   1                   3                   2                   2 
    ##             Latvian          Lithuanian       Lower Sorbian       Luxembourgish 
    ##                   2                   8                   3                   1 
    ##          Macedonian             Marathi              Nepali           Norwegian 
    ##                   1                   3                   4                   1 
    ##             Persian              Polish          Portuguese             Punjabi 
    ##                   3                   2                   3                   3 
    ##        Romagnol-std              Romani            Romanian             Russian 
    ##                   1                   2                   3                   2 
    ## Sinhala (Sinhalese)              Slovak           Slovenian             Spanish 
    ##                   2                   2                   1                   4 
    ##             Swedish           Ukrainian               Welsh             Yiddish 
    ##                   6                   1                   2                   3

EST-QUE RAJOUTER LES MOTS QUI MANQUE POUR ROUGH ET SMOOTH POUR LES
LANGUES ? -\> FAIT -\> les traductions sont aussi présentes sur
<https://github.com/soskuthy/rough_r/blob/master/raw_data/a2_google_translate/google_translations.csv>

## Langue - Exemple

``` r
#show_lang('langue')
#show_comments('langue')
#show_iso('langue')
```

Trill :  
R_type :

Dataset :

Phoible :

Wikipedia :

Glottologue :

Autres sources :

``` r
#show_lang('langue')$Form
```

``` r
#rough_r_data <- rough_r_data %>% 
#  dplyr::mutate(revision = ifelse(Language=="langue",'other',revision))
```

## Afrikaans

``` r
show_lang('Afrikaans')
```

    ## # A tibble: 1 × 6
    ##   Language  Dataset Meaning Form  Trill R_type
    ##   <chr>     <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Afrikaans Google  rough   rowwe yes   trill

``` r
show_comments('Afrikaans')
```

    ## # A tibble: 1 × 3
    ##   Language  Dataset Comments
    ##   <chr>     <chr>   <chr>   
    ## 1 Afrikaans Google  <NA>

``` r
show_iso('Afrikaans')
```

    ## # A tibble: 1 × 3
    ##   Language  ISO_code Phoible_code
    ##   <chr>     <chr>    <chr>       
    ## 1 Afrikaans afr      afr

``` r
# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Afrikaans', Dataset='Google_trad',Meaning='rough',Form='rof', rough=TRUE, r=TRUE,l=FALSE) %>% 
#       dplyr::add_row(Language='Afrikaans', Dataset='Google_trad',Meaning='smooth',Form='glad', rough=FALSE, r=FALSE,l=TRUE)
```

Trill : yes  
R_type : trill

Dataset :  
Google, NA

Phoible :  
<https://phoible.org/inventories/view/1395>  
- r allophone r

Donaldson, Bruce C. 1993. A grammar of Afrikaans. Mouton de Gruyter.  
- Page 15 : 1.4.12. The consonant r \[r\]  
Examples: rot \[rot\] ‘rat’, raar \[ra:r\] ’queer\*, verder \[f^rdar\]
‘further’.  
R is trilled in Afrikaans. In various areas of the Cape uvular r, both
\[R\] and \[ʁ\], occurs and in the north is regarded as a shibboleth of
a “Capie” (see footnote 9). It is called gebryde r, e.g. Hy bry, want hy
kom van Malmesbury se wêreld αf ‘He has a uvular r because he comes from
the Malmesbury area’.  
- Footnote 9 Page 6 : If one wants to parody a Transvaler, one uses this
sound, whereas the countryside of the Western Cape is parodied by using
the uvular r and raising ee and oo to \[i\] and \[u\], e.g. lees \[lis\]
‘to read’, brood \[bRut\]‘bread’.

Wikipedia :  
<https://en.wikipedia.org/wiki/Afrikaans>  
- Rhotic r  
- /r/ is usually an alveolar trill \[r\] or tap \[ɾ\].\[109\] In some
parts of the former Cape Province, it is realized uvularly, either as a
trill \[ʀ\] or a fricative \[ʁ\].\[110\] \[109\] - Lass, Roger (1987),
“Intradiphthongal Dependencies”, in Anderson, John; Durand, Jacques
(eds.), Explorations in Dependency Phonology, Dordrecht: Foris
Publications Holland, pp. 109–131, ISBN 90-6765-297-0  
\[110\] - Donaldson (1993), p. 15.

Glottologue :  
<https://glottolog.org/resource/languoid/id/afri1274>

Autres sources :

Wissing, D. (2020). Afrikaans. Journal of the International Phonetic
Association, 50(1), 127-140. <doi:10.1017/S0025100318000269>  
- Page 130 : Trill r ʀ - Page 131 : The alveolar trill \[r\] varies with
the uvular trill \[ʀ\], the latter found notably in the Boland region of
the Western Cape, but also quite frequently in other areas of SA
(Pienaar 2017 provides an overview and mentions relevant literature; see
also Ribbens-Klein 2016).  
- Page 132 : In colloquial speech, the voiced alveolar trill /r/ tends
to be omitted in syllable-final position. This happens mostly in
unstressed syllables, and regularly in the prefix ver-, as in
/fərˈkeːrd/ \> \[fəkiərt\] verkeerd ‘wrong’. In word-final position, as
in /lɛkər/ \> \[lækə\] lekker ‘nice’, /r/-deletion is quite common
across the board, especially in function words like daar, maar, hier,
vir: /daːr/ \> \[daː\] daar ‘there’, /maːr/ \> \[maː\]7 maar ‘but’,
/foːr/ \> \[fuə\] voor ‘in front of’, and /fər/ \> \[fə\] vir8 ‘for’.
This phenomena is widely present in Malayan Afrikaans. Kotz ́e (1983)
mentions similar examples, but adds to these the loss of /r/, even in
complex codas, e.g. \[fəket\] verkeerd ‘wrong’, \[bɔːs\] bors ‘breast’,
and \[kənəs\] kinders ‘children’ –transcriptions are Kotz ́e’s.

``` r
show_lang('Afrikaans')$Form
```

    ## [1] "rowwe"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Afrikaans","trilled",revision))
```

## Belarusian

``` r
show_lang('Belarusian')
```

    ## # A tibble: 2 × 6
    ##   Language   Dataset Meaning Form   Trill R_type
    ##   <chr>      <chr>   <chr>   <chr>  <chr> <chr> 
    ## 1 Belarusian Google  rough   грубы  yes   <NA>  
    ## 2 Belarusian Google  smooth  гладкі yes   <NA>

``` r
show_comments('Belarusian')
```

    ## # A tibble: 1 × 3
    ##   Language   Dataset Comments          
    ##   <chr>      <chr>   <chr>             
    ## 1 Belarusian Google  based on wikipedia

``` r
show_iso('Belarusian')
```

    ## # A tibble: 1 × 3
    ##   Language   ISO_code Phoible_code
    ##   <chr>      <chr>    <chr>       
    ## 1 Belarusian bel      bel

Trill : yes  
R_type : NA

Dataset :  
Google, based on wikipedia

Phoible : NA

Wikipedia :  
<https://en.wikipedia.org/wiki/Belarusian_language>  
<https://en.wikipedia.org/wiki/Belarusian_phonology>  
- Trill r

Mayo, Peter (2002), “Belorussian”, in Comrie, Bernard; Corbett, G. G.
(eds.), The Slavonic Languages, London: Routledge, pp. 887–946, ISBN
0-415-28078-8

Glottologue :  
<https://glottolog.org/resource/languoid/id/bela1254>

Autres sources :

Bird, S., & Litvin, N. (2021). Belarusian. Journal of the International
Phonetic Association, 51(3), 450-467. <doi:10.1017/S0025100319000288>  
- Page 453 : Trill r  
Présence de trills dans les audios joints à l’article

<https://clics.clld.org/languages/northeuralex-bel>  
Recherches : r -\> 306 entries (filtered from 1,013 total entries)  
l -\> 226 entries (filtered from 1,013 total entries)

``` r
show_lang('Belarusian')$Form
```

    ## [1] "грубы"  "гладкі"

грубы -\> gruby  
гладкі -\> gladkі

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Belarusian",'trilled',revision))
```

## Bulgarian

``` r
show_lang('Bulgarian')
```

    ## # A tibble: 5 × 6
    ##   Language  Dataset Meaning Form   Trill R_type
    ##   <chr>     <chr>   <chr>   <chr>  <chr> <chr> 
    ## 1 Bulgarian Google  rough   груб   yes   trill 
    ## 2 Bulgarian Google  smooth  гладък yes   trill 
    ## 3 Bulgarian CLICS   smooth  gládək yes   trill 
    ## 4 Bulgarian CLICS   rough   grápav yes   trill 
    ## 5 Bulgarian CLICS   rough   grúb   yes   trill

``` r
show_comments('Bulgarian')
```

    ## # A tibble: 2 × 3
    ##   Language  Dataset Comments
    ##   <chr>     <chr>   <chr>   
    ## 1 Bulgarian IDS     <NA>    
    ## 2 Bulgarian Google  <NA>

``` r
show_iso('Bulgarian')
```

    ## # A tibble: 1 × 3
    ##   Language  ISO_code Phoible_code
    ##   <chr>     <chr>    <chr>       
    ## 1 Bulgarian bul      bul

Trill : yes  
R_type : trill

Dataset :  
IDS, NA  
Google, NA

Phoible :  
<https://phoible.org/languages/bulg1262>  
4 inventaires  
- SPA r allophone r; rʲ allophone rʲ  
- UPSID r pas d’allophone; rʲ pas d’allophone  
- UZ r̟ allophone r̟  
- EA r pas d’allophone; rʲ pas d’allophone

Klagstad, H. 1958. The Phonemic System of Colloquial Standard Bulgarian.
Slavic and East European Journal 16. 42–54.  
Aronson, H. I. 1968. Bulgarian Inflectional Morphophonology. The Hague:
Mouton.  
Bidwell, C. 1968. The Stress Patterns of the Noun in Bulgarian. Studies
in Linguistics 20. 41–47.  
Scatton, Ernest A. 1984. A Reference Grammar of Modern Bulgarian.
Columbus, Ohio: Slavica Publishers.  
Бояджиев, Тодор Апостолов and Куцаров, Иван and Йордан, Пенчев. 1999.
Съвременен български език: фонетика, лексикология, словообразуване,
морфология, синтаксис: учебник за студенти от филологическите факултети
и педагогическите институти. Петър Берон.

Ternes, Elmar and Vladimirova-Buhtz, Tatjana. 1990. Illustrations of the
IPA: Bulgarian. Journal of the International Phonetic Association 20.
45–47. Cambridge University Press.  
- Page 45 : Trill r  
r roza ‘rose’  
- Page 46 : An alternative analysis postulates the following palatalized
consonants: /… rʲ …/  
/r/ is a trill.

Wikipedia :  
<https://en.wikipedia.org/wiki/Bulgarian_language>  
<https://en.wikipedia.org/wiki/Bulgarian_phonology>  
- Trill r rʲ

Glottologue :  
<https://glottolog.org/resource/languoid/id/bulg1262>

Autres sources :  
<https://clics.clld.org/languages/diacl-37300> Recherches : r -\> 70
entries (filtered from 255 total entries)  
l -\> 68 entries (filtered from 255 total entries)

<https://clics.clld.org/languages/ids-200>  
Recherches : r -\> 855 entries (filtered from 2,346 total entries)  
l -\> 602 entries (filtered from 2,346 total entries)

<https://clics.clld.org/languages/northeuralex-bul>  
Recherches : r -\> 389 entries (filtered from 1,115 total entries)  
l -\> 228 entries (filtered from 1,115 total entries)

``` r
show_lang('Bulgarian')$Form
```

    ## [1] "груб"   "гладък" "gládək" "grápav" "grúb"

груб -\> grub  
гладък -\> gladʺk  
gládək -\> gládək  
grápav -\> grápav  
grúb -\> grúb

<https://youtu.be/87qsKNoyx5A?t=117>  
On a des mots qui peuvent commencer par un r.

``` r
rough_r_data <- rough_r_data %>%   dplyr::mutate(revision = ifelse(Language=="Bulgarian",'trilled',revision))
```

## Bosnian

``` r
show_lang('Bosnian')
```

    ## # A tibble: 1 × 6
    ##   Language Dataset Meaning Form   Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>  <chr> <chr> 
    ## 1 Bosnian  Google  smooth  glatko yes   <NA>

``` r
show_comments('Bosnian')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments          
    ##   <chr>    <chr>   <chr>             
    ## 1 Bosnian  Google  based on wikipedia

``` r
show_iso('Bosnian')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Bosnian  bos      bos

``` r
# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Bosnian', Dataset='Google_trad',Meaning='rough',Form='grubo', rough=TRUE, r=TRUE,l=FALSE)
```

Trill : yes  
R_type : NA

Dataset :  
Google, based on wikipedia

Phoible : NA

Wikipedia :  
<https://en.wikipedia.org/wiki/Bosnian_language>  
- Р р R r /ɾ/

<https://es.wikipedia.org/wiki/Idioma_bosnio> - r р \[r\]

Glottologue :  
<https://glottolog.org/resource/languoid/id/bosn1245>

Autres sources :

Pas trouvé de descriptions phonétiques autre que Wikipédia non publié.  
2 audios à partir de <http://ilanguages.org/fr/bosnian_grammar.php>
regardés sur Praat : réalisations tappées

Le mot pour rough d’après Google Traduction est grubo
(<https://translate.google.com/?hl=fr&sl=en&tl=bs&text=rough&op=translate>),
donc vraisemblablement un tap en onset complex.

``` r
show_lang('Bosnian')$Form
```

    ## [1] "glatko"

On prend la décision après réflexion de dire que c’est un tap.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Bosnian",'other',revision))
```

## Catalan

``` r
show_lang('Catalan')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form  Trill R_type          
    ##   <chr>    <chr>   <chr>   <chr> <chr> <chr>           
    ## 1 Catalan  Google  rough   rugós yes   mixed with trill
    ## 2 Catalan  CLICS   smooth  llis  yes   mixed with trill
    ## 3 Catalan  CLICS   rough   aspre yes   mixed with trill

``` r
show_comments('Catalan')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Catalan  IDS     <NA>    
    ## 2 Catalan  Google  <NA>

``` r
show_iso('Catalan')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Catalan  cat      cat

Trill : yes  
R_type : mixed with trill

Dataset :  
Google, NA  
IDS, NA

Phoible :  
<https://phoible.org/languages/stan1289>  
4 inventaires  
- PH r allophone r; ɾ allophone ɾ  
- EA2425 r̺ pas d’allophone; ɾ̺ pas d’allophone - EA2555 r̺ pas
d’allophone; ɾ̺ pas d’allophone - EA2594 r̺ pas d’allophone; ɾ̺ pas
d’allophone

Carbonell, Joan F. and Llisterri, Joaquim. 1992. Illustrations of the
IPA: Catalan. Journal of the International Phonetic Association 22.
53–56. Cambridge University Press.  
- Page 53 : Trill r  
Tap or Flap ɾ

              r serra 'saw' (n.)  
              ɾ cera 'wax'  

Lloret, Maria-Rosa. 2011. La fonologia del catal\`a. Santillana, Grup
Promotor.

Wikipedia :  
<https://en.wikipedia.org/wiki/Catalan_language>  
- Tap ɾ  
- Trill r  
- The distribution of the two rhotics /r/ and /ɾ/ closely parallels that
of Spanish. Between vowels, the two contrast, but they are otherwise in
complementary distribution: in the onset of the first syllable in a
word, \[r\] appears unless preceded by a consonant. Dialects vary in
regards to rhotics in the coda with Western Catalan generally featuring
\[ɾ\] and Central Catalan dialects featuring a weakly trilled \[r\]
unless it precedes a vowel-initial word in the same prosodic unit, in
which case \[ɾ\] appears.\[104\]

Padgett, Jaye (December 2009). “Systemic Contrast and Catalan Rhotics”.
The Linguistic Review. 26 (4): 431–. <doi:10.1515/tlir.2009.016>. S2CID
15197551

Glottologue :  
<https://glottolog.org/resource/languoid/id/stan1289>

Autres sources :

<https://clics.clld.org/languages/diacl-53200>  
Recherches : r -\> 97 entries (filtered from 233 total entries)  
l -\> 81 entries (filtered from 233 total entries)

<https://clics.clld.org/languages/ids-177>  
Recherches : r -\> 1,542 total entries)  
l -\> 479 entries (filtered from 1,542 total entries)

<https://clics.clld.org/languages/northeuralex-cat>  
Recherches : r -\> 412 entries (filtered from 1,059 total entries)  
l -\> 174 entries (filtered from 1,059 total entries)  
Form in source:  
r -\> 64 entries (filtered from 1,059 total entries)  
ɾ -\> 364 entries (filtered from 1,059 total entries)

``` r
show_lang('Catalan')$Form
```

    ## [1] "rugós" "llis"  "aspre"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Catalan",'OUT',revision))
```

## Czech

``` r
show_lang('Czech')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form   Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>  <chr> <chr> 
    ## 1 Czech    Google  smooth  hladký yes   trill 
    ## 2 Czech    CLICS   smooth  hladký yes   trill 
    ## 3 Czech    CLICS   rough   drsný  yes   trill

``` r
show_comments('Czech')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Czech    IDS     <NA>    
    ## 2 Czech    Google  <NA>

``` r
show_iso('Czech')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Czech    ces      ces

Trill : yes  
R_type : trill

Dataset :  
IDS, NA  
Google, NA

Phoible :  
<https://phoible.org/languages/czec1258>  
3 inventaires  
- PH r̝ allophone r̝; r allophone r r̩  
- EA2274 r pas d’allophone; r̝ pas d’allophone  
- EA2480 r pas d’allophone; r̝ pas d’allophone

Dankovičová, Jana. 1997. Illustrations of the IPA: Czech. Journal of the
International Phonetic Association 27. 77–80. Cambridge University
Press.  
- Page 77 : Trill r  
r̝

There are two trills, /r/, as in ruka /ruka/ ‘hand’, and /r̝/, as in řeka
/r̝eka/ ‘river’. The first one is an alveolar apical trill with 1-3
periods of vibration. It is immune from voicing assimilation and occurs
voiced in all positions in the word. In case of /r̝/, the place of
articulation is normally similar to that for /r/. Although it may be
produced with the blade of the tongue, the main differentiating
characteristic from /r/ is the number of vibrations, which may be 1-2
greater than in /r/, and their lesser amplitude than for the vibrations
in Irl. Also, the constriction is narrower and the velocity of air
greater. This sound often starts as a trill but continues as a fricative
and thus probably the best term for it is ‘alveolar trill fricative’
with the symbol /r̝/. (The laminal diacritic /r̻/ used in Ladefoged &
Maddieson (1996) does not capture the sound’s defining property). A
voiceless allophone of /r̝/ occurs in places where voicing assimilation
applies.

Šimáčková, Šárka and Jonáš Podlipský, Václav. 2012. Illustrations of the
IPA: Czech spoken in Bohemia and Moravia. Journal of the International
Phonetic Association 42. 225–232. Cambridge University Press.  
- Page 226 : Trill r  
r̝  
- Page 227 : Intervocalically, /d/ is sometimes reduced to \[R\]
(Machaˇc & Skarnitzl 2009: 37–38)  
Both /r/ and /r̝/ are trills though commonly realized with a single
contact. Phonetically, the sound /r̝/ is a period of friction interrupted
at the beginning by a contact or contacts created by a retracted
apico-alveolar gesture (see Figure 1).  
Sonorants /r/, /l/, and marginally also /m/ and /n/, become syllabic
between two consonants or after a consonant at the end of a word

Wikipedia :  
<https://en.wikipedia.org/wiki/Czech_language>  
- Trill plain r ⟨r⟩  
- Trill fricative r̝ ⟨ř⟩  
- The phoneme represented by the letter ř (capital Ř) is very rare among
languages and often claimed to be unique to Czech, though it also occurs
in some dialects of Kashubian, and formerly occurred in Polish.\[39\] It
represents the raised alveolar non-sonorant trill (IPA: \[r̝\]), a sound
somewhere between Czech r and ž (example: “řeka” (river)
(help·info)),\[40\] and is present in Dvořák. In unvoiced environments,
/r̝/ is realized as its voiceless allophone \[r̝̊\], a sound somewhere
between Czech r and š.\[41\]  
- The consonants /r/, /l/, and /m/ can be syllabic, acting as syllable
nuclei in place of a vowel. Strč prst skrz krk (“Stick \[your\] finger
through \[your\] throat”) is a well-known Czech tongue twister using
syllabic consonants but no vowels.\[42\]

Glottologue :  
<https://glottolog.org/resource/languoid/id/czec1258>

Autres sources :  
<https://clics.clld.org/languages/diacl-37700> Recherches : r -\> 86
entries (filtered from 290 total entries)  
l -\> 73 entries (filtered from 290 total entries)  
Form in source:  
r -\> 74 entries (filtered from 290 total entries  
ř -\> 6 entries (filtered from 290 total entries)

<https://clics.clld.org/languages/ids-202>  
Recherches : r -\> 539 entries (filtered from 1,804 total entries)  
l -\> 502 entries (filtered from 1,804 total entries)  
Form in source:  
r -\> 539 entries (filtered from 1,804 total entries)  
ř -\> 171 entries (filtered from 1,804 total entries)

<https://clics.clld.org/languages/northeuralex-ces>  
Recherches : r -\> 320 entries (filtered from 1,060 total entries)  
l -\> 257 entries (filtered from 1,060 total entries)  
Form in source:  
r -\> 320 entries (filtered from 1,060 total entries)  
r̝ -\> 96 entries (filtered from 1,060 total entries)

``` r
show_lang('Czech')$Form
```

    ## [1] "hladký" "hladký" "drsný"

Pas d’impossibilité à triller, donc on prend la décision de mettre
trilled.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Czech",'trilled',revision))
```

## Welsh

``` r
show_lang('Welsh')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form  Trill R_type
    ##   <chr>    <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Welsh    Google  rough   garw  yes   <NA>  
    ## 2 Welsh    Google  smooth  llyfn yes   <NA>

``` r
show_comments('Welsh')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments          
    ##   <chr>    <chr>   <chr>             
    ## 1 Welsh    IDS     based on wikipedia
    ## 2 Welsh    Google  based on wikipedia

``` r
show_iso('Welsh')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Welsh    cym      cym

Trill : yes  
R_type : NA

Dataset :  
IDS, based on wikipedia  
Google, based on wikipedia

Phoible :  
<https://phoible.org/inventories/view/2406>  
- r pas d’allophone; r̥ pas d’allophone

Wikipedia :  
<https://en.wikipedia.org/wiki/Welsh_language>  
- Trill r̥ r

<https://en.wikipedia.org/wiki/Welsh_phonology>  
- Trill r̥ r

Martin J. Ball (1984), “Phonetics for phonology”, in M. J. Ball; G. E.
Jones (eds.), Welsh Phonology: Selected Readings, Cardiff: University of
Wales Press, pp. 5–39, ISBN 0-7083-0861-9  
Gareth King (1996), “Sounds and Spelling”, Modern Welsh, A Comprehensive
Grammar, London: Routledge, pp. 3–15, ISBN 978-1-138-82630-4  
John Jones (1913), “Phonology”, A Welsh Grammar, Historical and
Comparative, Oxford: Oxford University Press, pp. 9–188, pibn
1000706503  
Stephen J. Williams (1980), “Phonology”, A Welsh Grammar, Cardiff:
University of Wales Press, pp. 1–5, ISBN 0-7083-0737-X  
Zirui Liu (2018), “1.2 Background on the Welsh language”, Phonetics of
Southern Welsh Stress, London: University College London, p. 5  
S. J. Hannahs (2013), “A Survey of Welsh Phonetics”, The Phonology of
Welsh, Oxford: Oxford University Press, pp. 21–22, ISBN
978-0-19-960123-3

Glottologue :  
<https://glottolog.org/resource/languoid/id/wels1247>

Autres sources :

Bell, E., Archangeli, D., Anderson, S., Hammond, M., Webb-Davies, P., &
Brooks, H. (2021). Northern Welsh. Journal of the International Phonetic
Association, 1-24. <doi:10.1017/S0025100321000165>  
- Page 3 : Trill r̥ r  
- Page 4 : /r/ reːns rêns reins\*  
/r̥/ r̝̊heːn rhên ruler  
- Page 7 : Trills  
Welsh has two alveolar trills, voiced /r/ r and voiceless /r̥/ rh. The
voiced trill may appear in any position in the word, but the voiceless
trill is confined to initial or pre-tonic syllables. In intervocalic
position, the voiced trill may be realized as an alveolar tap \[ɾ\], as
in /ˈɐrɐv/ \[ʔɐɾɐv̥\] araf ‘slow’. In clusters, /r/ can also appear as
alveolar approximant \[ɹ\] (G. E. Jones Reference Jones1984: 49–50).
Morris (Reference Morris2013) observed that /r/ was produced as the
alveolar approximant \[ɹ\] by younger speakers of Northern Welsh,
particularly in casual contexts.  
The initial portion of the voiceless trill /r̥/ may be produced as a
voiceless trill or fricative, while the latter portion is the glottal
\[h\] (Figure 6, right). As has been observed for other languages (see
Solé Reference Solé2002), the Northern Welsh voiceless trill is
acoustically similar to a fricative, with spectral energy in the 2–4 kHz
range. The voiceless trill alternates with the voiced trill when
preceded by a trigger of soft mutation (see ‘Mutation’ section). Figure
6 illustrates the difference between the voiced trill (left), which
shows brief period of rapid tongue-tip opening and closure, and the
voiceless trill (right), which shows only frication.

<https://clics.clld.org/languages/diacl-39000> Recherches : r -\> 83
entries (filtered from 277 total entries) l -\> 72 entries (filtered
from 277 total entries) Form in source:  
rh -\> 7 entries (filtered from 277 total entries) r -\> 83 entries
(filtered from 277 total entries)

<https://clics.clld.org/languages/ids-182> Recherches : r -\> 833
entries (filtered from 2,122 total entries) l -\> 659 entries (filtered
from 2,122 total entries) Form in source:  
r -\> 833 entries (filtered from 2,122 total entries) rh -\> 57 entries
(filtered from 2,122 total entries)

<https://clics.clld.org/languages/northeuralex-cym> Recherches : r -\>
401 entries (filtered from 1,100 total entries) l -\> 307 entries
(filtered from 1,100 total entries) Form in source:  
r -\> 401 entries (filtered from 1,100 total entries) r̥ -\> 33 entries
(filtered from 1,100 total entries)

``` r
show_lang('Welsh')$Form
```

    ## [1] "garw"  "llyfn"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Welsh",'trilled',revision))
```

## Danish

``` r
show_lang('Danish')
```

    ## # A tibble: 4 × 6
    ##   Language Dataset Meaning Form  Trill R_type
    ##   <chr>    <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Danish   Google  rough   ru    no    <NA>  
    ## 2 Danish   Google  smooth  glat  no    <NA>  
    ## 3 Danish   CLICS   smooth  jævn  no    <NA>  
    ## 4 Danish   CLICS   rough   ujævn no    <NA>

``` r
show_comments('Danish')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments                               
    ##   <chr>    <chr>   <chr>                                  
    ## 1 Danish   IDS     voiced uvular fricative; says Wikipedia
    ## 2 Danish   Google  voiced uvular fricative; says Wikipedia

``` r
show_iso('Danish')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Danish   dan      dan

Trill : no  
R_type : NA

Dataset :  
IDS, voiced uvular fricative; says Wikipedia  
Google, voiced uvular fricative; says Wikipedia

Phoible :  
<https://phoible.org/languages/dani1285>  
2 inventaires  
- UZ r (marginal) allophones r ɐ̯ ʌ̯; ʁ̞ allophones ʁ̞  
- EA ʁ pas d’allophone

Grønnum, Nina. 1998. Illustrations of the IPA: Danish. Journal of the
International Phonetic Association 28. 99–105. Cambridge University
Press.  
- Page 99 : Approximant ʁ  
- Page 100 : Add to this the non-syllabic manifestation of /r/ in
post-vocalic position, transcribed either \[ɐ̯\] or (as in this paper)
\[ʌ̯\].  
Note that in a narrower transcription \[…\] since they are in fact
lenis. \[…\] And \[ð\] and \[ʁ\] would be \[ð̞\] and \[ʁ̞\], respectively,
for the same reason.

Wikipedia :  
<https://en.wikipedia.org/wiki/Danish_language>  
- Approximant ʁ  
- In onset /r/ is realized as a uvu-pharyngeal approximant, \[ʁ\], but
in coda it is either realized as a non-syllabic low central vowel, \[ɐ̯\]
or simply coalesces with the preceding vowel. The phenomenon is
comparable to the r in German or in non-rhotic pronunciations of
English. The Danish realization of /r/ as guttural – the so-called
skarre-r – distinguishes the language from those varieties of Norwegian
and Swedish that use trilled \[r\]. Only very few, middle-aged or
elderly, speakers of Jutlandic retain a frontal /r/ which is then
usually realised as a flapped \[ɾ\] or approximant \[ɹ\].

<https://en.wikipedia.org/wiki/Danish_phonology>  
- Syllable-initially, /r/ is a voiced uvular fricative \[ʁ\] or, more
commonly, an approximant \[ʁ̞\].\[25\] According to Nina Grønnum, the
fricative variant is voiceless \[χ\].\[26\] Its precise place of
articulation has been described as pharyngeal,\[27\] or more broadly, as
“supra-pharyngeal”.\[28\] When emphasizing a word, word-initial /r/ may
be realized as a voiced uvular fricative trill \[ʀ̝\].\[16\] In
syllable-final position, /r/ is realized as \[ɐ̯\].\[2\]  
The alveolar realization \[r\] of /r/ is very rare. According to Torp
(2001), it occurs in some varieties of Jutlandic dialect, and only for
some speakers (mostly the elderly). The alveolar realization is
considered non-standard, even in classical opera singing – it is
probably the only European language in which this is the case.\[29\]
According to Basbøll (2005), it occurs (or used to occur until recently)
in very old forms of certain conservative dialects in Northern Jutland
and Bornholm.\[30\]

Glottologue :  
<https://glottolog.org/resource/languoid/id/dani1285>

Autres sources :

<https://clics.clld.org/languages/diacl-40300>  
Recherches : r -\> 80 entries (filtered from 271 total entries)  
l -\> 76 entries (filtered from 271 total entries)

<https://clics.clld.org/languages/ids-186>  
Recherches : r -\> 589 entries (filtered from 1,559 total entries)  
l -\> 492 entries (filtered from 1,559 total entries)

<https://clics.clld.org/languages/northeuralex-dan>  
Recherches : r -\> 312 entries (filtered from 1,019 total entries)  
l -\> 265 entries (filtered from 1,019 total entries)  
Form in source:  
ʀ -\> 4175 entries (filtered from 1,019 total entries)  
ʁ -\> 125 entries (filtered from 1,019 total entries)  
r -\> 13 entries (filtered from 1,019 total entries)

``` r
show_lang('Danish')$Form
```

    ## [1] "ru"    "glat"  "jævn"  "ujævn"

De la même manière que je prendrais la décision pour le français, je
vais mettre other.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Danish",'other',revision))
```

## German

``` r
show_lang('German')
```

    ## # A tibble: 5 × 6
    ##   Language Dataset Meaning Form   Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>  <chr> <chr> 
    ## 1 German   Google  rough   rau    yes   trill 
    ## 2 German   Google  smooth  glatt  yes   trill 
    ## 3 German   CLICS   smooth  eben   yes   trill 
    ## 4 German   CLICS   rough   rauh   yes   trill 
    ## 5 German   CLICS   rough   uneben yes   trill

``` r
show_comments('German')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 German   IDS     R       
    ## 2 German   Google  R

``` r
show_iso('German')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 German   deu      deu

Trill : yes  
R_type : trill

Dataset :  
IDS, R  
Google, R

Phoible :  
<https://phoible.org/languages/stan1295>  
4 inventaires  
- SPA : ʀ allophones ʌ ʌ̯ ʀ r  
- UPSID : ʀ pas d’allophone  
- UZ : ʁ pas d’allophone ʁ ʔ ʁ̥ χ ɐ  
- EA : ʀ pas d’allophone

Moulton, William G. 1962. The Sounds of English and German. Chicago:
University of Chicago Press.  
Werner, Otmar. 1972. Phonemik des Deutschen. (Sammlung Metzler, 108.)
Stuttgart: J.B. Metzler Verlagsbuchhandlung.  
Philipp, Marthe. 1974. Phonologie des Deutschen. Stuttgart: W.
Kohlhammer.

Kohler, Klaus. 1990. Illustrations of the IPA: German. Journal of the
International Phonetic Association 20. 48–50. Cambridge University
Press.  
- Page 48 : Fricative ʁ ʁ Rasse ‘race’ - Page 49 : /ʁ/ can be an
approximant intervocalically (e.g. Herren ˈgentlemenˈ); after voiceless
plosives and fricatives, especially those within the same word, it is
devoiced (in e.g. trat ˈkickedˈ it is completely voiceless \[χ\]\]);
postvocalically before a consonant or word-final it is vocalized to
\[ɐ\], which results in diphthongs (e.g. \[ˈhaɐt\] hart ˈhardˈ, \[ˈoɐ\]
Ohr ˈearˈ, see the vowel chart below); the ending -er is realised as
\[ɐ\] (e.g. \[ˈbʊtɐ\] Butter ˈbutterˈ); the place of articulation of the
consonant varies from uvular in e.g. rot ˈredˈ to velar in e.g. treten
ˈkickˈ, depending on back or front vowel contexts./ can be an
approximant intervocalically (e.g. Herren ˈgentlemenˈ); after voiceless
plosives and fricatives, especially those within the same word, it is
devoiced (in e.g. trat ˈkickedˈ it is completely voiceless ft\]);
postvocalically before a consonant or word-final it is vocalized to
\[B\], which results in diphthongs (e.g. \[ˈham\] hart ˈhardˈ, \[ˈoe\]
Ohr ˈearˈ, see the vowel chart below); the ending -er is realised as
\[B\] (e.g. \[ˈbutB\] Butter ˈbutterˈ); the place of articulation of the
consonant varies from uvular in e.g. rot ˈredˈ to velar in e.g. treten
ˈkickˈ, depending on back or front vowel contexts.

Wikipedia :  
<https://en.wikipedia.org/wiki/German_language> - Liquid r

<https://en.wikipedia.org/wiki/Standard_German_phonology> - Liquid r  
- /r/ can be uvular, alveolar or even dental, a consonant or a
semivowel, see below. - /r/ has a number of possible realizations:
Voiced apical coronal trill \[r̺\],\[58\]\[59\]\[60\] either alveolar
(articulated with the tip of the tongue against the alveolar
ridge),\[58\]\[59\]\[60\] or dental (articulated with the tip of the
tongue against the back of the upper front teeth).\[58\] Distribution:
Common in the south (Bavaria and many parts of Switzerland and Austria),
but it is also found in some speakers in central and northern Germany,
especially the elderly. It is also one of possible realizations of /r/
in the Standard Austrian accent, but a more common alveolar realization
is an approximant \[ɹ\]. Even more common are uvular realizations,
fricatives \[ʁ \~ χ\] and a trill \[ʀ\].\[61\] Voiced uvular trill
\[ʀ\],\[58\]\[59\]\[62\]\[63\] which can be realized as voiceless \[ʀ̥\]
after voiceless consonants (as in treten).\[59\] According to Lodge
(2009) it is often a flap \[ʀ̆\] intervocalically (as in Ehre).\[64\]
Distribution: Occurs in some conservative varieties - most speakers with
a uvular /r/ realize it as a fricative or an approximant.\[65\] It is
also one of possible realizations of /r/ in the Standard Austrian
accent, but it is less common than a fricative \[ʁ \~ χ\].\[61\] Dorsal
continuant, about the quality of which there is not a complete
agreement: Krech et al. (2009) describe two fricative variants, namely
post-palatal \[ɣ˖\] and velar \[ɣ\]. The post-palatal variant appears
before and after front vowels, while the velar variant is used in all
other positions.\[66\] Morciniec & Prędota (2005) describe it as voiced
post-velar fricative \[ʁ̟\].\[67\] Mangold (2005) and Kohler (1999)
describe it as voiced uvular fricative \[ʁ\];\[58\]\[68\] Mangold (2005)
states that “with educated professional radio and TV announcers, as with
professional actors on the stage and in film, the \[voiced uvular\]
fricative \[realization of\] /r/ clearly predominates.”\[58\] In the
Standard Austrian accent, the uvular fricative is also the most common
realization, although its voicing is variable (that is, it can be either
voiced \[ʁ\] or voiceless \[χ\]).\[61\] Kohler (1999) writes that “the
place of articulation of the consonant varies from uvular in e.g. rot
(‘red’) to velar in e.g. treten (‘kick’), depending on back or front
vowel contexts.” He also notes that \[ʁ\] is devoiced after voiceless
plosives and fricatives, especially those within the same word, giving
the word treten as an example. According to this author, \[ʁ\] can be
reduced to an approximant in an intervocalic position.\[69\] Ladefoged &
Maddieson (1996) describe it as a uvular fricative \[ʁ\] or approximant
\[ʁ̞\]. The latter is less likely to occur word-initially.\[70\]
Distribution: Almost all areas apart from Bavaria and parts of
Switzerland. Near-open central unrounded vowel \[ɐ\] is a post-vocalic
allophone of (mostly dorsal) varieties of /r/. The non-syllabic variant
of it is not always near-open or central; it is similar to either \[ɑ\]
or \[ə\], depending on the environment.\[67\] distribution: Widespread,
but less common in Switzerland.

Glottologue :  
<https://glottolog.org/resource/languoid/id/stan1295>

Autres sources :

<https://clics.clld.org/languages/diacl-41700> Recherches : r -\> 131
entries (filtered from 334 total entries) rr -\> 2 entries (filtered
from 334 total entries) l -\> 96 entries (filtered from 334 total
entries)

<https://clics.clld.org/languages/ids-194> Recherches : r -\> 772
entries (filtered from 1,898 total entries) rr -\> 16 entries (filtered
from 1,898 total entries) l -\> 574 entries (filtered from 1,898 total
entries)

<https://clics.clld.org/languages/northeuralex-deu> Recherches : r -\>
165 entries (filtered from 952 total entries) rr -\> 0 entries (filtered
from 952 total entries) l -\> 249 entries (filtered from 952 total
entries) Form in source:  
ʁ -\> 165 entries (filtered from 952 total entries) r -\> 0 entries
(filtered from 952 total entries)

``` r
show_lang('German')$Form
```

    ## [1] "rau"    "glatt"  "eben"   "rauh"   "uneben"

De la même manière que je prendrais la décision pour le français, je
vais mettre other.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="German",'other',revision))
```

## Greek

``` r
show_lang('Greek')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form   Trill R_type  
    ##   <chr>    <chr>   <chr>   <chr>  <chr> <chr>   
    ## 1 Greek    Google  rough   τραχύς no    no trill
    ## 2 Greek    Google  smooth  λείος  no    no trill

``` r
show_comments('Greek')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments                                                   
    ##   <chr>    <chr>   <chr>                                                      
    ## 1 Greek    Google  tap; wikipedia says it may be produced as trill in clusters

``` r
show_iso('Greek')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Greek    ell      ell

Trill : no  
R_type : no trill

Dataset :  
Google, tap; wikipedia says it may be produced as trill in clusters

Phoible :  
<https://phoible.org/languages/mode1248>  
5 inventaires:  
- SPA ɾ allophone ɾ  
- UPSID ɾ pas d’allophone  
- UZ 2186 rː allophone rː; ɾ allophones ɾ rː  
- UZ 2187 ɾ allophones ɾ ɹ  
- EA ɾ̠ pas d’allophone

Kaisse, Ellen. 1976. Stress Melodies and a Fast Speech Rule in Modern
Greek. NELS VI. 165–175.  
Newton, Brian. 1972. The Generative Interpretation of Dialect: A Study
of Modern Greek Phonology. (Cambridge Studies in Linguistics, 8.)
Cambridge: Cambridge University Press.  
Kaisse, Ellen. 1975. A Mor- or Less-pheme: A Case of Delexicalization in
Modern Greek. Paper delivered at the 1975 Winter Meeting of the LSA, San
Francisco.  
Householder, Fred W. and Kazasis, Kostas and Koutsoudas, Andreas. 1964.
A Reference Grammar of Literary Dhimotiki. Bloomington, Indiana: Indiana
University Press.  
Pring, J. T. 1967. A Grammar of Modern Greek on a Phonetic Basis.
London: Hodder & Stoughton.  
Arvaniti, Amalia. 2007. Greek phonetics: The state of the art. Journal
of Greek Linguistics 8. 97–208. Citeseer.

Arvaniti, Amalia. 1999. Illustrations of the IPA: Standard Modern Greek.
Journal of the International Phonetic Association 29. 167–172/1–6.
Cambridge University Press.  
- Page 167 : Tap ɾ  
- Page 168-169 : Rhotics. Greek has one rhotic sound. This is pronounced
as an alveolar tap \[ɾ\] word-initially and intervocalically, or when it
is followed by another consonant; in fast speech it may be pronounced as
an alveolar approximate In clusters In which it is preceded by a plosive
or a fricative it is pronounced as a short trill.

Wikipedia :  
<https://en.wikipedia.org/wiki/Modern_Greek>  
<https://en.wikipedia.org/wiki/Modern_Greek_phonology>  
Tap /ɾ/ ρ

The only Greek rhotic /r/ is prototypically an alveolar tap \[ɾ\], often
retracted (\[ɾ̠\]). It may be an alveolar approximant \[ɹ\]
intervocalically, and is usually a trill \[r\] in clusters, with two or
three short cycles.\[8\]  
\[8\] Arvaniti, Amalia (2007). “Greek Phonetics: The State of the Art”
(PDF). Journal of Greek Linguistics. 8: 97–208. CiteSeerX
10.1.1.692.1365. <doi:10.1075/jgl.8.08arv>. Archived from the original
(PDF) on 2013-12-11.

Glottologue :  
<https://glottolog.org/resource/languoid/id/mode1248>

Autres sources :

<https://clics.clld.org/languages/diacl-43200>  
Recherches : r -\> 91 entries (filtered from 220 total entries)  
l -\> 55 entries (filtered from 220 total entries)

<https://clics.clld.org/languages/ids-168>  
Recherches : r -\> 677 entries (filtered from 1,721 total entries)  
rr -\> 0 entries (filtered from 1,721 total entries)  
l -\> 439 entries (filtered from 1,721 total entries)

<https://clics.clld.org/languages/northeuralex-ell>  
Recherches : r -\> 352 entries (filtered from 1,077 total entries)  
rr -\> 4 entries (filtered from 1,077 total entries)  
l -\> 232 entries (filtered from 1,077 total entries)

``` r
show_lang('Greek')$Form
```

    ## [1] "τραχύς" "λείος"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Greek",'other',revision))
```

## Spanish

``` r
show_lang('Spanish')
```

    ## # A tibble: 4 × 6
    ##   Language Dataset Meaning Form   Trill R_type          
    ##   <chr>    <chr>   <chr>   <chr>  <chr> <chr>           
    ## 1 Spanish  Google  rough   áspero yes   mixed with trill
    ## 2 Spanish  CLICS   smooth  suave  yes   mixed with trill
    ## 3 Spanish  CLICS   smooth  liso   yes   mixed with trill
    ## 4 Spanish  CLICS   rough   áspero yes   mixed with trill

``` r
show_comments('Spanish')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Spanish  IDS     <NA>    
    ## 2 Spanish  Google  <NA>

``` r
show_iso('Spanish')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Spanish  spa      spa

Trill : yes  
R_type : mixed with trill

Dataset :  
IDS, NA  
Google, NA

Phoible :  
<https://phoible.org/languages/stan1288>  
5 inventaires:  
- SPA r allophones r z͇; ɾ allophones ɾ z͇̥  
- UPSID r pas d’allophone  
- UZ r allophones r ɾ; ɾ allophone ɾ  
- EA 2303 r pas d’allophone; ɾ pas d’allophone  
- EA 2308 ɾ pas d’allophone

Harris, James W. 1969. Spanish Phonology. Cambridge, Massachusetts: MIT
Press.  
Saporta, S. and Contreras, H. 1962. A Phonological Grammar of Spanish.
Seattle: University of Washington Press.  
Navarro, Tomás T. 1961. Manual de Pronunciacion Espanola, Consejo
Superior de Investigaciones Cientificas. Madrid: Publicaciones de la
Revista de Filologia Espanola III.  
Frías Conde, Xavier. 2001. Introducci’on a la fonética y fonolog’ia del
español. (4.) Ianua. Revista Philologica Romanica. 23pp.

Martínez-Celdrán, Eugenio and Fernández-Planas, Ana Ma. 2003.
Illustrations of the IPA: Castilian Spanish. Journal of the
International Phonetic Association 33. 255–259. Cambridge University
Press.  
- Page 255 : Tap or flap ɾ  
Trill r  
- Page 258 : The trill appears in word onset and after \[l\], \[n\] and
\[s\] and in intervocalic position where it contrasts with the tap:
caro-carro ‘expensive-car’ \[’kaɾo\]-\[’karo\]. In other contexts, the
tap is usual.

Monroy, Rafael and Hernandez-Campoy, Juan Manuel. 2015. Murcian Spanish.
Journal of the International Phonetic 45. 229–240.  
- Page 229 : Flap ɾ  
Trill r  
- Page 232 : Spirantization, however, is systematic when /s/ is replaced
by \[ɾ\], a phenomenon restricted socially to a working-class,
older-generation, rural type of speech \[…\] Although frowned upon by
speakers using a more refined variety, the replacement of /l/ by /ɾ/ in
coda position is another distinguishing characteristic of Murcian
speech, also found in other Southern Spanish accents (Andalusian,
Canario and Extremeno according to Garcıa-Mouton 1994: 35). \[…\]  
Linked to this is the specific behaviour of syllable coda /ɾ/ followed
by syllable onset /l/ acting as codas and heads of a syllable. One of
the most noticeable characteristics of Murcian is the full assimilation
of /ɾ/ to /l/ \[…\]  
- Page 233 : Gemination can then be a regular feature of MuSp. The
opposite phenomenon – the replacement of /l/ by /ɾ/ – is also common in
colloquial Murcian, e.g. est ́a en el r ́ıo \[ . . . ɛɾ-ˈr ⁄ıo\] ‘in the
river, alrededor \[æɾ-rɛð̞ɛˈð̞ɔ\] ‘around’, el ramo \[ . . . ɛɾ-ˈramo\]
‘the branch’. This may happen not only in cases where /ɾ/ follows: /l/
may become /ɾ/ in front of any single consonant with the exception of
/ɾ/ itself, so it is not uncommon to hear expressions like \[ɛɾ ˈsɔ\] el
sol ‘the sun’, \[ɛɾ ˈθumo\] elzumo ‘the juice’, \[ɛɾ ˈkæmpo\] el campo
‘the country’, \[ɛɾ ˈβ̞ jen̪to\] el viento ‘the wind’, \[ɛɾˈʧiko\] el
chico ‘the boy’, etc.

Wikipedia :  
<https://en.wikipedia.org/wiki/Spanish_language>  
- Flap ɾ - Trill r - The Spanish consonant system is characterized by
\[…\] a phonemic distinction between the “tapped” and “trilled” r-sounds
(single ⟨r⟩ and double ⟨rr⟩ in orthography).

Martínez-Celdrán, Eugenio; Fernández-Planas, Ana Ma.; Carrera-Sabaté,
Josefina (2003). “Castilian Spanish”. Journal of the International
Phonetic Association. 33 (2): 255–59. <doi:10.1017/S0025100303001373>.

<https://en.wikipedia.org/wiki/Spanish_phonology> - Flap ɾ - Trill r -
Rhotics The alveolar trill \[r\] and the alveolar tap \[ɾ\] are in
phonemic contrast word-internally between vowels (as in carro ‘car’ vs
caro ‘expensive’), but are otherwise in complementary distribution, as
long as syllable division is taken into account: the tap occurs after
any syllable-initial consonant, while the trill occurs after any
syllable-final consonant.\[47\]\[48\]

Only the trill can occur word-initially (e.g. el rey ‘the king’, la
reina ‘the queen’) or at the start of a word-internal syllable when the
preceding syllable ends with a consonant, which is always the case after
/l/, /n/, /s/ (e.g. alrededor, enriquecer, Israel).

Only the tap can occur after a word-initial obstruent consonant
(e.g. tres ‘three’, frío ‘cold’).

Either a trill or a tap can be found after the word-medial obstruent
consonants /b/, /d/, /t/, depending on whether the rhotic consonant is
pronounced in the same syllable as the preceding obstruent (forming a
complex onset cluster) or in a separate syllable (with the obstruent in
the coda of the preceding syllable). For example, the words subrayar,
ciudadrealeño, postromántico have a trill.\[49\] Each of these words has
a morpheme boundary coinciding with the syllable division before the
rhotic consonant; the trills correspond to the word-initial trills found
in raya ‘line’, Ciudad Real “Ciudad Real”, and romántico “Romantic”. The
tap is found in words where there is no syllable boundary between the
obstruent and the following rhotic consonant, such as sobre ‘over’,
madre ‘mother’, ministro ‘minister’.

In syllable-final position inside a word, the tap is more frequent, but
the trill can also occur (especially in emphatic\[50\] or
oratorical\[51\] style) with no semantic difference—thus arma (‘weapon’)
may be either \[ˈaɾma\] (tap) or \[ˈarma\] (trill).\[52\] In word-final
position the rhotic is usually:

- either a tap or a trill when followed by a consonant or a pause, as in
  amo\[ɾ \~ r\] paterno (‘paternal love’), the former being more
  common;\[53\]  
- a tap when followed by a vowel-initial word, as in amo\[ɾ\] eterno
  (‘eternal love’).

Morphologically, a word-final rhotic always corresponds to the tapped
\[ɾ\] in related words. Thus the word olor ‘smell’ is related to olores,
oloroso ‘smells, smelly’ and not to \*olorres, \*olorroso.\[35\]

When two rhotics occur consecutively across a word or prefix boundary,
they result in one trill, so that da rocas (‘s/he gives rocks’) and dar
rocas (‘to give rocks’) are either neutralized, or distinguished by a
longer trill in the latter phrase.\[54\]

The tap/trill alternation has prompted a number of authors to postulate
a single underlying rhotic; the intervocalic contrast then results from
gemination (e.g. tierra /ˈtieɾɾa/ \> \[ˈtjera\]
‘earth’).\[55\]\[56\]\[57\]

Glottologue :  
<https://glottolog.org/resource/languoid/id/stan1288>

Autres sources :

<https://clics.clld.org/languages/diacl-54800> Recherches : r -\> 136
entries (filtered from 312 total entries) rr -\> 14 entries (filtered
from 312 total entries) l -\> 88 entries (filtered from 312 total
entries) ll -\> 26 entries (filtered from 312 total entries)

<https://clics.clld.org/languages/ids-176> Recherches : r -\> 1,015
entries (filtered from 1,770 total entries) rr -\> 57 entries (filtered
from 1,770 total entries) ^r -\> 72 entries (filtered from 1,770 total
entries) r\$ -\> 461 entries (filtered from 1,770 total entries) l -\>
475 entries (filtered from 1,770 total entries) ll -\> 89 entries
(filtered from 1,770 total entries)

<https://clics.clld.org/languages/northeuralex-spa> Recherches : r -\>
656 entries (filtered from 1,124 total entries) rr -\> 1 entries
(filtered from 1,124 total entries) \[darresultado -\> daɾ ɾesultaðo\] l
-\> 211 entries (filtered from 1,124 total entries) Form in source: r
-\> 33 entries (filtered from 1,124 total entries) ɾ -\> 637 entries
(filtered from 1,124 total entries) l -\> 211 entries (filtered from
1,124 total entries)

``` r
show_lang('Spanish')$Form
```

    ## [1] "áspero" "suave"  "liso"   "áspero"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Spanish",'OUT',revision))
```

## Persian

``` r
show_lang('Persian')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form   Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>  <chr> <chr> 
    ## 1 Persian  Google  smooth  صاف    yes   trill 
    ## 2 Persian  CLICS   smooth  hæmvɑr yes   trill 
    ## 3 Persian  CLICS   rough   dorošt yes   trill

``` r
show_comments('Persian')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Persian  IDS     <NA>    
    ## 2 Persian  Google  <NA>

``` r
show_iso('Persian')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Persian  pes      pes

Trill : yes  
R_type : trill

Dataset : IDS, NA  
Google, NA

Phoible :  
<https://phoible.org/languages/west2369>  
4 inventaires :  
- SPA r allophones ɾ r r̥  
- USPID r pas d’allophone  
- UZ r allophones r ɾ ɹ  
- EA r̪ pas d’allophone

Obolensky, Serge and Panah, Kambiz Yazdan and Nouri, Fereidoun Khaje.
1963. Persian Basic Course. Washington, D.C.: Center for Applied
Linguistics.  
Windfuhr, Gernot L. 1997. Persian phonology. In Kaye, Alan S. and
Daniels, Peter T. (eds.), Phonologies of Asia and Africa, 675–690.
Winona Lake, Indiana: Eisenbrauns.

Majidi, Mohammad-Reza and Ternes, Elmar. 1991. Illustrations of the IPA:
Persian (Farsi). Journal of the International Phonetic Association 21.
96–98. Cambridge University Press.  
- Page 97 : Trill r  
- Page 98 : /r/ varies between \[ɾ\] and \[ɹ\]

Wikipedia :  
<https://en.wikipedia.org/wiki/Iranian_Persian>  
<https://en.wikipedia.org/wiki/Persian_phonology>  
- Trill r  
- Tap ɾ  
- The flap /ɾ/ has a trilled allophone \[r\] at the beginning of a
word;\[16\] otherwise, they contrast between vowels wherein a trill
occurs as a result of gemination (doubling) of \[ɾ\], especially in
loanwords of Arabic origin. Only \[ɾ\] occurs before and after
consonants; in word-final position, it is usually a free variation
between a flap or a trill when followed by a consonant or a pause, but
flap is more common, only flap before vowel-initial words. An
approximant \[ɹ\] also occurs as an allophone of /ɾ/ before /t, d, s, z,
ʃ, l, ʒ/; \[ɹ\] is sometimes in free variation with \[ɾ\] in these and
other positions, such that فارسی (‘Persian’) is pronounced \[fɒːɹˈsiː\]
or \[fɒːɾˈsiː\] and سقرلات (‘scarlet’) \[sæɣeɹˈlɒːt\] or \[sæɣeɾˈlɒːt\].
/r/ is sometimes realized as a long approximant \[ɹː\].

Mahootian, Shahrzad (1997). Persian. London: Routledge. pp. 287, 292,
303, 305. ISBN 0-415-02311-4.  
- Page 287 : Liquid r - Page 292 : Voiced nonsyllabic /r/ is made by
flapping the tip of the tongue forward against the alveolar ridge. There
is allophonic variation, with more trill at the beginning of a word than
in the middle or at the end. Both /l/ and /r/ occur word-initially and
word-finally, syllable-initially and syllable-finally as well as
morpheme-initially and morpheme-finally. Liquids also occur in
word-medial, syllablemedial and morpheme-medial positions

Glottologue :  
<https://glottolog.org/resource/languoid/id/west2369>

Autres sources :

<https://clics.clld.org/languages/diacl-52100>  
Recherches : r -\> 56 entries (filtered from 174 total entries)  
rr -\> 1 entries (filtered from 174 total entries)  
l -\> 10 entries (filtered from 174 total entries)  
ll -\> 1 entries (filtered from 174 total entries)

<https://clics.clld.org/languages/ids-214> Recherches : r -\> 659
entries (filtered from 1,606 total entries)  
rr -\> 8 entries (filtered from 1,606 total entries)  
l -\> 190 entries (filtered from 1,606 total entries)  
ll -\> 8 entries (filtered from 1,606 total entries)

``` r
show_lang('Persian')$Form
```

    ## [1] "صاف"    "hæmvɑr" "dorošt"

Je ne suis pas sûr de si je dois exclure la langue. Mais si je me base
exclusivement sur l’Illustration alors je garde trill.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Persian",'trilled',revision))
```

## French

``` r
show_lang('French')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form    Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>   <chr> <chr> 
    ## 1 French   Google  rough   rugueux yes   trill 
    ## 2 French   Google  smooth  lisse   yes   trill 
    ## 3 French   CLICS   rough   rude    yes   trill

``` r
show_comments('French')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 French   IDS     <NA>    
    ## 2 French   Google  <NA>

``` r
show_iso('French')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 French   fra      fra

Trill : yes  
R_type : trill

Dataset :  
IDS, NA  
Google, NA

Phoible :  
<https://phoible.org/languages/stan1290>  
4 inventaires :  
- SPA r allophones r̥ r; ʀ allophones ʀ ʁ̞ ʀ̥  
- UPSID ʀ pas d’allophone  
- UZ ʁ allophones ʁ ʁ̞ ʁ̥ ʀ r  
- EA ʁ pas d’allophone

Sten, H. 1963. Manuel de Phonetique Francaise. Copenhagen: Munksgaard.

Fougeron, Cécile and Smith, Caroline L. 1993. Illustrations of the IPA:
French. Journal of the International Phonetic Association 23. 73–76.
Cambridge University Press. - Page 74 : Fricative ʁ  
French has one rhotic, whose production varies considerably among
speakers and phonetic contexts. The speaker presented here uses a uvular
fricative \[ʁ\] that is sometimes reduced to an approximant \[ʁ̞\],
particularly in final position; it may also be devoiced (for examples
see the transcribed text), and can be reduced to zero in some word-final
positions. For other speakers, a uvular trill \[ʀ\] is also fairly
common, and an apical trill \[r\] occurs in some dialects. Vowels are
often lengthened before this segment.

Wikipedia :  
<https://en.wikipedia.org/wiki/French_language>  
- Fricative ʁ  
French has one rhotic whose pronunciation varies considerably among
speakers and phonetic contexts. In general, it is described as a voiced
uvular fricative, as in \[ʁu\] roue, “wheel”. Vowels are often
lengthened before this segment. It can be reduced to an approximant,
particularly in final position (e.g., fort), or reduced to zero in some
word-final positions. For other speakers, a uvular trill is also common,
and an apical trill \[r\] occurs in some dialects.

Glottologue :  
<https://glottolog.org/resource/languoid/id/stan1290>

Autres sources :

<https://clics.clld.org/languages/diacl-53300>  
Recherches : r -\> 142 entries (filtered from 321 total entries)  
rr -\> 11 entries (filtered from 321 total entries)  
l -\> 87 entries (filtered from 321 total entries)  
ll -\> 15 entries (filtered from 321 total entries)

<https://clics.clld.org/languages/ids-176>  
Recherches : r -\> 934 entries (filtered from 1,643 total entries)  
rr -\> 30 entries (filtered from 1,643 total entries)  
^r -\> 76 entries (filtered from 1,643 total entries)  
r\$ -\> 422 entries (filtered from 1,643 total entries)  
l -\> 404 entries (filtered from 1,643 total entries)  
ll -\> 84 entries (filtered from 1,643 total entries)

<https://clics.clld.org/languages/northeuralex-spa>  
Recherches : r -\> 516 entries (filtered from 1,143 total entries)  
rr -\> 0 entries (filtered from 1,143 total entries)  
l -\> 225 entries (filtered from 1,143 total entries)  
Form in source:  
ʁ -\> 516 entries (filtered from 1,143 total entries)  
l -\> 225 entries (filtered from 1,143 total entries)

``` r
show_lang('French')$Form
```

    ## [1] "rugueux" "lisse"   "rude"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="French",'other',revision))
```

## Frisian

``` r
show_lang('Frisian')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form  Trill R_type
    ##   <chr>    <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Frisian  Google  rough   rûch  yes   trill 
    ## 2 Frisian  Google  smooth  glêd  yes   trill

``` r
show_comments('Frisian')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Frisian  Google  <NA>

``` r
show_iso('Frisian')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Frisian  fry      frr

Trill : yes  
R_type : trill

Dataset :  
Google, NA

Phoible :  
<https://phoible.org/inventories/view/1111>  
- r allophones r ɾ ɹ x ɐ̯

Lasswell, Steven Theophilos. 1998. An Ecological Reference Grammar of
Sölring North Frisian. (Doctoral dissertation, University of California
at Santa Barbara).

Wikipedia :  
<https://en.wikipedia.org/wiki/North_Frisian_language>  
- Trill r  
The alveolar trill /r/ is replaced with the uvular /ʀ/ in the
Bökingharde (Mooring) dialect.

Glottologue :  
<https://glottolog.org/resource/languoid/id/nort2626>

Autres sources :

Peters, J. (2019). Saterland Frisian. Journal of the International
Phonetic Association, 49(2), 223-230. <doi:10.1017/S0025100317000226>  
- Page 223 : Trill r  
- Page 224 : In onset position /r/ is an alveolar trill. Younger
speakers tend to replace the alveolar trill with the voiced uvular
fricative \[ʁ\]. Prevocalic /r/ may be preceded by /p/, /b/, /t/, /d/,
/k/, /ɡ/, /f/, and /v/. After a vowel in word-final position and before
a consonant it is \[ɐ\] (\[bɔːɐ̯ \] boar ‘cash ADJ’, \[bɔːɐ̯ st\] Boarst
‘fissure’).

<https://clics.clld.org/languages/diacl-41600>  
Recherches : r -\> 48 entries (filtered from 149 total entries)  
rr -\> 6 entries (filtered from 149 total entries)  
l -\> 34 entries (filtered from 149 total entries)  
ll -\> 2 entries (filtered from 149 total entries)

``` r
show_lang('Frisian')$Form
```

    ## [1] "rûch" "glêd"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Frisian",'trilled',revision))
```

## Irish

``` r
show_lang('Irish')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form  Trill R_type  
    ##   <chr>    <chr>   <chr>   <chr> <chr> <chr>   
    ## 1 Irish    Google  rough   garbh no    no trill
    ## 2 Irish    CLICS   smooth  réidh no    no trill
    ## 3 Irish    CLICS   smooth  mín   no    no trill

``` r
show_comments('Irish')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Irish    IDS     tap     
    ## 2 Irish    Google  tap

``` r
show_iso('Irish')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Irish    gle      gle

Trill : no  
R_type : no trill

Dataset :  
IDS, tap  
Google, tap

Phoible :  
<https://phoible.org/languages/iris1253>  
5 inventaires :  
- SPA ɾ̥ˠ allophones ɾ̥ˠ ɾ̥ˠ; ɾˠ allophones ɾ̪ˠ ɾˠ ɾ̃ˠ ɾˠ ɾˠ; ɾ̥ʲ allophones
ɾ̥ʲ ɾ̥ʲ; ɾʲ allophones ɾʲ ɾʲ ɾʲ  
- UPSID ɾˠ pas d’allophone; ɾʲ pas d’allophone; ɾ̥ˠ pas d’allophones; ɾ̥ʲ
pas d’allophone  
- UZ ɾˠ allophones ɾˠ ɹ ɹˠ ; ɾʲ allophones ɾʲ ʝ ɹ  
- EA 2262 ɾʲpas d’allophone; ɾˠ pas d’allophone  
- EA 2521 ɾ̪ˠ pas d’allophone; ɾ̪ʲ pas d’allophone

Burke, John Florence. 1970. The Irish of Tourmakeady, Co. Mayo. Dublin:
The Dublin Institute for Advanced Studies.  
Mac an Fhailigh, Eamonn. 1968. The Irish of Erris, Co. Mayo. Dublin: The
Dublin Institute for Advanced Studies.  
Brothers, Christian. 1905. Aids to the Pronunciation of Irish. Dublin:
M. H. Gill and Son.  
Sommerfelt, A. 1964. Consonant Clusters or Single Phonemes in Northern
Irish. In Abercrombie, D. (ed.), In Memory of Daniel Jones, 368–373.
London: Longmans, Green and Co Ltd.  
Sé, Diarmuid Ó. 2000. Gaeilge Chorca Dhuibhne. Institi’uid
Teangeola’iochta ’Eireann.

Ní Chasaide, Ailbhe. 1995. Illustrations of the IPA: Irish. Journal of
the International Phonetic Association 25. 34–39. Cambridge University
Press.  
- Page 35 : Tap ɾˠ  
ɾʲ  
- Page 37 : Rhotics: An older four-way distinction among rhotics has
been reduced to a two-way distinction in Gaoth Dobhair. Initially, the
/ɾˠ ɾʲ/ distinction has been neutralized to a voiced post-alveolar
approximant \[ɹ\], which takes its coloring from the following vowel. In
non-initial position, the velarized member can be realized as either
\[ɾˠ\] or \[ɹˠ\]. The historical palatalized //, which in most other
dialects is a palatalized tap or apico-postalveolar fricative, may also
appear in Gaoth Dobhair as a voiced palatal fricative \[ʝ\] in
non-initial position. For both palatalized and velarized taps there is
often incomplete closure with considerable frication.

Wikipedia :  
<https://en.wikipedia.org/wiki/Irish_language>  
- Tap ɾˠ ɾʲ

<https://en.wikipedia.org/wiki/Irish_phonology>  
- Tap ɾˠ ɾʲ  
/ɾʲ/ has the primary allophone \[ɹ̝ʲ\].

Ó Sé, Diarmuid (2000), Gaeilge Chorca Dhuibhne (in Ga), Dublin:
Institiúid Teangeolaíochta Éireann, ISBN 0-946452-97-0

Glottologue :  
<https://glottolog.org/resource/languoid/id/iris1253>

Autres sources :

<https://clics.clld.org/languages/diacl-39400> Recherches : r -\> 140
entries (filtered from 403 total entries) rr -\> 10 entries (filtered
from 403 total entries) l -\> 123 entries (filtered from 403 total
entries) ll -\> 15 entries (filtered from 403 total entries)

<https://clics.clld.org/languages/ids-181> Recherches : r -\> 1,013
entries (filtered from 2,420 total entries) rr -\> 48 entries (filtered
from 2,420 total entries) l -\> 728 entries (filtered from 2,420 total
entries) ll -\> 104 entries (filtered from 2,420 total entries)

<https://clics.clld.org/languages/northeuralex-gle> Recherches : r -\>
446 entries (filtered from 1,192 total entries) rr -\> 0 entries
(filtered from 1,192 total entries) Form in source: ɾ -\> 446 entries
(filtered from 1,192 total entries) ɾʲ -\> 176 entries (filtered from
1,192 total entries) ɾˠ -\> 277 entries (filtered from 1,192 total
entries)

``` r
show_lang('Irish')$Form
```

    ## [1] "garbh" "réidh" "mín"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Irish",'other',revision))
```

## Armenian

``` r
show_lang('Armenian')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form  Trill R_type
    ##   <chr>    <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Armenian Google  rough   կոպիտ yes   trill 
    ## 2 Armenian Google  smooth  հարթ  yes   trill

``` r
show_comments('Armenian')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Armenian Google  <NA>

``` r
show_iso('Armenian')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Armenian hye      hye

Trill : yes  
R_type : trill

Dataset :  
Google, NA

Phoible :  
<https://phoible.org/languages/nucl1235>  
2 inventaires :  
- SPA r allophone r  
- UPSID r pas d’allophone

Allen, W. Sidney. 1950. Notes on the Phonetics of an Eastern Armenian
Speaker. Transactions of the Philological Society. 180–206. (Reprint by
Hertford 1951).

Wikipedia :  
<https://en.wikipedia.org/wiki/Armenian_language>  
- Trill /r/ ռ – ṙ  
Flap /ɾ/ ր – r

Dum-Tragut, Jasmine (2009), Armenian: Modern Eastern Armenian,
Amsterdam: John Benjamins Publishing Company  
- Page 13 : ռ r̊ \[r\]  
ր r \[ɾ\]  
- Page 19 : Rhotics  
alveolar  
flap ɾ  
trill r  
In SMEA the flap n is much more wide-spread than its trill
counterpart.  
The flap \[ɾ\] can be found in all positions, whereas the distribution
of trill \[r\] is comparatively restricted.  
The trill \[r\] seldom occurs in the initial position, with the
exception of some words among that ṙus \[rus\] “Russian” and its
derivations as well as of loans with initial trill \[r\], \[…\]. In
medial position, trill r \[r\] occurs frequently \[…\].  
- Page 20 : In final position trill r \[r\] is also found; also in some
words which have lost their originally final n from Classical Armenian
\[…\].

Glottologue :  
<https://glottolog.org/resource/languoid/id/nucl1235>

Autres sources :

Catford, J. (2001). On Rs, rhotacism and paleophony. Journal of the
International Phonetic Association, 31(2), 171-185.
<doi:10.1017/S0025100301002018>  
- Page 180 : The case of Armenian  
Classical (and at least some modern dialects of ) Armenian have two
r-phonemes ± a trill and a fricative or, perhaps less commonly, a single
tap. The untrilled r appears to be what Armenian normally inherited from
Indo-European, the trill having various secondary sources

<https://clics.clld.org/languages/diacl-36100>  
Recherches : r -\> 98 entries (filtered from 231 total entries)  
l -\> 66 entries (filtered from 231 total entries)  
Form in source:  
r̃ -\> 10 entries (filtered from 231 total entries)

<https://clics.clld.org/languages/northeuralex-hye>  
Recherches : r -\> 485 entries (filtered from 1,047 total entries)  
rr -\> 1 entries (filtered from 1,047 total entries) \[ɑntʰɔɾɾutʰjun\]  
l -\> 365 entries (filtered from 1,047 total entries)  
Form in source:  
r -\> 56 entries (filtered from 1,047 total entries)  
ɾ -\> 369 entries (filtered from 1,047 total entries)

``` r
show_lang('Armenian')$Form
```

    ## [1] "կոպիտ" "հարթ"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Armenian",'OUT',revision))
```

## Icelandic

``` r
show_lang('Icelandic')
```

    ## # A tibble: 1 × 6
    ##   Language  Dataset Meaning Form  Trill R_type
    ##   <chr>     <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Icelandic Google  rough   gróft yes   trill

``` r
show_comments('Icelandic')
```

    ## # A tibble: 1 × 3
    ##   Language  Dataset Comments
    ##   <chr>     <chr>   <chr>   
    ## 1 Icelandic Google  <NA>

``` r
show_iso('Icelandic')
```

    ## # A tibble: 1 × 3
    ##   Language  ISO_code Phoible_code
    ##   <chr>     <chr>    <chr>       
    ## 1 Icelandic isl      isl

``` r
# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Icelandic', Dataset='Google_trad',Meaning='smooth',Form='slétt', rough=FALSE, r=FALSE,l=TRUE)
```

Trill : yes  
R_type : trill

Dataset :  
Google, NA

Phoible :  
<https://phoible.org/languages/icel1247>  
2 inventaires :  
- SPA r allophones rː rˑ r r; r̥ allophones r̥ r̥ˑ  
- EA r pas d’allophone; r̥ pas d’allophone

Einarsson, Stefan. 1949. Icelandic. Baltimore: John Hopkins Press.  
Haugen, Einar. 1958. The Phonemics of Modern Icelandic. Language 34.
55–88.

Thráinsson, Höskuldur. 1994. Icelandic. In König, Ekkehard and van der
Auwera, Johan (eds.), The Germanic Languages, 142–189. London:
Routledge.  
- Page 147 : The short \[r\] is also frequently a single flap rather
than a trill  
Voiceless trill/flap r̥/ɾ̥ (r)  
Voiced trill/flap r/ɾ (r)

Wikipedia :  
<https://en.wikipedia.org/wiki/Icelandic_language>  
- Rhotic (r̥) r  
The rhotic consonants may either be trills \[r̥ r\] or taps \[ɾ̥ ɾ\],
depending on the speaker.

Glottologue :  
<https://glottolog.org/resource/languoid/id/icel1247>

Autres sources :

<https://clics.clld.org/languages/diacl-40600>  
Recherches : r -\> 135 entries (filtered from 284 total entries)  
rr -\> 1 entries (filtered from 284 total entries)  
l -\> 77 entries (filtered from 284 total entries)

<https://clics.clld.org/languages/northeuralex-isl>  
Recherches : r -\> 530 entries (filtered from 1,092 total entries)  
rr -\> 0 entries (filtered from 1,092 total entries)  
l -\> 86 entries (filtered from 1,092 total entries)  
Form in source:  
r̥ -\> 309 entries (filtered from 1,092 total entries)  
r -\> 530 entries (filtered from 1,092 total entries)

``` r
show_lang('Icelandic')$Form
```

    ## [1] "gróft"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Icelandic",'trilled',revision))
```

## Italian

``` r
show_lang('Italian')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form   Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>  <chr> <chr> 
    ## 1 Italian  Google  rough   ruvido yes   trill 
    ## 2 Italian  Google  smooth  liscio yes   trill

``` r
show_comments('Italian')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Italian  IDS     <NA>    
    ## 2 Italian  Google  <NA>

``` r
show_iso('Italian')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Italian  ita      ita

Trill : yes  
R_type : trill

Dataset :  
IDS, NA  
Google, NA

Phoible :  
<https://phoible.org/languages/ital1282>  
3 invenaitres :  
- PH r allophones r rː ɾ  
- UZ rː allophones rː ; r allophones r ʁ ɹ ʋ ʀ  
- EA ɾ pas d’allophone

Kramer, Martin. 2009. The phonology of Italian. Oxford University Press.

Rogers, Derek and d’Arcangeli, Luciana. 2004. Illustrations of the IPA:
Italian. Journal of the International Phonetic Association 34. 117–121.
Cambridge University Press.  
- Page 117 : Trill r  
- Page 118 : /r/ is a trill, but can be a tap intervocalically  
- Page 120 : r/ can be an approximant (plus tap) \[ˈloːɹo\],
\[ˌalˌloːɹɾa\]

Bertinetto, Pier Marco and Loporcaro, Michele. 2005. Illustrations of
the IPA: The sound pattern of Standard Italian, as compared with the
varieties spoken in Florence, Milan and Rome. Journal of the
International Phonetic Association 35. 131–151. Cambridge University
Press.  
- Page 132 : Trill r  
- Page 133 : Besides \[r\], which is the unmarked allophone of the
rhotic phoneme, individual variants may be encountered (uvular, alveolar
or labio-dental approximant; uvular trill). In intervocalic position,
non-geminated \[r\] may often reduce in spontaneous speech to a single
linguo-palatal contact; however, after pause or before a consonant,
there is usually a double contact.

Wikipedia :  
<https://en.wikipedia.org/wiki/Italian_language>  
- Trill r

<https://en.wikipedia.org/wiki/Italian_phonology>  
- Trill r  
- Intervocalically, single /r/ is realised as a trill with one or two
contacts.\[17\] Some literature treats the single-contact trill as a tap
\[ɾ\].\[18\]\[19\] Single-contact trills can also occur elsewhere,
particularly in unstressed syllables.\[20\] Geminate /rr/ manifests as a
trill with three to seven contacts.  
\[17\] Ladefoged, Peter; Maddieson, Ian (1996). The Sounds of the
World’s Languages. Oxford: Blackwell. ISBN 978-0-631-19815-4.  
\[18\] Rogers, Derek; d’Arcangeli, Luciana (2004). “Italian” (PDF).
Journal of the International Phonetic Association. 34 (1): 117–121.
<doi:10.1017/S0025100304001628>. S2CID 232345223  
\[19\] Luciano Canepari, A Handbook of Pronunciation, chapter 3:
«Italian»  
\[20\] Romano, Antonio. “A preliminary contribution to the study of
phonetic variation of /r/ in Italian and Italo-Romance.” Rhotics. New
data and perspectives (Proc. of’r-atics-3, Libera Università di Bolzano
(2011): 209–226, pp. 213–214.

Glottologue :  
<https://glottolog.org/resource/languoid/id/ital1282>

Autres sources :

<https://clics.clld.org/languages/diacl-53500>  
Recherches : r -\> 125 entries (filtered from 299 total entries)  
rr -\> 6 entries (filtered from 299 total entries)  
^r -\> 16 entries (filtered from 299 total entries)  
r\$ -\> 0 entries (filtered from 299 total entries)  
l -\> 87 entries (filtered from 299 total entries)

<https://clics.clld.org/languages/ids-170>  
Recherches : r -\> 811 entries (filtered from 1,543 total entries)  
rr -\> 28 entries (filtered from 1,543 total entries)  
^r -\> 66 entries (filtered from 1,543 total entries)  
r\$ -\> 1 entries (filtered from 1,543 total entries)  
l -\> 426 entries (filtered from 1,543 total entries)

<https://clics.clld.org/languages/diacl-40600>  
Recherches : r -\> 664 entries (filtered from 1,173 total entries)  
l -\> 247 entries (filtered from 1,173 total entries)  
Form in source:  
r -\> 664 entries (filtered from 1,173 total entries)  
rː -\> 25 entries (filtered from 1,173 total entries)  
l -\> 247 entries (filtered from 1,173 total entries)

``` r
show_lang('Italian')$Form
```

    ## [1] "ruvido" "liscio"

On pourrait songer à une exclusion si on considère qu’il y a un
contraste entre la rhotique simple et la rhotique géminée, l’une étant
réalisée comme un tap et l’autre comme un trill; mais pour le moment on
va garder en ’trilled”.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Italian",'trilled',revision))
```

## Luxembourgish

``` r
show_lang('Luxembourgish')
```

    ## # A tibble: 1 × 6
    ##   Language      Dataset Meaning Form  Trill R_type
    ##   <chr>         <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Luxembourgish Google  smooth  glat  yes   <NA>

``` r
show_comments('Luxembourgish')
```

    ## # A tibble: 1 × 3
    ##   Language      Dataset Comments             
    ##   <chr>         <chr>   <chr>                
    ## 1 Luxembourgish Google  R; based on wikipedia

``` r
show_iso('Luxembourgish')
```

    ## # A tibble: 1 × 3
    ##   Language      ISO_code Phoible_code
    ##   <chr>         <chr>    <chr>       
    ## 1 Luxembourgish ltz      ltz

``` r
# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Luxembourgish', Dataset='Google_trad',Meaning='rough',Form='rau', rough=TRUE, r=TRUE,l=FALSE)
```

Trill : yes  
R_type : NA

Dataset :  
Google, R; based on wikipedia

Phoible :  
<https://phoible.org/languages/luxe1241>  
2 inventaires :  
- UZ ʀ allophones ʀ ʁ ə ɐ χ; ʁ allophones ʁ ə ɐ χ ʑ  
- EA ʀ pas d’allophones; ʁ pas d’allophones; χ pas d’allophones

Gilles, Peter and Trouvain, Jürgen. 2013. Illustrations of the IPA:
Luxembourgish. Journal of the International Phonetic Association 43.
67–74. Cambridge University Press.  
- Page 67 : Trill ʀ  
Fricative χ ʁ  
- Page 68 : \[ʀ\] \[ʀəʊ\] Rou ‘silence’  
\[ɑʀiˈveː\] Arriv ́ee ‘arrival’  
The main variant of /r/ in pre-vocalic position is the trill \[ʀ\], the
lesser used variant is the fricative \[ʁ\]. Older speakers pronounce
\[ʀ\] or \[ʁ\] also word-finally (Bir \[biːʀ\] ‘pear’) whereas younger
speakers often show r-vocalization and produce central \[ə\] or \[ɐ\]
instead (\[biːə\] for Bir). Between short vowels and consonants /r/ is
spirantized, either to a voiced \[ʁ\] as in Parmesan \[ˈpɑʁməzaːn\]
‘Parmesan cheese’ or to an unvoiced \[X\] as in parken \[ˈpɑXkən\] ‘to
park’. Thus, whether /r/ shows up here as voiced or unvoiced fricative
depends on the voicing feature of the following consonant.

Wikipedia :  
<https://en.wikipedia.org/wiki/Moselle_Franconian_language>  
<https://en.wikipedia.org/wiki/Luxembourgish>  
- The consonant inventory of Luxembourgish is quite similar to that of
Standard German.\[24\]  
- Fricative χ ʁ  
- Trill ʀ  
- /χ, ʁ/ have two types of allophones: alveolo-palatal \[ɕ, ʑ\] and
uvular \[χ, ʁ\]. The latter occur before back vowels, and the former
occur in all other positions.  
The \[ʑ\] allophone appears only in a few words, and speakers
increasingly fail to distinguish between the alveolo-palatal allophones
of /χ, ʁ/ and the postalveolar phonemes  
- Younger speakers tend to vocalize a word-final /ʀ/ to \[ɐ\].

<https://en.wikipedia.org/wiki/Luxembourgish_phonology>  
- The normal realization of /r/ is more often a trill \[ʀ\] than a
fricative \[ʁ\]. The fricative variant is used after short vowels before
consonants. If the consonant is voiceless, the fricative is also
voiceless, i.e. \[χ\]. Older speakers use the consonantal variant \[ʀ \~
ʁ\] also in the word-final position, where younger speakers tend to
vocalize the /r/ to \[ɐ\], as in German and Danish.\[4\]

Glottologue :  
<https://glottolog.org/resource/languoid/id/luxe1241>

Autres sources :

NA

``` r
show_lang('Luxembourgish')$Form
```

    ## [1] "glat"

Similairement au German, on recode en other.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Luxembourgish",'other',revision))
```

## Lithuanian

``` r
show_lang('Lithuanian')
```

    ## # A tibble: 8 × 6
    ##   Language   Dataset Meaning Form      Trill R_type
    ##   <chr>      <chr>   <chr>   <chr>     <chr> <chr> 
    ## 1 Lithuanian Google  rough   šiurkštus yes   trill 
    ## 2 Lithuanian Google  smooth  lygus     yes   trill 
    ## 3 Lithuanian CLICS   smooth  lýgus     yes   trill 
    ## 4 Lithuanian CLICS   smooth  plýnas    yes   trill 
    ## 5 Lithuanian CLICS   smooth  glotnùs   yes   trill 
    ## 6 Lithuanian CLICS   rough   šiurkštùs yes   trill 
    ## 7 Lithuanian CLICS   rough   nelýgus   yes   trill 
    ## 8 Lithuanian CLICS   rough   rupùs     yes   trill

``` r
show_comments('Lithuanian')
```

    ## # A tibble: 2 × 3
    ##   Language   Dataset Comments
    ##   <chr>      <chr>   <chr>   
    ## 1 Lithuanian IDS     <NA>    
    ## 2 Lithuanian Google  <NA>

``` r
show_iso('Lithuanian')
```

    ## # A tibble: 1 × 3
    ##   Language   ISO_code Phoible_code
    ##   <chr>      <chr>    <chr>       
    ## 1 Lithuanian lit      lit

Trill : yes  
R_type : trill

Dataset :  
IDS, NA  
Google, NA

Phoible :  
<https://phoible.org/languages/lith1251>  
6 inventaires :  
- SPA r allophone r; rʲ allophone rʲ  
- UPSID r pas d’allophone; rʲ pas d’allophone  
- EA2244 r pas d’allophone; rʲ pas d’allophone  
- EA2282 r pas d’allophone; rʲ pas d’allophone  
- EA2316 r pas d’allophone  
- EA2488 r pas d’allophone; rʲ pas d’allophone

Augustitis, Daine. 1964. Das lithauische Phonationssystem. (Slavistiche
Beiträge, 12.) München: Otto Sagner.  
Ambrazas, V. and Vajtkavichjute, V. and Valjatskene, A. and Morkunas, K.
and Sabaljauskas, A. and Ul’vidas, K. 1966. Litovskij jazyk. In
Vinogradov, V. V. (ed.), Jazyki narodov SSSR. Volume 1: Indoevropejskie
jazyki, 500–527. Leningrad / Moscow: Nauka.  
Senn, Alfred. 1966. Handbuch der litauischen Sprache: Band I: Grammatik.
Heidelberg: Carl Winter.

Balode, Laimute and Holvoet, Axel. 2001. The Lithuanian language and its
dialects. In Dahl, Östen and Koptjevskaja-Tamm, Maria (eds.), Past and
Present, 41-79. Amsterdam: John Benjamins.  
- Page 48 : Trill r  
rʲ

Wikipedia :  
<https://en.wikipedia.org/wiki/Lithuanian_language>  
- Trill r rʲ

<https://en.wikipedia.org/wiki/Lithuanian_phonology>  
- Trill r ⟨r⟩ rʲ ⟨ri⟩  
- /r, rʲ/ are apical alveolar \[r̺, r̺ʲ\].\[13\]\[24\]  
\[13\] Ambrazas, Vytautas; Geniušienė, Emma; Girdenis, Aleksas;
Sližienė, Nijolė; Valeckienė, Adelė; Valiulytė, Elena; Tekorienė,
Dalija; Pažūsis, Lionginas (1997). Ambrazas, Vytautas (ed.). Lithuanian
Grammar. Vilnius: Institute of the Lithuanian Language. ISBN
9986-813-22-0. (pp. 46–47)  
\[14\] Augustaitis, Daine (1964). Das litauische Phonationssystem (in
German). Munich: Sagner.

Glottologue :  
<https://glottolog.org/resource/languoid/id/lith1251>

Autres sources :

<https://clics.clld.org/languages/diacl-36600>  
Recherches : r -\> 98 entries (filtered from 345 total entries)  
ri -\> 13 entries (filtered from 345 total entries)  
^r -\> 16 entries (filtered from 345 total entries)  
r\$ -\> 0 entries (filtered from 345 total entries)  
l -\> 96 entries (filtered from 345 total entries)

<https://clics.clld.org/languages/ids-196>  
Recherches : r -\> 598 entries (filtered from 1,918 total entries)  
ri -\> 104 entries (filtered from 1,918 total entries)  
^r -\> 62 entries (filtered from 1,918 total entries)  
r\$ -\> 6 entries (filtered from 1,918 total entries)  
l -\> 571 entries (filtered from 1,918 total entries)

<https://clics.clld.org/languages/northeuralex-lit>  
Recherches : r -\> 372 entries (filtered from 1,223 total entries)  
ri -\> 27 entries (filtered from 1,223 total entries)  
Form in source:  
r -\> 372 entries (filtered from 1,223 total entries)  
rʲ -\> 53 entries (filtered from 1,223 total entries)  
l -\> 54 entries (filtered from 1,223 total entries)

``` r
show_lang('Lithuanian')$Form
```

    ## [1] "šiurkštus" "lygus"     "lýgus"     "plýnas"    "glotnùs"   "šiurkštùs"
    ## [7] "nelýgus"   "rupùs"

Pas d’étude en phonétique/acoustique sur la rhotique en Lithuanian à ce
jour. Sur la base de
<https://soundcomparisons.com/#/en/Slavic/language/BSv_Blt_Lith_Std_Std>,
pas abérrant de considérer le “trilled”.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Lithuanian",'trilled',revision))
```

## Latvian

``` r
show_lang('Latvian')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form   Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>  <chr> <chr> 
    ## 1 Latvian  Google  rough   raupja yes   <NA>  
    ## 2 Latvian  CLICS   rough   rupjš  yes   <NA>

``` r
show_comments('Latvian')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments          
    ##   <chr>    <chr>   <chr>             
    ## 1 Latvian  IDS     based on wikipedia
    ## 2 Latvian  Google  based on wikipedia

``` r
show_iso('Latvian')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Latvian  lav      lav

``` r
# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Latvian', Dataset='Google_trad',Meaning='smooth',Form='gluds', rough=FALSE, r=FALSE,l=TRUE)
```

Trill : yes  
R_type : NA

Dataset :  
IDS, based on wikipedia  
Google, based on wikipedia

Phoible :  
<https://phoible.org/inventories/view/2493>  
- r pas d’allophone

Balode, Laimute and Holvoet, Axel. 2001. The Lithuanian language and its
dialects. In Dahl, Östen and Koptjevskaja-Tamm, Maria (eds.), Past and
Present, 41-79. Amsterdam: John Benjamins.

<https://phoible.org/inventories/view/2593>  
- r pas d’allophone

Nau, Nicole. 2011. A short grammar of Latgalian. Lincom Europa.  
Leikuma, Lidija. 2003. Latgal=ivsu vol=uda 1: Intens=iv=a m=ac=ibu kursa
materi=ali. Sanktp=eterburgas Valsts universit=ate.

Wikipedia :  
<https://en.wikipedia.org/wiki/Latvian_language>  
- Central approximant/Trill r

<https://en.wikipedia.org/wiki/Latvian_phonology>  
- Trill r (rʲ)  
- A palatalized dental trill /rʲ/ is still used in some dialects (mainly
outside Latvia) but quite rarely, and hence the corresponding letter ⟨Ŗ
ŗ⟩ was removed from the alphabet.

Nau, Nicole (1998), Latvian, Lincom Europa, ISBN 3-89586-228-2  
- Page 6 : Trill r

Glottologue :  
<https://glottolog.org/resource/languoid/id/latv1249>

Autres sources :

Brenzinger, I. (1973). Notes on the phonetics of Latvian. Journal of the
International Phonetic Association, 3(1), 36-39.
<doi:10.1017/S0025100300000669>  
- Pas informatif pour la rhotique

<https://clics.clld.org/languages/diacl-36500>  
Recherches : r -\> 76 entries (filtered from 278 total entries)  
ri -\> 11 entries (filtered from 278 total entries)  
^r -\> 11 entries (filtered from 278 total entries)  
r\$ -\> 1 entries (filtered from 278 total entries)  
l -\> 78 entries (filtered from 278 total entries)

<https://clics.clld.org/languages/ids-197>  
Recherches : r -\> 534 entries (filtered from 1,693 total entries)  
ri -\> 93 entries (filtered from 1,693 total entries)  
^r -\> 52 entries (filtered from 1,693 total entries)  
r\$ -\> 5 entries (filtered from 1,693 total entries)  
l -\> 509 entries (filtered from 1,693 total entries)

<https://clics.clld.org/languages/northeuralex-lit>  
Recherches : r -\> 357 entries (filtered from 1,183 total entries)  
ri -\> 71 entries (filtered from 1,183 total entries)  
Form in source:  
r -\> 357 entries (filtered from 1,183 total entries)  
rʲ -\> 0 entries (filtered from 1,183 total entries)  
l -\> 284 entries (filtered from 1,183 total entries)

``` r
show_lang('Latvian')$Form
```

    ## [1] "raupja" "rupjš"

On rajoute les audios de SoundComparisons :
<https://soundcomparisons.com/#/en/Slavic/language/BSv_Blt_Lat_Std_Std>.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Latvian",'trilled',revision))
```

## Macedonian

``` r
show_lang('Macedonian')
```

    ## # A tibble: 1 × 6
    ##   Language   Dataset Meaning Form  Trill R_type
    ##   <chr>      <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Macedonian Google  smooth  мазна yes   trill

``` r
show_comments('Macedonian')
```

    ## # A tibble: 1 × 3
    ##   Language   Dataset Comments
    ##   <chr>      <chr>   <chr>   
    ## 1 Macedonian Google  <NA>

``` r
show_iso('Macedonian')
```

    ## # A tibble: 1 × 3
    ##   Language   ISO_code Phoible_code
    ##   <chr>      <chr>    <chr>       
    ## 1 Macedonian mkd      mkd

``` r
# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Macedonian', Dataset='Google_trad',Meaning='rough',Form='грубо', rough=TRUE, r=TRUE,l=FALSE)
```

Trill : yes  
R_type : trill

Dataset :  
Google, NA

Phoible :  
<https://phoible.org/languages/mace1250>  
2 inventaires :  
- PH r allophone r  
- EA r pas d’allophone

Friedman, Victor A. 2002. Macedonian. In Languages of the
World/Materials 117. LinCom Europa.  
- Page 10 : The consonant /r/ can normally be viewed as having a vocalic
(syllabic) realization between consonants and between a word or morpheme
boundary and a consonant. It is possible to have minimal or near minimal
pairs with vocalic /r/ between a vowel and consonant when the vowel
preceding vocalic /r/ is a morpheme boundary: porti - po’rti ‘doors’ -
‘begin to sprout’. However, the realization of vocalic /r/ can be \[ăr\]
(see 1.2.1.3). Note that vocalic /r/ occurs in final position only in a
few foreign and onomatopoetic words: tembr ‘timbre’, kotrr ‘cry used for
driving cattle’, prr ‘sound of birds taking flight’  
- Page 10 : trill r  
- Page 10 : 1.2.2.2 Consonnantal /r/ is trilled not flapped.

Friedman, Victor. 2001. Macedonian. (SEELRC. Online: url:
<http://www.seelrc.org:8080/grammar/mainframe.jsp?nLanguageID=3>).

Wikipedia :  
<https://en.wikipedia.org/wiki/Macedonian_language>  
- Trill r\[1\]  
- \[1\] The alveolar trill (/r/) is syllabic between two consonants; for
example, ⟨прст⟩ \[ˈpr̩st\] ‘finger’.

<https://en.wikipedia.org/wiki/Macedonian_phonology>  
- Trill r\[3\]  
- \[3\] The alveolar trill (/r/) is syllabic between two consonants; for
example, ⟨прст⟩ \[ˈpr̩st\] ‘finger’.

Glottologue :  
<https://glottolog.org/resource/languoid/id/mace1250>

Autres sources :

Groen, B. M. (1977). A structural description of the Macedonian dialect
of Dihovo: Phonology, morphology, texts, lexicon (Vol. 2). John
Benjamins Publishing.  
-\> mention de ‘rolled’

``` r
show_lang('Macedonian')$Form
```

    ## [1] "мазна"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Macedonian",'trilled',revision))
```

## Dutch

``` r
show_lang('Dutch')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form    Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>   <chr> <chr> 
    ## 1 Dutch    Google  rough   ruw     yes   trill 
    ## 2 Dutch    CLICS   smooth  effen   yes   trill 
    ## 3 Dutch    CLICS   rough   oneffen yes   trill

``` r
show_comments('Dutch')
```

    ## # A tibble: 3 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Dutch    IDS     R       
    ## 2 Dutch    Google  R       
    ## 3 Dutch    WOLD    <NA>

``` r
show_iso('Dutch')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Dutch    nld      nld

Trill : yes  
R_type : trill

Dataset :  
IDS, R  
Google, R  
WOLD, NA

Phoible :  
8 inventaires :  
- PH ʀ allophones ʀ r  
- UZ2169 r allophones r r̥ ʀ ʀ̥ ʁ̞  
- UZ2170 r allophones r ʀ ɾ ɢ̆  
- UZ2171 ʀʁ allophones ʀʁ ʀ̥ʁ̥  
- UZ2172 ʀʁ allophones ʀʁ ʀ̥ʁ̥  
- UZ2173 ʀ allophones ʀ χ  
- UZ2174 r allophones r ɾ r̞ ɹ̞ ɾɹ̞  
- EA ɾ pas d’allophone

Verhoeven, Jo. 2007. Illustrations of the IPA: The Belgian Limburg
dialect of Hamont. Journal of the International Phonetic Association 37.
219–225. Cambridge University Press.  
- Page 219 : Trill R  
- Page 220 : The voiced trill is typically uvular with a clear
transitional aspect of articulation (Laver 1994). The devoiced uvular
trill in word-final position is realised as a uvular fricative either
with or without a transitional aspect.

Verhoeven, Jo. 2005. Illustrations of the IPA: Belgian Standard Dutch.
Journal of the International Phonetic Association 35. 243–247. Cambridge
University Press.  
- Page 243 : Trill (r) R  
- Page 245 : Trills  
The phonological system of Belgian Dutch has free variation between an
alveolar and uvular trill. The alveolar trill is most frequent and
geographically most widely distributed. The uvular trill is regionally
confined to the cities of Ghent and Brussels and the province of
Limburg, but estimates suggest that it is gaining fast in popularity
(Van Reenen 1994). The alveolar/uvular trill is voiced in all positions,
except word-finally as in \[do:R9\] ‘through’ and when followed by a
voiceless stop in consonant clusters as in \[va:R9t\] ‘sails’. The
phonetic characteristics of the uvular trill in the province of Limburg
were investigated in Verhoeven (1994). It was found that uvular trills
are virtually always realised with a clear transitional aspect of
articulation, i.e. the uvula does actually trill against the back of the
tongue. Only in a very small number of cases the uvular trill was
realised phonetically as a voiced uvular approximant or a voiceless
uvular fricative, i.e. without a transitional aspect. This limited
phonetic variability of r-realisation in Belgian Dutch contrasts with
the Netherlands where /r/ is also frequently realised as amongst others
a central vowel, a retroflex approximant, an alveolar tap, an alveolar
approximant or a uvular fricative (Van de Velde 1994).

Peters, Jörg. 2006. Illustrations of the IPA: The dialect of Hasselt.
Journal of the International Phonetic Association 36. 117–124. Cambridge
University Press.  
- Page 117 : Trill r  
- Page 118 : The dialect has free variation between an alveolar and a
uvular trill.

Gussenhoven, Carlos and Aarts, Flor. 1999. Illustrations of the IPA: The
dialect of Maastricht. Journal of the International Phonetic Association
29. 155–166. Cambridge University Press.  
- Page 155 : Trill R  
- Page 156 : /R/ is a (pre-)uvular trill with a fricative component, the
latter element being particularly prominent in the coda, where the
consonant is partially devoiced.

Heijmans, Linda and Gussenhoven, Garlos. 1998. Illustrations of the IPA:
The Dutch dialect of Weert. Journal of the International Phonetic
Association 28. 107–112. Cambridge University Press.  
- Page 107 : Trill R  
- Page 108 : \[ʀ\] is a uvular or pre-uvular trill with a fricative
component, the latter element being particularly prominent in the coda,
where the consonant is partially devoiced.

Peters, Jörg. 2010. Illustrations of the IPA: The Flemish-Brabant
dialect of Orsmaal-Gussenhoven. Journal of the International Phonetic
Association 40. 239–246. Cambridge University Press.  
- Page 239 : Trill r  
- page 240 : /r/ before a stressed vowel in word-initial syllables is an
apicoalveolar trill or fricative. Intervocalic /r/ and /r/ in the onset
after a consonant may be reduced to \[ɾ\]. Word-final /r/ is highly
variable both within and between speakers. The most frequent variants
are the apicoalveolar fricative trill \[r6\], the apicoalveolar
fricative \[®6\], and an apicoalveolar affricate \[ɾ®6\]; the last two
variants tend to become voiceless in pre-pausal position (cf. Whitley
2003).

Wikipedia :  
<https://en.wikipedia.org/wiki/Dutch_language#Phonology>  
- Rhotic r  
- The realization of /r/ phoneme varies considerably from dialect to
dialect and even between speakers in the same dialect area. Common
realisations are an alveolar trill \[r\], alveolar tap \[ɾ\], uvular
trill \[ʀ\], voiced uvular fricative \[ʁ\], and alveolar approximant
\[ɹ\].

<https://en.wikipedia.org/wiki/Dutch_phonology>  
- Rhotic r  
- The realization of /r/ phoneme varies considerably from dialect to
dialect and even between speakers in the same dialect area:  
The historically original pronunciation is an alveolar trill \[r\], with
the alveolar flap \[ɾ\] as a common allophone.  
The uvular trill \[ʀ\] is a common alternative, found particularly in
the central and southern dialect areas. Uvular pronunciations appear to
be gaining ground in the Randstad.\[16\] Syllable-finally, it may be
vocalized to \[ɐ\], much as in German. This is more common in the
(south)eastern areas (Limburg, southeast Brabantian, Overijssel). The
coastal dialects of South Holland produce a voiced uvular fricative
\[ʁ\].  
The velar bunched approximant \[ɹ̈\] (the Gooise R, which sounds similar
to the retroflex approximant) is found at the end of a syllable in the
pronunciation of some speakers in the Netherlands, especially those from
the Randstad, but not in Belgium. Its use has been increasing in recent
years.\[17\]

    [16] Collins, Beverley; Mees, Inger M. (2003) [First published 1981]. The Phonetics of English and Dutch (5th ed.). Leiden: Brill Publishers. ISBN 9004103406.  
    [17] Sebregts, Koen (2014). The Sociophonetics and Phonology of Dutch r (PDF) (dissertation). Utrecht: LOT. ISBN 978-94-6093-161-1. Archived (PDF) from the original on 4 March 2016. Retrieved 25 March 2017.  

Glottologue :  
<https://glottolog.org/resource/languoid/id/dutc1256>

Autres sources :

<https://clics.clld.org/languages/diacl-41400>  
Recherches : r -\> 104 entries (filtered from 286 total entries)  
^r -\> 18 entries (filtered from 286 total entries)  
r\$ -\> 36 entries (filtered from 286 total entries)  
l -\> 74 entries (filtered from 286 total entries)

<https://clics.clld.org/languages/ids-191>  
Recherches : r -\> 665 entries (filtered from 1,731 total entries)  
^r -\> 64 entries (filtered from 1,731 total entries)  
r\$ -\> 172 entries (filtered from 1,731 total entries)  
l -\> 516 entries (filtered from 1,731 total entries)

<https://clics.clld.org/languages/northeuralex-nld>  
Recherches : r -\> 416 entries (filtered from 1,102 total entries)  
^r -\> 41 entries (filtered from 1,102 total entries)  
r\$ -\> 86 entries (filtered from 1,102 total entries)  
l -\> 276 entries (filtered from 1,102 total entries)

``` r
show_lang('Dutch')$Form
```

    ## [1] "ruw"     "effen"   "oneffen"

Je ne sais pas quoi penser du trio français/dutch/german…

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="langue",'trilled',revision))
```

## Norwegian

``` r
show_lang('Norwegian')
```

    ## # A tibble: 1 × 6
    ##   Language  Dataset Meaning Form  Trill R_type  
    ##   <chr>     <chr>   <chr>   <chr> <chr> <chr>   
    ## 1 Norwegian Google  smooth  glatt no    no trill

``` r
show_comments('Norwegian')
```

    ## # A tibble: 1 × 3
    ##   Language  Dataset Comments
    ##   <chr>     <chr>   <chr>   
    ## 1 Norwegian Google  tap

``` r
show_iso('Norwegian')
```

    ## # A tibble: 1 × 3
    ##   Language  ISO_code Phoible_code
    ##   <chr>     <chr>    <chr>       
    ## 1 Norwegian nob      nob

``` r
# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Norwegian', Dataset='Google_trad',Meaning='rough',Form='røft', rough=TRUE, r=TRUE,l=FALSE)
```

Trill : no  
R_type : no trill

Dataset :  
Google, tap

Phoible :  
<https://phoible.org/languages/norw1259>  
2 inventaires :  
- SPA ɹ̩ allophone ɹ̩ ; ɾ allophone ɾ  
- UPSID ɾ pas d’allophone

Vanvik, Arne. 1972. A Phonetic-phonemic Analysis of Standard Eastern
Norwegian. Norwegian Journal of Linguistics 27. 11–139.  
Vanvik, Arne. 1972. A Phonetic-phonemic Analysis of Standard Eastern
Norwegian. Norwegian Journal of Linguistics 26. 119–164.

Wikipedia :  
<https://en.wikipedia.org/wiki/Norwegian_language>  
- Tap ɾ ɽ  
- The realization of the rhotic /ɾ/ depends on the dialect. In Eastern,
Central, and Northern Norwegian dialects, it is a tap \[ɾ\], whereas in
Western and Southern Norway, and for some speakers also in Eastern
Norway, it is uvular \[χ\] or \[ʁ\]. And in the dialects of
North-Western Norway, it is realized as \[r\], much like the trilled
⟨rr⟩ of Spanish.

Glottologue :  
<https://glottolog.org/resource/languoid/id/norw1259>

Autres sources :

Lapsyd :
<https://lapsyd.huma-num.fr/lapsyd/index.php?data=view&code=509> -\> ɽ

Kristoffersen, Gjert. 2000. The Phonology of Norwegian. Oxford
University Press, Oxford.  
- Page 24 : The retroflex flap \[\] is found in eastern and northern
dialects of Norwegian as well as in a number of Swedish dialects. It is
articulated by moving the apex rapidly forward from a retroflexed
position, hitting the alveolar ridge momentarily on its way towards the
lower teeth. Historically it derives both from Old Norse /rð/ and /l/,
the former probably being the environment where it originated due to
similarity of articulatory movement. Its phonemic status is precarious.
In most roots where it may occur, it can alternate with \[ɾ\] in words
where Old Norse had /rð/ and with \[l\] where Old Norse had /l/.
Examples are \[\]/\[\], bord ‘table’ and \[\]/\[\], sol ‘sun’.23, 24 But
there are words that can only be pronounced with \[\], such as \[\],
møl,25 which contrasts minimally with \[\], mør ‘tender’.  
- Page 24 Note 21 : An example is \[\], rar ‘funny’. It can occasionally
also be realized as a trill, for instance in emphatic speech. Uvular (or
velar) realization may occur in two distinct sub-populations of
UEN-speakers. It is used by some older, upper-class Oslo speakers,
probably a vestige of an aborted change from apical to uvular
pronunciation that seems to have taken place earlier this century.
Uvular pronunciation can also be heard among speakers who have not
managed to acquire the more complex apical articulation.

<https://clics.clld.org/languages/northeuralex-nor>  
Recherches : r -\> 375 entries (filtered from 1,052 total entries)  
^r -\> 45 entries (filtered from 1,052 total entries)  
r\$ -\> 82 entries (filtered from 1,052 total entries)  
l -\> 281 entries (filtered from 1,052 total entries)

``` r
show_lang('Norwegian')$Form
```

    ## [1] "glatt"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Norwegian",'other',revision))
```

## Sinhala (Sinhalese)

``` r
show_lang('Sinhala (Sinhalese)')
```

    ## # A tibble: 2 × 6
    ##   Language            Dataset Meaning Form  Trill R_type  
    ##   <chr>               <chr>   <chr>   <chr> <chr> <chr>   
    ## 1 Sinhala (Sinhalese) Google  rough   රළු ය  no    no trill
    ## 2 Sinhala (Sinhalese) Google  smooth  සුමටයි  no    no trill

``` r
show_comments('Sinhala (Sinhalese)')
```

    ## # A tibble: 1 × 3
    ##   Language            Dataset Comments
    ##   <chr>               <chr>   <chr>   
    ## 1 Sinhala (Sinhalese) Google  <NA>

``` r
show_iso('Sinhala (Sinhalese)')
```

    ## # A tibble: 1 × 3
    ##   Language            ISO_code Phoible_code
    ##   <chr>               <chr>    <chr>       
    ## 1 Sinhala (Sinhalese) sin      sin

Trill : no  
R_type : no trill

Dataset :  
Google, NA

Phoible :  
<https://phoible.org/languages/sinh1246>  
2 inventaires :  
- SPA ɾ allophones ɹ̥ ɾ  
- UPSID ɾ pas d’allophone

Coates, W. A. and de Silva, M. W. S. 1960. The Segmental Phonemes of
Sinhalese. University of Ceylon Review 18. 163–175.  
- Page 164 : flap alveolar r  
- Page 165 : The /r/ phoneme has two allophones. Initially it is a
voiceless alveolar spirant; the tip of the tongue is raised towards the
tooth-ridge or even a point slightly further forward, while the front of
the tongue is depressed. In sound it somewhat resembles the voiceless
portion of the /r/ in the usual American pronunciation of tree. In all
other positions /r/ is a voiced flap or trill consisting of one or two
taps with the tip of the tongue against the tooth-ridge. In original
Sinhalese words Ir/ does not occur finally, but there are now borrowed
words in which it does occur in that position.  
Examples: /ratul/‘red’, /hari/ ‘correct’, /kaar/ ’cars.

Wikipedia :  
<https://en.wikipedia.org/wiki/Sinhala_language>  
- Trill r

<https://en.wikipedia.org/wiki/Sinhala_script>  
- ර 0DBB ra \[ra\]  
Vowels ඍ ඎ  
Transliteration r̥ r̥̄  
IPA \[ri,ru\] \[riː,ruː\]

Glottologue :  
<https://glottolog.org/resource/languoid/id/sinh1246>

Autres sources :

Wasala, A., & Gamage, K. (2005). Research report on phonetics and
phonology of Sinhala. Language Technology Research Laboratory,
University of Colombo School of Computing, 35, 11.  
- Page 474 : Trill r

Chandralal, D. (2010). Sinhala (Vol. 15). John Benjamins Publishing.  
- Page 31 : The alveolar flap /r/ is articulated with the tip of the
tongue approaching the tooth-ridge and further back. It may consist of
one single tap or of several taps formed by flapping the tip of the
tongue against the tooth–ridge. Initially and medially it may be
pronounced with a strong aspiration, and in final position with repeated
tapping.  
Examples: /raere/ ‘nighf, /bare/ ’heavy, /kaar/ ’car’.  
- Page 32 : \[r\] word initial  
\[r^h\] intervocalic

Gair, J. & Paolillo, J., 1997, “Sinhala”, Lincom Europa, München,
Newcastle  
- Page 3 : Sonorants dental r

<https://clics.clld.org/languages/diacl-49600>  
Recherches : r -\> 15 entries (filtered from 130 total entries)  
^r -\> 4 entries (filtered from 130 total entries)  
r\$ -\> 0 entries (filtered from 130 total entries)  
l -\> 23 entries (filtered from 130 total entries)

``` r
show_lang('Sinhala (Sinhalese)')$Form
```

    ## [1] "රළු ය" "සුමටයි"

Besoin de plus d’études sur la phonétique et la phonologie.  
Je laisse en ‘other’ mais je n’ai aucune certitude.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Sinhala (Sinhalese)",'other',revision))
```

## Slovak

``` r
show_lang('Slovak')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form   Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>  <chr> <chr> 
    ## 1 Slovak   Google  rough   hrubý  yes   <NA>  
    ## 2 Slovak   Google  smooth  hladký yes   <NA>

``` r
show_comments('Slovak')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments                                                     
    ##   <chr>    <chr>   <chr>                                                        
    ## 1 Slovak   Google  based on wikipedia; no description but that's the IPA charac…

``` r
show_iso('Slovak')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Slovak   slk      slk

Trill : yes  
R_type : NA

Dataset :  
Google, based on wikipedia; no description but that’s the IPA character
they use

Phoible :  
<https://phoible.org/languages/slov1269>  
2 inventaires :  
- UZ r allophone r ; rː allophone rː  
- EA r pas d’allophone ; rː pas d’allophone

Hanulíková, Adriana and Hamann, Silke. 2010. Illustrations of the IPA:
Slovak. Journal of the International Phonetic Association 40. 373–378.
Cambridge University Press.

Wikipedia :  
<https://en.wikipedia.org/wiki/Slovak_language>  
- Trill short r  
geminated rː

<https://en.wikipedia.org/wiki/Slovak_phonology>  
- Trill short r  
geminated rː  
- Short /r/ is most often a tap \[ɾ\].\[19\]

- r /r/, /r̩/ r more (‘sea’)  
- ŕ /r̩ː/ ŕ vŕba (‘willow tree’)

Hanulíková, Adriana; Hamann, Silke (2010), “Slovak” (PDF), Journal of
the International Phonetic Association, 40 (3): 373–378,
<doi:10.1017/S0025100310000162>  
- Page 374 : The long lateral and the long trill are often considered to
be allophones of /l/ and /r/, respectively (see e.g. ˇDuroviˇc 1975;
Short 1993, 2000) because there are no minimal pairs for /lː/ and /l/ or
/rː/ and /r/. However, the two pairs are not in complementary
distribution: the long lateral and the long rhotic can only occur in the
syllable nucleus, their short counterparts in the nucleus and elsewhere.
For this reason we follow Bujalka, Balaz & Ryyzkova (1996: 42) in
considering all four sounds phonemes. Short /r/ is in the majority of
the cases realized as a tap \[ɾ\] (Isacenko 1968: 185; Palvik 2004), but
some instances show two closure phases.

Isačenko, A. V. (1968). Spektrografická analýza slovenských hlások.
SAV.  
Pavlík, R. (2004). Slovenské hlásky a medzinárodná fonetická abeceda.
Jazykovedný časopis, (2), 87.

Glottologue :  
<https://glottolog.org/resource/languoid/id/slov1269>

Autres sources :

<https://soundcomparisons.com/#/en/Slavic/languagesXwords/Lgs_Sln/Wds_None>  
On a des trills.

<https://clics.clld.org/languages/northeuralex-slk> Recherches : r -\>
359 entries (filtered from 1,129 total entries)  
^r -\> 48 entries (filtered from 1,129 total entries)  
r\$ -\> 23 entries (filtered from 1,129 total entries)  
l -\> 230 entries (filtered from 1,129 total entries)

``` r
show_lang('Slovak')$Form
```

    ## [1] "hrubý"  "hladký"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Slovak",'trilled',revision))
```

## Slovenian

``` r
show_lang('Slovenian')
```

    ## # A tibble: 1 × 6
    ##   Language  Dataset Meaning Form   Trill R_type  
    ##   <chr>     <chr>   <chr>   <chr>  <chr> <chr>   
    ## 1 Slovenian Google  smooth  gladko yes   no trill

``` r
show_comments('Slovenian')
```

    ## # A tibble: 1 × 3
    ##   Language  Dataset Comments                                                    
    ##   <chr>     <chr>   <chr>                                                       
    ## 1 Slovenian Google  based on wikipedia; no description but that's the IPA chara…

``` r
show_iso('Slovenian')
```

    ## # A tibble: 1 × 3
    ##   Language  ISO_code Phoible_code
    ##   <chr>     <chr>    <chr>       
    ## 1 Slovenian slv      slv

``` r
# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Slovenian', Dataset='Google_trad',Meaning='rough',Form='grobo', rough=TRUE, r=TRUE,l=FALSE)
```

Trill : yes  
R_type : no trill

Dataset :  
Google, based on wikipedia; no description but that’s the IPA character
they use

Phoible :  
<https://phoible.org/languages/slov1268>  
2 inventaires :  
- PH ɾ allophone ɾ  
- EA r pas d’allophone

Ŝuŝtarŝiĉ, Rastislav and Komar, Smiljana and Petek, Bojan. 1995.
Illustrations of the IPA: Slovene. Journal of the International Phonetic
Association 25. 86–90. Cambridge University Press.  
- Page 86 : Tap ɾ

Greenberg, Marc L. 2006. A short reference grammar of Standard Slovene.
(Online: url{<a
href="https://kuscholarworks.ku.edu/bitstream/handle/1808/5469/m.l.greenberg\_slovene\_grammar\_8dec06.pdf?sequence=1&amp;isAllowed=y"
class="uri">https://kuscholarworks.ku.edu/bitstream/handle/1808/5469/m.l.greenberg\_slovene\_grammar\_8dec06.pdf?sequence=1&amp;isAllowed=y</a>}).  
- Page 17 :  
Letter IPA Example Notes  
R, r r, ɾ \[examples\] When r appears in syllabic position, i.e.,
between consonants, it is realized as ə + ɾ

Wikipedia :  
<https://en.wikipedia.org/wiki/Slovene_language>  
- Rhotic r

<https://en.wikipedia.org/wiki/Slovene_phonology>  
- Flap ɾ  
- /ɾ/ is uvular in a number of Upper Carniolan and Carinthian dialects,
but such pronunciation is not allowed in Standard Slovene.\[10\]  
- /ɾ/ is usually described as the sequence /əɾ/ (with an epenthetic
\[ə\]). Jones (2002)\[full citation needed\] found that a vocalic
segment similar to \[ə\] occurs before (and occasionally after) both
syllabic and non-syllabic /ɾ/, and that it is shorter than epenthetic
\[ə\], leading to the conclusion that this is not epenthetic \[ə\], but
simply a feature of rhotic consonant production in Slovene.

\[10\] Reindl, Donald F. (2008), Language Contact: German and Slovenian,
Universitätsverlag Dr. N. Brockmeyer, ISBN 978-3-8196-0715-8

Jones, M. J. (2002). The Status of the” Syllabic” Trill in Slovene: a
Phonological and Phonetic Analysis. Slovene Studies: Journal of the
Society of Slovene Studies, 27-45.

Glottologue :  
<https://glottolog.org/resource/languoid/id/slov1268>

Autres sources :

<https://soundcomparisons.com/#/en/Slavic/languagesXwords/Lgs_Sln/Wds_Sln>

<https://clics.clld.org/languages/diacl-37550>  
Recherches : r -\> 54 entries (filtered from 167 total entries)  
^r -\> 8 entries (filtered from 167 total entries)  
r\$ -\> 7 entries (filtered from 167 total entries)  
l -\> 48 entries (filtered from 167 total entries)

<https://clics.clld.org/languages/northeuralex-slv>  
Recherches : r -\> 348 entries (filtered from 1,035 total entries)  
^r -\> 37 entries (filtered from 1,035 total entries)  
r\$ -\> 29 entries (filtered from 1,035 total entries)  
l -\> 272 entries (filtered from 1,035 total entries)

``` r
show_lang('Slovenian')$Form
```

    ## [1] "gladko"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Slovenian",'other',revision))
```

## Albanian

``` r
show_lang('Albanian')
```

    ## # A tibble: 1 × 6
    ##   Language Dataset Meaning Form  Trill R_type
    ##   <chr>    <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Albanian Google  smooth  butë  yes   trill

``` r
show_comments('Albanian')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Albanian Google  <NA>

``` r
show_iso('Albanian')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Albanian als      als

``` r
# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Albanian', Dataset='Google_trad',Meaning='rough',Form='përafërt', rough=TRUE, r=TRUE,l=FALSE)
```

Trill : yes  
R_type : trill

Dataset :  
Google, NA

Phoible :  
<https://phoible.org/languages/tosk1239>  
- SPA r allophones ɾ r  
- UPSID r̪\|r pas d’allophones  
- EA r pas d’allophones

Newmark, Leonard. 1957. Structural Grammar of Albanian. (International
Journal of American Linguistics, 23.) Bloomington: Indiana University.

Klippenstein, Rachel. 2010. Word-initial consonant clusters in Albanian.
In Lesho, Marivic and Smith, Bridget J. and Campbell-Kibler, Kathryn and
Culicover, Peter W. (eds.), Working Papers in Linguistics, 10–32.

Camaj, Martin and Fox, Leonard. 1984. Albanian grammar: with exercises,
chrestomathy and glossaries. Otto Harrassowitz Verlag.

Wikipedia :  
<https://en.wikipedia.org/wiki/Tosk_Albanian>

<https://en.wikipedia.org/wiki/Albanian_language#Phonology>  
- Flap ɾ  
Trill r  
- The contrast between flapped r and trilled rr is the almost the same
as in Spanish or Armenian. However, in most of the dialects, as also in
standard Albanian, the single “r” changes from an alveolar flap /ɾ/ to
an alveolar approximant \[ɹ\].

Glottologue :  
<https://glottolog.org/resource/languoid/id/tosk1239>

Autres sources :

Coretta, S., Riverin-Coutlée, J., Kapia, E., & Nichols, S. (2022).
Northern Tosk Albanian. Journal of the International Phonetic
Association, 1-23. <doi:10.1017/S0025100322000044>  
- Page 3 : Trill r  
Flap ɽ  
- Page 8 : Rhotics  
Northern Tosk contrasts two rhotics, written as ⟨r⟩ and ⟨rr⟩, both of
which can occur word initially, medially and finally. While our speakers
always realised ⟨rr⟩ as an alveolar trill \[r\], ⟨r⟩ was characterised
by a great deal of allophonic variation, as also previously noted by
Belluscio (Reference Belluscio, Gesuato and Grazia Busà2014). Among the
range of realisations for ⟨r⟩, we found retroflex flaps \[ɽ\] and
retroflex approximants \[ɻ\]. These seem to be in complementary
distribution, with \[ɻ\] tending to appear in coda position, although
for some speakers and in some words, word-initial /r/ was also realised
as a retroflex approximant \[ɻ\]. Impressionistic studies and some
case-study analyses suggest that /r/ may increasingly be realised as
\[ɻ\] or variants thereof in different varieties of Albanian (Hysenaj
Reference Hysenaj2009, Jubani-Bengu Reference Jubani-Bengu2012,
Belluscio Reference Belluscio2016). Figure 8 shows illustrative examples
of the three main rhotic allophones. The limited set of contexts in the
words collected does not allow us to make definitive generalisations
about the distribution of these allophones and future work is warranted
on this matter. We have chosen to use /r/ for ⟨rr⟩ and /ɽ/ for ⟨r⟩,
though as noted above, /ɽ/ is frequently realised as \[ɻ\].

<https://clics.clld.org/languages/diacl-35300>  
Recherches : r -\> 112 entries (filtered from 293 total entries)  
rr -\> 18 entries (filtered from 293 total entries)  
^r -\> 13 entries (filtered from 293 total entries)  
r\$ -\> 31 entries (filtered from 293 total entries)  
l -\> 57 entries (filtered from 293 total entries)

<https://clics.clld.org/languages/ids-205>  
Recherches : r -\> 800 entries (filtered from 1,977 total entries)  
^r -\> 56 entries (filtered from 1,977 total entries)  
r\$ -\> 181 entries (filtered from 1,977 total entries)  
l -\> 416 entries (filtered from 1,977 total entries)  
Form in source :  
r̃ -\> 110 entries (filtered from 1,977 total entries)  
^r̃ -\> 31 entries (filtered from 1,977 total entries)  
ɾ -\> 380 entries (filtered from 1,109 total entries)

<https://clics.clld.org/languages/northeuralex-sqi>  
Recherches : r -\> 444 entries (filtered from 1,109 total entries)  
^r -\> 35 entries (filtered from 1,109 total entries)  
r\$ -\> 95 entries (filtered from 1,109 total entries)  
l -\> 202 entries (filtered from 1,109 total entries)  
Form in source :  
r -\> 67 entries (filtered from 1,109 total entries)  
^r -\> 23 entries (filtered from 1,109 total entries)  
ɾ -\> 380 entries (filtered from 1,109 total entries)  
^ɾ -\> 12 entries (filtered from 1,109 total entries)

``` r
show_lang('Albanian')$Form
```

    ## [1] "butë"

Donc contrast donc OUT.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Albanian",'OUT',revision))
```

## Swedish

``` r
show_lang('Swedish')
```

    ## # A tibble: 6 × 6
    ##   Language Dataset Meaning Form     Trill R_type  
    ##   <chr>    <chr>   <chr>   <chr>    <chr> <chr>   
    ## 1 Swedish  Google  smooth  slät     no    no trill
    ## 2 Swedish  CLICS   smooth  slät     no    no trill
    ## 3 Swedish  CLICS   smooth  jämn     no    no trill
    ## 4 Swedish  CLICS   smooth  glatt    no    no trill
    ## 5 Swedish  CLICS   rough   ojämn    no    no trill
    ## 6 Swedish  CLICS   rough   skrovlig no    no trill

``` r
show_comments('Swedish')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments   
    ##   <chr>    <chr>   <chr>      
    ## 1 Swedish  IDS     approximant
    ## 2 Swedish  Google  approximant

``` r
show_iso('Swedish')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Swedish  swe      swe

Trill : no  
R_type : no trill

Dataset :  
IDS, approximant  
Google, approximant

Phoible :  
<https://phoible.org/languages/swed1254>  
2 inventaires :  
- PH ɹ allophone ɹ  
- EA ɹ pas d’allophone

Engstrand, Olle. 1990. Illustrations of the IPA: Swedish. Journal of the
International Phonetic Association 20. 41–42. Cambridge University
Press.  
- Page 42 : Approximant ɹ  
- Page 43 : /ɹ/ can be approximant (e.g. star/cast), voiced fricative
(e.g. the first r in vandrare), or trilled; the trilled variant is
restricted to emphatic stress in many speakers and does not appear in
the recording.

Wikipedia :  
<https://en.wikipedia.org/wiki/Swedish_language>  
- There are 18 consonant phonemes, two of which, /ɧ/ and /r/, vary
considerably in pronunciation depending on the dialect and social status
of the speaker. In many dialects, sequences of /r/ (pronounced
alveolarly) with a dental consonant result in retroflex consonants;
alveolarity of the pronunciation of /r/ is a precondition for this
retroflexion. /r/ has a guttural or “French R” pronunciation in the
South Swedish dialects; consequently, these dialects lack retroflex
consonants.\[50\]  
\[50\] Garlén, Claes (1988), Svenskans fonologi (in Swedish), Lund:
Studentlitteratur, ISBN 978-91-44-28151-3, OCLC 67420810  
- Trill r

<https://en.wikipedia.org/wiki/Swedish_phonology>  
- Rhotic r  
- /r/ has distinct variations in Standard Swedish. For most speakers,
the realization as an alveolar trill occurs only in contexts where
emphatic stress is used.\[citation needed\] In Central Swedish, it is
often pronounced as a fricative (transcribed as \[ʐ\])\[50\] or
approximant (transcribed as \[ɹ\]),\[7\] which is especially frequent in
weakly articulated positions such as word-finally\[29\] and somewhat
less frequent in stressed syllable onsets, in particular after other
consonants.\[50\] It may also be an apico-alveolar tap.\[29\] One of the
most distinct features of the southern varieties is the uvular
realization of /r/, which may be a trill \[ʀ\],\[51\] a fricative \[ʁ\]
or an approximant \[ʁ̞\]. In Finland, /r/ is usually an apical trill
\[r\], and may be an approximant \[ɹ\] postvocalically.\[52\]  
\[29\] Andersson, Erik (2002), “Swedish”, in König, Ekkehard; van der
Auwera, Johan (eds.), The Germanic Languages, Routledge language family
descriptions, Routledge, pp. 271–312, ISBN 0-415-28079-6  
\[50\] Elert, Claes-Christian (2000), Allmän och svensk fonetik (in
Swedish) (8th ed.), Stockholm: Norstedts, ISBN 91-1-300939-7  
\[51\] Ladefoged, Peter; Maddieson, Ian (1996). The Sounds of the
World’s Languages. Oxford: Blackwell. ISBN 978-0-631-19815-4.  
\[52\] Riad, Tomas (2014), The Phonology of Swedish, Oxford University
Press, ISBN 978-0-19-954357-1

Glottologue :  
<https://glottolog.org/resource/languoid/id/swed1254>

Autres sources :

<https://soundcomparisons.com/#/en/Germaqnic/languagesXwords/Lgs_Sln/Wds_Sln>
=\> avec plein de trills pour le Swedish

<https://clics.clld.org/languages/diacl-41200>  
Recherches : r -\> 98 entries (filtered from 317 total entries)  
^r -\> 18 entries (filtered from 317 total entries)  
r\$ -\> 29 entries (filtered from 317 total entries)  
l -\> 89 entries (filtered from 317 total entries)

<https://clics.clld.org/languages/ids-187>  
Recherches : r -\> 808 entries (filtered from 2,101 total entries)  
^r -\> 49 entries (filtered from 1,095 total entries)  
r\$ -\> 188 entries (filtered from 2,101 total entries)  
l -\> 673 entries (filtered from 2,101 total entries)

<https://clics.clld.org/languages/northeuralex-swe>  
Recherches : r -\> 367 entries (filtered from 1,095 total entries)  
^r -\> 49 entries (filtered from 1,095 total entries)  
r\$ -\> 82 entries (filtered from 1,095 total entries)  
l -\> 287 entries (filtered from 1,095 total entries)

``` r
show_lang('Swedish')$Form
```

    ## [1] "slät"     "slät"     "jämn"     "glatt"    "ojämn"    "skrovlig"

Je ne sais pas si laisser en other ou mettre trill… Similairement au
français ? Si on se tient à JIPA, alors c’est other.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Swedish",'other',revision))
```

## Ukrainian

``` r
show_lang('Ukrainian')
```

    ## # A tibble: 1 × 6
    ##   Language  Dataset Meaning Form    Trill R_type
    ##   <chr>     <chr>   <chr>   <chr>   <chr> <chr> 
    ## 1 Ukrainian Google  smooth  гладкий yes   trill

``` r
show_comments('Ukrainian')
```

    ## # A tibble: 1 × 3
    ##   Language  Dataset Comments
    ##   <chr>     <chr>   <chr>   
    ## 1 Ukrainian Google  <NA>

``` r
show_iso('Ukrainian')
```

    ## # A tibble: 1 × 3
    ##   Language  ISO_code Phoible_code
    ##   <chr>     <chr>    <chr>       
    ## 1 Ukrainian ukr      ukr

``` r
# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Ukrainian', Dataset='Google_trad',Meaning='rough',Form='грубо', rough=TRUE, r=TRUE,l=FALSE)
```

Trill : yes  
R_type : trill

Dataset :  
Google, NA

Phoible :  
<https://phoible.org/languages/ukra1253>  
2 inventaires :  
- PH r̪ allophones r̪ r̪ʲ  
- EA r pas d’allophone, rʲ pas d’allophone

Pugh, Stefan M. and Press, Ian. 1999. Ukrainian: A Comprehensive
Grammar. Routledge.  
- Page 19 : P p \[cursive Cyrillic\] ep \[r\] r as in Spanish caro  
- Page 23 : Vibrant Dental r rʲ  
- Page 28 : hard/soft pairs : \[r\] - \[rʲ\]  
- Page 33 : A much broader range of consonantal alternations is as
follows:  
\[r\] - \[rʲ\]  
- Page 40 It should be borne in mind that the following consonants never
lengthen, but remain combined with \[j\]: \[b\], \[p\], \[B\], \[m\],
\[f\], \[r\], namely the labials and \[r\]

Buk, Solomija and Mačutek, Ján and Rovenchak, Andrij. 2008. Some
properties of the Ukrainian writing system. (Online:
urlhttp://arxiv.org/ftp/arxiv/papers/0802/0802.4198.pdf).  
- Page 3 : The first group consists of 22 phonemes: /r/  
The group of palatalized consonants consists of 10 phonemes: /rʲ/  
There is no complete agreement about the nature of the palatalization of
/rʲ/, sometimes it is considered as a semi-palatalized consonant
(Ponomariv 2001: 16, 20). As there is no special IPA mark for
semi-palatalization, we will use a superscript dotless ‘j’, e. g., /r^J
/.  
- Page 4 : Ukrainian has the following sonorants: /ʋ, l, lʲ, m, nʲ, r,
r^J , j/.  
- Page 5 : It is interesting that labials /b, ʋ, m, p, f/, as well as
/r/, are not geminated in such situations but appear as a ‘consonant +
/j/’ combination  
- Page 8 : /r/
<p>
pom  
/r^J/
<p>

followed by <IOP>: ряд  
\< рь \> only before \< о \>: трьох

Wikipedia :  
<https://en.wikipedia.org/wiki/Ukrainian_phonology>  
- Trill r rʲ  
- After voiceless consonants, word-final /m/, /l/, /r/ are voiceless
\[m̥\], \[l̥\], \[r̥\].\[8\] For /r/, this only happens after /t/. \[9\]  
- /r/ is often realized as a single tap \[ɾ\].  
- /t, d, dʲ, n, nʲ, s, sʲ,…/ are dental \[t̪, d̪, d̪ʲ , n̪, n̪ʲ, s̪, s̪ʲ,
z̪,…\],\[13\] while /tʲ, l, lʲ, r, rʲ/ are alveolar \[tʲ , l, lʲ, r,
rʲ\].\[14\] - The group of palatalized consonants consists of 10
phonemes: /j, dʲ, zʲ, lʲ, nʲ, rʲ, sʲ, tʲ, t͡sʲ, d͡zʲ/. All except /j/ have
a soft and a hard variant. There is no agreement about the nature of the
palatalization of /rʲ/; sometimes, it is considered as a
semi-palatalized\[clarification needed\] consonant.\[15\]  
- Unpalatalized dental consonants /n, t, d, t͡s, d͡z, s, z, r, l/ become
palatalized if they are followed by other palatalized dental consonants
/nʲ, tʲ, dʲ, t͡sʲ, d͡zʲ, sʲ, zʲ, rʲ, lʲ/.  
- There are some typical deviations which may appear in spoken language
(often under the influence of Russian).\[20\] They are usually
considered phonetic errors by pedagogists.\[21\] : \[rʲ\] for /r/

Danyenko, Andrii; Vakulenko, Serhii (1995), Ukrainian, Lincom Europa,
ISBN 978-3-929075-08-3  
- Page 8 : p r /r/ \[r\] “hard alveolar voiced trill: rasa \[’rasa\]
‘race’, dar \[dar\] ‘gift’, zirka \[’zi’rka\] ‘star’  
\[r \[chapeau sous le r\]\] voiceless variant of /r/ in word-final
position after /t/: centr \[centr\[chapeau sous le r\]\] ‘centre’  
/r’/”soft” alveolar trill: riasa \[’r’asa\] ‘cassock’, ric \[r’ic\]
‘thing’

Glottologue :  
<https://glottolog.org/resource/languoid/id/ukra1253>

Autres sources :

Pompino-Marschall, B., Steriopolo, E., & Żygis, M. (2017). Ukrainian.
Journal of the International Phonetic Association, 47(3), 349-357.
<doi:10.1017/S0025100316000372>  
- Page 350 : Trill Plain r  
Palatalized rʲ  
- Page 351 :r rad рад rad ‘councillor (GEN. PL )’  
rʲ rʲad ряд rjad ‘row’  
- Page 352 : Furthermore, the palatalized counterpart of the trill \[r\]
is frequently realized as \[ɾʲ\], which is most probably due to the fact
that trilling and palatalization are far more complex and effortful than
the production of a palatalized flap (see ˙Zygis 2005).

Notes à partir des audios : no trill in the narrative… et pour la liste
de mots : no trills\_ taps and approximants

<https://soundcomparisons.com/#/en/Slavic/language/BSv_SvE_Ukr_NW_Vol_Riv_Ostroh_Acc>
-\> Pas de trill sauf pour вірне qui sur Praat est plus un tap suivi
d’une nasal, les initiales sont des taps

<https://clics.clld.org/languages/northeuralex-ukr>  
Recherches : r -\> 324 entries (filtered from 1,015 total entries)  
^r -\> 49 entries (filtered from 1,015 total entries)  
r\$ -\> 14 entries (filtered from 1,015 total entries)  
l -\> 223 entries (filtered from 1,015 total entries)  
Form in source:  
rʲ -\> 40 entries (filtered from 1,015 total entries)

``` r
show_lang('Ukrainian')$Form
```

    ## [1] "гладкий"

Même si JIPA mentionne trill, tout fait penser à un tap, donc on mets
other.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Ukrainian",'other',revision))
```

## Yiddish

``` r
show_lang('Yiddish')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form    Trill R_type          
    ##   <chr>    <chr>   <chr>   <chr>   <chr> <chr>           
    ## 1 Yiddish  Google  smooth  גלאַט    yes   mixed with trill
    ## 2 Yiddish  CLICS   smooth  glat    yes   mixed with trill
    ## 3 Yiddish  CLICS   rough   šorstik yes   mixed with trill

``` r
show_comments('Yiddish')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Yiddish  IDS     <NA>    
    ## 2 Yiddish  Google  <NA>

``` r
show_iso('Yiddish')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Yiddish  ydd      ydd

Trill : yes  
R_type : mixed with trill

Dataset :  
IDS, NA  
Google, NA

Phoible :  
<https://phoible.org/languages/east2295>  
2 inventaires :  
- PH r allophone r; ʀ allophone ʀ; ɾ allophone ɾ  
- EA ʀ pas d’allophone

Kleine, Ane. 2003. Illustrations of the IPA: Standard Yiddish. Journal
of the International Phonetic Association 33. 261–265. Cambridge
University Press.  
- Page 262 : Trill-Flap-Tap r ɾ ʀ  
Trill-Flap-Tap  
In ‘Standard Yiddish’ \[r/ɾ\] and \[ʀ\] are admissible variants of one
phoneme according to speakers’ preferences, habitually occurring only
with a single contact of the articulator at the place of articulation.  
- Page 264 : Utilisation systématique du tap dans la transcription  
- Les audios ne sont pas disponibles

Jacobs, Neil G. 2005. Yiddish: a linguistic introduction. Cambridge:
Cambridge Univ. Press. xix+327pp. (Includes bibliographical references
and index).  
- Page 108 : Fricatives velar R  
- Page 109 : The phoneme /r/ in Yiddish is generally a trill - either
apical \[r\], or uvular \[R\]. Unless otherwise specified, the symbol
<r> is used generally to represent /r/ throughout the present work.
Although a trill, /r/ frequently functions as part. of a natural class
with the velar fricative /x/ in regional phonological processes (e.g.,
CY breaking and schwa-coloring; NEY obstruent voicing as-similation).
Prilutski (1940: 17) gives \[r\] as the norm for Poland Major, certain
Ukrainian varieties, some Lithuanian varieties, Bessarabia, and Old
Rumania. He describes a heavily rolled r as especially characteristic
for Bessarabia, as well as for inhabitants of small settlements and
villages. He further writes that urban Yiddish speakers usually produce
an r with fewer trills - even realized as a single tap. The \[R\]
articulation is commonly found in many CY varieties, throughout Galicia,
and in some varieties at the periphery of former Congress Poland, in
Volhynia and Podolia in a strip along Galicia. A further realization of
/r/ as \[ʁ\] - more like a true velar fricative - is found in certain
Ukrainian Yiddish dialects, as well as in many Lithuanian Yiddish
dialects (p. 19). Thus, it is hard to draw a clear synchronic map of
front vs. back /r/ in Yiddish dialects.  
- Page 110 : /r/ rojz ‘rose’ gojrem ‘factor’ dir ‘you’\[DAT\]

Wikipedia :  
<https://en.wikipedia.org/wiki/Yiddish>  
- Rhotic r  
- The rhotic /r/ can be either alveolar or uvular, either a trill \[r \~
ʀ\] or, more commonly, a flap/tap \[ɾ \~ ʀ̆\].\[46 : jipa\]

Glottologue :  
<https://glottolog.org/resource/languoid/id/east2295>

Autres sources :

<https://clics.clld.org/languages/diacl-42825>  
Recherches : r -\> 29 entries (filtered from 103 total entries)  
^r -\> 3 entries (filtered from 103 total entries  
r\$ -\> 8 entries (filtered from 103 total entries)  
l -\> 25 entries (filtered from 103 total entries)

<https://clics.clld.org/languages/ids-195>  
Recherches : r -\> 698 entries (filtered from 1,786 total entries)  
^r -\> 51 entries (filtered from 1,786 total entries)  
r\$ -\> 194 entries (filtered from 1,786 total entries)  
l -\> 559 entries (filtered from 1,786 total entries)

``` r
show_lang('Yiddish')$Form
```

    ## [1] "גלאַט"    "glat"    "šorstik"

C’est mixte, donc ça devrait être OUT, mais je change en other.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Yiddish",'other',revision))
```

## Nepali

``` r
show_lang('Nepali')
```

    ## # A tibble: 4 × 6
    ##   Language Dataset Meaning Form   Trill R_type  
    ##   <chr>    <chr>   <chr>   <chr>  <chr> <chr>   
    ## 1 Nepali   CLICS   smooth  cillā  yes   no trill
    ## 2 Nepali   CLICS   rough   khasro yes   no trill
    ## 3 Nepali   CLICS   smooth  ciplō  yes   no trill
    ## 4 Nepali   CLICS   rough   khasrō yes   no trill

``` r
show_comments('Nepali')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments                                     
    ##   <chr>    <chr>   <chr>                                        
    ## 1 Nepali   IDS     "also wikipedia says \"r is always a trill\""
    ## 2 Nepali   Google   <NA>

``` r
show_iso('Nepali')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Nepali   nep      nep

Trill : yes  
R_type : no trill

Dataset :  
IDS, also wikipedia says “r is always a trill”  
Google, NA

Phoible :  
Pour Eastern Pahari :  
<https://phoible.org/inventories/view/488>  
- ɾ pas d’allophone

Pour Nepali :  
<https://phoible.org/languages/nepa1254>  
- RA r pas d’allophone; ɽ pas d’allophone; ɽ̤ pas d’allophone  
- UZ ŋ allophones ŋ ɽ̃ ; rː allophone rː ; r allophones r ɾ  
- EA r pas d’allophone

Bandhu, C. M. and Dahal, B. M. and Holzhausen, A. and Hale, A. 1971.
Nepali Segmental Phonology. Kirtipur: Summer Institute of Linguistics,
Tribhuvan University. \[Pas cherché\]

Srivastava, Dayanand. 1962. Nepali Language: Its History and
development. Calcutta: Calcutta University Press. \[Pas cherché\]

Khatiwada, Rajesh. 2009. Illustrations of the IPA: Nepali. Journal of
the International Phonetic Association 39. 373–380. Cambridge University
Press.  
- Page 373 : Tap of flap r  
- Page 374 : r rat ‘night’^3 \[3: \[r\] and \[ɾ\] are two allophones of
/r/ (see the subsection on rhotics below).\]  
- Oage 377 : Rhotics  
Nepali /r/ has been described as a tap \[\|\] in intervocalic position
and as a trill \[r\] elsewhere (Pokharel 1989). In our data, it is
realized as a short (two- or three-tap) trill \[r\] or sometimes as a
single tap \[\|\] at the beginning of the word, but as a single tap or
flap when intervocalic or at the end of the word. These differences may
reflect inter-speaker variation. We take the word-initial trill variant
\[r\] as the basic variant of this phoneme, consistent with our practice
elsewhere, but it should be kept in mind that the Nepali trills are
weaker than Spanish or Italian trills. Geminate /r/ is fully trilled
and, in this case, the trill is quite strong.  
/rato/ ‘red’ /tir/ ‘arrow’  
/tara/ ‘star’ /sar/ ‘move’  
/tarro/ ‘bitter’  
A retroflex flap \[ɽ\] also occurs as a postvocalic variant of /ɖ/ as
described above.

Riccardi, Theodore. 2003. Nepali. In Cardona, George and Jain, Dhanesh
(eds.), The Indo-Aryan Languages, 538-579. London &New York:
Routledge. - Page 602 : The consonantal phonemes are as follows:
Semivowels: y r l v  
- Page 603 : With regard to the semi-vowels, /y/ and /v/ are glides, /r/
is lightly trilled and is in phonemic contrast to the flap /r./ as
described above.  
tap: /ramro/ ‘good’ : /d.amro/ ‘blister’

Wikipedia :  
<https://en.wikipedia.org/wiki/Nepali_language>  
- Rhotic r ⟨र⟩

<https://en.wikipedia.org/wiki/Nepali_phonology>  
- /ɖ ɖʱ/ are flapped (\[ɽ\]) in postvocalic position. /r/ is usually a
trill \[r\] but may be a tap \[ɾ\] in intervocalic position.\[8\]\[9\]  
- Typically, sounds transcribed with the retroflex symbols ⟨ʈ, ʈʰ, ɖ,
ɖʱ, ɽ, ɳ, ɽ̃⟩ are not purely retroflex \[ʈ, ʈʰ, ɖ, ɖʱ, ɽ, ɳ, ɽ̃\] but
apical postalveolar \[t̠, t̠ʰ, d̠, d̠ʱ, ɾ̠, n̠, ɾ̠̃\]. Some speakers may use
purely retroflex sounds after /u/ and /a/, but other speakers use the
apical articulation in all positions.\[10\]

Glottologue :  
<https://glottolog.org/resource/languoid/id/east1436>

<https://glottolog.org/resource/languoid/id/nepa1254>

Autres sources :

<https://clics.clld.org/languages/diacl-48800>  
Recherches : r -\> 26 entries (filtered from 106 total entries)  
rr -\> 0 entries (filtered from 106 total entries)  
^r -\> 5 entries (filtered from 106 total entries)  
r\$ -\> 4 entries (filtered from 106 total entries)  
l -\> 16 entries (filtered from 106 total entries)

<https://clics.clld.org/languages/halenepal-Nepali>  
Recherches : r -\> 296 entries (filtered from 876 total entries)  
rr -\> 0 entries (filtered from 876 total entries)  
^r -\> 14 entries (filtered from 876 total entries)  
r\$ -\> 36 entries (filtered from 876 total entries)  
l -\> 168 entries (filtered from 876 total entries)

<https://clics.clld.org/languages/ids-706>  
Recherches : r -\> 312 entries (filtered from 1,098 total entries)  
rr -\> 0 entries (filtered from 1,098 total entries)  
^r -\> 29 entries (filtered from 1,098 total entries)  
r\$ -\> 29 entries (filtered from 1,098 total entries)  
l -\> 190 entries (filtered from 1,098 total entries)

``` r
show_lang('Nepali')$Form
```

    ## [1] "cillā"  "khasro" "ciplō"  "khasrō"

Soit OUT soit other, mais à cause du contraste, on prend la décision de
mettre OUT.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Nepali",'OUT',revision))
```

## Judeo-Tat

``` r
show_lang('Judeo-Tat')
```

    ## # A tibble: 2 × 6
    ##   Language  Dataset Meaning Form         Trill R_type  
    ##   <chr>     <chr>   <chr>   <chr>        <chr> <chr>   
    ## 1 Judeo-Tat CLICS   smooth  šumol        no    no trill
    ## 2 Judeo-Tat CLICS   rough   bix̌urmetlüye no    no trill

``` r
show_comments('Judeo-Tat')
```

    ## # A tibble: 1 × 3
    ##   Language  Dataset Comments
    ##   <chr>     <chr>   <chr>   
    ## 1 Judeo-Tat CLICS   wiki

``` r
show_iso('Judeo-Tat')
```

    ## # A tibble: 1 × 3
    ##   Language  ISO_code Phoible_code
    ##   <chr>     <chr>    <chr>       
    ## 1 Judeo-Tat jdt      <NA>

Trill : no R_type : no trill

Dataset :  
CLICS, wiki

Phoible :  
<https://phoible.org/inventories/view/2284> - ɾ pas d’allophone

Дадашев, Михаил. 2006. Русско-татский (горско-еврейский) словарь Гофноме
эз уруси э жугьури. Москва: Собрание. - Dictionnaire parcouru avec
Google Traduction sur mon iPhone, pas de mention explicite de la
réalisation du “r”

Authier, Gilles. 2012. Grammaire juhuri, ou judéo-tat, langue iranienne
des Juifs du Caucase de l’est. Wiesbaden: Reichert. 336pp. - Page 32 :
L’inventaire des consonnes est presque identique à celui du persan et de
l’azéri : \[PS : pour les deux langues c’est compliqué on a mis trilled
mais vraisemblament tap/flap\] palatales r, y - Page 33 : Rhotacisme et
chute de d : après voyelles, d passe à r : vor ‘vent’ cf. persan bād,
juhūd = juhur ‘Juif’, yād = yor ‘mémoire’, dāmād = domor ‘gendre’, ādīna
= orine ‘semaine’, rūd = ruri ‘boyau’, būdan = bire ‘être’, gudāştan =
giroşde ‘passer’

Wikipedia :  
<https://en.wikipedia.org/wiki/Judeo-Tat> - Flap ɾ

Glottologue :  
<https://glottolog.org/resource/languoid/id/jude1256>

Autres sources :

<https://clics.clld.org/languages/ids-74> Recherches : r -\> 947 entries
(filtered from 2,217 total entries) rr -\> 13 entries (filtered from
2,217 total entries) ^r -\> 44 entries (filtered from 2,217 total
entries) r\$ -\> 173 entries (filtered from 2,217 total entries) l -\>
468 entries (filtered from 2,217 total entries)

``` r
show_lang('Judeo-Tat')$Form
```

    ## [1] "šumol"        "bix̌urmetlüye"

Possibilité du trill et du tap dans la langue ? On laisse en other ici.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Judeo-Tat",'other',revision))
```

## Portuguese

``` r
show_lang('Portuguese')
```

    ## # A tibble: 3 × 6
    ##   Language   Dataset Meaning Form   Trill R_type  
    ##   <chr>      <chr>   <chr>   <chr>  <chr> <chr>   
    ## 1 Portuguese CLICS   smooth  liso   no    no trill
    ## 2 Portuguese CLICS   smooth  macio  no    no trill
    ## 3 Portuguese CLICS   rough   áspero no    no trill

``` r
show_comments('Portuguese')
```

    ## # A tibble: 1 × 3
    ##   Language   Dataset Comments
    ##   <chr>      <chr>   <chr>   
    ## 1 Portuguese IDS     flap

``` r
show_iso('Portuguese')
```

    ## # A tibble: 1 × 3
    ##   Language   ISO_code Phoible_code
    ##   <chr>      <chr>    <chr>       
    ## 1 Portuguese por      por

Trill : no  
R_type : no trill

Dataset :  
IDS, flap

Phoible :  
<https://phoible.org/languages/port1283>  
4 inventaires :  
- SPA ɾ allophone ɾ ; x allophones x̟ ʀ̥ x  
- UZ2206 ɾ allophone ɾ ; ʁ allophones ʁ ʁ̥  
- UZ 2207 r allophone r ɣ h ; ɾ allophone ɾ  
- EA ɾ pas d’allophone; ʁ pas d’allophone

Camara, J. Mattoso. 1972. The Portuguese Language (Translated by Anthony
J. Naro. Chicago: University of Chicago Press. \[pas cherché\]

Head, Brian Franklin. 1964. A Comparison of the Segmental Phonology of
Lisbon and Rio De Janeiro. (Doctoral dissertation, University of Texas,
Austin). \[pas cherché\]

Cruz-Ferreira, Madalena. 1995. Illustrations of the IPA: European
Portuguese. Journal of the International Phonetic Association 25. 90–94.
Cambridge University Press.  
- Page 91 : Fricative ʁ  
Tap ɾ

              ɾ 'piɾa pira 'pyre'  
              ʁ 'ʁatu rato 'mouse ' (m)   

- Page 92 : /ɾ/ does not occur in word-initial position

Barbosa, Plínio A. and Albano, Eleonora C. 2004. Illustrations of the
IPA: Brazilian Portuguese. Journal of the International Phonetic
Association 34. 227–232. Cambridge University Press.  
- Page 228 : Tap ɾ  
Fricative ɣ

               ɣ  kaɣu carro 'car' ɾ kaɾu caro 'expensive"  
               

In intervocalic position there is a phonemic contrast between the
so-called strong and weak ‘r’s’, /r/ vs. /\|/. While the latter phoneme
is always realised as a tap (\[ɾ\]) in this position, the former varies
considerably across the Brazilian territory. In the variety described
here, it is a velar fricative, usually voiced between voiced sounds and
voiceless elsewhere (which justifies the use of the symbol \[ɣ\] to
indicate the realisation of /r/). It can nevertheless be realised as a
glottal fricative, usually in weak prosodic positions, particularly in
casual speech.  
In coda position, four archiphonemes are allowed in BP: /R/, /S/, /N/,
/L/ (cf. D’Angelis’ (2002) reanalysis of Cˆamara’s (1970) treatment of
the issue).  
As regards /R/, its phonetic realisation is extremely variable in BP,
and it may even include a tap. Within the framework of Labovian
sociolinguistics, Callou, Moraes & Leite (1996) showed, on the basis of
phonetically transcribed data, that an isogloss for /R/ realisations can
be established between the States of S˜ao Paulo and Rio de Janeiro: a
higher frequency of the tap from S˜ao Paulo southwards, as opposed to a
higher frequency of the velar and glottal fricatives from Rio de Janeiro
northwards (this result still awaits confirmation by means of
instrumental phonetic analysis). The tap is the preferred realisation
for /R/ in the variety presented here. In infinitives, however, this
sound usually drops word-finally except if followed by a vowel.

Emiliano, António. 2009. Fonética do Portugu^es Europeu. Descricao e
Transcricao. \[pas cherché\]

Wikipedia :  
- Fricative ʁ  
Flap ɾ  
- The consonant hereafter denoted as /ʁ/ has a variety of realizations
depending on dialect. In Europe, it is typically a uvular trill \[ʀ\];
however, a pronunciation as a voiced uvular fricative \[ʁ\] may be
becoming dominant in urban areas. There is also a realization as a
voiceless uvular fricative \[χ\], and the original pronunciation as an
alveolar trill \[r\] also remains very common in various
dialects.\[166\] A common realization of the word-initial /r/ in the
Lisbon accent is a voiced uvular fricative trill \[ʀ̝\].\[167\] In
Brazil, /ʁ/ can be velar, uvular, or glottal and may be voiceless unless
between voiced sounds.\[168\] It is usually pronounced as a voiceless
velar fricative \[x\], a voiceless glottal fricative \[h\] or voiceless
uvular fricative \[χ\]. See also Guttural R in Portuguese.

Mateus, Maria Helena; d’Andrade, Ernesto (2000). The Phonology of
Portuguese. Oxford University Press. ISBN 978-0-19-823581-1. \[pas
cherché\]  
Grønnum, Nina (2005). Fonetik og fonologi, Almen og Dansk (3rd ed.).
Copenhagen: Akademisk Forlag. ISBN 978-87-500-3865-8. \[pas cherché\]  
Barbosa, Plínio A.; Albano, Eleonora C. (2004). “Brazilian Portuguese”.
Journal of the International Phonetic Association. 34 (2): 227–232.
<doi:10.1017/S0025100304001756>. \[pas cherché\]

Glottologue :  
<https://glottolog.org/resource/languoid/id/port1283>

Autres sources :

<https://clics.clld.org/languages/diacl-54300>  
Recherches : r -\> 147 entries (filtered from 337 total entries)  
rr -\> 20 entries (filtered from 337 total entries)  
^r -\> 8 entries (filtered from 337 total entries)  
r\$ -\> 33 entries (filtered from 337 total entries)  
l -\> 79 entries (filtered from 337 total entries)

<https://clics.clld.org/languages/ids-178>  
Recherches : r -\> 957 entries (filtered from 1,683 total entries)  
rr -\> 50 entries (filtered from 1,683 total entries)  
^r -\> 72 entries (filtered from 1,683 total entries)  
r\$ -\> 414 entries (filtered from 1,683 total entries)  
l -\> 359 entries (filtered from 1,683 total entries)

<https://clics.clld.org/languages/northeuralex-por>  
Recherches : r -\> 675 entries (filtered from 1,162 total entries)  
rr -\> 2 entries (filtered from 1,162 total entries)  
^r -\> 49 entries (filtered from 1,162 total entries)  
r\$ -\> 345 entries (filtered from 1,162 total entries)  
l -\> 178 entries (filtered from 1,162 total entries)  
Form in source :  
ʀ -\> 59 entries (filtered from 1,162 total entries)

``` r
show_lang('Portuguese')$Form
```

    ## [1] "liso"   "macio"  "áspero"

A cause de la présence de deux r on mets OUT.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Portuguese",'OUT',revision))
```

## Romanian

``` r
show_lang('Romanian')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form  Trill R_type
    ##   <chr>    <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Romanian CLICS   smooth  neted yes   trill 
    ## 2 Romanian CLICS   rough   aspru yes   trill 
    ## 3 Romanian CLICS   smooth  lins  yes   trill

``` r
show_comments('Romanian')
```

    ## # A tibble: 3 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Romanian IDS     <NA>    
    ## 2 Romanian Google  <NA>    
    ## 3 Romanian WOLD    <NA>

``` r
show_iso('Romanian')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Romanian ron      ron

Trill : yes  
R_type : trill

Dataset :  
IDS, NA  
Google, NA  
WOLD, NA

Phoible :  
<https://phoible.org/languages/roma1327>  
3 inventaires :  
- SPA ɾ allophones ɾ r  
- UPSID r̪ pas d’allophone  
- EA ɾ̪ pas d’allophone

Ruhlen, Merritt. 1973. Rumanian Phonology. (Doctoral dissertation,
Stanford University). \[pas cherché\]

Agard, Frederick B. 1958. Structural Sketch of Rumanian. (Supplement to
Language Monographs, 26.) Baltimore: Linguistic Society of America.
\[pas cherché\]

Tataru, A. 1978. The Pronunciation of Rumanian and English: Two Basic
Contrastive Analyses. Frankfurt am Main: Haag and Herchen Verlag. \[pas
cherché\]

Chitoran, Ioana. 2002. The phonology of Romanian: a constraint-based
approach. (56.) Walter de Gruyter.  
- Page 10 : approximants r  
The approximant /r/ is generally realized as a flap \[ɾ\] and
occasionally as a trill in word initial position.

Wikipedia :  
<https://en.wikipedia.org/wiki/Romanian_language>

<https://en.wikipedia.org/wiki/Romanian_phonology>  
- Trill r ⟨r⟩

<https://en.wikipedia.org/wiki/Talk:Romanian_phonology>  
- R I am by no means a specialist in phonetics, but as a native Romanian
speaker, I just realized that I pronounce ‘r’ more like what seems to me
to be an alveolar tap. I think this is more general though. Shouldn’t we
then transcribe the Romanian ‘r’ phonetically as ‘ɾ’, since ‘r’ in IPA
would be an alveolar trill? Waardijner 00:16, 6 June 2007 (UTC)  
There is a basic difference between trills and taps (see Trill consonant
and Flap consonant for details). Romanian /r/ is a trill, because the
tongue is just placed against the alveolar ridge and it’s the air flow
that moves the tongue, whereas in a tap the tongue is moved by its own
muscles. Romanian /r/ is indeed most of the time pronounced with a
single period, which makes it similar to a tap, but trills don’t
necessarily have more than one period. Besides, Romanian /r/ can be
pronounced with more than one period — that is, it can be an
unquestionable alveolar trill — without a change in meaning (thus
single- and multiple-period realizations of /r/ are allophones in
Romanian), as opposed for example to Spanish where there are minimal
pairs such as caro vs. carro. Note that taps can never have more than
one period.  
All works on Romanian phonology and phonetics use the symbol r for this
sound, both phonetically and phonologically. — AdiJapan ☎ 04:14, 6 June
2007 (UTC)  
Thanks for explaining that. It looks like I need a lot more practising.
I wonder still: were somebody to use a ɾ, would it be noticeable in any
immediate way to other speakers? Waardijner 13:00, 6 June 2007 (UTC)  
Not really, at least to most listeners. If a speaker used an alveolar
flap instead of a trill, probably the possibility of confusion between
/r/ and /l/ would be slightly higher (because /l/ is a sort of flap), so
rac might sound a bit like lac, but otherwise you could still have a
comfortable conversation with that speaker. Personally I haven’t met
anyone to pronounce /r/ as a flap though. Actually many of us have some
kind of small speech defects, but we get used to ignoring them, both as
speakers and as listeners. As you probably know, quite a few Romanians
use the uvular trill for /r/ (Nicolae Ceauşescu and Nicu Alifantis come
to mind), and other than sounding a bit “funny”, phonologically speaking
there is no problem. — AdiJapan ☎ 14:57, 6 June 2007 (UTC)  
I agree (as a native speaker), though I suspect it’s harder to pronounce
consonant sequences like “tr” with a tap than with a trill. (I can’t
seem to figure out how to do it myself; trying to do it leads to a trill
by instinct. If it’s possible, it might sound a bit strange.)

Glottologue :  
<https://glottolog.org/resource/languoid/id/roma1327>

Autres sources :

<https://clics.clld.org/languages/ids-179>  
Recherches : r -\> 569 entries (filtered from 1,229 total entries)  
^r -\> 37 entries (filtered from 1,229 total entries)  
r\$ -\> 80 entries (filtered from 1,229 total entries)  
l -\> 234 entries (filtered from 1,229 total entries)

<https://clics.clld.org/languages/northeuralex-ron>  
Recherches : r -\> 512 entries (filtered from 1,197 total entries)  
^r -\> 42 entries (filtered from 1,197 total entries)  
r\$ -\> 48 entries (filtered from 1,197 total entries)  
l -\> 218 entries (filtered from 1,197 total entries)

``` r
show_lang('Romanian')$Form
```

    ## [1] "neted" "aspru" "lins"

La discussion de wiki est intéressante, on laisse trill.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Romanian",'trilled',revision))
```

## Breton

``` r
show_lang('Breton')
```

    ## # A tibble: 4 × 6
    ##   Language Dataset Meaning Form  Trill R_type   
    ##   <chr>    <chr>   <chr>   <chr> <chr> <chr>    
    ## 1 Breton   CLICS   smooth  flour yes   no rhotic
    ## 2 Breton   CLICS   rough   rust  yes   no rhotic
    ## 3 Breton   CLICS   smooth  lint  yes   no rhotic
    ## 4 Breton   CLICS   smooth  lɛvn  yes   no rhotic

``` r
show_comments('Breton')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Breton   IDS     <NA>

``` r
show_iso('Breton')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Breton   bre      bre

Trill : yes  
R_type : no rhotic

Dataset :  
IDS, NA

Phoible :  
3 inventaires : <https://phoible.org/languages/bret1244> - SPA r
allophones ʁ r ɾ r̩ ɹ - UPSID Null - EA r pas d’allophone; r̥ pas
d’allophonew

Ternes, Elmer. 1970. Grammaire Structurale du Breton de l’Ile de Groix.
Heidelberg: Carl Winter. - Page 1 : Vibrante dental r - Page 5 : l - r
lout “part, partie” - rout “oue r - s raj”raie (poisson)” - saj “robe” -
Page 18 : r Ce phoneme comprend deux allophones principaux, à points
d’articulation entièrement différents: 1° fricative postdorso-uvulaire
sonore \[ʁ\]. Cet allophone se rouve dans la seule position devant /l/.
\[examples\] 2° vibrante apico-alvéolaire : \[r\]. Cet allophone
principal se trouve dans toutes les positions sauf devant /l/. En
position finale (devant \#), /r/ comprend un à deux battements après
lesquels une sorte de détente \[etc.\] - Page 19 : Dans les positions
/CrəC/ et, plus rarement, /CərC/, /ə/ tend à se perdre, surtout dans un
débit rapide. On a dans ces cas un \[r̩\] formant syllabe, dont la force
articulatoire est en général assez importante. Il comporte un à trois
battements. La variation entre \[rə\], \[ər\] et \[r̩\] est libre et
dépend en partie de la rapidité du débit. \[examples\] Les remarques
suivantes sont d’un caractères général. Elles concernent la force
articulatoire et le nombre des vibrations de \[r\] dans certaines
positions par rapport à certaines autres positions. Les fluctuations de
prononciation sont pourtant considérables. Les indications suivantes ne
sont que la moyenne des réalisations possibles. - En position initiale
(après \#), \[r\] est généralement plus fortement articulé (deux à trois
vibrations) que dans toutes les autres positions. \[examples\] - Page 20
: Dans les syllabes à accent primaire ou secondaire (pg. 3.4.), \[r\]
est plus fortement articulé (et comprend plus de vibrations) que dans
des syllabes non-accentuées. P.ex. le premier \[r\] est plus faible que
le second dans (la noation de l’accent est non-phonémique):
u\[r\]gà\[r\]tén “une carte” -Plus le \[r\] se trouve éloigné de
l’accent primaire, plus son articulation est faible (et moins il
comprend de vibrations). P.ex. le \[r\] du premier ùpt est plus fort que
celui du second (la notation de l’accent est non-phonémique):
ə\[r\]gàrtén “la carte” ə\[r\]gàləxǿyl “le tapage (que font les
enfants)” -La force articulatoire de \[r\] dans le groupe /rC/ dépend de
la nature de C: Elle est relativement forte devant /p, t, k, b, d, g/
(deux à trois vibrations) Elle est moyenne devant /m, n, w’, w/ (une à
trois vibrations) Elle est relativement faible devant /c^v, j^v, f, s,
s^v, x, v, z, z^v, j/ une à deux vibrations /r/ ne forme pas de groupes
avec /gn, ng,w\~/. Pour /rl/, v. 1° ci-dessus (p.18). Exemples en
syllabe à accent primaire: \[examples\] Exemples en syllabe à accent
secondaire: \[examples\] - Page 21 : Exemples en syllabe non-accentuée
(articulation plus faible, moins de vibrations) : \[examples\] - La
force articulatoire de \[r\] dans le groupe /Cr/ est indépendante de la
nature de C. Elle est relativement forte (deux à trois vibrations). Elle
est un peu moins forte dans le groupe /CCr/ (une à deux vibrations).
\[examples\] Dans les positions suivantes, /r/ comprend très rarement
plus d’un seul battement: \[ɾ\]. Par contre, ce dernier peut varier
librement, surtout dans un début rapide avec une fricative alvéolaire
sonore ressemblant au \[ɹ\] anglais, mais avec très peu de frication,
formant presque une semi-voyelle \[ɹ̯\]. - Cette variation libre avec
\[ɹ̯\] est particulièrement fréquente devant /xl/ et /xr/. Dans ces cas,
\[ɹ̯\] peut même varier librement avec \[ø\], s’amalgamant avec le /x/
suivant dans une fricative glottale sonore \[ɦ\] : \[examples\] - Page
22 : En syllable non-accentué se trouvant entre deux syllabes dont la
précédente porte l’accent secondaire et la suivante l’accent primaire
(la noation de l’accent est non-phonémique: v. pg. 3.4): \[examples\] -
Dans une position relativement éloignée de l’accent primaire suivant,
surtout en position préconsonantique (la notation de l’accent est
non-phonémique): \[examples\] -En position intervolcaique (rare):
\[examples\]

Bothorel, A. 1982. Etude Phonétique et Phonologique du Breton Parlé à
Argol (Finistere-Sud). Lille: Atelier National Reproduction des Thèses,
Université Lille III. \[pas cherché\]

Iosad, Pavel. 2013. Representation and variation in substance-free
phonology: A case study in Celtic. (Doctoral dissertation, Universitetet
i Tromso). \[pas cherché\]

Wikipedia :

- Trill r /r/ (r /ʁ/) Approximant central (r /ɹ/) The pronunciation of
  the letter ⟨r⟩ varies nowadays: \[ʁ\] is used in the French-influenced
  standard language and, generally speaking, in the central parts of
  Lower Brittany (including the south of Trégor, the west of Vannetais
  and virtually all parts of Cornouaille) whereas \[r\] is the common
  realisation in Léon and often in the Haut-Vannetais dialect of central
  Morbihan (in and around the city of Vannes and the Pays de Pontivy),
  though in rapid speech mostly a tapped \[ɾ\] occurs. In the other
  regions of Trégor \[ɾ\] or even \[ɹ\] may be found.

Glottologue :  
<https://glottolog.org/resource/languoid/id/bret1244>

Autres sources :

<https://clics.clld.org/languages/ids-183> Recherches : r -\> 711
entries (filtered from 1,768 total entries) rr -\> 18 entries (filtered
from 1,768 total entries) ^r -\> 56 entries (filtered from 1,768 total
entries) r\$ -\> 188 entries (filtered from 1,768 total entries) l -\>
547 entries (filtered from 1,768 total entries)

<https://clics.clld.org/languages/northeuralex-ron>  
Recherches : r -\> 374 entries (filtered from 973 total entries)  
rr -\> 0 entries (filtered from 973 total entries) ^r -\> 23 entries
(filtered from 973 total entries) r\$ -\> 84 entries (filtered from 973
total entries) l -\> 271 entries (filtered from 973 total entries) Form
in source : ʀ -\> 374 entries (filtered from 973 total entries)

``` r
show_lang('Breton')$Form
```

    ## [1] "flour" "rust"  "lint"  "lɛvn"

La description de l’allophonie est précieuse.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Breton",'trilled',revision))
```

## Polish

``` r
show_lang('Polish')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form     Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>    <chr> <chr> 
    ## 1 Polish   CLICS   smooth  gładki   yes   trill 
    ## 2 Polish   CLICS   rough   chropawy yes   trill

``` r
show_comments('Polish')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Polish   IDS     <NA>    
    ## 2 Polish   Google  <NA>

``` r
show_iso('Polish')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Polish   pol      pol

Trill : yes  
R_type : trill

Dataset :  
IDS, NA  
Google, NA

Phoible :  
<https://phoible.org/languages/poli1260>  
2 inventaires :  
- PH r allophone r  
- EA Null

Jassem, Wiktor. 2003. Illustrations of the IPA: Polish. Journal of the
International Phonetic Association 33. 103–107. Cambridge University
Press.  
- Page 103 : Flap/Trill r  
- Page 104 : l : r  
bul bol ‘pain’ bur bor ‘forest’

Ostaszewska, Danuta and Tambor, Jolanta. 2000. Fonetyka i fonologia
wsp’olczesnego jkezyka polskiego. Wydawnictwo Naukowe PWN.  
\[traduit sur Google Traduction depuis mon iPhone12\]  
- Page 12 : Applications of phonetics  
\[…\]  
3. Removal of speech defects - speech therapy (from the Greek lógos -
word, speech, and paideía - upbringing), which is not synonymous with
prophylaxis and skilful correction of specific features of children’s
pronunciation, such as lisping, limping, inability to pronounce the
sound \[r\], etc. - through which every child goes through (normal
speech development should be completed around 5-6 years of age^2);
speech therapy removes more permanent disorders.  
\[2\] G. Demel, Speech defects, Warsaw 1979, p. 11.  
- Page 22 : The alphabet adopted International alphabet from the
Examples according to Examples by alphabet from the Spelling  
in the textbook Dictionary of Polish Pronunciation the alphabet adopted
in the Polish Pronunciation Dictionary  
textbook  
r r \[rover\] \[rɔver\] rower  
r̹ r̥ \[katr̹\] \[katr̥\] kadr  
rˈ r, \[rˈ ikša\] \[r,ikša\] riksza  
r̹ˈ r̥, \[mẽntr̹ˈ kẽm\] \[mɛ̃ntr̥,cɛ̃m\] medrkiem

- Page 34 : Alveolar \[Dziąsłowe\]  
  hard \[twar\] \[NB twarda?\] Soft \[zmięk\]  
  voiced \[dźwięczne\] voiceless \[bezdźwięczne\] voiced \[dźwięczne\]
  voiceless \[bezdźwięczne\]  
  Trill \[Drżące\] r r̹ rˈ r̹ˈ

- Page 61 : je ne traduit pas mais ça parle de l’écritre et de la
  correspondance avec la phonétique. Le r ne veut pas forcément dire r,
  mais peut vouloir être associé au z ou au s  

- Page 119 : Phoneme r: main variant \[r\]: \[advice\], \[year\],
  \[work\], \[porvać\], \[circus\];  
  secondary variants \[rˈ̹\]: \[r’ikša\], \[dar’ja\], \[star’će\],
  \[gar’nitur\];  
  \[r̹\]: \[ientika\], \[v’iatr\]  
  \[r̹ˈ\]: \[ientr’kem\]. \], \[utrfal’ić\], \[katṛ\];

                        Distinctive features: tremor; phonologically irrelevant features: sonority (voicedness in secondary variants), gingivality.  

Wikipedia :  
<https://en.wikipedia.org/wiki/Polish_language>  
- Tap/trill ɾ

<https://en.wikipedia.org/wiki/Polish_phonology>  
- Tap/trill r ^ \[d\]  
- \[d\] The /r/ phoneme is alternatively transcribed /ɾ/.  
- /r/ is apical alveolar. It has been traditionally classified as a
trill \[r̺\], with a tap \[ɾ̺\] supposedly only occurring as an allophone
or in fast speech.\[77\] However, more recent studies show that /r/ is
predominantly realized as a tap \[ɾ̺\], sometimes as an approximant or a
fricative, but almost never as a trill.\[78\]\[79\] One study found that
in an intervocalic context a trilled \[r\] occurs in less than 3% of
cases, while a tapped \[ɾ\] occurred in approximately 95% of cases.
Another study by the same researcher showed that in a postconsonantal
position, /r/ is realized as a tapped \[ɾ\] in 80–90% of cases, while
trilled \[r\] occurs in just 1.5% of articulations.\[80\] A palatalized
laminal tap \[ɾ̻ʲ\] is used before /i, j/ in recent loanwords.\[76\]  
\[77\] Rocławski, Bronisław (1976), Zarys fonologii, fonetyki,
fonotaktyki i fonostatystyki współczesnego języka polskiego \[Outline of
phonology, phonetics, phonotactics and phonostatistics of the
contemporary Polish language\] (in Polish), Wydawnictwo Uczelniane
Uniwersytetu Gdańskiego  
\[78\] Szpyra-Kozłowska, Jolanta (2018). “The rhotic in fake and
authentic Polish-accented English”. Lublin Studies in Modern Languages
and Literature. 42 (1): 81. <doi:10.17951/lsmll.2018.42.1.81>. ISSN
2450-4580.  
\[79\] “On the phonetic instability of the Polish rhotic /r/ \| Request
PDF”. ResearchGate. Retrieved 2019-09-09.  
\[80\] “Further analysis of the articulation of /r/ in Polish – The
postconsonantal position”. ResearchGate. Retrieved 2019-09-09.  
\[76\] Sawicka, Irena (1995), “Fonologia” \[Phonology\], in Wróbel,
Henryk (ed.), Gramatyka współczesnego języka polskiego. Fonetyka i
fonologia \[Grammar of the contemporary Polish language. Phonetics and
phonology\] (in Polish), Kraków: Wydawnictwo Instytut Języka Polskiego
PAN, pp. 105–195

Glottologue :  
<https://glottolog.org/resource/languoid/id/poli1260>

Autres sources :

<https://clics.clld.org/languages/diacl-37900>  
Recherches : r -\> 91 entries (filtered from 302 total entries)  
^r -\> 12 entries (filtered from 302 total entries)  
r\$ -\> 7 entries (filtered from 302 total entries)  
l -\> 78 entries (filtered from 302 total entries)

<https://clics.clld.org/languages/ids-203>  
Recherches : r -\> 664 entries (filtered from 2,042 total entries)  
^r -\> 79 entries (filtered from 2,042 total entries)  
r\$ -\> 25 entries (filtered from 2,042 total entries)  
l -\> 554 entries (filtered from 2,042 total entries)

<https://clics.clld.org/languages/ids-183>  
Recherches : r -\> 240 entries (filtered from 1,049 total entries)  
^r -\> 35 entries (filtered from 1,049 total entries)  
r\$ -\> 12 entries (filtered from 1,049 total entries)  
l -\> 100 entries (filtered from 1,049 total entries)

``` r
show_lang('Polish')$Form
```

    ## [1] "gładki"   "chropawy"

On laisse trilled quand même.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Polish",'trilled',revision))
```

## Russian

``` r
show_lang('Russian')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form    Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>   <chr> <chr> 
    ## 1 Russian  CLICS   smooth  gladkij yes   trill 
    ## 2 Russian  CLICS   rough   grubyj  yes   trill

``` r
show_comments('Russian')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Russian  IDS     <NA>    
    ## 2 Russian  Google  <NA>

``` r
show_iso('Russian')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Russian  rus      rus

Trill : yes  
R_type : trill

Dataset :  
IDS, NA  
Google, NA

Phoible :  
<https://phoible.org/languages/russ1263>  
3 inventaires :  
- SPA r allophones r̥ r ɾ; rʲ allophones r̥ʲ ɾʲ rʲ  
- UPSID r̪ pas d’allophones; r̪ʲ pas d’allophones  
- EA r pas d’allophones; ɾʲ pas d’allophones

Jones, Daniel and Ward, Dennis. 1969. The Phonetics of Russian.
Cambridge: Cambridge University Press.  
- De la page 175 à 188

Halle, Morris. 1959. The Sound Pattern of Russian. The Hague: Mouton.  
- Page 120 : The Russian /r/ and /r,/ are usually pronounced with a
trill. These trills are about 40 to 70 msecs long, and not more than one
is needed to give proper identification (see Fig. V-7). The sound is
thus made up of two separate parts, one of which is primarily vowel-like
and the other either a silence or a noise. In order to perceive a good
trilled \[r\] it is necessary that both parts be present. There are,
however, examples where no trill can be observed, and the phoneme
contains much noise, (see Fig. V-6).  
In the vocalic part of \[r\], the formants are less clearly defined than
in the vowels, and the intensity of the frequency band between 600 cps
and 1500 cps is much greater relative to the first formant than in the
nasals and in most vowels. In a few cases we observed a peak at 1500
cps; there were, however, other cases in which no energy maxima in this
region could be found. The best way to describe the vocalic parts of the
\[r\] is to say that they resemble most the neutral vowel \[A\],
strongly influenced by the adjacent phonemes (cf., Fig. V-7).  
In view of the evolution of Common Slavic palatal \[r\] into strident /r
chapeau inversé/ as in Czech and subsequently into /z chapeau inversé/
as in Polish, Slavicists will be interested to learn that we found
several cases in which the segment corresponding to the closure (noise)
when played in isolation gave a distinct \[z chapeau inversé\]
impression. All these were instances of the sharped /r,/.

- Page 121 : Fig. V-6. Sonagrams o f the syllable /ir,/. Subject K . The
  /r,/ is not trilled here. it has a very strong component as shown by
  the irregularly striated high frequency region.  
  Fig. V-7. Sonagrams of the syllable /re/. Subject Κ (left) and D
  (right). Subject Κ has a single trill; Subject D has three distinct
  trills. Note also that /r/ as pronounced by both subjects has evenly
  spaced formants characteristic of the neutral vowel \[λ\].

Yanushevskaya, Irena and Bunčić, Daniel. 2015. Russian. Journal of the
International Phonetic Association 45. 221–228. Cambridge Univ Press.  
- Page 222 : Trill r rʲ  
r /ˈrat/ rad ‘(am etc.) glad’  
rʲ /ˈrʲat/ rjad ’row”  
- Page 223 : /r/ is an alveolar trill in careful pronunciation, but its
palatalised counterpart /rʲ/ is usually realised as a tap \[ɾʲ\].

Wikipedia :  
<https://en.wikipedia.org/wiki/Russian_language>  
- Trill r rʲ

<https://en.wikipedia.org/wiki/Russian_phonology>  
- Trill rʲ r  
- Hard /r/ is postalveolar, typically a trill \[r̠\].\[64\]  
Soft /rʲ/ is an apical dental trill \[r̪ʲ\], usually with only a single
contact.\[64\]

\[64\] Skalozub, Larisa (1963), Palatogrammy i Rentgenogrammy Soglasnyx
Fonem Russkogo Literaturnogo Jazyka, Izdatelstvo Kievskogo Universiteta
cited in Ladefoged, Peter; Maddieson, Ian (1996), The Sounds of the
World’s Languages, Blackwell Publishing, ISBN 0-631-19815-6

Glottologue :  
<https://glottolog.org/resource/languoid/id/russ1263>

Autres sources :

<https://clics.clld.org/languages/diacl-37000>  
Recherches : r -\> 93 entries (filtered from 314 total entries)  
rj -\> 3 entries (filtered from 314 total entries)  
ri -\> 5 entries (filtered from 314 total entries)  
^r -\> 13 entries (filtered from 314 total entries)  
r\$ -\> 10 entries (filtered from 314 total entries)  
l -\> 85 entries (filtered from 314 total entries)

<https://clics.clld.org/languages/ids-204>  
Recherches : r -\> 594 entries (filtered from 1,695 total entries)  
rj -\> 19 entries (filtered from 1,695 total entries)  
ri -\> 58 entries (filtered from 1,695 total entries)  
^r -\> 73 entries (filtered from 1,695 total entries)  
r\$ -\> 40 entries (filtered from 1,695 total entries)  
l -\> 454 entries (filtered from 1,695 total entries)  
Form in source:  
r ́ - \> 5 entries (filtered from 1,695 total entries)

<https://clics.clld.org/languages/northeuralex-rus>  
Recherches : r -\> 300 entries (filtered from 971 total entries)  
rj -\> 91 entries (filtered from 971 total entries)  
ri -\> 18 entries (filtered from 971 total entries)  
^r -\> 45 entries (filtered from 971 total entries)  
r\$ -\> 12 entries (filtered from 971 total entries)  
l -\> 226 entries (filtered from 971 total entries)  
Form in source :  
rʲ -\> 91 entries (filtered from 971 total entries)

``` r
show_lang('Russian')$Form
```

    ## [1] "gladkij" "grubyj"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Russian",'trilled',revision))
```

## Romani

``` r
show_lang('Romani')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form  Trill R_type
    ##   <chr>    <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Romani   CLICS   smooth  kovlo yes   <NA>  
    ## 2 Romani   CLICS   rough   aspro yes   <NA>

``` r
show_comments('Romani')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments          
    ##   <chr>    <chr>   <chr>             
    ## 1 Romani   IDS     based on wikipedia

``` r
show_iso('Romani')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Romani   rmy      rmy

Trill : yes  
R_type : NA

Dataset :  
IDS, based on wikipedia

Phoible :  
<https://phoible.org/languages/vlax1238>  
2 inventaires :  
- UZ ɾ allophones ɾ r  
- EA r pas d’allophones; ɾ pas d’allophones

Adamou, Evangelia and Arvaniti, Amalia. 2014. Illustrations of the IPA:
Greek Thrace Xoraxane Romane. Journal of the International Phonetic
Association 44. 223–231. Cambridge University Press.  
- Page 225 : Tap ɾ  
r ˈɾat ‘blood’ aˈɾo ‘flour’ Xeɾ ‘donkey’  
- Page 226 : In addition to its pronunciation as a tap, /ɾ/ is often
pronounced as a short alveolar trill \[r\] showing two to three cycles.
This often happens before and after stops (e.g. \[oˈpre\] ‘above, up’,
\[drak\] ‘raisins’, \[marˈno\] ‘bread’), but it can also be the result
of emphasis particularly word- and utterance-initially, e.g. \[rat\]
‘blood’. Intervocalically, the tap can also be lenited to a short
approximant, while it is largely devoiced utterance-finally.

Leggio, Daniele Viktor. 2011. The dialect of the Mitrovica Roma. Romani
Studies 21. 57–113. Liverpool University Press.  
- Page 61 : Fricatives Voiced r  
Trill (rr)  
The long alveolar trill /rr/ continues the Early Romani /ř/ (whose
quality, however, is unclear). It occurs only word-initially, e.g. rrom
‘man’, and in some realisations of the historical cluster /ndř/ (see
section 1.4).  
- 1.4. Phonological and morphophonological processes  
The Early Romani */ndř/ cluster is continued in the Mitrovica dialect as
/nr, nrr, ngr/:  
(3)  
a. anro, anrro, angro \< Early Romani *andřo ‘egg’  
b. manro, manrro, mangro \< Early Romani *mandřo ‘bread’  
c. punro, punrro, pungro \< Early Romani *pindřo ‘leg’  
Such variation can be encountered even within individual speakers:  
(4)  
a. Informant 1:  
dijem cara manro tumare štare grasten.  
gave.1SG little bread your.OBL four.OBL horses.OBL  
‘I gave some bread to your four horses.’  
b. Informant 1:  
voj čindžarda o manrro.  
she broke.3SG the bread  
‘She broke the bread.’  
c. Informant 1:  
ni džav kur ano pijaco te činav mangro.  
not go.1SG never to.the market to buy.1SG bread  
‘I never go to the market to buy bread.’

Wikipedia :  
<https://en.wikipedia.org/wiki/Vlax_Romani_language>  
<https://en.wikipedia.org/wiki/Romani_language>  
- \[…\] the presence in some dialects of a second rhotic ⟨ř⟩, realized
as retroflex \[ɽ\] or \[ɻ\], a long trill \[rː\], or uvular
\[ʀ\].\[54\]  
- Rhotic r (ř)  
\[54\] Matras, Yaron (2006). “Romani” (PDF). In Brown, Keith (ed.).
Encyclopedia of Languages and Linguistics (Second ed.). Oxford:
Elsevier.

Glottologue :  
<https://glottolog.org/resource/languoid/id/vlax1238>

Autres sources :

<https://clics.clld.org/languages/ids-212>  
Recherches : r -\> 759 entries (filtered from 1,715 total entries)  
^r -\> 72 entries (filtered from 1,715 total entries)  
r\$ -\> 183 entries (filtered from 1,715 total entries)  
l -\> 386 entries (filtered from 1,715 total entries)

``` r
show_lang('Romani')$Form
```

    ## [1] "kovlo" "aspro"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Romani",'trilled',revision))
```

## Bengali

``` r
show_lang('Bengali')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form      Trill R_type  
    ##   <chr>    <chr>   <chr>   <chr>     <chr> <chr>   
    ## 1 Bengali  CLICS   smooth  molāyema  no    no trill
    ## 2 Bengali  CLICS   rough   asamatala no    no trill
    ## 3 Bengali  CLICS   smooth  mɔʃrin    no    no trill

``` r
show_comments('Bengali')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments                                              
    ##   <chr>    <chr>   <chr>                                                 
    ## 1 Bengali  IDS     cannot be determined; Phoible lists different phonemes
    ## 2 Bengali  Google  <NA>

``` r
show_iso('Bengali')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Bengali  ben      ben

Trill : no  
R_type : no trill

Dataset :  
IDS, cannot be determined; Phoible lists different phonemes Google, NA

Phoible :  
<https://phoible.org/languages/beng1280> - SPA ɾ allophones ɹ r ɾ; ɽ̤
allophones ɽ̤; ɽ allophones ɽ ɽ ɻ; ɾ̤ allophones ɾ̤ - UPSID ɽ pas
d’allophone; ɾ pas d’allophone - RA r pas d’allophone; ɽ pas
d’allophone; ɽ̤ pas d’allophone; ɾ pas d’allophone - UZ ɹ allophones ɹ ɾ
ɹ̥ ɾ̥ - EA r pas d’allophone

Ferguson, C. A. and Chowdhury, M. 1960. The Phonemes of Bengali.
Language 36. 22–59. - Page 32 : /r/ is a postdental trill or flap.
Medially and finally it is flapped. The final flap is so short that the
American English speaker often fails to hear it at all; e.g. car ‘four’
closely resembles ca ‘tea’ in auditory effect. Initially it is either
flapped or trilled with two or three oscillations. Also initially, or
after a stop, especially a labial or retroflex, it is sometimes a
continuant very much like English r (but this was not true of
Mr. Mondell’s speech). Examples: roy ‘color’, bar ‘time, day’, mar ‘hit,
beat’, gram ‘village’, pran ‘life’.

Bhattacharya, Krishna. 1988. Bengali Phonetic reader. Mysore: Central
Institute of Indian Languages. \[pas cherché\]

Khan, Sameer ud Dowla. 2010. Illustrations of the IPA: Bengali
(Bangladeshi Standard). Journal of the International Phonetic
Association 40. 221–225. Cambridge University Press. - Page 221 :
Approximant ɹ ɹ ɹaʃ ’reduction” - Page 223 : Excluding /d^ɦ ɡ^ɦ ŋ f s h
ɹ/, all consonants can occur geminated - Page 224 : Rhotic Bangladeshi
Standard Bengali has only one rhotic /ɹ/, as is the case in most Eastern
dialects (Dasgupta Reference Dasgupta, Cardona and Jain2003: 359; Masica
Reference Masica1991: 97); however, some speakers may maintain a
marginal /ɹ/ vs. /ɽ/ distinction in formal registers due to influence
from Kolkata Standard. The rhotic /ɹ/ can be realized as a tap \[ɾ\],
especially following dentals (Dasgupta Reference Dasgupta, Cardona and
Jain2003: 359), and both realizations can be devoiced \[ɹ̥ ɾ̥\] in codas.
Consonants are often lengthened following /ɹ/.

Barman, Binoy. 2011. A contrastive analysis of English and Bangla
phonemics. Dhaka University Journal of Linguistics 2. 19–42. - Page 37 :
Approximant r - Page 40 : Besides trill /r/, Bangla has also a flap /ɹ/
and a retroflex /ɽ/. (Chatterji 1988: 54; Datta 1999: 74) The last two
are, however, often considered as only allophones.

Wikipedia : <https://en.wikipedia.org/wiki/Bengali_language> - Rhotic
voiced unaspirated r ɽ  
voiced aspirated (ɽʱ)

- Rhotic voiced unaspirated r\[s\] ɽ\[t\] voiced aspirated (ɽʱ)\[u\]
- \[s\] The /r/ phoneme is pronounced either as a voiced alveolar flap
  \[ɾ\], voiced alveolar approximant \[ɹ\] or voiced alveolar trill
  \[r\]. Most speakers colloquially pronounce /r/ as a flap \[ɾ\],
  although the trill \[r\] may occur word-initially; with the flap \[ɾ\]
  occurring medially and finally. /r/ can also occur as an approximant
  \[ɹ\], especially in some Eastern dialects and sometimes in conjuncts
  before consonants.\[8\]\[9\] \[t\] /ɽ/: In the form of Standard
  Bengali spoken in Dhaka and other Eastern dialects, /r/ and /ɽ/ are
  often indistinct phonemically and both may be phonetically realised as
  either \[ɾ\] or \[ɹ\]. Thus the pairs পড়ে /pɔɽe/ ‘reads’/‘falls’
  vs. পরে /pɔre/ ‘wears’/‘after’, and করা /kɔra/ ‘do’ vs. কড়া \[kɔɽa\]
  ‘strict’ can be homophonous. \[u\] /ɽʱ/ only occurs in the individual
  pronunciation of the letter ঢ় \[ɽʱɔ\] but is usually pronounced as
  \[ɽ\] in ordinary speech.

Glottologue :  
<https://glottolog.org/resource/languoid/id/beng1280>

Autres sources :

<https://clics.clld.org/languages/diacl-48400>  
Recherches : r -\> 32 entries (filtered from 149 total entries) ^r -\> 1
entries (filtered from 149 total entries)  
r\$ -\> 7 entries (filtered from 149 total entries) l -\> 24 entries
(filtered from 149 total entries) Form in source : r - 19 entries
(filtered from 149 total entries) ṛ -\> 13 entries (filtered from 149
total entries)

<https://clics.clld.org/languages/ids-701> Recherches : r -\> 554
entries (filtered from 1,380 total entries) ^r -\> 33 entries (filtered
from 1,380 total entries) r\$ -\> 2 entries (filtered from 1,380 total
entries) l -\> 233 entries (filtered from 1,380 total entries) Form in
source : r̖ -\> 97 entries (filtered from 1,380 total entries) r -\> 553
entries (filtered from 1,380 total entries)

<https://clics.clld.org/languages/northeuralex-ben> Recherches : r -\>
413 entries (filtered from 965 total entries) ^r -\> 24 entries
(filtered from 965 total entries) r\$ -\> 55 entries (filtered from 965
total entries) l -\> 148 entries (filtered from 965 total entries) Form
in source : r -\> 374 entries (filtered from 965 total entries) ɽ -\> 42
entries (filtered from 965 total entries)

``` r
show_lang('Bengali')$Form
```

    ## [1] "molāyema"  "asamatala" "mɔʃrin"

A cause de la multitude de r on met OUT.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Bengali",'OUT',revision))
```

## Marathi

``` r
show_lang('Marathi')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form       Trill R_type  
    ##   <chr>    <chr>   <chr>   <chr>      <chr> <chr>   
    ## 1 Marathi  CLICS   smooth  sapaːʈ     no    no trill
    ## 2 Marathi  CLICS   smooth  guɭaguɭiːt̪ no    no trill
    ## 3 Marathi  CLICS   rough   kʰarabarit̪ no    no trill

``` r
show_comments('Marathi')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Marathi  IDS     tap/flap
    ## 2 Marathi  Google  <NA>

``` r
show_iso('Marathi')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Marathi  mar      mar

Trill : no  
R_type : no trill

Dataset :  
IDS; tap/flap  
Google, NA

Phoible :  
<https://phoible.org/inventories/view/1766>  
- ɾ pas d’allophones; ɾ̤ pas d’allophones

Kelkar, Ashok Ramchandra. 1958. The Phonology and Morphology of Marathi.
(Doctoral dissertation, Cornell University). \[Proquest - pas trouvé\]

Berntsen, Maxine and Nimbkar, Jai. 1975. A Marathi Reference Grammar.
\[pas trouvé\]

Wikipedia :  
<https://en.wikipedia.org/wiki/Marathi_language>  
<https://en.wikipedia.org/wiki/Marathi_phonology>  
- Flap/Trill plain ɾ\~r (ɺ̣ ou ɭ̆ (pas de unicode pour le moment) )\[5\]  
murmured ɾʱ\~rʱ  
- A defining feature of the Marathi language is the split of the
consonant ल /la/ in Sanskrit words into a retroflex lateral flap ळ (ḷa)
and alveolar ल (la). For instance, कुळ (kuḷa) for the Sanskrit कुलम् (kulam
‘clan’) and कमळ (kamaḷ) for Sanskrit कमलम् (kamalam ‘lotus’). Marathi got
ळ possibly due to long contact from Dravidian languages; there are some
ḷ words loaned from Kannada like ṭhaḷak from kn. taḷaku but most of the
words are native. Vedic Sanskrit did have /ɭ, ɭʱ/ as well, but they
merged with /ɖ, ɖʱ/ by the time of classical Sanskrit.  
- \[5\] Masica, Colin (1991), The Indo-Aryan Languages, Cambridge:
Cambridge University Press, ISBN 978-0-521-29944-2

Glottologue :  
<https://glottolog.org/resource/languoid/id/mara1378>

Autres sources :

Rajeshwari V. Pandharipande. 1997. Marathi. (Descriptive Grammars
Series.) London: London & New York: Routledge. xlviii+630pp.  
- Page 539 : Flap alveo-palatal r  
- Page 542 : \[r\] is an alveo-palatal flap or a short trill. ras
‘juice’, sarad ‘autumn’, ghar ‘hosuse’  
- Page 550 : liquids:  
rr bharrkan ‘quickly’

Dhongde, Ramesh Vaman and Kashi Wali. 2009. Marathi. (London Oriental
and African Languages Library, 13.) Amsterdam: John Benjamins.
xviii+340pp.  
- Page 11 : Flap V UNASP alveopalatal r  
ASP alveopalatal rh  
- Page 15 : \[r\] is a voiced alveo-palatal unaspirated flap or short
trill  
\[rh\] is a voiced-palatal aspirated trill

<https://clics.clld.org/languages/diacl-49900>  
Recherches : r -\> 24 entries (filtered from 145 total entries)  
rr -\> 0 entries (filtered from 145 total entries)  
^r -\> 7 entries (filtered from 145 total entries)  
r\$ -\> 5 entries (filtered from 145 total entries)  
l -\> 25 entries (filtered from 145 total entries)

<https://clics.clld.org/languages/ids-705>  
Recherches : r -\> 558 entries (filtered from 1,592 total entries)  
rr -\> 0 entries (filtered from 1,592 total entries)  
^r -\> 50 entries (filtered from 1,592 total entries)  
r\$ -\> 83 entries (filtered from 1,592 total entries)  
l -\> 349 entries (filtered from 1,592 total entries)

``` r
show_lang('Marathi')$Form
```

    ## [1] "sapaːʈ"     "guɭaguɭiːt̪" "kʰarabarit̪"

Présence du trill ? On chnage en trill.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Marathi",'trilled',revision))
```

## Punjabi

``` r
show_lang('Punjabi')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form     Trill R_type          
    ##   <chr>    <chr>   <chr>   <chr>    <chr> <chr>           
    ## 1 Punjabi  CLICS   smooth  mulāyama no    mixed with trill
    ## 2 Punjabi  CLICS   rough   rukhā    no    mixed with trill
    ## 3 Punjabi  CLICS   rough   khurdarā no    mixed with trill

``` r
show_comments('Punjabi')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments               
    ##   <chr>    <chr>   <chr>                  
    ## 1 Punjabi  IDS     flap based on wikipedia
    ## 2 Punjabi  Google  <NA>

``` r
show_iso('Punjabi')
```

    ## # A tibble: 1 × 3
    ##   Language ISO_code Phoible_code
    ##   <chr>    <chr>    <chr>       
    ## 1 Punjabi  pnb      pan

Trill : no  
R_type : mixed with trill

Dataset :  
IDS, flap based on wikipedia  
Google, NA

Phoible :  
<https://phoible.org/languages/panj1256>  
3 inventaires :  
- SPA r pas d’allophone; ɽ pas d’allophone  
- RA r pas d’allophone; ɽ pas d’allophone  
- EA r allophone r ; ɽ allophone ɽ

Dulai, N.K. and Koul, O.N. 1980. Punjabi Phonetic Reader. Mysore:
Central Institute of Indian Languages. \[pas cherché\]

Shackle, Christopher. 2003. Panjabi. In Cardona, George and Jain,
Dhanesh (eds.), The Indo-Aryan Languages, 581-621. London &New York:
Routledge. \[pas cherché\]

Bhatia, Tej K. 1993. Punjabi: A cognitive-descriptive grammar.
Routledge. \[pas cherché\]

Gill, H. S. and Gleason, H. A. 1969. A Reference Grammar of Punjabi.
(Hartford Studies in Linguistics.) Hartford: Hartford Seminary
Foundation. \[pas cherché\]

Wikipedia :  
<https://en.wikipedia.org/wiki/Punjabi_language>  
- Rhotic ɾ\~r ਰ ر ɽ ੜ ڑ  
- All consonants except six (ṇ, ṛ, h, r, v, y) are regularly geminated.

Glottologue :  
<https://glottolog.org/resource/languoid/id/panj1256>  
<https://glottolog.org/resource/languoid/id/west2386>

Autres sources :

Hussain, Q., Proctor, M., Harvey, M., & Demuth, K. (2020). Punjabi
(Lyallpuri variety). Journal of the International Phonetic Association,
50(2), 282-297. <doi:10.1017/S0025100319000021>  
- Page 284 : Tap or flap ɾ ɽ  
- Page 285 : ɾ ɾat night saɾa a female name tSaɾ four  
ɽ saɽa jealousy saɽ burnt  
- Page 286 : Rhotics are prototypically realized as taps in the speech
of our consultant. /ɾ/ and /l/ can occur in all word positions, but the
retroflex tap /ɽ/ and lateral /ɭ/ contrast with alveolar /ɾ/ and /l/
only word-medially and word-finally. Figure 5 illustrates the contrast
between alveolar /ɾ/ and retroflex /ɽ/ taps. The retroflex tap /ɽ/ is
characterized by earlier lowering of third and fourth formants into a
shorter, less attenuated interval of occlusion.

<https://clics.clld.org/languages/diacl-44600>  
Recherches : r -\> 40 entries (filtered from 163 total entries)  
rr -\> 0 entries (filtered from 163 total entries)  
^r -\> 7 entries (filtered from 163 total entries)  
r\$ -\> 15 entries (filtered from 163 total entries)  
l -\> 29 entries (filtered from 163 total entries)  
Form in source :  
ṛ -\> 6 entries (filtered from 163 total entries)

<https://clics.clld.org/languages/ids-707>  
Recherches : r -\> 561 entries (filtered from 1,602 total entries)  
rr -\> 0 entries (filtered from 1,602 total entries)  
^r -\> 49 entries (filtered from 1,602 total entries)  
r\$ -\> 0 entries (filtered from 1,602 total entries)  
l -\> 280 entries (filtered from 1,602 total entries)  
Form in source :  
ṛ -\> 116 entries (filtered from 1,602 total entries)

``` r
show_lang('Punjabi')$Form
```

    ## [1] "mulāyama" "rukhā"    "khurdarā"

Soit OUT soit other… On choisit OUT.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Punjabi",'OUT',revision))
```

## Romagnol-std

``` r
show_lang('Romagnol-std')
```

    ## # A tibble: 1 × 6
    ##   Language     Dataset Meaning Form  Trill R_type
    ##   <chr>        <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Romagnol-std CLICS   smooth  léss  yes   trill

``` r
show_comments('Romagnol-std')
```

    ## # A tibble: 1 × 3
    ##   Language     Dataset Comments
    ##   <chr>        <chr>   <chr>   
    ## 1 Romagnol-std CLICS   wiki

``` r
show_iso('Romagnol-std')
```

    ## # A tibble: 1 × 3
    ##   Language     ISO_code Phoible_code
    ##   <chr>        <chr>    <chr>       
    ## 1 Romagnol-std rgn      <NA>

``` r
# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Romagnol-std', Dataset='Google_trad',Meaning='rough',Form='rud', rough=TRUE, r=TRUE,l=FALSE)

#Vocabolario romagnolo - italiano, Imola, 1879 - Antonio Mattioli : rùd page 549
#Vocabolario romagnolo - italiano, Ravenna, 1960 - Libero Ercolani : rud page 350
```

Trill : yes  
R_type : trill

Dataset :  
CLICS, wiki

Phoible : NA

Wikipedia :  
<https://en.wikipedia.org/wiki/Romagnol>  
- Trill r

Glottologue :  
<https://glottolog.org/resource/languoid/id/roma1328>

Autres sources :

<https://clics.clld.org/languages/logos-Romagnolstd>  
Recherches : r -\> 269 entries (filtered from 627 total entries)  
rr -\> 0 entries (filtered from 627 total entries)  
^r -\> 21 entries (filtered from 627 total entries)  
r\$ -\> 90 entries (filtered from 627 total entries)  
l -\> 146 entries (filtered from 627 total entries)

Montanari, Simona. 2018. Sammarinese, the Endangered Language of the
Republic of San Marino: A Preliminary Study of Documentation and
Description. Dialectologia et Geolinguistica 26. 47-96.  
- Page 73 : Trill r  
- Page 74 : /r/\~/l/ \[rɛːnɐ\] ‘frog’ \~ \[lɛːnɐ\] ‘wool’

``` r
show_lang('Romagnol-std')$Form
```

    ## [1] "léss"

Donc on laisse en trill.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Romagnol-std",'trilled',revision))
```

## Lower Sorbian

``` r
show_lang('Lower Sorbian')
```

    ## # A tibble: 3 × 6
    ##   Language      Dataset Meaning Form     Trill R_type
    ##   <chr>         <chr>   <chr>   <chr>    <chr> <chr> 
    ## 1 Lower Sorbian CLICS   smooth  rowny    yes   trill 
    ## 2 Lower Sorbian CLICS   rough   gropny   yes   trill 
    ## 3 Lower Sorbian CLICS   rough   škropaty yes   trill

``` r
show_comments('Lower Sorbian')
```

    ## # A tibble: 2 × 3
    ##   Language      Dataset Comments
    ##   <chr>         <chr>   <chr>   
    ## 1 Lower Sorbian WOLD    <NA>    
    ## 2 Lower Sorbian CLICS   wiki

``` r
show_iso('Lower Sorbian')
```

    ## # A tibble: 1 × 3
    ##   Language      ISO_code Phoible_code
    ##   <chr>         <chr>    <chr>       
    ## 1 Lower Sorbian dsb      <NA>

Trill : yes  
R_type : trill

Dataset :  
WOLD, NA  
CLICS, wiki

Phoible :  
<https://phoible.org/inventories/view/2602>  
- r pas d’allophone; rʲ pas d’allophone

Hannusch, Erwin. 1998. Niedersorbisch praktisch und verständlich; zum
Lernen und Auffrischen. Domowina-Verl. \[pas cherché\]

Wikipedia :  
<https://en.wikipedia.org/wiki/Lower_Sorbian_language>  
- Trill r rʲ  
- /n, nʲ, l, r, rʲ/ are alveolar \[n, nʲ, l, r, rʲ\]

Stone, Gerald (2002), “Sorbian (Upper and Lower)”, in Comrie, Bernard;
Corbett, Greville G. (eds.), The Slavonic Languages, London and New
York: Routledge, pp. 593–685, ISBN 9780415280785  
- Page 605 : Trill r,

Zygis, Marzena (2003), “Phonetic and Phonological Aspects of Slavic
Sibilant Fricatives” (PDF), ZAS Papers in Linguistics, 3: 175–213,
archived from the original (PDF) on 2017-10-11, retrieved 2015-04-21

Glottologue :  
<https://glottolog.org/resource/languoid/id/lowe1385>

Autres sources :

<https://clics.clld.org/languages/wold-LowerSorbian>  
Recherches : r -\> 525 entries (filtered from 1,764 total entries)  
rj -\> 50 entries (filtered from 1,764 total entries)  
ri -\> 30 entries (filtered from 1,764 total entries)  
^r -\> 60 entries (filtered from 1,764 total entries)  
r\$ -\> 66 entries (filtered from 1,764 total entries)  
l -\> 500 entries (filtered from 1,764 total entries)

``` r
show_lang('Lower Sorbian')$Form
```

    ## [1] "rowny"    "gropny"   "škropaty"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Lower Sorbian",'trilled',revision))
```

# Si on ajoute plus de données par langue (pas pour le moment)

``` r
#On rajoute les informations manquantes mais déjà présentes dans le tablea
# rough_r_data <- rough_r_data %>% 
#   dplyr::group_by(Language) %>% 
#   tidyr::fill(ISO_code,Phoible_code,Trill,Latitude,Longitude,Family,Branch,Area,Continent,R_type,revision,.direction = "downup")
```

``` r
# rough_r_data %>%
#   #dplyr::filter(Language %in% nb_langue$Language) %>% 
#   dplyr::filter(Meaning %in% c("rough", "smooth")) %>% 
#   dplyr::select(Language,Dataset,Meaning,Form) %>% 
#   dplyr::group_by(Language) %>% 
#   dplyr::mutate(nb_row = dplyr::n()) %>% 
#   dplyr::ungroup() %>% dplyr::filter(nb_row<2)
```

``` r
# dplyr::add_row(Language='', Dataset='Google_trad',Meaning='smooth',Form='', rough=FALSE, r=FALSE,l=TRUE)
# dplyr::add_row(Language='', Dataset='Google_trad',Meaning='rough',Form='', rough=TRUE, r=TRUE,l=FALSE)

# rough_r_data <- rough_r_data %>% 
#       dplyr::add_row(Language='Haitian Creole', Dataset='Google_trad',Meaning='smooth',Form='lis', rough=FALSE, r=FALSE,l=TRUE) %>% 
#       dplyr::add_row(Language='Indonesian', Dataset='Google_trad',Meaning='rough',Form='kasar', rough=TRUE, r=TRUE,l=FALSE) %>% 
#       dplyr::add_row(Language='Hebrew', Dataset='Google_trad',Meaning='smooth',Form='חָלָק', rough=FALSE, r=FALSE,l=TRUE) %>% 
#       dplyr::add_row(Language='Kazakh', Dataset='Google_trad',Meaning='rough',Form='өрескел', rough=TRUE, r=TRUE,l=TRUE) %>% 
#       dplyr::add_row(Language='Sundanese', Dataset='Google_trad',Meaning='smooth',Form='mulus', rough=FALSE, r=FALSE,l=TRUE) %>% 
#       dplyr::add_row(Language='Turkish', Dataset='Google_trad',Meaning='rough',Form='pürüzlü', rough=TRUE, r=TRUE,l=TRUE) 
#       dplyr::add_row(Language='Laz', Dataset='Dictionnary',Meaning='rough',Form='სერთი', rough=TRUE, r=TRUE,l=FALSE) %>% #serti : emprunt du turc ?      
#       dplyr::add_row(Language='Megrelian', Dataset='Dictionnary',Meaning='rough',Form='ღინჩალი', rough=TRUE, r=FALSE,l=TRUE) %>% #ghinchali
#       dplyr::add_row(Language='Megrelian', Dataset='Dictionnary',Meaning='rough',Form='კონწარი', rough=TRUE, r=TRUE,l=FALSE)  #k'onts'ari


#chirila <- readr::read_csv("~/Téléchargements/chirila.csv")
#Laz et Megrelian : mkvaxe (uxeSi) | konwari | Turq. serTi | kUax | rough | page 155
#Laz et Megrelian : xeSi | RinCali | Turq. serTi | ujmajur | rough | Page 202
```

# Résultat du recodage

On reprend le même code que dans `Script_Chapitre_5.Rmd`.

``` r
rough_r_data$revision[rough_r_data$revision == "NA"] <- "NA_"
```

``` r
# save(rough_r_data, file = "data_replication/rough_r_data_IE.RData")
# write.csv(rough_r_data, "data_replication/rough_r_data_IE.csv", row.names=FALSE)
```

``` r
rough_r_data %>% 
  dplyr::filter(!is.na(revision) & Meaning %in% c("smooth",'rough')) %>% 
  dplyr::select(Language,Form,Meaning,r,revision) %>% 
  dplyr::mutate(nbchar = stringr::str_count(Form,"[:alpha:]")) %>% 
  dplyr::filter(revision %in% c("trilled","other"))
```

    ## # A tibble: 590 × 6
    ##    Language     Form   Meaning r     revision nbchar
    ##    <chr>        <chr>  <chr>   <lgl> <chr>     <int>
    ##  1 Afrikaans    rowwe  rough   TRUE  trilled       5
    ##  2 Azeerbaijani kobud  rough   FALSE trilled       5
    ##  3 Azeerbaijani hamar  smooth  TRUE  trilled       5
    ##  4 Belarusian   грубы  rough   TRUE  trilled       5
    ##  5 Belarusian   гладкі smooth  FALSE trilled       6
    ##  6 Bulgarian    груб   rough   TRUE  trilled       4
    ##  7 Bulgarian    гладък smooth  FALSE trilled       6
    ##  8 Bosnian      glatko smooth  FALSE other         6
    ##  9 Czech        hladký smooth  FALSE trilled       6
    ## 10 Welsh        garw   rough   TRUE  trilled       4
    ## # ℹ 580 more rows

``` r
rough_r_data %>% 
  dplyr::select(Language,revision,Trill) %>% 
  dplyr::filter(!is.na(revision)) %>% 
  dplyr::group_by(Trill) %>% 
  dplyr::distinct() %>% dplyr::select(revision) %>% 
  table() %>% as.data.frame() -> table_changes
```

    ## Adding missing grouping variables: `Trill`

``` r
table_changes_ss_IE <- rough_r_data %>% 
  dplyr::filter(Family != "Indo-European")  %>% 
  dplyr::select(Language,revision,Trill) %>% 
  dplyr::filter(!is.na(revision)) %>% 
  dplyr::group_by(Trill) %>% 
  dplyr::distinct() %>% dplyr::select(revision) %>% 
  table() %>% as.data.frame() 
```

    ## Adding missing grouping variables: `Trill`

``` r
table_changes_IE <- rough_r_data %>% 
  dplyr::filter(Family == "Indo-European")  %>% 
  dplyr::select(Language,revision,Trill) %>% 
  dplyr::filter(!is.na(revision)) %>% 
  dplyr::group_by(Trill) %>% 
  dplyr::distinct() %>% dplyr::select(revision) %>% 
  table() %>% as.data.frame()
```

    ## Adding missing grouping variables: `Trill`

L’analyse originelle comportait 332 langues. De ces langues nous n’en
avons gardé que 256 pour 71 familles de langues après notre recodage, et
à ces langues nous avons aussi rajoutés des langues indo-européennes
pour arriver à un total de 375 langues.

``` r
table_changes_2_ss_IE <- table_changes_ss_IE %>% 
         dplyr::mutate(Trill = ifelse(Trill=="yes","Trill","Other"),
                       revision = dplyr::case_when(revision == "ERROR" ~ "Error",
                                                   revision == "OUT" ~ "Contrast",
                                                   revision == "other" ~ "OTHER",
                                                   revision == "trilled" ~ "TRILL",
                                                   revision == "NA_" ~ "NA"),
                       revision = ifelse(is.na(revision),"NA",revision),
                       Data = ifelse(revision %in% c("TRILL","OTHER"),"Included","Excluded"),
                       family = "Non-IE")

table_changes_2_ss_IE$revision <- factor(table_changes_2_ss_IE$revision,levels = c("OTHER","Contrast","Error","NA","TRILL")) 

table_changes_2_IE <- table_changes_IE %>% 
         dplyr::mutate(Trill = ifelse(Trill=="yes","Trill","Other"),
                       revision = dplyr::case_when(revision == "ERROR" ~ "Error",
                                                   revision == "OUT" ~ "Contrast",
                                                   revision == "other" ~ "OTHER",
                                                   revision == "trilled" ~ "TRILL",
                                                   revision == "NA_" ~ "NA"),
                       revision = ifelse(is.na(revision),"NA",revision),
                       Data = ifelse(revision %in% c("TRILL","OTHER"),"Included","Excluded"),
                       family = "IE")

table_changes_2_IE$revision <- factor(table_changes_2_IE$revision,levels = c("OTHER","Contrast","Error","NA","TRILL"))

table_changes_2 <- dplyr::bind_rows(table_changes_2_ss_IE,table_changes_2_IE)
```

``` r
#Adapté de quelque part sur Internet
ggplot2::ggplot(data = table_changes_2,
       ggplot2::aes(axis0 = family, axis1 = Trill, axis2 = revision, axis3 = Data, y = Freq)) +
  ggalluvial::geom_alluvium(ggplot2::aes(fill = revision)) +
  ggalluvial::geom_stratum() +
  ggplot2::geom_text(stat = ggalluvial::StatStratum,
            ggplot2::aes(label = ggplot2::after_stat(stratum))) +
  ggplot2::scale_x_discrete(limits = c("Survey", "Response"),
                   expand = c(0.15, 0.05)) +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position="none")
```

![Diagramme illustrant le re-codage des langues en fonction des
informations collectées. A gauche le compte des langues telles que
codées par Winter et al., au milieu les conclusions de notre processus
de révision des valeurs de trill/other dans les langues, à droite le
compte des langues inclues ou exclues dans notre
réplication.](IE_recoding_files/figure-gfm/unnamed-chunk-149-1.png)

``` r
#Adapté de quelque part sur Internet

world_map <- ggplot2::map_data("world") %>% 
              dplyr::filter(region != "Antarctica")

basic4map <- ggplot2::ggplot() + 
  ggplot2::coord_fixed() +
  ggplot2::xlab("") +
  ggplot2::ylab("")

#Add map to base plot
base_world <- basic4map +
  ggplot2::geom_polygon(data=world_map,
                        ggplot2::aes(x=long,
                                     y=lat,
                                     group=group), 
                        colour="gray",
                        fill="gray") +
  ggplot2::theme(panel.background = ggplot2::element_rect(
    size = 0.5,
    linetype = "solid"),
    panel.grid.major = ggplot2::element_line(
      size = 0.5,
      linetype = 'solid',
      colour = "gray90"), 
    panel.grid.minor = ggplot2::element_line(
      size = 0.25,
      linetype = 'solid',
      colour = "gray90")) 
```

    ## Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
rm(basic4map)
```

``` r
data_R_revision_2 <- rough_r_data %>% 
                        dplyr::select(Language,Latitude,Longitude,revision,Trill) %>% 
                        dplyr::mutate(revision = ifelse(revision=="trilled","TRILL",
                                                        ifelse(revision=="other","OTHER","OLD")),
                                      Data = ifelse(revision%in%c("TRILL","OTHER"),"NEW","OLD")) %>% 
                        dplyr::distinct()

data_R_revision_2$revision <- factor(data_R_revision_2$revision,levels = c("OTHER","TRILL","OLD"))
  
r_col <- "#440154FF"
other_col <- "#FDE725FF"
```

``` r
base_world + 
  ggplot2::geom_point(data =  data_R_revision_2 %>% dplyr::filter(Data=="OLD"),
                      ggplot2::aes(x=Longitude, y=Latitude, shape=Data),
             size=2, alpha=1)+
  ggplot2::geom_point(data =  data_R_revision_2 %>% dplyr::filter(revision!="OLD"),
                      ggplot2::aes(x=Longitude, y=Latitude, fill=revision,shape=Data),
             size=2, alpha=1) +
  ggplot2::scale_fill_viridis_d(name = "Langues avec") +
  ggplot2::scale_shape_manual(values=c(21, 18),
                              labels=c("Inclusion", "Exclusion")) +
  ggplot2::ggtitle(NULL) + 
  ggplot2::theme(legend.position="bottom") +
  ggplot2::guides(fill=ggplot2::guide_legend(override.aes=list(shape = 21)))
```

![Distribution des langues incluses dans l’analyse originale de Winter
et al. (2022). Deux groupes sont inclus : les langues TRILL et les
langues OTHER. Les langues Indo-Européennes ne sont pas
incluses.](IE_recoding_files/figure-gfm/unnamed-chunk-152-1.png)

Le code est très largement adapté de celui de Winter et al. (2022).

``` r
source("donnees_winter2022/scripts/rough_helper.r")

xling <- readr::read_csv("donnees_winter2022/final_data/cross_linguistic.csv") %>%
  dplyr::filter(Meaning %in% c("rough","smooth"),
         Family!="Indo-European")
```

    ## Rows: 2729 Columns: 18
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (12): Language, ISO_code, Phoible_code, Meaning, Form, Trill, Dataset, F...
    ## dbl  (3): Latitude, Longitude, Rough.M
    ## lgl  (3): rough, r, l
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
xling$Meaning <- paste0("‘", xling$Meaning, "’")
xling$Meaning.f <- factor(xling$Meaning)#, levels=c("‘smooth’", "‘rough’"))
# only Google TRS data for Basque:
xling <- dplyr::filter(xling, !(Language=="Basque" & Dataset=="CLICS"))


# some languages happen to have more than one rough / smooth words (very small minority!)
# reduce to single data point via majority rule:
xling_single <- xling %>%
  dplyr::filter(Trill=="yes") %>%
  dplyr::group_by(Language,Meaning.f) %>%
  dplyr::summarise(r = as.logical(round(mean(r))),
            Longitude=Longitude[1],
            Latitude=Latitude[1]) %>%
  dplyr::ungroup()
```

    ## `summarise()` has grouped output by 'Language'. You can override using the
    ## `.groups` argument.

``` r
data_R_revision <- rough_r_data 

xling_revision <- data_R_revision %>%
  dplyr::filter(Meaning %in% c("rough","smooth")) %>% 
  dplyr::mutate(Trill = dplyr::case_when(revision == "trilled" ~ "yes",
                                         revision == "other" ~ "no",
                                         revision %in% c("OUT","NA") ~ "OUT")) %>% 
  dplyr::filter(Trill != "OUT")

IE_languages <- dplyr::filter(data_R_revision, Family=="Indo-European")

xling_revision$Meaning <- paste0("‘", xling_revision$Meaning, "’")
xling_revision$Meaning.f <- factor(xling_revision$Meaning)#, levels=c("‘smooth’", "‘rough’"))

# some languages happen to have more than one rough / smooth words (very small minority!)
# reduce to single data point via majority rule:
xling_single_revision <- xling_revision %>%
  dplyr::filter(Trill=="yes") %>%
  dplyr::group_by(Language,Meaning.f) %>%
  dplyr::summarise(r = as.logical(round(mean(r))),
            Longitude=Longitude[1],
            Latitude=Latitude[1]) %>%
  dplyr::ungroup()
```

    ## `summarise()` has grouped output by 'Language'. You can override using the
    ## `.groups` argument.

``` r
r_col <- "#440154FF"
other_col <- "#FDE725FF"

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

theme_rough <- 
ggplot2::theme_classic() + 
  ggplot2::theme(legend.position = "none",
        legend.key.height = ggplot2::unit(2,"line"),
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 12),
        legend.background = ggplot2::element_rect(fill = "transparent"),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 12, face = "bold"),
        panel.spacing = ggplot2::unit(2, "lines"),
        panel.border = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        panel.background = ggplot2::element_rect(fill = "transparent"),
        strip.text.y =ggplot2:: element_text(size = 12, hjust = 0),
        axis.text.x = ggplot2::element_text(size = 12, colour="black", face="bold"),
        axis.text.y = ggplot2::element_text(size = 12, colour="black"),
        axis.line = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.margin = ggplot2::unit(c(0.4,0.4,0.4,0.4),"cm"))
```

``` r
ggplot2::ggplot(world) +
  ggplot2::geom_sf(size=0.2, col="white", fill="lightgrey") +
  ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-60, 80), datum=NA) +
  ggplot2::geom_point(data = dplyr::filter(xling_single, !(Language %in% IE_languages$Language)), ggplot2::aes(x = Longitude, y = Latitude, fill = r, group = NA),
             alpha = 0.8, size = 2.5,
             col="black", pch=21, stroke=0.1,
             position = ggplot2::position_jitter(width=2.5,height=2.5,seed=1)) +
  ggplot2::facet_grid(~Meaning.f) +
  ggplot2::scale_fill_manual(breaks = c(F,T), values = c(other_col, r_col), labels=c("no /r/", "contains /r/")) +
  ggplot2::labs(title = "Trilled /r/ in the words ‘rough’ and ‘smooth’ cross-linguistically \n(Original study)") +
  theme_rough +
  ggplot2::theme(
    legend.position = "bottom",
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    plot.margin = ggplot2::unit(c(0.1,0.4,-0.4,0.4),"cm"),
    panel.spacing = ggplot2::unit(0, "lines"))
```

![Distribution des langues incluses dans l’analyse originale de Winter
et al. (2022). Deux groupes sont inclus : les langues TRILL et les
langues OTHER. Les langues Indo-Européennes n’e sont pas
incluses.](IE_recoding_files/figure-gfm/unnamed-chunk-154-1.png)

``` r
ggplot2::ggplot(world) +
  ggplot2::geom_sf(size=0.2, col="white", fill="lightgrey") +
  ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-60, 80), datum=NA) +
  ggplot2::geom_point(data = xling_single_revision, ggplot2::aes(x = Longitude, y = Latitude, fill = r, group = NA),
             alpha = 0.8, size = 2.5,
             col="black", pch=21, stroke=0.1,
             position = ggplot2::position_jitter(width=2.5,height=2.5,seed=1)) +
  ggplot2::facet_grid(~Meaning.f) +
  ggplot2::scale_fill_manual(breaks = c(F,T), values = c(other_col, r_col), labels=c("no /r/", "contains /r/")) +
  ggplot2::labs(title = "Trilled /r/ in the words ‘rough’ and ‘smooth’ cross-linguistically \n(Replication)") +
  theme_rough +
  ggplot2::theme(
    legend.position = "bottom",
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    plot.margin = ggplot2::unit(c(0.1,0.4,-0.4,0.4),"cm"),
    panel.spacing = ggplot2::unit(0, "lines"))
```

![Distribution des langues incluses dans la nouvelle analyse. Deux
groupes sont inclus : les langues TRILL et les langues
OTHER.](IE_recoding_files/figure-gfm/unnamed-chunk-155-1.png)
