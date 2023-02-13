Ressources complémentaires - Chapitre 5
================
Rémi Anselme
2023-02-13 18:50:25

  - [Chapitre 5](#chapitre-5)
      - [Présentation de l’article de Winter et
        al. (2022)](#présentation-de-larticle-de-winter-et-al-2022)
      - [Collection des données primaires et recodage des
        langues](#collection-des-données-primaires-et-recodage-des-langues)
      - [Langues recodées](#langues-recodées)
          - [Langue - Exemple](#langue---exemple)
          - [Basque](#basque)
          - [Turkish](#turkish)
          - [Gupapuyngu](#gupapuyngu)
          - [Buwandik](#buwandik)
          - [Bidjara-Gungabula](#bidjara-gungabula)
          - [Yuwaaliyaay](#yuwaaliyaay)
          - [Mandinka](#mandinka)
          - [Gurung](#gurung)
          - [Tebul\_Ure](#tebul_ure)
          - [Carapana](#carapana)
          - [Chamalal](#chamalal)
          - [Hunzib](#hunzib)
          - [Kumyk](#kumyk)

# Chapitre 5

**Les ressources supplémentaires du Chapitre 5 en cours de mise en
ligne.**

Au total, nous avons 6 datasets (Reflex, IDS, Chirila, Google, WOLD,
CLICS). `lang_info` et `clics_info` sont les deux tableaux des
ressources complémentaires de Winter et al. utlisés pour la prise de
décision des auteurs pour les rhotiques dans les langues étudiées.

``` r
rough_r_data <- readr::read_csv("donnees_winter2022/final_data/cross_linguistic.csv")
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

## Présentation de l’article de Winter et al. (2022)

``` r
rough_r_data %>%
  dplyr::filter(Family!="Indo-European") %>% 
  dplyr::filter(Meaning %in% c("rough", "smooth")) -> nb_langue
```

L’étude à visée typologique à large échelle se base sur 332 langues
appartenant à 84 familles de langues.

Au total, ce sont 179 langues avec un trill et 153 langues avec une
autre rhotique qui ont été incluses dans l’étude cross-linguistique.

## Collection des données primaires et recodage des langues

Nous partageons la *bibliographie partagée* sur Zotero :
<https://www.zotero.org/groups/4872307/anselme_2022_corpus/library> des
différentes données primaires qui ont servi au recodage des données.

## Langues recodées

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
```

Nous avons établi un patron de données à récupérer afin de pouvoir
recoder les langues en utilisant des fonctions facilitant l’utilisation
des différents tableaux de données. Nous présentons ce patron
ci-dessous. Pour des raisons de formatages, il est préférable de
consulter le document .Rmd pour s’assurer que tous les symboles
apparaissent correctement.

### Langue - Exemple

``` r
#show_lang('langue')
#show_comments('langue')
```

Trill :

R\_type :

Dataset :

Phoible :

Wikipedia :

Glottologue :

Autres sources :

Notes avant de passer au propre :

``` r
#show_lang('langue')$Form
```

``` r
#rough_r_data <- rough_r_data %>% 
#  dplyr::mutate(revision = ifelse(Language=="langue",'trilled',revision))
```

### Basque

``` r
show_lang('Basque')
```

    ## # A tibble: 4 × 6
    ##   Language Dataset Meaning Form    Trill R_type          
    ##   <chr>    <chr>   <chr>   <chr>   <chr> <chr>           
    ## 1 Basque   Google  rough   zakarra yes   mixed with trill
    ## 2 Basque   Google  smooth  leuna   yes   mixed with trill
    ## 3 Basque   CLICS   smooth  leɲ     yes   mixed with trill
    ## 4 Basque   CLICS   rough   lac̷     yes   mixed with trill

``` r
show_comments('Basque')
```

    ## # A tibble: 2 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Basque   IDS     <NA>    
    ## 2 Basque   Google  <NA>

Trill : yes  
R\_type : mixed with trill

Même si dans le mot ‘zakarra’ -\> doit correspondre au trill à cause de
<rr>

<https://phoible.org/languages/basq1248>  
Phoible : 7 inventaires  
SPA r allophones r ʁ; et ɾ allophones ɾ  
UPSID r̪|r pas d’allophones; et ɾ̪|ɾ pas d’allophones  
UZ r allophones r; ɾ allophones ɾ r  
EA 2340 r̺ pas d’allophones; et ɾ̺ pas d’allophones  
EA 2349 r̺ pas d’allophones; et ɾ̺ pas d’allophones  
EA 2583 r̺ pas d’allophones; et ɾ̺ pas d’allophones  
EA 2615 r̺ pas d’allophones; et ɾ̺ pas d’allophones

Les auteurs ont du garder UPSID.

Contraste systématiquement mentionné dans les inventaires.  
Pas de vérification de notre part des sources primaires à cause du grand
nombres d’inventaires.  
Mais si besoin :  
N’diaye, Geneviéve. 1970. Structure du dialécte basque de maya. The
Hague: Mouton.  
Gavel, Henri. 1929. Grammaire Basque. Bayonne: Courrier.  
Hualde, José Ignacio and Lujanbio, Oihana. 2010. Illustrations of the
IPA: Goizueta Basque. Journal of the International Phonetic Association
40. 113–127. Cambridge University Press.  
\- Page 113 : Rhotic tap ɾ \<-r-\>  
Rhotic trill <r->,  
\<-rr-\>  
\- Page 115 : /ɾ/ Initial: —– Medial: p are /p aɾe/ ‘pair’ Final: —–  
/r/ Initial: ro ́ eri /ro ́ e ɾi ‘drug store’ Medial: / arr ́ ai /ar ́
ai/ ‘fish’ Final: am ́ ar /am ́ ar/ ‘ten’  
\- Page 120 : Rhotics As in Spanish, the contrast between rhotic tap and
trill is only made in word-internal intervocalic position. Elsewhere the
distinction is neutralized. Unlike in Spanish, this neutralization in
Basque is generally in favor of the trill. Neither rhotic is found in
wordinitial position in the native lexicon, although the trill is found
in a few recent borrowings from Spanish, e.g. r\` adio ‘radio’. Notice
also ro ́ eri /ro ́ eɾi/ ‘drugstore’ \< Sp. droguer ́ıa. Phonetically
(also as in Spanish), rhotics are often realized without full contact,
as approximants or fricatives.

Hualde, José Ignacio. 1991. Basque phonology. (Theoretical linguistics.)
London and New York: Routledge. xiv+210pp. (Rev. version of author’s
University of Southern California dissertation Includes bibliographical
references (p. \[199\]-206) and index).  
\- Page 7 du document : Basque orthography  
rr = rhotic trill, \[\]  
r = rhotic flap \[r\] between vowels. Elsewhere the distinction between
the rhotics is neutralized.  
\- Page 8 du document : Phonetic symbols used in Basque transcriptions  
\[r\] alveolar rhotic flap  
\[r̄\] alveolar rhotic trill  
\- Page 10 : r  
r̄  
the sonorants /r,r̄/ are alveolar.  
\- Page 11 : The two rhotics segments, flap /r/ and trill /r̄/, only
contrast intervocally, as in Spanish: erre \[ere\] ‘also’, erre \[er̄e\]
‘to burn’. Elsewhere the distinction is neutralized. Unlike Spanish, in
pre-or-postconsonantal position the realization is a trilled \[r̄\] in
many Basque dialects arto \[ar̄to\] ‘corn’, andre \[andr̄e\] ‘woman’. In
the spelling, the double grapheme indicated the trill \[r̄\] between
vowels; r is a flap \[r\] intervocalically.  
\- Page 12 : Word-initially, the two rhotics, \[r\], \[r̄\] do not occur
in native words. Assimilated borrowings from Spanish with initial \[r̄\]
(orthographic r-) in the lender language undergo epenthesis, generally
of \[e-\], although there is some variation.  
\- Page 40 : In many dialects, some long vowels may be created by the
optional deletion of a consonant \[… r\] between two identical vowels
\[paratu\], \[paatu\] ‘to put’. In Arbizy, on the other hand, vowel
lenght is a distinctve feature.  
\- Page 54 : GERNIKA  
As in all other dialect, the rhotics are not found word-initially. The
trill \[r̄\] only appears word initially in unassimilated borrowings.
\[…\] In assimilated borrowings there is epenthesis. The epenthetic
vowel is \[e\] more consistently than in other dialects.  
\- Page 76 : Flapping  
In the variety ofOndarroa, intervocalic /d/ can optionally be realized
as \[r\]. In this position, there is free variation between the two
allophones \[ð\] and \[r\], although the \[r\] realization is preferred
in rapid speech. This alternation, however, is limited to certain
lexical domains, including seemingly all suffixation and cliticization
of /d/-initial third person conjugated verbal forms; see (119a). On the
other hand, across word-boundaries, outside of cliticization, and even
across the members of a compound, only \[ð\] and never \[r\] can be the
surface realization of intervocalic /d/, as shown in (119b): (120)
Flapping  
/d/ -\> \[+son\] / V\_V (optional)  
\- Page 84 : If, after final-vowel truncation, the final consonant is
/r/, it is changed to /l/:  
The consonantal change in (8) reflects a historical change in the
opposite direction, which changed I tor intervocalically:  
(9) l \> r / V\_V  
This historical change is readily observable in the treatment of Latin
loans (the examples are from Mujika (l982a)): In contemporary Basque,
however, rule (9) not only is not productive, but does not even reflect
a lexical regularity, since words with intervocalic 0\] are quite common
. The lexical generalization that holds is, rather, that stems ending in
/VrV/, not falling into the category to which rule (2) (Lowering)
applies, change their /r/ into \[1\] in their derivational form. This is
thus a case of historical rule inversion (cf. Vennemann (1972); see also
Hyman (1975: 176-178)). This rule, like the other rules discussed above,
can still be applied to new words which fulfil the necessary
requirements, even though its application is by no means obligatory.  
\- Page 122 : The only coronal that is never affected by Affective
Palatalization is the trill \[r̄\]. The other rhotic, the flap \[r\],
shows a variable behaviour; it may become (j) or \[ʎ\], it may be
deleted, or it may be unaffected.

``` r
rough_r_data <- rough_r_data %>% 
    dplyr::mutate(revision = ifelse(Language=="Basque",'OUT',NA))
```

### Turkish

``` r
show_lang('Turkish')
```

    ## # A tibble: 1 × 6
    ##   Language Dataset Meaning Form     Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>    <chr> <chr> 
    ## 1 Turkish  Google  smooth  pürüzsüz yes   trill

``` r
show_comments('Turkish')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Turkish  Google  <NA>

Trill : yes  
R\_type : trill

On a un mot avec un <r> : pürüzsüz

Google, no comments

Phoible :

Jipa Illustration : Tap - Zimmer and Orgun 1992

<https://phoible.org/languages/nucl1301>  
Et Phoible : 4 inventaires  
SPA ɾ allophones ɾ ʂ͇ ʐ͇  
UPSID r pas d’allophones  
UZ ɾ allophones ɾ ɾ̥  
EA ɾ̻

Ils ont du garder UPSID qui utilise les mêmes sources que SPA.

Pas de vérification de notre part des sources primaires (autre que JIPA)
à cause du grand nombres d’inventaires.

Mais si besoin :  
Lees, Robert B. 1961. The Phonology of Modern Standard Turkish. (Indiana
University Publications, Uralic and Altaic Series, 6.) Bloomington:
Indiana University Press.  
Underhill, Robert. 1976. Turkish Grammar. Cambridge, Mass: MIT Press.  
Swift, Lloyd B. 1963. A Reference Grammar of Modern Turkish.
Bloomington: Indiana University Press.  
Zimmer, Karl and Orgun, Orhan. 1992. Illustrations of the IPA: Turkish.
Journal of the International Phonetic Association 22. 43–45. Cambridge
University Press  
Göksel, Aslı and Kerslake, Celia. 2005. Turkish: a comprehensive
grammar. London: Routledge Curzon. xl+580pp.  
\- Page xxii : R, r produced with the tip of the tongue touching the
alveolar ridge  
\- Page 4 : /r/  
\- Page 9 : /r/ \[r\] rahat ‘comfort’, renk ‘colour’, iri ‘big’, artık
‘waste’, oruç ‘fasting’  
\[ɾ̥\] tür ‘type’, ger- ‘stretch’, iksir ‘elixir’, kar ‘snow’, mor
‘violet’  
\[r\] is a voiced alveolar tap produced with the tip of the tongue
touching the alveolar ridge. It occurs in initial and medial positions.
Its devoiced counterpart \[ɾ̥\] occurs in word-final position. /r/ is
sometimes deleted in colloquial speech, in particular in the
imperfective suffix -Iyor (see 8.2.3.3 for details) and in bir ‘a’,
‘one’. In the case of bir, this usually occurs when it is not
stressed (bir ev \[bi év\] ‘a house’) but also sometimes when it is
stressed (bir daha \[bída:\] ‘again’, sadece bir ev ‘only one hourse’)  
\- Page 14 : (ii) A single consonant alternating with its doubled
counterpart, e.g. sır ‘secret’ but sırrım ‘my secret’ (2.2)  
\- Page 17 : 2.2 ALTERNATIONS OF SINGLE CONSONANTS WITH DOUBLE
CONSONANTS  
In a small number of nouns ending in ‘b’, ‘t’, ‘d’, ‘k’, ‘l’, ‘s’, ‘z’,
‘m’ and ‘n’ the final consonant is duplicated when a vowel is attached
to it. These are words borrowed from Arabic, which, in their original
roots, have geminates, i.e. doubled consonants. In Turkish such words
appear with a single consonant in their bare form or when followed by a
suffix beginning with a consonant.  
sır ‘secret’ sırlar ‘secrets’ sırrımız ‘our secret’  
Some of these words keep their original double consonants when they
combine with the auxiliaries et- ‘do’ and ol- ‘be’ (13.3.2): hallet-
‘solve’, hallol- ‘be solved’, hisset- ‘feel’. Others appear in
nominal-verb compounds with a single consonant: hak et- ‘deserve’. It
should be noted that since bare stems do not give any indication that
they end in geminates, such words have to be learned individually. Nouns
belonging to this class are indicated in dictionary entries in the form
sır (-rrı), hat (-ttı), zıt (-ddı). The form of any nominal-verb
compounds derived from these words also has to be checked in a
dictionary.

Wikipedia :  
<https://en.wikipedia.org/wiki/Turkish_language>  
\- Tap ɾ  
<https://en.wikipedia.org/wiki/Turkish_phonology>  
\- Flap ɾ  
\- /n, t, d, s, z/ are dental \[n̪, t̪, d̪, s̪, z̪\], /ɫ/ is velarized
dental \[ɫ̪\], /ɾ/ is alveolar \[ɾ\], whereas /l/ is palatalized
post-alveolar \[l̠ʲ\].\[1\]\[8\]  
\- /ɾ/ is frequently devoiced word-finally and before a voiceless
consonant.\[3\] According to one source,\[9\] it is only realized as a
modal tap intervocalically. Word-initially, a location /ɾ/ is restricted
from occurring in native words, the constriction at the alveolar ridge
narrows sufficiently to create frication but without making full
contact, \[ɾ̞\]; the same happens in word-final position: \[ɾ̞̊\]\[9\]
(which can be mistaken for /ʃ/ or /ʂ/ by non-Turkish speakers).  
\- No word-initial /ɰ/ or /ɾ/ (in native words)  
\- In a complex coda: The first consonant is either a voiceless
fricative, /ɾ/ or /l/

Glottologue :  
<https://glottolog.org/resource/languoid/id/nucl1301>

Autres sources:  
<https://clics.clld.org/languages/northeuralex-tur>  
Recherches : r -\> 354 entries (filtered from 1,168 total entries)  
l -\> 298 entries (filtered from 1,168 total entries)  
Form in source:  
ɾ -\> 354 entries (filtered from 1,168 total entries)  
r -\> O entries (filtered from 1,168 total entries)

Gerjan van Schaaik. 2020. The Oxford Turkish Grammar. Oxford: Oxford
University Press. 784pp.  
\- Page 9 : r  
\- Page 10 : r radyo radio \[r\] as in: radio  
\- Page 11 : Two letters, r and ğ, deserve somewhat more attention. The
letter r stands for a ‘rolling r’ (as in Scottish or Spanish) at the
beginning of a syllable, but at the end of a word it is sounded with an
extra rustle in the form of an h-like sound; that is, the final r
receives strong aspiration. In this way, there is a clear audible
difference between the r in the first two words and the r in the second
pair of words.  
resim → \[ re–sim \] photo  
para → \[ pha–ra \] money  
var → \[ var^h \] there is  
dur → \[ dur^h \] stop

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Turkish",'other',revision))
```

### Gupapuyngu

``` r
show_lang('Gupapuyngu')
```

    ## # A tibble: 5 × 6
    ##   Language   Dataset Meaning Form     Trill R_type
    ##   <chr>      <chr>   <chr>   <chr>    <chr> <chr> 
    ## 1 Gupapuyngu Chirila smooth  bujubuju yes   trill 
    ## 2 Gupapuyngu Chirila smooth  bujuwuju yes   trill 
    ## 3 Gupapuyngu Chirila smooth  lapara   yes   trill 
    ## 4 Gupapuyngu Chirila rough   d̪irkt̪irk yes   trill 
    ## 5 Gupapuyngu Chirila smooth  lacu     yes   trill

``` r
show_comments('Gupapuyngu')
```

    ## # A tibble: 1 × 3
    ##   Language   Dataset Comments
    ##   <chr>      <chr>   <chr>   
    ## 1 Gupapuyngu Chirila <NA>

Trill : yes  
R\_type : trill

Chirila, no comments

Wikipedia :  
<https://en.wikipedia.org/wiki/Dhuwal_language>  
\-\> Dialect de Dhuwal language  
\-\> Dans l’inventaire phonémique de Wiki : Tap ɾ Glide ɻ

Heath, Jeffrey (1980). Dhuwal (Arnhem Land) texts on kinship and other
subjects, with grammatical sketch and dictionary. Oceania Linguistics
Monographs, 23.  
\- page 3: r is an alveolar tap, while r̠ is the sightly retroflexed
approximant r-sound found in American English

Glottologue :  
<https://glottolog.org/resource/languoid/id/gupa1247>

Anita van der Wal. 1992. Structure and function in Gupapuyngu, a Yolngu
dialect of North-east Arnhemland. (Doctoral dissertation, NSW,
Australia: University of Newcastle; viii+292pp.) -\> Pas trouvé

``` r
show_lang('Gupapuyngu')$Form
```

    ## [1] "bujubuju" "bujuwuju" "lapara"   "d̪irkt̪irk" "lacu"

Doit provenir de PHOIBLE:  
<https://phoible.org/inventories/view/2972> :  
\- r pas d’allophones et ɻ pas d’allophones

Wilkinson, Melanie P. n.d. Djambarrpuyngu, a Yolngu variety of northern
Australia. (Doctoral dissertation).
\[<https://glottolog.org/resource/languoid/id/djam1256>\]  
\-\> Dans la grammaire deux segments :  
page 41  
trill/tap (et pas uniquement trill) r <rr>  
continuant ɹ <r>

  - Page 39 : two way contrast for latrals, rhotics and semi-vowels  
  - Page 41 : RHOTICS - continuant  
    IPA ɹ  
    Orthography r  
    \- trill/tap  
    IPA r  
    Orthography rr  
  - Page 42 : The lenis member of this pari /ɖ/ is frequently realized
    as flap, perhaps a correlate of the weakening of the closure of the
    articulatoirs associated with the lenition of other lenis stops to
    semivowels.  
  - Page 45 : All consonants can occur syllable initially although there
    are few words with initial apico-alveolars.

Et comme dans Chirila online les données pour Smooth and Rough sont
absentes je ne peux non plus comparer.  
Mais dans tous les cas, deux segments qui contrastent.

J’ai pris seuleemnt en compte Heath, Jeffrey (1980). Dhuwal pour ma
décision.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Gupapuyngu",'other',revision))
```

### Buwandik

``` r
show_lang('Buwandik')
```

    ## # A tibble: 1 × 6
    ##   Language Dataset Meaning Form     Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>    <chr> <chr> 
    ## 1 Buwandik Chirila rough   batbarum yes   <NA>

``` r
show_comments('Buwandik')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Buwandik Chirila <NA>

Trill : yes  
R\_type : NA

Chirila, no comments

Dans Phoible :  
<https://phoible.org/inventories/view/2717>  
\- r pas d’allophones  
\- ɻ pas d’allophones

Blake, Barry J. n.d. The Bunganditj (Buwandik) language of the Mount
Gambier Region. (Pacific Linguistics.) Pacific Linguistics.  
\- Page 25 : There were probably two rhotics, a flap or trill, which I
represent as rr, and a glide, which I represent as r̠. These two
representations are shown in Table 13. However, since our sources do not
distinguish two rhotics I use r in our transcription, reserving rr and
r̠ for words quoted from other languages where the distinction has been
recorded.  
\- Page 27 : The distinction between r and rr is based on comparative
evidence, and does not appear in the transcription used in the rest of
this work.  
initial rr : - intervocalic : kurra kangaroo final : kapirr emu  
initial r̠ :- intervocalic: karip thigh final : mir eye

``` r
show_lang('Buwandik')$Form
```

    ## [1] "batbarum"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Buwandik",'other',revision))
```

### Bidjara-Gungabula

``` r
show_lang('Bidjara-Gungabula')
```

    ## # A tibble: 2 × 6
    ##   Language          Dataset Meaning Form       Trill R_type
    ##   <chr>             <chr>   <chr>   <chr>      <chr> <chr> 
    ## 1 Bidjara-Gungabula Chirila rough   pat̪aʈpat̪aʈ yes   <NA>  
    ## 2 Bidjara-Gungabula Chirila smooth  cuʈucuʈu   yes   <NA>

``` r
show_comments('Bidjara-Gungabula')
```

    ## # A tibble: 1 × 3
    ##   Language          Dataset Comments
    ##   <chr>             <chr>   <chr>   
    ## 1 Bidjara-Gungabula Chirila <NA>

Trill : yes  
R\_type : NA

Chirila, no comments

<http://chirila.yale.edu/languages/Bidjara-Gungabula>  
Dans Chirila :  
Pour les autres mots avec un r on a :  
gayarra -\> kajaɾa ou yira -\> jiɹa

Phoible :  
<https://phoible.org/languages/bidy1243>  
5 inventaires  
\- ER 2797 r pas d’allophones; et ɻ pas d’allophones  
\- ER 2812 ɹ̺ pas d’allophones  
\- ER 2821 r̺ pas d’allophones; et ɹ̺ pas d’allophones  
\- ER 2823 r pas d’allophones; et ɻ pas d’allophones  
\- ER 2824 ɻ pas d’allophones

Je n’ai pas vérifié les sources suivantes, mais si besoin :  
Breen, Gavan. n.d. Bidyara and Gungabula Grammar and Vocabulary.
(Linguistic Communications, 8.) Monash University.  
Breen, Gavan. n.d. Salvage studies of Western Queensland Aboriginal
languages. (Pacific Linguistics Series B.) Pacific Linguistics.

Wikipedia:  
<https://en.wikipedia.org/wiki/Bidjara_language>  
\- Rhotic r  
\- Approximant ɻ  
Dixon, Blake, Robert M. W., Barry J. (1981). Handbook of Australian
Languages, Volume 2. p. 283.  
\-\> Margany and Gunya by J,G, Breen  
\- Page 283 : Trill r  
Glide r.  
The only unusual feature of this inventory is the existence of two
series of stops, labelled above voiced and voiceless, but perhaps more
correctly lax and tense. In the environment in which they most commonly
contrast, i.e. intervocalically, the former are frequently lenited to
fricatives (in the case of /b/, /g/ and /dental d/) or a tap (/d/)  
\- Page 286 : There are slight differences in the speech of the younger
Gunya informants which would possibly result in a different distribution
of the phonemes /d/ and /r/ ; this will be discussed below.  
Intervocalically and following a lateral some stops are typically
softened to fricatives: \[…\]. /d/ in these positions becomes a tap,
occasionally heard as a stop,  
/d./ is occasionally a retroflexed flap \[ɽ\]  
Word-finally /d./ is generally a voiced stop and /d/ a tap but both tend
to be devoiced  
\- Page 288 : The trill occurs only intervocalically (the rare
occurrences of \[r\] in clusters are interpreted as realisations of
/d/). It is normally a voiced alveolar trill, sometimes prolonged after
a stressed vowel. It is rarely voiceless.  
\[examples\]  
The glides /w/, /r./ and /y/ have no noteworthy features. Note, however,
that /r./ is sometimes dropped by the younger Gunya speakers from the
concomitant suffix -bar.i, resulting in the form -bay i .  
\- Page 290 : Voiceless stops, retroflex consonants, laterals, rhotics
and /a/, which never occur initially, are omitted  
\- Page 300 : The voiced alveolar stop/flap is written d
intervocalically where it contrasts with the trill, and after a nasal,
and rr elsewhere. Thus /buda/ is buda, /gandu/ is gandu, /budgu/ is
burrgu, /waŋud/ is wangurr.

``` r
show_lang('Bidjara-Gungabula')$Form
```

    ## [1] "pat̪aʈpat̪aʈ" "cuʈucuʈu"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Bidjara-Gungabula",'other',revision))
```

### Yuwaaliyaay

``` r
show_lang('Yuwaaliyaay')
```

    ## # A tibble: 2 × 6
    ##   Language    Dataset Meaning Form     Trill R_type
    ##   <chr>       <chr>   <chr>   <chr>    <chr> <chr> 
    ## 1 Yuwaaliyaay Chirila rough   mat̪amat̪a yes   <NA>  
    ## 2 Yuwaaliyaay Chirila smooth  kajnkajn yes   <NA>

``` r
show_comments('Yuwaaliyaay')
```

    ## # A tibble: 1 × 3
    ##   Language    Dataset Comments
    ##   <chr>       <chr>   <chr>   
    ## 1 Yuwaaliyaay Chirila <NA>

Trill : yes  
R\_type : NA

Dataset :  
Chirila, no comments

Phoible :  
<https://phoible.org/inventories/view/2719>  
\- ɹ̺ pas d’allophones; r̺ pas d’allophones

Wikipedia :  
<https://en.wikipedia.org/wiki/Gamilaraay_language>  
\- Rhotic r ⟨rr⟩ ɻ ⟨r⟩

Glottologue :  
<https://glottolog.org/resource/languoid/id/gami1243>

Autres sources :

Giacon, John. n.d. A grammar of Yuwaalaraay and Gamilaraay: a
description of two New South Wales languages based on 160 years of
records. (Doctoral dissertation).  
\- Page 260 : Garriya occurs around 200 times in the tapes. The form on
the YR tapes clearly has the tap rhotic. Williams’ form (1980: 107) has
the retroflex rhotic, as does Wurm, who generally transcribes it as
gaṙija. I presume that Wurm’s informants had been influenced by
English and had lost the trill/tap.  
\- Page 495 : The tape transcripts are a major source for this study.
The major focus of my transcription was to record lexical and syntactic
features, so phonological features are under-recorded. This also made
the work of transcription simpler and made the transcription more easily
searchable. At times variation in the realisation of phonemes was
occasionally transcribed, for instance some examples of /nh/ /ny/
alternation after /i/ are transcribed. On the other hand the different
realisations of /rr/ as trill or tap (or even as an approximant) are not
noted.  
\- Page 496 : YG rhotics exemplify the challenges in describing the
phonology of YG. As in many other Australian languages YG has two
rhotics, /r/ and /rr/, with /rr/ in particular having a number of
allophones. Ridley and Mathews did not recognise phonemic differences
between the two rhotics. At times it is difficult to fully interpret the
symbols used. Wurm in his material distinguishes the retroflex
approximant rhotic (he usually records it as ‘ṙ’, also as ‘r’) from the
trill or tap (he records it as ‘ḍ’, ‘r’ and ‘r̃’). The variant
realisations of /rr/ include as a tap or trill. I assume ḍ represents
an  
alveolar tap or stop, r and r̃ a trill, but some uncertainty remains.
Some of the variation in Wurm’s rhotics are seen in Table 175. Laves did
not generally distinguish rhotics, generally using r in all words and
positions, as seen in Table 175. However on occasions he varied the
symbol, as seen in mari and barran, but the significance of the r and rr
are is uncertain. On the tapes it is often not possible to distinguish
intervocalic /r/ and /rr/.  
\- Page 497 : Tap/Trill rr  
Approximant r  
\- Page 498 : Words in YG are never vowel initial229 and begin with a
single consonant: stop, nasal (but not the apicals, /ny/ and probably
not /dj/) or glide.  
/rr/ is found word finally, intervocalically and as the first element of
a consonant cluster. /r/ is found only intervocalically, where it
contrasts with /rr/.  
\- Page 503 : There is considerable variation in the realisation of
rhotics. The variant realisation of /rr/ has been shown in Table 175. As
well rhotics are sometimes realised as laterals and occasionally as the
glide /y/ and less commonly as /w/.  
\- Page 504 : /rr/ is mostly a tap intervocalically, but at times its
realisation merges with that of /r/, i.e. an approximant. It is
generally a stop finally, unless emphasised, when it is trilled. At
times final /rr/ is not realised as in ˈgúnðì:, (gundiirr) ‘feathers’
(Wurm). FR2439A 129 has galaa-laa-nga for ‘how then?’. The standard form
of ‘how’ is galaarr, and there are other similar examples. Rr final
words which are followed by a suffix often drop the rr, particularly for
suffixes beginning in /d/ or /dh/. So yinarr- DHuul ‘woman-ONE/LITTLE’
is realised yinaduul. This feature is also fossilised in the word
biyaduul ‘alone’, from biyarr-DHuul ‘one-ONE/LITTLE’.  
There are many instances of alternation between /rr/ and /l/: ˈgálaŋài,
(garrangay) ‘duck’ (Wurm); ŋa·ru (ngaaluurr) ‘fish’, and babul (baburr)
‘foot’ (Laves). At AD5130 906 on different hearings the one token is
heard as yilaala and yirraala ‘then’. FR1988A 1114 has gulal initially,
then gularr ‘head band’. Some variation may be in the perception of the
recorders, but there are many instances where the difference is in the
production, and is consistently heard.  
Similar variation is found in other languages, sometimes with clear
conditioning factors. Threlkeld and Fraser (1892: 60) point out that for
Awakabal: ‘in the formation of the tenses and modifications, the letter
r is changed into its relative liquid l’. Hercus (1982: 193) points out
that Baagandji has ‘-la consonantal dissimilation to –ra if the
verb-stem contains an l-sound’ (/r/ in Baagandji is a tap).  
There is also rr \~ w alternation. AD 3999A 1791 has dhurrinba-nhi ‘hid’
whereas the usual form is dhuwinbanhi.

Je comprends pas cette phrase : “It is generally a stop finally, unless
emphasised, when it is trilled” -\> cf. une autre grammaire australienne
masi je ne me souviens plus de laquelle où le d peut devenir trillé en
finale et c’est relatviement expliqué

Dans Chirila: rr -\> r ou r -\> ɹ

``` r
show_lang('Yuwaaliyaay')$Form
```

    ## [1] "mat̪amat̪a" "kajnkajn"

Constraste entre trill et non trill donc ‘OUT’.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Yuwaaliyaay",'OUT',revision))
```

### Mandinka

``` r
show_lang('Mandinka')
```

    ## # A tibble: 1 × 6
    ##   Language Dataset Meaning Form  Trill R_type
    ##   <chr>    <chr>   <chr>   <chr> <chr> <chr> 
    ## 1 Mandinka Reflex  smooth  nuŋku yes   trill

``` r
show_comments('Mandinka')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Mandinka Reflex  <NA>

Trill : yes  
R\_type : trill

Dataset :  
Reflex, no comments

Phoible : <https://phoible.org/inventories/view/1126>  
\- ŋ allophones ŋ l r m n ɲ; r allphones r

Drame, Mallafe. 1981. Aspects of Mandingo Grammar. (Doctoral
dissertation, University of Illinois at Urbana-Champaign).

Wikipedia :  
<https://en.wikipedia.org/wiki/Mandinka_language>  
\- Approximant l (r)  
\- /r/ is only found initially in loans and onomatopoeia. Otherwise it
is the intervocalic allophone of /d/.  
\- Syllable-final /r/ and /s/ are found in French loans (e.g. /kùrtù/
“pants”).

Glottologue :  
<https://glottolog.org/resource/languoid/id/mand1436>

Autres sources : NA

Notes avant de passer au propre :

La seul entrée de Reflex dans notre échantillon

Denis Creissels and Pierre Sambou. 2013. Le mandinka: phonologie,
grammaire, textes. Paris: Karthala. 639pp.  
\- Page 26 : vibrante r  
On doit toutefois noter que le r est très rare en position initiale  
\- Page 29 : 2.3.3.2. L’alternance entre r et d  
Lorsqu’un suffixe commence par un r, ce r n’apparaît tel quel qu’après
une base se terminant par une voyelle. Après nasale, il laisse la place
à d, ce qui entraîne pour la nasale la réalisation n. Par exemple avec
le suffixe d’instrument -ráŋ :  
túlúŋ ‘jouer’ + -ráŋ → túlúndáŋ ‘jouet’  
2.3.3.3. L’alternance entre r et l  
Lorsqu’un suffixe commence par un r, ce r est remplacé par l lorsque la
dernière syllabe de la base a pour attaque un r. Par exemple avec le
suffixe antipassif -ri :  
mará ‘gouverner’ + -rí → mara-lí forme antipassive de mara  
‘gouverner’́  
Nous avons toutefois observé que certains locuteurs ne respectent pas
cette règle, et maintiennent r avec les bases qui déclenchent chez
d’autres l’apparition d’un l.

Denis Creissels. 2020. A sketch of Mandinka. In Friederike Lüpke (ed.),
The Oxford guide to the Atlantic languages of West Africa, 1-36. Oxford:
Oxford University Press.  
Dramé, Maŋ Lafi. 2003. Parlons Mandinka. (Collection Parlons.) In Dramé,
Man Lafi (ed.) Paris: L’Harmattan. 206pp.

Cressls used vibrant term but no description  
Creissels - A sketch of Mandinka  
Versus: on wikipedia : Approximant w l (r)\[b\]  
with the note \[b\] /r/ is only found initially in loans and
onomatopoeia. Otherwise it is the intervocalic allophone of /d/.  
On the frech page: on a le tap

Dans parlon Mandika: a l’expection de c, j, n et n, toutes les consonnes
ont la même valeur phonique qu’en français

Et j’ai pas eu accès à Denis Creissels and Pierre Sambou 2013 Le
mandinka: phonologie, grammaire, textes mais dans sa grammaire que jai
touvé sur tsétsé \[je pense que c’est un draft pour la version de
2013\]:  
On doit toutefois noter que le r est très rare en position initialee  
Lorsqu’un suffixe commence par un r, ce r n’apparaît tel quel qu’après
une base se terminant par une voyelle. Après nasale, il laisse la place
à d, ce qui entraîne pour la nasale la réalisation n.  
Lorsqu’un suffixe commence par un r, ce r est remplacé par l lorsque la
dernière syllabe de la base a pour attaque un r.  
Nous avons toutefois observé que certains locuteurs ne respectent pas
cette règle, et maintiennent r avec les bases qui déclenchent chez
d’autres l’apparition d’un l.

``` r
show_lang('Mandinka')$Form
```

    ## [1] "nuŋku"

On garde quand même ‘trilled’ à cause du terme ‘vibrant’ mais c’est
probablement ‘other’.

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Mandinka",'trilled',revision))
```

### Gurung

``` r
show_lang('Gurung')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form    Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>   <chr> <chr> 
    ## 1 Gurung   CLICS   smooth  ciqllo  yes   trill 
    ## 2 Gurung   CLICS   rough   khāsroq yes   trill

``` r
show_comments('Gurung')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments                                                     
    ##   <chr>    <chr>   <chr>                                                        
    ## 1 Gurung   CLICS   https://audio-video.shanti.virginia.edu/video/gurung-man-des…

Trill : yes  
R\_type : trill

Dataset : CLICS,
<https://audio-video.shanti.virginia.edu/video/gurung-man-describes-otar-village>

Phoible :  
<https://phoible.org/inventories/view/2344>  
\- ɾ pas d’allophones

Namkung, Ju. 1996. Phonological Inventories of Tibeto-Burman Languages.
(STEDT Monograph Series, 3.) In Namkung, Ju (ed.) Berkeley: Center for
Southeast Asia Studies. xxvii+507pp.

Glover, Warren W. 1974. Sememic and Grammatical Structures in Gurung
(Nepal). (Summer Institute of Linguistics: Publications in Linguistics,
49.) Norman, Oklahoma: The Summer Institute of Linguistics and the
University of Texas at Arlington. xxiii+232pp.

<http://www.sil.org/acpub/repository/10819_front.pdf> -\> ne fonctionne
pas  
<http://www.sil.org/acpub/repository/10819.pdf> -\> ne fonctionne pas

Wikipedia :  
<https://en.wikipedia.org/wiki/Gurung_language>

Glottologue :  
ggn; gvr  
<https://glottolog.org/resource/languoid/id/east2345>  
<https://glottolog.org/resource/languoid/id/guru1261>

Autres sources : NA

Notes avant de passer au propre :

<https://audio-video.shanti.virginia.edu/video/gurung-man-describes-otar-village>  
Ils ont pris leur décision que sur un audio?  
La qualité de l’audio est mauvaise.  
Exporté sur Praat -\> grosse, grosse majorité de tap mais je n’exclu pas
des trills

Dans R. Nakkeerar, Gurung  
\- Page 468 : /r/ is alveolar trill  
initially /ri/ ‘woman’  
/ric/ ‘niece’  
/ra/ ‘goat’  
medially /krase/ ‘head’  
/krõi/ ‘crab’  
/taurõ/ ‘fly’  
finally /sar/ ‘star’  
/Thãr/ ‘mountain’

ciqllo  
khāsroq

``` r
show_lang('Gurung')$Form
```

    ## [1] "ciqllo"  "khāsroq"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Gurung",'trilled',revision))
```

### Tebul\_Ure

``` r
show_lang('Tebul_Ure')
```

    ## # A tibble: 1 × 6
    ##   Language  Dataset Meaning Form    Trill R_type
    ##   <chr>     <chr>   <chr>   <chr>   <chr> <chr> 
    ## 1 Tebul_Ure CLICS   smooth  ɔ́rⁿɔ́nɛ̀: yes   trill

``` r
show_comments('Tebul_Ure')
```

    ## # A tibble: 1 × 3
    ##   Language  Dataset Comments                                                    
    ##   <chr>     <chr>   <chr>                                                       
    ## 1 Tebul_Ure CLICS   Blench, Dendo, & Douyon, 2005 (ms). Tebul Ure, a language o…

Trill : yes  
R\_type : trill

Dataset :  
CLICS, Blench, Dendo, & Douyon, 2005 (ms). Tebul Ure, a language of the
Dogon group in Northern Mali and its affinities

Phoible : NA

Wikipedia :  
<https://en.wikipedia.org/wiki/Tebul_Dogon>

Glottologue :  
<https://glottolog.org/resource/languoid/id/tebu1239>

Autres sources : NA

Notes avant de passer au propre :

Blench, Dendo, & Douyon, 2005 (ms). Tebul Ure, a language of the Dogon
group in Northern Mali and its affinities -\> 1 page sur la phonologie
et mention de trill dans le tableau

Heath, Jeffrey. 2014. A Grammar of Tebul Ure. Draft Ms., Nov 2014.
353pp  
A Grammar of Tebul Ure (Dogon, Mali) Jeffrey Heath -\> pas de mention de
trill, pas de manière d’articultion dans le tableau  
le r^n est juste une sonorante

ɔ́rⁿɔ́n

``` r
show_lang('Tebul_Ure')$Form
```

    ## [1] "ɔ́rⁿɔ́nɛ̀:"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Tebul_Ure",'NA',revision))
```

### Carapana

``` r
show_lang('Carapana')
```

    ## # A tibble: 2 × 6
    ##   Language Dataset Meaning Form   Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>  <chr> <chr> 
    ## 1 Carapana CLICS   smooth  jãbẽ́rõ yes   trill 
    ## 2 Carapana CLICS   smooth  jabé   yes   trill

``` r
show_comments('Carapana')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments                                                     
    ##   <chr>    <chr>   <chr>                                                        
    ## 1 Carapana CLICS   "\"vibrante\"; Instituto Linguistico de Verano (1973) Sistem…

Trill : yes  
R\_type : trill

Dataset : CLICS, “vibrante”; Instituto Linguistico de Verano (1973)
Sistemas fonologicos de idiomas colombianos. Vol 2. Editorial Townsend.
p. 126.

Phoible :  
<https://phoible.org/inventories/view/2054>  
\- ɾ pas d’allophones

Metzger, Ronald and Metzger, Lois. 1973. Fonolog’ia del carapana. In
Waterhouse, Viola G. (ed.), 121–132. Bogotá: Ministerio de Gobierno.

Wikipedia :  
<https://en.wikipedia.org/wiki/Carapana_language>

Glottologue :  
<https://glottolog.org/resource/languoid/id/cara1272>

Autres sources : NA

Notes avant de passer au propre :

“vibrante”; Instituto Linguistico de Verano (1973) Sistemas fonologicos
de idiomas colombianos. Vol 2. Editorial Townsend. p. 126.

ça parle de vibrante mais dans la section vibrante ça donne trois
allophones n l r -\> mais le dernier c’est r avec le circonflexe inversé
-\> tap

jãbẽ́rõ  
jabé

``` r
show_lang('Carapana')$Form
```

    ## [1] "jãbẽ́rõ" "jabé"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Carapana",'other',revision))
```

### Chamalal

``` r
show_lang('Chamalal')
```

    ## # A tibble: 4 × 6
    ##   Language Dataset Meaning Form       Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>      <chr> <chr> 
    ## 1 Chamalal CLICS   smooth  šaⁿšub     yes   trill 
    ## 2 Chamalal CLICS   rough   ƛ̄ʼaɣub     yes   trill 
    ## 3 Chamalal CLICS   rough   qačʼub     yes   trill 
    ## 4 Chamalal CLICS   rough   bihu-bicub yes   trill

``` r
show_comments('Chamalal')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments                                 
    ##   <chr>    <chr>   <chr>                                    
    ## 1 Chamalal CLICS   https://omniglot.com/writing/chamalal.htm

Trill : yes  
R\_type : trill

Dataset : CLICS, <https://omniglot.com/writing/chamalal.htm>

Phoible : NA

Wikipedia :  
<https://en.wikipedia.org/wiki/Chamalal_language>

Glottologue :  
<https://glottolog.org/resource/languoid/id/cham1309>

Autres sources : NA

Notes avant de passer au propre :

šaⁿšub

<https://omniglot.com/writing/chamalal.htm> -\> pas ouf comme ref

J’ai essayé de regardé pour le Karata et ça ne me donne que non-nasal
sonorant  
même cressels utilise la même terminologie pour le Northern Akhvakh
non-nasal sonorant r (neutralization of the /r/ vs. /n/ distinction
occurs in suffixes attached to roots including a nasal vowel)  
Je pense que Karata a réutilisé la term de cresseils qui l’avait déjà
peut-être prise de quelqu’un.e

Pas de mention de trill

Dans a Grammar of Hinuq Forket Diana - pas de trill mentionné mais
liquide

``` r
show_lang('Chamalal')$Form
```

    ## [1] "šaⁿšub"     "ƛ̄ʼaɣub"     "qačʼub"     "bihu-bicub"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Chamalal",'NA',revision))
```

### Hunzib

``` r
show_lang('Hunzib')
```

    ## # A tibble: 3 × 6
    ##   Language Dataset Meaning Form     Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>    <chr> <chr> 
    ## 1 Hunzib   CLICS   smooth  kakalu   yes   trill 
    ## 2 Hunzib   CLICS   rough   čʼečʼeru yes   trill 
    ## 3 Hunzib   CLICS   rough   qačʼab   yes   trill

``` r
show_comments('Hunzib')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments
    ##   <chr>    <chr>   <chr>   
    ## 1 Hunzib   CLICS   wiki

Trill : yes  
R\_type : trill

Dataset :  
CLICS, wiki

Phoible :  
<https://phoible.org/inventories/view/2237>  
\- r allophones r

Van den Berg, Helma. 1995. A Grammar of Hunzib. (1.) Lincom Europa.  
\- Page 19 : resonant r  
\- Page 21 : Variation with resonants  
/r/ and /l/ are distinct phonemes. However, I found free variation
between /r/ and /l/ even with a single informant in raxas/laxas ‘chain’,
seluk/seruk ‘pancake’, cak’urt’an/c’ak’ult’an ‘beet’, and
akic’ar/akic’al ‘tale’.  
\- Page 23 : All consonants occur in initial, medial and final
position.  
\- Page 24 : /r/ ris ‘oesophagus’, hare ‘eye’, xor ‘ram’  
Consonant groups  
Resonant plus obstruent  
Resonant plus resonant  
\- Page 25 : Geminate (=\> pas de rr)  
mm  
nn  
ll kell.u ‘old’, worn out’, telli ‘much’  
\- Page 31 : Instead of the class prefixes b- and -r (see 4.1.2. and
5.4.) we find m- and n-, respectively, before roots with nasalised
vowels. After these prefixes the vowel is denalised. Exemples:  
q’era n-ac’e ‘X sees the child’ (child/5 5-see-PRES)  
q’era r-iq’e ‘X knows the child’ (child/5 5-know-PRES)

Wikipedia :  
<https://en.wikipedia.org/wiki/Hunzib_language>  
\- Trill r

Glottologue :  
<https://glottolog.org/resource/languoid/id/hunz1247>

Autres sources : NA

Notes avant de passer au propre :

Wiki anglais : trill -\> aucune idée d’où vient le trill  
Wiki français : liquide

considéré comme une resonant avec m n l r y w  
I found free variation between /r/ and /l/ even with a single informant

Pas l’air d’y avoir des trills

``` r
show_lang('Hunzib')$Form
```

    ## [1] "kakalu"   "čʼečʼeru" "qačʼab"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Hunzib",'NA',revision))
```

### Kumyk

``` r
show_lang('Kumyk')
```

    ## # A tibble: 4 × 6
    ##   Language Dataset Meaning Form     Trill R_type
    ##   <chr>    <chr>   <chr>   <chr>    <chr> <chr> 
    ## 1 Kumyk    CLICS   smooth  tegiš    yes   trill 
    ## 2 Kumyk    CLICS   smooth  tüz      yes   trill 
    ## 3 Kumyk    CLICS   rough   išɣɨnsɨz yes   trill 
    ## 4 Kumyk    CLICS   rough   tiri     yes   trill

``` r
show_comments('Kumyk')
```

    ## # A tibble: 1 × 3
    ##   Language Dataset Comments                                  
    ##   <chr>    <chr>   <chr>                                     
    ## 1 Kumyk    CLICS   https://www.omniglot.com/writing/kumyk.php

Trill : yes  
R\_type : trill

Dataset :  
CLICS, <https://www.omniglot.com/writing/kumyk.php>

Phoible :  
<https://phoible.org/inventories/view/2355>  
\- r pas d’allophones

Doniyorova, Saodat. 2004. Parlons Koumyk. (Parlons.) Paris: L’Harmattan.
179pp. (Includes bibliographical references p. \[175\]).

Wikipedia :  
<https://en.wikipedia.org/wiki/Kumyk_language>  
\- Liquid rhotic r ⟨p⟩

Glottologue :  
<https://glottolog.org/resource/languoid/id/kumy1244>

Autres sources :

Abdullaeva, A. Z. and Gadžiaxmedov, N. È and Kadyradžiev, K. S. and
Kerimov, I. A. and Ol’mesov, N. X. and Xangišiev, D. M. 2014.
Sovremennyj Kumykskij Jazyk. 2nd edn. Maxačkala: RAN. 557pp.  
Современный кумыкский язык (Абдуллаева А. З., Гаджиахмедов Н. Э. etc.)  
\- Traduit du Russe avec DeepL - Translated with
www.DeepL.com/Translator (free version)  
\- Page 44 : The phoneme /r/ is an anteropharyngeal trembling sonorous
consonant, pronounced without tension, with the slightly elevated tip of
the tongue approaching the alveoli and the front part of the hard palate
due to the barely perceptible oscillation of the tongue tip under the
pressure of the air jet. Articulatory characteristic: it is articulated
in the intervocalic position as single-beat (one stop), before
consonants and at the end of the word as multi-beat (two or more stops).
Acoustic characteristic: always sonorous, not deafening.  
In original Turkic root morphemes the phoneme /r/, preserving its
initial distribution, is presented in auslaut and inlaut: bar “is”,
k’ara ‘black’, yara ‘wound’, bir ‘one’. In foreign-language loanwords
and and derivative words formed on their basis, it can also be in also
in the enclave: cf. razi “agree”, ragyat “calm”, russian “Russian”,
revolution. In modern Kumyk, the phoneme /r/, as in the protolanguage,
is in In today’s Kumyk language the phoneme /r/ is not used in the
auslaut position as a part of native words.  
In all positions /r/ is a voiced phoneme, except in Auslaut, where it is
noticeably deafened: k’ar “snow”, tar “narrow”. The phoneme /r/ is
characterized by is characterized by stability not only in root but also
in affixal morphemes.  
When used with a preceding consonant, the last and /r/ act as a syllable
division boundary: k’ui-ruk “tail”, bay-rak “flag”, siyrek “rarely”.  
\- Page 45 : The phoneme /r/ is combined with almost all vowels, and in
preposition it is also found in close proximity to most consonants.  
The palatal variant of the phoneme /r/ after the vowels and in the
vicinity of /u/ and /i/ belongs to the supramental ones; in other cases
it has an apical character: cf. on the one hand, bir “one”, bery “here”,
beryu “wolf”, on the other hand, yuryu “walk”, yer “earth”.  
Its velar variant is formed in the area of the hard palate or its front
part, depending on the nature of the neighboring vowel: it is
mid-lingual next to /a/ and /o/, mid-lingual next to /u/, it is
intermediate under /y/. Cf. ara “gap”, orman “forest”, rugy “spirit”,
ryzky “sustenance, daily bread”.  
\- Page 75 : Consonant Dropout  
Consonants, like vowels, drop out both in the middle and at the end of a
word, depending on the phonetic conditions. The sonorants \[l\], \[r\]
fall out in the middle of the word, and \[n\], \[l\], \[r\] fall out at
the end of the word.  
If affixes beginning with non-sonor consonants are added to the verb
stem bol “to be”, the \[l\] sound is dropped out, for example: bosun \<
bolsun “let there be”; busa \< bolsa “if there be”.  
When future tense verbs containing affixes that begin with a narrow
vowel (-yr -up, -yp -yur) are joined to personal affixes of the 1st and
2nd singular and plural, the final consonant \[p\] of the verbal base in
oral speech drops out: salyman \< salyrman “I will put”; salysan \<
salyrsan “you will put”; salybyz \< salyrbyz “we will put”; salyzyz \<
salyrsyz “you will put”.  
As we see, in all these cases the loss of \[p\] does not affect the
meaning of the word The vowels of the base affixes, the preceding narrow
vowels carry the main semantic load of the affixes. When the vowels are
wide, \[p\] does not drop out. The loss of the consonants \[n\], \[l\],
\[p\] in the final position is also due to the narrow vowel affixes.  
\- Page 76 : Metathesis  
The Kumyk language is also characterized by the transposition of
adjacent sounds. Such rearrangement involves adjacent consonants as well
as consonants and vowels. Metathesis is most common in oral speech, as
well as in dialects.  
Of the neighboring consonants, mainly the anterior, middle and posterior
consonants are rearranged:  
lr \> rl: shalbar\> sharbal “pants”;  
rn \> nr: khurjun \> khunjur “sack”;  
\- Page 77 : The prothetic vowels also appear before words ending in the
smooth consonants \[p\], \[l\]: rus \> orus “Russian”, rushbat \>
urushbat “bribe”. In this case the appearance of prothetic vowels is
explained by the fact that in Kumyk, as in other Turkic languages, the
sonorities \[r\], \[l\] usually do not beginnings of a word.

Notes avant de passer au propre :

<https://www.omniglot.com/writing/kumyk.php>  
dans le lien free variaiton entre le tap et le trill

WWikipedia : liquid rhotic  
et le wiki français juste liquide

Parlous Koumyk: p se prononce r sans grasseyement

Et j’arrive pas à trouver des références publiées

<https://phoible.org/inventories/view/2355> -\> pas d’allophones pour
/r/

En Karachay, on a trill dans le tableau mais aucune description

``` r
show_lang('Kumyk')$Form
```

    ## [1] "tegiš"    "tüz"      "išɣɨnsɨz" "tiri"

``` r
rough_r_data <- rough_r_data %>% 
  dplyr::mutate(revision = ifelse(Language=="Kumyk",'trilled',revision))
```
