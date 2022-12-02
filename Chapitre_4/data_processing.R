
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#-------------------------DATA PREPARATION-------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

# Diacritics

diactrics <- c("ˈ","ˌ","ː","ˑ","ʼ","ʴ","ʰ","ʱ","ʲ","ʷ","ˠ","ˤ","˞","̊","̆","̋",
               "̈","̴","̽","̚","̃","́","̄","̀","̏","̌","̂","̥","̤","̪",
               "̬","̰","̺","̼","̻","̹","̜","̟","̠","̝","̩","̞","̯","̘",".","*","˳")

#------------------------------------------------------------------
#------------------------------------------------------------------


# For Europe:

## Data loading
df3Eu <- readr::read_csv("Data_SoundComparison/Customexport_2020-04-04 18_47.csv") %>% 
  dplyr::select(LanguageId,LanguageName,Latitude,Longitude,Phonetic) %>% 
  dplyr::filter(!is.na(Phonetic))

## Creation of a table to align the transcriptions
df3EuAnal <- df3Eu %>% 
  dplyr::group_by(LanguageId) %>% 
  dplyr::mutate(Phonemes = strsplit(Phonetic, ""),
                Lenght = NA,
                Phonemes2 = NA,
                Lenght2 = NA,
                phoneme_1= NA,phoneme_2= NA,
                phoneme_3= NA,phoneme_4= NA,
                phoneme_5= NA,
                phoneme_6= NA,
                phoneme_7= NA) %>% 
  dplyr::ungroup()

## Splitting of the transcritpion and removing of the diacritics
for(i in 1:nrow(df3EuAnal)){
  df3EuAnal$Lenght[i] <- length(df3EuAnal$Phonemes[[i]])
  df3EuAnal$Phonemes2[i] <- list((df3EuAnal$Phonemes[[i]])[!df3EuAnal$Phonemes[[i]] %in%
                                                             diactrics])
  df3EuAnal$Lenght2[i] <- length(df3EuAnal$Phonemes2[[i]])
}


## Filling of the table with the data
for(i in 1:nrow(df3EuAnal)){
  for(j in 1:max(df3EuAnal$Lenght2)){
    df3EuAnal[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      df3EuAnal$Phonemes2[[i]][j]
  }
}
###Take some time to compute but it works

## We take out the missing remaing data
df3EuAnal <- df3EuAnal %>% dplyr::filter(Lenght2 > 0)

##We save the data
save(df3EuAnal, file = "df3EuAnal.RData")

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

# For Romance:

#------------------------------------------------------------------

#Cluster:

## Data loading
df3Ro <- readr::read_csv("Data_SoundComparison/Customexport_2020-04-09 19_59.csv") %>% 
  dplyr::select(LanguageId,LanguageName,Latitude,Longitude,Phonetic) %>% 
  dplyr::filter(!is.na(Phonetic))

## Creation of a table to align the transcriptions
df3RoAnal <- df3Ro %>% 
  dplyr::group_by(LanguageId) %>% 
  dplyr::mutate(Phonemes = strsplit(Phonetic, ""),
                Lenght = NA,
                Phonemes2 = NA,
                Lenght2 = NA,
                phoneme_1= NA,phoneme_2= NA,
                phoneme_3= NA,phoneme_4= NA,
                phoneme_5= NA,
                phoneme_6= NA,
                phoneme_7= NA) %>% 
  dplyr::ungroup()

## Splitting of the transcritpion and removing of the diacritics
for(i in 1:nrow(df3RoAnal)){
  df3RoAnal$Lenght[i] <- length(df3RoAnal$Phonemes[[i]])
  df3RoAnal$Phonemes2[i] <- list((df3RoAnal$Phonemes[[i]])[!df3RoAnal$Phonemes[[i]] %in%
                                                             diactrics])
  df3RoAnal$Lenght2[i] <- length(df3RoAnal$Phonemes2[[i]])
}

## Filling of the table with the data
for(i in 1:nrow(df3RoAnal)){
  for(j in 1:max(df3RoAnal$Lenght2)){
    df3RoAnal[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      df3RoAnal$Phonemes2[[i]][j]
  }
}
###Take some time to compute but it works

## We take out the missing remaing data
df3RoAnal <- df3RoAnal %>% dplyr::filter(Lenght2 > 0)

##We save the data
save(df3RoAnal, file = "df3RoAnal.RData")


#------------------------------------------------------------------

#Word-initial position:

## Data loading
dfroueRo <- readr::read_csv("Data_SoundComparison/Customexport_2020-04-09 19_48.csv") %>% 
  dplyr::select(LanguageId,LanguageName,Latitude,Longitude,Phonetic) %>% 
  dplyr::filter(!is.na(Phonetic))


## Creation of a table to align the transcriptions
dfroueRoAnal <- dfroueRo %>% 
  dplyr::group_by(LanguageId) %>% 
  dplyr::mutate(Phonemes = strsplit(Phonetic, ""),
                Lenght = NA,
                Phonemes2 = NA,
                Lenght2 = NA,
                phoneme_1= NA,phoneme_2= NA,
                phoneme_3= NA,phoneme_4= NA,
                phoneme_5= NA,
                phoneme_6= NA,
                phoneme_7= NA,
                phoneme_8= NA) %>% 
  dplyr::ungroup()

## Splitting of the transcritpion and removing of the diacritics
for(i in 1:nrow(dfroueRoAnal)){
  dfroueRoAnal$Lenght[i] <- length(dfroueRoAnal$Phonemes[[i]])
  dfroueRoAnal$Phonemes2[i] <- list((dfroueRoAnal$Phonemes[[i]])[!dfroueRoAnal$Phonemes[[i]] %in%
                                                                   diactrics])
  dfroueRoAnal$Lenght2[i] <- length(dfroueRoAnal$Phonemes2[[i]])
}

#max(dfroueRoAnal$Lenght2)

## Filling of the table with the data
for(i in 1:nrow(dfroueRoAnal)){
  for(j in 1:max(dfroueRoAnal$Lenght2)){
    dfroueRoAnal[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfroueRoAnal$Phonemes2[[i]][j]
  }
}
###Take some time to compute but it works

## We take out the missing remaing data
dfroueRoAnal <- dfroueRoAnal %>% dplyr::filter(Lenght2 > 0)

##We save the data
save(dfroueRoAnal, file = "dfroueRoAnal.RData")

#------------------------------------------------------------------

#Intervocalic position:

## Data loading
dfWaitRo <- readr::read_csv("Data_SoundComparison/Customexport_2020-04-10 10_51.csv") %>% 
  dplyr::select(LanguageId,LanguageName,Latitude,Longitude,Phonetic) %>% 
  dplyr::filter(!is.na(Phonetic))

## Creation of a table to align the transcriptions
dfWaitRoAnal <- dfWaitRo %>% 
  dplyr::group_by(LanguageId) %>% 
  dplyr::mutate(Phonemes = strsplit(Phonetic, ""),
                Lenght = NA,
                Phonemes2 = NA,
                Lenght2 = NA,
                phoneme_1= NA,phoneme_2= NA,
                phoneme_3= NA,phoneme_4= NA,
                phoneme_5= NA,
                phoneme_6= NA,
                phoneme_7= NA,
                phoneme_8= NA,
                phoneme_9= NA,
                phoneme_10= NA,
                phoneme_11= NA) %>% 
  dplyr::ungroup()

## Splitting of the transcritpion and removing of the diacritics
for(i in 1:nrow(dfWaitRoAnal)){
  dfWaitRoAnal$Lenght[i] <- length(dfWaitRoAnal$Phonemes[[i]])
  dfWaitRoAnal$Phonemes2[i] <- list((dfWaitRoAnal$Phonemes[[i]])[!dfWaitRoAnal$Phonemes[[i]] %in%
                                                                   diactrics])
  dfWaitRoAnal$Lenght2[i] <- length(dfWaitRoAnal$Phonemes2[[i]])
}

#max(dfWaitRoAnal$Lenght2)

## Filling of the table with the data
for(i in 1:nrow(dfWaitRoAnal)){
  for(j in 1:max(dfWaitRoAnal$Lenght2)){
    dfWaitRoAnal[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfWaitRoAnal$Phonemes2[[i]][j]
  }
}
#Take some time to compute but it works

## We take out the missing remaing data
dfWaitRoAnal <- dfWaitRoAnal %>% dplyr::filter(Lenght2 > 0)

##We save the data
save(dfWaitRoAnal, file = "dfWaitRoAnal.RData")


#------------------------------------------------------------------

# Word initial position

## Data loading
dffastRo <- readr::read_csv("Data_SoundComparison/Customexport_2020-04-11 16_29.csv") %>% 
  dplyr::select(LanguageId,LanguageName,Latitude,Longitude,Phonetic,NotCognateWithMainWordInThisFamily) %>% 
  dplyr::filter(NotCognateWithMainWordInThisFamily == 0) %>% 
  dplyr::filter(!is.na(Phonetic)) %>% 
  dplyr::filter(Phonetic != "**" & Phonetic != "..") %>% 
  dplyr::select(-NotCognateWithMainWordInThisFamily)

## Creation of a table to align the transcriptions
dffastRoAnal <- dffastRo %>% 
  dplyr::group_by(LanguageId) %>% 
  dplyr::mutate(Phonemes = strsplit(Phonetic, ""),
                Lenght = NA,
                Phonemes2 = NA,
                Lenght2 = NA,
                phoneme_1= NA,phoneme_2= NA,
                phoneme_3= NA,phoneme_4= NA,
                phoneme_5= NA,phoneme_6= NA,
                phoneme_7= NA,phoneme_8= NA,
                phoneme_9= NA,phoneme_10= NA,
                phoneme_11= NA,phoneme_12= NA) %>% 
  dplyr::ungroup()

## Splitting of the transcritpion and removing of the diacritics
for(i in 1:nrow(dffastRoAnal)){
  dffastRoAnal$Lenght[i] <- length(dffastRoAnal$Phonemes[[i]])
  dffastRoAnal$Phonemes2[i] <- list((dffastRoAnal$Phonemes[[i]])[!dffastRoAnal$Phonemes[[i]] %in%
                                                                   diactrics])
  dffastRoAnal$Lenght2[i] <- length(dffastRoAnal$Phonemes2[[i]])
}

#max(dffastRoAnal$Lenght2)

## Filling of the table with the data
for(i in 1:nrow(dffastRoAnal)){
  for(j in 1:max(dffastRoAnal$Lenght2)){
    dffastRoAnal[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dffastRoAnal$Phonemes2[[i]][j]
  }
}
#Take some time to compute but it works

## We take out the missing remaing data
dffastRoAnal <- dffastRoAnal %>% dplyr::filter(Lenght2 > 0)

##We save the data
save(dffastRoAnal, file = "dffastRoAnal.RData")


#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

#Consitency 25 - Romance

dfR25 <- readr::read_csv("Data_SoundComparison/languages_R20.csv") %>% 
  dplyr::select(-c(WordModernName2,	WordProtoName2,SpellingAltv2)) %>% 
  dplyr::filter(NotCognateWithMainWordInThisFamily == 0) %>% 
  dplyr::filter(!is.na(Phonetic)) %>% 
  dplyr::select(-NotCognateWithMainWordInThisFamily)

## Extracting the r-like segment based on what we be expecting
dfR25Anal <- dfR25 %>% dplyr::mutate(nbofR = stringr::str_count(Phonetic,"r|ɹ|χ|ɾ|ʁ|ɻ|x|ɽ|ʂ|ɦ|ħ|h|ʀ|ɣ|ʕ|ɰ|z̺")) %>% 
  dplyr::mutate(R = stringr::str_extract_all(dfR25$Phonetic,"r|ɹ|χ|ɾ|ʁ|ɻ|x|ɽ|ʂ|ɦ|ħ|h|ʕ|ʀ|ɣ|ɰ|z̺")) %>% 
  dplyr::mutate(phoneme_1 = NA,
                phoneme_2 = NA,
                phoneme_3 = NA)

## Filling of the table with the data
for(i in 1:nrow(dfR25Anal)){
  for(j in 1:max(dfR25Anal$nbofR)){
    dfR25Anal[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfR25Anal$R[[i]][j]
  }
}

## We wanted just one r-like segment per word:

dfR25Anal <- dfR25Anal %>% dplyr::mutate(phoneR =
                               dplyr::case_when(
                                 phoneme_1 == "h" & phoneme_2 == "r" ~ "r",
                                 phoneme_1 == "r" & phoneme_2 == "ɣ" ~ "r",
                                 phoneme_1 == "h" & phoneme_2 == "ɾ" ~ "ɾ",
                                 phoneme_1 == "ɾ" & phoneme_2 == "ɣ" ~ "ɾ",
                                 phoneme_1 == "x" & phoneme_2 == "r" ~ "r",
                                 phoneme_1 == "r" & phoneme_2 == "ɹ" ~ "r",
                                 phoneme_1 == "ɣ" & phoneme_2 == "ɾ" ~ "ɾ",
                                 phoneme_1 == "r" & phoneme_2 == "r" ~ "r",
                                 phoneme_1 == "ɾ" & phoneme_2 == "x" ~ "ɾ",
                                 phoneme_1 == "h" & phoneme_2 == "χ" ~ "χ",
                                 phoneme_1 == "h" & phoneme_2 == "ʁ" ~ "ʁ",
                                 phoneme_1 == "h" & phoneme_2 == "ɰ" ~ "ɰ",
                                 phoneme_1 == "ʁ" & phoneme_2 == "χ" ~ "χ",
                                 phoneme_1 == "ɾ" & phoneme_2 == "ʂ" ~ "ɾ",
                                 phoneme_1 == "r" & phoneme_2 == "x" ~ "r",
                                 phoneme_1 == "x" & phoneme_2 == "ɾ" ~ "ɾ",
                                 phoneme_1 == "ɾ" & phoneme_2 == "h" ~ "ɾ",
                                 phoneme_1 == "ʁ" & phoneme_2 == "h" ~ "ʁ",
                                 phoneme_1 == "x" & phoneme_2 == "ʁ" ~ "ʁ",
                                 phoneme_1 == "ɹ" & phoneme_2 == "ɹ" ~ "ɹ",
                                 phoneme_1 == "ɣ" & phoneme_2 == "ɹ" ~ "ɹ",
                                 phoneme_1 == "ʂ" & phoneme_2 == "ɻ" ~ "ɻ",
                                 phoneme_1 == "ɾ" & phoneme_2 == "ɾ" ~ "ɾ",
                                 phoneme_1 == "ɦ" & phoneme_2 == "ɾ" ~ "ɾ",
                                 phoneme_1 == "ħ" & phoneme_2 == "ħ" ~ "ħ",
                                 phoneme_1 == "x" & phoneme_2 == "x" ~ "x",
                                 phoneme_1 == "ɾ" & phoneme_2 == "z̺" ~ "ɾ",
                                 phoneme_1 == "ɾ" & phoneme_2 == "ɹ" ~ "ɾ",
                                 is.na(phoneme_2) ~ phoneme_1,
                               )
)


dfR25Anal <- dfR25Anal %>% dplyr::select(-c(phoneme_1,phoneme_2,phoneme_3,nbofR,R)) %>%
  dplyr::filter(Phonetic != "**") %>% 
  dplyr::mutate(phoneR = ifelse(is.na(phoneR),0,phoneR),
                phoneR = ifelse(phoneR=="χ","ʁ",phoneR),
                phoneR = ifelse(phoneR=="x","ɣ",phoneR),
                phoneR = ifelse(phoneR=="h","ɦ",phoneR),
                phoneR = ifelse(phoneR=="ħ","ʕ",phoneR))

##We save the data
save(dfR25Anal, file = "dfR25Anal.RData")


#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

#Consitency 50 - Romance


## Data loading - Two files because the collection of the data was made twice
dfR30 <- readr::read_csv("Data_SoundComparison/Customexport_2020-04-27 21_13.csv") %>% 
  dplyr::select(-c(WordModernName2,	WordProtoName2,SpellingAltv2)) %>% 
  dplyr::filter(NotCognateWithMainWordInThisFamily == 0) %>% 
  dplyr::filter(!is.na(Phonetic)) %>% 
  dplyr::select(-NotCognateWithMainWordInThisFamily) 

dfR25 <- readr::read_csv("Data_SoundComparison/languages_R20.csv") %>% 
  dplyr::select(-c(WordModernName2,	WordProtoName2,SpellingAltv2)) %>% 
  dplyr::filter(NotCognateWithMainWordInThisFamily == 0) %>% 
  dplyr::filter(!is.na(Phonetic)) %>% 
  dplyr::select(-NotCognateWithMainWordInThisFamily)


##Binding of the two files
dfR50 <- dplyr::bind_rows(dfR30,dfR25)

## Extracting the r-like segment based on what we be expecting
dfR50filtre1 <- dfR50 %>% dplyr::mutate(nbofR = stringr::str_count(Phonetic,"r|ɹ|χ|ɾ|ʁ|ɻ|x|ɽ|ʂ|ɦ|ħ|h|ʀ|ɣ|ʕ|ɰ|z̺")) %>% 
  dplyr::mutate(R = stringr::str_extract_all(dfR50$Phonetic,"r|ɹ|χ|ɾ|ʁ|ɻ|x|ɽ|ʂ|ɦ|ħ|h|ʕ|ʀ|ɣ|ɰ|z̺")) %>%  
  dplyr::mutate(phoneme_1 = NA,
                phoneme_2 = NA,
                phoneme_3 = NA)

## Filling of the table with the data
for(i in 1:nrow(dfR50filtre1)){
  for(j in 1:max(dfR50filtre1$nbofR)){
    dfR50filtre1[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfR50filtre1$R[[i]][j]
  }
}


## We wanted just one r-like segment per word:

dfR50filtre1 <- dfR50filtre1 %>% dplyr::mutate(phoneR =
                                                 dplyr::case_when(
                                                   phoneme_1 == "h" & phoneme_2 == "r" ~ "r",
                                                   phoneme_1 == "r" & phoneme_2 == "ɣ" ~ "r",
                                                   phoneme_1 == "h" & phoneme_2 == "ɾ" ~ "ɾ",
                                                   phoneme_1 == "ɾ" & phoneme_2 == "ɣ" ~ "ɾ",
                                                   phoneme_1 == "x" & phoneme_2 == "r" ~ "r",
                                                   phoneme_1 == "r" & phoneme_2 == "ɹ" ~ "r",
                                                   phoneme_1 == "ɣ" & phoneme_2 == "ɾ" ~ "ɾ",
                                                   phoneme_1 == "r" & phoneme_2 == "r" ~ "r",
                                                   phoneme_1 == "ɾ" & phoneme_2 == "x" ~ "ɾ",
                                                   phoneme_1 == "h" & phoneme_2 == "χ" ~ "χ",
                                                   phoneme_1 == "h" & phoneme_2 == "ʁ" ~ "ʁ",
                                                   phoneme_1 == "h" & phoneme_2 == "ɰ" ~ "ɰ",
                                                   phoneme_1 == "ʁ" & phoneme_2 == "χ" ~ "χ",
                                                   phoneme_1 == "ɾ" & phoneme_2 == "ʂ" ~ "ɾ",
                                                   phoneme_1 == "r" & phoneme_2 == "x" ~ "r",
                                                   phoneme_1 == "x" & phoneme_2 == "ɾ" ~ "ɾ",
                                                   phoneme_1 == "ɾ" & phoneme_2 == "h" ~ "ɾ",
                                                   phoneme_1 == "ʁ" & phoneme_2 == "h" ~ "ʁ",
                                                   phoneme_1 == "x" & phoneme_2 == "ʁ" ~ "ʁ",
                                                   phoneme_1 == "ɹ" & phoneme_2 == "ɹ" ~ "ɹ",
                                                   phoneme_1 == "ɣ" & phoneme_2 == "ɹ" ~ "ɹ",
                                                   phoneme_1 == "ʂ" & phoneme_2 == "ɻ" ~ "ɻ",
                                                   phoneme_1 == "ɾ" & phoneme_2 == "ɾ" ~ "ɾ",
                                                   phoneme_1 == "ɦ" & phoneme_2 == "ɾ" ~ "ɾ",
                                                   phoneme_1 == "ħ" & phoneme_2 == "ħ" ~ "ħ",
                                                   phoneme_1 == "x" & phoneme_2 == "x" ~ "x",
                                                   phoneme_1 == "ɾ" & phoneme_2 == "z̺" ~ "ɾ",
                                                   phoneme_1 == "ɾ" & phoneme_2 == "ɹ" ~ "ɾ",
                                                   is.na(phoneme_2) ~ phoneme_1,
                                                 )
)


## We decided to change the values of some segment to always have voiced phones
dfR50Anal <- dfR50filtre1 %>% dplyr::select(-c(phoneme_1,phoneme_2,phoneme_3,nbofR,R)) %>%
  dplyr::filter(!(Phonetic %in% c("**","..",".*.","**","▶"))) %>% 
  dplyr::mutate(phoneR = ifelse(is.na(phoneR),0,phoneR),
                phoneR = ifelse(phoneR=="χ","ʁ",phoneR),
                phoneR = ifelse(phoneR=="x","ɣ",phoneR),
                phoneR = ifelse(phoneR=="h","ɦ",phoneR),
                phoneR = ifelse(phoneR=="ħ","ʕ",phoneR)) %>% 
  dplyr::filter(!(WordModernName1 %in% c("name","star","blood","I_see","your_pl_fem")))


##We save the data
save(dfR50Anal, file = "dfR50Anal.RData")

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

# For Mapudungun:

## Word-initial: "House"

## Data loading
dfHouseMa <- readr::read_csv("Data_SoundComparison/Customexport_2020-04-09 22_18.csv") %>% 
  dplyr::select(LanguageId,LanguageName,Latitude,Longitude,Phonetic) %>% 
  dplyr::filter(!is.na(Phonetic))

## Creation of a table to align the transcriptions
dfHouseMaAnal <- dfHouseMa %>% 
  dplyr::group_by(LanguageId) %>% 
  dplyr::mutate(Phonemes = strsplit(Phonetic, ""),
                Lenght = NA,
                Phonemes2 = NA,
                Lenght2 = NA,
                phoneme_1= NA,phoneme_2= NA,
                phoneme_3= NA,phoneme_4= NA,
                phoneme_5= NA,phoneme_6= NA,
                phoneme_7= NA) %>% 
  dplyr::ungroup()

## Splitting of the transcritpion and removing of the diacritics
for(i in 1:nrow(dfHouseMaAnal)){
  dfHouseMaAnal$Lenght[i] <- length(dfHouseMaAnal$Phonemes[[i]])
  dfHouseMaAnal$Phonemes2[i] <- list((dfHouseMaAnal$Phonemes[[i]])[!dfHouseMaAnal$Phonemes[[i]] %in%
                                                                     diactrics])
  dfHouseMaAnal$Lenght2[i] <- length(dfHouseMaAnal$Phonemes2[[i]])
}

#max(dfHouseMaAnal$Lenght2)

## Filling of the table with the data
for(i in 1:nrow(dfHouseMaAnal)){
  for(j in 1:max(dfHouseMaAnal$Lenght2)){
    dfHouseMaAnal[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfHouseMaAnal$Phonemes2[[i]][j]
  }
}
###Take some time to compute but it works

## We take out the missing remaing data
dfHouseMaAnal <- dfHouseMaAnal %>% dplyr::filter(Lenght2 > 0)

##We save the data
save(dfHouseMaAnal, file = "dfHouseMaAnal.RData")

## Word-initial: "7"

## Data loading
df7Ma <- readr::read_csv("Data_SoundComparison/Customexport_2020-07-08 12 00.csv") %>% 
  dplyr::select(LanguageId,LanguageName,Latitude,Longitude,Phonetic) %>% 
  dplyr::filter(!is.na(Phonetic))

## Creation of a table to align the transcriptions
df7MaAnal <- df7Ma %>% 
  dplyr::group_by(LanguageId) %>% 
  dplyr::mutate(Phonemes = strsplit(Phonetic, ""),
                Lenght = NA,
                Phonemes2 = NA,
                Lenght2 = NA,
                phoneme_1= NA,phoneme_2= NA,
                phoneme_3= NA,phoneme_4= NA,
                phoneme_5= NA,phoneme_6= NA,
                phoneme_7= NA) %>% 
  dplyr::ungroup()

## Splitting of the transcritpion and removing of the diacritics
for(i in 1:nrow(df7MaAnal)){
  df7MaAnal$Lenght[i] <- length(df7MaAnal$Phonemes[[i]])
  df7MaAnal$Phonemes2[i] <- list((df7MaAnal$Phonemes[[i]])[!df7MaAnal$Phonemes[[i]] %in%
                                                                     diactrics])
  df7MaAnal$Lenght2[i] <- length(df7MaAnal$Phonemes2[[i]])
}

#max(df7MaAnal$Lenght2)

## Filling of the table with the data
for(i in 1:nrow(df7MaAnal)){
  for(j in 1:max(df7MaAnal$Lenght2)){
    df7MaAnal[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      df7MaAnal$Phonemes2[[i]][j]
  }
}
###Take some time to compute but it works

## We take out the missing remaing data
df7MaAnal <- df7MaAnal %>% dplyr::filter(Lenght2 > 0)

##We save the data
save(df7MaAnal, file = "df7MaAnal.RData")

#------------------------------------------------------------------
#------------------------------------------------------------------

# Consistency for Mapudugun

## Data loading - Two files because the collection of the data was made twice
dfMa20R <- readr::read_csv("Data_SoundComparison/Customexport_2020-07-08 14 26.csv") %>% 
  dplyr::select(-c(WordModernName2,	WordProtoName2,SpellingAltv2)) %>% 
  dplyr::filter(NotCognateWithMainWordInThisFamily == 0) %>% 
  dplyr::filter(!is.na(Phonetic)) %>% 
  dplyr::select(-NotCognateWithMainWordInThisFamily)

dfMa3R <- readr::read_csv("Data_SoundComparison/Customexport_2020-07-08 14 39.csv") %>% 
  dplyr::select(-c(WordModernName2,	WordProtoName2,SpellingAltv2)) %>% 
  dplyr::filter(NotCognateWithMainWordInThisFamily == 0) %>% 
  dplyr::filter(!is.na(Phonetic)) %>% 
  dplyr::select(-NotCognateWithMainWordInThisFamily) 

##Binding of the two files
dfMa23R <- dplyr::bind_rows(dfMa20R,dfMa3R)


## Extracting the r-like segment based on what we be expecting
dfMa23Rfiltre1 <- dfMa23R %>% 
  dplyr::mutate(nbofR = stringr::str_count(Phonetic,"ʐ|ɻ|ɾ|ʂ|ɽ|ɭ|l|r|ɹ|j|ʃ")) %>% 
  dplyr::mutate(R = stringr::str_extract_all(dfMa23R$Phonetic,"ʐ|ɻ|ɾ|ʂ|ɽ|ɭ|l|r|ɹ|j|ʃ")) %>%  
  dplyr::mutate(phoneme_1 = NA,
                phoneme_2 = NA,
                phoneme_3 = NA,
                phoneme_4 = NA,) %>% 
  dplyr::filter(!(WordModernName1 == "caliente" & nbofR == 0)) %>%
  dplyr::filter(!(WordModernName1 == "esposa" & nbofR == 0))

## Filling of the table with the data
for(i in 1:nrow(dfMa23Rfiltre1)){
  for(j in 1:max(dfMa23Rfiltre1$nbofR)){
    dfMa23Rfiltre1[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfMa23Rfiltre1$R[[i]][j]
  }
}

## We wanted just one r-like segment per word:

dfMa23Rfiltre1 <- dfMa23Rfiltre1 %>% dplyr::mutate(phoneR =
                                                 dplyr::case_when(
                                                   phoneme_1 == "ʂ" & phoneme_2 == "ʐ" &
                                                     phoneme_3 == "j" & phoneme_4 == "l" ~ "ʐ",
                                                   phoneme_1 == "ɻ" & phoneme_2 == "l" & phoneme_3 == "j" ~ "ɻ",
                                                   phoneme_1 == "r" & phoneme_2 == "l" & phoneme_3 == "j" ~ "r",
                                                   phoneme_1 == "ɻ" & phoneme_2 == "ɽ" & phoneme_3 == "j" ~ "ɽ",
                                                   phoneme_1 == "ʐ" & phoneme_2 == "j" & phoneme_3 == "l" ~ "ʐ",
                                                   phoneme_1 == "ɭ" & phoneme_2 == "j" & phoneme_3 == "l" ~ "ɭ",
                                                   phoneme_1 == "ɻ" & phoneme_2 == "j" & phoneme_3 == "l" ~ "ɻ",
                                                   phoneme_1 == "ʐ" & phoneme_2 == "j" ~ "ʐ",
                                                   phoneme_1 == "ʐ" & phoneme_2 == "l" ~ "ʐ",
                                                   phoneme_1 == "ɻ" & phoneme_2 == "j" ~ "ɻ",
                                                   phoneme_1 == "ɻ" & phoneme_2 == "l" ~ "ɻ",
                                                   phoneme_1 == "ʃ" & phoneme_2 == "j" ~ "ʃ",
                                                   phoneme_1 == "ʂ" & phoneme_2 == "j" ~ "ʂ",
                                                   phoneme_1 == "ʂ" & phoneme_2 == "l" ~ "ʂ",
                                                   phoneme_1 == "ɽ" & phoneme_2 == "l" ~ "ɽ",
                                                   phoneme_1 == "r" & phoneme_2 == "l" ~ "r",
                                                   phoneme_1 == "r" & phoneme_2 == "r" ~ "r",
                                                   phoneme_1 == "ʂ" & phoneme_2 == "ɻ" ~ "ɻ",
                                                   phoneme_1 == "ɻ" & phoneme_2 == "ʂ" ~ "ɻ",
                                                   phoneme_1 == "ʐ" & phoneme_2 == "ʂ" ~ "ʐ",
                                                   phoneme_1 == "ʐ" & phoneme_2 == "ʂ" ~ "ʐ",
                                                   phoneme_1 == "ʐ" & phoneme_2 == "ʐ" ~ "ʐ",
                                                   is.na(phoneme_2) & !is.na(phoneme_1) ~ phoneme_1,
                                                   is.na(phoneme_1) ~ "0",
                                                 )
)

## We decided to change the values of some segment to always have voiced phones
dfMa23RAnal <- dfMa23Rfiltre1 %>% dplyr::select(-c(phoneme_1,phoneme_2,phoneme_3,phoneme_4,nbofR,R)) %>%
  dplyr::mutate(phoneR = ifelse(is.na(phoneR),0,phoneR),
                phoneR = ifelse(phoneR=="ʂ","ʐ",phoneR))

##We save the data
save(dfMa23RAnal , file = "dfMa23RAnal.RData")

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

# For Quechua:

## Word-initial: "Person"

## Data loading
dfPersonQc <- readr::read_csv("Data_SoundComparison/Customexport_2020-04-09 22_14.csv") %>% 
  dplyr::select(LanguageId,LanguageName,Latitude,Longitude,Phonetic) %>% 
  dplyr::filter(!is.na(Phonetic)) %>% 
  dplyr::filter(LanguageId <= 21351778509) # Transition from Quechua to Aymara (and after Uru-Chipaya and Mapudungun)

## Creation of a table to align the transcriptions
dfPersonQcAnal <- dfPersonQc %>% 
  dplyr::group_by(LanguageId) %>% 
  dplyr::mutate(Phonemes = strsplit(Phonetic, ""),
                Lenght = NA,
                Phonemes2 = NA,
                Lenght2 = NA,
                phoneme_1= NA,phoneme_2= NA,
                phoneme_3= NA,phoneme_4= NA) %>% 
  dplyr::ungroup()

## Splitting of the transcritpion and removing of the diacritics
for(i in 1:nrow(dfPersonQcAnal)){
  dfPersonQcAnal$Lenght[i] <- length(dfPersonQcAnal$Phonemes[[i]])
  dfPersonQcAnal$Phonemes2[i] <- list((dfPersonQcAnal$Phonemes[[i]])[!dfPersonQcAnal$Phonemes[[i]] %in%
                                                                       diactrics])
  dfPersonQcAnal$Lenght2[i] <- length(dfPersonQcAnal$Phonemes2[[i]])
}

#max(dfPersonQcAnal$Lenght2)

## Filling of the table with the data
for(i in 1:nrow(dfPersonQcAnal)){
  for(j in 1:max(dfPersonQcAnal$Lenght2)){
    dfPersonQcAnal[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfPersonQcAnal$Phonemes2[[i]][j]
  }
}
###Take some time to compute but it works

## We take out the missing remaing data
dfPersonQcAnal <- dfPersonQcAnal %>% dplyr::filter(Lenght2 > 0)

##We save the data
save(dfPersonQcAnal , file = "dfPersonQcAnal.RData")

#------------------------------------------------------------------
#------------------------------------------------------------------


# Consistency for Quechua

## Data loading 
dfQc14R <- readr::read_csv("Data_SoundComparison/Customexport_2020-07-08 17 54.csv") %>% 
  dplyr::select(-c(WordModernName2,	WordProtoName2,SpellingAltv2)) %>% 
  dplyr::filter(NotCognateWithMainWordInThisFamily == 0) %>% 
  dplyr::filter(!is.na(Phonetic)) %>% 
  dplyr::filter(Phonetic != "..") %>% 
  dplyr::select(-NotCognateWithMainWordInThisFamily)


## Extracting the r-like segment based on what we be expecting
dfQc14Rfiltre1 <- dfQc14R %>% 
  dplyr::mutate(nbofR = stringr::str_count(Phonetic,"ɾ|ɹ|n|ɭ|r")) %>% 
  dplyr::mutate(R = stringr::str_extract_all(dfQc14R$Phonetic,"ɾ|ɹ|n|ɭ|r")) %>%  
  dplyr::mutate(phoneme_1 = NA,
                phoneme_2 = NA,
                phoneme_3 = NA,
                phoneme_4 = NA,
                phoneme_5 = NA) %>% 
  dplyr::filter(nbofR != 0)

## Filling of the table with the data
for(i in 1:nrow(dfQc14Rfiltre1)){
  for(j in 1:max(dfQc14Rfiltre1$nbofR)){
    dfQc14Rfiltre1[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfQc14Rfiltre1$R[[i]][j]
  }
}


## We wanted just one r-like segment per word:

dfQc14Rfiltre1 <- dfQc14Rfiltre1 %>% dplyr::mutate(phoneR =
                                                     dplyr::case_when(
                                                       phoneme_1 == "ɾ" & phoneme_2 == "ɾ" &
                                                         phoneme_3 == "ɾ" & phoneme_4 == "ɾ"  & phoneme_5 == "ɾ" ~ "ɾ",
                                                       phoneme_1 == "ɹ" & phoneme_2 == "ɹ" & phoneme_3 == "n" ~ "ɹ",
                                                       phoneme_1 == "ɹ" & phoneme_2 == "n" & phoneme_3 == "ɹ" ~ "ɹ",
                                                       phoneme_1 == "ɾ" & phoneme_2 == "n" & phoneme_3 == "ɾ" ~ "ɾ",
                                                       phoneme_1 == "ɾ" & phoneme_2 == "ɾ" ~ "ɾ",
                                                       phoneme_1 == "ɾ" & phoneme_2 == "n" ~ "ɾ",
                                                       phoneme_1 == "ɹ" & phoneme_2 == "n" ~ "ɹ",
                                                       phoneme_1 == "ɹ" & phoneme_2 == "ɹ" ~ "ɹ",
                                                       phoneme_1 == "ɹ" & phoneme_2 == "ɾ" ~ "ɾ",
                                                       phoneme_1 == "ɹ" & phoneme_2 == "ɾ" ~ "ɾ",
                                                       phoneme_1 == "ɹ" & phoneme_2 == "ɾ" ~ "ɾ",
                                                       phoneme_1 == "ɹ" & phoneme_2 == "ɾ" ~ "ɾ",
                                                       phoneme_1 == "ɹ" & phoneme_2 == "ɾ" ~ "ɾ",
                                                       phoneme_1 == "ɹ" & phoneme_2 == "ɾ" ~ "ɾ",
                                                       phoneme_1 == "ɹ" & phoneme_2 == "ɾ" ~ "ɾ",
                                                       phoneme_1 == "n" & phoneme_2 == "n" ~ "n",
                                                       phoneme_1 == "ɭ" & phoneme_2 == "ɭ" ~ "ɭ",
                                                       phoneme_1 == "ɭ" & phoneme_2 == "n" ~ "ɭ",
                                                       phoneme_1 == "n" & phoneme_2 == "ɾ" ~ "ɾ",
                                                       phoneme_1 == "n" & phoneme_2 == "ɾ" ~ "ɾ",
                                                       is.na(phoneme_2) & !is.na(phoneme_1) ~ phoneme_1,
                                                       is.na(phoneme_1) ~ "0",
                                                     )
)

## We decided to filter out some values that do not correspond to out cognates of interest
dfQc14RAnal <- dfQc14Rfiltre1 %>% dplyr::select(-c(phoneme_1,phoneme_2,phoneme_3,phoneme_4,phoneme_5,nbofR,R)) %>%
  dplyr::mutate(phoneR = ifelse(is.na(phoneR),0,phoneR)) %>% 
  dplyr::filter(WordProtoName1 != "rura" & LanguageId != "21221616509",
                WordProtoName1 != "riku" & LanguageId != "21221157709",
                WordProtoName1 != "wayra"  & LanguageId != "21111130701",
                WordProtoName1 != "wayra" & LanguageId != "21111165509",
                WordProtoName1 != "riku"  & LanguageId != "21222354501")

##We save the data
save(dfQc14RAnal, file = "dfQc14RAnal.RData")

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------




dfMaNo1R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 06.csv")
dfMaNo2R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 08.csv")
dfMaNo3R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 10.csv")
dfMaNo4R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 11.csv")
dfMaNo5R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 12.csv")
dfMaNo6R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 12(1).csv")
dfMaNo7R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 14.csv")
dfMaNo8R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 14(1).csv")
dfMaNo9R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 15.csv")
dfMaNo10R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 15(1).csv")
dfMaNo11R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 16.csv")
dfMaNo12R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 18.csv")
dfMaNo13R <- readr::read_csv("Data_SoundComparison/Vanuatu_Malakula_North/Customexport_2020-07-09 10 19.csv")

dfMaNoR <- dplyr::bind_rows(dfMaNo1R,dfMaNo2R,dfMaNo3R,dfMaNo4R,
                            dfMaNo5R,dfMaNo6R,dfMaNo7R,dfMaNo8R,
                            dfMaNo9R,dfMaNo10R,dfMaNo11R,dfMaNo12R,
                            dfMaNo13R)


dfMaNoRfiltre1 <- dfMaNoR %>% 
  dplyr::mutate(nbofR = stringr::str_count(Phonetic,"r|ɹ|ɻ|ɾ|ɽ")) %>% 
  dplyr::mutate(R = stringr::str_extract_all(dfMaNoR$Phonetic,"r|ɹ|ɻ|ɾ|ɽ")) %>%  
  dplyr::mutate(phoneme_1 = NA,
                phoneme_2 = NA,
                phoneme_3 = NA,
                phoneme_4 = NA) %>% 
  dplyr::filter(nbofR != 0)

## Filling of the table with the data
for(i in 1:nrow(dfMaNoRfiltre1)){
  for(j in 1:max(dfMaNoRfiltre1$nbofR)){
    dfMaNoRfiltre1[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfMaNoRfiltre1$R[[i]][j]
  }
}


dfMaNoRfiltre1 <- dfMaNoRfiltre1 %>% dplyr::mutate(phoneR = dplyr::case_when(
                                      phoneme_1 == "r" & phoneme_2 == "r" &
                                        phoneme_3 == "r" & phoneme_4 == "r"  ~ "r",
                                      phoneme_1 == "r" & phoneme_2 == "ɾ" & phoneme_3 == "ɾ"  ~ "ɾ",
                                      phoneme_1 == "r" & phoneme_2 == "r" & phoneme_3 == "ɾ"  ~ "r",
                                      phoneme_1 == "ɾ" & phoneme_2 == "ɾ" & phoneme_3 == "ɾ"  ~ "ɾ",
                                      phoneme_1 == "ɾ" & phoneme_2 == "ɾ"  ~ "ɾ",
                                      phoneme_1 == "r" & phoneme_2 == "ɾ"  ~ "r",
                                      phoneme_1 == "r" & phoneme_2 == "r"  ~ "r",
                                      phoneme_1 == "ɾ" & phoneme_2 == "ɾ"  ~ "r",
                                      is.na(phoneme_2) & !is.na(phoneme_1) ~ phoneme_1,
                                      is.na(phoneme_1) ~ "0",
                                                     )) 

dfMaNoRAnal <- dfMaNoRfiltre1 %>% dplyr::select(LanguageId,
                                                LanguageName,
                                                Latitude,
                                                Longitude,
                                                WordId,
                                                WordModernName1,
                                                WordProtoName1,
                                                Phonetic,
                                                SpellingAltv1,
                                                phoneR)

save(dfMaNoRAnal, file = "dfMaNoRAnal.RData")


#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------


dfWP1R <- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 18.csv")
dfWP2R <- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 18(1).csv")
dfWP3R <- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 19.csv")
dfWP4R<- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 19(1).csv")
dfWP5R <- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 19(2).csv")
dfWP6R <- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 19(3).csv")
dfWP7R <- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 19(4).csv")
dfWP8R <- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 21.csv")
dfWP9R <- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 21(1).csv")
dfWP10R <- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 21(2).csv")
dfWP11R <- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 21(3).csv")
dfWP12R <- readr::read_csv("Data_SoundComparison/West_papua/Customexport_2020-07-09 11 22.csv")


dfWPR <- dplyr::bind_rows(dfWP1R,dfWP2R,dfWP3R,dfWP4R,
                          dfWP5R,dfWP6R,dfWP7R,dfWP8R,
                          dfWP9R,dfWP10R,dfWP11R,dfWP12R)


dfWPRfiltre1 <- dfWPR %>% 
  dplyr::mutate(nbofR = stringr::str_count(Phonetic,"r|ɹ|ɻ|ɾ|ɽ")) %>% 
  dplyr::mutate(R = stringr::str_extract_all(dfWPR$Phonetic,"r|ɹ|ɻ|ɾ|ɽ")) %>%  
  dplyr::mutate(phoneme_1 = NA,
                phoneme_2 = NA,
                phoneme_3 = NA,
                phoneme_4 = NA) %>% 
  dplyr::filter(nbofR != 0)

## Filling of the table with the data
for(i in 1:nrow(dfWPRfiltre1)){
  for(j in 1:max(dfWPRfiltre1$nbofR)){
    dfWPRfiltre1[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfWPRfiltre1$R[[i]][j]
  }
}


dfWPRfiltre1 <- dfWPRfiltre1 %>% dplyr::mutate(phoneR = dplyr::case_when(
                                      phoneme_1 == "ɾ" & phoneme_2 == "ɾ" & phoneme_3 == "ɾ"  ~ "ɾ",
                                      phoneme_1 == "ɾ" & phoneme_2 == "ɾ"  ~ "ɾ",
                                      is.na(phoneme_2) & !is.na(phoneme_1) ~ phoneme_1,
                                      is.na(phoneme_1) ~ "0",
                                    )) 

dfWPRAnal <- dfWPRfiltre1 %>% dplyr::select(LanguageId,
                                                LanguageName,
                                                Latitude,
                                                Longitude,
                                                WordId,
                                                WordModernName1,
                                                WordProtoName1,
                                                Phonetic,
                                                SpellingAltv1,
                                                phoneR)

save(dfWPRAnal, file = "dfWPRAnal.RData")


#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------


Lolovoli <- readtextgrid::read_textgrid("Data_SoundComparison/Ambae_TG/East_Lolovoli.TextGrid")
SanaSolokave <- readtextgrid::read_textgrid("Data_SoundComparison/Ambae_TG/East_SanaSolokave.TextGrid")
Vuindondo <- readtextgrid::read_textgrid("Data_SoundComparison/Ambae_TG/East_Vuindondo.TextGrid")
HavaiLombaha <- readtextgrid::read_textgrid("Data_SoundComparison/Ambae_TG/Nth_HavaiLombaha.TextGrid")
Leonda <- readtextgrid::read_textgrid("Data_SoundComparison/Ambae_TG/Nth_Leonda.TextGrid")
LeoVandue <- readtextgrid::read_textgrid("Data_SoundComparison/Ambae_TG/Nth_LeoVandue.TextGrid")
Biribiri <- readtextgrid::read_textgrid("Data_SoundComparison/Ambae_TG/Sth_Biribiri.TextGrid")
Lolovele <- readtextgrid::read_textgrid("Data_SoundComparison/Ambae_TG/Sth_Lolovele.TextGrid")
Sakao <- readtextgrid::read_textgrid("Data_SoundComparison/Ambae_TG/Sth_Sakao.TextGrid")
Walaha <- readtextgrid::read_textgrid("Data_SoundComparison/Ambae_TG/West_Walaha.TextGrid")


Lolovoli <- Lolovoli %>% dplyr::filter(text != "") %>% 
  dplyr::select(text) %>% 
  dplyr::mutate(manner = ifelse(stringr::str_detect(text,"trill"),"trill",text),
                nb_trill = ifelse(stringr::str_detect(text,"trill"),stringr::str_extract(text,"[:digit:]"),NA),
                LanguageId = "48731605609")

SanaSolokave <- SanaSolokave %>% dplyr::filter(text != "") %>% 
  dplyr::select(text) %>% 
  dplyr::mutate(manner = ifelse(stringr::str_detect(text,"trill"),"trill",text),
                nb_trill = ifelse(stringr::str_detect(text,"trill"),stringr::str_extract(text,"[:digit:]"),NA),
                LanguageId = "48731608309")

Vuindondo <- Vuindondo %>% dplyr::filter(text != "") %>% 
  dplyr::select(text) %>% 
  dplyr::mutate(manner = ifelse(stringr::str_detect(text,"trill"),"trill",text),
                nb_trill = ifelse(stringr::str_detect(text,"trill"),stringr::str_extract(text,"[:digit:]"),NA),
                LanguageId = "48731603709")

HavaiLombaha <- HavaiLombaha %>% dplyr::filter(text != "") %>% 
  dplyr::select(text) %>% 
  dplyr::mutate(manner = ifelse(stringr::str_detect(text,"trill"),"trill",text),
                nb_trill = ifelse(stringr::str_detect(text,"trill"),stringr::str_extract(text,"[:digit:]"),NA),
                LanguageId = "48731305309")

Leonda <- Leonda %>% dplyr::filter(text != "") %>% 
  dplyr::select(text) %>% 
  dplyr::mutate(manner = ifelse(stringr::str_detect(text,"trill"),"trill",text),
                nb_trill = ifelse(stringr::str_detect(text,"trill"),stringr::str_extract(text,"[:digit:]"),NA),
                LanguageId = "48731303809")

LeoVandue <- LeoVandue %>% dplyr::filter(text != "") %>% 
  dplyr::select(text) %>% 
  dplyr::mutate(manner = ifelse(stringr::str_detect(text,"trill"),"trill",text),
                nb_trill = ifelse(stringr::str_detect(text,"trill"),stringr::str_extract(text,"[:digit:]"),NA),
                LanguageId = "48731307509")

Biribiri <- Biribiri %>% dplyr::filter(text != "") %>% 
  dplyr::select(text) %>% 
  dplyr::mutate(manner = ifelse(stringr::str_detect(text,"trill"),"trill",text),
                nb_trill = ifelse(stringr::str_detect(text,"trill"),stringr::str_extract(text,"[:digit:]"),NA),
                LanguageId = "48731707209")


Lolovele <- Lolovele %>% dplyr::filter(text != "") %>% 
  dplyr::select(text) %>% 
  dplyr::mutate(manner = ifelse(stringr::str_detect(text,"trill"),"trill",text),
                nb_trill = ifelse(stringr::str_detect(text,"trill"),stringr::str_extract(text,"[:digit:]"),NA),
                LanguageId = "48731705409")

Sakao <- Sakao %>% dplyr::filter(text != "") %>% 
  dplyr::select(text) %>% 
  dplyr::mutate(manner = ifelse(stringr::str_detect(text,"trill"),"trill",text),
                nb_trill = ifelse(stringr::str_detect(text,"trill"),stringr::str_extract(text,"[:digit:]"),NA),
                LanguageId = "48731703709")

Walaha <- Walaha %>% dplyr::filter(text != "") %>% 
  dplyr::select(text) %>% 
  dplyr::mutate(manner = ifelse(stringr::str_detect(text,"trill"),"trill",text),
                nb_trill = ifelse(stringr::str_detect(text,"trill"),stringr::str_extract(text,"[:digit:]"),NA),
                LanguageId = "48731405509")


dfAmR <- readr::read_csv("Data_SoundComparison/Customexport_2020-07-09 16 46.csv")

dfAmR %>% dplyr::select(LanguageId,LanguageName) %>% dplyr::distinct()

dfAmRmanner <- dplyr::bind_rows(Lolovoli,SanaSolokave,Vuindondo,HavaiLombaha,Leonda,
                                LeoVandue,Biribiri,Lolovele,Sakao,Walaha) %>% 
  dplyr::mutate(phoneR = dplyr::case_when(
    manner == "tap" | manner == "flap" ~ "ɾ",
    manner == "app" ~ "ɹ",
    manner == "trill" ~ "r"
  ))

dfAmRAnal <- dfAmR %>% dplyr::select(LanguageId,LanguageName,Latitude,Longitude) %>%
  dplyr::distinct() %>% 
  dplyr::mutate(LanguageId = as.character(LanguageId)) %>% 
  dplyr::left_join(dfAmRmanner,by="LanguageId")

save(dfAmRAnal, file = "dfAmRAnal.RData")

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

# For Slavic:

## Data loading
dfSlavic <- readr::read_csv("Data_SoundComparison/Customexport_2021-08-11_12_32.csv")%>% 
  dplyr::select(-c(WordModernName2,	WordProtoName2,SpellingAltv2)) %>% 
  dplyr::filter(NotCognateWithMainWordInThisFamily == 0) %>% 
  dplyr::filter(!is.na(Phonetic)) %>% 
  dplyr::select(-NotCognateWithMainWordInThisFamily)

## Creation of a table to align the transcriptions
dfSlavicAnal <- dfSlavic %>% 
  dplyr::group_by(LanguageId) %>% 
  dplyr::mutate(Phonemes = strsplit(Phonetic, ""),
                Lenght = NA,
                Phonemes2 = NA,
                Lenght2 = NA,
                phoneme_1= NA,phoneme_2= NA,
                phoneme_3= NA,phoneme_4= NA,
                phoneme_5= NA,
                phoneme_6= NA,
                phoneme_7= NA,
                phoneme_8= NA,
                phoneme_9= NA,
                phoneme_10= NA) %>% 
  dplyr::ungroup()

## Splitting of the transcritpion and removing of the diacritics
for(i in 1:nrow(dfSlavicAnal)){
  dfSlavicAnal$Lenght[i] <- length(dfSlavicAnal$Phonemes[[i]])
  dfSlavicAnal$Phonemes2[i] <- list((dfSlavicAnal$Phonemes[[i]])[!dfSlavicAnal$Phonemes[[i]] %in%
                                                             diactrics])
  dfSlavicAnal$Lenght2[i] <- length(dfSlavicAnal$Phonemes2[[i]])
}


## Filling of the table with the data
for(i in 1:nrow(dfSlavicAnal)){
  for(j in 1:max(dfSlavicAnal$Lenght2)){
    dfSlavicAnal[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfSlavicAnal$Phonemes2[[i]][j]
  }
}
###Take some time to compute but it works

## We take out the missing remaing data
dfSlavicAnal <- dfSlavicAnal %>% dplyr::filter(Lenght2 > 1)

##We save the data
save(dfSlavicAnal, file = "dfSlavicAnal.RData")

#------------------------------------------------------------------

## Extracting the r-like segment based on what we be expecting
dfSlavicAll <- dfSlavicAnal %>% dplyr::mutate(nbofR = stringr::str_count(Phonetic,"r|ɹ|χ|ɾ|ʁ|ɻ|x|ɽ|ʂ|ɦ|ħ|h|ʀ|ɣ|ʕ|ɰ|z|s|ʃ|ʐ|ɕ")) %>% 
  dplyr::mutate(R = stringr::str_extract_all(dfSlavicAnal$Phonetic,"r|ɹ|χ|ɾ|ʁ|ɻ|x|ɽ|ʂ|ɦ|ħ|h|ʀ|ɣ|ʕ|ɰ|z|s|ʃ|ʐ|ɕ")) %>%  
  dplyr::filter(nbofR > 0) %>% 
  dplyr::mutate(phonemeR_1 = NA,
                phonemeR_2 = NA,
                phonemeR_3 = NA)

## Filling of the table with the data
for(i in 1:nrow(dfSlavicAll)){
  for(j in 1:max(dfSlavicAll$nbofR)){
    dfSlavicAll[[as.factor(paste("phonemeR_",as.character(j),sep=""))]][i] =
      dfSlavicAll$R[[i]][j]
  }
}


dfSlavicAll1 <- dfSlavicAll %>% dplyr::mutate(phoneR =
                                                dplyr::case_when(
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "s" ~ "ɾ",
                                                  phonemeR_1 == "ʃ" & phonemeR_2 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "ʃ" ~ "ɾ",
                                                  phonemeR_1 == "z" & phonemeR_2 == "s" ~ "OUT",
                                                  phonemeR_1 == "r" & phonemeR_2 == "s" ~ "r",
                                                  phonemeR_1 == "s" & phonemeR_2 == "ʃ" ~ "OUT",
                                                  phonemeR_1 == "s" & phonemeR_2 == "s" ~ "ɾ",
                                                  phonemeR_1 == "s" & phonemeR_2 == "ɾ" & phonemeR_3 == "s" ~ "ɾ",
                                                  phonemeR_1 == "s" & phonemeR_2 == "s" ~ "OUT",
                                                  phonemeR_1 == "s" & phonemeR_2 == "r" ~ "r",
                                                  phonemeR_1 == "z" & phonemeR_2 == "ɾ" & phonemeR_3 == "s" ~ "ɾ",
                                                  phonemeR_1 == "z" & phonemeR_2 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "ʃ" & phonemeR_3 == "s" ~ "ɾ",
                                                  phonemeR_1 == "z" & phonemeR_2 == "r" & phonemeR_3 == "s" ~ "r",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "z" ~ "ɾ",
                                                  phonemeR_1 == "s" & phonemeR_2 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "r" & phonemeR_2 == "ʂ" ~ "r",
                                                  phonemeR_1 == "ʃ" & phonemeR_2 == "s" ~ "OUT",
                                                  phonemeR_1 == "ʂ" & phonemeR_2 == "ɾ" & phonemeR_3 == "s" ~ "ɾ",
                                                  phonemeR_1 == "ʃ" & phonemeR_2 == "ɾ" & phonemeR_3 == "s" ~ "ɾ",
                                                  phonemeR_1 == "ʐ" & phonemeR_2 == "ɣ" & phonemeR_3 == "s" ~ "OUT",
                                                  phonemeR_1 == "s" & phonemeR_2 == "r" & phonemeR_3 == "s" ~ "r",
                                                  phonemeR_1 == "ʐ" & phonemeR_2 == "ɾ" & phonemeR_3 == "s" ~ "ɾ",
                                                  phonemeR_1 == "ʐ" & phonemeR_2 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "ʂ" & phonemeR_2 == "s" ~ "ʂ",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "s" ~ "ɾ",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "ɕ" ~ "ɾ",
                                                  phonemeR_1 == "ʂ" & phonemeR_2 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "s" & phonemeR_2 == "z" ~ "OUT",
                                                  phonemeR_1 == "ɕ" & phonemeR_2 == "r" ~ "r",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "ʂ" ~ "ɾ",
                                                  phonemeR_1 == "z" & phonemeR_2 == "ɾ" & phonemeR_3 == "z" ~ "ɾ",
                                                  phonemeR_1 == "ʂ" & phonemeR_2 == "r" ~ "r",
                                                  phonemeR_1 == "ɦ" & phonemeR_2 == "s" & phonemeR_3 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "x" ~ "ɾ",
                                                  phonemeR_1 == "ɦ" & phonemeR_2 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "ɦ" & phonemeR_2 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "s" & phonemeR_2 == "ʂ" ~ "ʂ",
                                                  phonemeR_1 == "ɕ" & phonemeR_2 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "ʐ" & phonemeR_2 == "ʂ" ~ "ʐ",
                                                  phonemeR_1 == "z" & phonemeR_2 == "r" & phonemeR_3 == "z" ~ "r",
                                                  phonemeR_1 == "ʂ" & phonemeR_2 == "ɾ" & phonemeR_3 == "ɕ" ~ "ɾ",
                                                  phonemeR_1 == "z" & phonemeR_2 == "ʐ" ~ "ʐ",
                                                  phonemeR_1 == "ʐ" & phonemeR_2 == "s" ~ "ʐ",
                                                  phonemeR_1 == "ɕ" & phonemeR_2 == "s" ~ "OUT",
                                                  phonemeR_1 == "ʃ" & phonemeR_2 == "ɾ" & phonemeR_3 == "ɕ" ~ "ɾ",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "ɰ" & phonemeR_3 == "s" ~ "ɾ",
                                                  phonemeR_1 == "ʐ" & phonemeR_2 == "ɕ" ~ "ʐ",
                                                  phonemeR_1 == "ʐ" & phonemeR_2 == "ʃ" ~ "ʐ",
                                                  phonemeR_1 == "ʂ" & phonemeR_2 == "ɾ" & phonemeR_3 == "ʃ" ~ "ɾ",
                                                  phonemeR_1 == "ɕ" & phonemeR_2 == "ʃ" ~ "OUT",
                                                  phonemeR_1 == "ʂ" & phonemeR_2 == "ʂ" ~ "ʂ",
                                                  phonemeR_1 == "s" & phonemeR_2 == "ʐ" ~ "ʐ",
                                                  phonemeR_1 == "ʃ" & phonemeR_2 == "ɾ" & phonemeR_3 == "ʃ" ~ "ɾ",
                                                  phonemeR_1 == "ɕ" & phonemeR_2 == "ʂ" ~ "OUT",
                                                  phonemeR_1 == "ʃ" & phonemeR_2 == "ʁ" ~ "ʁ",
                                                  phonemeR_1 == "s" & phonemeR_2 == "ʀ" ~ "ʀ",
                                                  phonemeR_1 == "ʀ" & phonemeR_2 == "s" ~ "ʀ",
                                                  phonemeR_1 == "ʁ" & phonemeR_2 == "s" ~ "ʁ",
                                                  phonemeR_1 == "z" & phonemeR_2 == "ʀ" ~ "ʀ",
                                                  phonemeR_1 == "ʁ" & phonemeR_2 == "ʃ" ~ "ʁ",
                                                  phonemeR_1 == "z" & phonemeR_2 == "z" ~ "OUT",
                                                  phonemeR_1 == "ʃ" & phonemeR_2 == "ʀ" ~ "ʀ",
                                                  phonemeR_1 == "s" & phonemeR_2 == "ʁ" ~ "ʁ",
                                                  phonemeR_1 == "ʁ" & phonemeR_2 == "ʁ" ~ "ʁ",
                                                  phonemeR_1 == "z" & phonemeR_2 == "ʁ" ~ "ʁ",
                                                  phonemeR_1 == "ɹ" & phonemeR_2 == "s" ~ "ɹ",
                                                  phonemeR_1 == "ɹ" & phonemeR_2 == "ʂ" ~ "ɹ",
                                                  phonemeR_1 == "z" & phonemeR_2 == "ɹ" ~ "ɹ",
                                                  phonemeR_1 == "ʂ" & phonemeR_2 == "ɹ" ~ "ɹ",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "r" ~ "ɾ",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "r" & phonemeR_2 == "x" ~ "r",
                                                  phonemeR_1 == "z" & phonemeR_2 == "r" ~ "r",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "χ" ~ "ɾ",
                                                  phonemeR_1 == "r" & phonemeR_2 == "ɕ" ~ "r",
                                                  phonemeR_1 == "ɣ" & phonemeR_2 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "s" & phonemeR_2 == "ɾ" & phonemeR_3 == "z" ~ "ɾ",
                                                  phonemeR_1 == "s" & phonemeR_2 == "ɾ" & phonemeR_3 == "ɾ" ~ "ɾ",
                                                  is.na(phonemeR_2) & phonemeR_1 == "z" ~ "OUT",
                                                  is.na(phonemeR_2) & phonemeR_1 == "ɦ" ~ "OUT",
                                                  is.na(phonemeR_2) & phonemeR_1 == "ɰ" ~ "OUT",
                                                  is.na(phonemeR_2) & phonemeR_1 == "ɣ" ~ "OUT",
                                                  is.na(phonemeR_2) & phonemeR_1 == "x" ~ "OUT",
                                                  is.na(phonemeR_2) & phonemeR_1 == "s" & WordProtoName1 != "tri" ~ "OUT",
                                                  is.na(phonemeR_2) & phonemeR_1 == "ʃ" & WordProtoName1 != "krylo" ~ "OUT",
                                                  is.na(phonemeR_2) & phonemeR_1 == "ʂ" & !(WordProtoName1 %in% c("tri","veter","mokroe",
                                                                                                                  "krov_","trava")) ~ "OUT",
                                                  is.na(phonemeR_2) & phonemeR_1 == "ʐ" & !(WordProtoName1 %in% c("more","koren_","reki",
                                                                                                                  "grom","v_more")) ~ "OUT",
                                                  is.na(phonemeR_2) & phonemeR_1 == "ɕ" &!(WordProtoName1 %in% c("tri","krylo")) ~ "OUT",
                                                  is.na(phonemeR_2) ~ phonemeR_1,
                                                ),
                                              phoneR = ifelse(phoneR=="χ","ʁ",phoneR)
) %>% 
  dplyr::filter(phoneR != "OUT")

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

# For Germanic:

## Data loading

dfGermanic <- readr::read_csv("Data_SoundComparison/Customexport_2021-08-16_17_47.csv")%>% 
  dplyr::select(-c(WordModernName2,	WordProtoName2,SpellingAltv2)) %>% 
  dplyr::filter(NotCognateWithMainWordInThisFamily == 0) %>% 
  dplyr::filter(!is.na(Phonetic)) %>% 
  dplyr::select(-NotCognateWithMainWordInThisFamily)

## Creation of a table to align the transcriptions
dfGermanicAnal <- dfGermanic %>% 
  dplyr::group_by(LanguageId) %>% 
  dplyr::mutate(Phonemes = strsplit(Phonetic, ""),
                Lenght = NA,
                Phonemes2 = NA,
                Lenght2 = NA,
                phoneme_1= NA,phoneme_2= NA,
                phoneme_3= NA,phoneme_4= NA,
                phoneme_5= NA,
                phoneme_6= NA,
                phoneme_7= NA,
                phoneme_8= NA,
                phoneme_9= NA,
                phoneme_10= NA) %>% 
  dplyr::ungroup()

## Splitting of the transcritpion and removing of the diacritics
for(i in 1:nrow(dfGermanicAnal)){
  dfGermanicAnal$Lenght[i] <- length(dfGermanicAnal$Phonemes[[i]])
  dfGermanicAnal$Phonemes2[i] <- list((dfGermanicAnal$Phonemes[[i]])[!dfGermanicAnal$Phonemes[[i]] %in%
                                                                   diactrics])
  dfGermanicAnal$Lenght2[i] <- length(dfGermanicAnal$Phonemes2[[i]])
}


## Filling of the table with the data
for(i in 1:nrow(dfGermanicAnal)){
  for(j in 1:max(dfGermanicAnal$Lenght2)){
    dfGermanicAnal[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfGermanicAnal$Phonemes2[[i]][j]
  }
}
###Take some time to compute but it works

## We take out the missing remaing data
dfGermanicAnal <- dfGermanicAnal %>% dplyr::filter(Lenght2 > 1)

##We save the data
save(dfGermanicAnal, file = "dfGermanicAnal.RData")


#------------------------------------------------------------------

## Extracting the r-like segment based on what we be expecting
dfGermanicAll <- dfGermanicAnal %>% dplyr::mutate(nbofR = stringr::str_count(Phonetic,"r|ɹ|χ|ɾ|ʁ|ɻ|ɽ|ʀ|ɣ")) %>% 
  dplyr::mutate(R = stringr::str_extract_all(dfGermanicAnal$Phonetic,"r|ɹ|χ|ɾ|ʁ|ɻ|ɽ|ʀ|ɣ")) %>%  
  dplyr::filter(nbofR > 0) %>% 
  dplyr::mutate(phonemeR_1 = NA,
                phonemeR_2 = NA,
                phonemeR_3 = NA)

## Filling of the table with the data
for(i in 1:nrow(dfGermanicAll)){
  for(j in 1:max(dfGermanicAll$nbofR)){
    dfGermanicAll[[as.factor(paste("phonemeR_",as.character(j),sep=""))]][i] =
      dfGermanicAll$R[[i]][j]
  }
}

dfGermanicAll1 <- dfGermanicAll %>% dplyr::mutate(phoneR =
                                                dplyr::case_when(
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "s" ~ "ɾ",
                                                  phonemeR_1 == "ʃ" & phonemeR_2 == "ɾ" ~ "ɾ",
                                                  phonemeR_1 == "ɾ" & phonemeR_2 == "ʃ" ~ "ɾ"))


#------------------------------------------------------------------
#------------------------------------------------------------------

# New preparatop, for Mapudugun

dfMaNewR <- readr::read_csv("Data_SoundComparison/Customexport_2022-08-10 22 00.csv") %>% 
  dplyr::select(-c(WordModernName2,	WordProtoName2,SpellingAltv2)) %>% 
  dplyr::filter(NotCognateWithMainWordInThisFamily == 0) %>% 
  dplyr::filter(!is.na(Phonetic)) %>% 
  dplyr::select(-NotCognateWithMainWordInThisFamily)

liste_word_tr_mapu <- (dfMaNewR %>% 
  dplyr::filter((stringr::str_detect(SpellingAltv1,"tr") == TRUE  & stringr::str_detect(SpellingAltv1,"^r") == FALSE) |
                stringr::str_detect(SpellingAltv1,"r") == FALSE))$WordId

dfMaNewR <- dfMaNewR %>% 
  dplyr::filter(!(WordId %in% liste_word_tr_mapu))

## Extracting the r-like segment based on what we be expecting
dfMaNewRfiltre1 <- dfMaNewR %>% 
  dplyr::mutate(nbofR = stringr::str_count(Phonetic,"ʐ|ɻ|ɾ|ʂ|ɽ|ɭ|l|r|ɹ|j|ʃ")) %>% 
  dplyr::mutate(R = stringr::str_extract_all(dfMaNewR$Phonetic,"ʐ|ɻ|ɾ|ʂ|ɽ|ɭ|l|r|ɹ|j|ʃ")) %>%  
  dplyr::mutate(phoneme_1 = NA,
                phoneme_2 = NA,
                phoneme_3 = NA,
                phoneme_4 = NA,) %>% 
  dplyr::filter(!(WordModernName1 == "caliente" & nbofR == 0)) %>%
  dplyr::filter(!(WordModernName1 == "esposa" & nbofR == 0))

## Filling of the table with the data
for(i in 1:nrow(dfMaNewRfiltre1)){
  for(j in 1:max(dfMaNewRfiltre1$nbofR)){
    dfMaNewRfiltre1[[as.factor(paste("phoneme_",as.character(j),sep=""))]][i] =
      dfMaNewRfiltre1$R[[i]][j]
  }
}

## We wanted just one r-like segment per word:

dfMaNewRfiltre1 <- dfMaNewRfiltre1 %>% dplyr::mutate(phoneR =
                                                     dplyr::case_when(
                                                       phoneme_1 == "ʂ" & phoneme_2 == "ʐ" &
                                                         phoneme_3 == "j" & phoneme_4 == "l" ~ "ʐ",
                                                       phoneme_1 == "ɻ" & phoneme_2 == "l" & phoneme_3 == "j" ~ "ɻ",
                                                       phoneme_1 == "r" & phoneme_2 == "l" & phoneme_3 == "j" ~ "r",
                                                       phoneme_1 == "ɻ" & phoneme_2 == "ɽ" & phoneme_3 == "j" ~ "ɽ",
                                                       phoneme_1 == "ʐ" & phoneme_2 == "j" & phoneme_3 == "l" ~ "ʐ",
                                                       phoneme_1 == "ɭ" & phoneme_2 == "j" & phoneme_3 == "l" ~ "ɭ",
                                                       phoneme_1 == "ɻ" & phoneme_2 == "j" & phoneme_3 == "l" ~ "ɻ",
                                                       phoneme_1 == "ʐ" & phoneme_2 == "j" ~ "ʐ",
                                                       phoneme_1 == "ʐ" & phoneme_2 == "l" ~ "ʐ",
                                                       phoneme_1 == "ɻ" & phoneme_2 == "j" ~ "ɻ",
                                                       phoneme_1 == "ɻ" & phoneme_2 == "l" ~ "ɻ",
                                                       phoneme_1 == "ʃ" & phoneme_2 == "j" ~ "ʃ",
                                                       phoneme_1 == "ʂ" & phoneme_2 == "j" ~ "ʂ",
                                                       phoneme_1 == "ʂ" & phoneme_2 == "l" ~ "ʂ",
                                                       phoneme_1 == "ɽ" & phoneme_2 == "l" ~ "ɽ",
                                                       phoneme_1 == "r" & phoneme_2 == "l" ~ "r",
                                                       phoneme_1 == "r" & phoneme_2 == "r" ~ "r",
                                                       phoneme_1 == "ʂ" & phoneme_2 == "ɻ" ~ "ɻ",
                                                       phoneme_1 == "ɻ" & phoneme_2 == "ʂ" ~ "ɻ",
                                                       phoneme_1 == "ʐ" & phoneme_2 == "ʂ" ~ "ʐ",
                                                       phoneme_1 == "ʐ" & phoneme_2 == "ʂ" ~ "ʐ",
                                                       phoneme_1 == "ʐ" & phoneme_2 == "ʐ" ~ "ʐ",
                                                       is.na(phoneme_2) & !is.na(phoneme_1) ~ phoneme_1,
                                                       is.na(phoneme_1) ~ "0",
                                                     )
)

## We decided to change the values of some segment to always have voiced phones
dfMaNewRAnal <- dfMaNewRfiltre1 %>% dplyr::select(-c(phoneme_1,phoneme_2,phoneme_3,phoneme_4,nbofR,R)) %>%
  dplyr::mutate(phoneR = ifelse(is.na(phoneR),0,phoneR),
                phoneR = ifelse(phoneR=="ʂ","ʐ",phoneR))

##We save the data
save(dfMaNewRAnal, file = "dfMaNewRAnal.RData")
