#define load and install new packages
libraries = c("readxl", "xlsx", "readr", "rstudioapi", "readstata13", "ggplot2", "dplyr", "stargazer", "ggthemes", "confintr", "tidyverse") #, "spatstat", "Hmisc", "haven","foreign",
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


#Get citations
lapply(libraries, citation)
#set working directory to location of script
setwd(dirname(getSourceEditorContext()$path))
#Function to transform education system datasets
education_system_build_data<-function(dataset, years_dif, ignore_transition=F){
  dat_out<-data.frame(bundesland=character(), birth_year=numeric(), Value=character())
  for (bl in 1:length(bundesländer_hellbig)){
    bl_helbig<-bundesländer_hellbig[bl]
    bl_soep<-bundesländer_soep[bl]
    new_rows<-data.frame(bundesland=rep(bl_soep, length(min(dataset$Jahr):2009)), birth_year=((min(dataset$Jahr)-years_dif):(2009-years_dif)), Value=as.character(unlist(dataset[,colnames(dataset)==bl_helbig])))
    if(ignore_transition==F) new_rows$Value[which(new_rows$Value!=lag(new_rows$Value))]<-paste0("Transition ",lag(new_rows$Value)[which(new_rows$Value!=lag(new_rows$Value))] , " to ", new_rows$Value[which(new_rows$Value!=lag(new_rows$Value))])
    dat_out<-rbind(dat_out, new_rows)
  }
  return(dat_out)
}
sheets<-excel_sheets(path = "Datasets/Bildungssyteme Klassifikation Helbig 2015.xlsx")
datasets<-lapply(1:8, function (x) read_excel("Datasets/Bildungssyteme Klassifikation Helbig 2015.xlsx", sheet=sheets[x]))
#vorschule<-datasets[[1]]
#schulpflicht<-datasets[[2]]
#dauer_abi<-datasets[[3]]
#grundschuldauer<-datasets[[4]]
#gesamtschule_als_regelschulform<-datasets[[5]]
#zweigliedrigkeit<-datasets[[6]]
#anteil_gym_abitur<-datasets[[7]]
#alternative_wege_abi<-datasets[[8]]

bundesländer_hellbig<-c("BW","BY","BE","BB","HB","HH","HE","MV","NI","NW","RP","SL","SN","ST","SH","TH")
bundesländer_soep<-c("[1] Baden-Wuerttemberg","[2] Bayern","[3] Berlin","[4] Brandenburg","[5] Bremen","[6] Hamburg","[7] Hessen",
                "[8] Mecklenburg-Vorpommern","[9] Niedersachsen","[10] Nordrhein-Westfalen","[11] Rheinland-Pfalz","[12] Saarland",
                "[13] Sachsen","[14] Sachsen-Anhalt","[15] Schleswig-Holstein","[16] Thueringen")


#Vorschule
vorschule<-datasets[[1]]
# If a reform was introduced in 1960 (school year 1960/61), which is the first birth cohort affected?
# 1952 is not affected
# 1953 is partially affected #if you are born in the end of 1953, you are affected
# 1954 is affected
# Dataset begins 1949/50
# First Cohort affected is born in 1944
# difference year introduction and cohort affected
years_dif=7
dataset<-vorschule[1:which(is.na(vorschule$Jahr))[1]-1,]

dat_vorschule<-education_system_build_data(dataset = dataset, years_dif = years_dif)

#view(dat_vorschule)
dat_vorschule$Value<-gsub("0", "verpflichtend", dat_vorschule$Value)
dat_vorschule$Value<-gsub("1", "vorhanden", dat_vorschule$Value)
dat_vorschule$Value<-gsub("2", "nicht vorhanden", dat_vorschule$Value)
dat_vorschule$Value<-gsub("3", "Übergang FLEX", dat_vorschule$Value)
dat_vorschule$Value<-gsub("4", NA, dat_vorschule$Value)
dat_vorschule$Value<-gsub("5", NA, dat_vorschule$Value)
colnames(dat_vorschule)[3]="Vorschule"

#Grundschuldauer
grundschuldauer<-datasets[[4]]
# If a reform was introduced in 1960 (school year 1960/61), which is the first birth cohort affected?
# 1948 is not affected
# 1949 is partially affected #if you are born in the end of 1953, you are affected
# 1950 is affected
# Dataset begins 1949/50
# First Cohort affected is born in 1944
colnames(vorschule)
# difference year introduction and cohort partially affected
years_dif=11
dataset<-grundschuldauer[1:which(is.na(grundschuldauer$Jahr))[1]-1,]

dat_grundschuldauer<-education_system_build_data(dataset = dataset, years_dif = years_dif)

dat_grundschuldauer$Value<-gsub("-1", NA, dat_grundschuldauer$Value)
dat_grundschuldauer$Value<-gsub("-2", NA, dat_grundschuldauer$Value)
dat_grundschuldauer$Value<-gsub("-3", NA, dat_grundschuldauer$Value)
dat_grundschuldauer$Value<-gsub("1", "Orientierungsstufe", dat_grundschuldauer$Value)
dat_grundschuldauer$Value<-gsub("2", "Förderstufe", dat_grundschuldauer$Value)
colnames(dat_grundschuldauer)[3]="Grundschuldauer"
dat_grundschuldauer$Aufteilung_Sekundarschule<-dat_grundschuldauer$Grundschuldauer
dat_grundschuldauer$Aufteilung_Sekundarschule[dat_grundschuldauer$Grundschuldauer%in%c("Orientierungsstufe","Förderstufe")]<-"6"
dat_grundschuldauer$Aufteilung_Sekundarschule[dat_grundschuldauer$Aufteilung_Sekundarschule=="4"]<-"5"
dat_grundschuldauer$Aufteilung_Sekundarschule[dat_grundschuldauer$Aufteilung_Sekundarschule=="6"]<-"7"

#Dauer Schulpflicht
schulpflicht<-datasets[[2]]
# If a reform was introduced in 1960 (school year 1960/61), which is the first birth cohort affected?
# increase 8 to 9 years
# 1960/61 Einschulung ist Geburtsjahr 1953/54
# 1944 is not affected
# 1945 is partially affected #if you are born in the end of 1953, you are affected
# 1946 is affected
# Dataset begins 1949/50

# increase 9 to 10 years
# 1960/61 Einschulung ist Geburtsjahr 1953/54
# 1943 is not affected
# 1944 is partially affected #if you are born in the end of 1953, you are affected
# 1945 is affected
# Dataset begins 1949/50
# First Cohort affected is born in 1944
colnames(vorschule)
# difference year introduction and cohort partially affected


dataset<-schulpflicht[1:which(is.na(schulpflicht$Jahr))[1]-1,]

dat_schulpflicht<-data.frame(bundesland=character(), birth_year=numeric(), Value=character())
for (bl in 1:length(bundesländer_hellbig)){
  bl_helbig<-bundesländer_hellbig[bl]
  bl_soep<-bundesländer_soep[bl]
  new_rows<-data.frame(bundesland=character(), birth_year=numeric(), Value=character())
  for (year in 1940:1993){
    if(dataset[dataset$Jahr==year+16,colnames(dataset)==bl_helbig]=="10") {
      value="10"
    } else {
      if(dataset[dataset$Jahr==year+15,colnames(dataset)==bl_helbig]=="9") {
        value="9"
      } else {
        if(dataset[dataset$Jahr==year+14,colnames(dataset)==bl_helbig]=="8") {
          value="8"
        } else {
          if (dataset[dataset$Jahr==year+14,colnames(dataset)==bl_helbig]=="DDR") value="DDR" else value=NA
        }
      }
    }
    new_row<-data.frame(bundesland=bl_soep, birth_year=year, Value=as.character(value))
    new_rows<-rbind(new_rows, new_row)
  }
  new_rows$Value[which(new_rows$Value!=lag(new_rows$Value))]<-paste0("Transition ",lag(new_rows$Value)[which(new_rows$Value!=lag(new_rows$Value))] , " to ", new_rows$Value[which(new_rows$Value!=lag(new_rows$Value))])
  dat_schulpflicht<-rbind(dat_schulpflicht, new_rows)
}
colnames(dat_schulpflicht)[3]<-"Vollschulzeitpflicht"

#Gesamtschule
gesamtschule_als_regelschulform<-datasets[[5]]
# If a reform was introduced in 1960 (school year 1960/61), which is the first birth cohort affected?
# 1948 is not affected
# 1949 is partially affected #if you are born in the end of 1953, you are affected
# 1950 is affected
# Dataset begins 1949/50
# First Cohort affected is born in 1944
# difference year introduction and cohort partially affected
years_dif=11
dataset<-gesamtschule_als_regelschulform[1:which(is.na(gesamtschule_als_regelschulform$Jahr))[1]-1,]

dat_gesamtschule1<-education_system_build_data(dataset = dataset, years_dif = years_dif)
dat_gesamtschule1$Value<-gsub("-2", NA, dat_gesamtschule1$Value)
dat_gesamtschule1$Value<-gsub("1", "0", dat_gesamtschule1$Value)
dat_gesamtschule1$Value<-gsub("2", "1", dat_gesamtschule1$Value)
colnames(dat_gesamtschule1)[3]<-"Gesamtschule_Regelschule"

#Gesamtschule
zweigliedrigkeit<-datasets[[6]]
years_dif=11
dataset<-zweigliedrigkeit[1:which(is.na(zweigliedrigkeit$Jahr))[1]-1,]
dat_gesamtschule2<-education_system_build_data(dataset = dataset, years_dif = years_dif)
colnames(dat_gesamtschule2)[3]<-"Zweigliedrigkeit"



#Anteil Gymnasien an allen Schulen die zum Abitur führen
anteil_gym_abitur<-datasets[[7]]
# If a reform was introduced in 1960 (school year 1960/61), which is the first birth cohort affected?
# 1942 is not affected
# 1943 is partially affected #if you are born in the end of 1953, you are affected
# 1944 is affected
# Dataset begins 1949/50
# First Cohort affected is born in 1944

# difference year introduction and cohort partially affected
years_dif=17
dataset<-anteil_gym_abitur[1:which(is.na(anteil_gym_abitur$Jahr))[1]-1,]

dat_anteil_gym_abitur<-education_system_build_data(dataset = dataset, years_dif = years_dif, ignore_transition=T)
dat_anteil_gym_abitur$Value<-gsub("-1", NA, dat_anteil_gym_abitur$Value)
dat_anteil_gym_abitur$Value<-gsub("-2", NA, dat_anteil_gym_abitur$Value)
dat_anteil_gym_abitur$Value<-gsub("1", ">95%", dat_anteil_gym_abitur$Value)
dat_anteil_gym_abitur$Value<-gsub("2", "80-95%", dat_anteil_gym_abitur$Value)
dat_anteil_gym_abitur$Value<-gsub("3", "60-80%", dat_anteil_gym_abitur$Value)
dat_anteil_gym_abitur$Value<-gsub("4", "<60%", dat_anteil_gym_abitur$Value)

colnames(dat_anteil_gym_abitur)[3]<-"Anteil_Gym_Abitur"



#Alternative Wege zum Abitur 
alternative_wege_abi<-datasets[[8]]
# If a reform was introduced in 1960 (school year 1960/61), which is the first birth cohort affected?
# 1942 is not affected
# 1943 is partially affected #if you are born in the end of 1953, you are affected
# 1944 is affected
# Dataset begins 1949/50
# First Cohort affected is born in 1944

# difference year introduction and cohort partially affected
years_dif=17
dataset<-alternative_wege_abi[1:which(is.na(alternative_wege_abi$Jahr))[1]-1,]

dat_alternative_wege_abi<-education_system_build_data(dataset = dataset, years_dif = years_dif)
dat_alternative_wege_abi$Value<-gsub("-1", NA, dat_alternative_wege_abi$Value)
colnames(dat_alternative_wege_abi)[3]<-"Abi_beruflich_Schulen_moeglich"


# Final dataset
classification_helbig<-merge(dat_vorschule, dat_grundschuldauer, by=c("birth_year", "bundesland"), all=T)
classification_helbig<-merge(classification_helbig, dat_schulpflicht, by=c("birth_year", "bundesland"), all=T)
classification_helbig<-merge(classification_helbig, dat_gesamtschule1, by=c("birth_year", "bundesland"), all=T)
classification_helbig<-merge(classification_helbig, dat_gesamtschule2, by=c("birth_year", "bundesland"), all=T)
classification_helbig<-merge(classification_helbig, dat_anteil_gym_abitur, by=c("birth_year", "bundesland"), all=T)
classification_helbig<-merge(classification_helbig, dat_alternative_wege_abi, by=c("birth_year", "bundesland"), all=T)
write_excel_csv2(classification_helbig, "Datasets/classification_helbig_for_analysis.csv")
                                                         