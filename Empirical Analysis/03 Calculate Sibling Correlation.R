#Sibling Correlation and Restricted MaximumLikelihood
#Script produces some warnings:
#warnings NAs introduced by coercion when transforming character variables to numeric, because some values are transformed to NAs, that is intended
#produces warnings for variable scale because age^2 is on a very different scale
#produces warnings when REML is calculated on too small subsamples, sample sizes of all analysis are always reported

libraries = c("readxl", "xlsx", "readr", "rstudioapi", "ggplot2", "dplyr", "stargazer", "ggthemes", "tidyverse", "confintr", "texreg", "lme4", "merDeriv", "msm", "greekLetters", "psych",
              "ggcorrplot"  #For model.matrix to dispay correlations between a several variables in one matrix
              ) #, "spatstat", "Hmisc", "haven","foreign",
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


#Get citations
lapply(libraries, citation)
#set working directory to location of script
setwd(dirname(getSourceEditorContext()$path))

source("05 functions.R")

bundesländer<-c("[1] Baden-Wuerttemberg","[2] Bayern","[3] Berlin","[4] Brandenburg","[5] Bremen","[6] Hamburg","[7] Hessen",
                "[8] Mecklenburg-Vorpommern","[9] Niedersachsen","[10] Nordrhein-Westfalen","[11] Rheinland-Pfalz","[12] Saarland",
                "[13] Sachsen","[14] Sachsen-Anhalt","[15] Schleswig-Holstein","[16] Thueringen")
#######
data_analysis<-read.csv("Datasets/data_analysis_final.csv", sep=";", dec=",")

#Load datasets
sibling_dataset_restrictive<-read.csv("Datasets/sibling_dataset_restrictive.csv", sep=";", dec=",")
classification<-read.csv("Datasets/classification_helbig_for_analysis.csv", sep=";", dec=",")


#use state data from comparison with parents state to impute states of childhoood
data_analysis$bundesland_age_0_20<-as.numeric(data_analysis$bundesland_age_0_20)
data_analysis$bundesland_age_0_20[which(is.na(data_analysis$bundesland_age_0_20) & !is.na(data_analysis$bundesland_childhood))]<-data_analysis$bundesland_childhood[which(is.na(data_analysis$bundesland_age_0_20) & !is.na(data_analysis$bundesland_childhood))]
data_analysis$bundesland_age_0_20[which(is.na(data_analysis$bundesland_age_0_20) & !is.na(data_analysis$bundesland_shared_parents))]<-data_analysis$bundesland_shared_parents[which(is.na(data_analysis$bundesland_age_0_20) & !is.na(data_analysis$bundesland_shared_parents))]




#Extract relevant variables for sibling correlation analysis 
dat_relevant<-data_analysis[,c("pid", "years_of_education", "bundesland_eintritt", "bundesland_age_0_20", "loc1989", "age_last_survey","bundesland_change_0_18","bundesland_childhood_change","migback","age_migration")] 
#replace the state number by the state name
dat_relevant$bundesland_eintritt<-as.character(dat_relevant$bundesland_eintritt)
dat_relevant$bundesland_age_0_20<-as.character(dat_relevant$bundesland_age_0_20)
for(i in 1:16){
  dat_relevant$bundesland_eintritt[dat_relevant$bundesland_eintritt==as.character(i)]<-bundesländer[i]
  dat_relevant$bundesland_age_0_20[dat_relevant$bundesland_age_0_20==as.character(i)]<-bundesländer[i]
}

#Merge individual data with sibling dataset
sibling_dataset_restrictive<-merge(sibling_dataset_restrictive, dat_relevant, by="pid", all.x = T, all.y = F)

#remove all individuals where there was a change in the state during childhood
sibling_dataset_restrictive<-sibling_dataset_restrictive[-which(sibling_dataset_restrictive$bundesland_change_0_18==T|sibling_dataset_restrictive$bundesland_childhood_change==T),]
#remove individuals who migrated to Germany after the age of 6
sibling_dataset_restrictive<-sibling_dataset_restrictive[-which(sibling_dataset_restrictive$migback=="[2] direkter Migrationshintergrund"& (is.na(sibling_dataset_restrictive$age_migration)|sibling_dataset_restrictive$age_migration>6)),]

# Remove persons that left the survey before the age of 21 
if(length(which(sibling_dataset_restrictive$age_last_survey<21))>0) sibling_dataset_restrictive<-sibling_dataset_restrictive[-which(sibling_dataset_restrictive$age_last_survey<21),]

# Remove persons where years of education is missing
if(any(is.na(sibling_dataset_restrictive$years_of_education))) sibling_dataset_restrictive<-sibling_dataset_restrictive[-which(is.na(sibling_dataset_restrictive$years_of_education)),]

#remove all families where only one siblings is left in the dataset
family_one_sib <-  sibling_dataset_restrictive %>% group_by(family_id) %>% filter(n()==1) #
family_ids_out<-unique(family_one_sib$family_id)
sibling_dataset_restrictive<-sibling_dataset_restrictive[-which(sibling_dataset_restrictive$family_id%in%family_ids_out),]                                 
# 2491 individuals left in sibling dataset 



#identify the state where all siblings of a family went to school. 
# If there is not one unique state identified, NA is assigned
family_state<-data.frame(family_id=numeric(), family_state=character())
for(family_id in unique(sibling_dataset_restrictive$family_id)){
  dat<-sibling_dataset_restrictive[sibling_dataset_restrictive$family_id==family_id,]
  # If there is a unique state where people lived in childhood, we assign it
 if(length(unique(dat$bundesland_age_0_20))==1){
   state=unique(dat$bundesland_age_0_20)
 } else {
   #otherwise all siblings lived in the same state when they entered the survey, we assign this state
   if( length(unique(dat$bundesland_eintritt))== 1 & length(na.omit(dat$bundesland_eintritt))>1) {
     state=unique(dat$bundesland_eintritt)
   } else {
     #otherwise we assign a missing value
     state<-NA
   }
 }

  new_row=data.frame(family_id=as.numeric(family_id), family_state=as.character(state))
  family_state<-rbind(family_state,new_row)
}

#Merge sibling data with family state data
sibling_dataset_restrictive<-merge(sibling_dataset_restrictive, family_state, by="family_id", all.x = T, all.y = F)
table(family_state$family_state)


# Build a Variable that indicates whether a person went to school in East Germany (former GDR) or in West Germany
sibling_dataset_restrictive$east_west_Germany<-rep(NA, nrow(sibling_dataset_restrictive))
#First we use the variable that indicates whether a person lived in GDR or FRG in 1989
sibling_dataset_restrictive$east_west_Germany[sibling_dataset_restrictive$loc1989==1]<-"eastgermany"
sibling_dataset_restrictive$east_west_Germany[sibling_dataset_restrictive$loc1989==2]<-"westgermany"
sibling_dataset_restrictive$east_west_Germany[sibling_dataset_restrictive$loc1989==3]<-"abroad"
#If there is an NA (mostly because eople were born after 1989) we use the state they lived in
for (row in 1:nrow(sibling_dataset_restrictive)){
  if(is.na(sibling_dataset_restrictive$east_west_Germany[row])){
    if (sibling_dataset_restrictive$family_state[row] %in% c("[4] Brandenburg","[8] Mecklenburg-Vorpommern","[13] Sachsen","[14] Sachsen-Anhalt","[15] Schleswig-Holstein","[16] Thueringen")) {
      sibling_dataset_restrictive$east_west_Germany[row]="eastgermany"
    } else {
      if (sibling_dataset_restrictive$family_state[row] %in% c("[1] Baden-Wuerttemberg","[2] Bayern","[5] Bremen","[6] Hamburg","[7] Hessen",
                                                               "[9] Niedersachsen","[10] Nordrhein-Westfalen","[11] Rheinland-Pfalz","[12] Saarland")){
        sibling_dataset_restrictive$east_west_Germany[row]="westgermany"
      } else {
        if (sibling_dataset_restrictive$family_state[row] %in% "[3] Berlin") sibling_dataset_restrictive$east_west_Germany[row]="berlin"
      }
    }
  }
}





#Prepare education system classification dataset
#for identifying columns for merging adapt column names
colnames(classification)[colnames(classification)=="bundesland"]<-"family_state"
colnames(classification)[colnames(classification)=="birth_year"]<-"gebjahr"
#build proxy variables for preschool, length of primary schooling (including Förderstufe and Orientirungsstufe), alternatives two attain Abitur, two-tier education system, and categorical variables for share of Abitur gained at Gymnasiums and length of mandatory schooling
classification$preschool_exists<-0
classification$preschool_exists[classification$Vorschule %in% c("verpflichtend", "vorhanden", "Transition verpflichtend to vorhanden","Transition vorhanden to verpflichtend", "Transition vorhanden to Übergang FLEX")]<-1
classification$preschool_exists[classification$Vorschule %in% c("DDR", "Transition DDR to vorhanden","Transition DDR to nicht vorhanden")]<-NA
classification$preschool_exists[is.na(classification$Vorschule) ]<-NA
classification$preschool_mandatory<-0
classification$preschool_mandatory[classification$Vorschule == "verpflichtend"]<-1
classification$preschool_mandatory[classification$Vorschule %in% c("DDR","Transition nicht vorhanden to verpflichtend", "Transition verpflichtend to nicht vorhanden", "Transition verpflichtend to vorhanden","Transition vorhanden to verpflichtend","Transition DDR to vorhanden","Transition DDR to nicht vorhanden") ]<-NA
classification$preschool_mandatory[is.na(classification$Vorschule) ]<-NA
classification$primschool_6years<-NA
classification$primschool_6years[classification$Aufteilung_Sekundarschule %in% c("7","Transition Orientierungsstufe to Förderstufe","Transition Förderstufe to Orientierungsstufe")]<-1
classification$primschool_6years[classification$Aufteilung_Sekundarschule %in% c("5")]<-0
classification$mandatory_schooling_years<-as.numeric(classification$Vollschulzeitpflicht)
classification$mandatory_schooling_years<-factor(classification$mandatory_schooling_years, levels=c("8", "9", "10"), labels=c("8 years", "9 years", "10 years"))
classification$comprehensive_school_exists<-as.numeric(classification$Gesamtschule_Regelschule)
classification$two_tier_secondary<-as.numeric(classification$Zweigliedrigkeit)
classification$share_abitur_gymnasiums<-factor(classification$Anteil_Gym_Abitur, labels=c(">95%","80-95%","60-80%","<60%","DDR"))
classification$alternatives_abitur<-as.numeric(classification$Abi_beruflich_Schulen_moeglich)
classification$mandatory_schooling_10years<-rep(NA, nrow(classification))
classification$mandatory_schooling_10years[classification$mandatory_schooling_years=="10 years"]<-1
classification$mandatory_schooling_10years[classification$mandatory_schooling_years=="9 years"]<-0
classification$mandatory_schooling_10years[classification$mandatory_schooling_years=="8 years"]<-0


#extract relevant classification variables for analysis
classification_rel<-classification[,c("family_state", "gebjahr", "preschool_exists", "preschool_mandatory", "primschool_6years", "mandatory_schooling_10years", "comprehensive_school_exists", "two_tier_secondary","share_abitur_gymnasiums", "alternatives_abitur")]

#########Start re-running here
#build dataset for Germany
sib_data_years<-sibling_dataset_restrictive[,c("family_id","pid","gebjahr","sex","years_of_education","age_last_survey")]
#calculate birth cohort
sib_data_years<-get_cohort(sib_data_years)
table(sib_data_years$cohort)
#Drop cohort 1950-59, 1960-1964 and >=2000 for small sample sizes
sib_data_years<-sib_data_years[-which(sib_data_years$cohort%in%c("1950-1959", "1960-1964", ">2000")),]
#Remove observations with NAs
sib_data_years<-na.omit(sib_data_years)
#remove families with one sibling only
family_one_sib <-  sib_data_years %>% group_by(family_id) %>% filter(n()==1) #
family_ids_out<-unique(family_one_sib$family_id)
sib_data_years<-sib_data_years[-which(sib_data_years$family_id%in%family_ids_out),]                                 
#Family id as factor
sib_data_years$family_id<-as.factor(sib_data_years$family_id)


#Build dataset containing information about the state where the siblings went to school
sib_data_years_states<-sibling_dataset_restrictive[,c("family_id","pid","gebjahr","sex","years_of_education","age_last_survey", "family_state")]
#Calculate cohort for each sibling
sib_data_years_states<-get_cohort(sib_data_years_states)
#remove observations with NAs
sib_data_years_states<-na.omit(sib_data_years_states)
table(sib_data_years_states$cohort)
#Drop cohort 1950-59, 1960-1964 and >=2000 for small sample sizes
sib_data_years_states<-sib_data_years_states[-which(sib_data_years_states$cohort%in%c("1950-1959", "1960-1964", ">2000")),]

#remove families with one sibling only
family_one_sib <-  sib_data_years_states %>% group_by(family_id) %>% filter(n()==1) #
family_ids_out<-unique(family_one_sib$family_id)
sib_data_years_states<-sib_data_years_states[-which(sib_data_years_states$family_id%in%family_ids_out),]                                 

#Family id as factor
sib_data_years_states$family_id<-as.factor(sib_data_years_states$family_id)



#Build dataset for which contains information whether a person went to school in east or west Gerany
sib_data_years_east_west<-sibling_dataset_restrictive[,c("family_id","pid","gebjahr","sex","years_of_education","age_last_survey", "east_west_Germany")]
sib_data_years_east_west<-get_cohort(sib_data_years_east_west)
sib_data_years_east_west<-na.omit(sib_data_years_east_west)
#Drop cohort 1950-59
sib_data_years_east_west<-sib_data_years_east_west[-which(sib_data_years_east_west$cohort%in%c("1950-1959", "1960-1964", ">2000")),]

#remove families with one sibling only
family_one_sib <-  sib_data_years_east_west %>% group_by(family_id) %>% filter(n()==1) #
family_ids_out<-unique(family_one_sib$family_id)
sib_data_years_east_west<-sib_data_years_east_west[-which(sib_data_years_east_west$family_id%in%family_ids_out),]                                 

#Family id as factor
sib_data_years_east_west$family_id<-as.factor(sib_data_years_east_west$family_id)



#Descriptives Numerical Variables
descriptives_Ger_numeric<-describe(sib_data_years[,c("gebjahr", "years_of_education", "age_last_survey")])
descriptives_Ger_numeric<-descriptives_Ger_numeric[,c(2,3,4,5,8,9)]
descriptives_Ger_numeric<-as.data.frame(descriptives_Ger_numeric)
descriptives_numeric<-data.frame(Variable=character(), Population=character(), Cohort=character(), N=numeric(), mean=numeric(), sd=numeric(), median=numeric(), min=numeric(), max=numeric())
descriptives_Ger_numeric$Variable<-gsub("_", " ",rownames(descriptives_Ger_numeric))
descriptives_Ger_numeric$Population<-"Germany"
descriptives_Ger_numeric$Cohort<-"1965-1999"
descriptives_Ger_numeric<-rbind(descriptives_numeric,descriptives_Ger_numeric[, c(7:9,1:6)])
#save table with descriptives
stargazer(descriptives_Ger_numeric, summary=F, type="html", out="Output Paper/Descriptives/Sib_Cor_Descriptives_numerical_Germany.html", 
          rownames = F, digit.separate=0)

#Descriptives Categorical Variables
descr_sex<-as.data.frame(t(table(sib_data_years$sex)), Population="Germany", Cohort="1965-1999")
descr_sex$Var1<-"Sex"
descr_sex$Share<-descr_sex$Freq/sum(descr_sex$Freq)
colnames(descr_sex)[1]<-"Variable"
colnames(descr_sex)[2]<-"Value"
descr_sex$Population<-"Germany"
descr_sex$Cohort<-"1965-1999"
descr_cohort<-as.data.frame(t(table(sib_data_years$cohort)), Population="Germany", Cohort="1965-1999")
descr_cohort$Var1<-"Cohort"
descr_cohort$Share<-descr_cohort$Freq/sum(descr_cohort$Freq)
colnames(descr_cohort)[1]<-"Variable"
colnames(descr_cohort)[2]<-"Value"
descr_cohort$Population<-"Germany"
descr_cohort$Cohort<-"1965-1999"
descriptives_categorical<-rbind(descr_sex,descr_cohort)


#identify cohort of family and number of siblings
cohort_fam<-data.frame(family_id=numeric(), n_sibs=numeric(), mean_geb_jahr_fam=numeric()) #create empty dataframe
for (i in 1:length(levels(sib_data_years$family_id))){
  fam_id<-levels(sib_data_years$family_id)[i]
  dat_fam<-sib_data_years[sib_data_years$family_id==fam_id,]
  new_row<-data.frame(family_id=fam_id, n_sibs=nrow(dat_fam), mean_geb_jahr_fam=round(mean(dat_fam$gebjahr), digits=0))
  cohort_fam[nrow(cohort_fam)+1,]<-new_row
}
cohort_fam$gebjahr<-cohort_fam$mean_geb_jahr_fam
cohort_fam<-get_cohort(cohort_fam)
colnames(cohort_fam)[5:6]<-c("fam_cohort","fam_cohort2")
sib_data_years_states$family_id<-as.character(sib_data_years_states$family_id)
sib_data_years<-merge(sib_data_years,cohort_fam[,c("family_id", "fam_cohort", "fam_cohort2")], by="family_id", all.x=T, all.y=F)
sib_data_years_states<-merge(sib_data_years_states,cohort_fam[,c("family_id", "fam_cohort", "fam_cohort2")], by="family_id", all.x=T, all.y=F)
sib_data_years_east_west<-merge(sib_data_years_east_west,cohort_fam[,c("family_id", "fam_cohort", "fam_cohort2")], by="family_id", all.x=T, all.y=F)

descr_fam_size<-as.data.frame(t(table(cohort_fam$n_sibs)), Population="Germany", Cohort="1965-1999")
descr_fam_size$Var1<-"Number Sibings per Family"
descr_fam_size$Share<-descr_fam_size$Freq/sum(descr_fam_size$Freq)
colnames(descr_fam_size)[1]<-"Variable"
colnames(descr_fam_size)[2]<-"Value"
descr_fam_size$Population<-"Germany"
descr_fam_size$Cohort<-"1965-1999"
descriptives_categorical<-rbind(descriptives_categorical,descr_fam_size)
stargazer(descriptives_categorical, summary=F, type="html", out="Output Paper/Descriptives/Sib_Cor_Descriptives_categorical_Germany.html", 
          rownames = F, digit.separate=0)

#Descriptives cohorts
cohorts_descr<-c("1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")
descriptives_cohorts<-data.frame(Cohort=character(), N=numeric(), "Age mean"=numeric(), "Age standard deviation"=numeric(), "Years of Education mean"=numeric(), "Years of Education standard deviation"=numeric(), check.names = F)
for(i in 1:length(cohorts_descr)){
  coh<-cohorts_descr[i]
  dataset<-sib_data_years[sib_data_years$cohort==coh,c("pid","gebjahr","years_of_education",
                                                       "age_last_survey","cohort")]
  dataset<-na.omit(dataset)
  N<-nrow(dataset)
  age<-mean(dataset$age_last_survey)
  age_sd<-sd(dataset$age_last_survey)
  years_edu<-mean(dataset$years_of_education)
  years_edu_sd<-sd(dataset$years_of_education)
  descriptives_cohorts[i,]<-NA
  descriptives_cohorts$Cohort[i]<-coh
  descriptives_cohorts$N[i]<-N
  descriptives_cohorts$`Age mean`[i]<-age
  descriptives_cohorts$`Age standard deviation`[i]<-age_sd
  descriptives_cohorts$`Years of Education mean`[i]<-years_edu
  descriptives_cohorts$`Years of Education standard deviation`[i]<-years_edu_sd
}
stargazer(descriptives_cohorts, summary=F, type="html", out="Output Paper/Descriptives/Sib_Cor_Descriptives_Cohorts_Germany.html", 
          digits=2, rownames = F, digit.separate=0)


#Descriptives East and West Germany Cohorts
descriptives_east_west<-data.frame(Cohort=character(), Population=character(), N=numeric(), "Age mean"=numeric(), "Age standard deviation"=numeric(), "Years of Education mean"=numeric(), "Years of Education standard deviation"=numeric(), check.names = F)
for(i in 1:length(cohorts_descr)){
  coh<-cohorts_descr[i]
  data<-sib_data_years_east_west[sib_data_years_east_west$cohort==coh,c("pid","gebjahr","years_of_education",
                                                       "age_last_survey","cohort", "east_west_Germany")]
  data<-na.omit(data)
  for(pop in c("westgermany", "eastgermany")){
    row=row+1
    dataset<-data[data$east_west_Germany==pop,]
    N<-nrow(dataset)
    age<-mean(dataset$age_last_survey)
    age_sd<-sd(dataset$age_last_survey)
    years_edu<-mean(dataset$years_of_education)
    years_edu_sd<-sd(dataset$years_of_education)
    descriptives_east_west[row,]<-NA
    descriptives_east_west$Cohort[row]<-coh
    descriptives_east_west$Population[row]<-pop
    descriptives_east_west$N[row]<-N
    descriptives_east_west$`Age mean`[row]<-age
    descriptives_east_west$`Age standard deviation`[row]<-age_sd
    descriptives_east_west$`Years of Education mean`[row]<-years_edu
    descriptives_east_west$`Years of Education standard deviation`[row]<-years_edu_sd
  }
  
}
stargazer(descriptives_east_west, summary=F, type="html", out="Output Paper/Descriptives/Sib_Cor_descriptives_east_west_Germany.html", 
          digits=2, rownames = F, digit.separate=0)



#Descriptives States
descriptives_states<-data.frame(State=character(), N=numeric(), "Age mean"=numeric(), "Age standard deviation"=numeric(), "Birth Year mean"=numeric(), "Birth Year standard deviation"=numeric(), "Years of Education mean"=numeric(), "Years of Education standard deviation"=numeric(), check.names = F)
for(i in 1:length(bundesländer)){
  state<-bundesländer[i]
  dataset<-sib_data_years_states[sib_data_years_states$family_state==state,c("pid","gebjahr","years_of_education",
                                                       "age_last_survey")]
  dataset<-na.omit(dataset)
  N<-nrow(dataset)
  age<-mean(dataset$age_last_survey)
  age_sd<-sd(dataset$age_last_survey)
  birth<-mean(dataset$gebjahr)
  birth_sd<-sd(dataset$gebjahr)
  years_edu<-mean(dataset$years_of_education)
  years_edu_sd<-sd(dataset$years_of_education)
  descriptives_states[i,]<-NA
  descriptives_states$State[i]<-state
  descriptives_states$N[i]<-N
  descriptives_states$`Age mean`[i]<-age
  descriptives_states$`Age standard deviation`[i]<-age_sd
  descriptives_states$`Birth Year mean` [i]<-birth
  descriptives_states$`Birth Year standard deviation`[i]<-birth_sd
  descriptives_states$`Years of Education mean`[i]<-years_edu
  descriptives_states$`Years of Education standard deviation`[i]<-years_edu_sd
}
stargazer(descriptives_states, summary=F, type="html", out="Output Paper/Descriptives/Sib_Cor_descriptives_states_Germany.html", 
          digits=2, rownames = F, digit.separate=0)

#Add education system classification to dataset with states
sib_data_years_states<-merge(sib_data_years_states,classification_rel, by=c("gebjahr","family_state"), all.x=T, all.y=F)

descriptives_edu_system<-data.frame("Education System Variable"=character(),"Education System Characteristic"=character(), N=numeric(), "Age mean"=numeric(), "Age standard deviation"=numeric(), "Birth Year mean"=numeric(), "Birth Year standard deviation"=numeric(), "Years of Education mean"=numeric(), "Years of Education standard deviation"=numeric(), Number_States=character(), check.names = F)
#define education system variables
edu_vars<-c("preschool_exists","preschool_mandatory","primschool_6years","mandatory_schooling_10years",
            "comprehensive_school_exists","two_tier_secondary","share_abitur_gymnasiums","alternatives_abitur")
for (i in 1:length(edu_vars)){
  edu_var<-edu_vars[i]
  data<-sib_data_years_states[,c("gebjahr", "years_of_education", "age_last_survey", "family_state", edu_var)]
  data<-na.omit(data)
  #calculate descriptives for all possible characteristics of the education system (omit NAs and exclude DDR)
  vals<-na.omit(unique(data[,edu_var]))
  vals<-vals[!vals=="DDR"]
  for (val in vals){
    row=row+1
    dataset<-data[data[,edu_var]==val,]
    N<-nrow(dataset)
    age<-mean(dataset$age_last_survey)
    age_sd<-sd(dataset$age_last_survey)
    birth<-mean(dataset$gebjahr)
    birth_sd<-sd(dataset$gebjahr)
    years_edu<-mean(dataset$years_of_education)
    years_edu_sd<-sd(dataset$years_of_education)
    states<-length(unique(dataset$family_state))
    descriptives_edu_system[row,]<-NA
    descriptives_edu_system$`Education System Variable`[row]<-edu_var
    descriptives_edu_system$`Education System Characteristic`[row]<-val
    descriptives_edu_system$N[row]<-N
    descriptives_edu_system$`Age mean`[row]<-age
    descriptives_edu_system$`Age standard deviation`[row]<-age_sd
    descriptives_edu_system$`Birth Year mean` [row]<-birth
    descriptives_edu_system$`Birth Year standard deviation`[row]<-birth_sd
    descriptives_edu_system$`Years of Education mean`[row]<-years_edu
    descriptives_edu_system$`Years of Education standard deviation`[row]<-years_edu_sd
    descriptives_edu_system$Number_States[row]<-states
  }
}
stargazer(descriptives_edu_system, summary=F, type="html", out="Output Paper/Descriptives/Sib_Cor_Descriptives_Education_System_Germany.html", 
          digits=2, rownames = F, digit.separate=0)

#Plot correlation matrix for education system characteristics
plot1<-model.matrix(~0+., data=sib_data_years_states[,12:19]) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=3)
png("Output Paper/Descriptives/Sib Corr Education System Characteristics Correlations.png", width = 720, height = 720, units = "px")
plot1
dev.off()



###############  REML-Estimations of variance Components and  ################################

##Baseline Germany
#regression with age (and its 2rd and 3rd polynomial), sex, and cohort, as well as an interaction term between sex and cohort as control
#produces warnings for variable scale because age^2 is on a very different scale
baseline_Germany_reml<-lmer(years_of_education ~ 1 + age_last_survey + I(age_last_survey^(2))  + I(age_last_survey^(3)) + factor(sex)*factor(cohort) + (1 | family_id), data=sib_data_years)
summary(baseline_Germany_reml)
#extract Variance Components
reml_var_comp<-VarCorr(baseline_Germany_reml)
reml_var_comp
sigma_a2<-as.data.frame(reml_var_comp)$sdcor[1]^2
sigma_b2<-as.data.frame(reml_var_comp)$sdcor[2]^2
#calculate rho
rho_baseline_Germany<-sigma_a2/(sigma_a2+sigma_b2)
rho_baseline_Germany
#calculate standard error of rho using delta method by inserting the formula of rho (based on sigma_a2 and sigma_b2) and the Variance-Covariance Matrix of the Variance Components
vv <- vcov(baseline_Germany_reml, full = T, ranpar = "var")
rho_sd_baseline_Germany <- deltamethod(g = ~ x1/(x1+x2), c(sigma_a2,sigma_b2), vv[(dim(vv)[1]-1):dim(vv)[1],(dim(vv)[1]-1):dim(vv)[1]])

#save results in data frame
results_sib_corr<-data.frame(Population=character(), Cohort=character(), N=numeric(), 'Sibling Correlation (rho)'=numeric(), 'Std. Error'=numeric(), check.names = F)
new_row<-data.frame(Population="Germany", Cohort="1965-1999", N=nrow(sib_data_years) , 'Sibling Correlation (rho)'=rho_baseline_Germany, 'Std. Error'=rho_sd_baseline_Germany, check.names = F)
results_sib_corr<-rbind(results_sib_corr,new_row)

#Sisters and brothers
#Select only females from sibling datasets
sib_data_years_sisters<-sib_data_years[sib_data_years$sex=="[2] Weiblich",]
#remove families with one sibling only
family_one_sib <-  sib_data_years_sisters %>% group_by(family_id) %>% filter(n()==1)
family_ids_out<-unique(family_one_sib$family_id)
sib_data_years_sisters<-sib_data_years_sisters[-which(sib_data_years_sisters$family_id%in%family_ids_out),]                                 

#rerun for sisters
baseline_sisters_reml<-lmer(years_of_education ~ 1 + age_last_survey + I(age_last_survey^(2))  + I(age_last_survey^(3)) + factor(cohort) + (1 | family_id), data=sib_data_years_sisters)
summary(baseline_sisters_reml)
reml_var_comp<-VarCorr(baseline_sisters_reml)
reml_var_comp
sigma_a2<-as.data.frame(reml_var_comp)$sdcor[1]^2
sigma_b2<-as.data.frame(reml_var_comp)$sdcor[2]^2
rho_sisters_Germany<-sigma_a2/(sigma_a2+sigma_b2)
rho_sisters_Germany
vv <- vcov(baseline_sisters_reml, full = T, ranpar = "var")
rho_sd_sisters_Germany <- deltamethod(g = ~ x1/(x1+x2), c(sigma_a2,sigma_b2), vv[(dim(vv)[1]-1):dim(vv)[1],(dim(vv)[1]-1):dim(vv)[1]])

new_row<-data.frame(Population="Germany - Sisters", Cohort="1965-1999", N=nrow(sib_data_years_sisters) , 'Sibling Correlation (rho)'=rho_sisters_Germany, 'Std. Error'=rho_sd_sisters_Germany, check.names = F)
results_sib_corr_gender<-rbind(results_sib_corr,new_row)
stargazer(results_sib_corr_gender, summary=F, type="html", out="Output Paper/Sibling Correlation Brothers Sisters Germany.html", rownames = F)


#Brothers
sib_data_years_brothers<-sib_data_years[sib_data_years$sex=="[1] Männlich",]
#remove families with one sibling only
family_one_sib <-  sib_data_years_brothers %>% group_by(family_id) %>% filter(n()==1) #
family_ids_out<-unique(family_one_sib$family_id)
sib_data_years_brothers<-sib_data_years_brothers[-which(sib_data_years_brothers$family_id%in%family_ids_out),]                                 

baseline_brothers_reml<-lmer(years_of_education ~ 1 + age_last_survey + I(age_last_survey^(2))  + I(age_last_survey^(3)) + factor(cohort) + (1 | family_id), data=sib_data_years_brothers)
summary(baseline_brothers_reml)
reml_var_comp<-VarCorr(baseline_brothers_reml)
reml_var_comp
sigma_a2<-as.data.frame(reml_var_comp)$sdcor[1]^2
sigma_b2<-as.data.frame(reml_var_comp)$sdcor[2]^2
rho_brothers_Germany<-sigma_a2/(sigma_a2+sigma_b2)
rho_brothers_Germany
vv <- vcov(baseline_brothers_reml, full = T, ranpar = "var")
rho_sd_brothers_Germany <- deltamethod(g = ~ x1/(x1+x2), c(sigma_a2,sigma_b2), vv[(dim(vv)[1]-1):dim(vv)[1],(dim(vv)[1]-1):dim(vv)[1]])
new_row<-data.frame(Population="Germany - Brothers", Cohort="1965-1999", N=nrow(sib_data_years_brothers) , 'Sibling Correlation (rho)'=rho_brothers_Germany, 'Std. Error'=rho_sd_brothers_Germany, check.names = F)
results_sib_corr_gender<-rbind(results_sib_corr_gender,new_row)
stargazer(results_sib_corr_gender, summary=F, type="html", out="Output Paper/Sibling Correlation Brothers Sisters Germany.html", rownames = F)




#Calculate the Sibling Correlation for the Cohorts 
cohorts<-c("1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")
table(sib_data_years$fam_cohort)
#drop cohorts 1965-69 for small sample size
cohorts<-cohorts[-1]
#produces warnings for variable scale because age^3 is on a very different scale
for (coh in cohorts){
  baseline_coh_reml<-lmer(years_of_education ~ 1 + age_last_survey + I(age_last_survey^(2))  + I(age_last_survey^(3)) + factor(sex) + (1 | family_id), data=sib_data_years[sib_data_years$fam_cohort==coh,])
  reml_var_comp<-VarCorr(baseline_coh_reml)
  sigma_a2_coh<-as.data.frame(reml_var_comp)$sdcor[1]^2
  sigma_b2_coh<-as.data.frame(reml_var_comp)$sdcor[2]^2
  rho_baseline_coh<-sigma_a2_coh/(sigma_a2_coh+sigma_b2_coh)
  rho_baseline_coh
  vv <- vcov(baseline_coh_reml, full = T, ranpar = "var")
  rho_sd_baseline_coh <- deltamethod(g = ~ x1/(x1+x2), c(sigma_a2_coh,sigma_b2_coh), vv[(dim(vv)[1]-1):dim(vv)[1],(dim(vv)[1]-1):dim(vv)[1]])
  new_row<-data.frame(Population="Germany", Cohort=coh, N=nrow(sib_data_years[sib_data_years$fam_cohort==coh,]) , 'Sibling Correlation (rho)'=rho_baseline_coh, 'Std. Error'=rho_sd_baseline_coh, check.names = F)
  results_sib_corr<-rbind(results_sib_corr,new_row)
}
#save the results as a html table
stargazer(results_sib_corr, summary=F, type="html", out="Output Paper/Sibling Correlation Baseline Germany Cohorts.html", rownames = F)
#calculate confidence intervals (95 and 68%) for plot
data_plot<-results_sib_corr
data_plot$CI_95_l<-data_plot$`Sibling Correlation (rho)` - data_plot$`Std. Error`*qnorm(1-0.05/2)
data_plot$CI_95_h<-data_plot$`Sibling Correlation (rho)` + data_plot$`Std. Error`*qnorm(1-0.05/2)
data_plot$CI_68_l<-data_plot$`Sibling Correlation (rho)` - data_plot$`Std. Error`*qnorm(1-0.32/2)
data_plot$CI_68_h<-data_plot$`Sibling Correlation (rho)` + data_plot$`Std. Error`*qnorm(1-0.32/2)
#Create a line plot with confidence intervals
plot_cohorts_Germany<-ggplot(data_plot[-which(data_plot$Cohort%in% c("1965-1999")),], aes(x = factor(Cohort, levels = c("1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")), y = `Sibling Correlation (rho)`, group=Population))+#, color=Population)) +
  geom_ribbon(aes(ymin = `CI_95_l`, ymax = `CI_95_h`), fill = c("grey70"), alpha =0.2) +
  geom_ribbon(aes(ymin = `CI_68_l`, ymax = `CI_68_h`), fill = c("grey70"), alpha =0.3) +
  geom_line() + geom_point() + 
  labs(title="Sibling Correlation Cohorts", subtitle = "Dep. Var: Years of Education", x = "Cohort", y = "Sibling Correlation (68% & 95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_plot_5y_cohorts_Germany" ,".jpg"), plot=plot_cohorts_Germany, width = 10, height = 6, units = "in")

#create a bar plot with confidence intervals
plot_cohorts_Germany2<-ggplot(data_plot[-which(data_plot$Cohort%in% c("1965-1999")),], aes(x = factor(Cohort, levels = c("1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")), y = `Sibling Correlation (rho)`)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = `CI_95_l`, ymax = `CI_95_h`)) + 
  labs(title="Sibling Correlation Cohorts", subtitle = "Dep. Var: Years of Education", x = "Cohort", y = "Sibling Correlation (95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_plot_5y_cohorts_Germany2" ,".jpg"), plot=plot_cohorts_Germany2, width = 10, height = 6, units = "in")


#Calculate the sibling correlations for Cohorts in East and West Germany
cohorts<-c("1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")
table(sib_data_years_east_west$fam_cohort)
#drop cohorts 1965-69 and 1970-74 for small sample size
cohorts<-cohorts[-1:-2]
results_sib_corr_east_west<-data.frame(Population=character(), Cohort=character(), N=numeric(), 'Sibling Correlation (rho)'=numeric(), 'Std. Error'=numeric(), check.names = F)

#produces warnings for variable scale because age^2 is on a very different scale
#produces warnings boundary (singular fit) for eastgermany for cohort where too few observations are available
for (coh in cohorts){
  for (pop in c("eastgermany", "westgermany")){
    data_regr<-sib_data_years_east_west[sib_data_years_east_west$east_west_Germany==pop & sib_data_years_east_west$fam_cohort==coh,]
    #print(nrow(data_regr))
    family_one_sib <-  data_regr %>% group_by(family_id) %>% filter(n()==1) #
    family_ids_out<-unique(family_one_sib$family_id)
    if(length(which(data_regr$family_id%in%family_ids_out))>0) data_regr<-data_regr[-which(data_regr$family_id%in%family_ids_out),]                                 
    if (nrow(data_regr)>10){
      baseline_coh_reml<-lmer(years_of_education ~ 1 + age_last_survey + I(age_last_survey^(2))  + I(age_last_survey^(3)) + factor(sex) + (1 | family_id), data=data_regr)
      reml_var_comp<-VarCorr(baseline_coh_reml)
      sigma_a2_coh<-as.data.frame(reml_var_comp)$sdcor[1]^2
      sigma_b2_coh<-as.data.frame(reml_var_comp)$sdcor[2]^2
      rho_baseline_coh<-sigma_a2_coh/(sigma_a2_coh+sigma_b2_coh)
      rho_baseline_coh
      vv <- vcov(baseline_coh_reml, full = T, ranpar = "var")
      rho_sd_baseline_coh <- deltamethod(g = ~ x1/(x1+x2), c(sigma_a2_coh,sigma_b2_coh), vv[(dim(vv)[1]-1):dim(vv)[1],(dim(vv)[1]-1):dim(vv)[1]])
      new_row<-data.frame(Population=pop, Cohort=coh, N=nrow(data_regr) , 'Sibling Correlation (rho)'=rho_baseline_coh, 'Std. Error'=rho_sd_baseline_coh, check.names = F)
      
    } else {
      new_row<-data.frame(Population=pop, Cohort=coh, N=nrow(data_regr) , 'Sibling Correlation (rho)'=NA, 'Std. Error'=NA, check.names = F)
    }
    results_sib_corr_east_west<-rbind(results_sib_corr_east_west,new_row)
    
  }
}
#save as html table
stargazer(results_sib_corr_east_west, summary=F, type="html", out="Output Paper/Sibling Correlation Baseline East West Germany Cohorts.html", rownames = F)
#plot the results
data_plot<-results_sib_corr_east_west
data_plot$CI_95_l<-data_plot$`Sibling Correlation (rho)` - data_plot$`Std. Error`*qnorm(1-0.05/2)
data_plot$CI_95_h<-data_plot$`Sibling Correlation (rho)` + data_plot$`Std. Error`*qnorm(1-0.05/2)
data_plot$CI_68_l<-data_plot$`Sibling Correlation (rho)` - data_plot$`Std. Error`*qnorm(1-0.32/2)
data_plot$CI_68_h<-data_plot$`Sibling Correlation (rho)` + data_plot$`Std. Error`*qnorm(1-0.32/2)

#with 95% CI
plot_cohorts_east_west<-ggplot(data_plot, aes(x = factor(Cohort, levels = c("1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")), y = `Sibling Correlation (rho)`, group=Population)) +
  geom_ribbon(aes(ymin = `CI_95_l`, ymax = `CI_95_h`), fill = c("grey70"), alpha =0.2) +
  #geom_ribbon(aes(ymin = `CI_68_l`, ymax = `CI_68_h`), fill = c("grey70"), alpha =0.3) +
  geom_line(aes(color=Population)) + geom_point() + 
  labs(title="Sibling Correlation Cohorts", subtitle = "Dep. Var: Years of Education", x = "Cohort", y = "Sibling Correlation (95% CI)", fill = NULL)

ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_plot_5y_cohorts_east_west" ,".jpg"), plot=plot_cohorts_east_west, width = 10, height = 6, units = "in")

#with 68% CI
plot_cohorts_east_west68<-ggplot(data_plot, aes(x = factor(Cohort, levels = c("1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")), y = `Sibling Correlation (rho)`, group=Population)) +
  #geom_ribbon(aes(ymin = `CI_95_l`, ymax = `CI_95_h`), fill = c("grey70"), alpha =0.2) +
  geom_ribbon(aes(ymin = `CI_68_l`, ymax = `CI_68_h`), fill = c("grey70"), alpha =0.2) +
  geom_line(aes(color=Population)) + geom_point()  + 
  labs(title="Sibling Correlation Cohorts", subtitle = "Dep. Var: Years of Education", x = "Cohort", y = "Sibling Correlation (68% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_plot_5y_cohorts_east_west68" ,".jpg"), plot=plot_cohorts_east_west68, width = 10, height = 6, units = "in")

#with 68% and 95% CI
plot_cohorts_east_west68_95<-ggplot(data_plot, aes(x = factor(Cohort, levels = c("1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")), y = `Sibling Correlation (rho)`, group=Population)) +
  geom_ribbon(aes(ymin = `CI_95_l`, ymax = `CI_95_h`), fill = c("grey70"), alpha =0.2) +
  geom_ribbon(aes(ymin = `CI_68_l`, ymax = `CI_68_h`), fill = c("grey70"), alpha =0.2) +
  geom_line(aes(color=Population)) + geom_point()  + 
  labs(title="Sibling Correlation Cohorts", subtitle = "Dep. Var: Years of Education", x = "Cohort", y = "Sibling Correlation (68% & 95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_plot_5y_cohorts_east_west68_95" ,".jpg"), plot=plot_cohorts_east_west68_95, width = 10, height = 6, units = "in")

#as barplot with 95%
plot_cohorts_east_west2<-ggplot(data_plot, aes(x = factor(Cohort, levels = c("1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")), y = `Sibling Correlation (rho)`, group=Population)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = `CI_95_l`, ymax = `CI_95_h`)) + 
  labs(title="Sibling Correlation Cohorts", subtitle = "Dep. Var: Years of Education", x = "Cohort", y = "Sibling Correlation (95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_plot_5y_cohorts_east_west2" ,".jpg"), plot=plot_cohorts_east_west2, width = 10, height = 6, units = "in")


#Calculate Sibling Correlation States
#produces warning of rank deficiency for state where too few observations are available
#produces warnings for variable scale because age^2 is on a very different scale
bundeslander<-unique(sib_data_years_states$family_state)
for (state in bundeslander){
  baseline_coh_reml<-lmer(years_of_education ~ 1 + age_last_survey + I(age_last_survey^(2))  + I(age_last_survey^(3)) + factor(sex) + factor(cohort) + (1 | family_id), data=sib_data_years_states[sib_data_years_states$family_state==state,])
  reml_var_comp<-VarCorr(baseline_coh_reml)
  sigma_a2_coh<-as.data.frame(reml_var_comp)$sdcor[1]^2
  sigma_b2_coh<-as.data.frame(reml_var_comp)$sdcor[2]^2
  rho_baseline_coh<-sigma_a2_coh/(sigma_a2_coh+sigma_b2_coh)
  rho_baseline_coh
  if(nrow(sib_data_years_states[sib_data_years_states$family_state==state,])>20){
    vv <- vcov(baseline_coh_reml, full = T, ranpar = "var")
    rho_sd_baseline_coh <- deltamethod(g = ~ x1/(x1+x2), c(sigma_a2_coh,sigma_b2_coh), vv[(dim(vv)[1]-1):dim(vv)[1],(dim(vv)[1]-1):dim(vv)[1]])
  } else {
    rho_sd_baseline_coh=NA
  }
  #vv <- vcov(baseline_coh_reml, full = T, ranpar = "var")
  #rho_sd_baseline_coh <- deltamethod(g = ~ x1/(x1+x2), c(sigma_a2_coh,sigma_b2_coh), vv[(dim(vv)[1]-1):dim(vv)[1],(dim(vv)[1]-1):dim(vv)[1]])
  new_row<-data.frame(Population=state, Cohort="1965-1999", N=nrow(sib_data_years_states[sib_data_years_states$family_state==state,]) , 'Sibling Correlation (rho)'=rho_baseline_coh, 'Std. Error'=rho_sd_baseline_coh, check.names = F)
  results_sib_corr<-rbind(results_sib_corr,new_row)
}
#save the results as a html table
stargazer(results_sib_corr, summary=F, type="html", out="Output Paper/Sibling Correlation Baseline Germany Cohorts and States.html", rownames = F)

#Plot the results
data_plot<-results_sib_corr
data_plot<-data_plot[data_plot$Cohort=="1965-1999",]
data_plot$CI_95_l<-data_plot$`Sibling Correlation (rho)` - data_plot$`Std. Error`*qnorm(1-0.05/2)
data_plot$CI_95_h<-data_plot$`Sibling Correlation (rho)` + data_plot$`Std. Error`*qnorm(1-0.05/2)
data_plot$CI_68_l<-data_plot$`Sibling Correlation (rho)` - data_plot$`Std. Error`*qnorm(1-0.32/2)
data_plot$CI_68_h<-data_plot$`Sibling Correlation (rho)` + data_plot$`Std. Error`*qnorm(1-0.32/2)
data_plot$Population[data_plot$Population!="Germany"]<-apply(data_plot[data_plot$Population!="Germany",], 1, function (x) unlist(str_split(as.character(x["Population"]), " "))[[2]])
data_plot$Population<-gsub("-", "-\n", data_plot$Population)


plot_states_Germany<-ggplot(data_plot, aes(x = factor(Population, level = Population[order(`Sibling Correlation (rho)`, decreasing =T)]), y = `Sibling Correlation (rho)`)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = `CI_95_l`, ymax = `CI_95_h`)) + ylim(0,1) +
  labs(title="Sibling Correlation States", subtitle = "Dep. Var: Years of Education", x = "State", y = "Sibling Correlation (95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_states" ,".jpg"), plot=plot_states_Germany, width = 12, height = 7, units = "in")
plot_states_Germany2<-ggplot(data_plot, aes(x = factor(Population, level = Population[order(`Sibling Correlation (rho)`, decreasing =T)]), y = `Sibling Correlation (rho)`)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = `CI_68_l`, ymax = `CI_68_h`)) + ylim(0,1) +
  labs(title="Sibling Correlation States", subtitle = "Dep. Var: Years of Education", x = "State", y = "Sibling Correlation (68% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_states2" ,".jpg"), plot=plot_states_Germany2, width = 12, height = 7, units = "in")

#save table as html
stargazer(data_plot[,1:5], summary=F, type="html", out="Output Paper/Sibling Correlation Baseline Germany States.html", rownames = F)



#Calculate Sibling Correlations for different Education System Characteristics

#Write function to calculate rho and standard deviation of rho
calculate_rho_reml <- function(data, formula=as.formula(years_of_education ~ 1 + age_last_survey + I(age_last_survey^(2))  + I(age_last_survey^(3)) +  factor(cohort) + factor(sex) + (1 | family_id))){
  results_reml<-lmer(formula, data=data)
  reml_var_comp<-VarCorr(results_reml)
  sigma_a2<-as.data.frame(reml_var_comp)$sdcor[1]^2
  sigma_b2<-as.data.frame(reml_var_comp)$sdcor[2]^2
  rho<-sigma_a2/(sigma_a2+sigma_b2)
  vv <- vcov(results_reml, full = T, ranpar = "var")
  rho_sd <- deltamethod(g = ~ x1/(x1+x2), c(sigma_a2,sigma_b2), vv[(dim(vv)[1]-1):dim(vv)[1],(dim(vv)[1]-1):dim(vv)[1]])
  return(list(rho, rho_sd))
}
#Education proxy variables
edu_variables<-c("preschool_exists","preschool_mandatory","primschool_6years","comprehensive_school_exists",
                 "two_tier_secondary","alternatives_abitur","mandatory_schooling_10years")
#categorical variable
other_vars<-"share_abitur_gymnasiums"

sib_cor_edu_system<-data.frame('Education System Variable'=character(), Cohort=character(), N= numeric(), 'Sibling Correlation (rho)'=character(), 'Years of Education (Std. Dev.)'=character() , 'Birth Year (Std. Dev.)'=character(), check.names = F)

#produces warnings for variable scale because age^2 is on a very different scale
for (variable in edu_variables){
  #extract datasets for both levels of the proxy variable
  data_characteristic1<-sib_data_years_states[which(sib_data_years_states[,variable]==T),]
  family_one_sib <-  data_characteristic1 %>% group_by(family_id) %>% filter(n()==1) #
  family_ids_out<-unique(family_one_sib$family_id)
  data_characteristic1<-data_characteristic1[-which(data_characteristic1$family_id%in%family_ids_out),]                                 
  data_characteristic2<-sib_data_years_states[which(sib_data_years_states[,variable]==F),]
  family_one_sib <-  data_characteristic2 %>% group_by(family_id) %>% filter(n()==1) #
  family_ids_out<-unique(family_one_sib$family_id)
  data_characteristic2<-data_characteristic2[-which(data_characteristic2$family_id%in%family_ids_out),]                                 
  
  #calculate also some descriptives for the subsamples of the populations
  out<-data.frame('Education System Variable'=c(paste0(variable, "TRUE"), paste0(variable, "FALSE")), Cohort=c("1975-1999", "1975-1999"), N= c(NA,NA), 'Sibling Correlation (rho)'=c(NA,NA), 'Years of Education (Std. Dev.)'=c(NA,NA) , 'Birth Year (Std. Dev.)'=c(NA,NA), check.names = F)
  out$`Years of Education (Std. Dev.)`[1]<-paste0(round(mean(data_characteristic1$years_of_education), digits=3), " (", round(sd(data_characteristic1$years_of_education), digits=3), ")")
  out$`Years of Education (Std. Dev.)`[2]<-paste0(round(mean(data_characteristic2$years_of_education), digits=3), " (", round(sd(data_characteristic2$years_of_education), digits=3), ")")
  out$`Birth Year (Std. Dev.)`[1]<-paste0(round(mean(data_characteristic1$gebjahr), digits=3), " (", round(sd(data_characteristic1$gebjahr), digits=3), ")")
  out$`Birth Year (Std. Dev.)`[2]<-paste0(round(mean(data_characteristic2$gebjahr), digits=3), " (", round(sd(data_characteristic2$gebjahr), digits=3), ")")
  out$N<-c(nrow(data_characteristic1),nrow(data_characteristic2))
  
  #calculate rho and standard error
  sib_cor_1<-calculate_rho_reml(data=data_characteristic1)
  sib_cor_2<-calculate_rho_reml(data=data_characteristic2)
  
  out$`Sibling Correlation (rho)`[1]<-paste0(round(sib_cor_1[[1]], digits=3)," (", round(sib_cor_1[[2]], digits=3), ")")
  out$`Sibling Correlation (rho)`[2]<-paste0(round(sib_cor_2[[1]], digits=3)," (", round(sib_cor_2[[2]], digits=3), ")")
  sib_cor_edu_system<-rbind(sib_cor_edu_system,out)
}

#calculate sibling correlations for the categorical variable levels (share abitur at Gymnasium)
#build four datasets for the four levels and remove all families where only on sibling is left in the dataset
data_characteristic1<-sib_data_years_states[which(sib_data_years_states$share_abitur_gymnasiums==">95%"),]
family_one_sib <-  data_characteristic1 %>% group_by(family_id) %>% filter(n()==1) #
family_ids_out<-unique(family_one_sib$family_id)
data_characteristic1<-data_characteristic1[-which(data_characteristic1$family_id%in%family_ids_out),]                                 
data_characteristic2<-sib_data_years_states[which(sib_data_years_states$share_abitur_gymnasiums=="80-95%"),]
family_one_sib <-  data_characteristic2 %>% group_by(family_id) %>% filter(n()==1) #
family_ids_out<-unique(family_one_sib$family_id)
data_characteristic2<-data_characteristic2[-which(data_characteristic2$family_id%in%family_ids_out),]                                 
data_characteristic3<-sib_data_years_states[which(sib_data_years_states$share_abitur_gymnasiums=="60-80%"),]
family_one_sib <-  data_characteristic3 %>% group_by(family_id) %>% filter(n()==1) #
family_ids_out<-unique(family_one_sib$family_id)
data_characteristic3<-data_characteristic3[-which(data_characteristic3$family_id%in%family_ids_out),]                                 
data_characteristic4<-sib_data_years_states[which(sib_data_years_states$share_abitur_gymnasiums=="<60%"),]
family_one_sib <-  data_characteristic4 %>% group_by(family_id) %>% filter(n()==1) #
family_ids_out<-unique(family_one_sib$family_id)
data_characteristic4<-data_characteristic4[-which(data_characteristic4$family_id%in%family_ids_out),]                                 

#Calculate descriptives for the subsets
out<-data.frame('Education System Variable'=c(paste0("Share Abitur Gymnasium", levels(sib_data_years_states$share_abitur_gymnasiums)[1:4], sep=" ")), Cohort=c("1975-1999","1975-1999","1975-1999", "1975-1999"), N= c(NA,NA,NA,NA), 'Sibling Correlation (rho)'=c(NA,NA,NA,NA), 'Years of Education (Std. Dev.)'=c(NA,NA,NA,NA) , 'Birth Year (Std. Dev.)'=c(NA,NA,NA,NA), check.names = F)
out$`Years of Education (Std. Dev.)`[1]<-paste0(round(mean(data_characteristic1$years_of_education), digits=3), " (", round(sd(data_characteristic1$years_of_education), digits=3), ")")
out$`Years of Education (Std. Dev.)`[2]<-paste0(round(mean(data_characteristic2$years_of_education), digits=3), " (", round(sd(data_characteristic2$years_of_education), digits=3), ")")
out$`Years of Education (Std. Dev.)`[3]<-paste0(round(mean(data_characteristic3$years_of_education), digits=3), " (", round(sd(data_characteristic3$years_of_education), digits=3), ")")
out$`Years of Education (Std. Dev.)`[4]<-paste0(round(mean(data_characteristic4$years_of_education), digits=3), " (", round(sd(data_characteristic4$years_of_education), digits=3), ")")
out$`Birth Year (Std. Dev.)`[1]<-paste0(round(mean(data_characteristic1$gebjahr), digits=3), " (", round(sd(data_characteristic1$gebjahr), digits=3), ")")
out$`Birth Year (Std. Dev.)`[2]<-paste0(round(mean(data_characteristic2$gebjahr), digits=3), " (", round(sd(data_characteristic2$gebjahr), digits=3), ")")
out$`Birth Year (Std. Dev.)`[3]<-paste0(round(mean(data_characteristic3$gebjahr), digits=3), " (", round(sd(data_characteristic3$gebjahr), digits=3), ")")
out$`Birth Year (Std. Dev.)`[4]<-paste0(round(mean(data_characteristic4$gebjahr), digits=3), " (", round(sd(data_characteristic4$gebjahr), digits=3), ")")
out$N<-c(nrow(data_characteristic1),nrow(data_characteristic2),nrow(data_characteristic3),nrow(data_characteristic4))

#calculate sibling correlations and standard errors with the function defined above
#produces warnings for variable scale because age^2 is on a very different scale
sib_cor_1<-calculate_rho_reml(data=data_characteristic1)
sib_cor_2<-calculate_rho_reml(data=data_characteristic2)
sib_cor_3<-calculate_rho_reml(data=data_characteristic3)
sib_cor_4<-calculate_rho_reml(data=data_characteristic4)
# add them to the results of the descriptives
out$`Sibling Correlation (rho)`[1]<-paste0(round(sib_cor_1[[1]], digits=3)," (", round(sib_cor_1[[2]], digits=3), ")")
out$`Sibling Correlation (rho)`[2]<-paste0(round(sib_cor_2[[1]], digits=3)," (", round(sib_cor_2[[2]], digits=3), ")")
out$`Sibling Correlation (rho)`[3]<-paste0(round(sib_cor_3[[1]], digits=3)," (", round(sib_cor_3[[2]], digits=3), ")")
out$`Sibling Correlation (rho)`[4]<-paste0(round(sib_cor_4[[1]], digits=3)," (", round(sib_cor_4[[2]], digits=3), ")")
#add the results to the resutls for the other education variables
sib_cor_edu_system<-rbind(sib_cor_edu_system,out)
#Change name of primary school variable
sib_cor_edu_system$`Education System Variable`[sib_cor_edu_system$`Education System Variable`=="primschool_6yearsTRUE"] <-"secondary_school_after_6years"
sib_cor_edu_system$`Education System Variable`[sib_cor_edu_system$`Education System Variable`=="primschool_6yearsFALSE"] <-"secondary_school_after_4years"
#save results as html
stargazer(sib_cor_edu_system, summary=F, type="html", out="Output Paper/Sibling Correlation Education System.html", rownames = F)

#calculate CIs for plots
data_plot<-sib_cor_edu_system[,c(1,2,4)]
data_plot$"Std. Error"<-apply(data_plot, 1, function(x) as.numeric(gsub('[()]','', unlist(str_split(x["Sibling Correlation (rho)"], " "))[2])) )
data_plot$rho<-apply(data_plot, 1, function(x) as.numeric(unlist(str_split(x["Sibling Correlation (rho)"], " "))[1]) )
data_plot$`Sibling Correlation (rho)`<-data_plot$rho
data_plot$CI_95_l<-data_plot$`Sibling Correlation (rho)` - data_plot$`Std. Error`*qnorm(1-0.05/2)
data_plot$CI_95_h<-data_plot$`Sibling Correlation (rho)` + data_plot$`Std. Error`*qnorm(1-0.05/2)
data_plot$CI_68_l<-data_plot$`Sibling Correlation (rho)` - data_plot$`Std. Error`*qnorm(1-0.32/2)
data_plot$CI_68_h<-data_plot$`Sibling Correlation (rho)` + data_plot$`Std. Error`*qnorm(1-0.32/2)

#Plot results
#split variables for two plots
vars1<-c("preschool_existsTRUE","preschool_existsFALSE","preschool_mandatoryTRUE","preschool_mandatoryFALSE",
         "secondary_school_after_6years","secondary_school_after_4years", "alternatives_abiturTRUE","alternatives_abiturFALSE",
         "mandatory_schooling_10yearsTRUE","mandatory_schooling_10yearsFALSE")
vars2<-c("Share Abitur Gymnasium>95% ","Share Abitur Gymnasium80-95% ","Share Abitur Gymnasium60-80% ","Share Abitur Gymnasium<60% ", 
         "comprehensive_school_existsTRUE","comprehensive_school_existsFALSE","two_tier_secondaryTRUE","two_tier_secondaryFALSE")
#define labels for Plot
vars1_label<-c("preschool_exists_TRUE","preschool_exists_FALSE","preschool_mandatory_TRUE","preschool_mandatory_FALSE",
         "secondary_school_after_6years","secondary_school_after_4years", "alternatives_abitur_TRUE","alternatives_abitur_FALSE",
         "mandatory_schooling_10years","mandatory_schooling_9years")
vars2_label<-c("Share Abitur Gymnasium_>95%","Share Abitur Gymnasium_80-95%","Share Abitur Gymnasium_60-80%","Share Abitur Gymnasium_<60%", 
         "comprehensive_school_exists_TRUE","comprehensive_school_exists_FALSE","two_tier_secondary_TRUE","two_tier_secondary_FALSE")
#save plots
plot_edu_vars<-ggplot(data_plot[data_plot$`Education System Variable`%in% vars1,], 
                      aes(x = factor(`Education System Variable`, levels= vars1,labels=gsub('[_ ]', "\n",vars1_label)), y = `Sibling Correlation (rho)`)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = `CI_95_l`, ymax = `CI_95_h`)) + 
  labs(title="Sibling Correlation", subtitle = "Dep. Var: Years of Education", x = "Education System Characteristic", y = "Sibling Correlation (95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_education_system" ,".jpg"), plot=plot_edu_vars, width = 10, height = 6, units = "in")
plot_edu_vars2<-ggplot(data_plot[data_plot$`Education System Variable`%in% vars2,], 
                       aes(x = factor(`Education System Variable`, levels = vars2, labels=gsub('[_ ]', "\n",vars2_label)), y = `Sibling Correlation (rho)`)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = `CI_95_l`, ymax = `CI_95_h`)) + 
  labs(title="Sibling Correlation", subtitle = "Dep. Var: Years of Education", x = "Education System Characteristic", y = "Sibling Correlation (95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_education_system2" ,".jpg"), plot=plot_edu_vars2, width = 10, height = 6, units = "in")




#######Robustness check
#restricted sample with individuals older than 25

#Build restricted dataset containing individuals oder than 25
sib_data_years26<-sib_data_years[sib_data_years$age_last_survey>25,]
#remove families with one sibling only
family_one_sib <-  sib_data_years26 %>% group_by(family_id) %>% filter(n()==1) #
family_ids_out<-unique(family_one_sib$family_id)
sib_data_years26<-sib_data_years26[-which(sib_data_years26$family_id%in%family_ids_out),]                                 
#Family id as factor
sib_data_years26$family_id<-as.factor(sib_data_years26$family_id)

#Descriptives cohorts dataset older 25
#Descriptives cohorts
cohorts_descr2<-c("1970-1974","1975-1979","1980-1984","1985-1989","1990-1994")
descriptives_cohorts26<-data.frame(Cohort=character(), N=numeric(), "Age mean"=numeric(), "Age standard deviation"=numeric(), "Years of Education mean"=numeric(), "Years of Education standard deviation"=numeric(), check.names = F)
for(i in 1:length(cohorts_descr2)){
  coh<-cohorts_descr2[i]
  dataset<-sib_data_years26[sib_data_years26$cohort==coh,c("pid","gebjahr","years_of_education",
                                                           "age_last_survey","cohort")]
  dataset<-na.omit(dataset)
  N<-nrow(dataset)
  age<-mean(dataset$age_last_survey)
  age_sd<-sd(dataset$age_last_survey)
  years_edu<-mean(dataset$years_of_education)
  years_edu_sd<-sd(dataset$years_of_education)
  descriptives_cohorts26[i,]<-NA
  descriptives_cohorts26$Cohort[i]<-coh
  descriptives_cohorts26$N[i]<-N
  descriptives_cohorts26$`Age mean`[i]<-age
  descriptives_cohorts26$`Age standard deviation`[i]<-age_sd
  descriptives_cohorts26$`Years of Education mean`[i]<-years_edu
  descriptives_cohorts26$`Years of Education standard deviation`[i]<-years_edu_sd
}
stargazer(descriptives_cohorts26, summary=F, type="html", out="Output Paper/Descriptives/Sib_Cor_Descriptives_Cohorts_older_26_Germany.html", 
          digits=2, rownames = F, digit.separate=0)


#Calculate the Sibling Correlation for the Cohorts 
cohorts<-c("1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994")
table(sib_data_years26$fam_cohort)
#drop cohorts 1965-69 for small sample size
cohorts<-cohorts[-1]
#Empty Dataframe for results
results_sib_corr26<-data.frame(Population=character(), Cohort=character(), N=numeric(), 'Sibling Correlation (rho)'=numeric(), 'Std. Error'=numeric(), check.names = F)
#produces warnings for variable scale because age^3 is on a very different scale
for (coh in cohorts){
  baseline_coh_reml<-lmer(years_of_education ~ 1 + age_last_survey + I(age_last_survey^(2))  + I(age_last_survey^(3)) + factor(sex) + (1 | family_id), data=sib_data_years26[sib_data_years26$fam_cohort==coh,])
  reml_var_comp<-VarCorr(baseline_coh_reml)
  sigma_a2_coh<-as.data.frame(reml_var_comp)$sdcor[1]^2
  sigma_b2_coh<-as.data.frame(reml_var_comp)$sdcor[2]^2
  rho_baseline_coh<-sigma_a2_coh/(sigma_a2_coh+sigma_b2_coh)
  rho_baseline_coh
  vv <- vcov(baseline_coh_reml, full = T, ranpar = "var")
  rho_sd_baseline_coh <- deltamethod(g = ~ x1/(x1+x2), c(sigma_a2_coh,sigma_b2_coh), vv[(dim(vv)[1]-1):dim(vv)[1],(dim(vv)[1]-1):dim(vv)[1]])
  new_row<-data.frame(Population="Germany", Cohort=coh, N=nrow(sib_data_years26[sib_data_years26$fam_cohort==coh,]) , 'Sibling Correlation (rho)'=rho_baseline_coh, 'Std. Error'=rho_sd_baseline_coh, check.names = F)
  results_sib_corr26<-rbind(results_sib_corr26,new_row)
}
#save the results as a html table
stargazer(results_sib_corr26, summary=F, type="html", out="Output Paper/Sibling Correlation Robustness Check Age26 Cohorts.html", rownames = F)
#calculate confidence intervals (95 and 68%) for plot
data_plot<-results_sib_corr26
data_plot$CI_95_l<-data_plot$`Sibling Correlation (rho)` - data_plot$`Std. Error`*qnorm(1-0.05/2)
data_plot$CI_95_h<-data_plot$`Sibling Correlation (rho)` + data_plot$`Std. Error`*qnorm(1-0.05/2)
data_plot$CI_68_l<-data_plot$`Sibling Correlation (rho)` - data_plot$`Std. Error`*qnorm(1-0.32/2)
data_plot$CI_68_h<-data_plot$`Sibling Correlation (rho)` + data_plot$`Std. Error`*qnorm(1-0.32/2)
#Create a line plot with confidence intervals
plot_cohorts_Germany26<-ggplot(data_plot, aes(x = factor(Cohort, levels = c("1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")), y = `Sibling Correlation (rho)`, group=Population))+#, color=Population)) +
  geom_ribbon(aes(ymin = `CI_95_l`, ymax = `CI_95_h`), fill = c("grey70"), alpha =0.2) +
  geom_ribbon(aes(ymin = `CI_68_l`, ymax = `CI_68_h`), fill = c("grey70"), alpha =0.3) +
  geom_line() + geom_point() + 
  labs(title="Sibling Correlation Cohorts", subtitle = "Dep. Var: Years of Education", x = "Cohort", y = "Sibling Correlation (68% & 95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_plot_5y_ Robustness Check Age26 Cohorts" ,".jpg"), plot=plot_cohorts_Germany26, width = 10, height = 6, units = "in")

#create a bar plot with confidence intervals
plot_cohorts_Germany26_2<-ggplot(data_plot, aes(x = factor(Cohort, levels = c("1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")), y = `Sibling Correlation (rho)`)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = `CI_95_l`, ymax = `CI_95_h`)) + 
  labs(title="Sibling Correlation Cohorts", subtitle = "Dep. Var: Years of Education", x = "Cohort", y = "Sibling Correlation (95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "Sib_Cor_plot_5y_ Robustness Check Age26 Cohorts2" ,".jpg"), plot=plot_cohorts_Germany26_2, width = 10, height = 6, units = "in")

