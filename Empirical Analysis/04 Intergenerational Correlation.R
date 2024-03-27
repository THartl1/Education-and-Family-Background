#define load and install new packages
libraries = c("readxl", "xlsx", "readr", "rstudioapi", "ggplot2", "dplyr", "stargazer", "ggthemes", "confintr", "tidyverse", "texreg", "psych",
              "car", "lmtest" #for heteroskedasticity-robust standard errors
              )
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)




#Get citations
lapply(libraries, citation)
#set working directory to location of script
setwd(dirname(getSourceEditorContext()$path))

#define states
bundesländer<-c("[1] Baden-Wuerttemberg","[2] Bayern","[3] Berlin","[4] Brandenburg","[5] Bremen","[6] Hamburg","[7] Hessen",
                "[8] Mecklenburg-Vorpommern","[9] Niedersachsen","[10] Nordrhein-Westfalen","[11] Rheinland-Pfalz","[12] Saarland",
                "[13] Sachsen","[14] Sachsen-Anhalt","[15] Schleswig-Holstein","[16] Thueringen")

#load functions from function script
source("05 functions.R")

#load datasets
data_analysis<-read.csv("Datasets/data_analysis_final.csv", sep=";", dec=",")
classification<-read.csv("Datasets/classification_helbig_for_analysis.csv", sep=";", dec=",")

#Prepare education system classification dataset
colnames(classification)[colnames(classification)=="bundesland"]<-"family_state"
colnames(classification)[colnames(classification)=="birth_year"]<-"gebjahr"
#build proxy variables
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

classification_rel<-classification[,c("family_state", "gebjahr", "preschool_exists", "preschool_mandatory", "primschool_6years", "mandatory_schooling_years", "comprehensive_school_exists", "two_tier_secondary","share_abitur_gymnasiums", "alternatives_abitur")]



#Prepare data

#use state data from comparison with parents state to impute states of childhoood
data_analysis$family_state<-data_analysis$bundesland_age_0_20
data_analysis$family_state[which(is.na(data_analysis$family_state) & !is.na(data_analysis$bundesland_childhood))]<-data_analysis$bundesland_childhood[which(is.na(data_analysis$family_state) & !is.na(data_analysis$bundesland_childhood))]
data_analysis$family_state[which(is.na(data_analysis$family_state) & !is.na(data_analysis$bundesland_shared_parents))]<-data_analysis$bundesland_shared_parents[which(is.na(data_analysis$family_state) & !is.na(data_analysis$bundesland_shared_parents))]

#remove all individuals where there was a change in the state during childhood
data_analysis<-data_analysis[-which(data_analysis$bundesland_change_0_18==T|data_analysis$bundesland_childhood_change==T),]
#remove individuals who migrated to Germany after the age of 6
data_analysis<-data_analysis[-which(data_analysis$migback=="[2] direkter Migrationshintergrund" & (is.na(data_analysis$age_migration)|data_analysis$age_migration>6)),]


#Build a sample where the accuracy for the state variable is more loose. When a person entered the survey before the age of 30 and we use the state of residence at entry, when no more accurate information is available
data_analysis$bundesland_age_0_30<-data_analysis$family_state
data_analysis$bundesland_age_0_30[which(is.na(data_analysis$bundesland_age_0_30) & data_analysis$age_first_survey<30)]<-data_analysis$bundesland_eintritt[which(is.na(data_analysis$bundesland_age_0_30) & data_analysis$age_first_survey<30)]


#check the sample size for both state variables
sum(!is.na(data_analysis$family_state))
sum(!is.na(data_analysis$bundesland_age_0_30))

#replace the state number by the state name
data_analysis$family_state<-as.character(data_analysis$family_state)
data_analysis$bundesland_age_0_20<-as.character(data_analysis$bundesland_age_0_20)
data_analysis$bundesland_age_0_30<-as.character(data_analysis$bundesland_age_0_30)
for(i in 1:16){
  data_analysis$family_state[data_analysis$family_state==as.character(i)]<-bundesländer[i]
  data_analysis$bundesland_age_0_20[data_analysis$bundesland_age_0_20==as.character(i)]<-bundesländer[i]
  data_analysis$bundesland_age_0_30[data_analysis$bundesland_age_0_30==as.character(i)]<-bundesländer[i]
  
}

#Check number of persons where we have data for the parents, fathers and mothers education
data_analysis$parents_years_edu<-rowMeans(data_analysis[,c("m_years_of_education", "f_years_of_education")], na.rm = FALSE)
sum(!is.na(data_analysis$m_years_of_education))
sum(!is.na(data_analysis$f_years_of_education))
sum(!is.na(data_analysis$parents_years_edu))

#Build a proxy variable called female for gender
data_analysis$female<-0
data_analysis$female[data_analysis$sex=="[2] weiblich"]<-1
data_analysis$female[which(!data_analysis$sex%in%c("[1] maennlich","[2] weiblich"))]<-NA
#calculate cohort
data_analysis<- get_cohort(data_analysis)

#Check whether an individual went to school in east or west germany
# Check if a person went to school in ddr
data_analysis$east_west_Germany<-rep(NA, nrow(data_analysis))
data_analysis$east_west_Germany[data_analysis$loc1989==1]<-"eastgermany"
data_analysis$east_west_Germany[data_analysis$loc1989==2]<-"westgermany"
data_analysis$east_west_Germany[data_analysis$loc1989==3]<-"abroad"
for (row in 1:nrow(data_analysis)){
  if(is.na(data_analysis$east_west_Germany[row])){
    if (data_analysis$family_state[row] %in% c("[4] Brandenburg","[8] Mecklenburg-Vorpommern","[13] Sachsen","[14] Sachsen-Anhalt","[15] Schleswig-Holstein","[16] Thueringen")) {
      data_analysis$east_west_Germany[row]="eastgermany"
    } else {
      if (data_analysis$family_state[row] %in% c("[1] Baden-Wuerttemberg","[2] Bayern","[5] Bremen","[6] Hamburg","[7] Hessen",
                                                               "[9] Niedersachsen","[10] Nordrhein-Westfalen","[11] Rheinland-Pfalz","[12] Saarland")){
        data_analysis$east_west_Germany[row]="westgermany"
      } else {
        if (data_analysis$family_state[row] %in% "[3] Berlin") data_analysis$east_west_Germany[row]="berlin"
      }
    }
  }
}



##########################        Build Datasets         ############################

# Baseline Dataset Germany
# Use individuals who left the panel after the age of 20 to be sure that they obtained their school degree, and after the age of 30 to ensure that years of education doesn't change much anymore and which were born after 1949
dat_baseline_germany<-data_analysis[data_analysis$age_last_survey>20 & data_analysis$gebjahr>1949,]
#keep only observations wheere data for education and parent education is available
dat_baseline_germany<-dat_baseline_germany[which((!is.na(dat_baseline_germany$f_years_of_education) | !is.na(dat_baseline_germany$m_years_of_education)) & !is.na(dat_baseline_germany$years_of_education)),]
dat_baseline_germany<-dat_baseline_germany[,c("pid", "female", "years_of_education", "parents_years_edu","f_years_of_education", "m_years_of_education", "gebjahr", "cohort", "cohort2", "age_last_survey")]
#split the dataset in one for males and one for females (sons and daughter)
dat_baseline_germany_sons<-dat_baseline_germany[dat_baseline_germany$female==0,]
dat_baseline_germany_daughters<-dat_baseline_germany[dat_baseline_germany$female==1,]

#Second dataset where age at last survey is at least 26 and which were born after 1949
dat_baseline_germany25<-data_analysis[data_analysis$age_last_survey>25 & data_analysis$gebjahr>1949,]
dat_baseline_germany25<-dat_baseline_germany25[which((!is.na(dat_baseline_germany25$f_years_of_education) | !is.na(dat_baseline_germany25$m_years_of_education)) & !is.na(dat_baseline_germany25$years_of_education)),]
dat_baseline_germany25<-dat_baseline_germany25[,c("pid", "female", "years_of_education", "parents_years_edu","f_years_of_education", "m_years_of_education", "gebjahr", "cohort", "cohort2", "age_last_survey")]


# Build dataset with individuals where information about the state they lived in as a child is available and which were born after 1949
dat_baseline_states<-data_analysis[data_analysis$age_last_survey>20 & data_analysis$gebjahr>1949 & !is.na(data_analysis$family_state),]
dat_baseline_states<-dat_baseline_states[which((!is.na(dat_baseline_states$f_years_of_education) | !is.na(dat_baseline_states$m_years_of_education)) & !is.na(dat_baseline_states$years_of_education)),]
dat_baseline_states<-dat_baseline_states[,c("pid", "female", "family_state", "years_of_education", "parents_years_edu","f_years_of_education", "m_years_of_education", "gebjahr", "cohort", "cohort2", "age_last_survey")]
# dataset with less accurate state variable 
dat_baseline_states2<-data_analysis[data_analysis$age_last_survey>20 & data_analysis$gebjahr>1949 & !is.na(data_analysis$bundesland_age_0_30),]
dat_baseline_states2<-dat_baseline_states2[which((!is.na(dat_baseline_states2$f_years_of_education) | !is.na(dat_baseline_states2$m_years_of_education)) & !is.na(dat_baseline_states2$years_of_education)),]
dat_baseline_states2<-dat_baseline_states2[,c("pid", "female", "bundesland_age_0_30", "years_of_education", "parents_years_edu","f_years_of_education", "m_years_of_education", "gebjahr", "cohort", "cohort2", "age_last_survey")]

#Build dataset with individuals where we know if they grew up in east or west Germany and which were born after 1949
dat_baseline_east_west<-data_analysis[data_analysis$age_last_survey>20 & data_analysis$gebjahr>1949 & !is.na(data_analysis$east_west_Germany),]
dat_baseline_east_west<-dat_baseline_east_west[which((!is.na(dat_baseline_east_west$f_years_of_education) | !is.na(dat_baseline_east_west$m_years_of_education)) & !is.na(dat_baseline_east_west$years_of_education)),]
dat_baseline_east_west<-dat_baseline_east_west[,c("pid", "female", "east_west_Germany", "years_of_education", "parents_years_edu","f_years_of_education", "m_years_of_education", "gebjahr", "cohort", "cohort2", "age_last_survey")]






##################  Descriptives  ##############


#Descriptives Numerical Variables Baseline Dataset
descriptives_Ger_numeric<-describe(dat_baseline_germany[,c("gebjahr", "years_of_education", "parents_years_edu","f_years_of_education", "m_years_of_education","age_last_survey")])
descriptives_Ger_numeric<-descriptives_Ger_numeric[,c(2,3,4,5,8,9)]
descriptives_Ger_numeric$Variable<-c("Birth Year", "Years of Education", "Parents Mean Year of Education", "Father Years of Education", "Mother Years of Education", "Age at last Survey")
descriptives_Ger_numeric_sons<-describe(dat_baseline_germany_sons[,c("years_of_education", "gebjahr","age_last_survey")])
descriptives_Ger_numeric_sons<-descriptives_Ger_numeric_sons[,c(2,3,4,5,8,9)]
descriptives_Ger_numeric_sons$Variable<-c("Years of Education", "Birth Year", "Age")
descriptives_Ger_numeric_daughters<-describe(dat_baseline_germany_daughters[,c("years_of_education", "gebjahr", "age_last_survey")])
descriptives_Ger_numeric_daughters<-descriptives_Ger_numeric_daughters[,c(2,3,4,5,8,9)]
descriptives_Ger_numeric_daughters$Variable<-c("Years of Education", "Birth Year", "Age")
descriptives_Ger_numeric<-rbind(as.data.frame(descriptives_Ger_numeric), as.data.frame(descriptives_Ger_numeric_sons))
descriptives_Ger_numeric<-rbind(descriptives_Ger_numeric, as.data.frame(descriptives_Ger_numeric_daughters))
descriptives_Ger_numeric$Population<-c("Germany","Germany","Germany","Germany","Germany","Germany",
                                       "Germany - Male","Germany - Male","Germany - Male",
                                       "Germany - Female","Germany - Female","Germany - Female")
descriptives_Ger_numeric$Cohort<-"1950-1999"
descriptives_Ger_numeric<-descriptives_Ger_numeric[, c(7:9,1:6)]
colnames(descriptives_Ger_numeric)[4:9]<-c("N", "Mean", "Std. Dev.", "Median", "Min", "Max")

stargazer(descriptives_Ger_numeric, summary=F, type="html", out="Output Paper/Descriptives/IGC_Descriptives_numerical_Germany.html", 
          rownames = F, digit.separate=0)

#Descriptives Categorical Variables
descr_female<-as.data.frame(t(table(dat_baseline_germany$female)), Population="Germany", Cohort="1950-1999")
descr_female$Var1<-"female"
descr_female$Share<-descr_female$Freq/sum(descr_female$Freq)
colnames(descr_female)[1]<-"Variable"
colnames(descr_female)[2]<-"Value"
descr_female$Population<-"Germany"
descr_female$Cohort<-"1950-1999"
descr_cohort<-as.data.frame(t(table(dat_baseline_germany$cohort)), Population="Germany", Cohort="1950-1999")
descr_cohort$Var1<-"Cohort"
descr_cohort$Share<-descr_cohort$Freq/sum(descr_cohort$Freq)
colnames(descr_cohort)[1]<-"Variable"
colnames(descr_cohort)[2]<-"Value"
descr_cohort$Population<-"Germany"
descr_cohort$Cohort<-NA
descriptives_categorical<-rbind(descr_female,descr_cohort)
stargazer(descriptives_categorical, summary=F, type="html", out="Output Paper/Descriptives/IGC_Descriptives_categorical_Germany.html", 
          rownames = F, digit.separate=0)

#Descriptives cohorts for all individuals where data for both parents is available
#define cohorts
cohorts_descr<-c("1950-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")
#define empty dataframe
descriptives_cohorts<-data.frame(Cohort=character(), N=numeric(), "Age"=character(), "Years of Education"=character(), "Parent's Years of Education"=character(), "Father's Years of Education mean"=character(), "Mother's Years of Education"=character(), check.names = F)
for(i in 1:length(cohorts_descr)){
  coh<-cohorts_descr[i]
  dataset<-dat_baseline_germany[dat_baseline_germany$cohort==coh,c("pid","gebjahr","years_of_education",
                                                       "age_last_survey","cohort", "parents_years_edu",
                                                       "f_years_of_education","m_years_of_education")]

  dataset<-na.omit(dataset)
  #calculate means and standard deviations of all variables as well as the sample size
  N<-nrow(dataset)
  age<-round(mean(dataset$age_last_survey), digits=2)
  age_sd<-round(sd(dataset$age_last_survey), digits=2)
  years_edu<-round(mean(dataset$years_of_education), digits=2)
  years_edu_sd<-round(sd(dataset$years_of_education), digits=2)
  years_edu_parents<-round(mean(dataset$parents_years_edu), digits=2)
  years_edu_parents_sd<-round(sd(dataset$parents_years_edu), digits=2)
  years_edu_father<-round(mean(dataset$f_years_of_education), digits=2)
  years_edu_father_sd<-round(sd(dataset$f_years_of_education), digits=2)
  years_edu_mother<-round(mean(dataset$m_years_of_education), digits=2)
  years_edu_mother_sd<-round(sd(dataset$m_years_of_education), digits=2)
  
  # the standard error is added in parenthesis behind the value of each variable
  descriptives_cohorts[i,]<-NA
  descriptives_cohorts$Cohort[i]<-coh
  descriptives_cohorts$N[i]<-N
  descriptives_cohorts$Age[i]<-paste0(age, " (", age_sd, ")")
  descriptives_cohorts$`Years of Education` [i]<-paste0(years_edu, " (", years_edu_sd, ")")
  descriptives_cohorts$`Parent's Years of Education`[i]<-paste0(years_edu_parents, " (", years_edu_parents_sd, ")")
  descriptives_cohorts$`Father's Years of Education mean`[i]<-paste0(years_edu_father, " (", years_edu_father_sd, ")")
  descriptives_cohorts$`Mother's Years of Education`[i]<-paste0(years_edu_mother, " (", years_edu_mother_sd, ")")
}
#save descriptive table as html
stargazer(descriptives_cohorts, summary=F, type="html", out="Output Paper/Descriptives/IGC_Descriptives_Cohorts_Germany.html", 
          digits=2, rownames = F, digit.separate=0)


#Descriptives East and West Germany Cohorts for all individuals where data for both parents is available
cohorts_descr<-c("all","1950-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999")
descriptives_east_west<-data.frame(Cohort=character(), Population=character(), N=numeric(), "Age"=character(), "Years of Education"=character(), "Parent's Years of Education"=character(), "Father's Years of Education mean"=character(), "Mother's Years of Education"=character(), check.names = F)
row=1
for(i in 1:length(cohorts_descr)){
  coh<-cohorts_descr[i]
  if (coh=="all"){
    dataset<-dat_baseline_east_west[dat_baseline_east_west$east_west_Germany=="eastgermany",c("pid","gebjahr","years_of_education",
                                                                                                                                   "age_last_survey","cohort", "east_west_Germany", "parents_years_edu",
                                                                                                                                   "f_years_of_education","m_years_of_education")]
    
  } else {
    dataset<-dat_baseline_east_west[dat_baseline_east_west$east_west_Germany=="eastgermany" & dat_baseline_east_west$cohort==coh,c("pid","gebjahr","years_of_education",
                                                                                                                                   "age_last_survey","cohort", "east_west_Germany", "parents_years_edu",
                                                                                                                                   "f_years_of_education","m_years_of_education")]
    
  }
  
  dataset<-na.omit(dataset)
  
  N<-nrow(dataset)
  age<-round(mean(dataset$age_last_survey), digits=2)
  age_sd<-round(sd(dataset$age_last_survey), digits=2)
  years_edu<-round(mean(dataset$years_of_education), digits=2)
  years_edu_sd<-round(sd(dataset$years_of_education), digits=2)
  years_edu_parents<-round(mean(dataset$parents_years_edu), digits=2)
  years_edu_parents_sd<-round(sd(dataset$parents_years_edu), digits=2)
  years_edu_father<-round(mean(dataset$f_years_of_education), digits=2)
  years_edu_father_sd<-round(sd(dataset$f_years_of_education), digits=2)
  years_edu_mother<-round(mean(dataset$m_years_of_education), digits=2)
  years_edu_mother_sd<-round(sd(dataset$m_years_of_education), digits=2)
  
  descriptives_east_west[row,]<-NA
  descriptives_east_west$Cohort[row]<-coh
  descriptives_east_west$Population[row]<-"eastgermany"
  descriptives_east_west$N[row]<-N
  descriptives_east_west$Age[row]<-paste0(age, " (", age_sd, ")")
  descriptives_east_west$`Years of Education` [row]<-paste0(years_edu, " (", years_edu_sd, ")")
  descriptives_east_west$`Parent's Years of Education`[row]<-paste0(years_edu_parents, " (", years_edu_parents_sd, ")")
  descriptives_east_west$`Father's Years of Education mean`[row]<-paste0(years_edu_father, " (", years_edu_father_sd, ")")
  descriptives_east_west$`Mother's Years of Education`[row]<-paste0(years_edu_mother, " (", years_edu_mother_sd, ")")
  row=row+1
  if (coh=="all"){
    dataset<-dat_baseline_east_west[dat_baseline_east_west$east_west_Germany=="westgermany",c("pid","gebjahr","years_of_education",
                                                                                              "age_last_survey","cohort", "east_west_Germany", "parents_years_edu",
                                                                                              "f_years_of_education","m_years_of_education")]
    
  } else {
    dataset<-dat_baseline_east_west[dat_baseline_east_west$east_west_Germany=="westgermany" & dat_baseline_east_west$cohort==coh,c("pid","gebjahr","years_of_education",
                                                                                                                                   "age_last_survey","cohort", "east_west_Germany", "parents_years_edu",
                                                                                                                                   "f_years_of_education","m_years_of_education")]
    
  }
  dataset<-na.omit(dataset)
  
  N<-nrow(dataset)
  age<-round(mean(dataset$age_last_survey), digits=2)
  age_sd<-round(sd(dataset$age_last_survey), digits=2)
  years_edu<-round(mean(dataset$years_of_education), digits=2)
  years_edu_sd<-round(sd(dataset$years_of_education), digits=2)
  years_edu_parents<-round(mean(dataset$parents_years_edu), digits=2)
  years_edu_parents_sd<-round(sd(dataset$parents_years_edu), digits=2)
  years_edu_father<-round(mean(dataset$f_years_of_education), digits=2)
  years_edu_father_sd<-round(sd(dataset$f_years_of_education), digits=2)
  years_edu_mother<-round(mean(dataset$m_years_of_education), digits=2)
  years_edu_mother_sd<-round(sd(dataset$m_years_of_education), digits=2)
  descriptives_east_west[row,]<-NA
  descriptives_east_west$Cohort[row]<-coh
  descriptives_east_west$Population[row]<-"westgermany"
  descriptives_east_west$N[row]<-N
  descriptives_east_west$Age[row]<-paste0(age, " (", age_sd, ")")
  descriptives_east_west$`Years of Education` [row]<-paste0(years_edu, " (", years_edu_sd, ")")
  descriptives_east_west$`Parent's Years of Education`[row]<-paste0(years_edu_parents, " (", years_edu_parents_sd, ")")
  descriptives_east_west$`Father's Years of Education mean`[row]<-paste0(years_edu_father, " (", years_edu_father_sd, ")")
  descriptives_east_west$`Mother's Years of Education`[row]<-paste0(years_edu_mother, " (", years_edu_mother_sd, ")")
  row=row+1
}

stargazer(descriptives_east_west, summary=F, type="html", out="Output Paper/Descriptives/IGC_descriptives_east_west_Germany.html", 
          digits=2, rownames = F, digit.separate=0)



#Descriptives States
descriptives_states<-data.frame(Population=character(), N=numeric(), "Age"=character(), "Birth Year"=character(), "Years of Education"=character(), "Parent's Years of Education"=character(), "Father's Years of Education mean"=character(), "Mother's Years of Education"=character(), check.names = F)
for(i in 1:length(bundesländer)){
  state<-bundesländer[i]
  dataset<-dat_baseline_states[dat_baseline_states$family_state==state,c("pid","gebjahr","years_of_education",
                                                                         "age_last_survey","cohort", "family_state", "parents_years_edu",
                                                                         "f_years_of_education","m_years_of_education")]
  dataset<-na.omit(dataset)
  N<-nrow(dataset)
  age<-round(mean(dataset$age_last_survey), digits=2)
  age_sd<-round(sd(dataset$age_last_survey), digits=2)
  birth<-round(mean(dataset$gebjahr), digits=2)
  birth_sd<-round(sd(dataset$gebjahr), digits=2)
  years_edu<-round(mean(dataset$years_of_education), digits=2)
  years_edu_sd<-round(sd(dataset$years_of_education), digits=2)
  years_edu_parents<-round(mean(dataset$parents_years_edu), digits=2)
  years_edu_parents_sd<-round(sd(dataset$parents_years_edu), digits=2)
  years_edu_father<-round(mean(dataset$f_years_of_education), digits=2)
  years_edu_father_sd<-round(sd(dataset$f_years_of_education), digits=2)
  years_edu_mother<-round(mean(dataset$m_years_of_education), digits=2)
  years_edu_mother_sd<-round(sd(dataset$m_years_of_education), digits=2)
  
  descriptives_states[i,]<-NA
  descriptives_states$Population[i]<-state
  descriptives_states$N[i]<-N
  descriptives_states$Age[i]<-paste0(age, " (", age_sd, ")")
  descriptives_states$`Birth Year`[i]<-paste0(birth, " (", birth_sd, ")")
  descriptives_states$`Years of Education` [i]<-paste0(years_edu, " (", years_edu_sd, ")")
  descriptives_states$`Parent's Years of Education`[i]<-paste0(years_edu_parents, " (", years_edu_parents_sd, ")")
  descriptives_states$`Father's Years of Education mean`[i]<-paste0(years_edu_father, " (", years_edu_father_sd, ")")
  descriptives_states$`Mother's Years of Education`[i]<-paste0(years_edu_mother, " (", years_edu_mother_sd, ")")
  
}
stargazer(descriptives_states, summary=F, type="html", out="Output Paper/Descriptives/IGC_descriptives_states_Germany.html", 
          digits=2, rownames = F, digit.separate=0)








######################## Estimation Intergenerational Correlation Coefficients ##############

############## Baseline Germany
#create empty lists for regression results and robust standard errors
lm_baseline_germany<-list()
lm_baseline_germany_robust_ses<-list()
lm_baseline_germany_robust_p<-list()

#Univariate regression parents to children - all gender
lm_baseline_germany[[1]]<-lm(years_of_education ~ parents_years_edu , data=dat_baseline_germany)
#compute heteroskedasticity-robust standard errors and p-values (White, 1980)
robust_results<-coeftest(lm_baseline_germany[[1]], vcov=hccm(lm_baseline_germany[[1]],type="hc0"))
lm_baseline_germany_robust_ses[[1]]<-robust_results[,"Std. Error"]
lm_baseline_germany_robust_p[[1]]<-robust_results[,"Pr(>|t|)"]
#Calculate corrected intergenerational correlation coefficient
baseline_IGC<-calculate_IGC(lm_result=lm_baseline_germany[[1]], dataset=dat_baseline_germany, parent="both", cohort="all", population="parents-child")

#Univariate regression father to children - all gender
lm_baseline_germany[[2]]<-lm(years_of_education ~ f_years_of_education, data=dat_baseline_germany)
#compute heteroskedasticity-robust standard errors and p-values (White, 1980)
robust_results<-coeftest(lm_baseline_germany[[2]], vcov=hccm(lm_baseline_germany[[2]],type="hc0"))
lm_baseline_germany_robust_ses[[2]]<-robust_results[,"Std. Error"]
lm_baseline_germany_robust_p[[2]]<-robust_results[,"Pr(>|t|)"]
#Calculate corrected intergenerational correlation coefficient
baseline_IGC<-rbind(baseline_IGC,calculate_IGC(lm_result=lm_baseline_germany[[2]], dataset=dat_baseline_germany, parent="father", cohort="all", population="father-child"))

#Univariate regression mother to children - all gender
lm_baseline_germany[[3]]<-lm(years_of_education ~ m_years_of_education, data=dat_baseline_germany)
#compute heteroskedasticity-robust standard errors and p-values (White, 1980)
robust_results<-coeftest(lm_baseline_germany[[3]], vcov=hccm(lm_baseline_germany[[3]],type="hc0"))
lm_baseline_germany_robust_ses[[3]]<-robust_results[,"Std. Error"]
lm_baseline_germany_robust_p[[3]]<-robust_results[,"Pr(>|t|)"]
#Calculate corrected intergenerational correlation coefficient
baseline_IGC<-rbind(baseline_IGC,calculate_IGC(lm_result=lm_baseline_germany[[3]], dataset=dat_baseline_germany, parent="mother", cohort="all", population="mother-child"))

#female-Specific
#father-son
lm_baseline_germany[[4]]<-lm(years_of_education ~ f_years_of_education, data=dat_baseline_germany_sons)
#compute heteroskedasticity-robust standard errors and p-values (White, 1980)
robust_results<-coeftest(lm_baseline_germany[[4]], vcov=hccm(lm_baseline_germany[[4]],type="hc0"))
lm_baseline_germany_robust_ses[[4]]<-robust_results[,"Std. Error"]
lm_baseline_germany_robust_p[[4]]<-robust_results[,"Pr(>|t|)"]
#Calculate corrected intergenerational correlation coefficient
baseline_IGC<-rbind(baseline_IGC,calculate_IGC(lm_result=lm_baseline_germany[[4]], dataset=dat_baseline_germany_sons, parent="father", cohort="all", population="father-son"))

#mother-son
lm_baseline_germany[[5]]<-lm(years_of_education ~ m_years_of_education, data=dat_baseline_germany_sons)
#compute heteroskedasticity-robust standard errors and p-values (White, 1980)
robust_results<-coeftest(lm_baseline_germany[[5]], vcov=hccm(lm_baseline_germany[[5]],type="hc0"))
lm_baseline_germany_robust_ses[[5]]<-robust_results[,"Std. Error"]
lm_baseline_germany_robust_p[[5]]<-robust_results[,"Pr(>|t|)"]
#Calculate corrected intergenerational correlation coefficient
baseline_IGC<-rbind(baseline_IGC,calculate_IGC(lm_result=lm_baseline_germany[[5]], dataset=dat_baseline_germany_sons, parent="mother", cohort="all", population="mother-son"))

#father-daughter
lm_baseline_germany[[6]]<-lm(years_of_education ~ f_years_of_education, data=dat_baseline_germany_daughters)
#compute heteroskedasticity-robust standard errors and p-values (White, 1980)
robust_results<-coeftest(lm_baseline_germany[[6]], vcov=hccm(lm_baseline_germany[[6]],type="hc0"))
lm_baseline_germany_robust_ses[[6]]<-robust_results[,"Std. Error"]
lm_baseline_germany_robust_p[[6]]<-robust_results[,"Pr(>|t|)"]
#Calculate corrected intergenerational correlation coefficient
baseline_IGC<-rbind(baseline_IGC,calculate_IGC(lm_result=lm_baseline_germany[[6]], dataset=dat_baseline_germany_daughters, parent="father", cohort="all", population="father-daughter"))

#mother-daughter
lm_baseline_germany[[7]]<-lm(years_of_education ~ m_years_of_education, data=dat_baseline_germany_daughters)
#compute heteroskedasticity-robust standard errors and p-values (White, 1980)
robust_results<-coeftest(lm_baseline_germany[[7]], vcov=hccm(lm_baseline_germany[[7]],type="hc0"))
lm_baseline_germany_robust_ses[[7]]<-robust_results[,"Std. Error"]
lm_baseline_germany_robust_p[[7]]<-robust_results[,"Pr(>|t|)"]
#Calculate corrected intergenerational correlation coefficient
baseline_IGC<-rbind(baseline_IGC,calculate_IGC(lm_result=lm_baseline_germany[[7]], dataset=dat_baseline_germany_daughters, parent="mother", cohort="all", population="mother-daughter"))

#parents-son
lm_baseline_germany[[8]]<-lm(years_of_education ~ parents_years_edu , data=dat_baseline_germany_sons)
#compute heteroskedasticity-robust standard errors and p-values (White, 1980)
robust_results<-coeftest(lm_baseline_germany[[8]], vcov=hccm(lm_baseline_germany[[8]],type="hc0"))
lm_baseline_germany_robust_ses[[8]]<-robust_results[,"Std. Error"]
lm_baseline_germany_robust_p[[8]]<-robust_results[,"Pr(>|t|)"]
#Calculate corrected intergenerational correlation coefficient
baseline_IGC<-rbind(baseline_IGC,calculate_IGC(lm_result=lm_baseline_germany[[8]], dataset=dat_baseline_germany_sons, parent="both", cohort="all", population="parents-son"))


#parents-daughter
lm_baseline_germany[[9]]<-lm(years_of_education ~ parents_years_edu , data=dat_baseline_germany_daughters)
#compute heteroskedasticity-robust standard errors and p-values (White, 1980)
robust_results<-coeftest(lm_baseline_germany[[9]], vcov=hccm(lm_baseline_germany[[9]],type="hc0"))
lm_baseline_germany_robust_ses[[9]]<-robust_results[,"Std. Error"]
lm_baseline_germany_robust_p[[9]]<-robust_results[,"Pr(>|t|)"]
#Calculate corrected intergenerational correlation coefficient
baseline_IGC<-rbind(baseline_IGC,calculate_IGC(lm_result=lm_baseline_germany[[9]], dataset=dat_baseline_germany_daughters, parent="both", cohort="all", population="parents-daughter"))



#save regression results in html table
#define custom coefficient names
coef_names1=c("Intercept", "Years of Schooling Parents", "Years of Schooling Father", "Years of Schooling Mother")
#define note
note_baseline_germany1="Intergenerational Elasticity Coefficients calculated without covariates. Standard errors and P-Values are heteroskedasticity-robust (White, 1980). All adults were at least 21 years old when they were last interviewed. Data: German socio-economic Panel (SOEP)"
htmlreg(lm_baseline_germany[c(1:3,8,9)], file="Output Paper/IGE Baseline Results Germany.html",
        caption="Baseline Results Germany", center = F, caption.above = TRUE, omit.coef = c('Intercept|cohort|age_last_survey'),
        custom.coef.names = coef_names1,
        custom.header=list("Dep. Variable: Years of Education"=1:5), custom.model.names = c("1 - parents-child", "2 - father-child", "3 - mother-child", "4 - parents-son", "5 - pparents-daughter"),
        override.se=lm_baseline_germany_robust_ses[c(1:3,8,9)],
        override.pvalues = lm_baseline_germany_robust_p[c(1:3,8,9)],
        stars=c(0.01,0.05,0.1), digits=4, include.rs=T, include.adjrs = T, single.row=T,
        custom.note=note_baseline_germany1,
        ) 

#save IGE and IGC as html table
stargazer(baseline_IGC, summary=F, type="html", out="Output Paper/IGE_IGC_baseline_Germany.html", rownames = F, title = "Intergenerational Elasticity and Correlation of Education")

coef_names2=c("Intercept", "Years of Schooling Father", "Years of Schooling Mother")
note_baseline_germany1="Intergenerational Elasticity Coefficients calculated without covariates. Standard errors and P-Values are heteroskedasticity-robust (White, 1980). All adults were at least 21 years old when they were last interviewed. Data: German socio-economic Panel (SOEP)"
htmlreg(lm_baseline_germany[4:7], file="Output Paper/IGE Baseline Results Germany female-specific.html",
        caption="Baseline Results Germany", center = F, caption.above = TRUE, omit.coef = c('Intercept|cohort|age_last_survey'),
        custom.coef.names = coef_names2,
        custom.header=list("Dep. Variable: Years of Education"=1:4), custom.model.names = c("4 - father-son","5 - mother-son","6 - father-daughter","7 - mother-daughter"),
        override.se=lm_baseline_germany_robust_ses[4:7],
        override.pvalues = lm_baseline_germany_robust_p[4:7],
        stars=c(0.01,0.05,0.1), digits=4, include.rs=T, include.adjrs = T, single.row=T,
        #reorder.gof = c(1,2,3,5,6,7,4),
        custom.note=note_baseline_germany1,
        #custom.gof.rows=list("Time Fixed Effects" = c("Yes","Yes","Yes","Yes"), "Bank Type Fixed Effects" = c("No","No","No","No"), "Controls" = c("Yes","Yes","Yes","Yes"), "Years" = c(paste0(start_year, "-1998"), paste0("1999-", end_year),paste0(start_year, "-1998"), paste0("1999-", end_year)))
) 

stargazer(baseline_IGC[,-5], summary=F, type="html", out="Output Paper/IGC_baseline_Germany.html", rownames = F, title = "Intergenerational Elasticity and Correlation of Education")

#plot results
data_plot<-baseline_IGC
#separate estimates and standard errors
data_plot$ige_std_err<-apply(data_plot, 1, function(x) as.numeric(gsub('[()]','', unlist(str_split(x["IGE"], " "))[2])) )
data_plot$ige<-apply(data_plot, 1, function(x) as.numeric(unlist(str_split(x["IGE"], " "))[1]) )
data_plot$igc_std_err<-apply(data_plot, 1, function(x) as.numeric(gsub('[()]','', unlist(str_split(x["IGC"], " "))[2])) )
data_plot$igc<-apply(data_plot, 1, function(x) as.numeric(unlist(str_split(x["IGC"], " "))[1]) )
#calculate 95% confidence intervals
data_plot$ige_CI_lower<-data_plot$ige-(data_plot$ige_std_err*qnorm(1-0.05/2))
data_plot$ige_CI_upper<-data_plot$ige+(data_plot$ige_std_err*qnorm(1-0.05/2))
data_plot$igc_CI_lower<-data_plot$igc-(data_plot$igc_std_err*qnorm(1-0.05/2))
data_plot$igc_CI_upper<-data_plot$igc+(data_plot$igc_std_err*qnorm(1-0.05/2))


#Plot results 
#State names in two rows
data_plot$population<-factor(data_plot$population, level=data_plot$population, labels=gsub("-", "-\n", data_plot$population))
#plot IGE
plot_baseline_germany_ige<-ggplot(data_plot, aes(x = population, y = ige)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = ige_CI_lower, ymax=ige_CI_upper)) + 
  labs(title="Intergenerational Elasticity of Education", subtitle = "Dep. Var: Years of Education", x = "Population", y = "IGE (95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "IGE_baseline_germany" ,".jpg"), plot=plot_baseline_germany_ige, width = 10, height = 6, units = "in")
#Plot IGC
plot_baseline_germany_igc<-ggplot(data_plot, aes(x = population, y = igc)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = igc_CI_lower, ymax=igc_CI_upper)) + 
  labs(title="Intergenerational Elasticity of Education", subtitle = "Dep. Var: Years of Education", x = "Population", y = "IGC (95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "IGC_baseline_germany" ,".jpg"), plot=plot_baseline_germany_igc, width = 10, height = 6, units = "in")


#######Cohorts
# Years of Education Parents - Children
##############################Baseline Cohort
#cohort as factor
dat_baseline_germany$cohort<-factor(dat_baseline_germany$cohort, levels=c("1950-1959", "1960-1964", "1965-1969", "1970-1974", "1975-1979", "1980-1984", "1985-1989", "1990-1994", "1995-1999"))
table(dat_baseline_germany$cohort)
cohorts<-levels(dat_baseline_germany$cohort)

#run regression for each cohort and calculate robust standard errors and p-values
lm_cohorts_germany <- lapply(1:length(cohorts), function(x) lm(years_of_education ~ parents_years_edu, data=dat_baseline_germany[dat_baseline_germany$cohort==cohorts[x],]))
lm_cohorts_germany_robust_se <- lapply(lm_cohorts_germany, function(x) coeftest(x, vcov=hccm(x,type="hc0"))[,"Std. Error"])
lm_cohorts_germany_robust_p <- lapply(lm_cohorts_germany, function(x) coeftest(x, vcov=hccm(x,type="hc0"))[,"Pr(>|t|)"])
#save regression results
htmlreg(lm_cohorts_germany, file="Output Paper/Appendix/IGE_cohort.html",
        caption="Regression Results", center = F, caption.above = TRUE, 
        omit.coef = c('Intercept|cohort'),custom.model.names = cohorts,stars=c(0.01,0.05,0.1), 
        digits=4, include.rs=T, include.adjrs = T, single.row=F,
        override.se=lm_cohorts_germany_robust_se,
        override.pvalues = lm_cohorts_germany_robust_p) 

#Calculate IGC
for (i in 1:length(cohorts)){
  if (i==1){
    IGE_IGC_Baseline_cohorts<-calculate_IGC(lm_result=lm_cohorts_germany[[i]], dataset=dat_baseline_germany[dat_baseline_germany$cohort==cohorts[i],], parent="both", cohort=cohorts[i], population="parents-child")
    
  } else  {
    IGE_IGC_Baseline_cohorts<-rbind(IGE_IGC_Baseline_cohorts,calculate_IGC(lm_result=lm_cohorts_germany[[i]], dataset=dat_baseline_germany[dat_baseline_germany$cohort==cohorts[i],], parent="both", cohort=cohorts[i], population="parents-child"))
    
  }
}
stargazer(IGE_IGC_Baseline_cohorts, summary=F, type="html", out="Output Paper/IGE_IGC_cohorts.html", rownames = F)


############### Graphen
for (i in 1:length(cohorts)){
  if (i==1){
    table_plot_cohorts<-calculate_IGC(lm_result=lm_cohorts_germany[[i]], dataset=dat_baseline_germany[dat_baseline_germany$cohort==cohorts[i],], parent="both", cohort=cohorts[i], population="parents-child", return_CI = T)
    
  } else  {
    table_plot_cohorts<-rbind(table_plot_cohorts,calculate_IGC(lm_result=lm_cohorts_germany[[i]], dataset=dat_baseline_germany[dat_baseline_germany$cohort==cohorts[i],], parent="both", cohort=cohorts[i], population="parents-child", return_CI = T))
    
  }
}
table_plot_cohorts$cohort<-gsub("-", "-\n", table_plot_cohorts$cohort)

#Plot IGE with 95% CI
plot_IGE_cohort<-ggplot(table_plot_cohorts[table_plot_cohorts$cohort!="<1950",], aes(x= factor(cohort, levels=gsub("-", "-\n", cohorts)), IGE)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = CI_IGE_lower, ymax = CI_IGE_upper)) + 
  labs(title="Intergenerational Elasticity of Education", subtitle = "Parents - Child", x = "Cohort", y = "IGE (95% CI)", fill = NULL)

ggsave(filename=paste0("Output Paper/Graphen/", "plot_IGE_cohort" ,".jpg"), plot=plot_IGE_cohort, width = 6, height = 4, units = "in")
#plot IGC with 95% CI
plot_IGC_cohort<-ggplot(table_plot_cohorts[table_plot_cohorts$cohort!="<1950",], aes(x= factor(cohort, levels=gsub("-", "-\n", cohorts)), IGC)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = CI_IGC_lower, ymax = CI_IGC_upper)) + 
  labs(title="Intergenerational Correlation of Education", subtitle = "Parents - Child", x = "Cohort", y = "IGC (95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "plot_IGC_cohort" ,".jpg"), plot=plot_IGC_cohort, width = 6, height = 4, units = "in")

#IGC and IGE in one column to plot both in one graph
data_plot1<-table_plot_cohorts[,c(2,5,6,9,10)]
data_plot2<-table_plot_cohorts[,c(2,7,8,11,12)]
colnames(data_plot1)<-c("Cohort", "Estimate", "Std_Error", "CI_lower", "CI_upper")
data_plot1$Variable<-"IGE"
colnames(data_plot2)<-c("Cohort", "Estimate", "Std_Error", "CI_lower", "CI_upper")
data_plot2$Variable<-"IGC"
data_plot<-rbind(data_plot1, data_plot2)
#with 95% CI
plot_IGC_IGE_cohort<-ggplot(data_plot, aes(x = Cohort, y = Estimate, group=Variable)) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), fill = c("grey70"), alpha =0.2) +
  geom_line(aes(color=Variable)) + geom_point() + 
  labs(title="Intergenerational Elasticity and Correlation of Education", subtitle = "Parents - Child", x = "Cohorts", y = "Estimate (95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "plot_IGE_vs_IGC" ,".jpg"), plot=plot_IGC_IGE_cohort, width = 10, height = 6, units = "in")

#with 68% and 95% CI
plot_IGC_IGE_cohort2<-ggplot(data_plot, aes(x = Cohort, y = Estimate, group=Variable)) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), fill = c("grey70"), alpha =0.2) +
  geom_ribbon(aes(ymin = Estimate-Std_Error*qnorm(1-0.32/2), ymax = Estimate+Std_Error*qnorm(1-0.32/2)), fill = c("grey70"), alpha =0.2) +
  geom_line(aes(color=Variable)) + geom_point()  + 
  labs(title="Intergenerational Elasticity and Correlation of Education", subtitle = "Parents - Child", x = "Cohorts", y = "Estimate (68 and 95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "plot_IGE_vs_IGC68_95" ,".jpg"), plot=plot_IGC_IGE_cohort2, width = 6, height = 4, units = "in")






##################### East and West Germany Cohorts
# Years of Education Parents - Children
#cohort as factor
dat_baseline_east_west$cohort<-factor(dat_baseline_east_west$cohort, levels=c("1950-1959", "1960-1964", "1965-1969", "1970-1974", "1975-1979", "1980-1984", "1985-1989", "1990-1994", "1995-1999"))
table(dat_baseline_east_west$cohort[dat_baseline_east_west$east_west_Germany=="westgermany"])
table(dat_baseline_east_west$cohort[dat_baseline_east_west$east_west_Germany=="eastgermany"])
cohorts<-levels(dat_baseline_east_west$cohort)
#run regression for each cohort for east and west Germany and calculate robust standard errors and p-values
for (east_west in c("eastgermany", "westgermany")){
  lm_cohorts_eastwest <- lapply(1:length(cohorts), function(x) lm(years_of_education ~ parents_years_edu, data=dat_baseline_east_west[dat_baseline_east_west$cohort==cohorts[x] & dat_baseline_east_west$east_west_Germany==east_west,]))
  lm_cohorts_eastwest_robust_se <- lapply(lm_cohorts_eastwest, function(x) coeftest(x, vcov=hccm(x,type="hc0"))[,"Std. Error"])
  lm_cohorts_eastwest_robust_p <- lapply(lm_cohorts_eastwest, function(x) coeftest(x, vcov=hccm(x,type="hc0"))[,"Pr(>|t|)"])
  #save regression results
  htmlreg(lm_cohorts_eastwest, file=paste0("Output Paper/Appendix/IGE_cohorts_", east_west, ".html"),
          caption=paste0("Regression Results ", east_west), center = F, caption.above = TRUE, 
          omit.coef = c('Intercept|cohort'),custom.model.names = cohorts,stars=c(0.01,0.05,0.1), 
          digits=4, include.rs=T, include.adjrs = T, single.row=F,
          override.se=lm_cohorts_eastwest_robust_se,
          override.pvalues = lm_cohorts_eastwest_robust_p) 
  
  #Calculate IGC
  for (i in 1:length(cohorts)){
    if (i==1 & east_west=="eastgermany"){
      IGE_IGC_east_west<-calculate_IGC(lm_result=lm_cohorts_eastwest[[i]], dataset=dat_baseline_east_west[dat_baseline_east_west$cohort==cohorts[i] & dat_baseline_east_west$east_west_Germany==east_west,], parent="both", cohort=cohorts[i], population="parents-child")
      IGE_IGC_east_west$East_West=east_west
      
    } else  {
      new_row<-calculate_IGC(lm_result=lm_cohorts_eastwest[[i]], dataset=dat_baseline_east_west[dat_baseline_east_west$cohort==cohorts[i],], parent="both", cohort=cohorts[i], population="parents-child")
      new_row$East_West=east_west
      IGE_IGC_east_west<-rbind(IGE_IGC_east_west,new_row)
    }
  }
  
  ############### Graphen
  for (i in 1:length(cohorts)){
    if (i==1 & east_west=="eastgermany"){
      table_plot_east_west<-calculate_IGC(lm_result=lm_cohorts_eastwest[[i]], dataset=dat_baseline_east_west[dat_baseline_east_west$cohort==cohorts[i],], parent="both", cohort=cohorts[i], population="parents-child", return_CI = T)
      table_plot_east_west$East_West=east_west
    } else  {
      new_row<-calculate_IGC(lm_result=lm_cohorts_eastwest[[i]], dataset=dat_baseline_east_west[dat_baseline_east_west$cohort==cohorts[i],], parent="both", cohort=cohorts[i], population="parents-child", return_CI = T)
      new_row$East_West=east_west
      table_plot_east_west<-rbind(table_plot_east_west,new_row)
      
    }
  }
  
}
#save regression results as html
IGE_IGC_east_west<-IGE_IGC_east_west[,c(9,1:8)]
stargazer(IGE_IGC_east_west, summary=F, type="html", out="Output Paper/Appendix/IGE_IGC_east_west_germany.html", rownames = F)

# cohort and state variable as factor with modified labels for plots
unique(table_plot_east_west$cohort)
table_plot_east_west$cohort<-factor(table_plot_east_west$cohort, labels=gsub("-", "-\n", unique(table_plot_east_west$cohort)))
table_plot_east_west$East_West<-factor(table_plot_east_west$East_West, labels=c("East Germany", "West Germany"))
#plot IGE and IGC East and West Germany
plot_IGE_east_west<-ggplot(table_plot_east_west, aes(x= cohort, y=IGE, group=East_West)) +
  #geom_ribbon(aes(ymin = CI_IGE_lower, ymax = CI_IGE_upper), fill = c("grey70"), alpha =0.2) +
  geom_ribbon(aes(ymin = IGE-std_error_IGE*qnorm(1-0.32/2), ymax = IGE+std_error_IGE*qnorm(1-0.32/2)), fill = c("grey70"), alpha =0.2) +
  geom_line(aes(color=East_West)) + geom_point()  + 
  labs(title="Intergenerational Elasticity of Education", subtitle = "Parents - Child", x = "Cohorts", y = "IGE (68% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "plot_IGE_east_west" ,".jpg"), plot=plot_IGE_east_west, width = 10, height = 6, units = "in")

plot_IGC_east_west<-ggplot(table_plot_east_west, aes(x= cohort, y=IGC, group=East_West)) +
  #geom_ribbon(aes(ymin = CI_IGC_lower, ymax = CI_IGC_upper), fill = c("grey70"), alpha =0.2) +
  geom_ribbon(aes(ymin = IGC-std_error_IGC*qnorm(1-0.32/2), ymax = IGC+std_error_IGC*qnorm(1-0.32/2)), fill = c("grey70"), alpha =0.2) +
  geom_line(aes(color=East_West)) + geom_point()  + 
  labs(title="Intergenerational Correlation of Education", subtitle = "Parents - Child", x = "Cohorts", y = "IGC (68% CI)", fill = NULL)

ggsave(filename=paste0("Output Paper/Graphen/", "plot_IGC_east_west" ,".jpg"), plot=plot_IGC_east_west, width = 10, height = 6, units = "in")


####### Baseline States
#run regressions for each state
lm_states <- lapply(1:length(bundesländer), function(x) lm(years_of_education ~ parents_years_edu, data=dat_baseline_states[dat_baseline_states$family_state==bundesländer[x],]))
lm_states_robust_se <- lapply(lm_states, function(x) coeftest(x, vcov=hccm(x,type="hc0"))[,"Std. Error"])
lm_states_robust_p <- lapply(lm_states, function(x) coeftest(x, vcov=hccm(x,type="hc0"))[,"Pr(>|t|)"])
#save regression results
htmlreg(lm_states, file="Output Paper/Appendix/IGE_states.html",
        caption="Regression Results", center = F, caption.above = TRUE, 
        omit.coef = c('Intercept'),custom.model.names = bundesländer,stars=c(0.01,0.05,0.1), 
        digits=4, include.rs=T, include.adjrs = T, single.row=F,
        override.se=lm_states_robust_se,
        override.pvalues = lm_states_robust_p) 


###### Calculate IGC
#For results table
for (i in 1:length(bundesländer)){
  if (i==1){
    IGE_IGC_Baseline_states<-calculate_IGC(lm_result=lm_states[[i]], dataset=dat_baseline_states[dat_baseline_states$family_state==bundesländer[i],], parent="both", cohort="all", population=bundesländer[i], return_CI = F)
    
  } else  {
    IGE_IGC_Baseline_states<-rbind(IGE_IGC_Baseline_states,calculate_IGC(lm_result=lm_states[[i]], dataset=dat_baseline_states[dat_baseline_states$family_state==bundesländer[i],], parent="both", cohort="all", population=bundesländer[i], return_CI = F))
  }
}

# For Plot
for (i in 1:length(bundesländer)){
  if (i==1){
    table_plot_states<-calculate_IGC(lm_result=lm_states[[i]], dataset=dat_baseline_states[dat_baseline_states$family_state==bundesländer[i],], parent="both", cohort="all", population=bundesländer[i], return_CI = T)
    
  } else  {
    table_plot_states<-rbind(table_plot_states,calculate_IGC(lm_result=lm_states[[i]], dataset=dat_baseline_states[dat_baseline_states$family_state==bundesländer[i],], parent="both", cohort="all", population=bundesländer[i], return_CI = T))
  }
}
#Order States in descending order
IGE_IGC_Baseline_states<-IGE_IGC_Baseline_states[order(table_plot_states$IGC, decreasing=T),]
#Save as thml table
stargazer(IGE_IGC_Baseline_states, summary=F, type="html", out="Output Paper/Appendix/IGE_IGC_states.html",column.labels=bundesländer)

#change state names for plot
table_plot_states$State<-unlist(lapply(as.character(table_plot_states$population), function (x) {unlist(str_split(as.character(x), " "))[[2]]}))
#add Germany
Germany_CI<-calculate_IGC(lm_result=lm_baseline_germany[[1]], dataset=dat_baseline_germany, parent="both", cohort="all", population="parents-child", return_CI = T)
Germany_CI$population<-"Germany"
Germany_CI$State<-"Germany"
table_plot_states<-rbind(Germany_CI, table_plot_states)
#Change State names for x-Axis
table_plot_states$State<-gsub("-", "-\n", table_plot_states$State)
table_plot_states$State[table_plot_states$State=="Niedersachsen"]<-"Nieder-\nsachsen"
table_plot_states$State[table_plot_states$State=="Brandenburg"]<-"Branden-\nburg"
table_plot_states$State[table_plot_states$State=="Baden-\nWuerttemberg"]<-"Baden-\nWuerttem-\nberg"

#table_plot_states<-table_plot_states[order(table_plot_states$IGC, decreasing=T),]
plot_IGE_states<-ggplot(table_plot_states, aes(x= factor(State, level = State[order(IGE, decreasing =T)]), IGE)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = CI_IGE_lower, ymax = CI_IGE_upper)) + 
  labs(title="Intergenerational Elasticity of Education", subtitle = "Parents - Child", x = "Bundesland", y = "IGE (95% CI)", fill = NULL)

ggsave(filename=paste0("Output Paper/Graphen/", "plot_IGE_states" ,".jpg"), plot=plot_IGE_states, width = 10, height = 6, units = "in")

plot_IGC_states<-ggplot(table_plot_states, aes(x= factor(State, level = State[order(IGC, decreasing =T)]), IGC)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = CI_IGC_lower, ymax = CI_IGC_upper)) + 
  labs(title="Intergenerational Correlation of Education", subtitle = "Parents - Child", x = "Bundesland", y = "IGC (95% CI)", fill = NULL)

ggsave(filename=paste0("Output Paper/Graphen/", "plot_IGC_states" ,".jpg"), plot=plot_IGC_states, width = 10, height = 6, units = "in")







##################### States and Cohorts
# Estimate IGC and IGE for states and cohorts, when the sample size is large enough
cohorts2<-c("1960-1969","1970-1979","1980-1989","1990-1999")
large_states<-names(table(dat_baseline_states$family_state))[table(dat_baseline_states$family_state)>300]
for (coh in cohorts2){
  dat<-dat_baseline_states[dat_baseline_states$cohort2==coh,]
  lm_coh_länder<-list()
  table_plot<-data.frame()
  for (i in 1:length(large_states)){
    dat_state<-dat[dat$family_state==large_states[i],]
    if(nrow(dat_state)>10) {
      lm_coh_länder[[i]]<-lm(years_of_education ~ parents_years_edu, data=dat_state)
      if (nrow(table_plot)==0){
        table_plot<-calculate_IGC(lm_result=lm_coh_länder[[i]], dataset=dat_state, parent="both", cohort=coh, population=large_states[i], return_CI = T)
      } else  {
        table_plot<-rbind(table_plot,calculate_IGC(lm_result=lm_coh_länder[[i]], dataset=dat_state, parent="both", cohort=coh, population=large_states[i], return_CI = T))
      }
    }
  }
  if (coh=="1960-1969") results_coh_states<-table_plot else results_coh_states<-rbind(results_coh_states,table_plot)
}

#create subsamples for the largest eastgerman states  westgerman states
results_coh_states_east<-results_coh_states[results_coh_states$population%in% c("[13] Sachsen", "[14] Sachsen-Anhalt","[16] Thueringen", "[4] Brandenburg"),]
results_coh_states_west<-results_coh_states[results_coh_states$population%in% c("[1] Baden-Wuerttemberg","[10] Nordrhein-Westfalen","[11] Rheinland-Pfalz", "[2] Bayern","[7] Hessen","[9] Niedersachsen"),]

#Change state names for axis labels
results_coh_states$State<-unlist(lapply(as.character(results_coh_states$population), function (x) {unlist(str_split(as.character(x), " "))[[2]]}))
results_coh_states$State<-gsub("-", "-\n", results_coh_states$State)
results_coh_states_east$State<-unlist(lapply(as.character(results_coh_states_east$population), function (x) {unlist(str_split(as.character(x), " "))[[2]]}))
results_coh_states_east$State<-gsub("-", "-\n", results_coh_states_east$State)
results_coh_states_west$State<-unlist(lapply(as.character(results_coh_states_west$population), function (x) {unlist(str_split(as.character(x), " "))[[2]]}))
results_coh_states_west$State<-gsub("-", "-\n", results_coh_states_west$State)

#IGE large States and Cohorts
IGE_cohorts_states<-ggplot(results_coh_states, aes(x = factor(cohort), y = IGE, group=State)) +
  #geom_ribbon(aes(ymin = CI_IGE_lower , ymax = CI_IGE_upper), fill = c("grey70"), alpha =0.2) + 
  geom_line(aes(color=State)) + geom_point() + labs(title="Intergenerational Elasticity of Education", subtitle = "Parents - Child", x = "Cohorts", y = "IGE", fill = NULL)

ggsave(filename=paste0("Output Paper/Appendix/", "IGC_cohorts_states" ,".jpg"), plot=IGE_cohorts_states, width = 10, height = 6, units = "in")

#IGC large States and Cohorts
IGC_cohorts_states<-ggplot(results_coh_states, aes(x = factor(cohort), y = IGC, group=State)) +
  #geom_ribbon(aes(ymin = CI_IGE_lower , ymax = CI_IGE_upper), fill = c("grey70"), alpha =0.2) + 
  geom_line(aes(color=State)) + geom_point() + labs(title="Intergenerational Correlation of Education", subtitle = "Parents - Child", x = "Cohorts", y = "IGC", fill = NULL)

ggsave(filename=paste0("Output Paper/Appendix/", "IGC_cohorts_states" ,".jpg"), plot=IGC_cohorts_states, width = 10, height = 6, units = "in")

#IGE Eastgerman States and Cohorts
IGE_cohorts_states_east<-ggplot(results_coh_states_east, aes(x = factor(cohort), y = IGE, group=State)) +
  geom_ribbon(aes(ymin = IGE-std_error_IGE*qnorm(1-0.32/2), ymax = IGE+std_error_IGE*qnorm(1-0.32/2)), fill = c("grey70"), alpha =0.2) +
  geom_line(aes(color=State)) + geom_point() + labs(title="Intergenerational Elasticity of Education", subtitle = "Parents - Child", x = "Cohorts", y = "IGE  (68% CI)", fill = NULL)

ggsave(filename=paste0("Output Paper/Appendix/", "IGE_cohorts_states_east" ,".jpg"), plot=IGE_cohorts_states_east, width = 10, height = 6, units = "in")

#IGC Eastgerman States and Cohorts
IGC_cohorts_states_east<-ggplot(results_coh_states_east, aes(x = factor(cohort), y = IGC, group=State)) +
  geom_ribbon(aes(ymin = IGC-std_error_IGC*qnorm(1-0.32/2), ymax = IGC+std_error_IGC*qnorm(1-0.32/2)), fill = c("grey70"), alpha =0.2) +
  geom_line(aes(color=State)) + geom_point() + labs(title="Intergenerational Correlation of Education", subtitle = "Parents - Child", x = "Cohorts", y = "IGC  (68% CI)", fill = NULL)

ggsave(filename=paste0("Output Paper/Appendix/", "IGC_cohorts_states_east" ,".jpg"), plot=IGC_cohorts_states_east, width = 10, height = 6, units = "in")

#IGE Westgerman States and Cohorts
IGE_cohorts_states_west<-ggplot(results_coh_states_west, aes(x = factor(cohort), y = IGE, group=State)) +
  geom_ribbon(aes(ymin = IGE-std_error_IGE*qnorm(1-0.32/2), ymax = IGE+std_error_IGE*qnorm(1-0.32/2)), fill = c("grey70"), alpha =0.2) +
  geom_line(aes(color=State)) + geom_point() + labs(title="Intergenerational Elasticity of Education", subtitle = "Parents - Child", x = "Cohorts", y = "IGE  (68% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Appendix/", "IGE_cohorts_states_west" ,".jpg"), plot=IGE_cohorts_states_west, width = 10, height = 6, units = "in")

#IGC Westgerman States and Cohorts
IGC_cohorts_states_west<-ggplot(results_coh_states_west, aes(x = factor(cohort), y = IGC, group=State)) +
  geom_ribbon(aes(ymin = IGC-std_error_IGC*qnorm(1-0.32/2), ymax = IGC+std_error_IGC*qnorm(1-0.32/2)), fill = c("grey70"), alpha =0.2) +
  geom_line(aes(color=State)) + geom_point() + labs(title="Intergenerational Correlation of Education", subtitle = "Parents - Child", x = "Cohorts", y = "IGC  (68% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Appendix/", "IGC_cohorts_states_west" ,".jpg"), plot=IGC_cohorts_states_west, width = 10, height = 6, units = "in")









###########Robustness Checks
# Restrictions regarding age at last survey >25
#remove last cohort, since it is empty in this dataframe
cohorts<-cohorts[-which(cohorts=="1995-1999")]
lm_robustness_age2 <- lapply(1:length(cohorts), function(x) lm(years_of_education ~ parents_years_edu, data=dat_baseline_germany25[dat_baseline_germany25$cohort==cohorts[x],]))
lm_robustness_age2_robust_se <- lapply(lm_robustness_age2, function(x) coeftest(x, vcov=hccm(x,type="hc0"))[,"Std. Error"])
lm_robustness_age2_robust_p <- lapply(lm_robustness_age2, function(x) coeftest(x, vcov=hccm(x,type="hc0"))[,"Pr(>|t|)"])
#save regression results
htmlreg(lm_robustness_age2, file="Output Paper/Appendix/IGE_robustness_age2.html",
        caption="Regression Results", center = F, caption.above = TRUE, 
        omit.coef = c('Intercept|cohort'),custom.model.names = cohorts,stars=c(0.01,0.05,0.1), 
        digits=4, include.rs=T, include.adjrs = T, single.row=F,
        override.se=lm_robustness_age2_robust_se,
        override.pvalues = lm_robustness_age2_robust_p) 

#Calculate IGC
for (i in 1:length(cohorts)){
  if (i==1){
    IGE_IGC_robustness_age2<-calculate_IGC(lm_result=lm_robustness_age2[[i]], dataset=dat_baseline_germany25[dat_baseline_germany25$cohort==cohorts[i],], parent="both", cohort=cohorts[i], population="parents-child")
    
  } else  {
    IGE_IGC_robustness_age2<-rbind(IGE_IGC_robustness_age2,calculate_IGC(lm_result=lm_robustness_age2[[i]], dataset=dat_baseline_germany25[dat_baseline_germany25$cohort==cohorts[i],], parent="both", cohort=cohorts[i], population="parents-child"))
    
  }
}
stargazer(IGE_IGC_robustness_age2, summary=F, type="html", out="Output Paper/IGE_IGC_robustness_age2.html", rownames = F)


############### Graphen
for (i in 1:length(cohorts)){
  if (i==1){
    table_plot_robustness_age2<-calculate_IGC(lm_result=lm_robustness_age2[[i]], dataset=dat_baseline_germany25[dat_baseline_germany25$cohort==cohorts[i],], parent="both", cohort=cohorts[i], population="parents-child", return_CI = T)
    
  } else  {
    table_plot_robustness_age2<-rbind(table_plot_robustness_age2,calculate_IGC(lm_result=lm_robustness_age2[[i]], dataset=dat_baseline_germany25[dat_baseline_germany25$cohort==cohorts[i],], parent="both", cohort=cohorts[i], population="parents-child", return_CI = T))
    
  }
}
#state names in two rows
table_plot_robustness_age2$cohort<-factor(table_plot_robustness_age2$cohort, labels=gsub("-", "-\n", table_plot_robustness_age2$cohort))
#plot results
plot_IGE_robustness_age2<-ggplot(table_plot_robustness_age2, aes(x=cohort, y=IGE)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = CI_IGE_lower, ymax = CI_IGE_upper)) + 
  labs(title="Intergenerational Elasticity of Education", subtitle = "Parents - Child", x = "Cohort", y = "IGE (95% CI)", fill = NULL)

ggsave(filename=paste0("Output Paper/Graphen/", "IGE_robustness_age2" ,".jpg"), plot=plot_IGE_robustness_age2, width = 6, height = 4, units = "in")

plot_IGC_robustness_age2<-ggplot(table_plot_robustness_age2[table_plot_robustness_age2$cohort!="<1950",], aes(x= factor(cohort, levels=gsub("-", "-\n", cohorts)), IGC)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = CI_IGC_lower, ymax = CI_IGC_upper)) + 
  labs(title="Intergenerational Correlation of Education", subtitle = "Parents - Child", x = "Cohort", y = "IGC (95% CI)", fill = NULL)

ggsave(filename=paste0("Output Paper/Graphen/", "IGC_robustness_age2" ,".jpg"), plot=plot_IGC_robustness_age2, width = 6, height = 4, units = "in")

#IGC and IGE in one column to plot both in one graph
data_plot1<-table_plot_robustness_age2[,c(2,5,6,9,10)]
data_plot2<-table_plot_robustness_age2[,c(2,7,8,11,12)]
colnames(data_plot1)<-c("Cohort", "Estimate", "Std_Error", "CI_lower", "CI_upper")
data_plot1$Variable<-"IGE"
colnames(data_plot2)<-c("Cohort", "Estimate", "Std_Error", "CI_lower", "CI_upper")
data_plot2$Variable<-"IGC"
data_plot<-rbind(data_plot1, data_plot2)
#with 95% CI
plot_IGC_IGE_robustness_age2<-ggplot(data_plot, aes(x = Cohort, y = Estimate, group=Variable)) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), fill = c("grey70"), alpha =0.2) +
  geom_line(aes(color=Variable)) + geom_point() + 
  labs(title="Intergenerational Elasticity and Correlation of Education", subtitle = "Parents - Child", x = "Cohorts", y = "Estimate (95% CI)", fill = NULL)
ggsave(filename=paste0("Output Paper/Graphen/", "IGC_IGE_robustness_age2" ,".jpg"), plot=plot_IGC_IGE_robustness_age2, width = 10, height = 6, units = "in")

#Descriptives for the restricted Dataset for individuals aged >25
cohorts_descr<-c("1950-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994")
descriptives_cohorts2<-data.frame(Cohort=character(), N=numeric(), "Age"=character(), "Years of Education"=character(), "Parent's Years of Education"=character(), check.names = F)
for(i in 1:length(cohorts_descr)){
  coh<-cohorts_descr[i]
  dataset<-dat_baseline_germany25[dat_baseline_germany25$cohort==coh,c("pid","gebjahr","years_of_education",
                                                                   "age_last_survey","cohort", "parents_years_edu")]
  
  dataset<-na.omit(dataset)
  #calculate means and standard deviations of all variables as well as the sample size
  N<-nrow(dataset)
  age<-round(mean(dataset$age_last_survey), digits=2)
  age_sd<-round(sd(dataset$age_last_survey), digits=2)
  years_edu<-round(mean(dataset$years_of_education), digits=2)
  years_edu_sd<-round(sd(dataset$years_of_education), digits=2)
  years_edu_parents<-round(mean(dataset$parents_years_edu), digits=2)
  years_edu_parents_sd<-round(sd(dataset$parents_years_edu), digits=2)
  
  
  # the standard error is added in parenthesis behind the value of each variable
  descriptives_cohorts2[i,]<-NA
  descriptives_cohorts2$Cohort[i]<-coh
  descriptives_cohorts2$N[i]<-N
  descriptives_cohorts2$Age[i]<-paste0(age, " (", age_sd, ")")
  descriptives_cohorts2$`Years of Education` [i]<-paste0(years_edu, " (", years_edu_sd, ")")
  descriptives_cohorts2$`Parent's Years of Education`[i]<-paste0(years_edu_parents, " (", years_edu_parents_sd, ")")
}
#save descriptives
stargazer(descriptives_cohorts2, summary=F, type="html", out="Output Paper/Descriptives/IGC_Descriptives_Cohorts_Germany_restricted25.html", 
          digits=2, rownames = F, digit.separate=0)

descriptives_Ger_numeric3<-describe(dat_baseline_germany25[,c("gebjahr", "years_of_education", "parents_years_edu","f_years_of_education", "m_years_of_education","age_last_survey")])
descriptives_Ger_numeric3<-descriptives_Ger_numeric3[,c(2,3,4,5,8,9)]
descriptives_Ger_numeric3<-as.data.frame(descriptives_Ger_numeric3)
descriptives_Ger_numeric3$Variable<-c("Birth Year", "Years of Education", "Parents Mean Year of Education", "Father Years of Education", "Mother Years of Education", "Age at last Survey")

descriptives_Ger_numeric3<-descriptives_Ger_numeric3[, c(7,1:6)]
colnames(descriptives_Ger_numeric3)<-c("Variable", "N", "Mean", "Std. Dev.", "Median", "Min", "Max")

stargazer(descriptives_Ger_numeric3, summary=F, type="html", out="Output Paper/Descriptives/IGC_Descriptives_numerical_Germany_restricted25.html", 
          rownames = F, digit.separate=0)


################### Education System Characteristics
#Education proxy variables
edu_variables<-c("preschool_exists","preschool_mandatory","primschool_6years","comprehensive_school_exists",
                 "two_tier_secondary","alternatives_abitur")
#categorical variable
edu_variables2<-c("share_abitur_gymnasiums", "mandatory_schooling_years")

dat_states_edu_system<-merge(dat_baseline_states,classification_rel, by=c("gebjahr","family_state"), all.x=T, all.y=F)
education_variables<-c(edu_variables, edu_variables2)

table(dat_states_edu_system$mandatory_schooling_years)
#remove observations for 8 years of mandatory schooling
dat_states_edu_system<-dat_states_edu_system[dat_states_edu_system$mandatory_schooling_years!="8 years",]
dat_states_edu_system$mandatory_schooling_years<-factor(dat_states_edu_system$mandatory_schooling_years, levels=c("9 years", "10 years"))
descriptives_edu_system<-data.frame("Education System Variable"=character(),"Education System Characteristic"=character(), N=numeric(), "Age mean"=numeric(), "Age standard deviation"=numeric(), "Birth Year mean"=numeric(), "Birth Year standard deviation"=numeric(), "Years of Education mean"=numeric(), "Years of Education standard deviation"=numeric(), Number_States=character(), check.names = F)

row<-0
for (i in 1:length(education_variables)){
  edu_var<-education_variables[i]
  data<-dat_states_edu_system[,c("gebjahr", "years_of_education", "age_last_survey", "family_state", edu_var)]
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
#save results
stargazer(descriptives_edu_system, summary=F, type="html", out="Output Paper/Descriptives/IGC_Descriptives_Education_System_Germany.html", 
          digits=2, rownames = F, digit.separate=0)



#Calculate IGE and IGC for each education system classification
education_system_results<-data.frame(Variable=character(), Value=character(), N=numeric(), IGE=numeric(), IGE_std_err=numeric(),IGC=numeric(), IGC_std_err=numeric())
for (edu_var in education_variables){
  dat_lm<-na.omit(dat_states_edu_system[,c("years_of_education", "parents_years_edu", edu_var)])
  new_rows<-data.frame(Variable=gsub("_", " ", edu_var), Value=NA, N=NA, IGE=NA, IGE_std_err=NA,IGC=NA, IGC_std_err=NA)
  for (i in 1:length(unique(dat_lm[,edu_var]))){
    val=as.character(unique(dat_lm[,edu_var])[i])
    dat_lm1<-dat_lm[dat_lm[,edu_var]==val,]
    n=nrow(dat_lm1)
    sd_y<-sd(dat_lm1$years_of_education)
    sd_ypar<-sd(dat_lm1$parents_years_edu)
    lm_out<-lm(years_of_education ~ parents_years_edu, data=dat_lm1)
    ige<-lm_out$coefficients["parents_years_edu"]
    ige_std_err<-summary(lm_out)$coefficients["parents_years_edu",]["Std. Error"]
    sigma_ypar<-sd(dat_lm1[,"parents_years_edu"])
    sigma_y<-sd(dat_lm1[,"years_of_education"])
    igc<-ige*(sigma_ypar/sigma_y)
    igc_std_err<-ige_std_err*(sigma_ypar/sigma_y)
    new_rows[i+1,]<-NA
    new_rows$Variable[i+1]<-""
    new_rows$Value[i+1]<-val
    new_rows[i+1, 3:7]<-c(n,ige,ige_std_err, igc, igc_std_err)
  }
  education_system_results<-rbind(education_system_results,new_rows)
}

for (i in 3:7){
  education_system_results[,i]<-as.character(round(education_system_results[,i],digits=4))
}
education_system_results[is.na(education_system_results)]<-""
stargazer(education_system_results, summary=F, type="html", out="Output Paper/Appendix/IGE_IGC_education_system.html", rownames = F)


#Generate similar table for plots
education_system_results_plot<-data.frame(Variable=character(), Value=character(), N=numeric(), IGE=numeric(), IGE_std_err=numeric(),IGC=numeric(), IGC_std_err=numeric())
for (edu_var in education_variables){
  dat_lm<-na.omit(dat_states_edu_system[,c("years_of_education", "parents_years_edu", edu_var)])
  new_rows<-data.frame(Variable=gsub("_", " ", edu_var), Value=NA, N=NA, IGE=NA, IGE_std_err=NA,IGC=NA, IGC_std_err=NA)
  for (i in 1:length(unique(dat_lm[,edu_var]))){
    val=as.character(unique(dat_lm[,edu_var])[i])
    dat_lm1<-dat_lm[dat_lm[,edu_var]==val,]
    n=nrow(dat_lm1)
    sd_y<-sd(dat_lm1$years_of_education)
    sd_ypar<-sd(dat_lm1$parents_years_edu)
    lm_out<-lm(years_of_education ~ parents_years_edu, data=dat_lm1)
    ige<-lm_out$coefficients["parents_years_edu"]
    ige_std_err<-summary(lm_out)$coefficients["parents_years_edu",]["Std. Error"]
    sigma_ypar<-sd(dat_lm1[,"parents_years_edu"])
    sigma_y<-sd(dat_lm1[,"years_of_education"])
    igc<-ige*(sigma_ypar/sigma_y)
    igc_std_err<-ige_std_err*(sigma_ypar/sigma_y)
    if(i==1){
      new_rows<-data.frame(Variable=gsub("_", "\n", edu_var), Value=val, N=n, IGE=ige, IGE_std_err=ige_std_err,IGC=igc, IGC_std_err=igc_std_err)
    }
    else
    new_rows[i,]<-NA
    new_rows$Variable[i]<-gsub("_", "\n", edu_var)
    new_rows$Value[i]<-val
    new_rows[i, 3:7]<-c(n,ige,ige_std_err, igc, igc_std_err)
  }
  education_system_results_plot<-rbind(education_system_results_plot,new_rows)
}

#x axis labels (Education System Variables) in several rows
education_system_results_plot$education_system<-paste0(education_system_results_plot$Variable, "\n", education_system_results_plot$Value)
#plot Education system Characteristics mandatory years of schooling and share of Abitur that is obtained on Gymnasium
plot_IGC_edu_sys1<-ggplot(education_system_results_plot[education_system_results_plot$Variable%in%c("mandatory\nschooling\nyears", "share\nabitur\ngymnasiums"),], 
                          aes(x= factor(education_system, levels=c("mandatory\nschooling\nyears\n8 years", "mandatory\nschooling\nyears\n9 years","mandatory\nschooling\nyears\n10 years","share\nabitur\ngymnasiums\nDDR","share\nabitur\ngymnasiums\n<60%","share\nabitur\ngymnasiums\n60-80%","share\nabitur\ngymnasiums\n80-95%","share\nabitur\ngymnasiums\n>95%")), IGC)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = IGC-(IGC_std_err*qnorm(0.975)), ymax = IGC+(IGC_std_err*qnorm(0.975)))) + 
  labs(title="Intergenerational Correlation of Years ofEducation", subtitle = "Explanatory Var.: Mean Years of Education Parents", x = "Education System Characteristic", y = "IGC (95% CI)", fill = NULL)

#Plot other Education System characteristics
plot_IGC_edu_sys2<-ggplot(education_system_results_plot[-which(education_system_results_plot$Variable%in%c("mandatory\nschooling\nyears", "share\nabitur\ngymnasiums")),], aes(x= factor(education_system), IGC)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = IGC-(IGC_std_err*qnorm(0.975)), ymax = IGC+(IGC_std_err*qnorm(0.975)))) + 
  labs(title="Intergenerational Correlation of Years of Education", subtitle = "Explanatory Var.: Mean Years of Education Parents", x = "Education System Characteristic", y = "IGC (95% CI)", fill = NULL)

ggsave(filename=paste0("Output Paper/Appendix/", "IGC_education_system1" ,".jpg"), plot=plot_IGC_edu_sys1, width = 10, height = 6, units = "in")
ggsave(filename=paste0("Output Paper/Appendix/", "IGC_education_system2" ,".jpg"), plot=plot_IGC_edu_sys2, width = 10, height = 6, units = "in")


### All Education System characteristics in one regression (IGE)
lm_education_system_characteristics<-lm(years_of_education ~ parents_years_edu + parents_years_edu*preschool_exists + parents_years_edu*preschool_mandatory + parents_years_edu*primschool_6years + parents_years_edu*mandatory_schooling_years + parents_years_edu*comprehensive_school_exists + parents_years_edu*two_tier_secondary + parents_years_edu*share_abitur_gymnasiums + parents_years_edu*alternatives_abitur , data=dat_states_edu_system)
lm_education_system_characteristics_robust_se <- coeftest(lm_education_system_characteristics, vcov=hccm(lm_education_system_characteristics,type="hc0"))[,"Std. Error"]
lm_education_system_characteristics_robust_p <- coeftest(lm_education_system_characteristics, vcov=hccm(lm_education_system_characteristics,type="hc0"))[,"Pr(>|t|)"]
#save regression results
htmlreg(lm_education_system_characteristics, file="Output Paper/IGE_education_system_characteristics_all.html",
        caption="Regression Results", center = F, caption.above = TRUE, 
        omit.coef = c('Intercept'),stars=c(0.01,0.05,0.1), 
        digits=4, include.rs=T, include.adjrs = T, single.row=F,
        override.se=lm_education_system_characteristics_robust_se,
        override.pvalues = lm_education_system_characteristics_robust_p) 

summary(lm_education_system_characteristics)

#add cohort and interaction terms between cohort and parents education to control for time trends
dat_states_edu_system$cohort<-factor(dat_states_edu_system$cohort)
lm_education_system_characteristics2<-lm(years_of_education ~ parents_years_edu + parents_years_edu*preschool_exists + parents_years_edu*preschool_mandatory + parents_years_edu*primschool_6years + parents_years_edu*mandatory_schooling_years + parents_years_edu*comprehensive_school_exists + parents_years_edu*two_tier_secondary + parents_years_edu*share_abitur_gymnasiums + parents_years_edu*alternatives_abitur + parents_years_edu*cohort, data=dat_states_edu_system)
lm_education_system_characteristics2_robust_se <- coeftest(lm_education_system_characteristics2, vcov=hccm(lm_education_system_characteristics2,type="hc0"))[,"Std. Error"]
lm_education_system_characteristics2_robust_p <- coeftest(lm_education_system_characteristics2, vcov=hccm(lm_education_system_characteristics2,type="hc0"))[,"Pr(>|t|)"]

#save regression results
htmlreg(lm_education_system_characteristics2, file="Output Paper/IGE_education_system_characteristics_all2.html",
        caption="Regression Results", center = F, caption.above = TRUE, 
        omit.coef = c('Intercept|cohort'),stars=c(0.01,0.05,0.1), 
        digits=4, include.rs=T, include.adjrs = T, single.row=F,
        override.se=lm_education_system_characteristics2_robust_se,
        override.pvalues = lm_education_system_characteristics2_robust_p) 

#save results of both models
htmlreg(list(lm_education_system_characteristics, lm_education_system_characteristics2), file="Output Paper/IGE_education_system_characteristics_all3.html",
        caption="Regression Results", center = F, caption.above = TRUE, 
        omit.coef = c('Intercept|cohort'),stars=c(0.01,0.05,0.1), 
        digits=4, include.rs=T, include.adjrs = T, single.row=T,
        override.se=list(lm_education_system_characteristics_robust_se,lm_education_system_characteristics2_robust_se),
        override.pvalues = list(lm_education_system_characteristics_robust_p,lm_education_system_characteristics2_robust_p),
        custom.gof.rows=list("Cohort and Interaction Between Cohort and Parents Education as Control" = c("No","Yes")) )
