#########Build SOEP Dataset

libraries = c("haven","rstudioapi", "readxl", "xlsx", "readr", "readstata13", "ggplot2", "ggthemes", "dplyr", "tidyverse", "stargazer", "texreg") #, "spatstat", "Hmisc", "haven","foreign",
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


#Get citations
lapply(libraries, citation)
#(de-)activate areas of code
load_raw_dataset=T #(15 minutes)
build_main_dataset=T #(30 minutes)
build_parent_dataset=T #(20 minutes)
build_sibling_family_dataset=T #(5minutes)

######Insert path to SOEP Stata Datasets
path_soep<-"Y:/XXXX/XXXX/XXXX"






#set working directory to location of script
setwd(dirname(getSourceEditorContext()$path))

if(load_raw_dataset==T){
  
  #load relevant dataset
  hgen<-read.dta13(paste(path_soep,"hgen.dta", sep="/"))
  pgen<-read.dta13(paste(path_soep,"pgen.dta", sep="/"))
  ppath<-read.dta13(paste(path_soep,"ppath.dta", sep="/"))
  bioparen<-read.dta13(paste(path_soep,"bioparen.dta", sep="/"))
  #pbrutto<-read.dta13(paste(path_soep,"pbrutto.dta", sep="/"))
  
  #Extract relevant variables and build a dataset for each person and year and a dataset with metadata for a person.
  h_data1<-hgen[,c("hid", "syear", "hgtyp1hh", "hgtyp2hh", "hgnuts1")]
  h_data1[h_data1<0]<-NA
  p_data1<-pgen[,c("hid", "pid", "syear","pgmonth","pgpsbil","pgpsbilo", "pgpbbilo", "pgpsbila", "pgpbbila", "pgpbbil01", "pgpbbil02", "pgpbbil03", "pgbilzeit")]
  p_data1[p_data1<0]<-NA
  p_meta1<-ppath[, c("pid", "sex", "gebjahr", "gebmonat", "eintritt", "erstbefr", "austritt","letztbef", "loc1989", "todjahr", "immiyear", "migback", "corigin", "birthregion","arefback")]
  p_meta1[p_meta1<0]<-NA
  p_meta2<-bioparen[,c("pid", "fnr", "mnr", "fsedu", "msedu", "fprofedu", "mprofedu", "locchild1", "fnat", "mnat", "morigin", "forigin")]
  p_meta2[p_meta2<0]<-NA
  p_meta2[p_meta2==0]<-NA
  p_meta1$age_migration<-p_meta1$immiyear-p_meta1$gebjahr
  data_meta<-merge(p_meta1, p_meta2, by=c("pid"), all.x = T, all.y=F)
  data<-merge(p_data1, h_data1, by=c("hid", "syear"), all.x = T, all.y=F)
  data<-merge(data, data_meta[,c("pid", "gebjahr", "gebmonat")], by=c("pid"), all.x=T, all.y = F)
  
  
  
  #calculate the age of the person during the survey.
  data$age_survey<-data$syear-data$gebjahr
  
  
  #Calculate the age at the first and last time an individual was surveyed
  data_meta$age_first_survey<-rep(NA, nrow(data_meta))
  data_meta$age_last_survey<-rep(NA, nrow(data_meta))
  for(i in 1:nrow(data_meta)){
    if( i/10000==round(i/10000)) print(paste0("progress:",i/nrow(data_meta)*100, "%"))
    pid<-data_meta$pid[i]
    if(sum(data$pid==pid)>0) {
      ages<-data$age_survey[data$pid==pid]
      data_meta$age_first_survey[i]<-min(ages, na.rm = T)
      data_meta$age_last_survey[i]<-max(ages, na.rm = T)
    } else {
      data_meta$age_first_survey[i]<-NA
      data_meta$age_last_survey[i]<-NA
      
    }
  }
  
  write_excel_csv2(data, "Datasets/p_data.csv")
  write_excel_csv2(data_meta, "Datasets/p_meta_data.csv")
  
}





#Build main dataset which transforms ongitudinal data for Bundesland and Years of Education to Cross-Sectional Data
if (build_main_dataset==T){
  data<-read.csv("Datasets/p_data.csv", sep=";", dec=",", check.names = F)
  data_meta<-read.csv("Datasets/p_meta_data.csv", sep=";", dec=",", check.names = F)
  #Remove all individuals who left the survey before the age of 21
  data_analysis<-data_meta[which(data_meta$age_last_survey>20),c("pid","sex","gebjahr","gebmonat", "loc1989", "migback", "age_migration", "birthregion","arefback","locchild1","fnr","mnr","age_first_survey","age_last_survey")]
  #Survey Years
  data_analysis$syears<-rep(NA, nrow(data_analysis))
  #Household ids
  data_analysis$hids<-rep(NA, nrow(data_analysis))
  #Years of education
  data_analysis$years_of_education<-rep(NA, nrow(data_analysis))
  #Various state IDs
  data_analysis$bundesland<-rep(NA, nrow(data_analysis))
  data_analysis$bundesland_time<-rep(NA, nrow(data_analysis))
  data_analysis$bundesland_age_0_20<-rep(NA, nrow(data_analysis))
  data_analysis$bundesland_change_0_18<-rep(NA, nrow(data_analysis))
  for (i in 1:nrow(data_analysis)){
    if( i/1000==round(i/1000)) print(paste0("progress:",i/nrow(data_analysis)*100, "%"))
    bundesland<-NA
    bundesland_age_0_20<-NA
    bundesland_change_0_18<-NA
    bundesland_time<-NA
    syears<-NA
    pid<-data_analysis$pid[i]
    data_person<-data[data$pid==pid,]
    data_person<-data_person[order(data_person$syear, decreasing = F), ]
    
    #Use the highest years of education a person reported in the survey
    if (length(na.omit(data_person$pgbilzeit))>0) data_analysis$years_of_education[i]<-max(data_person$pgbilzeit, na.rm = T)
  
    #Paste all HH-Ids in one string and all survey years in one string
    data_analysis$hids[i]<-paste0(data_person$hid[-which(is.na(data_person$hgnuts1))], collapse = "_")
    
    syears<-data_person$syear
    if( any(is.na(data_person$hgnuts1))) syears<-syears[-which(is.na(data_person$hgnuts1))]
    data_analysis$syears[i]<-paste0(syears, collapse = "_")
    #vector of all states a person's household lived in the years a person was surveyed
    bundesland<-data_person$hgnuts1
    bundesland<-na.omit(bundesland)
    #save all states a person lived in
    if(length(bundesland)>0) data_analysis$bundesland_time[i]<-paste(bundesland, sep="_", collapse="_")
    bundesland_all_obs<-bundesland
    bundesland<-unique(bundesland)
    if(length(bundesland)>0){
      #Paste all States in one string (if there are several and assign it to bundesland
      if (length(bundesland)>1) bundesland<-paste(bundesland, sep="_", collapse="_")
      if (length(bundesland)==1) data_analysis$bundesland[i]<-bundesland
      
      #identify the state a person lived in before the age of 20 if a person entered the survey before the age of 20. 
      #If there is a unique state for the years before a person was 20 (or 19 or 18), we assign it to bundesland_age_0_20
      #If there are several states for the years a person is between 0 and 18 years old, we paste them to one string and assign them. Additionally we assign bundesland_change_0_18 a TRUE.
      bula_child<-NA
      bula_20<-bundesland_all_obs[syears %in% data_analysis$gebjahr[i]:(data_analysis$gebjahr[i]+20)]
      if (length(bula_20)>0){
        if (length(unique(bula_20))==1) bula_child=unique(bula_20) else {
          bula_19<-na.omit(bundesland_all_obs[syears %in% data_analysis$gebjahr[i]:(data_analysis$gebjahr[i]+19)])
          if (length(unique(bula_19))==1) bula_child=unique(bula_19) else {
            bula_18<-na.omit(bundesland_all_obs[syears %in% data_analysis$gebjahr[i]:(data_analysis$gebjahr[i]+18)])
            if (length(unique(bula_18))==1) bula_child=unique(bula_18) else {
              bula_child<-NA
              bundesland_change_0_18=T
            }
          }
        }
      } else {
        bula_child<-NA
      }
      
      data_analysis$bundesland_age_0_20[i]<-bula_child
      data_analysis$bundesland_change_0_18[i]<-bundesland_change_0_18
    } else {
      data_analysis$bundesland[i]<-NA
      data_analysis$bundesland_change_0_18[i]<-NA
      data_analysis$bundesland_age_0_20[i]<-NA
    }
    
  }
  data_analysis$bundesland_eintritt<-rep(NA, nrow(data_analysis))
  for (row in 1:nrow(data_analysis)){
    data_analysis$bundesland_eintritt[row]<-unlist(str_split(data_analysis$bundesland[row], "_"))[1]
  }
  
  write_excel_csv2(data_analysis, "Datasets/data_analysis_step1.csv")
}


#############Estimate years of education of parents and state during childhood through parents


if (build_parent_dataset==T){
  data_analysis<-read.csv("Datasets/data_analysis_step1.csv", sep=";", dec=",")
  data<-read.csv("Datasets/p_data.csv", sep=";", dec=",", check.names = F)

  
  #add years of education of father and mother to dataset
  dat_ms_years<-data_analysis[,c("pid", "years_of_education")]
  dat_fs_years<-data_analysis[,c("pid", "years_of_education")]
  colnames(dat_ms_years)<-c("mnr", "m_years_of_education")
  colnames(dat_fs_years)<-c("fnr", "f_years_of_education")
  data_analysis<-merge(data_analysis, dat_ms_years, by="mnr", all.x=T, all.y=F)
  data_analysis<-merge(data_analysis, dat_fs_years, by="fnr", all.x=T, all.y=F)
  #estimate the bundesland in which the parents live or lived when the person was a child
  #colnames(data_analysis)
  data_bundesland_parents<-data_analysis[-which(is.na(data_analysis$fnr)& is.na(data_analysis$mnr)) ,c("pid", "gebjahr", "bundesland", "bundesland_age_0_20", "fnr", "mnr", "bundesland_change_0_18", "bundesland_eintritt", "age_first_survey", "age_last_survey")]
  
  
  #estimate  State of childhood for individuals where only information about mother is available
  data_bundesland_parents_only_mother<-data_bundesland_parents[which(is.na(data_bundesland_parents$fnr)),]
  data_bundesland_parents_only_mother$bundesland_childhood<-rep(NA, nrow(data_bundesland_parents_only_mother))
  data_bundesland_parents_only_mother$bundesland_shared_parents<-rep(NA, nrow(data_bundesland_parents_only_mother))
  data_bundesland_parents_only_mother$bundesland_childhood_change<-rep(NA, nrow(data_bundesland_parents_only_mother))
  
  for(i in 1:nrow(data_bundesland_parents_only_mother)){
    if( i/10000==round(i/10000)) print(paste0("progress:",i/nrow(data_bundesland_parents_only_mother)*100, "%"))
    bundesland_childhood<-NA
    bundesland_shared_parents<-NA
    bundesland_childhood_change<-NA
    #get the birth year and the childhood years
    dat_person<-data_bundesland_parents_only_mother[i,]
    gebjahr<-dat_person$gebjahr
    childhood_years<-gebjahr:(gebjahr+19)
    #get the data for the parents, order it by survey years and remove years where the state is missing
    m_pid<-dat_person$mnr
    dat_mother<-data[data$pid==m_pid,]
    dat_mother<-dat_mother[order(dat_mother$syear, decreasing = F),]
    #remove survey years where bundesland is missing
    index_rm<-which(is.na(dat_mother$hgnuts1))
    if(length(index_rm)>0)dat_mother<-dat_mother[-index_rm,]
    syears_mother<-dat_mother$syear
    
    bundesland_mother<-dat_mother$hgnuts1
    bundesland_mother<-bundesland_mother[syears_mother>=gebjahr]
    syears_mother<-syears_mother[syears_mother>=gebjahr]
    eintritt_bundesland_mother<-bundesland_mother[1]
    
    if ( !is.na(dat_person$bundesland) & length(bundesland_mother)>0) {
      #If we know where the parent lived during childhood, we use that information
      if (sum(syears_mother %in% childhood_years)>0)bundesland_childhood<-unique(dat_mother$hgnuts1[dat_mother$syear %in% childhood_years]) else bundesland_childhood<-NA
      #If the parent lived in several states, we assign a missing value NA
      if(length(bundesland_childhood)>1){
        bundesland_childhood_change=T
        bundesland_childhood<-NA
        bundesland_shared_parents<-NA
        
      } else {
        #if no information about the state during childhood is available, 
        #we check whether the state a person lived in is the same state when it entered the survey,
        # as the state where parent lived in when it entered the survey
        if(length(na.omit(bundesland_childhood))==1){
          bundesland_shared_parents<-bundesland_childhood
        } else {
          if(dat_person$bundesland_eintritt==eintritt_bundesland_mother){
            bundesland_shared_parents<-eintritt_bundesland_mother
          } else {
            bundesland_shared_parents<-NA
          }
        }
      }
      
      data_bundesland_parents_only_mother$bundesland_childhood[i]<-bundesland_childhood
      data_bundesland_parents_only_mother$bundesland_shared_parents[i]<-bundesland_shared_parents
      data_bundesland_parents_only_mother$bundesland_childhood_change[i]<-bundesland_childhood_change
      
    }
  }
  
  #estimate State of childhood for individuals where only information about father is available
  data_bundesland_parents_only_father<-data_bundesland_parents[which(is.na(data_bundesland_parents$mnr)),]
  data_bundesland_parents_only_father$bundesland_childhood<-rep(NA, nrow(data_bundesland_parents_only_father))
  data_bundesland_parents_only_father$bundesland_shared_parents<-rep(NA, nrow(data_bundesland_parents_only_father))
  data_bundesland_parents_only_father$bundesland_childhood_change<-rep(NA, nrow(data_bundesland_parents_only_father))
  
  for(i in 1:nrow(data_bundesland_parents_only_father)){
    if( i/10000==round(i/10000)) print(paste0("progress:",i/nrow(data_bundesland_parents_only_father)*100, "%"))
    bundesland_childhood<-NA
    bundesland_shared_parents<-NA
    bundesland_childhood_change<-NA
    #get the birth year and the childhood years
    dat_person<-data_bundesland_parents_only_father[i,]
    gebjahr<-dat_person$gebjahr
    childhood_years<-gebjahr:(gebjahr+19)
    #get the data for the parents, order it by survey years and remove years where the state is missing
    f_pid<-dat_person$fnr
    dat_father<-data[data$pid==f_pid,]
    dat_father<-dat_father[order(dat_father$syear, decreasing = F),]
    #remove survey years where bundesland is missing
    index_rm<-which(is.na(dat_father$hgnuts1))
    if(length(index_rm)>0)dat_father<-dat_father[-index_rm,]
    #Estimate where the parents lived, when the person was a child (if possible) and where the parents lived in the first year surveyed (after the childhood of the person)
    syears_father<-dat_father$syear
    bundesland_father<-dat_father$hgnuts1
    bundesland_father<-bundesland_father[syears_father>=gebjahr]
    syears_father<-syears_father[syears_father>=gebjahr]
    eintritt_bundesland_father<-bundesland_father[1]
    
    if ( !is.na(dat_person$bundesland) & length(bundesland_father)>0) {
      #If we know where the parent lived during childhood, we use that information
      if (sum(syears_father %in% childhood_years)>0)bundesland_childhood<-unique(dat_father$hgnuts1[dat_father$syear %in% childhood_years]) else bundesland_childhood<-NA
      #If the parent lived in several states, we assign a missing value NA
      if(length(bundesland_childhood)>1){
        bundesland_childhood_change=T
        bundesland_childhood<-NA
        bundesland_shared_parents<-NA
        
      } else {
        #if no information about the state during childhood is available, 
        #we check whether the state a person lived in is the same state when it entered the survey,
        # as the state where parent lived in when it entered the survey
        if(length(na.omit(bundesland_childhood))==1){
          bundesland_shared_parents<-bundesland_childhood
        } else {
          if(dat_person$bundesland_eintritt==eintritt_bundesland_father){
            bundesland_shared_parents<-eintritt_bundesland_father 
          } else {
            bundesland_shared_parents<-NA
          }
        }
      }
      data_bundesland_parents_only_father$bundesland_childhood[i]<-bundesland_childhood
      data_bundesland_parents_only_father$bundesland_shared_parents[i]<-bundesland_shared_parents
      data_bundesland_parents_only_father$bundesland_childhood_change[i]<-bundesland_childhood_change
      
    }
  }
  
  #estimate State of childhoold for individuals where information for both parents is is available
  data_bundesland_parents_both<-data_bundesland_parents[-which(is.na(data_bundesland_parents$mnr)|is.na(data_bundesland_parents$fnr)),]
  data_bundesland_parents_both$bundesland_childhood<-rep(NA, nrow(data_bundesland_parents_both))
  data_bundesland_parents_both$bundesland_shared_parents<-rep(NA, nrow(data_bundesland_parents_both))
  data_bundesland_parents_both$bundesland_childhood_change<-rep(NA, nrow(data_bundesland_parents_both))
  
  for(i in 1:nrow(data_bundesland_parents_both)){
    if( i/10000==round(i/10000)) print(paste0("progress:",i/nrow(data_bundesland_parents_both)*100, "%"))
    bundesland_childhood<-NA
    bundesland_shared_parents<-NA
    bundesland_childhood_change<-NA
    #get the birth year, the childhood years and the state a person lived in, when it entered the survey
    dat_person<-data_bundesland_parents_both[i,]
    gebjahr<-dat_person$gebjahr
    childhood_years<-gebjahr:(gebjahr+19)
    bundesland_eintritt<-dat_person$bundesland_eintritt
    #get the data for the parents, order it by survey years and remove years where the state is missing
    f_pid<-dat_person$fnr
    dat_father<-data[data$pid==f_pid,]
    dat_father<-dat_father[order(dat_father$syear, decreasing = F),]
    index_rm<-which(is.na(dat_father$hgnuts1))
    if(length(index_rm)>0)dat_father<-dat_father[-index_rm,]
    m_pid<-dat_person$mnr
    dat_mother<-data[data$pid==m_pid,]
    dat_mother<-dat_mother[order(dat_mother$syear, decreasing = F),]
    index_rm<-which(is.na(dat_mother$hgnuts1))
    if(length(index_rm)>0)dat_mother<-dat_mother[-index_rm,]
    
    #Estimate where the parents lived, when the person was a child (if possible) and where the parents lived in the first year surveyed (after the childhood of the person)
    m_state_childhood<-unique(dat_mother$hgnuts1[dat_mother$syear%in%childhood_years])
    f_state_childhood<-unique(dat_father$hgnuts1[dat_father$syear%in%childhood_years])
    m_state_after_childhood<-unique(dat_mother$hgnuts1[dat_mother$syear>(gebjahr+19)])[1]
    f_state_after_childhood<-unique(dat_father$hgnuts1[dat_father$syear>(gebjahr+19)])[1]
  
    bundesländer_fam_initial_after_childhood<-unique(m_state_after_childhood, f_state_after_childhood)
    bundesländer_fam_childhood<-na.omit(unique(c(dat_mother$hgnuts1[dat_mother$syear%in%childhood_years], dat_father$hgnuts1[dat_father$syear%in%childhood_years])))
    #case1: we know where the parents lived, when the person was a child we use that information 
    if(!is.na(bundesland_eintritt)&length(c(bundesländer_fam_initial_after_childhood, bundesländer_fam_childhood))>0 ){
      if(length(m_state_childhood)>0 | length(f_state_childhood)>0){
        #if both parents lived in the same state during the childhood, we assume that this is the state where the person lived in as a child
        if(length(bundesländer_fam_childhood)==1) bundesland_childhood = m_state_childhood
        #if we have information for just one of the parents for the childhood, we assume that this is the state where the person lived in as a child
        if((length(m_state_childhood)==0 & length(f_state_childhood)==1) |(length(m_state_childhood)==1 & length(f_state_childhood)==0)) bundesland_childhood <- bundesländer_fam_childhood
        # if both parents lived in several states during childhood, assign a missing value NA and set bundesland change childhood to true
        if((length(m_state_childhood)>1 & length(f_state_childhood)>1)) {
          bundesland_childhood <- NA
          bundesland_childhood_change=T
        }
        # If one of the parents changed the state and one didn't, we check if there is an intersection between the states the parents lived in, the state the person lived in, when it entered the survey
        if((length(m_state_childhood)==1 & length(f_state_childhood)>1) | (length(m_state_childhood)>1 & length(f_state_childhood)==1)) {
          if (length(intersect(bundesländer_fam_childhood, bundesland_eintritt))>0) bundesland_childhood<-intersect(bundesländer_fam_childhood, bundesland_eintritt)
        }
        bundesland_shared_parents<-bundesland_childhood
      } else {
        #Case 2: If we don't have information where the parents lived during the childhood of the person,
        #we check whether there is an intersection between the state a person lived in, when it entered the survey, 
        #and the states where the parents lived when they were first surveyed (after childhood)
        if (length(intersect(bundesländer_fam_initial_after_childhood, bundesland_eintritt))>0) bundesland_shared_parents<-intersect(bundesländer_fam_initial_after_childhood, bundesland_eintritt)
      }
      data_bundesland_parents_both$bundesland_childhood[i]<-bundesland_childhood
      data_bundesland_parents_both$bundesland_shared_parents[i]<-bundesland_shared_parents
      data_bundesland_parents_both$bundesland_childhood_change[i]<-bundesland_childhood_change
    }
  }
  #join the three datasets
  data_bundesland_parents_new<-rbind(data_bundesland_parents_only_mother, data_bundesland_parents_only_father)
  data_bundesland_parents_new<-rbind(data_bundesland_parents_new, data_bundesland_parents_both)
  #add the new columns to the main dataset
  data_analysis<-merge(data_analysis, data_bundesland_parents_new[,c("pid", "bundesland_childhood", "bundesland_shared_parents", "bundesland_childhood_change")], all.x=T, all.y=F)
  write_excel_csv2(data_analysis, "Datasets/data_analysis_final.csv")
  
}




#build sibling dataset
if (build_sibling_family_dataset==T){
  data_analysis<-read.csv("Datasets/data_analysis_final.csv", sep=";", dec=",")
  biosib<-read.dta13(paste(path_soep,"biosib.dta", sep="/"))
  #sibling_dataset<-data.frame(family_id=as.numeric(), pid=as.numeric(), gebjahr=as.numeric(), sex=as.character())
  #restrictive dataset with all siblings that are full siblings (they share both parents)
  sibling_dataset_restrictive<-data.frame(family_id=as.numeric(), pid=as.numeric(), gebjahr=as.numeric(), sex=as.character())
  storage<-data.frame(family_id=as.numeric(), pid=as.numeric(), gebjahr=as.numeric(), sex=as.character())
  
  for (i in 1:nrow(biosib)){
    n_sibs<-biosib$num_sib[i]
    family_id<-biosib$famcount[i]
    for (j in 1:min(n_sibs,12)){
      if(j==1) {
        #get the persons id, gender and birth year
        pid=biosib[i, "pid"]
        sex=biosib[i,"sex"]
        gebjahr=biosib[i,"gebjahr"]
        #check if person has any full siblings
        any_full_sibs<-any(biosib[i,str_detect(colnames(biosib), "sibdef")]==1)
        #add sibling with family id to dataset
        #sibling_dataset<-rbind(sibling_dataset, new_row)
        #add sibling with family id to restrictive dataset if he has any full siblings
        if(any_full_sibs){
          new_row<-data.frame(family_id=as.numeric(family_id), pid=as.numeric(pid), gebjahr=as.numeric(gebjahr), sex=as.character(sex))
          storage<-rbind(storage, new_row)
        }
      } else {
        #get the id, sex and birth year of the siblings
        pid=biosib[i, paste0("sibpnr", j-1)]
        sex=biosib[i,paste0("sexsib", j-1)]
        gebjahr=biosib[i,paste0("gebsib", j-1)]
        #check if the sibling iss a full (biological) sibling
        if(biosib[i,paste0("sibdef", j-1)]==1) full_sib<-T else full_sib<-F
        #add sibling to the dataset
        #sibling_dataset[nrow(sibling_dataset)+1,]<-rbind(sibling_dataset, new_row)
        #add sibling with family id to restrictive dataset if he is a full sibling of the person
        if(full_sib){
          new_row<-data.frame(family_id=as.numeric(family_id), pid=as.numeric(pid), gebjahr=as.numeric(gebjahr), sex=as.character(sex))
          storage<-rbind(storage, new_row)
        }  
      }
      
    }
    #use a storage dataset, so that in each loop only a smaller dataset has to be saved to decrease the run-time of the code. 
    if( i/10000==round(i/10000)){
      print(paste0("progress:",i/nrow(biosib)*100, "%"))
      sibling_dataset_restrictive<-rbind(sibling_dataset_restrictive,storage)
      storage<-data.frame(family_id=as.numeric(), pid=as.numeric(), gebjahr=as.numeric(), sex=as.character())
    }
  }
  
  #these steps are not necessary for the restricted sample where only full siblings are used
  sibling_dataset_restrictive<-sibling_dataset_restrictive %>% distinct
  
  write_excel_csv2(sibling_dataset_restrictive, "Datasets/sibling_dataset_restrictive.csv")
}

