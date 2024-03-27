
#script to add two column with the cohorts (5 and 10-year cohorts) to a dataset based on the variable birth year (geb_jahr)
get_cohort<- function (dataset){
  dataset$cohort<-rep(NA, nrow(dataset))  
  dataset$cohort2<-rep(NA, nrow(dataset))
  for (row in 1:nrow(dataset)){
    geb_jahr<-dataset$gebjahr[row]
    cohort=NA
    cohort2=NA
    if(geb_jahr < 1950) {
      cohort="<1950"
      cohort2="<1950"
    } else {
      if(geb_jahr < 1960) {
        cohort="1950-1959"
        cohort2="1950-1959"
      } else {
        if(geb_jahr < 1965) {
          cohort="1960-1964"
          cohort2="1960-1969"
        } else {
          if(geb_jahr < 1970) {
            cohort="1965-1969"
            cohort2="1960-1969"
          } else {
            if(geb_jahr < 1975) {
              cohort="1970-1974"
              cohort2="1970-1979"
            } else {
              if(geb_jahr < 1980) {
                cohort="1975-1979"
                cohort2="1970-1979"
              } else {
                if(geb_jahr < 1985) {
                  cohort="1980-1984"
                  cohort2="1980-1989"
                } else {
                  if(geb_jahr < 1990) {
                    cohort="1985-1989"
                    cohort2="1980-1989"
                  } else {
                    if(geb_jahr < 1995) {
                      cohort="1990-1994"
                      cohort2="1990-1999"
                    } else {
                      if(geb_jahr < 2000) {
                        cohort="1995-1999"
                        cohort2="1990-1999"
                      } else {
                        cohort=">2000"
                        cohort2=">2000"
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    dataset$cohort[row]<-cohort
    dataset$cohort2[row]<-cohort2
  }
  return(dataset)
}

#function to calculate the intergenerational correlation coefficients (from the intergeneratioanl elasticity)
#CI=T confidence intervalls are reported
#lm_result: results from IGE regression
#parent indicates whether father, morther or education of both parents should be used
#cohort indicates if dataset is only a specific cohort or all
#population indicates whether a the populaton of a state or for whole Germany is used
calculate_IGC<-function (lm_result, dataset, parent="Father", cohort="all", population="all", return_CI=F){
  if (return_CI==T) {
    table_out<-data.frame(population=as.character(NA),cohort=as.character(NA), Parent=as.character(NA), N=as.numeric(NA), IGE=as.numeric(NA),std_error_IGE=as.numeric(NA), IGC=as.numeric(NA), std_error_IGC=as.numeric(NA), CI_IGE_lower=as.numeric(NA), CI_IGE_upper=as.numeric(NA), CI_IGC_lower=as.numeric(NA), CI_IGC_upper=as.numeric(NA) , R2=as.numeric(NA), R2_adj=as.numeric(NA))
  } else {
    table_out<-data.frame(population=as.character(NA),cohort=as.character(NA), Parent=as.character(NA), N=as.numeric(NA), IGE=as.character(NA), IGC=as.character(NA) , R2=as.numeric(NA), R2_adj=as.numeric(NA))
    
  }
  if (population %in% c("all", "Germany", "germany")){
    table_out$population[1]="Germany"
  } else {
    table_out[1]=population
  }
  table_out$cohort[1]=cohort
  if (parent %in% c("Father", "father")){
    table_out$Parent[1]="Father"
    dataset<-na.omit(dataset[,c("pid", "years_of_education", "f_years_of_education")])
    ige<-lm_result$coefficients["f_years_of_education"]
    stderror<-summary(lm_result)$coefficients["f_years_of_education",]["Std. Error"]
    CI<-confint(lm_result, "f_years_of_education", level=0.95)
    sigma_p<-sd(dataset[,"f_years_of_education"])
    
  } else {
    if (parent %in% c("Mother", "mother") ) {
      table_out$Parent[1]="Mother"
      dataset<-na.omit(dataset[,c("pid", "years_of_education", "m_years_of_education")])
      ige<-lm_result$coefficients["m_years_of_education"]
      stderror<-summary(lm_result)$coefficients["m_years_of_education",]["Std. Error"]
      CI<-confint(lm_result, "m_years_of_education", level=0.95)
      sigma_p<-sd(dataset[,"m_years_of_education"])
    } else {
      table_out$Parent[1]="both"
      dataset<-na.omit(dataset[,c("pid", "years_of_education", "parents_years_edu")])
      ige<-lm_result$coefficients["parents_years_edu"]
      stderror<-summary(lm_result)$coefficients["parents_years_edu",]["Std. Error"]
      CI<-confint(lm_result, "parents_years_edu", level=0.95)
      sigma_p<-sd(dataset[,"parents_years_edu"])
    }
  }
  
  sigma_c<-sd(dataset$years_of_education)
  
  table_out$N[1]<-nrow(dataset)
  
  igc<-ige*(sigma_p/sigma_c)
  CI_igc<-CI*(sigma_p/sigma_c)
  stderror_igc<-stderror*(sigma_p/sigma_c)
  if (return_CI==T){
    table_out$IGE[1]<-as.numeric(round(ige, digits=4))
    table_out$std_error_IGE[1]<-round(stderror, digits=4)
    table_out$CI_IGE_lower[1]<-round(CI[1], digits=4)
    table_out$CI_IGE_upper[1]<-round(CI[2], digits=4)
    table_out$IGC[1]<-as.numeric(round(igc, digits=4))
    table_out$std_error_IGC[1]<-round(stderror_igc, digits=4)
    table_out$CI_IGC_lower[1]<-round(CI_igc[1], digits=4)
    table_out$CI_IGC_upper[1]<-round(CI_igc[2], digits=4)
  } else {
    table_out$IGE[1]<-paste0(round(ige, digits=4), " (",round(stderror, digits=4), ")")
    table_out$IGC[1]<-paste0(round(igc, digits=4), " (",round(stderror_igc, digits=4), ")")
  }
  table_out$R2[1]<-round(summary(lm_result)$r.squared, digits=4)
  table_out$R2_adj[1]<-round(summary(lm_result)$adj.r.squared, digits=4)
  
  
  return(table_out)
}

