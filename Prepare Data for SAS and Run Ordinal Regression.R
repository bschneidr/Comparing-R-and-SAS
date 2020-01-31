library(dplyr)
library(survey)
library(srvyr)

# Set up example data ----

  data(api)

  rm(list = setdiff(ls(), 'apistrat'))
  
  ##_ Add an ordinal dependent variable based on percent of parents with college degree
  
  apistrat <- apistrat %>%
    mutate(
      Parent_Col_Grad_Category = case_when(
        col.grad < 10 ~ 1,
        col.grad < 20 ~ 2,
        col.grad < 30 ~ 3,
        TRUE ~ 4
      )
    )
  
  ##_ Create a character version of school type ----
  
    apistrat <- apistrat %>%
      mutate(School_Type = as.character(stype))

  ##_ Create simple stratified survey design object ----
    stratified_design <- apistrat %>%
      as_survey_design(strata = stype, weights = pw)
  
  ##_ Output the data to a sav file ----
    
    haven::write_sav(
      data = apistrat,
      path = "apistrat_data.sav"
    )
  
# Estimate an ordinal logistic regression model ----
  
  ordinal_model <- svyolr(
    formula = Parent_Col_Grad_Category ~ School_Type,
    design = stratified_design %>%
      mutate(
        Parent_Col_Grad_Category = factor(Parent_Col_Grad_Category, levels = 1:4)
      )
  )
  
  summary(ordinal_model)
  
  broom::tidy(
    ordinal_model
  )
  
  regTermTest(
    model = ordinal_model,
    test.terms = ~ School_Type,
    method = 'Wald', lrt.approximation = 'satterthwaite'
  )
    
# Estimate a contingency table, with cell design effects ----
  
  stratified_design %>%
    group_by(stype, awards) %>%
    summarize(
      Percent = survey_mean(vartype = 'se', deff = TRUE)
    )
  