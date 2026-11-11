library(srvyr)
library(dplyr)
library(ggplot2)
library(modelsummary)
library(tidyverse)
#############################################################################################
load("design.RData")  ###load your data
###########################################################################################
coef_map <- c(     #Customization for regression outputs
  "D_Male1"                = "Gender: male",
  "Age"                    = "Age",
  "Age_z"                  = "Age",  #z-standardization
  "D_old50"                = "Germans above 50 y.",
  "Edu_grp_allLSD"         = "Lower secondary degree",
  "Edu_grp_allUSD"         = "Upper secondary degree",
  "Edu_grp_allUD"          = "Higher education level",
  "Inc_grpmid"             = "Income 2,000-3,999€",
  "Inc_grphigh"            = "Income ≥ 4.000",
  "Inc_equiv_z"            = "Equiv. income",  #z-standardization
  "AmountCig"              = "Amount cigarettes",
  "Cig_z"                  = "Amount cigarettes", #z-standardization
  "Inc_equiv_z × Cig_z"    = "stand. Income × amount cigarettes ",
  "D_Current_new"          = "New products",
  "D_Border_east"          = "Nearby east border",
  "D_Border_west"          = "Nearby west border",
  "D_Ost"                  = "East Germany",
  "D_Ost_wo_berlin"        = "East Germany wo Berlin",
  "D_Ost_up50"             = "East Germans above 50 y.",
  "D_old50:D_Ost"          = "East Germans × older than 50 y.",
  "TaxFreedom_w"           = "Restriction of freedom",
  "Freedom_z"              = "Restriction of freedom", #z-standardization
  "D_Right"                = "AfD voter",
  "D_Left"                 = "Left Party voter",
  "D_Mid"                  = "Union voter",
  "D_Anti"                 = "Anti-establishment",
  "D_other_voter"          = "NA voter",
  "Inc_grpmid:AmountCig"   = "Middle income × Amount cigarettes",
  "Inc_grphigh:AmountCig"  = "High income × cigarettes",
  "Inc_grpmid:Cig_z"   = "Middle income × stand. amount cigarettes",
  "Inc_grphigh:Cig_z"  = "High income × stand. amount cigarettes",
  "Inc_grpmid:Edu_grp_allUSD"   = "Middle income × Upper secondary degree",
  "Inc_grphigh:Edu_grp_allUSD"   = "High income × Upper secondary degree",
  "Inc_grpmid:Edu_grp_allUD"= "Middle income × Higher education level",
  "Inc_grphigh:Edu_grp_allUD"= "High income × Higher education level"
)

###################################################################################################
### creates a srvyr-object to use dplyr-friendly functions (if needed)
df.srvyr <- as_survey_design(design)

###################################################################################################
#preparation
df_D$D_Right <- factor(df_D$D_Right)
df_D$D_Left <- factor(df_D$D_Left)
df_D$D_Ost <- factor(df_D$D_Ost)
df_D$D_Current_new <- factor(df_D$D_Current_new)
df_D$D_Opportunities <- factor(df_D$D_Opportunities)


#### Analysis ####
fit_svy <- function(f) {
  survey::svyglm(
    formula = f,
    design  = design,
    family  = quasibinomial()
  )
}

f_list_illegalaccess <- list(
  reg1_access     = D_IllegalAccess ~ Age_z + D_Male + Inc_equiv_z + Cig_z + Edu_grp_all + D_Current_new + D_Border_west + D_Border_east + D_Ost + Freedom_z + D_Right, #Tab. 2
  reg2_access     = D_IllegalAccess ~ Age_z + D_Male + Inc_equiv_z + Edu_grp_all + Cig_z + D_Current_new + D_Border_west + D_Border_east + D_Ost + D_Anti, #Tab. 3
  reg3_access     = D_IllegalAccess ~ Age_z + D_Male + Inc_equiv_z + Cig_z + Edu_grp_all + D_Current_new + D_Border_west + D_Border_east + Freedom_z + D_Right, #Tab. A1
  reg4_access     = D_IllegalAccess ~ Age_z + D_Male + Inc_equiv_z + Cig_z + Edu_grp_all + D_Current_new + D_Border_west + D_Ost + Freedom_z + D_Right, #Tab. A2
  reg5_access     = D_IllegalAccess ~ D_old50 + D_Male + Inc_equiv_z + Cig_z + Edu_grp_all + D_Current_new + D_Border_west + D_Border_east + D_Ost + D_old50:D_Ost + Freedom_z + D_Right #Tab. A3
)

f_list_propensity <- list(
  reg1_propensity     = D_Propensity_Illegal ~ Age_z + D_Male + Inc_equiv_z + Cig_z + Edu_grp_all + + D_Current_new + D_Border_west + D_Border_east + D_Ost + Freedom_z + D_Right, #Tab. 2 
  reg2_propensity     = D_Propensity_Illegal ~ Age_z + D_Male + Inc_equiv_z + Edu_grp_all + Cig_z + D_Current_new + D_Border_west + D_Border_east + D_Ost + D_Anti, #Tab. 3
  reg3_propensity     = D_Propensity_Illegal ~ Age_z + D_Male + Inc_equiv_z + Cig_z + Edu_grp_all + D_Current_new + D_Border_west + D_Border_east + Freedom_z + D_Right, #Tab. A1
  reg4_propensity     = D_Propensity_Illegal ~ Age_z + D_Male + Inc_equiv_z + Cig_z + Edu_grp_all + D_Current_new + D_Border_west + D_Ost + Freedom_z + D_Right, #Tab. A2
  reg5_propensity     = D_Propensity_Illegal ~ D_old50 + D_Male + Inc_equiv_z + Cig_z + Edu_grp_all + D_Current_new + D_Border_west + D_Border_east + D_Ost + D_old50:D_Ost + Freedom_z + D_Right #Tab. A3
)

#rm()

mod_illegalaccess <- lapply(f_list_illegalaccess, fit_svy)
mod_propensity    <- lapply(f_list_propensity, fit_svy)


main0 <- mod_illegalaccess[[1]]
main1 <- mod_propensity[[1]]
modelsummary(list("Source knowledge" = main0,"Social acceptance" = main1),
  exponentiate = T,
  statistic = "({conf.low}, {conf.high})",
  stars = TRUE,
  coef_map = coef_map,
  coef_omit = "(Intercept)",
  vcov = "HC1",
  gof_omit = "^(?!AIC|BIC|Num\\.Obs\\.|Std.Errors)",
  #output = "yourName.tex"
)
