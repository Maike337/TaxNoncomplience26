library(survey)
library(srvyr)
library(dplyr)
library(ggplot2)
library(modelsummary)
#############################################################################################
load("design.RData")  ###load your data
###########################################################################################
coef_map <- c(   #Customization for regression outputs
  "D_Male1"                = "Male",
  "Age"                    = "Age",
  "Edu_grp_allLSD"         = "Lower secondary degree",
  "Edu_grp_allUSD"         = "Upper secondary degree",
  "Edu_grp_allUD"          = "Higher education level",
  "Inc_grpmid"             = "Income 2,000-3,999€",
  "Inc_grphigh"            = "Income ≥ 4.000",
  "AmountCig"              = "Amount Cigarettes",
  "D_Current_new"          = "New products",
  "D_Opportunities"        = "Nearby border",
  "D_Ost"                  = "East Germany",
  "TaxFreedom_w"           = "Restriction of freedom",
  "D_Right"                = "AfD voter",
  "D_Left"                 = "Left Party voter",
  "D_Mid"                  = "Union voter",
  "D_Anti" = "Anti-establishment",
  "Inc_grpmid:AmountCig"   = "Middle income × Amount Cigarettes",
  "Inc_grphigh:AmountCig"  = "High income × Cigarettes",
  "Inc_grpmid:Edu_grp_allUSD"   = "Middle income × Secondary degree",
  "Inc_grphigh:Edu_grp_allUSD"   = "High income × Secondary degree",
  "Inc_grpmid:Edu_grp_allUD"= "Middle income × University degree",
  "Inc_grphigh:Edu_grp_allUD"= "High income × University degree"
)

###################################################################################################
### creates a srvyr-object to use dplyr-friendly functions
df.srvyr <- as_survey_design(design)
df.srvyr %>% 
  group_by(Sex) %>% 
  summarise(mean_age = survey_mean(Age, vartype = "ci", level = 0.95))

prop.table(svytable(~Sundayquestion, design = design))

df.srvyr %>%
  group_by(Sex, Sundayquestion) %>%
  summarise(prop = survey_prop(vartype = "ci", level = 0.99))
### make better plots
# instead of 
svyhist(~Age, design = design)
## do: (essentially: use df_D and consider the weights, r_rake, explicitly)
ggplot(df_D, aes(x = Age, weight = w_rake)) +
  geom_histogram(binwidth = 5, colour = "white", fill = "purple") +
  geom_vline(xintercept = as.numeric(svymean(~Age, design)[1]), linetype = "dashed" ) +
  theme_minimal(base_size = 18)

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
  m0     = D_IllegalAccess ~ Age + D_Male + Inc_grp + Edu_grp_all + AmountCig + D_Current_new + D_Opportunities + D_Ost + TaxFreedom_w + D_Right, #model 1
  m1     = D_IllegalAccess ~ Age + D_Male + Inc_grp + Edu_grp_all + D_Opportunities, #model 2
  m2     = D_IllegalAccess ~ D_Male + D_Ost + D_Current_new + TaxFreedom_w + D_Right, 
  m3     = D_IllegalAccess ~ D_Male + D_Ost + D_Current_new + D_Right, 
  m4     = D_IllegalAccess ~ Age + D_Male + D_Current_new + D_Ost + D_Anti, 
  m5     = D_IllegalAccess ~ Age + D_Male + Inc_grp + Edu_grp_all + AmountCig + D_Current_new + D_Opportunities + D_Ost + D_Anti 
)

f_list_propensity <- list(
  p0     = D_Propensity_Illegal ~ Age + D_Male + Inc_grp + Edu_grp_all + AmountCig + D_Current_new + D_Opportunities + D_Ost + TaxFreedom_w + D_Right, #model 1
  p1     = D_Propensity_Illegal ~ Age + D_Male + Inc_grp + Edu_grp_all + D_Opportunities, #model 2
  p2     = D_Propensity_Illegal ~ Age + D_Male + D_Ost + TaxFreedom_w + D_Right,
  p3     = D_Propensity_Illegal ~ Age + D_Male + D_Ost + D_Right, 
  p4     = D_Propensity_Illegal ~ Age + D_Male + D_Current_new + D_Ost + D_Anti,
  p5     = D_Propensity_Illegal ~ Age + D_Male + Inc_grp + Edu_grp_all + AmountCig + D_Current_new + D_Opportunities + D_Ost + D_Anti
)

#rm(m0, m1, m2, m3, m4, m5,
#   p0, p1, p2, p3, p4, p5,
#   s0, s1)

mod_heavysmoker <- lapply(f_list_heavysmoker, fit_svy)
mod_illegalaccess <- lapply(f_list_illegalaccess, fit_svy)
mod_propensity    <- lapply(f_list_propensity, fit_svy)

# start sb1303
main0 <- mod_illegalaccess[[1]]
main1 <- mod_propensity[[1]]
modelsummary(list(main0,main1),
  exponentiate = T,
  statistic = "({conf.low}, {conf.high})",
  stars = TRUE,
  coef_map = coef_map,
  coef_omit = "(Intercept)",
  vcov = "robust",
  gof_omit = "^(?!AIC|BIC|Num\\.Obs\\.|Std.Errors)",
#  cluster = "Bundesland"    
)


main2 <- mod_illegalaccess[[6]]
main3 <- mod_propensity[[6]]
modelsummary(list(main2,main3),
  exponentiate = T,
  statistic = "({conf.low}, {conf.high})",
  stars = TRUE,
  coef_map = coef_map,
  coef_omit = "(Intercept)",
  vcov = "robust",
  gof_omit = "^(?!AIC|BIC|Num\\.Obs\\.|Std.Errors)",
#  cluster = "Bundesland"
)