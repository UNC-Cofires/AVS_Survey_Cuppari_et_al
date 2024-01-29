library(dplyr)
library(reshape)
library(ggplot2)
library(readxl)
library(HH) 
library(tidyverse)
library(tidyr)
library(reshape)
library(tibble)
library(likert)
library(scales)
library(corrplot)
library(cowplot)

## helpful articles from which I took some code: 
## https://towardsdatascience.com/working-with-survey-data-clean-and-visualize-likert-scale-questions-in-r-6a78e3b9c7b2
## https://www.r-bloggers.com/2020/05/binary-logistic-regression-with-r/
font_fam = 'Montserrat'
setwd("C:/Users/rcuppari/OneDrive - University of North Carolina at Chapel Hill/Research/PhD Work/AVS/Survey_Results")

## NOTE: all were in English, so drop Spanish results
results = read_excel("survey_results_clean2.xlsx")
cols = read_excel("survey_results_clean2.xlsx", sheet = 'key')[2,]
colnames(results) = cols

results100 = results[results$Progress == 100,]

## only keep rows with at least 70% of responses
results100 = results100[rowMeans(is.na(results100)) < 0.7,]
## only keep questions with at least 50% of responses 
results100 = results100[, colMeans(is.na(results100)) < 0.5]

## need to drop row that had only pasture animals as response
## go from 40 to 41 responses
results100 = results100[results100$crops != "Sheep and Dairy Cows",]

firstcol = which(colnames(results100)=="lang_pref")
lastcol = which(colnames(results100)=="pref_ytoy")

res_thresh = results100[,firstcol:lastcol]

#avs_fin = res_thresh[,c(7:13, 32:37)]
avs_fin = res_thresh[,c(5:11, 30:35)]
avs_fin_t = as.data.frame(t(avs_fin))

avs_fin_t = rownames_to_column(avs_fin_t)

avs_fin_melt = melt(avs_fin_t, id = "rowname")

agg_tbl <- avs_fin_melt %>% group_by(rowname, value) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

# change the order
agg_tbl$value <- factor(agg_tbl$value, levels = c('Agree Strongly', 'Agree', 
                                                  'Disagree Strongly','Disagree', 
                                                  'Neutral'))
## need to make sure bars are in the right order 
agg_tbl <- agg_tbl %>% 
  mutate(value = fct_relevel(value, "Agree Strongly", "Agree", 
                             "Neutral", "Disagree", "Disagree Strongly"),
         value = fct_rev(value)) 

##
lst <- c("Agree Strongly", "Agree", 
         "Neutral", "Disagree", "Disagree Strongly")

fin_qs = res_thresh[,c(31:36)]
avs_qs = res_thresh[,c(6:12)]
goals_qs = res_thresh[,c(13:18)]
mu_qs = res_thresh[,c(26:30)]

names(avs_qs) <-
  c("Interested in AVS",
    "Upfront cost is the major factor",
    "AVS fear of yield reduction", 
    "Solar can increase profitability",
    "Would keep farming",
    "Solar could reduce risk",
    "Positive perception of solar")

names(fin_qs) <-
  c("Biggest risk to income is weather",
    "Cannot afford large CAPEX",
    "Stability is more important than profitability", 
    "Market prices make my farm revenues unpredictable",
    "My biggest costs are fixed, or overhead",
    "Prefer year-to-year planning")

## Q7_1 - Q7_5
names(mu_qs) <-
  c("I am willing to use some portion of my land to test new technology and techniques", 
    "I am not interested in changing my farming choices even if it is equally costly", 
    "I am not interested in changing my farming choices even if it saves me a little money", 
    "I am not interested in reducing my farm's water usage even if I can afford it", 
    "I care how many farms there are in my area")


##Q4_1 - Q4_6
names(goals_qs) <-
  c("My goal is to lower costs", 
    "I want my children/relatives to continue farming", 
    "My goal is to maximize my farm's gross sales", 
    "I care about reducing my farm's water footprint", 
    "Farming is just a job", 
    "I plan on expanding my farm")


clean_plot_likert = function(df, name) {
  df_t = as.data.frame(t(df))
  df_t = rownames_to_column(df_t)
  df_melt = melt(df_t, id = "rowname") 
  
  agg_df = df_melt %>% group_by(rowname, value) %>% 
    count(name = "n_answers") %>% 
    group_by(rowname) %>% 
    mutate(percent_answers = n_answers/sum(n_answers))
  
  # you need the data in the wide format
  data_df <- spread(agg_df[,c('rowname', 'value', 'percent_answers')], key = value, value = percent_answers)
  
  # now add colnames
  data_df[is.na(data_df)] <- 0
  
  ## need to make sure bars are in the right order 
  data_df = data_df[,c("rowname", "Disagree Strongly", "Disagree", "Neutral", 
                         "Agree", "Agree Strongly")]
  
  print(HH::likert(rowname~., data_df, positive.order=TRUE,
             as.percent = "noRightAxis",  
#             col = likertColorBrewer(nc =5,BrewerPaletteName="PuOr"),
             main= paste("Likert Scale Responses, ", name, sep = ""),
             xlab="Percentage of Respondents", ylab="",
             scales=list(x=list(at=seq(-100,100,20),
                                labels=c(seq(50,0,-10),seq(20,100,20))))
             ))

  return(data_df)
  }

avs_df = clean_plot_likert(avs_qs, "AVS and Solar")
## to be done later
fr_df = clean_plot_likert(fin_qs, "Financial Risk")
mu_df = clean_plot_likert(mu_qs, "Multiple Uses")
goals_df = clean_plot_likert(goals_qs, "Goals")


## also make them all numeric 
#### Recode the Likert responses ####
likert_recode <- function(x) {
  as.numeric(case_when(
    x == "Disagree Strongly" ~ -2,
    x == "Disagree" ~ -1,
    x == "Neutral" ~ 0,
    x == "Agree" ~ 1,
    x == "Agree Strongly" ~ 2,
  ))
}

# Negative Recode 
likert_recode_negative <- function(x) {
  as.numeric(case_when(
    x == "Disagree Strongly" ~ 2,
    x == "Disagree" ~ 1,
    x == "Neutral" ~ 0,
    x == "Agree" ~ -1,
    x == "Agree Strongly" ~ -2,
  ))
}

## others 
sex_recode <- function(x) {
  as.numeric(case_when(
    x == "Female" ~ 1, 
    x == "Male" ~ 2, 
    x == "Prefer Not to Respond" ~ NaN
  ))
}
## can rescale the negatively phrased questions: 
## Q7_2 I am not interested in changing my farming choices even if it is equally costly 
## Q7_3 I am not interested in changing my farming choices even if it saves me a little money 
## Q7_4 I am not interested in reducing my farm's water usage even if I can afford it
## Q8_2 I cannot afford to make large capital investments in my farm
## Q10_3 I am worried an agrivoltaic system would reduce my crop yield
survey_negative <- res_thresh %>%
  dplyr::select(chg_eq_cost, chg_low_cost, water_aff, afford_cap, red_yield) %>%
  mutate_all(likert_recode_negative)

##
survey_positive <- res_thresh %>%
  dplyr::select(-lang_pref, -where, -qualify, -role, 
         -chg_eq_cost, -chg_low_cost, -water_aff, -afford_cap, -red_yield) %>%
  mutate_all(likert_recode)

#Recombine data
survey_recoded <- cbind(survey_positive, survey_negative) 

## to do a second combination/aggregation 
survey_recoded_groups <- cbind(survey_positive, survey_negative) 

## add prefix to name to make it easier to visualize 
library(data.table)

#names_goals = c('Goal: Low Costs', 'Goal: Cont. Farming', 'AVS Inc. Profits', 'Goal: Red. Water', 
#                'Farm Just Job', 'Goal: Exp. Farm')

#names_mu = c('Would Test Alts', 'No Int. Change, Eq. Cost', 'No Int. Change, Low Cost', 'No Int. Red. Water',
#             'Care About Farms')

#names_avs = c('Interest in AVS', 'Concern: AVS CAPEX', 'Concern: AVS Red. Yield', 'inc_prof', 
#              'Farm W. Sol Income', 'AVS Diversify', 'Pos. Percep. AVS')

#names_fr = c('Risk: Weather', 'CAPEX Inafford.', 'Pref. Fixed Costs', 'Risk: Unpred. Mkt.',  
#             'Costs Mostly Fixed', 'Pref. Y-to-Y Planning')
names_goals = c('low_cost', 'cont_farm', 'max_sales', 'red_water', 'just_job', 'exp_farm')
names_mu = c('port_test', 'chg_eq_cost', 'chg_low_cost', 'water_aff', 'care_farms')
names_avs = c('int_avs', 'up_cost', 'red_yield', 'inc_prof', 'suff_income', 'diversify', 
              'pos_percep')
names_fr = c('weath_risk', 'afford_cap', 'pref_fixed', 'unpred_mkt', 'costs_fix', 'pref_ytoy')

setnames(survey_recoded, old = names_goals,
         new = paste("goals", names_goals, sep = '_'))

setnames(survey_recoded, old = names_mu,
         new = paste("mult_uses", names_mu, sep = '_'))

setnames(survey_recoded, old = names_avs,
         new = paste("avs", names_avs, sep = '_'))

setnames(survey_recoded, old = names_fr,
         new = paste("fin_risk", names_fr, sep = '_'))

## get sums by category 
survey_recoded2 <- survey_recoded %>%
  rowwise() %>%
  mutate(goals_total = sum(goals_low_cost, goals_cont_farm, goals_max_sales,
                           goals_red_water, goals_just_job, goals_exp_farm),
         
         multiple_uses_total = sum(mult_uses_port_test, mult_uses_chg_eq_cost,
                                   mult_uses_chg_low_cost, mult_uses_water_aff, 
                                   mult_uses_care_farms),
         
         avs_total = sum(avs_int_avs, avs_up_cost, avs_red_yield, 
                         avs_inc_prof, avs_suff_income, avs_diversify, 
                         avs_pos_percep),
         
         fin_risk_total = sum(fin_risk_weath_risk, fin_risk_afford_cap, 
                              fin_risk_pref_fixed, fin_risk_unpred_mkt, 
                              fin_risk_costs_fix, fin_risk_pref_ytoy)) %>%
  ungroup()

##
names_risk = c("pref_ytoy", "pref_fixed")
names_change = c("port_test", "chg_low_cost", "chg_eq_cost")
names_culture = c("care_farms", "cont_farm", "just_job", "suff_income")
names_costs = c("inc_prof", "costs_fix", "afford_cap")

setnames(survey_recoded_groups, old = names_risk,
         new = paste("risk", names_risk, sep = '_'))

setnames(survey_recoded_groups, old = names_change,
         new = paste("change", names_change, sep = '_'))

setnames(survey_recoded_groups, old = names_culture,
         new = paste("cult", names_culture, sep = '_'))

setnames(survey_recoded_groups, old = names_costs,
         new = paste("costs", names_costs, sep = '_'))

## get sums by category 
survey_recoded_groups2 <- survey_recoded_groups %>%
  rowwise() %>%
  mutate(risk_total = sum(risk_pref_fixed, risk_pref_ytoy, na.rm = TRUE),
         cult_total = sum(cult_suff_income, cult_just_job, cult_care_farms, 
                            cult_cont_farm, na.rm = TRUE),
         change_total = sum(change_chg_eq_cost, change_chg_low_cost, 
                            change_port_test, na.rm = TRUE), 
         costs_total = sum(costs_inc_prof, costs_costs_fix, costs_afford_cap)) %>%
  ungroup()

## reshape
survey_long <- survey_recoded2 %>%
  dplyr::select(-goals_total, -multiple_uses_total, -avs_total, -fin_risk_total) %>%
  pivot_longer(data = .,
               cols = 1:31,
               names_to = "question",
               values_to = "response") %>%
  mutate(category = str_match(question, "[^[^_]+(?=_)]]"),
         question_no = str_match(question, "[0-9]$")
  )

## summaries by category don't work well 
indices = survey_long %>%
  group_by(category) %>%
  summarise(avg_response = mean(response, na.rm = TRUE), 
            sd_response = sd(response, na.rm = TRUE),
            sum_response = sum(response, na.rm = TRUE))

index_avs <- rowSums(survey_recoded[, c('avs_up_cost', 'avs_inc_prof', 
                                'avs_suff_income', 'avs_diversify', 'avs_pos_percep')])

index_mu = rowSums(survey_recoded[,c('mult_uses_care_farms', 'mult_uses_port_test',
                     'mult_uses_chg_eq_cost', 'mult_uses_chg_low_cost',
                     'mult_uses_water_aff')])

index_fr = rowSums(survey_recoded[,c('fin_risk_unpred_mkt', 'fin_risk_weath_risk',
                                     'fin_risk_pref_fixed', 'fin_risk_costs_fix', 
                                     'fin_risk_pref_ytoy')])

index_goals = rowSums(survey_recoded[,c('goals_max_sales', 'goals_red_water', 'goals_just_job',
                        'goals_low_cost', 'goals_cont_farm')])

res_thresh = cbind(res_thresh, index_avs, index_goals, index_mu, index_fr)
survey_recoded = cbind(survey_recoded, index_avs, index_goals, index_mu, index_fr)

###################### Cronbach's Alpha for reliability ########################
## mostly for understanding within the groups ##
## note, some questions need to be recoded 
## specifically, chg_eq_cost, chg_low_cost, water_aff, afford_cap, red_yield
mu_qs2 = survey_recoded[,c('mult_uses_care_farms', 'mult_uses_port_test',
                           'mult_uses_chg_eq_cost', 'mult_uses_chg_low_cost',
                           'mult_uses_water_aff')]
avs_qs2 = survey_recoded[, c('avs_up_cost', 'avs_inc_prof', 
                             'avs_suff_income', 'avs_diversify', 'avs_pos_percep')]
fin_qs2 = survey_recoded[,c('fin_risk_unpred_mkt', 'fin_risk_weath_risk',
                            'fin_risk_pref_fixed', 'fin_risk_costs_fix', 
                            'fin_risk_pref_ytoy')]
goals_qs2 = survey_recoded[,c('goals_max_sales', 'goals_red_water', 'goals_just_job',
                              'goals_low_cost', 'goals_cont_farm')]

risk_qs2 = survey_recoded_groups[, c("risk_pref_ytoy", "risk_pref_fixed")]
change_qs2 = survey_recoded_groups[, c("change_port_test", "change_chg_eq_cost", "change_chg_low_cost")]
cult_qs2 = survey_recoded_groups[, c("cult_care_farms", "cult_cont_farm", 
                                     "cult_just_job", "cult_suff_income")]
costs_qs2 = survey_recoded_groups[, c("costs_inc_prof", "costs_costs_fix", "costs_afford_cap")]

## cronbach's alpha is passable (but questionable) for the avs qs, but unacceptable for the rest
psych::alpha(avs_qs2, na.rm = T)
psych::alpha(change_qs2, na.rm = T) 
psych::alpha(goals_qs2, na.rm = T) ## poor

psych::alpha(mu_qs2, na.rm = T) ## unacceptable
psych::alpha(fin_qs2, na.rm = T) ## unacceptable

psych::alpha(risk_qs2, na.rm = T) ## unacceptable
psych::alpha(costs_qs2, na.rm = T, check.keys = TRUE) ## unacceptable
psych::alpha(cult_qs2, na.rm = T, check.keys = TRUE) ## unacceptable

## suggests the only one we should be doing as a sum score is AVS qs and Change qs

## scatter plot with different questions 
survey_recoded3 = survey_recoded[, colSums(is.na(survey_recoded)) < 2]
survey_recoded_groups3 = survey_recoded_groups2[, colSums(is.na(survey_recoded_groups2)) < 2]

survey_recoded3 = cbind(survey_recoded3, survey_recoded_groups3[,"change_total"])
survey_recoded3 = subset(survey_recoded3, select = -c(index_goals, index_fr, index_mu))

corr_mat = cor(survey_recoded3, method = 's', use = "pairwise.complete.obs")
co = melt(corr_mat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

plot_lower_tri = function(melted_tri) { 
  ggplot(melted_tri, aes(Var1, Var2, fill = value)) + 
    geom_tile(color = 'white') + 
    geom_text(aes(label = round(melted_tri$value, 2))) +
    scale_fill_gradient2(low = muted("darkred"), 
                         mid = "white", 
                         high = muted("midnightblue"), 
                         midpoint = 0) +
    theme(#legend.justification = c(1,0),
          #legend.position = c(0.6, 0.7), 
          #legend.direction = "horizontal",
          panel.grid.major.x=element_blank(), 
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.y=element_blank(), 
          panel.grid.minor.y=element_blank(),
          panel.background=element_rect(fill="white")) +
    theme(legend.title=element_text(face="bold", size=14)) + 
    scale_x_discrete(name="") +
    scale_y_discrete(name="") 
    }

lower_tri <- get_lower_tri(corr_mat)
melted_tri= melt(lower_tri, na.rm = TRUE)
#plot_lower_tri(melted_tri)
## okay, overwhelming

## let's make three plots: 
## one with just the avs qs 
avs = survey_recoded3 %>% 
  dplyr::select(contains("avs"))
corr_avs = cor(avs, method = 's', use = "pairwise.complete.obs")
co_avs = melt(corr_avs)
lower_tri_avs <- get_lower_tri(co_avs)
#tri_fr = lower_tri_fr[lower_tri_fr$value < 0.999,]
tri_avs = lower_tri_avs %>% drop_na()

#plot_lower_tri(tri_avs)

## one with int avs & fin risk 
## one with avs & mult uses 
## one with avs & goals 

avs_fr = survey_recoded3 %>% 
  dplyr::select(-(contains("goals") | contains("mult_uses")))

corr_fr = cor(avs_fr, method = 's', use = "pairwise.complete.obs")
co_fr = melt(corr_fr)
lower_tri_fr <- get_lower_tri(co_fr)
tri_fr = lower_tri_fr[lower_tri_fr$value < 0.999,]
tri_fr = tri_fr %>% drop_na()

#plot_lower_tri(tri_fr)
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
## let's make two plots: corrs > 0.3 and corrs < 0.3 

## corrs > 0.3
co = melt(corr_mat)

co_high = co[co$value > 0.5,]
lower_tri_high <- get_lower_tri(co_high)
tri_high = lower_tri_high[lower_tri_high$value < 0.999,]
tri_high = tri_high %>% drop_na()

#plot_lower_tri(tri_high)

## corrs < 0.3
co_low = co[co$value < -0.3,]
lower_tri_low <- get_lower_tri(co_low)
melted_tri_low = melt(lower_tri_low, na.rm = TRUE)

#plot_lower_tri(melted_tri_low)

## combine both together
co_both = co[co$value < -0.3 | co$value > 0.5 & co$value < 0.9,]
lower_tri_both <- get_lower_tri(co_both)
melted_tri_both = melt(lower_tri_both, na.rm = TRUE)

colnames(corr_mat) = c('Interest in AVS', 'Concern: AVS CAPEX', 'AVS Inc. Profits', 'Farm W. Sol Income', 
                       'AVS Diversify', 'Pos. Percep. Solar', 'Goal: Low Costs', 'Goal: Cont. Farming', 
                       'Goal: Max. Sales', 'Goal: Red. Water', 'Farm Just Job', 'Goal: Exp. Farm', 
                       'Would Test Alts', 'Care About Farms', 'Concern Weath. Risk', 'Pref. Fixed Costs',
                       'Risk: Unpred. Mkt.', 'Costs Mostly Fixed', 'Pref. Y-to-Y Planning', 
                       'No Int. Change, Eq. Cost', 'No Int. Change, Low Cost', 'No Int. Red. Water', 
                       'CAPEX Inafford.', 'Concern: AVS Red. Yield', 
                       "AVS/Solar Sum Score", "Change Sum Score")

rownames(corr_mat) = c('Interest in AVS', 'Concern: AVS CAPEX', 'AVS Inc. Profits', 'Farm W. Sol Income', 
                       'AVS Diversify', 'Pos. Percep. Solar', 'Goal: Low Costs', 'Goal: Cont. Farming', 
                       'Goal: Max. Sales', 'Goal: Red. Water', 'Farm Just Job', 'Goal: Exp. Farm', 
                       'Would Test Alts', 'Care About Farms', 'Concern Weath. Risk', 'Pref. Fixed Costs',
                       'Risk: Unpred. Mkt.', 'Costs Mostly Fixed', 'Pref. Y-to-Y Planning', 
                       'No Int. Change, Eq. Cost', 'No Int. Change, Low Cost', 'No Int. Red. Water', 
                       'CAPEX Inafford.', 'Concern: AVS Red. Yield', 
                       "AVS/Solar Sum Score", "Change Sum Score")

png(filename = "mycorrplot.png", width = 1200, height = 800)
corrplot(corr_mat, type = 'upper',# order = 'hclust',
         tl.srt = 45, tl.col = 'black', tl.cex = 1.25, cl.cex = 1.5)
dev.off()
#####################################################
## plot associations 

library(gridExtra)

plot_labs = data.frame(NULL)
plot_labs = rbind(plot_labs, c("Interest in AVS", 'Concern: AVS CAPEX', 'AVS Inc. Profits', 'Farm W. Sol Income', 
                              'AVS Diversify', 'Pos. Percep. Solar', 'Goal: Low Costs', 'Goal: Cont. Farming', 
                              'Goal: Max. Sales', 'Goal: Red. Water', 'Farm Just Job', 'Goal: Exp. Farm', 
                              'Would Test Alts', 'Care About Farms', 'Concern Weath. Risk', 'Pref. Fixed Costs',
                              'Risk: Unpred. Mkt.', 'Costs Mostly Fixed', 'Pref. Y-to-Y Planning', 
                              'No Int. Change, Eq. Cost', 'No Int. Change, Low Cost', 'No Int. Red. Water', 
                              'CAPEX Inafford.', 'Concern: AVS Red. Yield', 
                              "Change Sum Score", "AVS/Solar Sum Score"))
colnames(plot_labs) = colnames(survey_recoded3) 

p = lapply(colnames(survey_recoded3), 
       function(i)ggplot(survey_recoded3, 
                         aes_string(x=i, y="avs_int_avs")) + 
         geom_point(alpha = 0.25) + theme_minimal() +  
         geom_smooth(method = "auto", se = TRUE, fullrange = FALSE, 
                     level = 0.95) + 
         labs(x = plot_labs[i], y = 'Interest in AVS'))
ggsave(filename = "plots2.jpg", 
       plot = marrangeGrob(p, nrow = 7, ncol = 4), 
       width = 10, height = 10)

survey_og <- res_thresh %>%
  dplyr::select(-lang_pref, -where, -qualify, -role) %>%
  mutate_all(likert_recode)
survey_og = survey_og[ , colSums(is.na(survey_og))<2]

p = lapply(colnames(survey_og), 
           function(i)ggplot(survey_og, 
                             aes_string(x=i, y="int_avs")) + 
             geom_point(alpha = 0.25) + theme_minimal() +
             geom_smooth(method = "auto", se = TRUE, fullrange = FALSE, 
                         level = 0.95))
ggsave(filename = "plots_not_rec.jpg", 
       plot = marrangeGrob(p, nrow = 4, ncol = 6), 
       width = 10, height = 10)

################################################################################
## it also seems interesting to plot who is most important in farming decision making
## Q5_1 through Q5_8
drivers = melt(res_thresh[,c("mkt", "farmers", "fam_friends",
                             "academ", "other", "cca", "banks")] %>% drop_na())

ggplot(drivers, aes(value, fill = factor(value))) + 
  facet_wrap(~variable, 
             labeller = labeller(variable = 
                                   c("mkt" = "The Market",
                                     "farmers" = "Colleagues \n(other farmers)", 
                                     "fam_friends" = "Family and Friends", 
                                     "academ" = "Academics", 
                                     "other" = "Other", 
                                     "cca" = "Certified Crop Advisors",
                                     "banks" = "Banks"))) + 
  geom_histogram(binwidth = 1) +
  scale_fill_viridis_d(direction = 1) + 
  theme_minimal() + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x=element_blank(), 
        text = element_text(size = 16, family =font_fam)) + 
  guides(fill=guide_legend(title = "Ranking")) +
  xlab("Importance Ranking") + 
  ylab("Frequency") 

## time to do some other tests
# Shapiro Wilks to test normality and set the stage for others
# if p-val <= 0.05 not normal
shapiro.test(survey_recoded3$avs_int_avs)
## okay, not normal :( That means that we can use kruskal-wallis test
demo_data2 = cbind(survey_recoded3, 
                   demo_data[, c('age', 'sex', 'edu', 'where', 'gender',
                                 'race', 'acres', 'purch', 'pct_prof', 
                                 'hrs_off', 'gen', 'ins', 'irr')])
library(grepl)
demo_data2$landowner = ifelse(grepl("owner", demo_data$role), "owner", "not")

og_cols = colnames(survey_recoded3)
new_cols = c(og_cols, 'age', 'sex', 'edu', 'where', 'gender', 'race',
             'acres', 'purch', 'pct_prof', 'hrs_off', 'gen', 
             'ins', 'irr', 'owner')

colnames(demo_data2) = new_cols

## p-val < 0.05 = sig difference between groups
library(broom)
kw_df = demo_data2 %>% gather(key, value, -avs_int_avs) %>% 
  group_by(key) %>% 
  do(tidy(kruskal.test(x = .$value, g = .$avs_int_avs)))

## now appropriate to use Dunn's test for the categories which are significant
sig_names = kw_df$key[kw_df$p.value < 0.05]
sig_vals = demo_data2[,c(sig_names, 'avs_int_avs')]

## make factors for test
#sig_vals[, c(sig_names)] = lapply(sig_vals[,c(sig_names)], factor)

## run Dunn's Test
library(dunn.test)
dunns_df = apply(demo_data2[,c(sig_names)], 2, 
      function(x) dunn.test(demo_data2$avs_int_avs, demo_data2$x, 
                            method = "bonferroni"))

## note: only significant differences in groups are 
## 1) wrt diversify, agree strongly -- disagree and agree strongly -- neutral
## 2) wrt inc. prof., agree strongly -- disagree
## 3) wrt red. water, agree -- agree strongly 

## end up using fisher test when counts are < 5 as opposed to chi squared
library(descr)
CrossTable(demo_data2$avs_int_avs, demo_data2$where,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')

CrossTable(demo_data2$avs_int_avs, demo_data2$owner,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')

CrossTable(demo_data2$avs_int_avs, demo_data2$avs_diversify,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')

CrossTable(demo_data2$avs_int_avs, demo_data2$goals_red_water,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')

CrossTable(demo_data2$avs_int_avs, demo_data2$avs_inc_prof,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')

CrossTable(demo_data2$avs_int_avs, demo_data2$sex,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')

CrossTable(demo_data2$avs_int_avs, demo_data2$age,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')

CrossTable(demo_data2$avs_int_avs, demo_data2$irr,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')

CrossTable(demo_data2$avs_int_avs, demo_data2$pct_prof,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')

## NOTE!
CrossTable(demo_data2$avs_int_avs, demo_data2$acres,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')

CrossTable(demo_data2$avs_int_avs, demo_data2$hrs_off,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')

## state diff
CrossTable(demo_data2$where, demo_data2$goals_red_water,
           fisher = T, chisq = T, expected = T,
           prop.c = F, prop.t = F, prop.chisq = F, 
           sresid = T, format = 'SPSS')


## can visually show this too.. example with sex + state
library(ggplot2)
library(rlang)

## weirdly seem to have duplicated columns? 
## MIGHT NO LONGER NEED THIS
demo_data = results100[, !duplicated(colnames(results100), fromLast = TRUE)]

## none of the demographics are influential, but you can see that
## wanting to diversify + reduce water + thinking avs would > prof 
## have significant differences between groups. 

## so now let's move on to conducting ordinal logistic regression 
## the below gives the odds ratios // probability of 
## being over a given level (proportional odds Logisitic regression)
library(rlang)
library(dplyr)
demo_data$int_avs = as.factor(demo_data$int_avs)
demo_data$red_water = as.factor(demo_data$red_water)
demo_data$diversify = as.factor(demo_data$diversify)
demo_data$inc_prof = as.factor(demo_data$inc_prof)
demo_data$port_test = as.factor(demo_data$port_test)
demo_data$max_sales = as.factor(demo_data$max_sales)
demo_data$chg_low_cost = as.factor(demo_data$chg_low_cost)
demo_data$costs_fix = as.factor(demo_data$costs_fix)

## make sure our levels are in the right order
demo_data <- demo_data %>% 
  mutate(int_avs = factor(int_avs,
                        levels = c("Disagree Strongly", "Disagree", "Neutral", 
                                   "Agree", "Agree Strongly")))

demo_data <- demo_data %>% 
  mutate(diversify = factor(diversify,
                           levels = c("Disagree Strongly", "Disagree", "Neutral", 
                                      "Agree", "Agree Strongly")))

demo_data <- demo_data %>% 
  mutate(red_water = factor(red_water,
                           levels = c("Disagree Strongly", "Disagree", "Neutral", 
                                      "Agree", "Agree Strongly")))


library(MASS)
fit.wat = MASS::polr(int_avs ~ red_water, 
                     data = demo_data,
                     Hess = T)
summary(fit.wat)

fit.div = MASS::polr(int_avs ~ diversify, 
                     data = demo_data,
                     Hess = T)
summary(fit.div)



## using the glm package
require(caret)
model_div = glm(int_avs ~ diversify,
            data = demo_data, family = 'binomial')
model_div

model_wat = glm(int_avs ~ red_water,
            data = demo_data, family = 'binomial')
model_wat ## higher AIC and Residual Deviance than the diversify input

## can convert intercept to probabilities using sigmoid function
## exp(intercept) / (1 + exp(intercept))

############################# summary data ##################################
table(demo_data$int_avs)
table(demo_data$red_water)
table(demo_data$diversify)

## let's combine some levels 
demo_data3 = demo_data2[,c(1:24)]
demo_data3$int_avs[demo_data3$avs_int_avs == "Disagree Strongly"] <- "Disagree"
demo_data3$int_avs[demo_data3$avs_int_avs == "Agree Strongly"] <- "Agree"
table(demo_data3$avs_int_avs)

demo_data3$diversify[demo_data3$avs_diversify == "Disagree Strongly"] <- "Disagree"
demo_data3$diversify[demo_data3$avs_diversify == "Agree Strongly"] <- "Agree"
table(demo_data3$avs_diversify)

############################## more models #####################################
index <- createDataPartition(demo_data3$int_avs, p = .90, list = FALSE)

demo_data3 = cbind(demo_data3, res_thresh[,c("index_avs", "index_mu", 
                                             "index_goals", "index_fr")])
demo_data3[, c(sig_names, 'int_avs')] = 
  lapply(demo_data3[,c(sig_names, 'int_avs')], factor)

train <- demo_data3[index, c(sig_names, 'avs_int_avs')]#c('int_avs', 'diversify', 'red_water')]
test <- demo_data3[-index, c(sig_names, 'avs_int_avs')]#c('int_avs', 'diversify', 'red_water')]

# Training the model
#logistic_model <- glm(avs_int_avs ~ avs_diversify + goals_red_water, 
#                      family = binomial(), train)
# Checking the model
#summary(logistic_model)

# Loading the packages
ctrl <- trainControl(method = "LOOCV")
#fit a regression model and use LOOCV to evaluate performance
## use generalized linear model 
model <- train(avs_int_avs ~  index_avs, 
               data = demo_data2, 
               method = "glm", trControl = ctrl)
#view summary of LOOCV               
print(model)

model2 <- train(avs_int_avs ~  avs_diversify + goals_red_water, 
               data = demo_data2, 
               method = "glm", trControl = ctrl)
#view summary of LOOCV               
print(model2)


################################ counts ########################################
##################### and diverging bar plot (FIGURE 3) ########################
table(demo_data$cont_farm)
table(demo_data$pos_percep)
table(demo_data$up_cost)
table(demo_data$weath_risk)
table(demo_data$inc_prof)
table(demo_data$int_avs)
table(demo_data$red_yield)


## Diverging Bar plot using ggplot()
## split neutral down the middle 
names_fin_risk = colnames(demo_data2[, grepl("fin_risk", names(demo_data2))]) #colnames(res_thresh[,c(30:35)])
names_avs = colnames(demo_data2[, grepl("avs", names(demo_data2))]) # colnames(res_thresh[,c(6:12)])
names_goals = colnames(demo_data2[, grepl("goals", names(demo_data2))]) # colnames(res_thresh[,c(12:17)])
names_mu = colnames(demo_data2[, grepl("mult", names(demo_data2))]) # colnames(res_thresh[,c(25:29)])

fr_cols_bar = demo_data2[,names_fin_risk]
fr_bar_t = as.data.frame(t(fr_cols_bar))
fr_bar = rownames_to_column(fr_bar_t)
fr_melt = melt(fr_bar, id = "rowname")

goals_cols_bar = demo_data2[,names_goals]
goals_bar_t = as.data.frame(t(goals_cols_bar))
goals_bar = rownames_to_column(goals_bar_t)
goals_melt = melt(goals_bar, id = "rowname")

mu_cols_bar = demo_data2[,names_mu]
mu_bar_t = as.data.frame(t(mu_cols_bar))
mu_bar = rownames_to_column(mu_bar_t)
mu_melt = melt(mu_bar, id = "rowname")

avs_cols_bar = demo_data2[,names_avs]
avs_bar_t = as.data.frame(t(avs_cols_bar))
avs_bar = rownames_to_column(avs_bar_t)
avs_melt = melt(avs_bar, id = "rowname")

par(mfrow= c(3,1))

## want everything else in one plot with subplots
goals_df$type = 'Goals'
fr_df$type = 'Financial Risk'
mu_df$type = 'Multiple Uses'

combined_df = rbind(goals_df, fr_df, mu_df)

HH::likert(rowname~. | type, combined_df, ReferenceZero=3, 
           as.percent = TRUE,
           main = list("Likert Scale Responses by Question Category", 
           x=unit(.6, "npc")), 
           layout=c(1,3), auto.key = list(columns = 5),
           scales=list(y=list(relation="free"), 
                       x=list(at=seq(-100,100,20),
                              labels=c(seq(50,0,-10),seq(20,100,20)))), 
           between=list(y=1), strip.left=FALSE, strip = TRUE,
           par.strip.text=list(cex=.7, lines=2), ylab="", 
           xlab = "Percentage of Responses"
           )


######################### TABLE SX: contestation ##############################
means = colMeans(survey_recoded2, na.rm = TRUE)
medians = colMedians(survey_recoded2, na.rm = TRUE)
stds = sapply(survey_recoded3, sd, na.rm = TRUE)

################################################################################
## insurance v int in avs? 
results100$ins[results100$ins == 'Yes'] = int(1)
results100$ins[results100$ins == 'No'] = int(0)
results100$ins = as.numeric(results100$ins)

results100$int_avs = likert_recode(results100$int_avs)
cor(results100$ins, results100$int_avs)

################### 
library(tigris)

zips_NC <- zctas(year = 2010, state = "NC", progress_bar = FALSE)
zips_CA <- zctas(year = 2010, state = "CA", progress_bar = FALSE)
names(zips_CA)[names(zips_CA) == 'ZCTA5CE10'] = 'zip'
names(zips_NC)[names(zips_NC) == 'ZCTA5CE10'] = 'zip'

results100$zip2 = substr(results100$zip, 1, 3)
counts = data.frame(table(results100$zip2))
colnames(counts) = c('zip2', 'count')
counts$count = as.factor(counts$count)

zips_CA$zip2 = substr(zips_CA$zip, 1, 3)
zips_CA$zip2 = as.factor(zips_CA$zip2)
ca_data = merge(zips_CA, counts, by = 'zip2')
#ca_data$zip2 = substr(ca_data$zip, 1, 2)

zips_NC$zip2 = substr(zips_NC$zip, 1, 3)
zips_NC$zip2 = as.factor(zips_NC$zip2)
nc_data = merge(zips_NC, counts, by = 'zip2')
#nc_data$zip2 = substr(nc_data$zip, 1, 2)

us_states <- states(progress_bar = FALSE)

ca <- us_states |> filter(STUSPS == "CA")
nc <- us_states |> filter(STUSPS == "NC")

## for borders create a new dataset with grouped areas (thanks ChatGPT!)
grouped_nc <- nc_data %>% 
  group_by(zip2) %>% 
  summarise() 

grouped_ca <- ca_data %>% 
  group_by(zip2) %>% 
  summarise() 

#palette = "ag_Sunset" # '#EE5D6C',
custom_colors = c('#EEAF61', '#EE6C2B', '#CE4993', '#6A0D83')

plot1 = ggplot() +
  geom_sf(data = ca, colour = NA) + 
#          colour = "black", size = 1.5) +
  geom_sf(aes(fill = count, color = count), data = ca_data) +
  geom_sf(data = grouped_ca, aes(), fill = NA, 
          colour = "black", size = 1.5) +  # Draw borders around grouped areas
  theme_void() + 
  theme(legend.position = "none") +
  scale_fill_manual(values = custom_colors) +
  scale_colour_manual(values = custom_colors) 
  #  scale_fill_brewer(palette = palette) +
#  scale_colour_brewer(palette = palette)

plot2 = ggplot() +
  geom_sf(data = nc, colour = NA) +
#          colour = "black", size = 1.5) +
  geom_sf(aes(fill = count, color = count), data = nc_data) +
  geom_sf(data = grouped_nc, aes(), fill = NA, 
          colour = "black", size = 1.5) +  # Draw borders around grouped areas
  theme_void() + 
  labs(fill = "Count", color = "Count") + 
  theme(legend.position = 'bottom', legend.key.size = unit(.75, 'cm'),
        legend.text = element_text(size = rel(1.25)),
        legend.title = element_text(size = rel(1.5))) +
  scale_fill_manual(values = custom_colors) +
  scale_colour_manual(values = custom_colors) 
#  scale_fill_brewer(palette = palette) +
#  scale_colour_brewer(palette = palette)

grid.arrange(
  arrangeGrob(plot1, plot2, ncol = 2), widths = c(2,1))

ggsave("zip_map.png", width = 7, height = 4, dpi = 96)
