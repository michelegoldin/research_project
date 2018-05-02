
# Load packages
library(tidyverse)
library(readxl)
library(lme4)
library(here)

#Tidy data

#here()

test <- read_excel(here("data", "Data_long_R_withL2.xlsx"))

#test <- read_excel("../data/Data_long_R_withL2.xlsx")
#View(test)

test <- test %>% 
  rename(., profEn = `prof en`, profSp = `prof sp`, corrResp = `response`, respBoth = `responded both`) %>% 
  filter(., status != "distractor") %>%
  as.data.frame(.)

test$response <- 'hi'
# Eng
test[test$status == 'overt' & test$language == 'eng' & test$corrResp == 1, 'response'] <- 'overt'
test[test$status == 'overt' & test$language == 'eng' & test$corrResp == 0, 'response'] <- 'null'
test[test$status == 'null'  & test$language == 'eng' & test$corrResp == 1, 'response'] <- 'null'
test[test$status == 'null'  & test$language == 'eng' & test$corrResp == 0, 'response'] <- 'overt'

# Sp
test[test$status == 'overt' & test$language == 'sp'  & test$corrResp == 1, 'response'] <- 'overt'
test[test$status == 'overt' & test$language == 'sp'  & test$corrResp == 0, 'response'] <- 'null'
test[test$status == 'null'  & test$language == 'sp'  & test$corrResp == 1, 'response'] <- 'null'
test[test$status == 'null'  & test$language == 'sp'  & test$corrResp == 0, 'response'] <- 'overt'

df_null_sub <- test %>%
  mutate(., respFactor = ifelse(response == 'overt', yes = 1, no = 0))

#View(df_null_sub)

  
#Proficiency scores in English
  


# BESA scores as a function of group
besa_table_markdown <- df_null_sub %>% 
  group_by(., group) %>% 
  summarize(., mean_en = mean(profEn), sd_en = sd(profEn), mean_sp = mean(profSp), sd_sp = sd(profSp))%>%
  knitr::kable(., format = 'markdown', digits = 2)


#Average null and overt responses in English and Spanish for all groups
  

response_table_markdown <- df_null_sub %>% 
  group_by(., group, language, status) %>% 
  summarize(., meanResp = mean(respFactor))%>%
  knitr::kable(., format = 'markdown', digits = 2) 

#English analysis

# Create wide data frame with counts for each level of 'type' (overt, null)
# Create new 'n' variable

df_counts_wide <- df_null_sub %>%
  group_by(., participant, group, language, response, profEn, profSp) %>% 
  summarize(., total = sum(respFactor)) %>% 
  spread(., response, total) %>% 
  mutate(., null2 = 8 - overt, 
         n = 8) %>%
  select(., -null) %>% 
  rename(., null = null2)

#English continued

# Convert to long and plot it
df_counts_long <- df_counts_wide %>% 
  gather(., key = type, value = count, -participant, -language, -group, -n, -profEn, -profSp) %>% 
  mutate(., perc = count / 8) %>%
  arrange(., participant, language, type)

#English plot

english_plot <- df_counts_long %>% 
  filter(., language == "eng")%>%
  ggplot(., aes(x = group, y = perc, color = type, dodge = type)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', position = position_dodge(0.5), size = 1) + 
  labs(y = "% Response", x = 'Group')

# test with GLM
df_counts_wide$group <- factor(df_counts_wide$group, levels = c("l2", "mo", "hl"))
glm_null <- glm(cbind(overt, null) ~ 1, data = df_counts_wide[df_counts_wide$language == 'eng', ], family = 'binomial')
glm_add <- glm(cbind(overt, null) ~ group, data = df_counts_wide[df_counts_wide$language == 'eng', ], family = 'binomial')

anova(glm_null, glm_add, test = 'Chisq')

summary(glm_add)

#Acceptance of nulls/overts as a function of proficiency
  

prof_plot <- df_counts_long %>%
  filter(., language == 'eng') %>%
  ggplot(., aes(x = profEn, y = perc, color = type)) + 
  facet_grid(.~ group) + 
  geom_jitter() + 
  geom_smooth(method = 'lm', se = F, fullrange = T) + 
  labs(y = "% response", x = 'English proficiency')

#Model Summary

#View(df_null_sub)

mod_full <- glm(respFactor ~ profEn + group + profEn:group, data = df_null_sub[df_null_sub$language == 'eng', ], family = 'binomial')
mod_add <- glm(respFactor ~ profEn + group, data = df_null_sub[df_null_sub$language == 'eng', ], family = 'binomial')
mod_prof <- glm(respFactor ~ profEn, data = df_null_sub[df_null_sub$language == 'eng', ], family = 'binomial')
mod_group <- glm(respFactor ~ group, data = df_null_sub[df_null_sub$language == 'eng', ], family = 'binomial')
mod_null <- glm(respFactor ~ 1, data = df_null_sub[df_null_sub$language == 'eng', ], family = 'binomial')

anova(mod_add, mod_full)

anova(mod_null, mod_group, mod_prof, mod_add, mod_full, test = 'Chisq')

anova(mod_null, mod_group, mod_add, test = 'Chisq')

#Main effect of group, group + prof, and marginally significant interaction

summary(mod_full)

#Spanish analysis

#Spanish plot

spanish_plot <- df_counts_long %>% 
  filter(., language == "sp")%>%
  ggplot(., aes(x = group, y = perc, color = type, dodge = type)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', position = position_dodge(0.5), size = 1) + 
  stat_summary(fun.y = mean, geom = 'pointrange', position = position_dodge(0.5), size = 1) + 
  ylim(0, 1) + 
  labs(y = "% Response", x = 'Group')



# test with GLM
df_counts_wide$group <- factor(df_counts_wide$group, levels = c("l2", "mo", "hl"))
glm_null <- glm(cbind(overt, null) ~ 1, data = df_counts_wide[df_counts_wide$language == 'sp', ], family = 'binomial')
glm_add <- glm(cbind(overt, null) ~ group, data = df_counts_wide[df_counts_wide$language == 'sp', ], family = 'binomial')

anova(glm_null, glm_add, test = 'Chisq')

summary(glm_add)

#Acceptance of nulls/overts as a function of proficiency


prof_plot_sp <- df_counts_long %>%
  filter(., language == 'sp') %>%
  ggplot(., aes(x = profEn, y = perc, color = type)) + 
  facet_grid(.~ group) + 
  geom_jitter() + 
  geom_smooth(method = 'lm', se = F, fullrange = T) + 
  labs(y = "% response", x = 'Spanish proficiency')

#Model Summary

#View(df_null_sub)

mod_full <- glm(respFactor ~ profSp + group + profSp:group, data = df_null_sub[df_null_sub$language == 'sp', ], family = 'binomial')
mod_add <- glm(respFactor ~ profSp + group, data = df_null_sub[df_null_sub$language == 'sp', ], family = 'binomial')
mod_prof <- glm(respFactor ~ profSp, data = df_null_sub[df_null_sub$language == 'sp', ], family = 'binomial')
mod_group <- glm(respFactor ~ group, data = df_null_sub[df_null_sub$language == 'sp', ], family = 'binomial')
mod_null <- glm(respFactor ~ 1, data = df_null_sub[df_null_sub$language == 'sp', ], family = 'binomial')

anova(mod_add, mod_full)

anova(mod_null, mod_group, mod_prof, mod_add, mod_full, test = 'Chisq')
anova(mod_null, mod_full, test = 'Chisq')

summary(mod_full)

