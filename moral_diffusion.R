#### PACKAGES ####
library(tidyverse)
library(data.table)
library(lme4)
library(lmerTest)
library(reghelper)
library(emmeans)
library(ggeffects)
library(vader)

#### CLINTON/TRUMP ####

### Data ----
clinton <- read.csv("clinton_ddr.csv")
trump <- read.csv("trump_ddr.csv")

# source variable
trump$source = 0
clinton$source = 1

# Combined dataset
pres <- combine(trump, clinton)

# Rename columns
pres <- pres %>%
  dplyr::rename(Vice = Vice.txt,
                Virtue = Virtue.txt,
                Moral = Moral.txt)

# Log-transform retweet count
pres$retweet_log <- log(pres$retweet_count)

### Main Effects ---
pres_model <- lm(retweet_log ~ Virtue + Vice + source, data = pres)
summary(pres_model)

#### Interaction ----
pres_model_int <- lm(retweet_log ~ Virtue + Vice + source +
                   Virtue*source + Vice*source, data = pres)
summary(pres_model_int)

### Robustness Checks ----
# Number of tweets that day by each author
pres <- pres %>% dplyr::add_count(source, time)

# Average morality that day
pres <- pres %>% dplyr::group_by(time) %>% 
  dplyr::mutate(mean_pos_morality = mean(Virtue), mean_neg_morality = mean(Vice))

# Total num of tweets that day
pres <- pres %>% dplyr::add_count(time, name = 'total_n')

## Emotionality checks
# VADER
vader_pres <- vader_df(pres$text_cleaned)

# Merge VADER scores w/ data
pres_full <- merge(pres, vader_pres, by.x = "text_cleaned", by.y = "text")

# Run model
pres_model_rc <- lm(retweet_log ~ Virtue + Vice + source + n + mean_neg_morality +
                   mean_pos_morality + total_n + pos + neg + Virtue*source + Vice*source, 
                 data = pres_full)
summary(pres_model_rc)

### By-foundation analyses ----
# Read in loadings
trump_loadings <- read.table("document_dictionary_loadings_trump_allfoundations.tsv", header = TRUE)
clinton_loadings <- read.table("document_dictionary_loadings_clinton_allfoundations.tsv", header = TRUE)

# Combine with retweets
trump_10 <- merge(trump, trump_loadings, by = "ID")
clinton_10 <- merge(clinton, clinton_loadings, by = "ID")

# add source variable
trump_10$source = 0
clinton_10$source = 1

# Combine
pres_10 <- combine(trump_10, clinton_10)

# Rename columns
pres_10 <- pres_10 %>%
  dplyr::rename(LoyaltyVirtue = loyaltyVirtue_base,
                LoyaltyVice = loyaltyVice_base,
                AuthorityVirtue = authorityVirtue_base,
                AuthorityVice = authorityVice_base,
                FairnessVice = fairnessVice_base,
                FairnessVirtue = fairnessVirtue_base,
                CareVirtue = careVirtue_base,
                CareVice = careVice_base,
                PurityVirtue = sancityVirtue_base,
                PurityVice = sancityVice_base)

# Create retweet log variable
pres_10$retweet_log <- log(pres_10$retweet_count + 1)

# Model
pres10_model <- lm(retweet_log ~ CareVirtue + CareVice + FairnessVirtue + FairnessVice + 
                     LoyaltyVirtue + LoyaltyVice + AuthorityVirtue + AuthorityVice +
                     PurityVirtue + PurityVice + source, data = pres_10)
summary(pres10_model)

#### CONGRESS ####
### Data and preprocessing ----
tweets <- NULL
# WARNING: If data are not present locally, this will download a ~543 MB file - don't run over a metered connection
# try to read local copy
try( tweets <- fread('wang_congress_ddr.csv', data.table = FALSE ), silent = TRUE )

if( is.null(tweets) ) {
  # local read failed, get it remotely
  tweets <- fread("https://dataverse.harvard.edu/api/access/datafile/4600376", 
                  data.table = FALSE )
  # save for future runs
  fwrite(tweets, file = 'wang_congress_ddr.csv')
}

# Read in VADER scores (these were run in Python)
tweets_vader <- fread('congress_tweets_vader.csv')
# Merge
congress_full <- merge(tweets, tweets_vader, by.x = "generated_id", by.y = "id")

# Create variables
congress_full$retweet_log <- log(congress_full$retweets + 1)
congress_full$followers_log <- log(congress_full$followers + 1)
# standardizing makes simple slopes test easier later
congress_full$dw_score <- scale(congress_full$dim1) 

# Group mean centering
congress_full <- congress_full %>%
  group_by(author) %>%
  dplyr::mutate(
                Virtue_gc = Virtue - mean(Virtue, na.rm=TRUE),
                Vice_gc = Vice - mean(Vice, na.rm=TRUE),
                
                Carevirtue_gc = Carevirtue - mean(Carevirtue, na.rm=TRUE),
                Carevice_gc = Carevice - mean(Carevice, na.rm=TRUE),
                
                Fairnessvirtue_gc = Fairnessvirtue - mean(Fairnessvirtue, na.rm=TRUE),
                Fairnessvice_gc = Fairnessvice - mean(Fairnessvice, na.rm=TRUE),
                
                Loyaltyvirtue_gc = Loyaltyvirtue - mean(Loyaltyvirtue, na.rm=TRUE),
                Loyaltyvice_gc = Loyaltyvice - mean(Loyaltyvice, na.rm=TRUE),
                
                Authorityvirtue_gc = Authorityvirtue - mean(Authorityvirtue, na.rm=TRUE),
                Authorityvice_gc = Authorityvice - mean(Authorityvice, na.rm=TRUE),
                
                Purityvirtue_gc = Purityvirtue - mean(Purityvirtue, na.rm=TRUE),
                Purityvice_gc = Purityvice - mean(Purityvice, na.rm=TRUE),
                
                Virtue_meanauthor = mean(Virtue, na.rm=TRUE),
                Vice_meanauthor = mean(Vice, na.rm=TRUE),
                
                Carevirtue_meanauthor = mean(Carevirtue, na.rm=TRUE),
                Carevice_meanauthor = mean(Carevice, na.rm=TRUE),
                
                Fairnessvirtue_meanauthor = mean(Fairnessvirtue, na.rm=TRUE),
                Fairnessvice_meanauthor = mean(Fairnessvice, na.rm=TRUE),
                
                Loyaltyvirtue_meanauthor = mean(Loyaltyvirtue, na.rm=TRUE),
                Loyaltyvice_meanauthor = mean(Loyaltyvice, na.rm=TRUE),
                
                Authorityvirtue_meanauthor = mean(Authorityvirtue, na.rm=TRUE),
                Authorityvice_meanauthor = mean(Authorityvice, na.rm=TRUE),
                
                Purityvirtue_meanauthor = mean(Purityvirtue, na.rm=TRUE),
                Purityvice_meanauthor = mean(Purityvice, na.rm=TRUE)  
                ) %>%
  ungroup()

### Main effects ----
congress_full_model <- lmer(retweet_log ~ followers_log + Virtue_meanauthor + Vice_meanauthor + 
                                  Virtue_gc + Vice_gc + dw_score +
                                  (0 + Virtue_gc + Vice_gc|author) + (1|author),
                                  data = congress_full, REML=TRUE)
summary(congress_full_model)

### Interactions ----
congress_full_model_int <- lmer(retweet_log ~ followers_log + Virtue_meanauthor + Vice_meanauthor + 
                          Virtue_gc + Vice_gc + dw_score + Virtue_gc * dw_score + 
                          Vice_gc * dw_score + Virtue_meanauthor * dw_score + 
                          Vice_meanauthor * dw_score + (0 + Virtue_gc + Vice_gc|author) + (1|author),
                          data = congress_full, REML=TRUE)
summary(congress_full_model_int)

# test of simple slopes
simple_slopes(congress_full_model_int, levels=list(dw_score=c(-1, 1)))

# Need to re-fit the model to get the Vice simple slope
congress_full_model_int <- lmer(retweet_log ~ followers_log + Virtue_meanauthor + Vice_meanauthor + 
                          Virtue_gc + Vice_gc + dw_score + Vice_gc * dw_score + 
                          Virtue_gc * dw_score + (0 + Virtue_gc + Vice_gc|author) +
                          (1|author),
                          data = congress_full, REML=TRUE)

# test of Virtue simple slopes
simple_slopes(congress_full_model_int, levels=list(dw_score=c(-1, 1)))

### Robustness checks ---
# Create variables for robustness checks
# Number of tweets that day by each author
congress_full <- congress_full %>% dplyr::add_count(author, totaldays)

# Average morality that day
congress_full <- congress_full %>% dplyr::group_by(totaldays) %>%
  dplyr::mutate(mean_pos_morality = mean(Virtue), mean_neg_morality = mean(Vice))

# Total num of tweets that day
congress_full <- congress_full %>% dplyr::add_count(totaldays, name = 'total_n')
congress_full$total_n_log <- log(congress_full$total_n)
# CWC for positive/negative language
congress_full <- congress_full %>%
  group_by(author) %>%
  dplyr::mutate(pos_gc = pos - mean(pos, na.rm=TRUE),
                neg_gc = neg - mean(neg, na.rm=TRUE),
                pos_meanauthor = mean(pos, na.rm=TRUE),
                neg_meanauthor = mean(neg, na.rm=TRUE)) %>%
  ungroup

# Model
dw_congress_robustness_full <- lmer(retweet_log ~ followers_log + Virtue_meanauthor + Vice_meanauthor + 
                                      Virtue_gc + Vice_gc + dw_score +
                                      n + mean_neg_morality + mean_pos_morality + total_n_log + pos_gc +
                                      neg_gc + pos_meanauthor + neg_meanauthor + Virtue_gc * dw_score + 
                                      Vice_gc * dw_score + Virtue_meanauthor * dw_score + 
                                      Vice_meanauthor * dw_score + (0 + Virtue_gc + Vice_gc|author) + (1|author),
                                    data = congress_full, REML = TRUE)
summary(dw_congress_robustness_full)

### Separate Models per Foundation ----
# Care
# Main effects only
dw_congress_care <- lmer(retweet_log ~ followers_log + Carevirtue_meanauthor + Carevice_meanauthor + 
                           Carevirtue_gc + Carevice_gc + dw_score + 
                           (0 + Carevirtue_gc + Carevice_gc|author) + (1|author),
                         data = congress_full, REML=TRUE)
summary(dw_congress_care)

# Care - interaction
dw_congress_care_int <- lmer(retweet_log ~ followers_log + 
                               Carevirtue_meanauthor + Carevice_meanauthor +
                               Carevirtue_gc + Carevice_gc +
                               dw_score + 
                               Carevice_gc * dw_score + Carevirtue_gc * dw_score +
                               (0 + Carevirtue_gc + Carevice_gc|author) + (1|author),
                             data = congress_full, REML=TRUE)
summary(dw_congress_care_int)
# simple slopes
simple_slopes(dw_congress_care_int, levels=list(dw_score=c(-1, 1)))

# Fairness
dw_congress_fairness <- lmer(retweet_log ~ followers_log + Fairnessvirtue_meanauthor + Fairnessvice_meanauthor + 
                               Fairnessvirtue_gc + Fairnessvice_gc + dw_score + 
                               (0 + Fairnessvirtue_gc + Fairnessvice_gc|author) + (1|author),
                             data = congress_full, REML=TRUE)
summary(dw_congress_fairness)

# Fairness - interaction
dw_congress_fairness_int <- lmer(retweet_log ~ followers_log + 
                                   Fairnessvirtue_meanauthor + Fairnessvice_meanauthor +
                                   Fairnessvirtue_gc + Fairnessvice_gc +
                                   dw_score + 
                                   Fairnessvice_gc * dw_score + Fairnessvirtue_gc * dw_score +
                                   (0 + Fairnessvirtue_gc + Fairnessvice_gc|author) + (1|author),
                                 data = congress_full, REML=TRUE)
summary(dw_congress_fairness_int)
# simple slopes
simple_slopes(dw_congress_fairness_int, levels=list(dw_score=c(-1, 1)))

# Loyalty
dw_congress_loyalty <- lmer(retweet_log ~ followers_log + Loyaltyvirtue_meanauthor + Loyaltyvice_meanauthor + 
                              Loyaltyvirtue_gc + Loyaltyvice_gc + dw_score + 
                              (0 + Loyaltyvirtue_gc + Loyaltyvice_gc|author) + (1|author),
                            data = congress_full, REML=TRUE)
summary(dw_congress_loyalty)

# Loyalty - interaction
dw_congress_loyalty_int <- lmer(retweet_log ~ followers_log + 
                                  Loyaltyvirtue_meanauthor + Loyaltyvice_meanauthor +
                                  Loyaltyvirtue_gc + Loyaltyvice_gc +
                                  dw_score + 
                                  Loyaltyvice_gc * dw_score + Loyaltyvirtue_gc * dw_score + 
                                  (0 + Loyaltyvirtue_gc + Loyaltyvice_gc|author) + (1|author),
                                data = congress_full, REML=TRUE)
summary(dw_congress_loyalty_int)
# simple slopes
simple_slopes(dw_congress_loyalty_int, levels=list(dw_score=c(-1, 1)))

# Authority
dw_congress_authority <- lmer(retweet_log ~ followers_log + Authorityvirtue_meanauthor + Authorityvice_meanauthor + 
                                Authorityvirtue_gc + Authorityvice_gc + dw_score + 
                                (0 + Authorityvirtue_gc + Authorityvice_gc|author) + (1|author),
                              data = congress_full, REML=TRUE)
summary(dw_congress_authority)

# Authority - interaction
dw_congress_authority_int <- lmer(retweet_log ~ followers_log + 
                                    Authorityvirtue_meanauthor + Authorityvice_meanauthor +
                                    Authorityvirtue_gc + Authorityvice_gc +
                                    dw_score + 
                                    Authorityvice_gc * dw_score + Authorityvirtue_gc * dw_score + 
                                    (0 + Authorityvirtue_gc + Authorityvice_gc|author) + (1|author),
                                  data = congress_full, REML=TRUE)
summary(dw_congress_authority_int)
# simple slopes
simple_slopes(dw_congress_authority_int, levels=list(dw_score=c(-1, 1)))

# Purity
dw_congress_purity <- lmer(retweet_log ~ followers_log + Purityvirtue_meanauthor + Purityvice_meanauthor + 
                             Purityvirtue_gc + Purityvice_gc + dw_score + 
                             (0 + Purityvirtue_gc + Purityvice_gc|author) + (1|author),
                           data = congress_full, REML=TRUE)
summary(dw_congress_purity)

# Purity - interaction
dw_congress_purity_int <- lmer(retweet_log ~ followers_log + 
                                 Purityvirtue_meanauthor + Purityvice_meanauthor +
                                 Purityvirtue_gc + Purityvice_gc +
                                 dw_score + 
                                 Purityvice_gc * dw_score +Purityvirtue_gc * dw_score +
                                 (0 + Purityvirtue_gc + Purityvice_gc|author) + (1|author),
                               data = congress_full, REML=TRUE)
summary(dw_congress_purity_int)
# simple slopes
simple_slopes(dw_congress_purity, levels=list(dw_score=c(-1, 1)))

#### PLOTS ####

### Pres ----
# lm model
pres_model_plot <- lm(retweet_log ~ Virtue + Vice + source + Virtue*source + Vice * source, data = pres)

pres.emmeans.vice <- emmeans(pres_model_plot, c("Vice", "source"), 
                            at=list(Vice = seq(min(pres$Vice), 
                            max(pres$Vice), .01), source = c(1,0)))

# Need to recode numeric source variable
pres.emmeans.vice <- as.data.frame(pres.emmeans.vice)
pres.emmeans.vice$source <- dplyr::recode(pres.emmeans.vice$source, `1`="Clinton", `0`="Trump")

pres.emmeans.vice$exp_rt <- exp(pres.emmeans.vice$emmean)
pres.emmeans.vice$exp.LCL <- exp(pres.emmeans.vice$upper.CL)
pres.emmeans.vice$exp.UCL <- exp(pres.emmeans.vice$lower.CL)

# Vice plot (pres)
pres_plot_vice <- ggplot()
pres_plot_vice + geom_line(data=pres.emmeans.vice, aes(x = Vice, y = exp_rt, colour = source)) + 
  geom_ribbon(data=pres.emmeans.vice, aes(x = Vice, y = exp_rt, group = source, ymin=exp.LCL, ymax=exp.UCL), alpha = .3) +
  geom_point(data=pres, aes(x = Vice, y = retweet_count, colour = dplyr::recode(source, `1`="Clinton", `0`="Trump")), alpha = 0.2, size = 0.5) +
  coord_cartesian(ylim = c(0,10000)) +
  xlab("Negative Moral Language Loading") + ylab("Predicted Retweets") +
  scale_color_manual(name = "Source", values = c("blue3","red3"))

# Virtue plot (pres)
pres.emmeans.virtue <- emmeans(pres_model_plot, c("Virtue", "source"), at=list(Virtue = seq(min(pres$Virtue), max(pres$Virtue), .01), source = c(1,0)))

# Need to recode numeric "source" variable
pres.emmeans.virtue <- as.data.frame(pres.emmeans.virtue)
pres.emmeans.virtue$source <- dplyr::recode(pres.emmeans.virtue$source, `1`="Clinton", `0`="Trump")

pres.emmeans.virtue$exp_rt <- exp(pres.emmeans.virtue$emmean)
pres.emmeans.virtue$exp.LCL <- exp(pres.emmeans.virtue$upper.CL)
pres.emmeans.virtue$exp.UCL <- exp(pres.emmeans.virtue$lower.CL)


pres_plot_virtue <- ggplot()
pres_plot_virtue + geom_line(data=pres.emmeans.virtue, aes(x = Virtue, y = exp_rt, colour = source)) + 
  geom_ribbon(data=pres.emmeans.virtue, aes(x = Virtue, y = exp_rt, group = source, ymin=exp.LCL, ymax=exp.UCL), alpha = .3) +
  geom_point(data=pres, aes(x = Virtue, y = retweet_count, colour = dplyr::recode(source, `1`="Clinton", `0`="Trump")), alpha = 0.2, size = 0.5) +
  coord_cartesian(ylim = c(0,10000)) +
  xlab("Positive Moral Language Loading") + ylab("Predicted Retweets") +
  scale_color_manual(name = "Source", values = c("blue3","red3")) 

### Congress ----
pred.vice <- ggpredict(congress_full_model_int, terms = c("Vice_gc [all]", "dw_score [-1,1]"))
pred.vice <- pred.vice %>% mutate(
  exp.rt = exp(predicted),
  exp.asymp.LCL = exp(conf.low),
  exp.asymp.UCL = exp(conf.high)
)

congress_plot_vice <- ggplot(pred.vice, aes(x = x, y = exp.rt, group = group))
congress_plot_vice + geom_line(aes(colour=group)) + geom_ribbon(aes(ymin=exp.asymp.LCL, ymax=exp.asymp.UCL), alpha = .3) +
  xlab("Account-Centered Negative Language Loadings") + ylab("Predicted Retweets") +
  scale_color_manual(name = "DW-NOMINATE", values = c("blue3", "red3"), labels = c("-1SD (Liberal)", "+1SD (Conservative)")) 

# virtue
pred.virtue <- ggpredict(congress_full_model_int,terms = c("Virtue_gc [all]", "dw_score [-1,1]"))
pred.virtue <- pred.virtue %>% mutate(
  exp.rt = exp(predicted),
  exp.asymp.LCL = exp(conf.low),
  exp.asymp.UCL = exp(conf.high)
)

congress_plot_virtue <- ggplot(pred.virtue, aes(x = x, y = exp.rt, group = group))
congress_plot_virtue + geom_line(aes(colour=group)) + geom_ribbon(aes(ymin=exp.asymp.LCL, ymax=exp.asymp.UCL), alpha = .3) +
  xlab("Account-Centered Positive Moral Language Loadings") + ylab("Predicted Retweets") +
  scale_color_manual(name = "DW-NOMINATE", values = c("blue3", "red3"), labels = c("-1SD (Liberal)", "+1SD (Conservative)")) 

#### MISC ----
#### Correlations ----
# Correlations between moral loadings & pos/neg sentiment scores #
# In presidential candidate data
pres_merged <- merge(pres_full, pres_10, by=c("ID", "source"))
cor_vars <- c("Virtue.x", "Vice.x", "pos", "neg", "CareVirtue", "CareVice", "FairnessVirtue", "FairnessVice", "AuthorityVirtue",
              "AuthorityVice", "LoyaltyVirtue", "LoyaltyVice", "PurityVirtue", "PurityVice")
pres_cor <- select(pres_merged, cor_vars)
pres_cor_table <- cor(pres_cor)
round(pres_cor_table, 2)

# In congressional data
cor_vars_congress <- c("Virtue", "Vice", "pos", "neg", "Carevirtue", "Carevice", "Fairnessvirtue", "Fairnessvice", "Authorityvirtue",
              "Authorityvice", "Loyaltyvirtue", "Loyaltyvice", "Purityvirtue", "Purityvice")
cong_cor <- select(congress_full, cor_vars_congress)
cong_cor_table <- cor(cong_cor)
round(cong_cor_table, 2)

