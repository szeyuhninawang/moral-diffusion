#### PACKAGES ####
library(tidyverse)
library(car)
library(Rmisc)
library(data.table)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(lme4)
library(lmerTest)
library(splines)
library(vader)

#### CLINTON ####

### Data ----
clinton <- read.csv("clinton_ddr.csv")

# Log-transform retweet count
clinton$retweet_log <- log(clinton$retweet_count + 1)

# Rename columns
clinton <- clinton %>%
  dplyr::rename(Vice = Vice.txt,
                Virtue = Virtue.txt,
                Moral = Moral.txt)

###  Models ----
clinton_model <- lm(retweet_log ~ Vice + Virtue, data = clinton)
summary(clinton_model)

#### TRUMP ####

### Data ----
trump <- read.csv("trump_ddr.csv")

# Log-transform retweet count
trump$retweet_log <- log(trump$retweet_count + 1)

# Rename columns
trump <- trump %>%
  dplyr::rename(Vice = Vice.txt,
                Virtue = Virtue.txt,
                Moral = Moral.txt)

###  Models ----
trump_model <- lm(retweet_log ~ Vice + Virtue, data = trump)
summary(trump_model)

#### CLINTON + TRUMP ####
# source variable
trump$source = 0
clinton$source = 1

# Combined dataset, effect of author
pres <- combine(trump, clinton)



# Mean center predictors
pres$Virtuemc <- scale(pres$Virtue)
pres$Vicemc <- scale(pres$Vice)

# Model
pres_model <- lm(retweet_log ~ Virtuemc + Vicemc + source +
                   Virtuemc*source + Vicemc * source, data = pres)
summary(pres_model)

#### Robustness Checks ####
# Parse date variable
pres$date <- as.numeric(pres$time)

# Number of tweets that day by each author
pres <- pres %>% dplyr::add_count(source, date)
pres$n_mc <- scale(pres$n) # scale

# Average morality that day
pres <- pres %>% dplyr::group_by(date) %>% 
  dplyr::mutate(mean_pos_morality = mean(Virtue), mean_neg_morality = mean(Vice))

# Scale avg morality variables
pres$mean_neg_morality_mc <- scale(pres$mean_neg_morality)
pres$mean_pos_morality_mc <- scale(pres$mean_pos_morality)

# Total num of tweets that day
pres <- pres %>% dplyr::add_count(date, name = 'total_n')
pres$total_n_mc <- scale(pres$total_n) # scale

## Emotionality checks
# VADER
vader_pres <- vader_df(pres$text_cleaned)

# Merge VADER scores w/ data
pres_full <- merge(pres, vader_pres, by.x = "text_cleaned", by.y = "text")

# Scale VADER variables
pres_full$pos_sc <- scale(pres_full$pos)
pres_full$neg_sc <- scale(pres_full$neg)

# Run model
pres_model_rc <- lm(retweet_log ~ Virtuemc + Vicemc + source + n_mc + mean_neg_morality_mc +
                   mean_pos_morality_mc + total_n_mc + pos_sc + neg_sc + Virtuemc*source + Vicemc * source, 
                 data = pres_full)
summary(pres_model_rc)

#### Separate Foundations ####
### By-foundation analyses
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
### Data ----
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

# Create variables for robustness checks

# Number of tweets that day by each author
tweets <- tweets %>% dplyr::add_count(author, totaldays)
tweets$n_mc <- scale(tweets$n) # scale

# Average morality that day
tweets <- tweets %>% dplyr::group_by(totaldays) %>%
  dplyr::mutate(mean_pos_morality = mean(Virtue), mean_neg_morality = mean(Vice))

# Scale avg morality variables
tweets$mean_neg_morality_mc <- scale(tweets$mean_neg_morality)
tweets$mean_pos_morality_mc <- scale(tweets$mean_pos_morality)

# Total num of tweets that day
tweets <- tweets %>% dplyr::add_count(totaldays, name = 'total_n')
tweets$total_n_mc <- scale(tweets$total_n) # scale

# Read in VADER scores (these were run in Python)
tweets_vader <- fread('congress_tweets_vader.csv')
# Merge
congress_full <- merge(tweets, tweets_vader, by.x = "generated_id", by.y = "id")

# Scale VADER variables
congress_full$pos_sc <- scale(congress_full$pos)
congress_full$neg_sc <- scale(congress_full$neg)

# Summarise by day and author
byday.author <- congress_full %>% dplyr::group_by(totaldays, author) %>% dplyr::summarize(
  Virtue = mean(Virtue),
  Vice = mean(Vice),
  Carevirtue = mean(Carevirtue),
  Carevice = mean(Carevice),
  Fairnessvirtue = mean(Fairnessvirtue),
  Fairnessvice = mean(Fairnessvice),
  Loyaltyvirtue = mean(Loyaltyvirtue),
  Loyaltyvice = mean(Loyaltyvice),
  Authorityvirtue = mean(Authorityvirtue),
  Authorityvice = mean(Authorityvice),
  Purityvirtue = mean(Purityvirtue),
  Purityvice = mean(Purityvice),
  retweets = mean(retweets),
  followers = mean(followers),
  dw_score = mean(dim1),
  partyd = mean(partyd),
  n_mc = mean(n_mc),
  mean_neg_morality_mc = mean(mean_neg_morality_mc),
  mean_pos_morality_mc = mean(mean_pos_morality_mc),
  total_n_mc = mean(total_n_mc),
  pos_sc = mean(pos_sc),
  neg_sc = mean(neg_sc)
  ) %>% 
  ungroup()

# Create columns for analysis
byday.author$retweet_log <- log(byday.author$retweets + 1)
byday.author$followers_log <- log(byday.author$followers)
byday.author$dw_score_sc <- scale(byday.author$dw_score)

byday.author <- na.omit(byday.author)
byday.author$author <- as.factor(byday.author$author)

# Group mean centering
byday.author <- byday.author %>%
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

#### Models ####

# Main effects only
dw_congress_maineffects <- lmer(retweet_log ~ followers_log + Virtue_meanauthor + Vice_meanauthor + 
                            Virtue_gc + Vice_gc + dw_score_sc + (0 + Virtue_gc + Vice_gc|author) +
                            (1|author),
                            data = byday.author, REML=TRUE)
summary(dw_congress_maineffects)

# With interactions
dw_congress_mlm <- lmer(retweet_log ~ followers_log + Virtue_meanauthor + Vice_meanauthor + 
                        Virtue_gc + Vice_gc + dw_score_sc + Virtue_gc * dw_score_sc + 
                        Vice_gc * dw_score_sc + (0 + Virtue_gc + Vice_gc|author) +
                        (1|author),
                        data = byday.author, REML=TRUE)
summary(dw_congress_mlm)

# test of simple slopes
simple_slopes(dw_congress_mlm, levels=list(dw_score_sc=c(-1, 1)))

# Need to re-fit the model to get the Vice simple slope
dw_congress_mlm <- lmer(retweet_log ~ followers_log + Virtue_meanauthor + Vice_meanauthor + 
                          Virtue_gc + Vice_gc + dw_score_sc + Vice_gc * dw_score_sc + 
                          Virtue_gc * dw_score_sc + (0 + Virtue_gc + Vice_gc|author) +
                          (1|author),
                        data = byday.author, REML=TRUE)

# test of Vice simple slopes
simple_slopes(dw_congress_mlm, levels=list(dw_score_sc=c(-1, 1)))

#### Robustness Checks ####
dw_congress_robustness <- lmer(retweet_log ~ followers_log + Virtue_meanauthor + Vice_meanauthor + 
                                 Virtue_gc + Vice_gc + dw_score_sc +
                                 n_mc + mean_neg_morality_mc + mean_pos_morality_mc + total_n_mc + pos_sc +
                                 neg_sc + Virtue_gc * dw_score_sc + 
                                 Vice_gc * dw_score_sc + (Vice_gc + Virtue_gc|author),
                                 data = byday.author, REML = TRUE)
summary(dw_congress_robustness)

#### Separate Models per Foundation ####
# Care
dw_congress_care <- lmer(retweet_log ~ followers_log + 
                           Carevirtue_meanauthor + Carevice_meanauthor +
                           Carevirtue_gc + Carevice_gc +
                           dw_score_sc + 
                           Carevice_gc * dw_score_sc + Carevirtue_gc * dw_score_sc +
                           (0 + Carevirtue_gc + Carevice_gc|author) + (1|author),
                           data = byday.author, REML=TRUE)
summary(dw_congress_care)
# simple slopes
simple_slopes(dw_congress_care, levels=list(dw_score_sc=c(-1, 1)))

# Fairness
dw_congress_fairness <- lmer(retweet_log ~ followers_log + 
                           Fairnessvirtue_meanauthor + Fairnessvice_meanauthor +
                           Fairnessvirtue_gc + Fairnessvice_gc +
                           dw_score_sc + 
                           Fairnessvice_gc * dw_score_sc + Fairnessvirtue_gc * dw_score_sc +
                           (0 + Fairnessvirtue_gc + Fairnessvice_gc|author) + (1|author),
                         data = byday.author, REML=TRUE)
summary(dw_congress_fairness)
# simple slopes
simple_slopes(dw_congress_fairness, levels=list(dw_score_sc=c(-1, 1)))

# Loyalty
dw_congress_loyalty <- lmer(retweet_log ~ followers_log + 
                           Loyaltyvirtue_meanauthor + Loyaltyvice_meanauthor +
                           Loyaltyvirtue_gc + Loyaltyvice_gc +
                           dw_score_sc + 
                           Loyaltyvice_gc * dw_score_sc + Loyaltyvirtue_gc * dw_score_sc + 
                           (0 + Loyaltyvirtue_gc + Loyaltyvice_gc|author) + (1|author),
                           data = byday.author, REML=TRUE)
summary(dw_congress_loyalty)
# simple slopes
simple_slopes(dw_congress_loyalty, levels=list(dw_score_sc=c(-1, 1)))

# Authority
dw_congress_authority <- lmer(retweet_log ~ followers_log + 
                           Authorityvirtue_meanauthor + Authorityvice_meanauthor +
                           Authorityvirtue_gc + Authorityvice_gc +
                           dw_score_sc + 
                           Authorityvice_gc * dw_score_sc + Authorityvirtue_gc * dw_score_sc + 
                           (0 + Authorityvirtue_gc + Authorityvice_gc|author) + (1|author),
                           data = byday.author, REML=TRUE)
summary(dw_congress_authority)
# simple slopes
simple_slopes(dw_congress_authority, levels=list(dw_score_sc=c(-1, 1)))

# Purity
dw_congress_purity <- lmer(retweet_log ~ followers_log + 
                           Purityvirtue_meanauthor + Purityvice_meanauthor +
                           Purityvirtue_gc + Purityvice_gc +
                           dw_score_sc + 
                           Purityvice_gc * dw_score_sc +Purityvirtue_gc * dw_score_sc +
                           (0 + Purityvirtue_gc + Purityvice_gc|author) + (1|author),
                           data = byday.author, REML=TRUE)
summary(dw_congress_purity)
# simple slopes
simple_slopes(dw_congress_purity, levels=list(dw_score_sc=c(-1, 1)))

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
pred.vice <- ggpredict(dw_congress_mlm,terms = c("Vice_gc [all]", "dw_score_sc [-1,1]"))
pred.vice <- pred.vice %>% mutate(
  exp.rt = exp(predicted),
  exp.asymp.LCL = exp(conf.low),
  exp.asymp.UCL = exp(conf.high)
)

congress_plot_vice <- ggplot(pred.vice, aes(x = x, y = exp.rt, group = group))
congress_plot_vice + geom_line(aes(colour=group)) + geom_ribbon(aes(ymin=exp.asymp.LCL, ymax=exp.asymp.UCL), alpha = .3) +
  xlab("Standardized Negative Language Loadings") + ylab("Predicted Retweets") +
  scale_color_manual(name = "DW-NOMINATE", values = c("blue3", "red3"), labels = c("-1SD (Liberal)", "+1SD (Conservative)")) 

# virtue
pred.virtue <- ggpredict(dw_congress_mlm,terms = c("Virtue_gc [all]", "dw_score_sc [-1,1]"))
pred.virtue <- pred.virtue %>% mutate(
  exp.rt = exp(predicted),
  exp.asymp.LCL = exp(conf.low),
  exp.asymp.UCL = exp(conf.high)
)

congress_plot_virtue <- ggplot(pred.virtue, aes(x = x, y = exp.rt, group = group))
congress_plot_virtue + geom_line(aes(colour=group)) + geom_ribbon(aes(ymin=exp.asymp.LCL, ymax=exp.asymp.UCL), alpha = .3) +
  xlab("Standardized Positive Moral Language Loadings") + ylab("Predicted Retweets") +
  scale_color_manual(name = "DW-NOMINATE", values = c("blue3", "red3"), labels = c("-1SD (Liberal)", "+1SD (Conservative)")) 
