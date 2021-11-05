#### PACKAGES ####
library(tidyverse)
library(car)
library(Rmisc)
library(data.table)
library(emmeans)
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

# WARNING: If data are not present locally, this will download a ~88 MB file - don't run over a metered connection
congress <- NULL
# try to read local copy
try( congress <- fread('brady_congress_ddr.csv', data.table = FALSE ), silent = TRUE )

if( is.null(congress) ) {
  # local read failed, get it remotely
  congress <- fread("https://dataverse.harvard.edu/api/access/datafile/4600378", 
                  data.table = FALSE )
  # save for future runs
  fwrite(congress, file = 'brady_congress_ddr.csv')
}

# Log-transform retweet count
congress$retweet_log <- log(congress$retweet_count + 1)

# Rename columns
congress <- congress %>%
  dplyr::rename(Vice = Vice.txt,
                Virtue = Virtue.txt,
                Moral = Moral.txt)

###  Models ----
### DDR models

# Set up variables
congress$followers1000 <- congress$followers/1000
congress$age10 <- congress$age/10

# Mean center predictors
congress$followers1000mc <- scale(congress$followers1000, scale = FALSE)
congress$Virtuemc <- scale(congress$Virtue, scale = FALSE)
congress$Vicemc <- scale(congress$Vice, scale = FALSE)
congress$dw_score_mc <- scale(congress$dw_score, scale = FALSE)
congress$dwextr_rs_mc <- scale(congress$dwextr_rs, scale = FALSE)
congress$age10mc <- scale(congress$age10, scale = FALSE)

# Main model
main_model <- geeglm(retweet_log ~ followers1000mc + media + Virtuemc + Vicemc,
                           id = elite, data = congress, corstr = "exchangeable")
summary(main_model)

# DW-NOMINATE

# With interaction terms
dw_model <- geeglm(retweet_log ~ followers1000mc + media + Virtuemc + Vicemc + dw_score_mc +
                           Virtuemc * dw_score_mc + Vicemc * dw_score_mc,
                         id = elite, data = congress, corstr = "exchangeable")
summary(dw_model)

#### LARGER CONGRESS ####
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
)
# Create columns for analysis
byday.author$retweet_log <- log(byday.author$retweets + 1)
byday.author$followers_sc <- scale(byday.author$followers)
byday.author$Virtue_sc <- scale(byday.author$Virtue)
byday.author$Vice_sc <- scale(byday.author$Vice)
byday.author$dw_score_sc <- scale(byday.author$dw_score)

byday.author <- na.omit(byday.author)
byday.author$author <- as.factor(byday.author$author)

# Group mean centering
byday.author <- byday.author %>%
  group_by(author) %>%
  dplyr::mutate(Virtue_gc = Virtue - mean(Virtue, na.rm=TRUE),
                Vice_gc = Vice - mean(Vice, na.rm=TRUE),
                Virtue_meanauthor = mean(Virtue, na.rm=TRUE),
                Vice_meanauthor = mean(Vice, na.rm=TRUE)) %>%
  ungroup

#### Models ####

# Main effects only
dw_congress_maineffects <- lmer(retweet_log ~ followers_sc + Virtue_meanauthor + Vice_meanauthor + 
                            Virtue_gc + Vice_gc + dw_score_sc + (1|author),
                          data = byday.author, REML=TRUE)
summary(dw_congress_maineffects)

# With interactions
dw_congress_mlm <- lmer(retweet_log ~ followers_sc + Virtue_meanauthor + Vice_meanauthor + 
                         Virtue_gc + Vice_gc + dw_score_sc + Virtue_gc * dw_score_sc + 
                         Vice_gc * dw_score_sc + Virtue_meanauthor * dw_score_sc + 
                         Vice_meanauthor * dw_score_sc + (1|author),
                       data = byday.author, REML=TRUE)
summary(dw_congress_mlm)

#### Robustness Checks ####
dw_congress_robustness <- lmer(retweet_log ~ bs(totaldays) + followers_sc + Virtue_meanauthor + Vice_meanauthor + 
                                 Virtue_gc + Vice_gc + dw_score_sc +
                                 n_mc + mean_neg_morality_mc + mean_pos_morality_mc + total_n_mc + pos_sc +
                                 neg_sc + Virtue_gc * dw_score_sc + 
                                 Vice_gc * dw_score_sc + Virtue_meanauthor * dw_score_sc + 
                                 Vice_meanauthor * dw_score_sc + (1|author),
                               data = byday.author, REML = TRUE)
summary(dw_congress_robustness)

#### PLOTS ####

### Pres ----

# lm model
pres_model_plot <- lm(retweet_log ~ Virtue + Vice + source + Virtue*source + Vice * source, data = pres)

pres.emmeans.vice <- emmeans(pres_model_plot, c("Vice", "source"), at=list(Vice = seq(min(pres$Vice), max(pres$Vice), .01), source = c("clinton","trump")))

pres.emmeans.vice <- as.data.frame(pres.emmeans.vice)

pres.emmeans.vice$exp_rt <- exp(pres.emmeans.vice$emmean)
pres.emmeans.vice$exp.LCL <- exp(pres.emmeans.vice$upper.CL)
pres.emmeans.vice$exp.UCL <- exp(pres.emmeans.vice$lower.CL)

# Vice plot (pres) - NEW
pres_plot_vice <- ggplot()
pres_plot_vice + geom_line(data=pres.emmeans.vice, aes(x = Vice, y = exp_rt, colour = source)) + 
  geom_ribbon(data=pres.emmeans.vice, aes(x = Vice, y = exp_rt, group = source, ymin=exp.LCL, ymax=exp.UCL), alpha = .3) +
  geom_point(data=pres, aes(x = Vice, y = retweet_count, colour = source), alpha = 0.2, size = 0.5) +
  coord_cartesian(ylim = c(0,10000)) +
  xlab("Negative Moral Language Loading") + ylab("Predicted Retweets") +
  scale_color_manual(name = "Source", values = c("blue3","red3"), labels = c("Clinton", "Trump")) 

# Virtue plot pres (new)
pres.emmeans.virtue <- emmeans(pres_model_plot, c("Virtue", "source"), at=list(Virtue = seq(min(pres$Virtue), max(pres$Virtue), .01), source = c("clinton","trump")))

pres.emmeans.virtue <- as.data.frame(pres.emmeans.virtue)

pres.emmeans.virtue$exp_rt <- exp(pres.emmeans.virtue$emmean)
pres.emmeans.virtue$exp.LCL <- exp(pres.emmeans.virtue$upper.CL)
pres.emmeans.virtue$exp.UCL <- exp(pres.emmeans.virtue$lower.CL)


pres_plot_virtue <- ggplot()
pres_plot_virtue + geom_line(data=pres.emmeans.virtue, aes(x = Virtue, y = exp_rt, colour = source)) + 
  geom_ribbon(data=pres.emmeans.virtue, aes(x = Virtue, y = exp_rt, group = source, ymin=exp.LCL, ymax=exp.UCL), alpha = .3) +
  geom_point(data=pres, aes(x = Virtue, y = retweet_count, colour = source), alpha = 0.2, size = 0.5) +
  coord_cartesian(ylim = c(0,10000)) +
  xlab("Positive Moral Language Loading") + ylab("Predicted Retweets") +
  scale_color_manual(name = "Source", values = c("blue3","red3"), labels = c("Clinton", "Trump")) 

### Congress ----

# YI - altered 3/22/21
full.congress.emmeans.vice <- emmeans(dw_model_congress_large, c("Vicemc", "dw_score_mc"), 
                                      at=list(Vicemc = seq(min(tweets$Vice), .35, .01),
                                              dw_score_mc = c(-1*sd(byday.author$dw_score_mc,na.rm=T), 
                                                              sd(byday.author$dw_score_mc,na.rm=T))))
full.congress.emmeans.vice <- as.data.frame(full.congress.emmeans.vice)

full.congress.emmeans.vice$exp_rt <- exp(full.congress.emmeans.vice$emmean)
full.congress.emmeans.vice$exp.asymp.LCL <- exp(full.congress.emmeans.vice$asymp.LCL)
full.congress.emmeans.vice$exp.asymp.UCL <- exp(full.congress.emmeans.vice$asymp.UCL)

full.congress.emmeans.vice$dw_score_mc <- as.character(full.congress.emmeans.vice$dw_score_mc)
byday.author$dw_score_mc <- as.character(byday.author$dw_score_mc)

congress_plot_vice <- ggplot(full.congress.emmeans.vice, aes(x = Vicemc, y = exp_rt, group = dw_score_mc))
congress_plot_vice + geom_line(aes(colour=dw_score_mc)) + geom_ribbon(aes(ymin=exp.asymp.LCL, ymax=exp.asymp.UCL), alpha = .3) +
  xlab("Mean-Centered Negative Moral Language Loadings") + ylab("Predicted Retweets") +
  scale_color_manual(name = "DW-NOMINATE", values = c("blue3", "red3"), labels = c("-1SD (Liberal)", "+1SD (Conservative)")) 

# virtue
full.congress.emmeans.virtue <- emmeans(dw_model_congress_large, c("Virtuemc", "dw_score_mc"), 
                                        at=list(Virtuemc = seq(min(tweets$Virtue), .35, .01),
                                                dw_score_mc = c(-1*sd(byday.author$dw_score_mc,na.rm=T), 
                                                                sd(byday.author$dw_score_mc,na.rm=T))))
full.congress.emmeans.virtue <- as.data.frame(full.congress.emmeans.virtue)

full.congress.emmeans.virtue$exp_rt <- exp(full.congress.emmeans.virtue$emmean)
full.congress.emmeans.virtue$exp.asymp.LCL <- exp(full.congress.emmeans.virtue$asymp.LCL)
full.congress.emmeans.virtue$exp.asymp.UCL <- exp(full.congress.emmeans.virtue$asymp.UCL)

full.congress.emmeans.virtue$dw_score_mc <- as.character(full.congress.emmeans.virtue$dw_score_mc)
byday.author$dw_score_mc <- as.character(byday.author$dw_score_mc)

congress_plot_virtue <- ggplot(full.congress.emmeans.virtue, aes(x = Virtuemc, y = exp_rt, group = dw_score_mc))
congress_plot_virtue + geom_line(aes(colour=dw_score_mc)) + geom_ribbon(aes(ymin=exp.asymp.LCL, ymax=exp.asymp.UCL), alpha = .3) +
  xlab("Mean-Centered Positive Moral Language Loadings") + ylab("Predicted Retweets") +
  scale_color_manual(name = "DW-NOMINATE", values = c("blue3", "red3"), labels = c("-1SD (Liberal)", "+1SD (Conservative)")) 


# Congress plot (no party effects)        
congress_plot_data <- read.csv('/Users/ninawang/Dropbox/Dropbox (Personal)/brady/congress_plot_emmeans.csv')
congress_plot_data$exp_rt <- exp(congress_plot_data$emmean)
congress_plot_data$exp.asymp.LCL <- exp(congress_plot_data$asymp.LCL)
congress_plot_data$exp.asymp.UCL <- exp(congress_plot_data$asymp.UCL)
congress_plot <- ggplot(congress_plot_data, aes(x = mean_loading, y = exp_rt, group = moral_cat))

congress_plot + geom_point(aes(colour=moral_cat)) + 
  geom_errorbar(aes(x = mean_loading, ymin = exp.asymp.LCL, ymax = exp.asymp.UCL, colour = moral_cat), width = 0.005, size = 0.5) +
  geom_line(aes(colour=moral_cat)) +  xlab("Mean-Centered Moral Loadings") + ylab("Predicted Retweets") +
  scale_color_manual(name = "Moral Category", values = c("deepskyblue3", "violetred")) 
