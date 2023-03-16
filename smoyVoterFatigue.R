rm(list=ls())
setwd("~/Moneyball")
library(tidyverse)
library(nbastatR)
library(plotly)
library(gridExtra)
library(betareg)
library(modelr)

Voter_Fatigue_Data_NBA_SMOY <- read.csv("Data/Voter Fatigue Data - NBA SMOY.csv")
team_name_to_abbreviation <- read.csv("Data/Team Names and Abbreviations.csv")

bref_awards_votes(seasons = c(1977:2019))

bref_players_stats(seasons = c(1977:2019), tables = c("advanced", "totals"), widen = TRUE, assign_to_environment = TRUE)
bref_advanced <- dataBREFPlayerAdvanced
bref_totals <- dataBREFPlayerTotals

standardize <- function(x){
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  return((x - mu)/sigma)
}

bref_totals <- bref_totals %>%
  select(slugPlayerSeason, ptsTotals)

bref_advanced <- merge(bref_advanced, bref_totals, by="slugPlayerSeason")
bref_advanced_filtered <- bref_advanced %>%
  group_by(yearSeason) %>%
  mutate(pctMaxGames = countGames/max(countGames)) %>%
  filter(pctMaxGames >= 0.4) %>%
  mutate(
    zBPM = standardize(ratioBPM),
    PPG = ptsTotals/countGames,
    zPPG = standardize(PPG),
    zVORP = standardize(ratioVORP),
  ) %>%
  mutate(perBPM = ratioBPM/max(ratioBPM)) %>%
  ungroup()

simple_SMOY_votes <- dataSMOYVotes %>%
  select(slugSeason, slugPlayerBREF, slugTeam, rankVotes, pctVote:isWinner)

simple_SMOY_votes <- merge(simple_SMOY_votes, team_name_to_abbreviation, by = "slugTeam")

SMOY_advanced <- merge(simple_SMOY_votes, bref_advanced_filtered, by = c("slugSeason", "slugPlayerBREF"))
#figure out how to merge simpler, use inner join

standings <- standings(seasons = 1977:2019, season_types = c("Regular Season"),
                       resolve_records = TRUE, nest_data = F, return_message = TRUE)

simple_standings <- standings %>%
  select(pctWinTeam, slugSeason, nameTeam)

# colnames(SMOY_advanced)[which(names(SMOY_advanced) == "slugTeamBREF")] <- "slugTeam"
#there is a function called rename() which I should maybe use

SMOY_advanced <- merge(SMOY_advanced, simple_standings, by = c("slugSeason", "nameTeam"))

Voter_Fatigue_Data_NBA_SMOY <- Voter_Fatigue_Data_NBA_SMOY %>%
  select(namePlayer, yearSeason, PriorAwards, PriorShares)

SMOY_advanced <- SMOY_advanced %>%
  mutate(yearSeason = yearSeason + 1)

SMOY_advanced$isWinner <- as.integer(as.logical(SMOY_advanced$isWinner))
SMOY_advanced <- merge(SMOY_advanced, Voter_Fatigue_Data_NBA_SMOY, by = c("namePlayer", "yearSeason"))

#################################################################################
#Linear Model
# fit_shares <- lm(pctVote ~ zBPM + pctWinTeam, data = SMOY_advanced)
fit_shares <- lm(pctVote ~ zBPM + pctWinTeam + zPPG + zVORP, data = SMOY_advanced)
fit_win <- lm(isWinner ~ zBPM + pctWinTeam + zPPG + zVORP, data = SMOY_advanced)
# fit$coefficients
predicted_SMOY_shares <- data.frame(SMOY_pred = predict(fit_shares, SMOY_advanced), shares=SMOY_advanced$pctVote)
predicted_SMOY_wins <- data.frame(SMOY_pred = predict(fit_win, SMOY_advanced), isWinner=SMOY_advanced$isWinner)



gzBPM <- ggplot(data=SMOY_advanced) +
  geom_point(mapping = aes(zBPM, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5)+
  labs(title="zBPM vs. SMOY Shares", x="zBPM", y="SMOY Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()
gpctWinTeam <- ggplot(data=SMOY_advanced) +
  geom_point(mapping = aes(pctWinTeam, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5)+
  labs(title="Team Winning Percentage vs. SMOY Shares", x="Team Winning Percentage", y="SMOY Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()
gzPPG <- ggplot(data=SMOY_advanced) +
  geom_point(mapping = aes( zPPG, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5)+
  labs(title="zPPG vs. SMOY Shares", x="zPPG", y="SMOY Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()
gzVORP <- ggplot(data=SMOY_advanced) +
  geom_point(mapping = aes( zVORP, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5) +
  labs(title="zVORP vs. SMOY Shares", x="zVORP", y="SMOY Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()


grid.arrange(gzBPM, gpctWinTeam, gzVORP, gzPPG, ncol=2)

# SMOY_advanced <- SMOY_advanced %>%
#   filter(pctVote != 1) %>%
#   filter(pctVote > 0.1)

prediction_graph_shares <- ggplot(data=predicted_SMOY_shares) +
  geom_point(mapping=aes(x=SMOY_pred, y=shares, size=SMOY_advanced$PriorAwards, col=as.factor(SMOY_advanced$isWinner)), alpha=0.5) +
  labs(title="Predicted Shares vs. Actual Shares", x="Predicted Shares", y="Actual Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()

prediction_graph_wins <- ggplot(data=predicted_SMOY_wins) +
  geom_point(mapping=aes(x=SMOY_pred, y=isWinner, size=SMOY_advanced$PriorAwards, col=as.factor(SMOY_advanced$isWinner)), alpha=0.5) +
  labs(title="Projected Chance of Victory vs. Actual Win", x="Projected Chance of Victory", y="Actual Victory") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()

prediction_graph_shares
prediction_graph_wins
# gBPM <- ggplotly(gBPM, tooltip="text")
# gzBPM <- ggplotly(gzBPM, tooltip="text")
# gperBPM <- ggplotly(gperBPM, tooltip="text")


# predicted_SMOY_shares <- predicted_SMOY_shares %>%
#   filter(shares != 1) %>%
#   filter(shares > 0.02)

# logit_pred_fit_shares <- betareg(shares ~ SMOY_pred, data = predicted_SMOY_shares, link="cauchit")
logit_pred_fit_shares <- glm(shares ~ SMOY_pred, family="quasibinomial", data = predicted_SMOY_shares)
# logit_pred_fit_shares_exp <- nls(shares ~ I(SMOY_pred^power), data = predicted_SMOY_shares, start = list(power = 1.1))
logit_pred_fit_wins <- glm(isWinner ~ SMOY_pred, family="binomial", data = predicted_SMOY_wins)
summary(logit_pred_fit_shares)
# summary(logit_pred_fit_shares_exp)
summary(logit_pred_fit_wins)

logit_pred_SMOY_shares <- predicted_SMOY_shares %>%
  data_grid(SMOY_pred) %>%
  add_predictions(model = logit_pred_fit_shares, type = "response", var = "predictedShares")

# logit_pred_SMOY_shares_exp <- predicted_SMOY_shares %>%
#    data_grid(SMOY_pred) %>%
#    add_predictions(model = logit_pred_fit_shares_exp, type = "response", var = "predictedShares")

logit_pred_SMOY_wins <- predicted_SMOY_wins %>%
  data_grid(SMOY_pred) %>%
  add_predictions(model = logit_pred_fit_wins, type = "response", var = "predictedShares")

ggplot(data = logit_pred_SMOY_shares) + 
  geom_line(mapping = aes(x = SMOY_pred, y = predictedShares))

ggplot(data = logit_pred_SMOY_wins) + 
  geom_line(mapping = aes(x = SMOY_pred, y = predictedShares))

prediction_graph_wins <- prediction_graph_wins + geom_line(data=logit_pred_SMOY_wins, mapping = aes(x = SMOY_pred, y = predictedShares))
prediction_graph_shares <- prediction_graph_shares + geom_line(data=logit_pred_SMOY_shares, mapping = aes(x = SMOY_pred, y = predictedShares))
# prediction_graph_shares_exp <- prediction_graph_shares + geom_line(data=logit_pred_SMOY_shares_exp, mapping = aes(x = SMOY_pred, y = predictedShares))


prediction_graph_wins
prediction_graph_shares
# prediction_graph_shares_exp
ggplotly(prediction_graph_shares)

logit_pred_fit_residuals_shares <- residuals(logit_pred_fit_shares, type="response")
logit_pred_fit_residuals_wins <- residuals(logit_pred_fit_wins, type="response")
awards_resid_fit_shares <- lm(logit_pred_fit_residuals_shares ~ PriorAwards, data=SMOY_advanced)
awards_resid_fit_wins <- lm(logit_pred_fit_residuals_wins ~ PriorAwards, data=SMOY_advanced)
summary(awards_resid_fit_shares)
summary(awards_resid_fit_wins)

residual_SMOY_shares <- ggplot(data=SMOY_advanced) +
  geom_point(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_shares, col=as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason))) +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_shares), method = lm) +
  labs(title="Prior Shares vs. Residual Between Model Relating Predicted Shares and Actual Shares", x="Prior Shares", y="Residual") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?")) +
  theme_bw()
residual_SMOY_wins <- ggplot(data=SMOY_advanced) +
  geom_point(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_wins, col=as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason))) +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_wins), method = lm)+
  labs(title="Prior Shares vs. Residual Between Model Relating Predicted Winning Chance and Actual Win", x="Prior Shares", y="Residual") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?")) +
  theme_bw()

residual_SMOY_shares
residual_SMOY_wins
ggplotly(residual_SMOY_shares)
ggplotly(residual_SMOY_wins)

ggplotly()



