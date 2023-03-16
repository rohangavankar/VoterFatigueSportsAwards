rm(list=ls())
setwd("~/Moneyball")
library(tidyverse)
library(nbastatR)
library(plotly)
library(gridExtra)
library(betareg)
library(modelr)

Voter_Fatigue_Data_NBA_DPOY <- read.csv("Data/Voter Fatigue Data - NBA DPOY.csv")
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
    zDBPM = standardize(ratioDBPM),
    ) %>%
  mutate(perBPM = ratioBPM/max(ratioBPM)) %>%
  ungroup()

simple_DPOY_votes <- dataDPOYVotes %>%
  select(slugSeason, slugPlayerBREF, slugTeam, rankVotes, pctVote:isWinner)

simple_DPOY_votes <- merge(simple_DPOY_votes, team_name_to_abbreviation, by = "slugTeam")

DPOY_advanced <- merge(simple_DPOY_votes, bref_advanced_filtered, by = c("slugSeason", "slugPlayerBREF"))
#figure out how to merge simpler, use inner join

standings <- standings(seasons = c(1977:2019), season_types = c("Regular Season"),
          resolve_records = TRUE, nest_data = F, return_message = TRUE)

simple_standings <- standings %>%
  select(pctWinTeam, slugSeason, nameTeam, ptsPerGameOpponent) %>%
  group_by(slugSeason) %>%
  mutate(zPPGO = standardize(ptsPerGameOpponent)) %>%
  ungroup()

# colnames(DPOY_advanced)[which(names(DPOY_advanced) == "slugTeamBREF")] <- "slugTeam"
#there is a function called rename() which I should maybe use

DPOY_advanced <- merge(DPOY_advanced, simple_standings, by = c("slugSeason", "nameTeam"))

Voter_Fatigue_Data_NBA_DPOY <- Voter_Fatigue_Data_NBA_DPOY %>%
  select(namePlayer, yearSeason, PriorAwards, PriorShares)

DPOY_advanced <- DPOY_advanced %>%
  mutate(yearSeason = yearSeason + 1)
  
DPOY_advanced$isWinner <- as.integer(as.logical(DPOY_advanced$isWinner))
DPOY_advanced <- merge(DPOY_advanced, Voter_Fatigue_Data_NBA_DPOY, by = c("namePlayer", "yearSeason"))

#################################################################################
#Linear Model
# fit_shares <- lm(pctVote ~ zBPM + pctWinTeam, data = DPOY_advanced)
fit_shares <- lm(pctVote ~ pctWinTeam  + zDBPM + zPPGO, data = DPOY_advanced)
fit_win <- lm(isWinner ~  pctWinTeam  + zDBPM + zPPGO, data = DPOY_advanced)
summary(fit_shares)
# fit$coefficients
predicted_DPOY_shares <- data.frame(DPOY_pred = predict(fit_shares, DPOY_advanced), shares=DPOY_advanced$pctVote)

predicted_DPOY_wins <- data.frame(DPOY_pred = predict(fit_win, DPOY_advanced), isWinner=DPOY_advanced$isWinner)


gzBPM <- ggplot(data=DPOY_advanced) +
  geom_point(mapping = aes(zBPM, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5)+
  labs(title="zBPM vs. DPOY Shares", x="zBPM", y="DPOY Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()
gzBPM
gpctWinTeam <- ggplot(data=DPOY_advanced) +
  geom_point(mapping = aes(pctWinTeam, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5)+
  labs(title="Team Winning Percentage vs. DPOY Shares", x="Team Winning Percentage", y="DPOY Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()
gzPPG <- ggplot(data=DPOY_advanced) +
  geom_point(mapping = aes( zPPG, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5)+
  labs(title="zPPG vs. DPOY Shares", x="zPPG", y="DPOY Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()
gzVORP <- ggplot(data=DPOY_advanced) +
  geom_point(mapping = aes( zVORP, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5) +
  labs(title="zVORP vs. DPOY Shares", x="zVORP", y="DPOY Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()


grid.arrange(gzBPM, gpctWinTeam, gzVORP, gzPPG, ncol=2)

# DPOY_advanced <- DPOY_advanced %>%
#   filter(pctVote != 1) %>%
#   filter(pctVote > 0.1)

prediction_graph_shares <- ggplot(data=predicted_DPOY_shares) +
  geom_point(mapping=aes(x=DPOY_pred, y=shares, size=DPOY_advanced$PriorAwards, col=as.factor(DPOY_advanced$isWinner)), alpha=0.5) +
  labs(title="Predicted Shares vs. Actual Shares", x="Predicted Shares", y="Actual Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()

prediction_graph_wins <- ggplot(data=predicted_DPOY_wins) +
  geom_point(mapping=aes(x=DPOY_pred, y=isWinner, size=DPOY_advanced$PriorAwards, col=as.factor(DPOY_advanced$isWinner)), alpha=0.5) +
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


# predicted_DPOY_shares <- predicted_DPOY_shares %>%
#   filter(shares != 1) %>%
#   filter(shares > 0.02)

# logit_pred_fit_shares <- betareg(shares ~ DPOY_pred, data = predicted_DPOY_shares, link="cauchit")
logit_pred_fit_shares <- glm(shares ~ DPOY_pred, family="quasibinomial", data = predicted_DPOY_shares)
# logit_pred_fit_shares_exp <- nls(shares ~ I(DPOY_pred^power), data = predicted_DPOY_shares, start = list(power = 1.1))
logit_pred_fit_wins <- glm(isWinner ~ DPOY_pred, family="binomial", data = predicted_DPOY_wins)
summary(logit_pred_fit_shares)
# summary(logit_pred_fit_shares_exp)
summary(logit_pred_fit_wins)

logit_pred_DPOY_shares <- predicted_DPOY_shares %>%
   data_grid(DPOY_pred) %>%
   add_predictions(model = logit_pred_fit_shares, type = "response", var = "predictedShares")

# logit_pred_DPOY_shares_exp <- predicted_DPOY_shares %>%
#    data_grid(DPOY_pred) %>%
#    add_predictions(model = logit_pred_fit_shares_exp, type = "response", var = "predictedShares")

logit_pred_DPOY_wins <- predicted_DPOY_wins %>%
   data_grid(DPOY_pred) %>%
   add_predictions(model = logit_pred_fit_wins, type = "response", var = "predictedShares")
 
ggplot(data = logit_pred_DPOY_shares) + 
   geom_line(mapping = aes(x = DPOY_pred, y = predictedShares))

ggplot(data = logit_pred_DPOY_wins) + 
   geom_line(mapping = aes(x = DPOY_pred, y = predictedShares))

prediction_graph_wins <- prediction_graph_wins + geom_line(data=logit_pred_DPOY_wins, mapping = aes(x = DPOY_pred, y = predictedShares))


prediction_graph_shares <- prediction_graph_shares + geom_line(data=logit_pred_DPOY_shares, mapping = aes(x = DPOY_pred, y = predictedShares))
# prediction_graph_shares_exp <- prediction_graph_shares + geom_line(data=logit_pred_DPOY_shares_exp, mapping = aes(x = DPOY_pred, y = predictedShares))


prediction_graph_wins
prediction_graph_shares
# prediction_graph_shares_exp
ggplotly(prediction_graph_shares)

logit_pred_fit_residuals_shares <- residuals(logit_pred_fit_shares, type="response")
logit_pred_fit_residuals_wins <- residuals(logit_pred_fit_wins, type="response")
awards_resid_fit_shares <- lm(logit_pred_fit_residuals_shares ~ PriorAwards.y, data=DPOY_advanced)
awards_resid_fit_wins <- lm(logit_pred_fit_residuals_wins ~ PriorAwards.y, data=DPOY_advanced)
summary(awards_resid_fit_shares)
summary(awards_resid_fit_wins)

residual_DPOY_shares <- ggplot(data=DPOY_advanced) +
  geom_point(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_shares, col=as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason))) +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_shares), method = lm) +
  labs(title="Prior Shares vs. Residual Between Model Relating Predicted Shares and Actual Shares", x="Prior Shares", y="Residual") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?")) +
  theme_bw()
residual_DPOY_wins <- ggplot(data=DPOY_advanced) +
  geom_point(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_wins, col=as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason))) +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_wins), method = lm)+
  labs(title="Prior Shares vs. Residual Between Model Relating Predicted Winning Chance and Actual Win", x="Prior Shares", y="Residual") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?")) +
  theme_bw()

residual_DPOY_shares
residual_DPOY_wins
ggplotly(residual_DPOY_shares)
ggplotly(residual_DPOY_wins)

################################################################################
bref_players_stats(seasons = c(2020:2020), tables = c("advanced", "totals"), widen = TRUE, assign_to_environment = TRUE)
bref_advanced <- dataBREFPlayerAdvanced
bref_totals <- dataBREFPlayerTotals

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
    zDBPM = standardize(ratioDBPM),
  ) %>%
  mutate(perBPM = ratioBPM/max(ratioBPM)) %>%
  ungroup()

DPOY_advanced_2020 <- bref_advanced_filtered

standings <- standings(seasons = 2020:2020, season_types = c("Regular Season"),
                       resolve_records = TRUE, nest_data = F, return_message = TRUE)

simple_standings <- standings %>%
  select(pctWinTeam, slugSeason, nameTeam, ptsPerGameOpponent) %>%
  group_by(slugSeason) %>%
  mutate(zPPGO = standardize(ptsPerGameOpponent)) %>%
  ungroup()


DPOY_advanced_2020 <- rename(DPOY_advanced_2020, c("slugTeam" = "slugTeamBREF"))

simple_standings <- merge(simple_standings, team_name_to_abbreviation, by = "nameTeam")


DPOY_advanced_2020 <- merge(DPOY_advanced_2020, simple_standings, by = c("slugSeason", "slugTeam"))


predicted_DPOY_shares_2020 <- data.frame(DPOY_pred = predict(fit_shares, DPOY_advanced_2020), DPOY_advanced_2020$namePlayer)

DPOY_2020_predictions <- ggplot(data=predicted_DPOY_shares_2020) +
  geom_boxplot(mapping=aes(x=DPOY_pred), text=sprintf("Player: %s", DPOY_advanced_2020$namePlayer))
ggplotly(DPOY_2020_predictions, tooltip=text)


