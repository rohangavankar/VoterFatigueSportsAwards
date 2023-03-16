rm(list=ls())
setwd("~/Moneyball")
library(tidyverse)
library(nbastatR)
library(plotly)
library(gridExtra)
library(betareg)
library(modelr)

Voter_Fatigue_Data_NBA_MVP <- read.csv("Data/Voter Fatigue Data - NBA MVP.csv")
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

simple_MVP_votes <- dataMVPVotes %>%
  select(slugSeason, slugPlayerBREF, slugTeam, rankVotes, pctVote:isWinner)

simple_MVP_votes <- merge(simple_MVP_votes, team_name_to_abbreviation, by = "slugTeam")

MVP_advanced <- merge(simple_MVP_votes, bref_advanced_filtered, by = c("slugSeason", "slugPlayerBREF"))
#figure out how to merge simpler, use inner join

standings <- standings(seasons = 1977:2019, season_types = c("Regular Season"),
          resolve_records = TRUE, nest_data = F, return_message = TRUE)

simple_standings <- standings %>%
  select(pctWinTeam, slugSeason, nameTeam)

# colnames(MVP_advanced)[which(names(MVP_advanced) == "slugTeamBREF")] <- "slugTeam"
#there is a function called rename() which I should maybe use

MVP_advanced <- merge(MVP_advanced, simple_standings, by = c("slugSeason", "nameTeam"))

Voter_Fatigue_Data_NBA_MVP <- Voter_Fatigue_Data_NBA_MVP %>%
  select(namePlayer, yearSeason, PriorAwards, PriorShares)

MVP_advanced <- MVP_advanced %>%
  mutate(yearSeason = yearSeason + 1)
  
MVP_advanced$isWinner <- as.integer(as.logical(MVP_advanced$isWinner))
MVP_advanced <- merge(MVP_advanced, Voter_Fatigue_Data_NBA_MVP, by = c("namePlayer", "yearSeason"))

#################################################################################
#Linear Model
# fit_shares <- lm(pctVote ~ zBPM + pctWinTeam, data = MVP_advanced)
fit_shares <- lm(pctVote ~ zBPM + pctWinTeam + zPPG + zVORP, data = MVP_advanced)
fit_win <- lm(isWinner ~ zBPM + pctWinTeam + zPPG + zVORP, data = MVP_advanced)
# fit$coefficients
predicted_mvp_shares <- data.frame(mvp_pred = predict(fit_shares, MVP_advanced), shares=MVP_advanced$pctVote)
predicted_mvp_wins <- data.frame(mvp_pred = predict(fit_win, MVP_advanced), isWinner=MVP_advanced$isWinner)



gzBPM <- ggplot(data=MVP_advanced) +
  geom_point(mapping = aes(zBPM, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5)+
  labs(title="zBPM vs. MVP Shares", x="zBPM", y="MVP Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()
gpctWinTeam <- ggplot(data=MVP_advanced) +
  geom_point(mapping = aes(pctWinTeam, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5)+
  labs(title="Team Winning Percentage vs. MVP Shares", x="Team Winning Percentage", y="MVP Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()
gzPPG <- ggplot(data=MVP_advanced) +
  geom_point(mapping = aes( zPPG, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5)+
  labs(title="zPPG vs. MVP Shares", x="zPPG", y="MVP Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()
gzVORP <- ggplot(data=MVP_advanced) +
  geom_point(mapping = aes( zVORP, y = pctVote, size = PriorAwards, color = as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason)), alpha=0.5) +
  labs(title="zVORP vs. MVP Shares", x="zVORP", y="MVP Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()


grid.arrange(gzBPM, gpctWinTeam, gzVORP, gzPPG, ncol=2)

# MVP_advanced <- MVP_advanced %>%
#   filter(pctVote != 1) %>%
#   filter(pctVote > 0.1)

prediction_graph_shares <- ggplot(data=predicted_mvp_shares) +
  geom_point(mapping=aes(x=mvp_pred, y=shares, size=MVP_advanced$PriorAwards, col=as.factor(MVP_advanced$isWinner)), alpha=0.5) +
  labs(title="Predicted Shares vs. Actual Shares", x="Predicted Shares", y="Actual Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()

prediction_graph_wins <- ggplot(data=predicted_mvp_wins) +
  geom_point(mapping=aes(x=mvp_pred, y=isWinner, size=MVP_advanced$PriorAwards, col=as.factor(MVP_advanced$isWinner)), alpha=0.5) +
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


# predicted_mvp_shares <- predicted_mvp_shares %>%
#   filter(shares != 1) %>%
#   filter(shares > 0.02)

# logit_pred_fit_shares <- betareg(shares ~ mvp_pred, data = predicted_mvp_shares, link="cauchit")
logit_pred_fit_shares <- glm(shares ~ mvp_pred, family="quasibinomial", data = predicted_mvp_shares)
# logit_pred_fit_shares_exp <- nls(shares ~ I(mvp_pred^power), data = predicted_mvp_shares, start = list(power = 1.1))
logit_pred_fit_wins <- glm(isWinner ~ mvp_pred, family="binomial", data = predicted_mvp_wins)
summary(logit_pred_fit_shares)
# summary(logit_pred_fit_shares_exp)
summary(logit_pred_fit_wins)

logit_pred_mvp_shares <- predicted_mvp_shares %>%
   data_grid(mvp_pred) %>%
   add_predictions(model = logit_pred_fit_shares, type = "response", var = "predictedShares")

# logit_pred_mvp_shares_exp <- predicted_mvp_shares %>%
#    data_grid(mvp_pred) %>%
#    add_predictions(model = logit_pred_fit_shares_exp, type = "response", var = "predictedShares")

logit_pred_mvp_wins <- predicted_mvp_wins %>%
   data_grid(mvp_pred) %>%
   add_predictions(model = logit_pred_fit_wins, type = "response", var = "predictedShares")
 
ggplot(data = logit_pred_mvp_shares) + 
   geom_line(mapping = aes(x = mvp_pred, y = predictedShares))

ggplot(data = logit_pred_mvp_wins) + 
   geom_line(mapping = aes(x = mvp_pred, y = predictedShares))

prediction_graph_wins <- prediction_graph_wins + geom_line(data=logit_pred_mvp_wins, mapping = aes(x = mvp_pred, y = predictedShares))
prediction_graph_shares <- prediction_graph_shares + geom_line(data=logit_pred_mvp_shares, mapping = aes(x = mvp_pred, y = predictedShares))
# prediction_graph_shares_exp <- prediction_graph_shares + geom_line(data=logit_pred_mvp_shares_exp, mapping = aes(x = mvp_pred, y = predictedShares))


prediction_graph_wins
prediction_graph_shares
# prediction_graph_shares_exp
ggplotly(prediction_graph_shares)

logit_pred_fit_residuals_shares <- residuals(logit_pred_fit_shares, type="response")
logit_pred_fit_residuals_wins <- residuals(logit_pred_fit_wins, type="response")
awards_resid_fit_shares <- lm(logit_pred_fit_residuals_shares ~ PriorAwards, data=MVP_advanced)
awards_resid_fit_wins <- lm(logit_pred_fit_residuals_wins ~ PriorAwards, data=MVP_advanced)
summary(awards_resid_fit_shares)
summary(awards_resid_fit_wins)

residual_MVP_shares <- ggplot(data=MVP_advanced) +
  geom_point(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_shares, col=as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason))) +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_shares), method = lm) +
  labs(title="Prior Shares vs. Residual Between Model Relating Predicted Shares and Actual Shares", x="Prior Shares", y="Residual") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?")) +
  theme_bw()
residual_MVP_wins <- ggplot(data=MVP_advanced) +
  geom_point(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_wins, col=as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s", namePlayer, slugSeason))) +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_wins), method = lm)+
  labs(title="Prior Shares vs. Residual Between Model Relating Predicted Winning Chance and Actual Win", x="Prior Shares", y="Residual") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?")) +
  theme_bw()

residual_MVP_shares
residual_MVP_wins
ggplotly(residual_MVP_shares)
ggplotly(residual_MVP_wins)




##############################################################################################
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
  ) %>%
  mutate(perBPM = ratioBPM/max(ratioBPM)) %>%
  ungroup()

MVP_advanced_2020 <- bref_advanced_filtered

standings <- standings(seasons = 2020:2020, season_types = c("Regular Season"),
                       resolve_records = TRUE, nest_data = F, return_message = TRUE)

simple_standings <- standings %>%
  select(pctWinTeam, slugSeason, nameTeam)
simple_standings <- merge(simple_standings, team_name_to_abbreviation, by="nameTeam")

MVP_advanced_2020 <- rename(MVP_advanced_2020, c("slugTeam" = "slugTeamBREF"))
MVP_advanced_2020 <- merge(MVP_advanced_2020, simple_standings, by = c("slugSeason", "slugTeam"))

Voter_Fatigue_Data_NBA_MVP <- Voter_Fatigue_Data_NBA_MVP %>%
  select(namePlayer, yearSeason, PriorAwards, PriorShares)

MVP_advanced_2020 <- MVP_advanced_2020 %>%
  mutate(yearSeason = yearSeason + 1)

MVP_advanced_2020 <- merge(MVP_advanced_2020, Voter_Fatigue_Data_NBA_MVP, by = c("namePlayer", "yearSeason"))

predicted_mvp_shares_2020 <- data.frame(mvp_pred = predict(fit_shares, MVP_advanced_2020), MVP_advanced_2020$namePlayer)

mvp_2020_predictions <- ggplot(data=predicted_mvp_shares_2020) +
  geom_boxplot(mapping=aes(x=mvp_pred), text=sprintf("Player: %s", MVP_advanced_2020$namePlayer))
ggplotly(mvp_2020_predictions, tooltip=text)
