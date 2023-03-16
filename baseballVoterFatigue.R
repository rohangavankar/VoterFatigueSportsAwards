rm(list=ls())
setwd("~/Moneyball")
library(tidyverse)
library(Lahman)
library(plotly)
library(gridExtra)
library(modelr)
library(betareg)
library(ggrepel)

data(AwardsSharePlayers)
AwardsSharePlayers <- AwardsSharePlayers
AwardsSharePlayers <- AwardsSharePlayers %>%
  mutate(shares = pointsWon/pointsMax)

war_daily_bat = read.csv("Data/war_daily_bat.csv")
war_daily_pitch = read.csv("Data/war_daily_pitch.csv")

war_daily_bat <- war_daily_bat %>%
  select(player_ID, year_ID, name_common, WAA, WAR)
war_daily_pitch <- war_daily_pitch %>%
  select(player_ID, year_ID, name_common, WAA, WAR)
war_daily_bat <- rename(war_daily_bat, c("playerID" = "player_ID", "yearID" = "year_ID", "WAAb" = "WAA", "name_commonB" = "name_common", "WARb" = "WAR"))
war_daily_pitch <- rename(war_daily_pitch, c("playerID" = "player_ID", "yearID" = "year_ID", "WAAp" = "WAA", "name_commonP" = "name_common", "WARp" = "WAR"))

baseball_awards <- read.csv("Data/Voter Fatigue Data - MLB Awards.csv")
fangraphs_FIP <- read.csv("Data/FanGraphs FIP.csv")
colnames(fangraphs_FIP)[1] <- gsub('^...','',colnames(fangraphs_FIP)[1])
fangraphs_FIP <- fangraphs_FIP %>%
  select(yearID, cFIP)

data(Batting)
data(Pitching)

standardize <- function(x){
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  return((x - mu)/sigma)
}

batting <- Batting %>%
  filter((lgID == "NL"| lgID == "AL") & yearID >= 1911) %>%
  group_by(yearID) %>%
  filter(G/max(G) >= 0.4) %>%
  ungroup() %>%
  mutate(
    BA = H/AB,
    uBB = BB-IBB,
    X1B = H-X2B-X3B-HR,
    PA = AB+BB+HBP+SH+SF,
    BBP = BB/PA,
    KP = SO/PA,
    TB = X1B + 2*X2B + 3*X3B + 4*HR,
    OBP = (H+BB+HBP)/(AB+BB+HBP+SF),
    SLG = TB/AB,
    ISO = SLG-BA,
    OPS = OBP + SLG
  ) %>%
  group_by(yearID, lgID) %>%
  mutate(
    zBA = standardize(BA),
    zHR = standardize(HR),
    zSB = standardize(SB),
    zTB = standardize(TB),
    zOPS = standardize(OPS),
    zBBPb = standardize(BBP),
    zISO = standardize(ISO),
  ) %>%
  ungroup()

pitching <- merge(Pitching, fangraphs_FIP, by="yearID")
pitching <- pitching %>%
  filter((lgID == "NL"| lgID == "AL") & yearID >= 1911 & IPouts >= 100) %>%
  group_by(yearID) %>%
  filter(IPouts/max(IPouts) >= 0.15) %>%
  ungroup() %>%
  mutate(
    IP = IPouts/3,
    FIP = ((13*HR)+(3*(BB+HBP))-(2*SO))/IP + cFIP, 
    BBP = BB/BFP,
    SOP = SO/BFP,
    WHIP = (BB+H)/IP
    ) %>%
  group_by(yearID, lgID) %>%
  mutate(
    zSO = standardize(SO),
    zERA = standardize(ERA),
    zFIP = standardize(FIP),
    zWHIP = standardize(WHIP),
    zBAOpp = standardize(BAOpp),
    zBBPp = standardize(BBP),
    zSOP = standardize(SOP),
    zIP = standardize(IP)
  ) %>%
  ungroup()

baseball_awards <- left_join(baseball_awards, batting, by=c("playerID", "yearID"))
baseball_awards <- left_join(baseball_awards, pitching, by=c("playerID", "yearID"))
baseball_awards <- left_join(baseball_awards, war_daily_bat, by=c("playerID", "yearID"))
baseball_awards <- left_join(baseball_awards, war_daily_pitch, by=c("playerID", "yearID"))

mvp_award <- baseball_awards %>%
  filter(awardID == "MVP" & yearID >= 1950)
cy_award <- baseball_awards %>%
  filter(awardID == "Cy Young" & yearID >= 1956)

mvp_award[is.na(mvp_award)] <- 0
fit_shares_mvp <- lm(shares ~ zBA + zTB + zISO + zBBPb + zWHIP + zBAOpp + zERA + zSOP, data = mvp_award)

predicted_mvp_shares <- data.frame(mvp_pred = predict(fit_shares_mvp, mvp_award), shares=mvp_award$shares)

prediction_graph_shares <- ggplot(data=predicted_mvp_shares, aes(x=mvp_pred, y=shares)) +
  geom_point(mapping=aes(size=mvp_award$PriorAwards, col=as.factor(mvp_award$isWinner), text = sprintf("Player: %s<br>Season: %s", mvp_award$name_commonB, mvp_award$yearID)), alpha=0.5) +
  labs(title="Predicted Shares vs. Actual Shares", x="Predicted Shares", y="Actual Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()
prediction_graph_shares
ggplotly(prediction_graph_shares)

logit_pred_fit_shares <- glm(shares ~ mvp_pred, family="quasibinomial", data = predicted_mvp_shares)

logit_pred_mvp_shares <- predicted_mvp_shares %>%
  data_grid(mvp_pred) %>%
  add_predictions(model = logit_pred_fit_shares, type = "response", var = "predictedShares")

prediction_graph_shares <- prediction_graph_shares + 
  geom_line(data=logit_pred_mvp_shares, mapping = aes(x = mvp_pred, y = predictedShares))
  # geom_text(data = subset(mvp_award, logit_pred_mvp_shares$mvp_pred < 0 & shares > 0.75), aes(label = name_commonB))
prediction_graph_shares
ggplotly(prediction_graph_shares)

logit_pred_fit_residuals_shares <- residuals(logit_pred_fit_shares, type="response")

residual_MVP_shares <- ggplot(data=mvp_award) +
  geom_point(mapping=aes(x=yearID, y=logit_pred_fit_residuals_shares, col=as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s<br> Name: %s", playerID, yearID, name_commonB))) +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping=aes(x=yearID, y=logit_pred_fit_residuals_shares), method = lm) +
  labs(title="Prior Shares vs. Residual Between Model Relating Predicted Shares and Actual Shares", x="Prior Shares", y="Residual") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?")) +
  theme_bw()

residual_MVP_shares
ggplotly(residual_MVP_shares)



####################################################
cy_award[is.na(cy_award)] <- 0
fit_shares_cy <- lm(shares ~ zWHIP + zBAOpp + zERA + zSOP + zBBPp + zIP, data = cy_award)

predicted_cy_shares <- data.frame(cy_pred = predict(fit_shares_cy, cy_award), shares=cy_award$shares)

prediction_graph_shares <- ggplot(data=predicted_cy_shares, aes(x=cy_pred, y=shares)) +
  geom_point(mapping=aes(size=cy_award$PriorAwards, col=as.factor(cy_award$isWinner), text = sprintf("Player: %s<br>Season: %s", cy_award$name_commonP, cy_award$yearID)), alpha=0.5) +
  labs(title="Predicted Shares vs. Actual Shares", x="Predicted Shares", y="Actual Shares") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?"),
         size=guide_legend("Prior Awards")) +
  theme_bw()
prediction_graph_shares
ggplotly(prediction_graph_shares)

logit_pred_fit_shares <- glm(shares ~ cy_pred, family="quasibinomial", data = predicted_cy_shares)

logit_pred_cy_shares <- predicted_cy_shares %>%
  data_grid(cy_pred) %>%
  add_predictions(model = logit_pred_fit_shares, type = "response", var = "predictedShares")

prediction_graph_shares <- prediction_graph_shares + 
  geom_line(data=logit_pred_cy_shares, mapping = aes(x = cy_pred, y = predictedShares))
# geom_text(data = subset(cy_award, logit_pred_cy_shares$cy_pred < 0 & shares > 0.75), aes(label = name_commonB))
prediction_graph_shares
ggplotly(prediction_graph_shares)

logit_pred_fit_residuals_shares <- residuals(logit_pred_fit_shares, type="response")

residual_cy_shares <- ggplot(data=cy_award) +
  geom_point(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_shares, col=as.factor(isWinner), text = sprintf("Player: %s<br>Season: %s<br> Name: %s", playerID, yearID, name_commonB))) +
  geom_hline(yintercept = 0) +
  stat_smooth(mapping=aes(x=PriorShares, y=logit_pred_fit_residuals_shares), method = lm) +
  labs(title="Prior Shares vs. Residual Between Model Relating Predicted Shares and Actual Shares", x="Prior Shares", y="Residual") +
  scale_color_manual(values = c("#696969", "dodgerblue"), aesthetics = "color") +
  guides(col=guide_legend("Win?")) +
  theme_bw()

residual_cy_shares
ggplotly(residual_cy_shares)
