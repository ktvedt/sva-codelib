### This code replicates tables and figures in
### "When Do Voters Punish Corrupt Politicians? Evidence from a Field and Survey Experiment"
### By Miguel P. De Figueiredo, F. Daniel Hidalgo, and Yuri Kasahara
###

## Tables are produced in latex format
## See bottom of script for R and package versions


library(reshape)
library(xtable)
library(ggplot2)
library(plyr)
library(reporttools)
library(estimatr)


load("SPexperiment_data.RData")

blocked.ate <- function(y, treat, block) {
  data <- data.frame(y, treat, block)
  block.ate <- ddply(data, "block", summarise, block.ate = mean(y[treat == 1], na.rm = TRUE) - mean(y[treat == 0], na.rm = TRUE), block.wt = length(y))
  block.ate$block.wt <- block.ate$block.wt / nrow(data)
  ate <- sum(block.ate$block.ate * block.ate$block.wt)
  ate
}

# Function to get heteroskedastically robust SE
summaryR.lm <- function(model, type = c("hc3", "hc0", "hc1", "hc2", "hc4"), ...) {
  if (!require(car)) stop("Required car package is missing.")
  type <- match.arg(type)
  V <- hccm(model, type = type)
  sumry <- summary(model)
  table <- coef(sumry)
  table[, 2] <- sqrt(diag(V))
  table[, 3] <- table[, 1] / table[, 2]
  table[, 4] <- 2 * pt(abs(table[, 3]), df.residual(model), lower.tail = FALSE)
  sumry$coefficients <- table
  p <- nrow(table)
  hyp <- cbind(0, diag(p - 1))
  sumry$fstatistic[1] <- linearHypothesis(model, hyp, white.adjust = type)[2, "F"]
  sumry
}

blocked.se <- function(y, treat, block) {
  data <- data.frame(y, treat, block)
  block.var <- ddply(data, "block", summarise, block.var = (var(y[treat == 0], na.rm = TRUE) / (length(y) - sum(treat)) + var(y[treat == 1], na.rm = TRUE) / (sum(treat))), block.wt = length(y))
  block.var$block.wt <- (block.var$block.wt)^2 / nrow(data)^2
  var <- sum(block.var$block.var * block.var$block.wt)
  sqrt(var)
}




#### Table 1 and 2


marta.results <- list()
kassab.results <- list()
martakassab.results <- list()
marta.data$marta.pr <- marta.data$marta.pr * 100
marta.data$turnout <- marta.data$turnout * 100
kassab.data$kassab.pr <- kassab.data$kassab.pr * 100
kassab.data$turnout <- kassab.data$turnout * 100
marta.data$nulo.votes <- (marta.data$nulo.votes / marta.data$voters) * 100
kassab.data$nulo.votes <- (kassab.data$nulo.votes / kassab.data$voters) * 100
marta.data$branco.votes <- (marta.data$branco.votes / marta.data$voters) * 100
kassab.data$branco.votes <- (kassab.data$branco.votes / kassab.data$voters) * 100
marta.data$branconulo.votes <- marta.data$branco.votes + marta.data$nulo.votes
kassab.data$branconulo.votes <- kassab.data$branco.votes + kassab.data$nulo.votes
covariates <- c("treat", "match.id", "pt.pref.04", "voters")


#
### Adjusted Estimates
##
## Effect on Marta Vote Share with covariates
marta.results$lm.marta.pr.cov <- summaryR.lm(lm(as.formula(paste("marta.pr ~ ", paste(covariates, collapse = "+"))), data = marta.data), type = "hc2")[4]$coefficients[2, ]
## Effect on Kassab Vote Share with covariates
kassab.results$lm.kassab.pr.cov <- summaryR.lm(lm(as.formula(paste("kassab.pr ~ ", paste(covariates, collapse = "+"))), data = kassab.data), type = "hc2")[4]$coefficients[2, ]
## Effect on Turnout (of Marta flier) with Covariates
marta.results$lm.turnout.marta.cov <- summaryR.lm(lm(as.formula(paste("turnout ~ ", paste(covariates, collapse = "+"))), data = marta.data), type = "hc2")[4]$coefficients[2, ]
## Effect on Turnout (Kassab Flier) with Covariates
kassab.results$lm.turnout.kassab.cov <- summaryR.lm(lm(as.formula(paste("turnout ~ ", paste(covariates, collapse = "+"))), data = kassab.data), type = "hc2")[4]$coefficients[2, ]
## Effect on Nulos (Marta flier) with Covariates
marta.results$lm.nulo.cov <- summaryR.lm(lm(as.formula(paste("nulo.votes ~ ", paste(covariates, collapse = "+"))), data = marta.data), type = "hc2")[4]$coefficients[2, ]
## Effect on Nulos (Kassab flier) with Covariates
kassab.results$lm.nulo.cov <- summaryR.lm(lm(as.formula(paste("nulo.votes ~ ", paste(covariates, collapse = "+"))), data = kassab.data), type = "hc2")[4]$coefficients[2, ]
## Effect on Brancos (Marta flier) with Covariates
marta.results$lm.branco.cov <- summaryR.lm(lm(as.formula(paste("branco.votes ~ ", paste(covariates, collapse = "+"))), data = marta.data), type = "hc2")[4]$coefficients[2, ]
## Effect on Brancos (Kassab flier) with Covariates
kassab.results$lm.branco.cov <- summaryR.lm(lm(as.formula(paste("branco.votes ~ ", paste(covariates, collapse = "+"))), data = kassab.data), type = "hc2")[4]$coefficients[2, ]


##
### Unadjusted Estimates
##
## Effect on Marta vote share
marta.results$itt.marta.pr[1] <- sum(tapply(marta.data$marta.pr[marta.data$treat == 1], marta.data$match.id[marta.data$treat == 1], mean) - tapply(marta.data$marta.pr[marta.data$treat == 0], marta.data$match.id[marta.data$treat == 0], mean)) / length(unique(marta.data$match.id))
marta.results$itt.marta.pr[2] <- sqrt(1 / (100 * (100 - 1)) * sum(((tapply(marta.data$marta.pr[marta.data$treat == 1], marta.data$match.id[marta.data$treat == 1], mean) - tapply(marta.data$marta.pr[marta.data$treat == 0], marta.data$match.id[marta.data$treat == 0], mean)) - marta.results$itt.marta.pr[1])^2))
marta.results$itt.marta.pr[3] <- pt(marta.results$itt.marta.pr[1] / marta.results$itt.marta.pr[2], 99) * 2
## Effect on Turnout (Marta Flier)
marta.results$itt.turnout[1] <- sum(tapply(marta.data$turnout[marta.data$treat == 1], marta.data$match.id[marta.data$treat == 1], mean) - tapply(marta.data$turnout[marta.data$treat == 0], marta.data$match.id[marta.data$treat == 0], mean)) / length(unique(marta.data$match.id))
marta.results$itt.turnout[2] <- sqrt(1 / (100 * (100 - 1)) * sum(((tapply(marta.data$turnout[marta.data$treat == 1], marta.data$match.id[marta.data$treat == 1], mean) - tapply(marta.data$turnout[marta.data$treat == 0], marta.data$match.id[marta.data$treat == 0], mean)) - marta.results$itt.turnout[1])^2))
marta.results$itt.turnout[3] <- pt(marta.results$itt.turnout[1] / marta.results$itt.turnout[2], 99) * 2

## Effect on Nulos (Marta Flier)
marta.results$itt.nulo.votes[1] <- sum(tapply(marta.data$nulo.votes[marta.data$treat == 1], marta.data$match.id[marta.data$treat == 1], mean) - tapply(marta.data$nulo.votes[marta.data$treat == 0], marta.data$match.id[marta.data$treat == 0], mean)) / length(unique(marta.data$match.id))
marta.results$itt.nulo.votes[2] <- sqrt(1 / (100 * (100 - 1)) * sum(((tapply(marta.data$nulo.votes[marta.data$treat == 1], marta.data$match.id[marta.data$treat == 1], mean) - tapply(marta.data$nulo.votes[marta.data$treat == 0], marta.data$match.id[marta.data$treat == 0], mean)) - marta.results$itt.nulo.votes[1])^2))
marta.results$itt.nulo.votes[3] <- pt(marta.results$itt.nulo.votes[1] / marta.results$itt.nulo.votes[2], 99) * 2
## Effect on Brancos (Marta Flier)
marta.results$itt.branco.votes[1] <- sum(tapply(marta.data$branco.votes[marta.data$treat == 1], marta.data$match.id[marta.data$treat == 1], mean) - tapply(marta.data$branco.votes[marta.data$treat == 0], marta.data$match.id[marta.data$treat == 0], mean)) / length(unique(marta.data$match.id))
marta.results$itt.branco.votes[2] <- sqrt(1 / (100 * (100 - 1)) * sum(((tapply(marta.data$branco.votes[marta.data$treat == 1], marta.data$match.id[marta.data$treat == 1], mean) - tapply(marta.data$branco.votes[marta.data$treat == 0], marta.data$match.id[marta.data$treat == 0], mean)) - marta.results$itt.branco.votes[1])^2))
marta.results$itt.branco.votes[3] <- (1 - pt(marta.results$itt.branco.votes[1] / marta.results$itt.branco.votes[2], 99)) * 2

## Effect on Kassab Vote Share
kassab.results$itt.kassab.pr[1] <- sum(tapply(kassab.data$kassab.pr[kassab.data$treat == 1], kassab.data$match.id[kassab.data$treat == 1], mean) - tapply(kassab.data$kassab.pr[kassab.data$treat == 0], kassab.data$match.id[kassab.data$treat == 0], mean)) / length(unique(kassab.data$match.id))
kassab.results$itt.kassab.pr[2] <- sqrt(1 / (100 * (100 - 1)) * sum(((tapply(kassab.data$kassab.pr[kassab.data$treat == 1], kassab.data$match.id[kassab.data$treat == 1], mean) - tapply(kassab.data$kassab.pr[kassab.data$treat == 0], kassab.data$match.id[kassab.data$treat == 0], mean)) - kassab.results$itt.kassab.pr[1])^2))
kassab.results$itt.kassab.pr[3] <- (1 - pt(kassab.results$itt.kassab.pr[1] / kassab.results$itt.kassab.pr[2], 99)) * 2
#### Effect on Turnout (Kassab Flier)
kassab.results$itt.turnout[1] <- sum(tapply(kassab.data$turnout[kassab.data$treat == 1], kassab.data$match.id[kassab.data$treat == 1], mean) - tapply(kassab.data$turnout[kassab.data$treat == 0], kassab.data$match.id[kassab.data$treat == 0], mean)) / length(unique(kassab.data$match.id))
kassab.results$itt.turnout[2] <- sqrt(1 / (100 * (100 - 1)) * sum(((tapply(kassab.data$turnout[kassab.data$treat == 1], kassab.data$match.id[kassab.data$treat == 1], mean) - tapply(kassab.data$turnout[kassab.data$treat == 0], kassab.data$match.id[kassab.data$treat == 0], mean)) - kassab.results$itt.turnout[1])^2))
kassab.results$itt.turnout[3] <- (1 - pt(kassab.results$itt.turnout[1] / kassab.results$itt.turnout[2], 99)) * 2
## Effect on Nulos (Kassab Flier)
kassab.results$itt.nulo.votes[1] <- sum(tapply(kassab.data$nulo.votes[kassab.data$treat == 1], kassab.data$match.id[kassab.data$treat == 1], mean) - tapply(kassab.data$nulo.votes[kassab.data$treat == 0], kassab.data$match.id[kassab.data$treat == 0], mean)) / length(unique(kassab.data$match.id))
kassab.results$itt.nulo.votes[2] <- sqrt(1 / (100 * (100 - 1)) * sum(((tapply(kassab.data$nulo.votes[kassab.data$treat == 1], kassab.data$match.id[kassab.data$treat == 1], mean) - tapply(kassab.data$nulo.votes[kassab.data$treat == 0], kassab.data$match.id[kassab.data$treat == 0], mean)) - kassab.results$itt.nulo.votes[1])^2))
kassab.results$itt.nulo.votes[3] <- pt(kassab.results$itt.nulo.votes[1] / kassab.results$itt.nulo.votes[2], 99) * 2
## Effect on Brancos (Kassab Flier)
kassab.results$itt.branco.votes[1] <- sum(tapply(kassab.data$branco.votes[kassab.data$treat == 1], kassab.data$match.id[kassab.data$treat == 1], mean) - tapply(kassab.data$branco.votes[kassab.data$treat == 0], kassab.data$match.id[kassab.data$treat == 0], mean)) / length(unique(kassab.data$match.id))
kassab.results$itt.branco.votes[2] <- sqrt(1 / (100 * (100 - 1)) * sum(((tapply(kassab.data$branco.votes[kassab.data$treat == 1], kassab.data$match.id[kassab.data$treat == 1], mean) - tapply(kassab.data$branco.votes[kassab.data$treat == 0], kassab.data$match.id[kassab.data$treat == 0], mean)) - kassab.results$itt.branco.votes[1])^2))
kassab.results$itt.branco.votes[3] <- pt(kassab.results$itt.branco.votes[1] / kassab.results$itt.branco.votes[2], 99) * 2



conf.int <- function(x) paste("[", round(x[1] - 1.96 * x[2], digits = 1), ", ", round(x[1] + 1.96 * x[2], digits = 1), "]", sep = "")

options(scipen = 999)

marta.table <- cbind(
  rbind(round(marta.results$itt.marta.pr[1], 1), round(marta.results$itt.marta.pr[2], 2), conf.int(marta.results$itt.marta.pr), round(marta.results$itt.marta.pr[3], 2)),
  rbind(round(marta.results$lm.marta.pr.cov[1], 1), round(marta.results$lm.marta.pr.cov[2], 2), conf.int(marta.results$lm.marta.pr.cov), round(marta.results$lm.marta.pr.cov[4], 2)),
  rbind(round(marta.results$itt.turnout[1], 1), round(marta.results$itt.turnout[2], 2), conf.int(marta.results$itt.turnout), round(marta.results$itt.turnout[3], 4)),
  rbind(round(marta.results$lm.turnout.marta.cov[1], 1), round(marta.results$lm.turnout.marta.cov[2], 2), conf.int(marta.results$lm.turnout.marta.cov), round(marta.results$lm.turnout.marta.cov[4], 4)),
  rbind(round(marta.results$itt.branco.votes[1], 2), round(marta.results$itt.branco.votes[2], 2), conf.int(marta.results$itt.branco.votes), round(marta.results$itt.branco.votes[3], 2)),
  rbind(round(marta.results$lm.branco.cov[1], 2), round(marta.results$lm.branco.cov[2], 2), conf.int(marta.results$lm.branco.cov), round(marta.results$lm.branco.cov[4], 2))
)
marta.table <- rbind(marta.table, c("", "X", "", "X", "", "X"))



# col.names <- c("", "\\multicolumn{2}{c}{Vote Share (\\%)}", "\\multicolumn{2}{c}{Turnout (\\%)}", "\\multicolumn{2}{c}{Spoiled Ballots (\\%)}")
rownames(marta.table) <- c("Estimate", "Standard Error", "95 % Conf. Int.", "P-value", "Covariates")
marta.table <- xtable(marta.table, digits = 4)
align(marta.table) <- "c|cccccc"

print(marta.table, add.to.row = list(pos = list(0), command = c("& \\multicolumn{2}{c}{Vote Share (\\%)} &  \\multicolumn{2}{c}{Turnout (\\%)}  & \\multicolumn{2}{c}{Spoiled Ballots (\\%)} \\\\")), include.colnames = FALSE, floating = TRUE)



kassab.table <- cbind(
  rbind(round(kassab.results$itt.kassab.pr[1], 1), round(kassab.results$itt.kassab.pr[2], 2), conf.int(kassab.results$itt.kassab.pr), round(kassab.results$itt.kassab.pr[3], 2)),
  rbind(round(kassab.results$lm.kassab.pr.cov[1], 1), round(kassab.results$lm.kassab.pr.cov[2], 2), conf.int(kassab.results$lm.kassab.pr.cov), round(kassab.results$lm.kassab.pr.cov[4], 2)),
  rbind(round(kassab.results$itt.turnout[1], 1), round(kassab.results$itt.turnout[2], 2), conf.int(kassab.results$itt.turnout), round(kassab.results$itt.turnout[3], 2)),
  rbind(round(kassab.results$lm.turnout.kassab.cov[1], 1), round(kassab.results$lm.turnout.kassab.cov[2], 2), conf.int(kassab.results$lm.turnout.kassab.cov), round(kassab.results$lm.turnout.kassab.cov[4], 2)),
  rbind(round(kassab.results$itt.branco.votes[1], 2), round(kassab.results$itt.branco.votes[2], 2), conf.int(kassab.results$itt.branco.votes), round(kassab.results$itt.branco.votes[3], 2)),
  rbind(round(kassab.results$lm.branco.cov[1], 2), round(kassab.results$lm.branco.cov[2], 2), conf.int(kassab.results$lm.branco.cov), round(kassab.results$lm.branco.cov[4], 2))
)
kassab.table <- rbind(kassab.table, c("", "X", "", "X", "", "X"))
rownames(kassab.table) <- c("Estimate", "Standard Error", "95 % Conf. Int.", "P-value", "Covariates")
kassab.table <- xtable(kassab.table)
align(kassab.table) <- "c|cccccc"

print(kassab.table, add.to.row = list(pos = list(0), command = c("& \\multicolumn{2}{c}{Vote Share (\\%)} &  \\multicolumn{2}{c}{Turnout (\\%)}  & \\multicolumn{2}{c}{Spoiled Ballots (\\%)} \\\\")), include.colnames = FALSE, floating = TRUE)


#### Figure 1

corrupt.ranking$id <- 1:nrow(corrupt.ranking)
corrupt.ranking <- melt(id = c("id", "vote04"), data = corrupt.ranking)
corrupt.ranking.vote04 <- cast(corrupt.ranking[is.na(corrupt.ranking$vote04) == FALSE, ], vote04 ~ variable, mean)
corrupt.ranking.vote04 <- melt(corrupt.ranking.vote04, id = "vote04")
levels(corrupt.ranking.vote04$variable) <- c("Suplicy is more corrupt", "Kassab is more corrupt", "Both equally corrupt", "Don't Know")

corrupt.ranking.plot.vote04 <- ggplot(corrupt.ranking.vote04, aes(value, variable)) +
  geom_point(aes(color = vote04, shape = vote04), size = 4) +
  xlab("Proportion") +
  theme_bw() +
  labs(colour = "", shape = "") +
  scale_colour_brewer(palette = "Set1") +
  ylab("")


corrupt.ranking.plot.vote04



#### Table 3

svy.exp.data$marta.treat <- ifelse(svy.exp.data$treat == "M", 1, 0)
svy.exp.data$kassab.treat <- ifelse(svy.exp.data$treat == "K", 1, 0)
svy.exp.data$vote.marta.pre[is.na(svy.exp.data$vote.marta.pre)] <- 0
svy.exp.data$vote.kassab.pre[is.na(svy.exp.data$vote.kassab.pre)] <- 0

conf.int <- function(ate, se) {
  paste("[", round(ate - 1.96 * se, digits = 2), ", ", round(ate + 1.96 * se, digits = 2), "]", sep = "")
}



# Marta Flier Effect on Feeling Therm
marta.placebo.mod <- difference_in_means(marta.therm.change ~ treat,
  data = svy.exp.data[svy.exp.data$treat != "K", ],
  blocks = block
)
marta.placebo.est <- round(marta.placebo.mod$coefficients, 2)
marta.placebo.se <- round(marta.placebo.mod$std.error, 2)
marta.placebo.ci <- paste0("[", round(marta.placebo.mod$conf.low, 2), ", ", round(marta.placebo.mod$conf.high, 2), "]")
marta.placebo.p <- round(marta.placebo.mod$p.value, 2)

# Kassab Flier Effect on Feeling Therm
kassab.placebo.mod <- difference_in_means(kassab.therm.change ~ treat,
                                         data = svy.exp.data[svy.exp.data$treat != "M", ],
                                         blocks = block
)
kassab.placebo.est <- round(kassab.placebo.mod$coefficients, 2)
kassab.placebo.se <- round(kassab.placebo.mod$std.error, 2)
kassab.placebo.ci <- paste0("[", round(kassab.placebo.mod$conf.low, 2), ", ", round(kassab.placebo.mod$conf.high, 2), "]")
kassab.placebo.p <- round(kassab.placebo.mod$p.value, 2)

marta.kassab.mod <- difference_in_means(both.therm.change ~ treat,
                                          data = svy.exp.data[svy.exp.data$treat != "C", ],
                                          blocks = block
)
marta.kassab.est <- round(marta.kassab.mod$coefficients, 2)
marta.kassab.se <- round(marta.kassab.mod$std.error, 2)
marta.kassab.ci <- paste0("[", round(marta.kassab.mod$conf.low, 2), ", ", round(marta.kassab.mod$conf.high, 2), "]")
marta.kassab.p <- round(marta.kassab.mod$p.value, 2)

svy.exp.table <- data.frame(rbind(cbind(marta.placebo.est, kassab.placebo.est, marta.kassab.est),
                                  cbind(marta.placebo.se, kassab.placebo.se, marta.kassab.se),
                                  cbind(marta.placebo.ci, kassab.placebo.ci, marta.kassab.ci),
                                  cbind(marta.placebo.p, kassab.placebo.p, marta.kassab.p)))
names(svy.exp.table) <- c("Suplicy (PT) Flier vs Placebo", "Kassab (DEM/PFL) Flier vs Placebo", "Suplicy (PT) Flier vs Kassab (DEM/PFL) Flier")
rownames(svy.exp.table) <- c("Estimate", "Standard Error", "95 % Conf. Int.", " p-Value")
svy.exp.table <- xtable(svy.exp.table, digits = 2)
align(svy.exp.table) <- "c|ccc"
print(svy.exp.table, include.colnames = TRUE)


#### Table 4

# Marta Flier Effect on Feeling Therm
marta.placebo.martavoter.mod <- difference_in_means(marta.therm.change ~ treat,
                                                    data = svy.exp.data, subset = treat != "K" & vote.marta.pre == 1)
marta.placebo.martavoter.est <- round(marta.placebo.martavoter.mod$coefficients, 2)
marta.placebo.martavoter.se <- round(marta.placebo.martavoter.mod$std.error, 2)
marta.placebo.martavoter.ci <- paste0("[", round(marta.placebo.martavoter.mod$conf.low, 2), ", ", round(marta.placebo.martavoter.mod$conf.high, 2), "]")
marta.placebo.martavoter.p <- round(marta.placebo.martavoter.mod$p.value, 2)
marta.placebo.martavoter.n <- marta.placebo.martavoter.mod$nobs


marta.placebo.nonmartavoter.mod <- difference_in_means(marta.therm.change ~ treat,
                                                    data = svy.exp.data, subset = treat != "K" & vote.marta.pre == 0)
marta.placebo.nonmartavoter.est <- round(marta.placebo.nonmartavoter.mod$coefficients, 2)
marta.placebo.nonmartavoter.se <- round(marta.placebo.nonmartavoter.mod$std.error, 2)
marta.placebo.nonmartavoter.ci <- paste0("[", round(marta.placebo.nonmartavoter.mod$conf.low, 2), ", ", round(marta.placebo.nonmartavoter.mod$conf.high, 2), "]")
marta.placebo.nonmartavoter.p <- round(marta.placebo.nonmartavoter.mod$p.value, 2)
marta.placebo.nonmartavoter.n <- marta.placebo.nonmartavoter.mod$nobs


kassab.placebo.kassabvoter.mod <- difference_in_means(kassab.therm.change ~ treat,
                                                    data = svy.exp.data, subset = treat != "M" & vote.kassab.pre == 1)
kassab.placebo.kassabvoter.est <- round(kassab.placebo.kassabvoter.mod$coefficients, 2)
kassab.placebo.kassabvoter.se <- round(kassab.placebo.kassabvoter.mod$std.error, 2)
kassab.placebo.kassabvoter.ci <- paste0("[", round(kassab.placebo.kassabvoter.mod$conf.low, 2), ", ", round(kassab.placebo.kassabvoter.mod$conf.high, 2), "]")
kassab.placebo.kassabvoter.p <- round(kassab.placebo.kassabvoter.mod$p.value, 2)
kassab.placebo.kassabvoter.n <- kassab.placebo.kassabvoter.mod$nobs

kassab.placebo.nonkassabvoter.mod <- difference_in_means(kassab.therm.change ~ treat,
                                                      data = svy.exp.data, subset = treat != "M" & vote.kassab.pre == 0)
kassab.placebo.nonkassabvoter.est <- round(kassab.placebo.nonkassabvoter.mod$coefficients, 2)
kassab.placebo.nonkassabvoter.se <- round(kassab.placebo.nonkassabvoter.mod$std.error, 2)
kassab.placebo.nonkassabvoter.ci <- paste0("[", round(kassab.placebo.nonkassabvoter.mod$conf.low, 2), ", ", round(kassab.placebo.nonkassabvoter.mod$conf.high, 2), "]")
kassab.placebo.nonkassabvoter.p <- round(kassab.placebo.nonkassabvoter.mod$p.value, 2)
kassab.placebo.nonkassabvoter.n <- kassab.placebo.nonkassabvoter.mod$nobs

svy.exp.hetero.table <- data.frame(rbind(cbind(marta.placebo.nonmartavoter.est, marta.placebo.martavoter.est, kassab.placebo.nonkassabvoter.est, kassab.placebo.kassabvoter.est),
                                         cbind(marta.placebo.nonmartavoter.se, marta.placebo.martavoter.se, kassab.placebo.nonkassabvoter.se, kassab.placebo.kassabvoter.se),
                                         cbind(marta.placebo.nonmartavoter.ci, marta.placebo.martavoter.ci, kassab.placebo.nonkassabvoter.ci, kassab.placebo.kassabvoter.ci),
                                         cbind(marta.placebo.nonmartavoter.p, marta.placebo.martavoter.p, kassab.placebo.nonkassabvoter.p, kassab.placebo.kassabvoter.p),
                                         cbind(marta.placebo.nonmartavoter.n, marta.placebo.martavoter.n, kassab.placebo.nonkassabvoter.n, kassab.placebo.kassabvoter.n)))
 names(svy.exp.table) <- c("Suplicy (PT) Flier vs Placebo", "Kassab (DEM/PFL) Flier vs Placebo", "Suplicy (PT) Flier vs Kassab (DEM/PFL) Flier")
rownames(svy.exp.hetero.table) <- c("Estimate", "Standard Error", "95 % Conf. Int.", "p-value", "n")
svy.exp.hetero.table <- xtable(svy.exp.hetero.table, digits = 2)
align(svy.exp.hetero.table) <- "c|cccc"
print(svy.exp.hetero.table, include.colnames = TRUE)


### Appendix Table 5

exp.data <- exp.data[order(exp.data$match.id, exp.data$treat), ]
bal.data <- exp.data[, c("voters", "pt.pres.06", "pt.pref.04", "pt.depfed.06", "psdb.depfed.06", "marta.1st.pr", "kassab.1st.pr", "branco.1st.pr", "nulo.1st.pr", "turnout.1st.pr", "ver.pt.1st.pr", "ver.psdb.1st.pr", "ver.dem.1st.pr")]
bal.data$pt.pres.06 <- bal.data$pt.pres.06 * 100
bal.data$pt.pref.04 <- bal.data$pt.pref.04 * 100
bal.data$pt.depfed.06 <- bal.data$pt.depfed.06 * 100
bal.data$psdb.depfed.06 <- bal.data$psdb.depfed.06 * 100
bal.data$marta.1st.pr <- bal.data$marta.1st.pr * 100
bal.data$kassab.1st.pr <- bal.data$kassab.1st.pr * 100
bal.data$branco.1st.pr <- bal.data$branco.1st.pr * 100
bal.data$nulo.1st.pr <- bal.data$nulo.1st.pr * 100
bal.data$turnout.1st.pr <- bal.data$turnout.1st.pr * 100
bal.data$ver.pt.1st.pr <- bal.data$ver.pt.1st.pr * 100
bal.data$ver.psdb.1st.pr <- bal.data$ver.psdb.1st.pr * 100
bal.data$ver.dem.1st.pr <- bal.data$ver.dem.1st.pr * 100

names(bal.data) <- c("\\# of Registered Voters", "PT Pres. Vote \\% (2006)", "PT Mayor Vote \\% (2004)", "PT Congress Vote \\% (2006)", "PSDB Congress Vote \\% (2006)", "1st Round Suplicy Vote \\% (2008)", "1st Round Kassab Vote \\% (2008)", "1st Round Blank Vote \\% (2008)", "1st Round Invalid Vote \\% (2008)", "1st Round Turnout \\% (2008)", "PT City Council Vote \\% (2008)", "PSDB City Council Vote \\% (2008)", "DEM City Council Vote \\% (2008)")
bal.cap <- "Balance on Baseline Variables"
stats <- list("Mean Difference" = function(x) {
  mean(x[exp.data$treat == 1] - x[exp.data$treat == 0])
}, "Standard Error" = function(x) {
  sd(x[exp.data$treat == 1] - x[exp.data$treat == 0]) / sqrt(200)
}, "t-Test p-Value" = function(x) {
  t.test(x[exp.data$treat == 1], x[exp.data$treat == 0], paired = TRUE)$p.value
}, "KS-test p-Value" = function(x) {
  ks.test(x[exp.data$treat == 1], x[exp.data$treat == 0])$p.value
})
tableContinuous(vars = bal.data, cap = bal.cap, lab = "tab:bal", longtable = FALSE, stats = stats, prec = 2)


# > sessionInfo()
# R version 4.1.0 (2021-05-18)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 11.4
#
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
#
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] car_3.0-11        carData_3.0-4     estimatr_0.30.2   reporttools_1.1.2 plyr_1.8.6        ggplot2_3.3.5     xtable_1.8-4
# [8] reshape_0.8.8     usethis_2.0.1
#
# loaded via a namespace (and not attached):
#   [1] zip_2.2.0          Rcpp_1.0.6         RColorBrewer_1.1-2 cellranger_1.1.0   pillar_1.6.1       compiler_4.1.0     forcats_0.5.1
# [8] tools_4.1.0        digest_0.6.27      lifecycle_1.0.0    tibble_3.1.2       gtable_0.3.0       pkgconfig_2.0.3    rlang_0.4.11
# [15] openxlsx_4.2.4     DBI_1.1.1          curl_4.3.2         parallel_4.1.0     haven_2.4.1        rio_0.5.27         withr_2.4.2
# [22] dplyr_1.0.7        hms_1.1.0          generics_0.1.0     fs_1.5.0           vctrs_0.3.8        grid_4.1.0         tidyselect_1.1.1
# [29] data.table_1.14.0  glue_1.4.2         R6_2.5.0           fansi_0.5.0        readxl_1.3.1       foreign_0.8-81     Formula_1.2-4
# [36] farver_2.1.0       purrr_0.3.4        magrittr_2.0.1     scales_1.1.1       ellipsis_0.3.2     abind_1.4-5        assertthat_0.2.1
# [43] colorspace_2.0-2   labeling_0.4.2     utf8_1.2.1         stringi_1.6.2      munsell_0.5.0      crayon_1.4.1