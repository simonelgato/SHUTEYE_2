## SHUTEYE: adaptive trial of duration of antibiotics for CAP
## 5 arm adaptive trial
## starting with 5 durations open: 4 days to 8 days (1 day increments)
## program to produce graphs for design report from FACTS output

## April 2022

## weeks has 1 line per interim
## summary has 1 line per scenario
## patients has 1 line per patient
## simulations has 1 line per simulation

library(ggplot2)
library(dplyr)

setwd("C:/temp/SHUTEYE_working")

##############################################################################################

## PLOTS OF TREATMENT EFFECT SCENARIOS

## read in data from Excel file

scenarios <- read.csv("c:/temp/SHUTEYE_working/treatment_scenarios.csv")

ggplot(data=scenarios, aes(x=arm, y=response, colour=key)) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ key) +
  labs(x = "Duration (days)") +
  ylim(0.4, 1)

#####################################################################################################

## evaluation of maximum overall sample size
## comparison of 200/arm up to 500/arm

## read in data - aggregated (pivoted) simulation data from
## SHUTEYE_resub_ss.facts

sims <- read.csv("C:/temp/SHUTEYE_working/agg_data_sample_size.csv", 
             skip = 2)

## create edq95 variable and fill with appropriate values

sims$edq95 = with(sims, 
                   ifelse(Dose %in% c("4d"), Pr.EDq._at_4d, 
                                 ifelse(Dose %in% c("5d"), Pr.EDq._at_5d, 
                                        ifelse(Dose %in% c("6d"), Pr.EDq._at_6d, 
                                               ifelse(Dose %in% c("7d"), Pr.EDq._at_7d, 
                                                     ifelse(Dose %in% c("8d"), Pr.EDq._at_8d, 999))))))

## check that there aren't any 999s in the new variable
max(sims$edq95)

## subset the data to make a file for each treatment effect scenario

scen1 <- sims %>% filter(Response.Profile == "equal")
scen2 <- sims %>% filter(Response.Profile == "linear")
scen3 <- sims %>% filter(Response.Profile == "plateau")
scen4 <- sims %>% filter(Response.Profile == "threshold")
scen5 <- sims %>% filter(Response.Profile == "Ushaped")
scen6 <- sims %>% filter(Response.Profile == "reverse_lin")
scen7 <- sims %>% filter(Response.Profile == "peak")


#graphlabs <- data.frame("labels" = c("A: null", "B: two good", "C: one good", "D: moderate & good",
#                   "E: two moderate", "F: one moderate"), "Dose.Response.Profile" = c("null", "2_good", "1_good",
#                    "mod&good", "2_mod", "1_mod"))
#
#sim2 <- left_join(graphlabs, sims, by = "Dose.Response.Profile")

##############################################################################################

## histograms probability of EDq95

dose.labs <- c("4 day", "5 day", "6 day", "7 day", "8 day")
names(dose.labs) <- c("4d", "5d", "6d", "7d", "8d")

X.Scenario.ID.labs <- c("200 per arm", "300 per arm", "400 per arm", "500 per arm")
names(X.Scenario.ID.labs) <- c(1, 2, 3, 4)

ggplot(scen1, aes(x=edq95)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of EDq95") +
  ggtitle("Equal scenario: all durations have the same effectiveness")

ggplot(scen2, aes(x=edq95)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of EDq95") +
  ggtitle("Linear scenario: linear increase with duration")

ggplot(scen3, aes(x=edq95)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of EDq95") +
  ggtitle("Plateau scenario: 6 days best")

ggplot(scen4, aes(x=edq95)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of EDq95") +
  ggtitle("Threshold scenario: 6 days best")

ggplot(scen5, aes(x=edq95)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of EDq95") +
  ggtitle("U-shaped scenario: 4 and 8 days best")

ggplot(scen6, aes(x=edq95)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of EDq95") +
  ggtitle("Reverse linear scenario: 4 days best")

ggplot(scen7, aes(x=edq95)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of EDq95") +
  ggtitle("Peak scenario: 6 days best")

####################################################################################################

## histograms of probability of being the best arm

ggplot(scen1, aes(x = Pr.Max.)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "dark green", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of being the best arm") +
  ggtitle("Equal scenario: all durations have the same effectiveness")

ggplot(scen2, aes(x = Pr.Max.)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "dark green", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of being the best arm") +
  ggtitle("Linear scenario: linear increase with duration")

ggplot(scen3, aes(x = Pr.Max.)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "dark green", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of being the best arm") +
  ggtitle("Plateau scenario: 6 days best")

ggplot(scen4, aes(x = Pr.Max.)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "dark green", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of being the best arm") +
  ggtitle("Threshold scenario: 6 days best")

ggplot(scen5, aes(x = Pr.Max.)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "dark green", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of being the best arm") +
  ggtitle("U-shaped scenario: 4 and 8 days best")

ggplot(scen6, aes(x = Pr.Max.)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "dark green", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of being the best arm") +
  ggtitle("Reverse linear scenario: 4 days best")

ggplot(scen7, aes(x = Pr.Max.)) +
  facet_grid(X.Scenario.ID ~ Dose, labeller = labeller(Dose = dose.labs, X.Scenario.ID = X.Scenario.ID.labs)) +
  geom_histogram(fill = "dark green", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="Probability of being the best arm") +
  ggtitle("Peak scenario: 6 days best")

####################################################################################################
####################################################################################################

## histogram of 5% difference 

ggplot(simulations.df, aes(x=simulations.df$Pr.CSD.2)) +
  facet_wrap(~labels, ncol=2) +
  geom_histogram(fill = "brown", alpha=0.4, binwidth=0.025) +
  theme_minimal() +
  labs(x="probability of >5% benefit", subtitle = "red line = probability of 0.9") +
  geom_vline(xintercept=0.9, color="red") +
  ggtitle("Probability of benefit > 5%") +


#################################################################################################

## histograms of final odds ratio

ggplot(simulations.df, aes(x=simulations.df$fOR)) +
  facet_wrap(~labels, ncol=2) +
  geom_histogram(fill = "darkred", alpha=0.4, binwidth=0.025) +
  theme_minimal() +
  labs(x="odds ratio", subtitle = "red line = odds ratio of 1") +
  geom_vline(xintercept=1, color="red") +
  ggtitle("Final odds ratios") +
  xlim(0, 2.5)



## histogram of number of trials with each type of outcome
## in each scenario

subA <- subset(simulations.df, simulations.df$Outcome == 4)

ggplot(data=subA, aes(x=subA$Response.Profile)) + 
  geom_bar(stat = "count", fill = "light blue", width=0.5) + 
  theme_minimal() +
  ggtitle("Stopped early for lack of effectiveness 5000 simulations") +
  labs(x="Scenario") +
  ylim(0, 5000)

subB <- subset(simulations.df, simulations.df$Outcome == 1)

ggplot(data=subB, aes(x=subB$Response.Profile)) + 
  geom_bar(stat = "count", fill = "light blue", width=0.5) + 
  theme_minimal() +
  ggtitle("Stopped early for success 5000 simulations") +
  labs(x="Scenario") +
  ylim(0,5000)

###############################################################

## plot of power
## power being proportion of trials that have a "positive" outcome
## or stop early for success

px <- as.data.frame(table(simulations.df$Response.Profile, simulations.df$Success.Pr.PBO__at_abx))
px2 <- px[c(14, 15, 16, 17, 18, 19), ]
px2$power <- px2$Freq/5000

ggplot(px2, aes(x = Var1, y = power, group = 1)) +
  geom_point(size=2) +
  labs(x = "scenario", y="prob positive") +
  geom_line()

###############################################################

## need to sort out criterion for "success" and add up number satisfying that
## don't think this is quite right yet
## also plots of mean sample size and error bars

###############################################################

subpat <- subset(patients.df, patients.df$Scenario.ID == 1)
table(subpat$Visit.1)
table(subpat$Visit.2)
table(subpat$Visit.3)
table(subpat$Visit.4)
table(subpat$Visit.5)
table(subpat$Visit.6)

###############################################################
## this is just some code for helping with plotting
## from EOLIA profram which is why it isn't relevant here

ggplot(data=xy, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", aes(fill=Reason), width=0.8, position=position_dodge(0.5)) + 
  theme_minimal() +
  ggtitle("Control 60%, ECMO 40%") +
  labs(x="interim at which trial stopped") +
  scale_fill_brewer(palette="Paired") +
  scale_x_discrete(labels=c("Interim 1", "Interim 2", "Interim 3", "Interim 4", 
                            "Interim 5", "6" = "Final analysis"))

# histogram of odds ratio
ggplot(stoptrial, aes(x=stoptrial$OR)) + geom_histogram(fill = "blue", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="odds ratio") +
  geom_vline(xintercept=1.0, color="red") +
  ggtitle("Control 60%, ECMO 40%") +
  
  
  # tables
  table(stoptrial$stoptype)
table(stoptrial$stop)

# subset data to get sims stopped at each interim
# and plot OR

sub1 <- subset(stoptrial, stoptrial$InterimNumber == 1)
ggplot(sub1, aes(x=sub1$OR)) + geom_histogram(fill = "blue", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="odds ratio") +
  geom_vline(xintercept=2.25, color="red") +
  ggtitle("Stopped at interim 1")

ggplot(subsim, aes(x = V_val, y = Z_val)) +
  xlim(0, 25) + ylim(-10, 25) +
  geom_point(size=2) +
  geom_abline(intercept = -5.54, slope = 0.81, colour="red") +
  geom_abline(intercept = 5.54, slope = 0.27, colour="red") +
  geom_line(data = subsim[subsim$Sim == 1, ]) +
  geom_line(data = subsim[subsim$Sim == 2, ]) +
  geom_line(data = subsim[subsim$Sim == 3, ]) +
  geom_line(data = subsim[subsim$Sim == 4, ]) +
  geom_line(data = subsim[subsim$Sim == 5, ])

## plot selected simulations in f8

plotline1 <- data.frame(x1 = 0, x2 = 20.5185, y1 = 5.54, y2 = 11.08)
plotline2 <- data.frame(x1 = 0, x2 = 20.5185, y1 = -5.54, y2 = 11.08)

f9 <- subset(f8, f8$InterimNumber == f8$stopat)

ggplot(f8, aes(x = V_val, y = Z_val)) +
  xlim(0, 25) + ylim(-10, 15) +
  geom_point(size=2) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour="red"), data = plotline1) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour="red"), data = plotline2) +
  geom_line(data = f8[f8$Sim == 48, ]) +
  geom_line(data = f8[f8$Sim == 49, ]) +
  geom_line(data = f8[f8$Sim == 50, ]) +
  geom_line(data = f8[f8$Sim == 51, ]) +
  geom_point(aes(x=fV_val, y=fZ_val, colour="red"), data=f9)


## this is trying to do a facet plot which would be easier

subX <- subset(simulations.df, simulations.df$Scenario.ID == c(1, 2, 3, 4))

ggplot(subX, aes(x=subX$Subjects)) +
  facet_grid(rows = vars(subX$Response.Profile)) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=20) +
  theme_minimal() +
  labs(x="sample size") +
  ggtitle("Overall sample size")
ylim(0, 4000) 


## just messing around with this - seems to work now

ggplot(simulations.df, aes(x=simulations.df$Subjects)) +
  facet_wrap(~simulations.df$Response.Profile, ncol=2) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=20) +
  theme_minimal() +
  labs(x="sample size") +
  ggtitle("Overall sample size") +
  ylim(0, 4000) +
  xlim(400, 1600) +
  scale_x_continuous(breaks=c(500, 1000, 1500))
