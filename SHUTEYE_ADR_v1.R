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

sims <- read.csv("C:/temp/SHUTEYE_working/agg_data_ss_simulations.csv", 
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

## summary tables to complement plots

library(formattable)

sumdat <- read.csv("c:/temp/SHUTEYE_working/agg_data_ss_summary.csv", skip = 2) %>%
rename(scenario = Response.Profile, n_per_arm = Mean.Alloc.1)

st1 <- sumdat %>%
  select(scenario, n_per_arm, Ppn.Pr.Max..1, Ppn.Pr.Max..2, Ppn.Pr.Max..3, Ppn.Pr.Max..4, Ppn.Pr.Max..5) %>%
  rename(four_day = Ppn.Pr.Max..1, five_day = Ppn.Pr.Max..2, six_day = Ppn.Pr.Max..3, 
         seven_day = Ppn.Pr.Max..4, eight_day = Ppn.Pr.Max..5)
st1$stat = "PpnMax"

st2 <- sumdat %>%
  select(scenario, n_per_arm, Ppn.Pr.EDq..1, Ppn.Pr.EDq..2, Ppn.Pr.EDq..3, Ppn.Pr.EDq..4, Ppn.Pr.EDq..5) %>%
  rename(four_day = Ppn.Pr.EDq..1, five_day = Ppn.Pr.EDq..2, six_day = Ppn.Pr.EDq..3, 
         seven_day = Ppn.Pr.EDq..4, eight_day = Ppn.Pr.EDq..5)
st2$stat = "PpnEDq95"

st3 <- sumdat %>%
  select(scenario, n_per_arm, Ppn.QOI_4_1.1, Ppn.QOI_4_1.2, Ppn.QOI_4_1.3, Ppn.QOI_4_1.4, Ppn.QOI_4_1.5) %>%
  rename(four_day = Ppn.QOI_4_1.1, five_day = Ppn.QOI_4_1.2, six_day = Ppn.QOI_4_1.3, 
         seven_day = Ppn.QOI_4_1.4, eight_day = Ppn.QOI_4_1.5)
st3$stat = "PpnEDq90"

st4 <- sumdat %>%
  select(scenario, n_per_arm, Pr.EDq._at_4d, Pr.EDq._at_5d, Pr.EDq._at_6d, Pr.EDq._at_7d, Pr.EDq._at_8d) %>%
  rename(four_day = Pr.EDq._at_4d, five_day = Pr.EDq._at_5d, six_day = Pr.EDq._at_6d, 
         seven_day = Pr.EDq._at_7d, eight_day = Pr.EDq._at_8d)
st4$stat = "MeanPrEDq95"
st4r <- st4 %>%
  mutate_if(is.numeric, round, 3)

## NEED TO ROUND ALL THE NUMBERS IN ST4 BECAUSE THEY HAVE LOTS OF DECIMAL PLACES
## export numbers to csv files for the 4 tables

write.csv(st1, "tabmax.csv", row.names = F)

write.csv(st2, "tabedq95.csv", row.names = F)

write.csv(st3, "tabedq90.csv", row.names = F)

write.csv(st4r, "tabmeanedq.csv", row.names = F)

## ENDS

####################################################################################################
####################################################################################################

