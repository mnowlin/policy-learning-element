# nuclear energy analysis for chapter 3 

## load packages 
library(ggplot2)
library(QuantPsyc)

## load data 
nukeData <- read.csv("data/nukeData.csv")

names(nukeData)

plot(aggregate(nrsk1 ~ year, data=nukeData, mean), type="l")
plot(aggregate(nrsk2 ~ year, data=nukeData, mean), type="l")
plot(aggregate(nrsk3 ~ year, data=nukeData, mean), type="l")
plot(aggregate(nrsk4 ~ year, data=nukeData, mean), type="l")

plot(aggregate(nben1 ~ year, data=nukeData, mean), type="l")
plot(aggregate(nben2 ~ year, data=nukeData, mean), type="l")
plot(aggregate(nben3 ~ year, data=nukeData, mean), type="l")
plot(aggregate(nben4 ~ year, data=nukeData, mean), type="l")

plot(aggregate(morePlants ~ year, data=nukeData, mean), type="l")

nrsk1M <- aggregate(nrsk1 ~ year, data=nukeData, mean)
nrsk2M <- aggregate(nrsk2 ~ year, data=nukeData, mean)
nrsk3M <- aggregate(nrsk3 ~ year, data=nukeData, mean)
nrsk4M <- aggregate(nrsk4 ~ year, data=nukeData, mean)

nben1M <- aggregate(nben1 ~ year, data=nukeData, mean)
nben2M <- aggregate(nben2 ~ year, data=nukeData, mean)
nben3M <- aggregate(nben3 ~ year, data=nukeData, mean)
nben4M <- aggregate(nben4 ~ year, data=nukeData, mean)


NRdf <- data.frame(mean1=c(nrsk1M[[1,2]],nrsk1M[[2,2]],nrsk1M[[3,2]],
                            nrsk1M[[4,2]],nrsk1M[[5,2]],nrsk1M[[6,2]],
                            nrsk1M[[7,2]],nrsk1M[[8,2]],nrsk1M[[9,2]],
                            nrsk1M[[10,2]],nrsk1M[[11,2]],nrsk1M[[12,2]],
                           nrsk1M[[13,2]],nrsk1M[[14,2]],nrsk1M[[15,2]],
                           nrsk1M[[16,2]]),
                    mean2=c(nrsk2M[[1,2]],nrsk2M[[2,2]],nrsk2M[[3,2]],
                            nrsk2M[[4,2]],nrsk2M[[5,2]],nrsk2M[[6,2]],
                            nrsk2M[[7,2]],nrsk2M[[8,2]],nrsk2M[[9,2]],
                            nrsk2M[[10,2]],nrsk2M[[11,2]],nrsk2M[[12,2]],
                            nrsk2M[[13,2]],nrsk2M[[14,2]],nrsk2M[[15,2]],
                            nrsk2M[[16,2]]),
                    mean3=c(nrsk3M[[1,2]],nrsk3M[[2,2]],nrsk3M[[3,2]],
                            nrsk3M[[4,2]],nrsk3M[[5,2]],nrsk3M[[6,2]],
                            nrsk3M[[7,2]],nrsk3M[[8,2]],nrsk3M[[9,2]],
                            nrsk3M[[10,2]],nrsk3M[[11,2]],nrsk3M[[12,2]],
                            nrsk3M[[13,2]],nrsk3M[[14,2]],nrsk3M[[15,2]],
                            nrsk3M[[16,2]]),
                    mean4=c(nrsk4M[[1,2]],nrsk4M[[2,2]],nrsk4M[[3,2]],
                            nrsk4M[[4,2]],nrsk4M[[5,2]],nrsk4M[[6,2]],
                            nrsk4M[[7,2]],nrsk4M[[8,2]],nrsk4M[[9,2]],
                            nrsk4M[[10,2]],nrsk4M[[11,2]],nrsk4M[[12,2]],
                            nrsk4M[[13,2]],nrsk4M[[14,2]],nrsk4M[[15,2]],
                            nrsk4M[[16,2]]),
                    year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014",
                           "2015","2018","2019","2020","2021","2022","2023")) 

NBdf <- data.frame(mean1=c(nben1M[[1,2]],nben1M[[2,2]],nben1M[[3,2]],
                           nben1M[[4,2]],nben1M[[5,2]],nben1M[[6,2]],
                           nben1M[[7,2]],nben1M[[8,2]],nben1M[[9,2]],
                           nben1M[[10,2]],nben1M[[11,2]],nben1M[[12,2]],
                           nben1M[[13,2]],nben1M[[14,2]],nben1M[[15,2]],
                           nben1M[[16,2]]),
                   mean2=c(nben2M[[1,2]],nben2M[[2,2]],nben2M[[3,2]],
                           nben2M[[4,2]],nben2M[[5,2]],nben2M[[6,2]],
                           nben2M[[7,2]],nben2M[[8,2]],nben2M[[9,2]],
                           nben2M[[10,2]],nben2M[[11,2]],nben2M[[12,2]],
                           nben2M[[13,2]],nben2M[[14,2]],nben2M[[15,2]],
                           nben2M[[16,2]]),
                   mean3=c(nben3M[[1,2]],nben3M[[2,2]],nben3M[[3,2]],
                           nben3M[[4,2]],nben3M[[5,2]],nben3M[[6,2]],
                           nben3M[[7,2]],nben3M[[8,2]],nben3M[[9,2]],
                           nben3M[[10,2]],nben3M[[11,2]],nben3M[[12,2]],
                           nben3M[[13,2]],nben3M[[14,2]],nben3M[[15,2]],
                           nben3M[[16,2]]),
                   mean4=c(nben4M[[1,2]],nben4M[[2,2]],nben4M[[3,2]],
                           nben4M[[4,2]],nben4M[[5,2]],nben4M[[6,2]],
                           nben4M[[7,2]],nben4M[[8,2]],nben4M[[9,2]],
                           nben4M[[10,2]],nben4M[[11,2]],nben4M[[12,2]],
                           nben4M[[13,2]],nben4M[[14,2]],nben4M[[15,2]],
                           nben4M[[16,2]]),
                   year=c("2006","2007","2008","2009","2010","2011","2012","2013","2014",
                          "2015","2018","2019","2020","2021","2022","2023")) 

NRp <- ggplot(NRdf, aes(year, mean1, group = 1, linetype = "Plant Accident")) +
  geom_line() +
  stat_smooth(color="black", lty="solid") + 
  geom_line(NRdf, mapping = aes(year, mean2,  group = 1, linetype = "Transportation")) +
  stat_smooth(NRdf, mapping = aes(year, mean2),
              color="black", lty=3) +
  geom_line(NRdf, mapping = aes(year, mean3,  group = 1, linetype = "Terrorist")) +
  stat_smooth(NRdf, mapping = aes(year, mean3),
              color="black", lty=2) +
  geom_line(NRdf, mapping = aes(year, mean4,  group = 1, linetype = "Weapons")) +
  stat_smooth(NRdf, mapping = aes(year, mean4),
              color="black", lty=4) +
  ylim(0,10) + 
  labs(y="Mean Risk Perception",
       x="",
       title = "Issue Dimensions of Nuclear Energy",
       linetype="Risk Dimensions") +
  theme_classic() +
  theme(plot.title=element_text(face='bold'))


## changes in mean 

meanChange <- ((nrsk1M[[16,2]]-nrsk1M[[15,2]]), (nrsk1M[[15,2]]-nrsk1M[[14,2]]))
nrsk1M[[15,2]]-nrsk1M[[14,2]]
nrsk1M[[14,2]]-nrsk1M[[13,2]]

nrsk1M[[1,2]]-nrsk1M[[2,2]]

(nrsk1M[[16,2]]-nrsk1M[[15,2]])-nrsk1M[[14,2]]



## playing with some models 
names(nukeDataC)
nukeDataC <- subset(nukeData, college==1)

nukeData$nukeRisk <- round((nukeData$nrsk1+nukeData$nrsk2+nukeData$nrsk3+
                   nukeData$nrsk4)/4,0)
summary(nukeData$nukeRisk)
  
nukeData$nukeBenefit <- round((nukeData$nben1+nukeData$nben2+nukeData$nben3+
                               nukeData$nben4)/4,0)
summary(nukeData$nukeBenefit)

nukeRiskA <- psy::cronbach(data.frame(nukeData$nrsk1,nukeData$nrsk2,nukeData$nrsk3,
                                      nukeData$nrsk4))

nukeBenefitA <- psy::cronbach(data.frame(nukeData$nben1,nukeData$nben2,nukeData$nben3,
                                      nukeData$nben4))

nukeData$fuk1 <- ifelse(nukeData$year==2012,1,0)
nukeData$fuk2 <- ifelse(nukeData$year==2012 | 
                         nukeData$year==2013,1,0)
nukeData$fuk3 <- ifelse(nukeData$year==2012 | 
                         nukeData$year==2013 |
                         nukeData$year==2014,1,0)
nukeData$fuk4 <- ifelse(nukeData$year==2012 | 
                           nukeData$year==2013 |
                           nukeData$year==2014 |
                           nukeData$year==2015,1,0)
nukeData$fuk5 <- ifelse(nukeData$year==2012 | 
                           nukeData$year==2013 |
                           nukeData$year==2014 |
                           nukeData$year==2015 |
                           nukeData$year==2016,1,0)

nukeData$fuk6 <- ifelse(nukeData$year==2012 | 
                           nukeData$year==2013 |
                           nukeData$year==2014 |
                           nukeData$year==2015 |
                           nukeData$year==2016 |
                           nukeData$year==2017,1,0)
nukeData$fuk7 <- ifelse(nukeData$year==2012 | 
                           nukeData$year==2013 |
                           nukeData$year==2014 |
                           nukeData$year==2015 |
                           nukeData$year==2016 |
                           nukeData$year==2017 |
                           nukeData$year==2018,1,0)
nukeData$fuk8 <- ifelse(nukeData$year==2012 | 
                           nukeData$year==2013 |
                           nukeData$year==2014 |
                           nukeData$year==2015 |
                           nukeData$year==2016 |
                           nukeData$year==2017 |
                           nukeData$year==2018 |
                           nukeData$year==2019,1,0)
nukeData$fuk9 <- ifelse(nukeData$year==2012 | 
                           nukeData$year==2013 |
                           nukeData$year==2014 |
                           nukeData$year==2015 |
                           nukeData$year==2016 |
                           nukeData$year==2017 |
                           nukeData$year==2018 |
                           nukeData$year==2019 |
                           nukeData$year==2020,1,0)
nukeData$fuk10 <- ifelse(nukeData$year==2012 | 
                           nukeData$year==2013 |
                           nukeData$year==2014 |
                           nukeData$year==2015 |
                           nukeData$year==2016 |
                           nukeData$year==2017 |
                           nukeData$year==2018 |
                           nukeData$year==2019 |
                           nukeData$year==2020 |
                           nukeData$year==2021,1,0)
nukeData$fuk11 <- ifelse(nukeData$year==2012 | 
                            nukeData$year==2013 |
                            nukeData$year==2014 |
                            nukeData$year==2015 |
                            nukeData$year==2016 |
                            nukeData$year==2017 |
                            nukeData$year==2018 |
                            nukeData$year==2019 |
                            nukeData$year==2020 |
                            nukeData$year==2021 |
                            nukeData$year==2022,1,0)
nukeData$fuk12 <- ifelse(nukeData$year==2012 | 
                            nukeData$year==2013 |
                            nukeData$year==2014 |
                            nukeData$year==2015 |
                            nukeData$year==2016 |
                            nukeData$year==2017 |
                            nukeData$year==2018 |
                            nukeData$year==2019 |
                            nukeData$year==2020 |
                            nukeData$year==2021 |
                            nukeData$year==2022 |
                            nukeData$year==2023,1,0)

nukeData$fuk <- ifelse(nukeData$year==2012 | 
                           nukeData$year==2013 |
                           nukeData$year==2014 |
                           nukeData$year==2015 |
                           nukeData$year==2016 |
                           nukeData$year==2017 |
                           nukeData$year==2018 |
                           nukeData$year==2019 |
                           nukeData$year==2020 |
                           nukeData$year==2021 |
                           nukeData$year==2022 |
                           nukeData$year==2023,1,0)


summary(olsRisk <- lm(nukeRisk~age+gender+white+ideology, data = subset(nukeData, college==1)))
summary(olsBenefit <- lm(nukeBenefit~age+gender+white+ideology, data = subset(nukeData, college==1)))

summary(olsRiskFuk <- lm(nukeRisk~age+gender+white+ideology+fuk, data = subset(nukeData, college==1)))
summary(olsBenefitFuk <- lm(nukeBenefit~age+gender+white+ideology+fuk, data = subset(nukeData, college==1)))

lm.beta(olsRisk)
lm.beta(olsBenefit)
lm.beta(olsRiskFuk)
lm.beta(olsBenefitFuk)

summary(olsMorePlants <- lm(morePlants~nukeRisk+nukeBenefit+age+gender+white+ideology, 
                             data = subset(nukeData, college==1)))
summary(olsMorePlantsFuk <- lm(morePlants~nukeRisk+nukeBenefit+age+gender+white+ideology+fuk, 
                            data = subset(nukeData, college==1)))

lm.beta(olsMorePlants)
lm.beta(olsMorePlantsFuk)

summary(olsMorePlantsFukI <- lm(morePlants~nukeRisk*fuk+nukeBenefit*fuk+age+gender+white+ideology, 
                               data = subset(nukeData, college==1)))


summary(olsRiskPre <- lm(nukeRisk~age+gender+white+ideology, data = subset(nukeDataC, fuk==0)))
summary(olsBenefitPre <- lm(nukeBenefit~age+gender+white+ideology, data = subset(nukeDataC, fuk==0)))
summary(olsMorePlantsPre <- lm(morePlants~nukeRisk+nukeBenefit+age+gender+white+ideology, 
                            data = subset(nukeDataC, fuk==0)))
lm.beta(olsRiskPre)
lm.beta(olsBenefitPre)
lm.beta(olsMorePlantsPre)

summary(olsRiskPost <- lm(nukeRisk~age+gender+white+ideology, data = subset(nukeDataC, fuk==1)))
summary(olsBenefitPost <- lm(nukeBenefit~age+gender+white+ideology, data = subset(nukeDataC, fuk==1)))
summary(olsMorePlantsPost <- lm(morePlants~nukeRisk+nukeBenefit+age+gender+white+ideology, 
                               data = subset(nukeDataC, fuk==1)))
lm.beta(olsRiskPost)
lm.beta(olsBenefitPost)
lm.beta(olsMorePlantsPost)
