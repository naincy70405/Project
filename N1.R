bos <- read.csv('https://raw.githubusercontent.com/bkrai/Sports-Analytics-With-R/main/2022BOS.csv')
str(bos)
head(bos,3)
tail(bos,2)
summary(bos)

bos$slugMatchup <- as.factor(bos$slugMatchup)

#Piechart
tab <- table(bos$slugOpponent)
pie(tab,
    col = rainbow(30),
    main = 'Boston Opponents')

#Bar plot
barplot(tab,
        col = rainbow(30),
        las = 2)
#histogram
hist(bos$pts,
     col = 'green',
     main = 'Points Per Player Per Game',
     xlab = 'Points Made')

#Scatter plot
plot(pts ~ minutes, bos)
pairs(bos[,30:35])

library(ggplot2)
library(corrplot)
library(MASS)
library(ggExtra)

library(psych)
pairs.panels(bos[,30:35])

#Missing data
mean(bos$pctFT, na.rm = T)
sd(bos$pctFT,na.rm = T)

bos[!complete.cases(bos), ] #rows with missing data
dim(bos)
new <- na.omit(bos)
dim(new)

#matches played
max(bos$numberGamePlayerSeason)
table(bos$numberGamePlayerSeason)
#benchplayers
table(bos$numberGamePlayerSeason,bos$outcomeGame)

#select
library(dplyr)
bos %>% select(23, 31:35, 'pts')
#filter
bos %>% select(23, 31:35, 'pts') %>% 
  filter(namePlayer == 'Jayson Tatum')
bos %>% select(23, 31:35, 'pts') %>% 
  filter(pts >40) %>% 
  arrange(desc(fg3m))
#summarise 
bos %>%  group_by(namePlayer) %>% 
  summarise(AVG =mean(pts),
                   SD = sd(pts),
                   COUNT =n()) %>% 
  arrange(desc(AVG))

#mutate
bos %>%  group_by(namePlayer) %>% 
  summarise(EFG = (sum(fgm) +0.5*sum(fg3m))/sum(fga),
            COUNT =n()) %>% 
  filter(COUNT>10) %>% 
  arrange(desc(EFG)) #Effective Field goal Percentage

# highest free throws made by player
bos %>%  group_by(namePlayer) %>% 
  summarise(FT = sum(ftm)) %>% 
  arrange(desc(FT))

#histogram(win/loss)
library(ggplot2)
bos %>% ggplot(aes(x= plusminus ,fill = outcomeGame)) +
  geom_histogram(alpha = 0.5, color ='black') +
  facet_wrap(~outcomeGame) +
  ggtitle('Player Point Differential in a win/loss','Season : 2021-2022')

#Barplot
new <- bos %>%  group_by(namePlayer) %>% 
  summarise(EFG = (sum(fgm) +0.5*sum(fg3m))/sum(fga),
            COUNT =n()) %>% 
  filter(COUNT>10) %>% 
  arrange(desc(EFG))

new %>% ggplot(aes(x=factor(namePlayer), y = EFG, fill = namePlayer))+
  geom_col(show.legend = F) +
  coord_flip()

#boxplot
bos %>% filter(minutes > 5) %>% 
  ggplot(aes(x = interaction(namePlayer,outcomeGame),y = pts, fill = outcomeGame)) +
  geom_boxplot(show.legend = F) +
  coord_flip()
#--------------------------------------------------------------------------------
#Correlation matrix
library(corrplot)
data <- bos %>% select(32:34, 37:40)
cor(data)
round(cor(data),digits = 3)
corrplot(cor(data),
         type = 'upper',
         method ='number',
         number.cex = 1.5)

library(BasketballAnalyzeR)
Tadd$team
#Tbox
Tbox[1,]
Tbox[2,]
Tbox[1:4,]

#Obox
Obox[1,]
Obox[1:4,]

#Pace ,Ratings & Four factors
FF <- fourfactors(Tbox[1:2,],Obox[1:2,])
plot(FF)
#for three teams
FF <- fourfactors(Tbox[c(2,6,10),],Obox[c(2,6,10),])
plot(FF)

FF <- fourfactors(Tbox,Obox)
# correlations
corrplot(cor(FF[-1]),
         method ='number',
         type = 'upper')
#team success
efg_dev <- FF$F1.Off - FF$F1.Def
to_dev <- FF$F2.Def - FF$F2.Off
rb_dev <- FF$F3.Off - FF$F3.Def
ft_dev <- FF$F4.Off - FF$F4.Def
wins <- Tbox$W
data <- data.frame(wins, efg_dev, to_dev, rb_dev,ft_dev )

library(corrplot)
corrplot(cor(data),
         method = 'number',
         type = 'upper')

#multiple linear regression
model <- lm(wins ~ .,data)
summary(model)

