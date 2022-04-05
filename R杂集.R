library(car)
el27773 <- Davis
#create a new variable
heightDiff <- el27773$height - el27773$repht
hist(heightDiff, main = "The histogram of difference in height")
heightDiff <- el27773$height[el27773$height > 100] - el27773$repht
hist(heightDiff, main = "The histogram of difference in height")
#run a paired t.test
t.test(el27773$height, el27773$repht, paired = T)

centralR <- na.omit(Leprosy$CasesPer100K[Leprosy$Region == "Central"])
median(centralR)

easternR <- na.omit(Leprosy$CasesPer100K[Leprosy$Region == "Eastern"])
median(easternR)

wilcox.test(centralR, easternR)

alone <- Lab8$Abdomen[Lab8$Mating == "alone"]
mated <- Lab8$Abdomen[Lab8$Mating == "mated"]
hist(alone)
hist(mated)

sum(table(alone[Lab8$Pigment == 'andro']))
sum(table(mated[Lab8$Pigment == 'andro']))

wilcox.test(alone[Lab8$Pigment == 'andro'], mated[Lab8$Pigment == 'andro'])

fivenum(alone[Lab8$Pigment == 'andro'])
fivenum(mated[Lab8$Pigment == 'andro'])


#import dataset
library(car)
el27773 <- Cowles

#draw a graph
boxplot(el27773$neuroticism[el27773$volunteer == 'no'], el27773$neuroticism[el27773$volunteer == 'yes'], xlab = 'not volunteer   volunteer', ylab = 'neuroticism scores', main = 'Boxplot of neuroticism scores')

#conduct an independent t.test
t.test(el27773$neuroticism[el27773$volunteer == 'no'], el27773$neuroticism[el27773$volunteer == 'yes'])

cor(bodyfat$Percent, bodyfat$Height)
cor.test(bodyfat$Percent, bodyfat$Height)

mymodel <- lm(bodyfat$Percent~bodyfat$Height)
summary(mymodel)

mymodel1 <- lm(seasnails$Weight~seasnails$Length)

#check assumption
#check linearity
#plot(x, y)
plot(seasnails$Length, seasnails$Weight, main = 'Length and Weight', xlab = 'Length(cm)', ylab = 'Weight(g)', pch = 20)

#check normality
qqnorm(mymodel1$residuals, main = 'QQ-plot of Model Residuals')
qqline(mymodel1$residuals, col = 'blue')

#constant variance
plot(mymodel1$fitted.values, mymodel1$residuals, xlab = 'Fitted Values', ylab = 'Residuals', pch = 20)
abline(h = 0, col = 'blue')

#carry out linear regression test
summary(mymodel1)


#draw graph for response variable
hist(coloredsolution$Wavelength, main = 'Histogram of Response Variable', xlab = 'Absorbed Wavelength (nm)')
summary(coloredsolution$Wavelength)
#try transformation to reach normality
response1 <- log10(coloredsolution$Wavelength)
response2 <- log(coloredsolution$Wavelength)
response3 <- sqrt(coloredsolution$Wavelength)
response4 <- 1/(coloredsolution$Wavelength)
hist(response1, main = 'Histogram#1 of Asjusted Response Variable', xlab = 'Absorbed Wavelength (nm)')
hist(response2, main = 'Histogram#2 of Asjusted Response Variable', xlab = 'Absorbed Wavelength (nm)')
hist(response3, main = 'Histogram#3 of Asjusted Response Variable', xlab = 'Absorbed Wavelength (nm)')
hist(response4, main = 'Histogram#4 of Asjusted Response Variable', xlab = 'Absorbed Wavelength (nm)')

#explanatory variable 1: color of the solution
explanatory1 <- table(coloredsolution$Color)
barplot(explanatory1, main  = 'Barplot of color of the solution', ylab = 'frequency')

#bivariate graph of different concentrations of blue solutions
wavelengthBlue <- coloredsolution$Wavelength[coloredsolution$Color == 'blue']
concentrationBlue <- coloredsolution$concentration[coloredsolution$Color == 'blue']
boxplot(wavelengthBlue~concentrationBlue, main = 'Boxplot of Different Concentrations of Blue Dye Solution', xlab = 'Concentration (ppm)', ylab = 'Absorbed Wavelength (nm)')

#explanatory variable 2: 8 ppm and 4 ppm
explanatory2 <- table(coloredsolution$concentration)
barplot(explanatory2, main  = 'Barplot of concentration of blue dye solution', xlab = 'Concentration (ppm)',  ylab = 'frequency')

#bivariate graph of red and blue dye solutions
wavelengthColo <- coloredsolution$Wavelength[coloredsolution$concentration == '8']
twoColor <- coloredsolution$Color[coloredsolution$concentration == '8']
boxplot(wavelengthColo~twoColor, main = 'Boxplot of Red and Blue Dye Solutions', xlab = 'Color', ylab = 'Absorbed Wavelength (nm)')

mean(birthwt$bwt[birthwt$ht == 'yes'])
mean(birthwt$bwt[birthwt$ht == 'no'])
library(car)
myaov <- aov(birthwt$bwt~birthwt$ht)
Anova(myaov, type=2)

#import functions/datasets
install.packages('emmeans')
library(emmeans)
library(car)

#check assumptions
#check normality
cost <- log(insurance$costs)
boxplot(cost~insurance$smoker, xlab = 'Smokers', ylab = 'Annual Insurance Cost ($)', main = 'Boxplot of smokers and insurance cost')

#equal variance
leveneTest(cost~insurance$smoker)

#create a linear model of my data
my_anova <- lm(log(insurance$costs) ~ insurance$smoker)

anova(my_anova,type=3)
Anova(my_anova, type = 3)

# effect size
summary(my_anova)$r.squared

#post hoc
emmeans(my_anova, pairwise ~ costs)

#import dataset
library(car)
data('Friendly')
el27773 <- Friendly
#assign variables
sfr <- el27773$correct[el27773$condition == 'SFR']
before <- el27773$correct[el27773$condition == 'Before']
#check assumptions
hist(sfr, main = 'Histogram of SFR Condition')
hist(before, main = 'Histogram of Before Condition')
table(sfr)
table(before)
#carry out non parametric analysis
wilcox.test(before, sfr)

mymodel <- lm(Handout12_crabs$FL~Handout12_crabs$Sex*Handout12_crabs$CL)

summary(mymodel)

#import dataset
library(car)
el27773 <- Robey
#check assumptions for one way ANOVA
boxplot(el27773$tfr~el27773$region, main = 'Boxplot of Tfr and Region', xlab = 'region', ylab = 'tfr')
leveneTest(el27773$tfr~el27773$region)
#Build ANOVA model
myaov <- aov(el27773$tfr~el27773$region)
Anova(myaov, type = 3)
summary.lm(myaov)$r.squared
#run a multi factor Anova
el27773$contraceptors <- as.factor(el27773$contraceptors)
my2aov <- aov(tfr~region+contraceptors, data = el27773, contrasts = list(region = contr.sum, contraceptors = contr.sum))
Anova(my2aov, type = 3)


coloredsolution$concentration <- as.factor(coloredsolution$concentration)
#check normality of each groups
boxplot(coloredsolution$Wavelength~coloredsolution$concentration, main = 'Boxplot of Maximum Wavelength and Conentration', xlab = 'Concentration (ppm)', ylab = 'Maximum Wavelength (nm)')
boxplot(coloredsolution$Wavelength~coloredsolution$Color, main = 'Boxplot of Maximum Wavelength and Colors', xlab = 'Colors', ylab = 'Maximum Wavelength (nm)')

#check equal variance assumptions
library(car)
leveneTest(coloredsolution$Wavelength~coloredsolution$Color)
leveneTest(coloredsolution$Wavelength~coloredsolution$concentration)

my2aov <- aov(Wavelength~Color*concentration, data = coloredsolution, contrasts = list(Color = contr.sum, concentration = contr.sum))
Anova(my2aov, type = 3)

summary.lm(my2aov)$adj.r.squared

table(coloredsolution$concentration)
table(coloredsolution$Color)

mean(coloredsolution$Wavelength)
sd(coloredsolution$Wavelength)

options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
mymodel1 <- lm(Wavelength ~ concentration * Color, data = coloredsolution)
Anova(mymodel1, type=3)

library(emmeans)
library(ggplot2)
mymeans <- summary(emmeans(mymodel1, pairwise~concentration|Color)$emmeans)

mymeans_plot <- mymeans[c('concentration','Color','emmean','SE')]
ggplot(mymeans_plot,aes(x=Color,y=emmean, col=concentration)) + geom_point(position=position_dodge(width=0.4)) + geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE), width=.4, size=.7, position=position_dodge(width=0.4)) + xlab('Color of the solution') + ylab('Mean Maximum Wavelength +/- SE') + ggtitle('Mean Maximum Wavelength by Color and Concentration') +  theme_classic() 

dbinom(0, 400, 0.01)

qnorm(0.1)