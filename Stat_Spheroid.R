setwd('/Users/tarapongsrisongkram/OneDrive - Khon Kaen University (1)/Manuscript 4/Model')
df = read.csv('spheroid.csv')
df
Control = subset(df, subset = df$Samples=='Control')
SM3mM = subset(df, subset = df$Samples=='3mM')
SM6mM = subset(df, subset = df$Samples=='6mM')
#-------------------
#Volume
shapiro.test(Control$Volume)
shapiro.test(SM3mM$Volume)
shapiro.test(SM6mM$Volume)
bartlett.test(df$Volume~df$Samples)
#SM 3mM has non-normal distribution--> should be test with KruskaWallis
kruskal = kruskal.test(df$Volume~df$Samples)
kruskal
#p-value = 0.02651
require(conover.test)
conover.test(df$Volume, df$Samples)


#------------------
#CTCF_Green
shapiro.test(Control$CTCF_Green)
shapiro.test(SM3mM$CTCF_Green)
shapiro.test(SM6mM$CTCF_Green)
bartlett.test(df$CTCF_Green~df$Samples)
#normal distribution and homogeneity of variances--> can be tested by ANOVA
anova = aov(df$CTCF_Green~df$Samples)
summary.aov(anova)
#p-value = 0.986
#------------------
#CTCF_Red
shapiro.test(Control$CTCF_Red)
shapiro.test(SM3mM$CTCF_Red)
shapiro.test(SM6mM$CTCF_Red)
bartlett.test(df$CTCF_Red~df$Samples)
#normal distribution and homogeneity of variances--> can be tested by ANOV
anova = aov(df$CTCF_Red~df$Samples)
summary.aov(anova)
#p-value = 0.000535
tukey = TukeyHSD(anova)
tukey
plot(tukey)
#------
#Desciptive stat
v0 = round(mean(Control$Volume), digits = 3)
v3 = round(mean(SM3mM$Volume), digits = 3)
v6 = round(mean(SM6mM$Volume), digits = 3)
round(sd(Control$Volume), digits = 3)
round(sd(SM3mM$Volume), digits = 3)
round(sd(SM6mM$Volume), digits = 3)
(v0-v3)/v0 *100
(v0-v6)/v0 *100
r0 = round(mean(Control$CTCF_Red), digits = 3)
r3 = round(mean(SM3mM$CTCF_Red), digits = 3)
r6 = round(mean(SM6mM$CTCF_Red), digits = 3)
r0
r3
r6
round(sd(Control$CTCF_Red), digits = 3)
round(sd(SM3mM$CTCF_Red), digits = 3)
round(sd(SM6mM$CTCF_Red), digits = 3)
(r3-r0)/r0 *100
(r6-r0)/r0 *100
g0 = round(mean(Control$CTCF_Green), digits = 3)
g3 = round(mean(SM3mM$CTCF_Green), digits = 3)
g6 = round(mean(SM6mM$CTCF_Green), digits = 3)
g0
g3
g6
round(sd(Control$CTCF_Green), digits = 3)
round(sd(SM3mM$CTCF_Green), digits = 3)
round(sd(SM6mM$CTCF_Green), digits = 3)
(g3-g0)/g0 *100
(g6-g0)/g0 *100