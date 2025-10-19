#install.packages("readxl")
library("readxl")

nhefs <- read_excel("C:/Users/Milica/Documents/Causal-Inference/NHEFS.xls")
nhefs$cens <- ifelse(is.na(nhefs$wt82), 1, 0)


#Model linerarne regresije sa kvadratnim i interakcionim terminima kako bismo uhvatili nelinearnost
fit <- glm(wt82_71 ~ qsmk + sex + race + age + I(age*age) + as.factor(education)
           + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
           + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
           + wt71 + I(wt71*wt71) + I(qsmk*smokeintensity), data=nhefs)
summary(fit)

#Matrica kontrasta računa konkretne vrednosti efekta tretmana za odabrane intenzitete pušenja
library("multcomp")
makeContrastMatrix <- function(model, nrow, names) {
  m <- matrix(0, nrow = nrow, ncol = length(coef(model)))
  colnames(m) <- names(coef(model))
  rownames(m) <- names
  return(m)
}
K1 <- makeContrastMatrix(fit, 2, c('Effect of Quitting Smoking at Smokeintensity of 5',
                                      'Effect of Quitting Smoking at Smokeintensity of 40'))
#Biramo intenzitete pušenja 5 i 40 cigareta dnevno
K1[1:2, 'qsmk'] <- 1
K1[1:2, 'I(qsmk * smokeintensity)'] <- c(5, 40)


K1 

#Računanje efekata
estimates1 <- glht(fit, K1)
  summary(estimates1)
  confint(estimates1)

#Linearni model bez interakcionog termina
fit2 <- glm(wt82_71 ~ qsmk + sex + race + age + I(age*age) + as.factor(education)
           + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
           + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
           + wt71 + I(wt71*wt71), data=nhefs)
  
summary(fit2)

#Poredimo model sa i bez qsmk*smokeintensity
anova(fit2, fit, test="Chisq")
#Poredimo mere kvaliteta modela
AIC(fit2, fit)
BIC(fit2, fit)

#Pravimo model logističke regresije kako bismo izračunali verovatnoću dodele tretmana za svaku jedinicu
fit3 <- glm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
            + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
            + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
            + wt71 + I(wt71*wt71), data=nhefs, family=binomial())
summary(fit3)
#Bazi podataka dodajemo kolonu sa vrednostima pomenute verovatnoće
nhefs$ps <- predict(fit3, nhefs, type="response")

#Prikaz statistika onih koji su prestali da puše i onih koji nisu
summary(nhefs$ps[nhefs$qsmk==0])
summary(nhefs$ps[nhefs$qsmk==1])

library("ggplot2")
library("dplyr")
#Plot gustine raspodele, za procenu preklapanja
ggplot(nhefs, aes(x = ps, fill = as.factor(qsmk))) + geom_density(alpha = 0.2) +
  xlab('Probability of Quitting Smoking During Follow-up') +
  ggtitle('Propensity Score Distribution by Treatment Group') +
  scale_fill_discrete('') +
  theme(legend.position = 'bottom', legend.direction = 'vertical')


#Histogram sa konkretnim brojem ljudi u obe kontrolne grupe
nhefs %>%
  mutate(ps.grp = round(ps/0.05) * 0.05) %>%
  group_by(qsmk, ps.grp) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(n2 = ifelse(qsmk == 0, yes = n, no =  -1*n)) %>%
  ggplot(aes(x = ps.grp, y = n2, fill = as.factor(qsmk))) +
  geom_bar(stat = 'identity', position = 'identity') +
  geom_text(aes(label = n, x = ps.grp, y = n2 + ifelse(qsmk == 0, 8, -8))) +
  xlab('Probability of Quitting Smoking During Follow-up') +
  ylab('N') +
  ggtitle('Propensity Score Distribution by Treatment Group') +
  scale_fill_discrete('') +
  scale_x_continuous(breaks = seq(0, 1, 0.05)) +
  theme(legend.position = 'bottom', legend.direction = 'vertical',
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

#install.packages("MatchIt") 
library(MatchIt)

#Primena matchit funkcije, čiji su rezultat indeksi uparenih jedinica sa najbližom verovatnoćom dodele tretmana 
ps_model <- matchit(
  qsmk ~ sex + race + age + I(age*age) + as.factor(education)
  + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
  + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
  + wt71 + I(wt71*wt71), data=nhefs,              
  method = "nearest",        # nearest neighbor matcing
  distance = "logit"         # koristi logistički model
)
summary(ps_model)

plot(ps_model, type = "jitter")  # raspodela verovatnoće dodele
plot(summary(ps_model))
#plot(ps_model, type = "qq")      # QQ plot pre i posle matchinga

#Pravimo novu bazu sa uparenim jedinicama
matched_data <- match.data(ps_model, distance = 'prop_score', data = nhefs)

library(cobalt)
love.plot(ps_model, binary = "std")  # vizualizacija standardizovane razlike
bal.tab(ps_model) 

#Pravimo model linearne regresije nad uparenim podacima
# [TODO] ispitati koji od ovih modela je bolji, napisati koliku ulogu ima ovaj matching
fit_matched <- glm(wt82_71 ~ qsmk + sex + race + age + I(age*age) + as.factor(education)
                   + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
                   + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
                   + wt71 + I(wt71*wt71), data=matched_data)
summary(fit_matched)
confint(fit_matched)


#Stratifikacija na osnovu uparenih podataka
matched_data$ps.dec <- cut(matched_data$ps, 
                    breaks=c(quantile(matched_data$ps, probs=seq(0,1,0.1))),
                    labels=seq(1:10),
                    include.lowest=TRUE)

# Descriptive statistics by decile
# Ispisujemo deskriptivne statistike po stratumima
tapply(matched_data$ps, matched_data$ps.dec, summary)

# function to create deciles easily
decile <- function(x) {
  return(factor(quantcut(x, seq(0, 1, 0.1), labels = FALSE)))
}

# regression on PS deciles, not allowing for effect modification
# pravi se regresija u kojoj ne posmatramo efekat tretmana zasebno za svaki stratum 
# i za svaki stratum se taj efekat kontrolise, jaca statisticka snaga, dobija se globalni ATE
fit.psdec <- glm(wt82_71 ~ qsmk + as.factor(ps.dec), data = matched_data)
summary(fit.psdec)

# Kauzalni efekat, globalni. 
fit.psdec$coefficients['qsmk']

# interval poverenja za globalni efekat
confint(fit.psdec)['qsmk', ]

# pojedinacni intervali poverenja za procenu efekta na pojedinacnim stratumima 
for (deciles in c(1:10)) {
  fit.psdec <- glm(wt82_71 ~ qsmk, data=nhefs[which(nhefs$ps.dec==deciles),])
  # interval poverenja za svaki stratum
  print(confint(fit.psdec))
  # kauzalni efekat po stratumu
  print(fit.psdec$coefficients['qsmk'])
}


############################################
# Program 15.3         
# Stratification on the propensity score 
# Data from NHEFS  
############################################

# calculation of deciles
# na osnovu prethodnog grafika smo resili da podelimo na 10 stratuma (ili je tamo reseno da podelimo na 10, kako god)
nhefs$ps.dec <- cut(nhefs$ps, 
                    breaks=c(quantile(nhefs$ps, probs=seq(0,1,0.1))),
                    labels=seq(1:10),
                    include.lowest=TRUE)

# Descriptive statistics by decile
# Ispisujemo deskriptivne statistike po stratumima
tapply(nhefs$ps, nhefs$ps.dec, summary)

# function to create deciles easily
decile <- function(x) {
  return(factor(quantcut(x, seq(0, 1, 0.1), labels = FALSE)))
}

# regression on PS deciles, not allowing for effect modification
# pravi se regresija u kojoj ne posmatramo efekat tretmana zasebno za svaki stratum 
# i za svaki stratum se taj efekat kontrolise, jaca statisticka snaga, dobija se globalni ATE
fit.psdec <- glm(wt82_71 ~ qsmk + as.factor(ps.dec), data = nhefs)
summary(fit.psdec)

# Kauzalni efekat, globalni. 
fit.psdec$coefficients['qsmk']

# interval poverenja za globalni efekat
confint(fit.psdec)['qsmk', ]

# pojedinacni intervali poverenja za procenu efekta na pojedinacnim stratumima 
for (deciles in c(1:10)) {
fit.psdec <- glm(wt82_71 ~ qsmk, data=nhefs[which(nhefs$ps.dec==deciles),])
# interval poverenja za svaki stratum
print(confint(fit.psdec))
# kauzalni efekat po stratumu
print(fit.psdec$coefficients['qsmk'])
}
