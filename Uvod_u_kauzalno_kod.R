#install.packages("readxl")
library("readxl")
library(MatchIt)
library(ggplot2)
library(dplyr)
library(xtable)

#======================
# Procena ATE-a
#======================
# 1) D NASUMICAN
set.seed(20)
n <- 200
X1 <- rnorm(n); X2 <- rnorm(n)
D <- rbinom(n,1,0.5)
Y <- 2*D + X1 + rnorm(n)

data <- data.frame(Y, D, X1, X2)

# Naivna razlika u sredinama
ate_diff <- mean(data$Y[data$D==1]) - mean(data$Y[data$D==0])

# Regresija (kontrola za X)
mod_lm <- lm(Y ~ D + X1 + X2, data=data)
ate_reg <- coef(mod_lm)["D"]

# Uparivanje (verovatnoca dodele)

m.out <- matchit(D ~ X1 + X2, method="nearest", data=data)
matched_data <- match.data(m.out, distance = 'ps', data = data, drop.unmatched = FALSE)
matched_data$Grupa <- ifelse(matched_data$D==1, "Tretirana", "Kontrolna")

# ilustracija uparivanja, dodela tretmana je nasumicna, ne mora svaka tacka biti uparena
# zato crtamo ovako 
matched_data_matched <- matched_data %>% 
  filter(weights > 0)

ate_match <- with(matched_data_matched, mean(Y[D==1]) - mean(Y[D==0]))

ggplot() +
  # Crtamo linije samo za uparene
  geom_line(
    data = matched_data_matched,
    aes(x = ps, y = Y, group = subclass),
    color = "gray70", alpha = 0.5
  ) +
  # Tacke za sve jedinice (uparene i neuparene)
  geom_point(
    data = matched_data,
    aes(x = ps, y = Y, color = Grupa),
    size = 2, alpha = 0.8
  ) +
  labs(
    title = "Uparivanje tretiranih i kontrolnih jedinica po verovatnoćama dodele tretmana",
    x = "Verovatnoća dodele tretmana",
    y = "Ishod (Y)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Tretirana" = "blue", "Kontrolna" = "red"))


# 2) D ZAVISI OD X -> KONFAUNDING
# Identican proces kao i gore, s tim sto D zavisi od X
set.seed(22)
n <- 200
X1 <- rnorm(n); X2 <- rnorm(n)
# D zavisi od X1 (ne nasumicno)
ps_true <- plogis(0.5*X1 - 0.2*X2) # stvarni propensity
D <- rbinom(n, 1, ps_true)
Y <- 2*D + X1 + rnorm(n)

data2 <- data.frame(Y, D, X1, X2)

# Naivna razlika u sredinama
ate_diff2 <- mean(data2$Y[data2$D==1]) - mean(data2$Y[data2$D==0])

# Regresija (kontrola za X)
mod_lm2 <- lm(Y ~ D + X1 + X2, data=data2)
ate_reg2 <- coef(mod_lm2)["D"]

# Uparivanje (verovatnoca dodele)
m.out2 <- matchit(D ~ X1 + X2, method="nearest", data=data2)
mdata2 <- match.data(m.out2, distance = 'ps', data = data, drop.unmatched = FALSE)
mdata2$Grupa <- ifelse(mdata2$D==1, "Tretirana", "Kontrolna")

mdata2_matched <- mdata2 %>% 
  filter(weights > 0)
ate_match2 <- with(mdata2_matched, mean(Y[D==1]) - mean(Y[D==0]))

ggplot() +
  geom_line(
    data = mdata2_matched,
    aes(x = ps, y = Y, group = subclass),
    color = "gray70", alpha = 0.5
  ) +
  geom_point(
    data = mdata2,
    aes(x = ps, y = Y, color = Grupa),
    size = 2, alpha = 0.8
  ) +
  labs(
    title = "Uparivanje tretiranih i kontrolnih jedinica po verovatnoćama dodele tretmana",
    x = "Verovatnoća dodele tretmana",
    y = "Ishod (Y)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Tretirana" = "blue", "Kontrolna" = "red"))

# spajamo dobijene podatke
ate_estim <- rbind(
  "D nezavisno od X" = c(ate_diff = ate_diff, ate_reg = ate_reg, ate_match = ate_match),
  "D zavisi od X" = c(ate_diff = ate_diff2, ate_reg = ate_reg2, ate_match = ate_match2)
)
colnames(ate_estim) <- c("Razlika", "Regresija", "Uparivanje")

print(round(ate_estim, 4))


#pravimo tabelu za ubacivanje u latex
#ate_estim <- xtable(ate_estim,
#                    caption = 'Razlicite metode procene ATE-a, u razlicitim scenarijima',
#                    label = 'tab:ate_estim')
#print(ate_estim)

#============================
# Randomizovani eksperiment
#=============================

install.packages("agricolae")
library(agricolae)
set.seed(20)

n = 1000

#pravimo vestacke podatke koji predstavljaju 
#visine dece pre primene tretmana i prosecne visine njihovih roditelja
X <- sample(c(110:140), n, replace = TRUE)
g <- sample(c(155:190), n, replace = TRUE)
#nasumicno dodeljujemo tretman svakom detetu
T<-rbinom(n,1,0.5)

Y=c();

#vestacki pravimo podatke o visinama dece nakon primene tretmana 
#namerno postavljamo da deca koja su dobila tretman imaju veci napredak u visini
for(i in 1:length(T)){
  if(T[i]==0){
    Y[i]=X[i]+sample(0:8,1);
  }else if(T[i]==1){
    Y[i]=X[i]+sample(4:12,1);
  }
}

#racunamo stvarnu vrednost ATE
ate_diff <- mean(Y[which(T == 1)]) - mean(Y[which(T == 0)])

#pravimo modele
q<-Y-X;
data <- data.frame(Y, T, X, g, q)
M1<-lm(Y~T, data = data)
M2<-lm(Y~T+X, data = data)
M3<-lm(q~T, data=data)
M4<-lm(Y~T+X+g, data = data)

summary(M1)
summary(M2)
summary(M3)
summary(M4)

t<-t.test(Y~T);
t

#naredni primer sa 3 vrste djubriva
set.seed(123)
n2<-300;
djubriva<-c("A", "B", "C");

#verovatnoce za drugi slucaj
p<-c(0.5, 0.3, 0.2);

#dodeljujemo tretmane u oba slucaja na nasumican nacin
tretmani1<-sample(djubriva, n2, replace = TRUE);
tretmani2<-sample(djubriva, n2, replace = TRUE, prob = p);

#vestacki podaci o prinosu paradajza
Y1<-rnorm(n2, 50, 5);
Y2<-rnorm(n2, 50, 5);

#namerno postavljamo da 1 tretman ima mnogo veci uticaj kako bi ANOVA test
#pokazao da postoje razlike i da vidimo sta se esava kada u tom slucaju radimo post-testove
Y2[which(tretmani2=="C")]=Y2[which(tretmani2=="C")]+5;

#pravimo modele za oba slucaja (kada su vrovatnoce jednake i kada nisu)
model1<-lm(Y1~tretmani1);
model2<-lm(Y2~tretmani2);

summary(model1)
summary(model2)

#primenjujemo ANOVA test na drugi model
model<-aov(Y2~tretmani2);
summary(model)

#Fiserov LSD test se radi preko funkcije LSD.test iz paketa "agricolae"

#vrsimo LSD i HSD post-testove
lsdtest<-LSD.test(model, "tretmani2");
lsdtest
tukiHSD<-TukeyHSD(model);
tukiHSD



#==========================
# Opservaciona studija
#==========================

#install.packages("MatchIt") 
library("multcomp")
library(MatchIt)

nhefs <- read_excel("NHEFS.xls")
nhefs$cens <- ifelse(is.na(nhefs$wt82), 1, 0)

mean_untreated <- mean(na.omit(nhefs$wt82_71[which(nhefs$qsmk==0)]))
mean_treated <- mean(na.omit(nhefs$wt82_71[which(nhefs$qsmk==1)]))

ate <- mean_treated - mean_untreated # ate po definiciji, tj ocena, jer nemamo celu populaciju

# Model linerarne regresije sa kvadratnim i interakcionim terminima kako bismo uhvatili nelinearnost
fit <- glm(wt82_71 ~ qsmk + sex + race + age + I(age*age) + as.factor(education)
           + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
           + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
           + wt71 + I(wt71*wt71) + I(qsmk*smokeintensity), data=nhefs)
summary(fit)

# Matrica kontrasta računa konkretne vrednosti efekta tretmana za odabrane intenzitete pušenja

makeContrastMatrix <- function(model, nrow, names) {
  m <- matrix(0, nrow = nrow, ncol = length(coef(model)))
  colnames(m) <- names(coef(model))
  rownames(m) <- names
  return(m)
}
K1 <- makeContrastMatrix(fit, 2, c('Effect of Quitting Smoking at Smokeintensity of 5',
                                      'Effect of Quitting Smoking at Smokeintensity of 40'))

# Biramo intenzitete pušenja 5 i 40 cigareta dnevno
K1[1:2, 'qsmk'] <- 1
K1[1:2, 'I(qsmk * smokeintensity)'] <- c(5, 40)


K1 

# Računanje efekata
estimates1 <- glht(fit, K1)
  summary(estimates1)
  confint(estimates1)

# Linearni model bez interakcionog termina
fit2 <- glm(wt82_71 ~ qsmk + sex + race + age + I(age*age) + as.factor(education)
           + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
           + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
           + wt71 + I(wt71*wt71), data=nhefs)
  
summary(fit2)

# Poredimo model sa i bez qsmk*smokeintensity
anova(fit2, fit, test="Chisq")
# Poredimo mere kvaliteta modela
AIC(fit2, fit)
BIC(fit2, fit)

# Pravimo model logističke regresije kako bismo izračunali verovatnoću dodele tretmana za svaku jedinicu
fit3 <- glm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
            + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
            + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
            + wt71 + I(wt71*wt71), data=nhefs, family=binomial())
summary(fit3)

# Bazi podataka dodajemo kolonu sa vrednostima pomenute verovatnoće
nhefs$ps <- predict(fit3, nhefs, type="response")

# Prikaz statistika onih koji su prestali da puše i onih koji nisu
summary(nhefs$ps[nhefs$qsmk==0])
summary(nhefs$ps[nhefs$qsmk==1])



# Plot gustine raspodele, za procenu preklapanja
ggplot(nhefs, aes(x = ps, fill = as.factor(qsmk))) + geom_density(alpha = 0.2) +
  xlab('Verovatnoća prestanka pusenja između dve ankete') +
  ylab('Gustina') + 
  ggtitle('Verovatnoća dodele tretmana po tretmanskoj grupi') +
  scale_fill_discrete('') +
  theme(
    #legend.position = c(0.95, 0.95),
    legend.justification = c('right', 'top'), 
    legend.direction = 'vertical')


# Histogram sa konkretnim brojem ljudi u obe kontrolne grupe
nhefs %>%
  mutate(ps.grp = round(ps/0.05) * 0.05) %>%
  group_by(qsmk, ps.grp) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(n2 = ifelse(qsmk == 0, yes = n, no =  -1*n)) %>%
  ggplot(aes(x = ps.grp, y = n2, fill = as.factor(qsmk))) +
  geom_bar(stat = 'identity', position = 'identity') +
  geom_text(aes(label = n, x = ps.grp, y = n2 + ifelse(qsmk == 0, 8, -8))) +
  xlab('Verovatnoća prestanka pusenja između dve ankete') +
  ylab('N') +
  ggtitle('Verovatnoća dodele tretmana po tretmanskoj grupi') +
  scale_fill_discrete('') +
  scale_x_continuous(breaks = seq(0, 1, 0.05)) +
  theme(legend.justification = c('right', 'top'), legend.direction = 'vertical',
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())




# Primena matchit funkcije, čiji su rezultat indeksi uparenih jedinica sa najbližom verovatnoćom dodele tretmana 
ps_model <- matchit(
  qsmk ~ sex + race + age + I(age*age) + as.factor(education)
  + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
  + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
  + wt71 + I(wt71*wt71), data=nhefs,              
  method = "nearest",        # nearest neighbor matcing
  distance = "logit"         # koristi logistički model
)
summary(ps_model)

# Pravimo novu bazu sa uparenim jedinicama
matched_data <- match.data(ps_model, distance = 'prop_score', data = nhefs)

# Pravimo model linearne regresije nad uparenim podacima
fit_matched <- glm(wt82_71 ~ qsmk + sex + race + age + I(age*age) + as.factor(education)
                   + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
                   + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
                   + wt71 + I(wt71*wt71), data=matched_data)
summary(fit_matched)
confint(fit_matched)['qsmk', ]


# Stratifikacija na osnovu uparenih podataka
matched_data$ps.dec <- cut(matched_data$ps, 
                    breaks=c(quantile(matched_data$ps, probs=seq(0,1,0.1))),
                    labels=seq(1:10),
                    include.lowest=TRUE)

# Ispisujemo deskriptivne statistike po stratumima
tapply(matched_data$ps, matched_data$ps.dec, summary)

decile <- function(x) {
  return(factor(quantcut(x, seq(0, 1, 0.1), labels = FALSE)))
}

# Regresija sa faktorom za kontrolu razlika u raspodeli kovarijata između tretiranih i kontrolnih jedinica, 
# Na ovaj način dobijamo globalnu procenu efekta nad tretiranim jedinicama ATT
fit.psdec_matched <- glm(wt82_71 ~ qsmk + as.factor(ps.dec), data = matched_data)
summary(fit.psdec_matched)

# Kauzalni efekat, ATT 
fit.psdec_matched$coefficients['qsmk']

# Interval poverenja za globalni efekat
confint(fit.psdec_matched)['qsmk', ]

# pojedinacni intervali poverenja za procenu efekta na pojedinacnim stratumima 
for (deciles in c(1:10)) {
  fit.psdec <- glm(wt82_71 ~ qsmk, data=matched_data[which(matched_data$ps.dec==deciles),])
  # interval poverenja za svaki stratum
  print(confint(fit.psdec))
  # kauzalni efekat po stratumu
  print(fit.psdec$coefficients['qsmk'])
}

# stratifikacija na celom uzorku
nhefs$ps.dec <- cut(nhefs$ps, 
                    breaks=c(quantile(nhefs$ps, probs=seq(0,1,0.1))),
                    labels=seq(1:10),
                    include.lowest=TRUE)

# Ispisujemo deskriptivne statistike po stratumima
tapply(nhefs$ps, nhefs$ps.dec, summary)


decile <- function(x) {
  return(factor(quantcut(x, seq(0, 1, 0.1), labels = FALSE)))
}

# pravi se regresija u kojoj ne posmatramo efekat tretmana zasebno za svaki stratum 
# i za svaki stratum se taj efekat kontrolise, jaca statisticka snaga, dobija se globalni ATE
fit.psdec_all <- glm(wt82_71 ~ qsmk + as.factor(ps.dec), data = nhefs)
summary(fit.psdec_all)

# Kauzalni efekat, globalni. 
fit.psdec_all$coefficients['qsmk']

# interval poverenja za globalni efekat
confint(fit.psdec_all)['qsmk', ]

# pojedinacni intervali poverenja za procenu efekta na pojedinacnim stratumima 
for (deciles in c(1:10)) {
fit.psdec <- glm(wt82_71 ~ qsmk, data=nhefs[which(nhefs$ps.dec==deciles),])
# interval poverenja za svaki stratum
print(confint(fit.psdec))
# kauzalni efekat po stratumu
print(fit.psdec$coefficients['qsmk'])
}
