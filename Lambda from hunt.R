############################################################################
#Sensitivity of Lambda and Seroprevalence to differential harvest management 
############################################################################

# making population matricies for each harvest regime. 
library(devtools)
require(CWDsims)
require(popbio)
require(tidyr)
require(dplyr)
require(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(RColorBrewer)
library(reshape2)
library(stringr)
#launchApp("CWDapp")

##########################
#Editing Function
##########################

#' params <- list(fawn.an.sur = 0.6, juv.an.sur = 0.8, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.6, ad.repro = 1, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.1, hunt.mort.ad.m = 0.2, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.43, env.foi = 0,  beta.f = 0.08,  beta.m = 0.08,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0)

cwd_det_model_sensitivity <- function(params) {
  # write the list objects to the local environment
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])
  
  ######### CREATE INITIAL CONDITIONS########## monthly index
  months <- seq(1, n.years * 12)  # monthly timestep
  hunt.mo <- rep(0, n.years * 12)  # months in where the hunt occurs
  hunt.mo[months%%12 == 7] <- 1  # hunt.mo==1 on Nov
  
  # Natural monthly survival rates
  fawn.sur <- fawn.an.sur^(1/12)
  juv.sur <- juv.an.sur^(1/12)
  ad.f.sur <- ad.an.f.sur^(1/12)
  ad.m.sur <- ad.an.m.sur^(1/12)
  
  # group into a vector initial female prevalence
  ini.f.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.f.prev, (n.age.cats -
                                                                     2)))
  # initial male prevalence
  ini.m.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.m.prev, (n.age.cats -
                                                                     2)))
  
  # Create the survival and birth vectors vector of survival rates for 12
  # age classes
  Sur.f <- c(fawn.sur, juv.sur, rep(ad.f.sur, n.age.cats - 2))
  Sur.m <- c(fawn.sur, juv.sur, rep(ad.m.sur, n.age.cats - 2))
  
  # Create the Leslie Matrix to start the population at stable age dist
  M <- matrix(rep(0, n.age.cats * 2 * n.age.cats * 2), nrow = n.age.cats *
                2)
  # replace the -1 off-diagonal with the survival rates
  M[row(M) == (col(M) + 1)] <- c(juv.an.sur * (1 - hunt.mort.juv.f),
                                 rep(ad.an.f.sur * (1 - hunt.mort.ad.f),
                                     n.age.cats - 2), 0,
                                 c(juv.an.sur *
                                     (1 - hunt.mort.juv.m),
                                   rep(ad.an.m.sur * (1 - hunt.mort.ad.m),
                                       n.age.cats - 2)))
  # if you want the top age category to continue to survive adult female
  # survival in top age cat
  M[n.age.cats, n.age.cats] <- ad.an.f.sur * (1 - hunt.mort.ad.f)
  # adult male survival in top age cat
  M[n.age.cats * 2, n.age.cats * 2] <- ad.an.m.sur * (1 - hunt.mort.ad.m)
  
  # insert the fecundity vector for prebirth census
  M[1, 1:n.age.cats] <- c(0, juv.repro, rep(ad.repro, n.age.cats - 2)) *
    0.5 * fawn.an.sur * (1 - hunt.mort.fawn)
  M[n.age.cats + 1, 1:n.age.cats] <- M[1, 1:n.age.cats]
  
  # calculate R0s for adult females and males
  # in the denominator find the average minimum survival for the 3 mortality types
  f.R0 <-  (beta.f * n0) / (n0 ^ theta) * 
    mean(apply(cbind(rnbinom(1000, 1, (1 - ad.an.f.sur^(1/12))), 
                     rnbinom(1000, 1, (1 - (1 - hunt.mort.ad.f)^(1/12))),
                     rgamma(1000, 10, p)), 1, FUN = min, na.rm = T))
  
  m.R0 <-  (beta.m * n0)  / (n0 ^ theta) *
    mean(apply(cbind(rnbinom(1000, 1, (1 - ad.an.m.sur^(1/12))), 
                     rnbinom(1000, 1, (1 - (1 - hunt.mort.ad.m)^(1/12))),
                     rgamma(1000, 10, p)), 1, FUN = min, na.rm = T))
  
  lambda <- popbio::lambda(M)
  
  m <- M
  output <- list(f.R0 = f.R0, m.R0 = m.R0, lambda = lambda, matrix=m)
  
}


##########################3

fawn.an.sur = 0.6
juv.an.sur = 0.8
ad.an.f.sur = 0.95
ad.an.m.sur = 0.9
fawn.repro = 0
juv.repro = 0.6
ad.repro = 1
hunt.mort.fawn = 0.01
hunt.mort.juv.f = 0.1
hunt.mort.juv.m = 0.1
ini.fawn.prev = 0.02
ini.juv.prev = 0.03
ini.ad.f.prev = 0.04
ini.ad.m.prev = 0.04
n.age.cats = 12
p = 0.43
env.foi = 0.001
n0 = 10000
n.years = 25
rel.risk = 1.0

cut2.75 <- 2.75
cut4.75 <- 4.75
cut9.75 <- 9.75
cut14.75 <- 14.75
cut19.75 <- 19.75
cut24.75 <- 24.75

hunt.mort.ad.f    <- seq(0, .99, length.out = 99) #vector of female harvest to run through
hunt.mort.ad.m    <- seq(0, .99, length.out = 99) #vector of male harvest to run through

hunt.f <- c(seq(0, .99, by= .01), rep(.1, 100*3))
hunt.m <- c(rep(.2, 100), seq(0, .99, by= .01),rep(.2, 100*2))
hunt.j.m <- c(rep(.2, 100*2), seq(0, .99, by= .01),rep(.2, 100))
hunt.j.f <- c(rep(.1, 100*3), seq(0, .99, by= .01))
# df.dens <- data.frame(hunt.f,hunt.m, beta.f.1, beta.m.1, theta.1 =rep(0,length(hunt.f)))
# df.dens$beta.f.1 <- df.dens$beta.f.1/(n0*.8)
# df.dens$beta.m.1 <- df.dens$beta.m.1/(n0*.8)

lambda <- rep(NA, length(hunt.f))
for (i in 1:length(hunt.f)){
  out <- cwd_det_model_sensitivity(params = list(fawn.an.sur = fawn.an.sur,
                                          juv.an.sur = juv.an.sur,
                                          ad.an.f.sur = ad.an.f.sur,
                                          ad.an.m.sur = ad.an.m.sur,
                                          fawn.repro = fawn.repro,
                                          juv.repro = juv.repro,
                                          ad.repro = ad.repro,
                                          hunt.mort.fawn = hunt.mort.fawn,
                                          hunt.mort.juv.f = hunt.j.f[i],
                                          hunt.mort.juv.m = hunt.j.m[i],
                                          hunt.mort.ad.f = hunt.f[i],
                                          hunt.mort.ad.m = hunt.m[i],
                                          ini.fawn.prev = ini.fawn.prev,
                                          ini.juv.prev = ini.juv.prev,
                                          ini.ad.f.prev = ini.ad.f.prev,
                                          ini.ad.m.prev = ini.ad.m.prev,
                                          n.age.cats = n.age.cats,
                                          p = p,
                                          env.foi = env.foi,
                                          beta.f = .8,
                                          beta.m = .8,
                                          theta = 1,
                                          n0 = n0,
                                          n.years = n.years,
                                          rel.risk = rel.risk))
  lambda[i] <- out$lambda
}

df <- data.frame(hunt.f, hunt.m, hunt.j.f, hunt.j.m, lambda)

head(df)
require(dplyr)
require(ggplot2)
# f.dom <- df.freq %>% 
#   filter(beta.f.1==0.12 &
#            beta.m.1==0.04)
# m.dom <- df.freq %>% 
#   filter(beta.f.1==0.04 &
#            beta.m.1==0.12)
# equal <- df.freq %>% 
#   filter(beta.f.1==0.08 &
#            beta.m.1==0.08)
df.c <- melt(df, id.vars = "lambda")
df.c <- df.c[c(1:100,501:600,1101:1200,1401:1500),]
df.c %>% 
  ggplot(aes(value, lambda, color = variable)) + 
  geom_line()+
  scale_color_discrete(name = "Demo Param") + 
  theme_classic()

out <- cwd_det_model(params = list(fawn.an.sur = fawn.an.sur,
                            juv.an.sur = juv.an.sur,
                            ad.an.f.sur = ad.an.f.sur,
                            ad.an.m.sur = ad.an.m.sur,
                            fawn.repro = fawn.repro,
                            juv.repro = juv.repro,
                            ad.repro = ad.repro,
                            hunt.mort.fawn = hunt.mort.fawn,
                            hunt.mort.juv.f = hunt.mort.juv.f,
                            hunt.mort.juv.m = hunt.mort.juv.m,
                            ini.fawn.prev = ini.fawn.prev,
                            ini.juv.prev = ini.juv.prev,
                            ini.ad.f.prev = ini.ad.f.prev,
                            ini.ad.m.prev = ini.ad.m.prev,
                            n.age.cats = n.age.cats,
                            p = p,
                            env.foi = env.foi,
                            theta = 1,
                            n0 = n0,
                            n.years = n.years,
                            rel.risk = rel.risk,
                            beta.f = 0.08,
                            beta.m = 0.08,
                            hunt.mort.ad.f = .4,
                            hunt.mort.ad.m = .02))



#####################
# #Freq Dep
#####################
df <- df[rep(seq_len(nrow(df)), 3), ]
df$beta.f <- c(rep(.08,400), rep(.04,400), rep(.12,400))
df$beta.m <- c(rep(.08,400), rep(.12,400), rep(.04,400))

#for sim - items to hold intermediates and output
df$fin.pop9.75   <- rep(NA, length(df$hunt.f))
df$fin.prev9.75  <- rep(NA, length(df$hunt.f))
df$fin.pop19.75  <- rep(NA, length(df$hunt.f))
df$fin.prev19.75 <- rep(NA, length(df$hunt.f))
df$fin.pop24.75  <- rep(NA, length(df$hunt.f))
df$fin.prev24.75 <- rep(NA, length(df$hunt.f))
df$total.deaths.f <- rep(NA, length(df$hunt.f))
df$total.deaths.m <- rep(NA, length(df$hunt.f))

out <- list()
pop.sum <- list()
prev.sum <- list()
df.freq <- list(df.freq)

for (i in 1:length(df$hunt.f)){
  out <- CWDsims::cwd_det_model(params = list(fawn.an.sur = fawn.an.sur,
                                          juv.an.sur = juv.an.sur,
                                          ad.an.f.sur = ad.an.f.sur,
                                          ad.an.m.sur = ad.an.m.sur,
                                          fawn.repro = fawn.repro,
                                          juv.repro = juv.repro,
                                          ad.repro = ad.repro,
                                          hunt.mort.fawn = hunt.mort.fawn,
                                          hunt.mort.juv.f = df$hunt.j.f[i],
                                          hunt.mort.juv.m = df$hunt.j.m[i],
                                          hunt.mort.ad.f = df$hunt.f[i],
                                          hunt.mort.ad.m = df$hunt.m[i],
                                          ini.fawn.prev = ini.fawn.prev,
                                          ini.juv.prev = ini.juv.prev,
                                          ini.ad.f.prev = ini.ad.f.prev,
                                          ini.ad.m.prev = ini.ad.m.prev,
                                          n.age.cats = n.age.cats,
                                          p = p,
                                          env.foi = env.foi,
                                          beta.f = df$beta.f[i],
                                          beta.m = df$beta.m[i],
                                          theta = 1,
                                          n0 = n0,
                                          n.years = 25,
                                          rel.risk = rel.risk))
  pop.sum <- out$counts %>%
    filter(month %% 12 == 10) %>%
    group_by(year) %>%
    summarize(n = sum(population)) #Total pop at time

  prev.sum   <- out$counts %>%
    filter(month %% 12 == 10) %>%
    group_by(year, disease) %>%
    summarize(n = sum(population)) %>%
    pivot_wider(names_from = disease, values_from = n) %>%
    mutate(prev = yes/(no + yes)) #Prev at time

  #If you know a way to index these, it would speed the sim
  df$fin.pop9.75[i]    <- pop.sum$n[pop.sum$year == 9.75]
  df$fin.pop19.75[i]   <- pop.sum$n[pop.sum$year == 19.75]
  df$fin.pop24.75[i]   <- pop.sum$n[pop.sum$year == 24.75]
  df$fin.prev9.75[i]   <- prev.sum$prev[prev.sum$year == 9.75]
  df$fin.prev19.75[i]  <- prev.sum$prev[prev.sum$year == 19.75]
  df$fin.prev24.75[i]  <- prev.sum$prev[prev.sum$year == 24.75]
  
  #number of deaths by hunting by sex
  deaths.sum   <- out$deaths %>%
    filter(category == "Ht.f" |
             category == "Ht.m") %>%
    group_by(category) %>%
    summarize(n = sum(population)) %>%
    pivot_wider(names_from = category, values_from = n) 
  
  df$total.deaths.f[i] <- deaths.sum$Ht.f[1]
  df$total.deaths.m[i] <- deaths.sum$Ht.m[1]
}

df. <- melt(df, id.vars = c("fin.pop9.75",   "fin.prev9.75",  "fin.pop19.75", 
                        "fin.prev19.75", "fin.pop24.75",  "fin.prev24.75",
                        "lambda", "beta.f", "beta.m", "total.deaths.f", "total.deaths.m"))

equal <- df. %>%
  filter(
    beta.f == .08 &
    beta.m == .08
  )
male.dom <- df. %>%
  filter(
    beta.f == .04 &
      beta.m == .12
  )
female.dom <- df. %>%
  filter(
    beta.f == .12 &
      beta.m == .04
  )
equal <- equal [c(1:100,501:600,1101:1200,1401:1500),]
equal$scenario <- "equal"
male.dom <- male.dom [c(1:100,501:600,1101:1200,1401:1500),]
male.dom$scenario <- "male.dom"
female.dom <- female.dom [c(1:100,501:600,1101:1200,1401:1500),]
female.dom$scenario <- "female.dom"
rbind(equal,male.dom,female.dom)
df.a <- rbind(equal,male.dom,female.dom)  

prev <- df.a %>% 
  ggplot(aes(value, fin.prev19.75, color = variable)) + 
  geom_line()+
  facet_wrap(~scenario)+
  labs(x = "Rate of Harvest",
       y = "Prevalence (at 19.75 Years)",
       color = "Hunting Parameter") +
  scale_color_discrete(labels = c(
    "Adult Female Hunting",
    "Adult Male Hunting",
    "Juvenile Female Hunting",
    "Juvenile Male Hunting"
  )) +
  theme_classic()

pop <- df.a%>% 
  ggplot(aes(value, log(fin.pop19.75), color = variable)) + 
  geom_line()+
  facet_wrap(~scenario)+
  labs(x = "Rate of Harvest",
       y = "Population (at 19.75 Years)",
       color = "Hunting Parameter") +
  scale_color_discrete(labels = c(
    "Adult Female Hunting",
    "Adult Male Hunting",
    "Juvenile Female Hunting",
    "Juvenile Male Hunting"
  )) +
  theme_classic()

df.a$index9.75 <- (df.a$fin.pop19.75/(1-df.a$fin.prev19.75))

index <- df.a%>% 
  ggplot(aes(value, index9.75, color = variable)) + 
  geom_line()+
  facet_wrap(~scenario)+
  theme_classic()

df.a$R0 <- 1/(1-df.a$fin.prev19.75)

R0 <- df.a%>% 
  ggplot(aes(value, R0, color = variable)) + 
  geom_line()+
  facet_wrap(~scenario)+
  labs(x = "Rate of Harvest",
       y = expression(R[0]),
       color = "Hunting Parameter") +
  scale_color_discrete(labels = c(
    "Adult Female Hunting",
    "Adult Male Hunting",
    "Juvenile Female Hunting",
    "Juvenile Male Hunting"
  )) +
  theme_classic()

lambda <- df.a%>% 
  ggplot(aes(value, lambda, color = variable)) + 
  geom_line()+
  theme_classic()

require(ggpubr)
ggarrange(prev, pop, R0, ncol=1, common.legend = T, legend = "right")
ggsave("2D_comparisons.png", dpi=300, width = 5, height = 15)

df.a %>% 
  ggplot(aes(value,total.deaths.f, color = variable)) + 
  geom_line()+
  facet_wrap(~scenario)+
  theme_classic()

df.a %>% 
  ggplot(aes(fin.prev24.75,total.deaths.f, color = variable)) + 
  geom_line()+
  facet_wrap(~scenario)+
  theme_classic()

df.a %>% 
  ggplot(aes(lambda,total.deaths.f, color = variable)) + 
  geom_line()+
  facet_wrap(~scenario)+
  theme_classic()





