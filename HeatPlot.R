library(devtools)
library(CWDsims)
require(tidyr)
require(dplyr)
require(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(RColorBrewer)
################################################################
#These whould largely follow the "Example" values
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
env.foi = 0
n0 = 10000
n.years = 10
rel.risk = 1.0

hunt.mort.ad.f    <- seq(0, .95, length.out = 19) #vector of female harvest to run through
hunt.mort.ad.m    <- seq(0, .95, length.out = 19) #vector of male harvest to run through
beta.f            <- c(0.04, 0.08, 0.12) #vector of female transmission to run through
beta.m            <- c(0.04, 0.08, 0.12) #vector of male transmission to run through
theta             <- c(1,0) #freq vs dens dep

cutoff <- 9.75 #take values from the final time period

#for sim - items to hold intermediates and output
out <- list() 
pop.sum <- list() 
prev.sum <- list()

hunt.f <- c()
hunt.m <- c()
beta.f.1 <- c()
beta.m.1 <- c()
theta.1 <- c()
fin.pop <- c()
fin.prev <- c()

#sim
for (m in 1:length(theta)){
  for (l in 1:length(beta.m)){
    for (k in 1:length(beta.f)){
      for (j in 1:length(hunt.mort.ad.m)){
        for (i in 1:length(hunt.mort.ad.f)){
          out[[i]] <- cwd_det_model(params = list(fawn.an.sur = fawn.an.sur,
                                                  juv.an.sur = juv.an.sur,
                                                  ad.an.f.sur = ad.an.f.sur,
                                                  ad.an.m.sur = ad.an.m.sur,
                                                  fawn.repro = fawn.repro,
                                                  juv.repro = juv.repro,
                                                  ad.repro = ad.repro,
                                                  hunt.mort.fawn = hunt.mort.fawn,
                                                  hunt.mort.juv.f = hunt.mort.juv.f,
                                                  hunt.mort.juv.m = hunt.mort.juv.m,
                                                  hunt.mort.ad.f = hunt.mort.ad.f[i],
                                                  hunt.mort.ad.m = hunt.mort.ad.m[j],
                                                  ini.fawn.prev = ini.fawn.prev,
                                                  ini.juv.prev = ini.juv.prev,
                                                  ini.ad.f.prev = ini.ad.f.prev,
                                                  ini.ad.m.prev = ini.ad.m.prev,
                                                  n.age.cats = n.age.cats,
                                                  p = p,
                                                  env.foi = .001,
                                                  beta.f = beta.f[k], 
                                                  beta.m = beta.m[l], 
                                                  theta = theta[m],
                                                  n0 = n0, 
                                                  n.years = n.years,
                                                  rel.risk = rel.risk))
          pop.sum[[i]] <- out[[i]]$counts %>%
            filter(month %% 12 == 10) %>%
            group_by(year) %>%              
            summarize(n = sum(population)) #Total pop at time 
         
           prev.sum[[i]]   <- out[[i]]$counts %>%
            filter(month %% 12 == 10) %>%
            group_by(year, disease) %>%
            summarize(n = sum(population)) %>%
            pivot_wider(names_from = disease, values_from = n) %>%
            mutate(prev = yes/(no + yes)) #Prev at time
           
          #If you know a way to index these, it would speed the sim
          fin.pop <- c(fin.pop, (pop.sum[[i]]$n[pop.sum[[i]]$year==cutoff]))
          fin.prev <- c(fin.prev, prev.sum[[i]]$prev[prev.sum[[i]]$year==cutoff])
          hunt.f <- c(hunt.f, hunt.mort.ad.f[i])
          hunt.m <- c(hunt.m, hunt.mort.ad.m[j])
          beta.f.1 <- c(beta.f.1, beta.f[k])
          beta.m.1 <- c(beta.m.1, beta.m[l])
          theta.1 <- c(theta.1, theta[m])
          }
        }
      }
    }
  }
    
pop.cut <- 1000 #population critical value
prev.cut <- 0.5 #prevalence critical value
pop.cut.d <- 0
prev.cut.d <-1

df <- data.frame(hunt.f,hunt.m,fin.pop,fin.prev, beta.f.1, beta.m.1, theta.1)

df$ratio <- (df$fin.pop/(df$fin.prev))

df$beta.f.1[which(df$beta.f.1 == 0.04)] <- "F Beta = 0.04"
df$beta.f.1[which(df$beta.f.1 == 0.08)] <- "F Beta = 0.08"
df$beta.f.1[which(df$beta.f.1 == 0.12)] <- "F Beta = 0.12"
df$beta.m.1[which(df$beta.m.1 == 0.04)] <- "M Beta = 0.04"
df$beta.m.1[which(df$beta.m.1 == 0.08)] <- "M Beta = 0.08"
df$beta.m.1[which(df$beta.m.1 == 0.12)] <- "M Beta = 0.12"

df$class <- rep(NA, length(df$hunt.f))
df$class[which(df$theta.1==1 & df$fin.pop >= pop.cut & df$fin.prev <= prev.cut)] <- "Survive"
df$class[which(df$theta.1==1 & df$fin.pop < pop.cut & df$fin.prev <= prev.cut)] <- "Crash"
df$class[which(df$theta.1==1 & df$fin.pop >= pop.cut & df$fin.prev > prev.cut)] <- "Disease"
df$class[which(df$theta.1==1 & df$fin.pop < pop.cut & df$fin.prev > prev.cut)] <- "Crash+Disease"
df$class[which(df$theta.1==0 & df$fin.pop >= pop.cut.d & df$fin.prev <= prev.cut.d)] <- "Survive"
df$class[which(df$theta.1==0 & df$fin.pop < pop.cut.d & df$fin.prev <= prev.cut.d)] <- "Crash"
df$class[which(df$theta.1==0 & df$fin.pop >= pop.cut.d & df$fin.prev > prev.cut.d)] <- "Disease"
df$class[which(df$theta.1==0 & df$fin.pop < pop.cut.d & df$fin.prev > prev.cut.d)] <- "Crash+Disease"
df$class[is.na(df$fin.pop)] <- "N/A"
df$class <- factor(df$class)

write.csv(df, "tileplotdf.csv")
df <- read.csv("tileplotdf.csv")

freq.dep <- 
  ggplot(df[which(df$theta.1==1),], aes(x=hunt.f, y=hunt.m, fill=class)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",cutoff,"Years"), subtitle = "Theta = 1; Crit Pop 100; Crit Prev .6") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.ratio <- 
  ggplot(df[which(df$theta.1==1),], aes(x=hunt.f, y=hunt.m, fill=fin.prev)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",cutoff,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

density.dep <- 
  ggplot(df[which(df$theta.1==0),], aes(x=hunt.f, y=hunt.m, fill=class)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",cutoff,"Years"), subtitle = "Theta = 0; Crit Pop 20; Crit Prev 0.7") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.ratio<- 
  ggplot(df[which(df$theta.1==0),], aes(x=hunt.f, y=hunt.m, fill=ratio)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",cutoff,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

grid.arrange(freq.dep, density.dep, ncol=2)

gridExtra::grid.arrange(freq.ratio, dens.ratio, ncol=2)





