library(devtools)
library(CWDsims)
require(tidyr)
require(dplyr)
require(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(RColorBrewer)
#####################
#Building df
#####################
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
beta.f            <- c(0.04, 0.08, 0.12) #vector of female transmission to run through
beta.m            <- c(0.04, 0.08, 0.12) #vector of male transmission to run through

# hunt.f <- c()
# hunt.m <- c()
# beta.f.1 <- c()
# beta.m.1 <- c()
# 
# for (l in 1:length(beta.m)){
#   for (k in 1:length(beta.f)){
#     for (j in 1:length(hunt.mort.ad.m)){
#       for (i in 1:length(hunt.mort.ad.f)){
#         hunt.f <- c(hunt.f, hunt.mort.ad.f[i])
#         hunt.m <- c(hunt.m, hunt.mort.ad.m[j])
#         beta.f.1 <- c(beta.f.1, beta.f[k])
#         beta.m.1 <- c(beta.m.1, beta.m[l])
#       }
#     }
#   }
# }
# 
# #df.freq <- data.frame(hunt.f,hunt.m, beta.f.1, beta.m.1, theta.1 = rep(1,length(hunt.f)))
# 
# # df.dens <- data.frame(hunt.f,hunt.m, beta.f.1, beta.m.1, theta.1 =rep(0,length(hunt.f)))
# # df.dens$beta.f.1 <- df.dens$beta.f.1/(n0*.8)
# # df.dens$beta.m.1 <- df.dens$beta.m.1/(n0*.8)
# 
#####################
# #Freq Dep
#####################
# 
# #for sim - items to hold intermediates and output
# df.freq$fin.pop2.75   <- rep(NA, length(df.freq$hunt.f))
# df.freq$fin.prev2.75  <- rep(NA, length(df.freq$hunt.f))
# df.freq$fin.pop4.75   <- rep(NA, length(df.freq$hunt.f))
# df.freq$fin.prev4.75  <- rep(NA, length(df.freq$hunt.f))
# df.freq$fin.pop9.75   <- rep(NA, length(df.freq$hunt.f))
# df.freq$fin.prev9.75  <- rep(NA, length(df.freq$hunt.f))
# df.freq$fin.pop14.75  <- rep(NA, length(df.freq$hunt.f))
# df.freq$fin.prev14.75 <- rep(NA, length(df.freq$hunt.f))
# df.freq$fin.pop19.75  <- rep(NA, length(df.freq$hunt.f))
# df.freq$fin.prev19.75 <- rep(NA, length(df.freq$hunt.f))
# df.freq$fin.pop24.75  <- rep(NA, length(df.freq$hunt.f))
# df.freq$fin.prev24.75 <- rep(NA, length(df.freq$hunt.f))
# 
# out <- list() 
# pop.sum <- list() 
# prev.sum <- list()
# df.freq <- list(df.freq)
# 
# for (i in 1:length(df.freq[[1]]$hunt.f)){
#   out <- cwd_det_model(params = list(fawn.an.sur = fawn.an.sur,
#                                           juv.an.sur = juv.an.sur,
#                                           ad.an.f.sur = ad.an.f.sur,
#                                           ad.an.m.sur = ad.an.m.sur,
#                                           fawn.repro = fawn.repro,
#                                           juv.repro = juv.repro,
#                                           ad.repro = ad.repro,
#                                           hunt.mort.fawn = hunt.mort.fawn,
#                                           hunt.mort.juv.f = hunt.mort.juv.f,
#                                           hunt.mort.juv.m = hunt.mort.juv.m,
#                                           hunt.mort.ad.f = df.freq[[1]]$hunt.f[i],
#                                           hunt.mort.ad.m = df.freq[[1]]$hunt.m[i],
#                                           ini.fawn.prev = ini.fawn.prev,
#                                           ini.juv.prev = ini.juv.prev,
#                                           ini.ad.f.prev = ini.ad.f.prev,
#                                           ini.ad.m.prev = ini.ad.m.prev,
#                                           n.age.cats = n.age.cats,
#                                           p = p,
#                                           env.foi = env.foi,
#                                           beta.f = df.freq[[1]]$beta.f.1[i], 
#                                           beta.m = df.freq[[1]]$beta.m.1[i], 
#                                           theta = 1,
#                                           n0 = n0, 
#                                           n.years = n.years,
#                                           rel.risk = rel.risk))
#   pop.sum <- out$counts %>%
#     filter(month %% 12 == 10) %>%
#     group_by(year) %>%              
#     summarize(n = sum(population)) #Total pop at time 
#   
#   prev.sum   <- out$counts %>%
#     filter(month %% 12 == 10) %>%
#     group_by(year, disease) %>%
#     summarize(n = sum(population)) %>%
#     pivot_wider(names_from = disease, values_from = n) %>%
#     mutate(prev = yes/(no + yes)) #Prev at time
#   
#   #If you know a way to index these, it would speed the sim
#   df.freq[[1]]$fin.pop2.75[i]    <- pop.sum$n[pop.sum$year==cut2.75]
#   df.freq[[1]]$fin.pop4.75[i]    <- pop.sum$n[pop.sum$year==cut4.75]
#   df.freq[[1]]$fin.pop9.75[i]    <- pop.sum$n[pop.sum$year==cut9.75]
#   df.freq[[1]]$fin.pop14.75[i]   <- pop.sum$n[pop.sum$year==cut14.75]
#   df.freq[[1]]$fin.pop19.75[i]   <- pop.sum$n[pop.sum$year==cut19.75]
#   df.freq[[1]]$fin.pop24.75[i]   <- pop.sum$n[pop.sum$year==cut24.75]
#   df.freq[[1]]$fin.prev2.75[i]   <- prev.sum$prev[prev.sum$year==cut2.75]
#   df.freq[[1]]$fin.prev4.75[i]   <- prev.sum$prev[prev.sum$year==cut4.75]
#   df.freq[[1]]$fin.prev9.75[i]   <- prev.sum$prev[prev.sum$year==cut9.75]
#   df.freq[[1]]$fin.prev14.75[i]  <- prev.sum$prev[prev.sum$year==cut14.75]
#   df.freq[[1]]$fin.prev19.75[i]  <- prev.sum$prev[prev.sum$year==cut19.75]
#   df.freq[[1]]$fin.prev24.75[i]  <- prev.sum$prev[prev.sum$year==cut24.75]
# }
# 
# df.freq <- data.frame(df.freq[[1]])
# df.freq$beta.f.1[which(df.freq$beta.f.1 == 0.04)] <- "F Beta = 0.04"
# df.freq$beta.f.1[which(df.freq$beta.f.1 == 0.08)] <- "F Beta = 0.08"
# df.freq$beta.f.1[which(df.freq$beta.f.1 == 0.12)] <- "F Beta = 0.12"
# df.freq$beta.m.1[which(df.freq$beta.m.1 == 0.04)] <- "M Beta = 0.04"
# df.freq$beta.m.1[which(df.freq$beta.m.1 == 0.08)] <- "M Beta = 0.08"
# df.freq$beta.m.1[which(df.freq$beta.m.1 == 0.12)] <- "M Beta = 0.12"

pop.cut <- 3000 #population critical value
prev.cut <- 0.3 #prevalence critical value

# df.freq$class2.75 <- rep(NA, length(df.freq$hunt.f))
# df.freq$class2.75[which(df.freq$fin.pop2.75 >= pop.cut & df.freq$fin.prev2.75 <= prev.cut)] <- "Survive"
# df.freq$class2.75[which(df.freq$fin.pop2.75 < pop.cut & df.freq$fin.prev2.75 <= prev.cut)] <- "Crash"
# df.freq$class2.75[which(df.freq$fin.pop2.75 >= pop.cut & df.freq$fin.prev2.75 > prev.cut)] <- "Disease"
# df.freq$class2.75[which(df.freq$fin.pop2.75 < pop.cut & df.freq$fin.prev2.75 > prev.cut)] <- "Crash+Disease"
# df.freq$class2.75[is.na(df.freq$fin.pop2.75)] <- "NA/Crash"
# df.freq$class2.75 <- factor(df.freq$class2.75)
# 
# df.freq$class4.75 <- rep(NA, length(df.freq$hunt.f))
# df.freq$class4.75[which(df.freq$fin.pop4.75 >= pop.cut & df.freq$fin.prev4.75 <= prev.cut)] <- "Survive"
# df.freq$class4.75[which(df.freq$fin.pop4.75 < pop.cut & df.freq$fin.prev4.75 <= prev.cut)] <- "Crash"
# df.freq$class4.75[which(df.freq$fin.pop4.75 >= pop.cut & df.freq$fin.prev4.75 > prev.cut)] <- "Disease"
# df.freq$class4.75[which(df.freq$fin.pop4.75 < pop.cut & df.freq$fin.prev4.75 > prev.cut)] <- "Crash+Disease"
# df.freq$class4.75[is.na(df.freq$fin.pop4.75)] <- "NA/Crash"
# df.freq$class4.75 <- factor(df.freq$class4.75)
# 
# df.freq$class9.75 <- rep(NA, length(df.freq$hunt.f))
# df.freq$class9.75[which(df.freq$fin.pop9.75 >= pop.cut & df.freq$fin.prev9.75 <= prev.cut)] <- "Survive"
# df.freq$class9.75[which(df.freq$fin.pop9.75 < pop.cut & df.freq$fin.prev9.75 <= prev.cut)] <- "Crash"
# df.freq$class9.75[which(df.freq$fin.pop9.75 >= pop.cut & df.freq$fin.prev9.75 > prev.cut)] <- "Disease"
# df.freq$class9.75[which(df.freq$fin.pop9.75 < pop.cut & df.freq$fin.prev9.75 > prev.cut)] <- "Crash+Disease"
# df.freq$class9.75[is.na(df.freq$fin.pop9.75)] <- "NA/Crash"
# df.freq$class9.75 <- factor(df.freq$class9.75)
# 
# df.freq$class14.75 <- rep(NA, length(df.freq$hunt.f))
# df.freq$class14.75[which(df.freq$fin.pop14.75 >= pop.cut & df.freq$fin.prev14.75 <= prev.cut)] <- "Survive"
# df.freq$class14.75[which(df.freq$fin.pop14.75 < pop.cut & df.freq$fin.prev14.75 <= prev.cut)] <- "Crash"
# df.freq$class14.75[which(df.freq$fin.pop14.75 >= pop.cut & df.freq$fin.prev14.75 > prev.cut)] <- "Disease"
# df.freq$class14.75[which(df.freq$fin.pop14.75 < pop.cut & df.freq$fin.prev14.75 > prev.cut)] <- "Crash+Disease"
# df.freq$class14.75[is.na(df.freq$fin.pop14.75)] <- "NA/Crash"
# df.freq$class14.75 <- factor(df.freq$class14.75)
# 
# df.freq$class19.75 <- rep(NA, length(df.freq$hunt.f))
# df.freq$class19.75[which(df.freq$fin.pop19.75 >= pop.cut & df.freq$fin.prev19.75 <= prev.cut)] <- "Survive"
# df.freq$class19.75[which(df.freq$fin.pop19.75 < pop.cut & df.freq$fin.prev19.75 <= prev.cut)] <- "Crash"
# df.freq$class19.75[which(df.freq$fin.pop19.75 >= pop.cut & df.freq$fin.prev19.75 > prev.cut)] <- "Disease"
# df.freq$class19.75[which(df.freq$fin.pop19.75 < pop.cut & df.freq$fin.prev19.75 > prev.cut)] <- "Crash+Disease"
# df.freq$class19.75[is.na(df.freq$fin.pop19.75)] <- "NA/Crash"
# df.freq$class19.75 <- factor(df.freq$class19.75)
# 
# df.freq$class24.75 <- rep(NA, length(df.freq$hunt.f))
# df.freq$class24.75[which(df.freq$fin.pop24.75 >= pop.cut & df.freq$fin.prev24.75 <= prev.cut)] <- "Survive"
# df.freq$class24.75[which(df.freq$fin.pop24.75 < pop.cut & df.freq$fin.prev24.75 <= prev.cut)] <- "Crash"
# df.freq$class24.75[which(df.freq$fin.pop24.75 >= pop.cut & df.freq$fin.prev24.75 > prev.cut)] <- "Disease"
# df.freq$class24.75[which(df.freq$fin.pop24.75 < pop.cut & df.freq$fin.prev24.75 > prev.cut)] <- "Crash+Disease"
# df.freq$class24.75[is.na(df.freq$fin.pop24.75)] <- "NA/Crash"
# df.freq$class24.75 <- factor(df.freq$class24.75)

# write.csv(df.freq, "freqdf.csv")
df.freq <- read.csv("freqdf.csv")

#####################
#Frequency Plotting
#####################

levels(df.freq$class2.75)
order <- c("Survive", "Crash", "Disease", "Crash+Disease", "NA/Crash")
df.freq$class2.75  <- factor(df.freq$class2.75, levels = order)
df.freq$class4.75  <- factor(df.freq$class4.75, levels = order)
df.freq$class9.75  <- factor(df.freq$class9.75, levels = order)
df.freq$class14.75 <- factor(df.freq$class14.75, levels = order)
df.freq$class19.75 <- factor(df.freq$class19.75, levels = order)
df.freq$class24.75 <- factor(df.freq$class24.75, levels = order)

myColors <- c("#3288BD", "#ABDDA4", "#FFFFBF", "#D53E4F", "#F46D43")
names(myColors) <- order
colScalef <- scale_fill_manual(name = "class2.75",values = myColors)

freq.dep2.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=class2.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",2.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop2.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=fin.pop2.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",2.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev2.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=fin.prev2.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",2.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

freq.dep4.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=class4.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",4.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop4.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=fin.pop4.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",4.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev4.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=fin.prev4.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",4.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

freq.dep9.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=class9.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",9.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop9.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=fin.pop9.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",9.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev9.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=fin.prev9.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",9.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

freq.dep14.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=class14.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",14.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop14.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=fin.pop14.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",14.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev14.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=fin.prev14.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",14.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

freq.dep19.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=class19.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",19.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop19.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=fin.pop19.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",19.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev19.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=fin.prev19.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",19.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

freq.dep24.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=class24.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",24.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop24.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=log(fin.pop24.75))) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",24.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev24.75 <- 
  ggplot(df.freq, aes(x=hunt.f, y=hunt.m, fill=fin.prev24.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",24.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

#####################
#Density dependence 
#####################

# #for sim - items to hold intermediates and output
# df.dens$fin.pop2.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$fin.prev2.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$fin.pop4.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$fin.prev4.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$fin.pop9.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$fin.prev9.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$fin.pop14.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$fin.prev14.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$fin.pop19.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$fin.prev19.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$fin.pop24.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$fin.prev24.75 <- rep(NA, length(df.dens$hunt.f))
# 
# out <- list() 
# pop.sum <- list() 
# prev.sum <- list()
# df.dens <- list(df.dens)
# 
# for (i in 1:length(df.dens[[1]]$hunt.f)){
#   out <- cwd_det_model(params = list(fawn.an.sur = fawn.an.sur,
#                                           juv.an.sur = juv.an.sur,
#                                           ad.an.f.sur = ad.an.f.sur,
#                                           ad.an.m.sur = ad.an.m.sur,
#                                           fawn.repro = fawn.repro,
#                                           juv.repro = juv.repro,
#                                           ad.repro = ad.repro,
#                                           hunt.mort.fawn = hunt.mort.fawn,
#                                           hunt.mort.juv.f = hunt.mort.juv.f,
#                                           hunt.mort.juv.m = hunt.mort.juv.m,
#                                           hunt.mort.ad.f = df.dens[[1]]$hunt.f[i],
#                                           hunt.mort.ad.m = df.dens[[1]]$hunt.m[i],
#                                           ini.fawn.prev = ini.fawn.prev,
#                                           ini.juv.prev = ini.juv.prev,
#                                           ini.ad.f.prev = ini.ad.f.prev,
#                                           ini.ad.m.prev = ini.ad.m.prev,
#                                           n.age.cats = n.age.cats,
#                                           p = p,
#                                           env.foi = env.foi,
#                                           beta.f = df.dens[[1]]$beta.f.1[i], 
#                                           beta.m = df.dens[[1]]$beta.m.1[i], 
#                                           Theta = 1,
#                                           n0 = n0, 
#                                           n.years = n.years,
#                                           rel.risk = rel.risk))
#   pop.sum <- out$counts %>%
#     filter(month %% 12 == 10) %>%
#     group_by(year) %>%              
#     summarize(n = sum(population)) #Total pop at time 
#   
#   prev.sum   <- out$counts %>%
#     filter(month %% 12 == 10) %>%
#     group_by(year, disease) %>%
#     summarize(n = sum(population)) %>%
#     pivot_wider(names_from = disease, values_from = n) %>%
#     mutate(prev = yes/(no + yes)) #Prev at time
#   
#   #If you know a way to index these, it would speed the sim
#   df.dens[[1]]$fin.pop2.75[i] <- pop.sum$n[pop.sum$year==cut2.75]
#   df.dens[[1]]$fin.pop4.75[i] <- pop.sum$n[pop.sum$year==cut4.75]
#   df.dens[[1]]$fin.pop9.75[i] <- pop.sum$n[pop.sum$year==cut9.75]
#   df.dens[[1]]$fin.pop14.75[i] <- pop.sum$n[pop.sum$year==cut14.75]
#   df.dens[[1]]$fin.pop19.75[i] <- pop.sum$n[pop.sum$year==cut19.75]
#   df.dens[[1]]$fin.pop24.75[i] <- pop.sum$n[pop.sum$year==cut24.75]
#   df.dens[[1]]$fin.prev2.75[i] <- prev.sum$prev[prev.sum$year==cut2.75]
#   df.dens[[1]]$fin.prev4.75[i] <- prev.sum$prev[prev.sum$year==cut4.75]
#   df.dens[[1]]$fin.prev9.75[i] <- prev.sum$prev[prev.sum$year==cut9.75]
#   df.dens[[1]]$fin.prev14.75[i] <- prev.sum$prev[prev.sum$year==cut14.75]
#   df.dens[[1]]$fin.prev19.75[i] <- prev.sum$prev[prev.sum$year==cut19.75]
#   df.dens[[1]]$fin.prev24.75[i] <- prev.sum$prev[prev.sum$year==cut24.75]
# }
# 
# df.dens <- data.frame(df.dens[[1]])
# df.dens$beta.f.1[which(df.dens$beta.f.1 == 0.04/(n0*.8))] <- "F Beta = 5.0e-06"
# df.dens$beta.f.1[which(df.dens$beta.f.1 == 0.08/(n0*.8))] <- "F Beta = 1.0e-05"
# df.dens$beta.f.1[which(df.dens$beta.f.1 == 0.12/(n0*.8))] <- "F Beta = 1.5e-05"
# df.dens$beta.m.1[which(df.dens$beta.m.1 == 0.04/(n0*.8))] <- "M Beta = 5.0e-06"
# df.dens$beta.m.1[which(df.dens$beta.m.1 == 0.08/(n0*.8))] <- "M Beta = 1.0e-05"
# df.dens$beta.m.1[which(df.dens$beta.m.1 == 0.12/(n0*.8))] <- "M Beta = 1.5e-05"
# 
# df.dens$class2.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$class2.75[which(df.dens$fin.pop2.75 >= pop.cut & df.dens$fin.prev2.75 <= prev.cut)] <- "Survive"
# df.dens$class2.75[which(df.dens$fin.pop2.75 < pop.cut & df.dens$fin.prev2.75 <= prev.cut)] <- "Crash"
# df.dens$class2.75[which(df.dens$fin.pop2.75 >= pop.cut & df.dens$fin.prev2.75 > prev.cut)] <- "Disease"
# df.dens$class2.75[which(df.dens$fin.pop2.75 < pop.cut & df.dens$fin.prev2.75 > prev.cut)] <- "Crash+Disease"
# df.dens$class2.75[is.na(df.dens$fin.pop2.75)] <- "NA/Crash"
# df.dens$class2.75 <- factor(df.dens$class2.75)
# 
# df.dens$class4.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$class4.75[which(df.dens$fin.pop4.75 >= pop.cut & df.dens$fin.prev4.75 <= prev.cut)] <- "Survive"
# df.dens$class4.75[which(df.dens$fin.pop4.75 < pop.cut & df.dens$fin.prev4.75 <= prev.cut)] <- "Crash"
# df.dens$class4.75[which(df.dens$fin.pop4.75 >= pop.cut & df.dens$fin.prev4.75 > prev.cut)] <- "Disease"
# df.dens$class4.75[which(df.dens$fin.pop4.75 < pop.cut & df.dens$fin.prev4.75 > prev.cut)] <- "Crash+Disease"
# df.dens$class4.75[is.na(df.dens$fin.pop4.75)] <- "NA/Crash"
# df.dens$class4.75 <- factor(df.dens$class4.75)
# 
# df.dens$class9.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$class9.75[which(df.dens$fin.pop9.75 >= pop.cut & df.dens$fin.prev9.75 <= prev.cut)] <- "Survive"
# df.dens$class9.75[which(df.dens$fin.pop9.75 < pop.cut & df.dens$fin.prev9.75 <= prev.cut)] <- "Crash"
# df.dens$class9.75[which(df.dens$fin.pop9.75 >= pop.cut & df.dens$fin.prev9.75 > prev.cut)] <- "Disease"
# df.dens$class9.75[which(df.dens$fin.pop9.75 < pop.cut & df.dens$fin.prev9.75 > prev.cut)] <- "Crash+Disease"
# df.dens$class9.75[is.na(df.dens$fin.pop9.75)] <- "NA/Crash"
# df.dens$class9.75 <- factor(df.dens$class9.75)
# 
# df.dens$class14.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$class14.75[which(df.dens$fin.pop14.75 >= pop.cut & df.dens$fin.prev14.75 <= prev.cut)] <- "Survive"
# df.dens$class14.75[which(df.dens$fin.pop14.75 < pop.cut & df.dens$fin.prev14.75 <= prev.cut)] <- "Crash"
# df.dens$class14.75[which(df.dens$fin.pop14.75 >= pop.cut & df.dens$fin.prev14.75 > prev.cut)] <- "Disease"
# df.dens$class14.75[which(df.dens$fin.pop14.75 < pop.cut & df.dens$fin.prev14.75 > prev.cut)] <- "Crash+Disease"
# df.dens$class14.75[is.na(df.dens$fin.pop14.75)] <- "NA/Crash"
# df.dens$class14.75 <- factor(df.dens$class14.75)
# 
# df.dens$class19.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$class19.75[which(df.dens$fin.pop19.75 >= pop.cut & df.dens$fin.prev19.75 <= prev.cut)] <- "Survive"
# df.dens$class19.75[which(df.dens$fin.pop19.75 < pop.cut & df.dens$fin.prev19.75 <= prev.cut)] <- "Crash"
# df.dens$class19.75[which(df.dens$fin.pop19.75 >= pop.cut & df.dens$fin.prev19.75 > prev.cut)] <- "Disease"
# df.dens$class19.75[which(df.dens$fin.pop19.75 < pop.cut & df.dens$fin.prev19.75 > prev.cut)] <- "Crash+Disease"
# df.dens$class19.75[is.na(df.dens$fin.pop19.75)] <- "NA/Crash"
# df.dens$class19.75 <- factor(df.dens$class19.75)
# 
# df.dens$class24.75 <- rep(NA, length(df.dens$hunt.f))
# df.dens$class24.75[which(df.dens$fin.pop24.75 >= pop.cut & df.dens$fin.prev24.75 <= prev.cut)] <- "Survive"
# df.dens$class24.75[which(df.dens$fin.pop24.75 < pop.cut & df.dens$fin.prev24.75 <= prev.cut)] <- "Crash"
# df.dens$class24.75[which(df.dens$fin.pop24.75 >= pop.cut & df.dens$fin.prev24.75 > prev.cut)] <- "Disease"
# df.dens$class24.75[which(df.dens$fin.pop24.75 < pop.cut & df.dens$fin.prev24.75 > prev.cut)] <- "Crash+Disease"
# df.dens$class24.75[is.na(df.dens$fin.pop24.75)] <- "NA/Crash"
# df.dens$class24.75 <- factor(df.dens$class24.75)
# 
# write.csv(df.dens, "densdf.csv")
df.dens <- read.csv("densdf.csv")
#####################
#Density Plotting
#####################

df.dens$class2.75  <- factor(df.dens$class2.75, levels = order)
df.dens$class4.75  <- factor(df.dens$class4.75, levels = order)
df.dens$class9.75  <- factor(df.dens$class9.75, levels = order)
df.dens$class14.75 <- factor(df.dens$class14.75, levels = order)
df.dens$class19.75 <- factor(df.dens$class19.75, levels = order)
df.dens$class24.75 <- factor(df.dens$class24.75, levels = order)

betaorderf <- c("F Beta = 5.0e-06", "F Beta = 1.0e-05", "F Beta = 1.5e-05")
df.dens$beta.f.1  <- factor(df.dens$beta.f.1, levels = betaorderf)
betaorderm <- c("M Beta = 5.0e-06", "M Beta = 1.0e-05", "M Beta = 1.5e-05")
df.dens$beta.m.1  <- factor(df.dens$beta.m.1, levels = betaorderm)

names(myColors) <- levels(df.dens$class2.75)
colScaled <- scale_fill_manual(name ="class2.75", values = myColors)

dens.dep2.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=class2.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",2.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScaled

dens.pop2.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.pop2.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",2.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev2.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.prev2.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",2.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

dens.dep4.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=class4.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",4.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScaled

dens.pop4.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.pop4.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",4.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev4.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.prev4.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",4.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

dens.dep9.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=class9.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",9.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScaled

dens.pop9.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.pop9.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",9.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev9.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.prev9.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",9.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

dens.dep14.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=class14.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",14.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScaled

dens.pop14.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.pop14.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",14.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev14.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.prev14.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",14.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

dens.dep19.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=class19.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",19.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScaled

dens.pop19.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.pop19.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",19.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev19.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.prev19.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",19.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

dens.dep24.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=class24.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",24.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScaled

dens.pop24.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.pop24.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",24.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev24.75 <- 
  ggplot(df.dens, aes(x=hunt.f, y=hunt.m, fill=fin.prev24.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",24.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.f.1), cols = vars(beta.m.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))
########################
# Across all simultions 
########################

grid.arrange(freq.dep4.75, dens.dep4.75,
             freq.dep9.75, dens.dep9.75,
             freq.dep14.75, dens.dep14.75,
             freq.dep19.75, dens.dep19.75,
             ncol=2)

grid.arrange(freq.pop4.75, dens.pop4.75,
             freq.pop9.75, dens.pop9.75,
             freq.pop14.75, dens.pop14.75,
             freq.pop19.75, dens.pop19.75,
             ncol=2)

grid.arrange(freq.prev4.75, dens.prev4.75,
             freq.prev9.75, dens.prev9.75,
             freq.prev14.75, dens.prev14.75,
             freq.prev19.75, dens.prev19.75,
             ncol=2)


#################
# Breaking it down over time
#################
require(reshape2)
colnames(df.freq)
longfredf <- melt(df.freq, id.vars = c("X.1","X","hunt.f","hunt.m","beta.f.1","beta.m.1","theta.1"),
                  measure.vars = c("fin.pop2.75","fin.prev2.75","fin.pop4.75","fin.prev4.75","fin.pop9.75",
                                   "fin.prev9.75","fin.pop14.75","fin.prev14.75","fin.pop19.75","fin.prev19.75",
                                   "fin.pop24.75","fin.prev24.75","class2.75","class4.75","class9.75",
                                   "class14.75","class19.75","class24.75"), 
                  variable.name = "Measure", value.name = "Value")

longfredf$Year <- rep(NA, length(longfredf$X.1))
longfredf$Type <- rep(NA, length(longfredf$X.1))
longfredf$Year <- ifelse((longfredf$Measure=="fin.pop2.75"|
                           longfredf$Measure=="fin.prev2.75"|
                           longfredf$Measure=="class2.75"), 2.75, longfredf$Year)
longfredf$Year <- ifelse((longfredf$Measure=="fin.pop4.75"|
                            longfredf$Measure=="fin.prev4.75"|
                            longfredf$Measure=="class4.75"), 4.75, longfredf$Year)
longfredf$Year <- ifelse((longfredf$Measure=="fin.pop9.75"|
                            longfredf$Measure=="fin.prev9.75"|
                            longfredf$Measure=="class9.75"), 9.75, longfredf$Year)
longfredf$Year <- ifelse((longfredf$Measure=="fin.pop14.75"|
                            longfredf$Measure=="fin.prev14.75"|
                            longfredf$Measure=="class14.75"), 14.75, longfredf$Year)
longfredf$Year <- ifelse((longfredf$Measure=="fin.pop19.75"|
                            longfredf$Measure=="fin.prev19.75"|
                            longfredf$Measure=="class19.75"), 19.75, longfredf$Year)
longfredf$Year <- ifelse((longfredf$Measure=="fin.pop24.75"|
                            longfredf$Measure=="fin.prev24.75"|
                            longfredf$Measure=="class24.75"), 24.75, longfredf$Year)
longfredf$Type <- ifelse((longfredf$Measure=="fin.pop2.75"|
                            longfredf$Measure=="fin.pop4.75"|
                            longfredf$Measure=="fin.pop9.75"|
                            longfredf$Measure=="fin.pop14.75"|
                            longfredf$Measure=="fin.pop19.75"|
                            longfredf$Measure=="fin.pop24.75"), "Population", longfredf$Type)
longfredf$Type <- ifelse((longfredf$Measure=="fin.prev2.75"|
                            longfredf$Measure=="fin.prev4.75"|
                            longfredf$Measure=="fin.prev9.75"|
                            longfredf$Measure=="fin.prev14.75"|
                            longfredf$Measure=="fin.prev19.75"|
                            longfredf$Measure=="fin.prev24.75"), "Prevalence", longfredf$Type)
longfredf$Type <- ifelse((longfredf$Measure=="class2.75"|
                            longfredf$Measure=="class4.75"|
                            longfredf$Measure=="class9.75"|
                            longfredf$Measure=="class14.75"|
                            longfredf$Measure=="class19.75"|
                            longfredf$Measure=="class24.75"), "Class", longfredf$Type)
longfredf$Population <- ifelse((longfredf$Type=="Population"), as.numeric(as.character(longfredf$Value)), NA)
longfredf$Prevalence <- ifelse((longfredf$Type=="Prevalence"), as.numeric(as.character(longfredf$Value)), NA)
longfredf$Class <- ifelse((longfredf$Type=="Class"), longfredf$Value, NA)

unique(longfredf$Class)
order <- c("Survive", "Crash", "Disease", "Crash+Disease", "NA/Crash")
longfredf$Class  <- factor(longfredf$Class, levels = order)

names(myColors) <- levels(longfredf$Class)
colScale <- scale_fill_manual(name ="Class", values = myColors)

# mfbeta.8.pop <- 
#   ggplot(longfredf[which(longfredf$beta.f.1=="F Beta = 0.08" &
#                            longfredf$beta.m.1=="M Beta = 0.08"&
#                            longfredf$Type=="Population"),], 
#          aes(x=hunt.f, y=hunt.m, fill=sqrt(Population))) + 
#   geom_tile() +
#   theme_bw() +
#   scale_fill_distiller(palette="RdYlGn", direction=1) +
#   #labs(title = paste("Heatplot of Prev at",24.75,"Years"), subtitle = "Theta = 1") +
#   facet_wrap(~Year, ncol=3) +
#   theme(axis.text.x = element_text(angle = 270, hjust = -1))
# 
# mfbeta.8.prev <- 
#   ggplot(longfredf[which(longfredf$beta.f.1=="F Beta = 0.08" &
#                            longfredf$beta.m.1=="M Beta = 0.08"&
#                            longfredf$Type=="Prevalence"),], 
#          aes(x=hunt.f, y=hunt.m, fill=Prevalence)) + 
#   geom_tile() +
#   theme_bw() +
#   scale_fill_distiller(palette="RdYlGn", direction=-1) +
#   #labs(title = paste("Heatplot of Prev at",24.75,"Years"), subtitle = "Theta = 1") +
#   facet_wrap(~Year, ncol=3) +
#   theme(axis.text.x = element_text(angle = 270, hjust = -1))

mfbeta.8.class <- 
  ggplot(longfredf[which(longfredf$beta.f.1=="F Beta = 0.08" &
                           longfredf$beta.m.1=="M Beta = 0.08"&
                           longfredf$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Beta F (0.08)=M (0.08) w/ Theta = 1")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

mfbeta.4.class <- 
  ggplot(longfredf[which(longfredf$beta.f.1=="F Beta = 0.04" &
                           longfredf$beta.m.1=="M Beta = 0.04"&
                           longfredf$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Beta F (0.04)=M (0.04) w/ Theta = 1")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

mfbeta.12.class <- 
  ggplot(longfredf[which(longfredf$beta.f.1=="F Beta = 0.12" &
                           longfredf$beta.m.1=="M Beta = 0.12"&
                           longfredf$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Beta F (0.04)=M (0.04) w/ Theta = 1")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

m.12f.4.class <- 
  ggplot(longfredf[which(longfredf$beta.f.1=="F Beta = 0.04" &
                           longfredf$beta.m.1=="M Beta = 0.12"&
                           longfredf$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Beta M (0.12)>F (0.04) w/ Theta = 1")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

m.4f.12.class <- 
  ggplot(longfredf[which(longfredf$beta.f.1=="F Beta = 0.12" &
                           longfredf$beta.m.1=="M Beta = 0.04"&
                           longfredf$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Beta F (0.12)>M (0.04) w/ Theta = 1")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

mfbeta.8.class
m.12f.4.class
m.4f.12.class
grid.arrange(m.12f.4.class,
             m.4f.12.class, ncol=2)
########################
#For Density Dep
########################
colnames(df.dens)
longdendf <- melt(df.dens, id.vars = c("X","hunt.f","hunt.m","beta.f.1","beta.m.1","theta.1"),
                  measure.vars = c("fin.pop2.75","fin.prev2.75","fin.pop4.75","fin.prev4.75","fin.pop9.75",
                                   "fin.prev9.75","fin.pop14.75","fin.prev14.75","fin.pop19.75","fin.prev19.75",
                                   "fin.pop24.75","fin.prev24.75","class2.75","class4.75","class9.75",
                                   "class14.75","class19.75","class24.75"), 
                  variable.name = "Measure", value.name = "Value")

longdendf$Year <- rep(NA, length(longdendf$X))
longdendf$Type <- rep(NA, length(longdendf$X))
longdendf$Year <- ifelse((longdendf$Measure=="fin.pop2.75"|
                            longdendf$Measure=="fin.prev2.75"|
                            longdendf$Measure=="class2.75"), 2.75, longdendf$Year)
longdendf$Year <- ifelse((longdendf$Measure=="fin.pop4.75"|
                            longdendf$Measure=="fin.prev4.75"|
                            longdendf$Measure=="class4.75"), 4.75, longdendf$Year)
longdendf$Year <- ifelse((longdendf$Measure=="fin.pop9.75"|
                            longdendf$Measure=="fin.prev9.75"|
                            longdendf$Measure=="class9.75"), 9.75, longdendf$Year)
longdendf$Year <- ifelse((longdendf$Measure=="fin.pop14.75"|
                            longdendf$Measure=="fin.prev14.75"|
                            longdendf$Measure=="class14.75"), 14.75, longdendf$Year)
longdendf$Year <- ifelse((longdendf$Measure=="fin.pop19.75"|
                            longdendf$Measure=="fin.prev19.75"|
                            longdendf$Measure=="class19.75"), 19.75, longdendf$Year)
longdendf$Year <- ifelse((longdendf$Measure=="fin.pop24.75"|
                            longdendf$Measure=="fin.prev24.75"|
                            longdendf$Measure=="class24.75"), 24.75, longdendf$Year)
longdendf$Type <- ifelse((longdendf$Measure=="fin.pop2.75"|
                            longdendf$Measure=="fin.pop4.75"|
                            longdendf$Measure=="fin.pop9.75"|
                            longdendf$Measure=="fin.pop14.75"|
                            longdendf$Measure=="fin.pop19.75"|
                            longdendf$Measure=="fin.pop24.75"), "Population", longdendf$Type)
longdendf$Type <- ifelse((longdendf$Measure=="fin.prev2.75"|
                            longdendf$Measure=="fin.prev4.75"|
                            longdendf$Measure=="fin.prev9.75"|
                            longdendf$Measure=="fin.prev14.75"|
                            longdendf$Measure=="fin.prev19.75"|
                            longdendf$Measure=="fin.prev24.75"), "Prevalence", longdendf$Type)
longdendf$Type <- ifelse((longdendf$Measure=="class2.75"|
                            longdendf$Measure=="class4.75"|
                            longdendf$Measure=="class9.75"|
                            longdendf$Measure=="class14.75"|
                            longdendf$Measure=="class19.75"|
                            longdendf$Measure=="class24.75"), "Class", longdendf$Type)
longdendf$Population <- ifelse((longdendf$Type=="Population"), as.numeric(as.character(longdendf$Value)), NA)
longdendf$Prevalence <- ifelse((longdendf$Type=="Prevalence"), as.numeric(as.character(longdendf$Value)), NA)
longdendf$Class <- ifelse((longdendf$Type=="Class"), longdendf$Value, NA)

unique(longdendf$Class)
order <- c("Survive", "Crash", "Disease", "Crash+Disease", "NA/Crash")
longdendf$Class  <- factor(longdendf$Class, levels = order)

names(myColors) <- levels(longdendf$Class)
colScale <- scale_fill_manual(name ="Class", values = myColors)

# mfbeta.8.pop <- 
#   ggplot(longdendf[which(longdendf$beta.f.1=="F Beta = 0.08" &
#                            longdendf$beta.m.1=="M Beta = 0.08"&
#                            longdendf$Type=="Population"),], 
#          aes(x=hunt.f, y=hunt.m, fill=sqrt(Population))) + 
#   geom_tile() +
#   theme_bw() +
#   scale_fill_distiller(palette="RdYlGn", direction=1) +
#   #labs(title = paste("Heatplot of Prev at",24.75,"Years"), subtitle = "Theta = 1") +
#   facet_wrap(~Year, ncol=3) +
#   theme(axis.text.x = element_text(angle = 270, hjust = -1))
# 
# mfbeta.8.prev <- 
#   ggplot(longdendf[which(longdendf$beta.f.1=="F Beta = 0.08" &
#                            longdendf$beta.m.1=="M Beta = 0.08"&
#                            longdendf$Type=="Prevalence"),], 
#          aes(x=hunt.f, y=hunt.m, fill=Prevalence)) + 
#   geom_tile() +
#   theme_bw() +
#   scale_fill_distiller(palette="RdYlGn", direction=-1) +
#   #labs(title = paste("Heatplot of Prev at",24.75,"Years"), subtitle = "Theta = 1") +
#   facet_wrap(~Year, ncol=3) +
#   theme(axis.text.x = element_text(angle = 270, hjust = -1))

mfbeta.1.class <- 
  ggplot(longdendf[which(longdendf$beta.f.1=="F Beta = 1.0e-05" &
                           longdendf$beta.m.1=="M Beta = 1.0e-05"&
                           longdendf$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Beta F (1E-5)=M (1E-5) w/ Theta = 1")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

m.1.5f.5.class <- 
  ggplot(longdendf[which(longdendf$beta.f.1=="F Beta = 5.0e-06" &
                           longdendf$beta.m.1=="M Beta = 1.5e-05"&
                           longdendf$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Beta M (1.5E-5)>F (0.5E-5) w/ Theta = 1")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

m.5f.1.5.class <- 
  ggplot(longdendf[which(longdendf$beta.f.1=="F Beta = 1.5e-05" &
                           longdendf$beta.m.1=="M Beta = 5.0e-06"&
                           longdendf$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Beta F (1.5E-5)>M (0.5E-5) w/ Theta = 1")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

mfbeta.1.class
m.1.5f.5.class
m.5f.1.5.class
##########################
#Is there a difference when we consider f-m transmission and m-f transmission
##########################


# fawn.an.sur = 0.6
# juv.an.sur = 0.8
# ad.an.f.sur = 0.95
# ad.an.m.sur = 0.9
# fawn.repro = 0
# juv.repro = 0.6
# ad.repro = 1
# hunt.mort.fawn = 0.01
# hunt.mort.juv.f = 0.1
# hunt.mort.juv.m = 0.1
# ini.fawn.prev = 0.02
# ini.juv.prev = 0.03
# ini.ad.f.prev = 0.04
# ini.ad.m.prev = 0.04
# n.age.cats = 12
# p = 0.43
# env.foi = 0.001
# n0 = 10000
# n.years = 25
# rel.risk = 1.0
# 
# cut2.75 <- 2.75
# cut4.75 <- 4.75
# cut9.75 <- 9.75
# cut14.75 <- 14.75
# cut19.75 <- 19.75
# cut24.75 <- 24.75
# 
# hunt.mort.ad.f    <- seq(0, .99, length.out = 99) #vector of female harvest to run through
# hunt.mort.ad.m    <- seq(0, .99, length.out = 99) #vector of male harvest to run through
# beta.fm            <- c(0.5, 1, 2) #vector of female transmission to run through
# beta.mf            <- c(0.5, 1, 2) #vector of male transmission to run through
# 
# hunt.f <- c()
# hunt.m <- c()
# beta.fm.1 <- c()
# beta.mf.1 <- c()
# 
# for (l in 1:length(beta.mf)){
#   for (k in 1:length(beta.fm)){
#     for (j in 1:length(hunt.mort.ad.m)){
#       for (i in 1:length(hunt.mort.ad.f)){
#         hunt.f <- c(hunt.f, hunt.mort.ad.f[i])
#         hunt.m <- c(hunt.m, hunt.mort.ad.m[j])
#         beta.fm.1 <- c(beta.fm.1, beta.fm[k])
#         beta.mf.1 <- c(beta.mf.1, beta.mf[l])
#       }
#     }
#   }
# }
# 
# df.freq.wiw <- data.frame(hunt.f,hunt.m, beta.fm.1, beta.mf.1, theta.1 = rep(1,length(hunt.f)))
# 
# df.dens.wiw <- data.frame(hunt.f,hunt.m, beta.fm.1, beta.mf.1, theta.1 =rep(0,length(hunt.f)))

#####################
#Freq Dep WIW
#####################

#for sim - items to hold intermediates and output
# df.freq.wiw$fin.pop2.75   <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$fin.prev2.75  <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$fin.pop4.75   <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$fin.prev4.75  <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$fin.pop9.75   <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$fin.prev9.75  <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$fin.pop14.75  <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$fin.prev14.75 <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$fin.pop19.75  <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$fin.prev19.75 <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$fin.pop24.75  <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$fin.prev24.75 <- rep(NA, length(df.freq.wiw$hunt.f))
# 
# out <- list()
# pop.sum <- list()
# prev.sum <- list()
# df.freq.wiw <- list(df.freq.wiw)
# 
# for (i in 1:length(df.freq.wiw[[1]]$hunt.f)){
#   out <- cwd_det_model_wiw(params = list(fawn.an.sur = fawn.an.sur,
#                                           juv.an.sur = juv.an.sur,
#                                           ad.an.f.sur = ad.an.f.sur,
#                                           ad.an.m.sur = ad.an.m.sur,
#                                           fawn.repro = fawn.repro,
#                                           juv.repro = juv.repro,
#                                           ad.repro = ad.repro,
#                                           hunt.mort.fawn = hunt.mort.fawn,
#                                           hunt.mort.juv.f = hunt.mort.juv.f,
#                                           hunt.mort.juv.m = hunt.mort.juv.m,
#                                           hunt.mort.ad.f = df.freq.wiw[[1]]$hunt.f[i],
#                                           hunt.mort.ad.m = df.freq.wiw[[1]]$hunt.m[i],
#                                           ini.fawn.prev = ini.fawn.prev,
#                                           ini.juv.prev = ini.juv.prev,
#                                           ini.ad.f.prev = ini.ad.f.prev,
#                                           ini.ad.m.prev = ini.ad.m.prev,
#                                           n.age.cats = n.age.cats,
#                                           p = p,
#                                           env.foi = env.foi,
#                                           beta.ff = 0.08,
#                                           gamma.mm = 1,
#                                           gamma.mf = df.freq.wiw[[1]]$beta.mf.1[i],
#                                           gamma.fm = df.freq.wiw[[1]]$beta.fm.1[i],
#                                           theta = 1,
#                                           n0 = n0,
#                                           n.years = n.years,
#                                           rel.risk = rel.risk))
#   pop.sum <- out$counts %>%
#     filter(month %% 12 == 10) %>%
#     group_by(year) %>%
#     summarize(n = sum(population)) #Total pop at time
# 
#   prev.sum   <- out$counts %>%
#     filter(month %% 12 == 10) %>%
#     group_by(year, disease) %>%
#     summarize(n = sum(population)) %>%
#     pivot_wider(names_from = disease, values_from = n) %>%
#     mutate(prev = yes/(no + yes)) #Prev at time
# 
#   #If you know a way to index these, it would speed the sim
#   df.freq.wiw[[1]]$fin.pop2.75[i]    <- pop.sum$n[pop.sum$year==cut2.75]
#   df.freq.wiw[[1]]$fin.pop4.75[i]    <- pop.sum$n[pop.sum$year==cut4.75]
#   df.freq.wiw[[1]]$fin.pop9.75[i]    <- pop.sum$n[pop.sum$year==cut9.75]
#   df.freq.wiw[[1]]$fin.pop14.75[i]   <- pop.sum$n[pop.sum$year==cut14.75]
#   df.freq.wiw[[1]]$fin.pop19.75[i]   <- pop.sum$n[pop.sum$year==cut19.75]
#   df.freq.wiw[[1]]$fin.pop24.75[i]   <- pop.sum$n[pop.sum$year==cut24.75]
#   df.freq.wiw[[1]]$fin.prev2.75[i]   <- prev.sum$prev[prev.sum$year==cut2.75]
#   df.freq.wiw[[1]]$fin.prev4.75[i]   <- prev.sum$prev[prev.sum$year==cut4.75]
#   df.freq.wiw[[1]]$fin.prev9.75[i]   <- prev.sum$prev[prev.sum$year==cut9.75]
#   df.freq.wiw[[1]]$fin.prev14.75[i]  <- prev.sum$prev[prev.sum$year==cut14.75]
#   df.freq.wiw[[1]]$fin.prev19.75[i]  <- prev.sum$prev[prev.sum$year==cut19.75]
#   df.freq.wiw[[1]]$fin.prev24.75[i]  <- prev.sum$prev[prev.sum$year==cut24.75]
# }
# 
# df.freq.wiw <- data.frame(df.freq.wiw[[1]])
# df.freq.wiw$beta.fm.1[which(df.freq.wiw$beta.fm.1 == 0.5)] <- "FM Gamma = 0.5"
# df.freq.wiw$beta.fm.1[which(df.freq.wiw$beta.fm.1 == 1)] <- "FM Gamma = 1"
# df.freq.wiw$beta.fm.1[which(df.freq.wiw$beta.fm.1 == 2)] <- "FM Gamma = 2"
# df.freq.wiw$beta.mf.1[which(df.freq.wiw$beta.mf.1 == 0.5)] <- "MF Gamma = 0.5"
# df.freq.wiw$beta.mf.1[which(df.freq.wiw$beta.mf.1 == 1)] <- "MF Gamma = 1"
# df.freq.wiw$beta.mf.1[which(df.freq.wiw$beta.mf.1 == 2)] <- "MF Gamma = 2"
# 
# pop.cut <- 3000 #population critical value
# prev.cut <- 0.3 #prevalence critical value
# 
# df.freq.wiw$class2.75 <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$class2.75[which(df.freq.wiw$fin.pop2.75 >= pop.cut & df.freq.wiw$fin.prev2.75 <= prev.cut)] <- "Survive"
# df.freq.wiw$class2.75[which(df.freq.wiw$fin.pop2.75 < pop.cut & df.freq.wiw$fin.prev2.75 <= prev.cut)] <- "Crash"
# df.freq.wiw$class2.75[which(df.freq.wiw$fin.pop2.75 >= pop.cut & df.freq.wiw$fin.prev2.75 > prev.cut)] <- "Disease"
# df.freq.wiw$class2.75[which(df.freq.wiw$fin.pop2.75 < pop.cut & df.freq.wiw$fin.prev2.75 > prev.cut)] <- "Crash+Disease"
# df.freq.wiw$class2.75[is.na(df.freq.wiw$fin.pop2.75)] <- "NA/Crash"
# df.freq.wiw$class2.75 <- factor(df.freq.wiw$class2.75)
# 
# df.freq.wiw$class4.75 <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$class4.75[which(df.freq.wiw$fin.pop4.75 >= pop.cut & df.freq.wiw$fin.prev4.75 <= prev.cut)] <- "Survive"
# df.freq.wiw$class4.75[which(df.freq.wiw$fin.pop4.75 < pop.cut & df.freq.wiw$fin.prev4.75 <= prev.cut)] <- "Crash"
# df.freq.wiw$class4.75[which(df.freq.wiw$fin.pop4.75 >= pop.cut & df.freq.wiw$fin.prev4.75 > prev.cut)] <- "Disease"
# df.freq.wiw$class4.75[which(df.freq.wiw$fin.pop4.75 < pop.cut & df.freq.wiw$fin.prev4.75 > prev.cut)] <- "Crash+Disease"
# df.freq.wiw$class4.75[is.na(df.freq.wiw$fin.pop4.75)] <- "NA/Crash"
# df.freq.wiw$class4.75 <- factor(df.freq.wiw$class4.75)
# 
# df.freq.wiw$class9.75 <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$class9.75[which(df.freq.wiw$fin.pop9.75 >= pop.cut & df.freq.wiw$fin.prev9.75 <= prev.cut)] <- "Survive"
# df.freq.wiw$class9.75[which(df.freq.wiw$fin.pop9.75 < pop.cut & df.freq.wiw$fin.prev9.75 <= prev.cut)] <- "Crash"
# df.freq.wiw$class9.75[which(df.freq.wiw$fin.pop9.75 >= pop.cut & df.freq.wiw$fin.prev9.75 > prev.cut)] <- "Disease"
# df.freq.wiw$class9.75[which(df.freq.wiw$fin.pop9.75 < pop.cut & df.freq.wiw$fin.prev9.75 > prev.cut)] <- "Crash+Disease"
# df.freq.wiw$class9.75[is.na(df.freq.wiw$fin.pop9.75)] <- "NA/Crash"
# df.freq.wiw$class9.75 <- factor(df.freq.wiw$class9.75)
# 
# df.freq.wiw$class14.75 <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$class14.75[which(df.freq.wiw$fin.pop14.75 >= pop.cut & df.freq.wiw$fin.prev14.75 <= prev.cut)] <- "Survive"
# df.freq.wiw$class14.75[which(df.freq.wiw$fin.pop14.75 < pop.cut & df.freq.wiw$fin.prev14.75 <= prev.cut)] <- "Crash"
# df.freq.wiw$class14.75[which(df.freq.wiw$fin.pop14.75 >= pop.cut & df.freq.wiw$fin.prev14.75 > prev.cut)] <- "Disease"
# df.freq.wiw$class14.75[which(df.freq.wiw$fin.pop14.75 < pop.cut & df.freq.wiw$fin.prev14.75 > prev.cut)] <- "Crash+Disease"
# df.freq.wiw$class14.75[is.na(df.freq.wiw$fin.pop14.75)] <- "NA/Crash"
# df.freq.wiw$class14.75 <- factor(df.freq.wiw$class14.75)
# 
# df.freq.wiw$class19.75 <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$class19.75[which(df.freq.wiw$fin.pop19.75 >= pop.cut & df.freq.wiw$fin.prev19.75 <= prev.cut)] <- "Survive"
# df.freq.wiw$class19.75[which(df.freq.wiw$fin.pop19.75 < pop.cut & df.freq.wiw$fin.prev19.75 <= prev.cut)] <- "Crash"
# df.freq.wiw$class19.75[which(df.freq.wiw$fin.pop19.75 >= pop.cut & df.freq.wiw$fin.prev19.75 > prev.cut)] <- "Disease"
# df.freq.wiw$class19.75[which(df.freq.wiw$fin.pop19.75 < pop.cut & df.freq.wiw$fin.prev19.75 > prev.cut)] <- "Crash+Disease"
# df.freq.wiw$class19.75[is.na(df.freq.wiw$fin.pop19.75)] <- "NA/Crash"
# df.freq.wiw$class19.75 <- factor(df.freq.wiw$class19.75)
# 
# df.freq.wiw$class24.75 <- rep(NA, length(df.freq.wiw$hunt.f))
# df.freq.wiw$class24.75[which(df.freq.wiw$fin.pop24.75 >= pop.cut & df.freq.wiw$fin.prev24.75 <= prev.cut)] <- "Survive"
# df.freq.wiw$class24.75[which(df.freq.wiw$fin.pop24.75 < pop.cut & df.freq.wiw$fin.prev24.75 <= prev.cut)] <- "Crash"
# df.freq.wiw$class24.75[which(df.freq.wiw$fin.pop24.75 >= pop.cut & df.freq.wiw$fin.prev24.75 > prev.cut)] <- "Disease"
# df.freq.wiw$class24.75[which(df.freq.wiw$fin.pop24.75 < pop.cut & df.freq.wiw$fin.prev24.75 > prev.cut)] <- "Crash+Disease"
# df.freq.wiw$class24.75[is.na(df.freq.wiw$fin.pop24.75)] <- "NA/Crash"
# df.freq.wiw$class24.75 <- factor(df.freq.wiw$class24.75)
# 
#write.csv(df.freq.wiw, "df.freq.wiw.csv")
df.freq.wiw <- read.csv("df.freq.wiw.csv")

df.freq.wiw$class2.75  <- factor(df.freq.wiw$class2.75, levels = order)
df.freq.wiw$class4.75  <- factor(df.freq.wiw$class4.75, levels = order)
df.freq.wiw$class9.75  <- factor(df.freq.wiw$class9.75, levels = order)
df.freq.wiw$class14.75 <- factor(df.freq.wiw$class14.75, levels = order)
df.freq.wiw$class19.75 <- factor(df.freq.wiw$class19.75, levels = order)
df.freq.wiw$class24.75 <- factor(df.freq.wiw$class24.75, levels = order)

myColors <- c("#3288BD", "#ABDDA4", "#FFFFBF", "#D53E4F", "#F46D43")
names(myColors) <- levels(df.freq.wiw$class9.75)
colScalef <- scale_fill_manual(name = "class2.75",values = myColors)

freq.dep.wiw2.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=class2.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",2.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop2.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.pop2.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",2.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev2.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev2.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",2.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

freq.dep.wiw4.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=class4.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",4.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop4.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.pop4.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",4.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev4.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev4.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",4.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

freq.dep.wiw9.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=class9.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",9.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop9.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=log(fin.pop9.75))) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",9.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev9.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev9.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",9.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

freq.dep.wiw14.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=class14.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",14.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop14.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=log(fin.pop14.75))) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",14.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev14.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev14.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",14.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

freq.dep.wiw19.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=class19.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",19.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop19.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.pop19.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",19.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev19.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev19.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",19.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

freq.dep.wiw24.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=class24.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",24.75,"Years"), subtitle = paste("Theta = 1; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

freq.pop24.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=log(fin.pop24.75))) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",24.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

freq.prev24.75 <- 
  ggplot(df.freq.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev24.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",24.75,"Years"), subtitle = "Theta = 1") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

#############################
#WIW over time (frequency)
#############################
require(reshape2)
colnames(df.freq.wiw)
longfredf.wiw <- melt(df.freq.wiw, id.vars = c("X","hunt.f","hunt.m","beta.fm.1","beta.mf.1","theta.1"),
                  measure.vars = c("fin.pop2.75","fin.prev2.75","fin.pop4.75","fin.prev4.75","fin.pop9.75",
                                   "fin.prev9.75","fin.pop14.75","fin.prev14.75","fin.pop19.75","fin.prev19.75",
                                   "fin.pop24.75","fin.prev24.75","class2.75","class4.75","class9.75",
                                   "class14.75","class19.75","class24.75"), 
                  variable.name = "Measure", value.name = "Value")
length(df.freq.wiw$X)
length(longfredf.wiw$X)
longfredf.wiw$Year <- rep(NA, length(longfredf.wiw$X))
longfredf.wiw$Type <- rep(NA, length(longfredf.wiw$X))
longfredf.wiw$Year <- ifelse((longfredf.wiw$Measure=="fin.pop2.75"|
                            longfredf.wiw$Measure=="fin.prev2.75"|
                            longfredf.wiw$Measure=="class2.75"), 2.75, longfredf.wiw$Year)
longfredf.wiw$Year <- ifelse((longfredf.wiw$Measure=="fin.pop4.75"|
                            longfredf.wiw$Measure=="fin.prev4.75"|
                            longfredf.wiw$Measure=="class4.75"), 4.75, longfredf.wiw$Year)
longfredf.wiw$Year <- ifelse((longfredf.wiw$Measure=="fin.pop9.75"|
                            longfredf.wiw$Measure=="fin.prev9.75"|
                            longfredf.wiw$Measure=="class9.75"), 9.75, longfredf.wiw$Year)
longfredf.wiw$Year <- ifelse((longfredf.wiw$Measure=="fin.pop14.75"|
                            longfredf.wiw$Measure=="fin.prev14.75"|
                            longfredf.wiw$Measure=="class14.75"), 14.75, longfredf.wiw$Year)
longfredf.wiw$Year <- ifelse((longfredf.wiw$Measure=="fin.pop19.75"|
                            longfredf.wiw$Measure=="fin.prev19.75"|
                            longfredf.wiw$Measure=="class19.75"), 19.75, longfredf.wiw$Year)
longfredf.wiw$Year <- ifelse((longfredf.wiw$Measure=="fin.pop24.75"|
                            longfredf.wiw$Measure=="fin.prev24.75"|
                            longfredf.wiw$Measure=="class24.75"), 24.75, longfredf.wiw$Year)
longfredf.wiw$Type <- ifelse((longfredf.wiw$Measure=="fin.pop2.75"|
                            longfredf.wiw$Measure=="fin.pop4.75"|
                            longfredf.wiw$Measure=="fin.pop9.75"|
                            longfredf.wiw$Measure=="fin.pop14.75"|
                            longfredf.wiw$Measure=="fin.pop19.75"|
                            longfredf.wiw$Measure=="fin.pop24.75"), "Population", longfredf.wiw$Type)
longfredf.wiw$Type <- ifelse((longfredf.wiw$Measure=="fin.prev2.75"|
                            longfredf.wiw$Measure=="fin.prev4.75"|
                            longfredf.wiw$Measure=="fin.prev9.75"|
                            longfredf.wiw$Measure=="fin.prev14.75"|
                            longfredf.wiw$Measure=="fin.prev19.75"|
                            longfredf.wiw$Measure=="fin.prev24.75"), "Prevalence", longfredf.wiw$Type)
longfredf.wiw$Type <- ifelse((longfredf.wiw$Measure=="class2.75"|
                            longfredf.wiw$Measure=="class4.75"|
                            longfredf.wiw$Measure=="class9.75"|
                            longfredf.wiw$Measure=="class14.75"|
                            longfredf.wiw$Measure=="class19.75"|
                            longfredf.wiw$Measure=="class24.75"), "Class", longfredf.wiw$Type)
longfredf.wiw$Population <- ifelse((longfredf.wiw$Type=="Population"), as.numeric(as.character(longfredf.wiw$Value)), NA)
longfredf.wiw$Prevalence <- ifelse((longfredf.wiw$Type=="Prevalence"), as.numeric(as.character(longfredf.wiw$Value)), NA)
longfredf.wiw$Class <- ifelse((longfredf.wiw$Type=="Class"), longfredf.wiw$Value, NA)

unique(longfredf.wiw$Class)
order <- c("Survive", "Crash", "Disease", "Crash+Disease", "NA/Crash")
longfredf.wiw$Class  <- factor(longfredf.wiw$Class, levels = order)

names(myColors) <- levels(longfredf.wiw$Class)
colScale <- scale_fill_manual(name ="Class", values = myColors)

FMgtMF.class <- 
  ggplot(longfredf.wiw[which(longfredf.wiw$beta.fm.1=="FM Gamma = 2" &
                           longfredf.wiw$beta.mf.1=="MF Gamma = 0.5"&
                           longfredf.wiw$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Sex Dep. Trans. FM(2)>MF(.5) w/ Theta = 1")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

MFgtFM.class <- 
  ggplot(longfredf.wiw[which(longfredf.wiw$beta.fm.1=="FM Gamma = 0.5" &
                               longfredf.wiw$beta.mf.1=="MF Gamma = 2"&
                               longfredf.wiw$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Sex Dep. Trans. FM(.5)<MF(2) w/ Theta = 1")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

FMgtMF.class
MFgtFM.class
#####################
#Density Dep WIW
#####################

#for sim - items to hold intermediates and output
# df.dens.wiw$fin.pop2.75   <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$fin.prev2.75  <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$fin.pop4.75   <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$fin.prev4.75  <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$fin.pop9.75   <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$fin.prev9.75  <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$fin.pop14.75  <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$fin.prev14.75 <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$fin.pop19.75  <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$fin.prev19.75 <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$fin.pop24.75  <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$fin.prev24.75 <- rep(NA, length(df.dens.wiw$hunt.f))
# 
# out <- list()
# pop.sum <- list()
# prev.sum <- list()
# df.dens.wiw <- list(df.dens.wiw)
# 
# for (i in 1:length(df.dens.wiw[[1]]$hunt.f)){
#   out <- cwd_det_model_wiw(params = list(fawn.an.sur = fawn.an.sur,
#                                          juv.an.sur = juv.an.sur,
#                                          ad.an.f.sur = ad.an.f.sur,
#                                          ad.an.m.sur = ad.an.m.sur,
#                                          fawn.repro = fawn.repro,
#                                          juv.repro = juv.repro,
#                                          ad.repro = ad.repro,
#                                          hunt.mort.fawn = hunt.mort.fawn,
#                                          hunt.mort.juv.f = hunt.mort.juv.f,
#                                          hunt.mort.juv.m = hunt.mort.juv.m,
#                                          hunt.mort.ad.f = df.dens.wiw[[1]]$hunt.f[i],
#                                          hunt.mort.ad.m = df.dens.wiw[[1]]$hunt.m[i],
#                                          ini.fawn.prev = ini.fawn.prev,
#                                          ini.juv.prev = ini.juv.prev,
#                                          ini.ad.f.prev = ini.ad.f.prev,
#                                          ini.ad.m.prev = ini.ad.m.prev,
#                                          n.age.cats = n.age.cats,
#                                          p = p,
#                                          env.foi = env.foi,
#                                          beta.ff = 0.08/(n0*.8),
#                                          gamma.mm = 1,
#                                          gamma.mf = df.dens.wiw[[1]]$beta.mf.1[i],
#                                          gamma.fm = df.dens.wiw[[1]]$beta.fm.1[i],
#                                          theta = 1,
#                                          n0 = n0,
#                                          n.years = n.years,
#                                          rel.risk = rel.risk))
#   pop.sum <- out$counts %>%
#     filter(month %% 12 == 10) %>%
#     group_by(year) %>%
#     summarize(n = sum(population)) #Total pop at time
#   
#   prev.sum   <- out$counts %>%
#     filter(month %% 12 == 10) %>%
#     group_by(year, disease) %>%
#     summarize(n = sum(population)) %>%
#     pivot_wider(names_from = disease, values_from = n) %>%
#     mutate(prev = yes/(no + yes)) #Prev at time
#   
#   #If you know a way to index these, it would speed the sim
#   df.dens.wiw[[1]]$fin.pop2.75[i]    <- pop.sum$n[pop.sum$year==cut2.75]
#   df.dens.wiw[[1]]$fin.pop4.75[i]    <- pop.sum$n[pop.sum$year==cut4.75]
#   df.dens.wiw[[1]]$fin.pop9.75[i]    <- pop.sum$n[pop.sum$year==cut9.75]
#   df.dens.wiw[[1]]$fin.pop14.75[i]   <- pop.sum$n[pop.sum$year==cut14.75]
#   df.dens.wiw[[1]]$fin.pop19.75[i]   <- pop.sum$n[pop.sum$year==cut19.75]
#   df.dens.wiw[[1]]$fin.pop24.75[i]   <- pop.sum$n[pop.sum$year==cut24.75]
#   df.dens.wiw[[1]]$fin.prev2.75[i]   <- prev.sum$prev[prev.sum$year==cut2.75]
#   df.dens.wiw[[1]]$fin.prev4.75[i]   <- prev.sum$prev[prev.sum$year==cut4.75]
#   df.dens.wiw[[1]]$fin.prev9.75[i]   <- prev.sum$prev[prev.sum$year==cut9.75]
#   df.dens.wiw[[1]]$fin.prev14.75[i]  <- prev.sum$prev[prev.sum$year==cut14.75]
#   df.dens.wiw[[1]]$fin.prev19.75[i]  <- prev.sum$prev[prev.sum$year==cut19.75]
#   df.dens.wiw[[1]]$fin.prev24.75[i]  <- prev.sum$prev[prev.sum$year==cut24.75]
# }
# 
# df.dens.wiw <- data.frame(df.dens.wiw[[1]])
# df.dens.wiw$beta.fm.1[which(df.dens.wiw$beta.fm.1 == 0.5)] <- "FM Gamma = 0.5"
# df.dens.wiw$beta.fm.1[which(df.dens.wiw$beta.fm.1 == 1)] <- "FM Gamma = 1"
# df.dens.wiw$beta.fm.1[which(df.dens.wiw$beta.fm.1 == 2)] <- "FM Gamma = 2"
# df.dens.wiw$beta.mf.1[which(df.dens.wiw$beta.mf.1 == 0.5)] <- "MF Gamma = 0.5"
# df.dens.wiw$beta.mf.1[which(df.dens.wiw$beta.mf.1 == 1)] <- "MF Gamma = 1"
# df.dens.wiw$beta.mf.1[which(df.dens.wiw$beta.mf.1 == 2)] <- "MF Gamma = 2"
# 
# pop.cut <- 3000 #population critical value
# prev.cut <- 0.3 #prevalence critical value
# 
# df.dens.wiw$class2.75 <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$class2.75[which(df.dens.wiw$fin.pop2.75 >= pop.cut & df.dens.wiw$fin.prev2.75 <= prev.cut)] <- "Survive"
# df.dens.wiw$class2.75[which(df.dens.wiw$fin.pop2.75 < pop.cut & df.dens.wiw$fin.prev2.75 <= prev.cut)] <- "Crash"
# df.dens.wiw$class2.75[which(df.dens.wiw$fin.pop2.75 >= pop.cut & df.dens.wiw$fin.prev2.75 > prev.cut)] <- "Disease"
# df.dens.wiw$class2.75[which(df.dens.wiw$fin.pop2.75 < pop.cut & df.dens.wiw$fin.prev2.75 > prev.cut)] <- "Crash+Disease"
# df.dens.wiw$class2.75[is.na(df.dens.wiw$fin.pop2.75)] <- "NA/Crash"
# df.dens.wiw$class2.75 <- factor(df.dens.wiw$class2.75)
# 
# df.dens.wiw$class4.75 <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$class4.75[which(df.dens.wiw$fin.pop4.75 >= pop.cut & df.dens.wiw$fin.prev4.75 <= prev.cut)] <- "Survive"
# df.dens.wiw$class4.75[which(df.dens.wiw$fin.pop4.75 < pop.cut & df.dens.wiw$fin.prev4.75 <= prev.cut)] <- "Crash"
# df.dens.wiw$class4.75[which(df.dens.wiw$fin.pop4.75 >= pop.cut & df.dens.wiw$fin.prev4.75 > prev.cut)] <- "Disease"
# df.dens.wiw$class4.75[which(df.dens.wiw$fin.pop4.75 < pop.cut & df.dens.wiw$fin.prev4.75 > prev.cut)] <- "Crash+Disease"
# df.dens.wiw$class4.75[is.na(df.dens.wiw$fin.pop4.75)] <- "NA/Crash"
# df.dens.wiw$class4.75 <- factor(df.dens.wiw$class4.75)
# 
# df.dens.wiw$class9.75 <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$class9.75[which(df.dens.wiw$fin.pop9.75 >= pop.cut & df.dens.wiw$fin.prev9.75 <= prev.cut)] <- "Survive"
# df.dens.wiw$class9.75[which(df.dens.wiw$fin.pop9.75 < pop.cut & df.dens.wiw$fin.prev9.75 <= prev.cut)] <- "Crash"
# df.dens.wiw$class9.75[which(df.dens.wiw$fin.pop9.75 >= pop.cut & df.dens.wiw$fin.prev9.75 > prev.cut)] <- "Disease"
# df.dens.wiw$class9.75[which(df.dens.wiw$fin.pop9.75 < pop.cut & df.dens.wiw$fin.prev9.75 > prev.cut)] <- "Crash+Disease"
# df.dens.wiw$class9.75[is.na(df.dens.wiw$fin.pop9.75)] <- "NA/Crash"
# df.dens.wiw$class9.75 <- factor(df.dens.wiw$class9.75)
# 
# df.dens.wiw$class14.75 <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$class14.75[which(df.dens.wiw$fin.pop14.75 >= pop.cut & df.dens.wiw$fin.prev14.75 <= prev.cut)] <- "Survive"
# df.dens.wiw$class14.75[which(df.dens.wiw$fin.pop14.75 < pop.cut & df.dens.wiw$fin.prev14.75 <= prev.cut)] <- "Crash"
# df.dens.wiw$class14.75[which(df.dens.wiw$fin.pop14.75 >= pop.cut & df.dens.wiw$fin.prev14.75 > prev.cut)] <- "Disease"
# df.dens.wiw$class14.75[which(df.dens.wiw$fin.pop14.75 < pop.cut & df.dens.wiw$fin.prev14.75 > prev.cut)] <- "Crash+Disease"
# df.dens.wiw$class14.75[is.na(df.dens.wiw$fin.pop14.75)] <- "NA/Crash"
# df.dens.wiw$class14.75 <- factor(df.dens.wiw$class14.75)
# 
# df.dens.wiw$class19.75 <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$class19.75[which(df.dens.wiw$fin.pop19.75 >= pop.cut & df.dens.wiw$fin.prev19.75 <= prev.cut)] <- "Survive"
# df.dens.wiw$class19.75[which(df.dens.wiw$fin.pop19.75 < pop.cut & df.dens.wiw$fin.prev19.75 <= prev.cut)] <- "Crash"
# df.dens.wiw$class19.75[which(df.dens.wiw$fin.pop19.75 >= pop.cut & df.dens.wiw$fin.prev19.75 > prev.cut)] <- "Disease"
# df.dens.wiw$class19.75[which(df.dens.wiw$fin.pop19.75 < pop.cut & df.dens.wiw$fin.prev19.75 > prev.cut)] <- "Crash+Disease"
# df.dens.wiw$class19.75[is.na(df.dens.wiw$fin.pop19.75)] <- "NA/Crash"
# df.dens.wiw$class19.75 <- factor(df.dens.wiw$class19.75)
# 
# df.dens.wiw$class24.75 <- rep(NA, length(df.dens.wiw$hunt.f))
# df.dens.wiw$class24.75[which(df.dens.wiw$fin.pop24.75 >= pop.cut & df.dens.wiw$fin.prev24.75 <= prev.cut)] <- "Survive"
# df.dens.wiw$class24.75[which(df.dens.wiw$fin.pop24.75 < pop.cut & df.dens.wiw$fin.prev24.75 <= prev.cut)] <- "Crash"
# df.dens.wiw$class24.75[which(df.dens.wiw$fin.pop24.75 >= pop.cut & df.dens.wiw$fin.prev24.75 > prev.cut)] <- "Disease"
# df.dens.wiw$class24.75[which(df.dens.wiw$fin.pop24.75 < pop.cut & df.dens.wiw$fin.prev24.75 > prev.cut)] <- "Crash+Disease"
# df.dens.wiw$class24.75[is.na(df.dens.wiw$fin.pop24.75)] <- "NA/Crash"
# df.dens.wiw$class24.75 <- factor(df.dens.wiw$class24.75)
# 
# levels(df.dens.wiw$class2.75)

#write.csv(df.dens.wiw, "df.dens.wiw.csv")
df.dens.wiw <- read.csv("df.dens.wiw.csv")

df.dens.wiw$class2.75  <- factor(df.dens.wiw$class2.75, levels = order)
df.dens.wiw$class4.75  <- factor(df.dens.wiw$class4.75, levels = order)
df.dens.wiw$class9.75  <- factor(df.dens.wiw$class9.75, levels = order)
df.dens.wiw$class14.75 <- factor(df.dens.wiw$class14.75, levels = order)
df.dens.wiw$class19.75 <- factor(df.dens.wiw$class19.75, levels = order)
df.dens.wiw$class24.75 <- factor(df.dens.wiw$class24.75, levels = order)

myColors <- c("#3288BD", "#ABDDA4", "#FFFFBF", "#D53E4F", "#F46D43")
names(myColors) <- levels(c("Survive", "Crash", "Disease", "Crash+Disease", "NA/Crash"))
colScalef <- scale_fill_manual(name = "class2.75",values = myColors)

dens.dep2.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=class2.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",2.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

dens.pop2.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.pop2.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",2.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev2.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev2.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",2.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

dens.dep4.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=class4.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",4.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

dens.pop4.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.pop4.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",4.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev4.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev4.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",4.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

dens.dep9.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=class9.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",9.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

dens.pop9.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.pop9.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",9.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev9.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev9.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",9.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

dens.dep14.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=class14.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",14.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

dens.pop14.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.pop14.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",14.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev14.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev14.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",14.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

dens.dep19.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=class19.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",19.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

dens.pop19.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.pop19.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",19.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev19.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev19.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",19.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

dens.dep24.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=class24.75)) + 
  geom_tile() +
  theme_bw() +
  labs(title = paste("Class at",24.75,"Years"), subtitle = paste("Theta = 0; Crit Pop =",pop.cut,"; Crit Prev =",prev.cut, sep="")) +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScalef

dens.pop24.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=log(fin.pop24.75))) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=1) +
  labs(title = paste("Heatplot of Pop at",24.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dens.prev24.75 <- 
  ggplot(df.dens.wiw, aes(x=hunt.f, y=hunt.m, fill=fin.prev24.75)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(palette="RdYlGn", direction=-1) +
  labs(title = paste("Heatplot of Prev at",24.75,"Years"), subtitle = "Theta = 0") +
  facet_grid(rows = vars(beta.fm.1), cols = vars(beta.mf.1)) +
  theme(axis.text.x = element_text(angle = 270, hjust = -1))

#############################
#WIW over time (density)
#############################
require(reshape2)
colnames(df.dens.wiw)
longdendf.wiw <- melt(df.dens.wiw, id.vars = c("X","hunt.f","hunt.m","beta.fm.1","beta.mf.1","theta.1"),
                      measure.vars = c("fin.pop2.75","fin.prev2.75","fin.pop4.75","fin.prev4.75","fin.pop9.75",
                                       "fin.prev9.75","fin.pop14.75","fin.prev14.75","fin.pop19.75","fin.prev19.75",
                                       "fin.pop24.75","fin.prev24.75","class2.75","class4.75","class9.75",
                                       "class14.75","class19.75","class24.75"), 
                      variable.name = "Measure", value.name = "Value")
length(df.dens.wiw$X)
length(longdendf.wiw$X)
longdendf.wiw$Year <- rep(NA, length(longdendf.wiw$X))
longdendf.wiw$Type <- rep(NA, length(longdendf.wiw$X))
longdendf.wiw$Year <- ifelse((longdendf.wiw$Measure=="fin.pop2.75"|
                                longdendf.wiw$Measure=="fin.prev2.75"|
                                longdendf.wiw$Measure=="class2.75"), 2.75, longdendf.wiw$Year)
longdendf.wiw$Year <- ifelse((longdendf.wiw$Measure=="fin.pop4.75"|
                                longdendf.wiw$Measure=="fin.prev4.75"|
                                longdendf.wiw$Measure=="class4.75"), 4.75, longdendf.wiw$Year)
longdendf.wiw$Year <- ifelse((longdendf.wiw$Measure=="fin.pop9.75"|
                                longdendf.wiw$Measure=="fin.prev9.75"|
                                longdendf.wiw$Measure=="class9.75"), 9.75, longdendf.wiw$Year)
longdendf.wiw$Year <- ifelse((longdendf.wiw$Measure=="fin.pop14.75"|
                                longdendf.wiw$Measure=="fin.prev14.75"|
                                longdendf.wiw$Measure=="class14.75"), 14.75, longdendf.wiw$Year)
longdendf.wiw$Year <- ifelse((longdendf.wiw$Measure=="fin.pop19.75"|
                                longdendf.wiw$Measure=="fin.prev19.75"|
                                longdendf.wiw$Measure=="class19.75"), 19.75, longdendf.wiw$Year)
longdendf.wiw$Year <- ifelse((longdendf.wiw$Measure=="fin.pop24.75"|
                                longdendf.wiw$Measure=="fin.prev24.75"|
                                longdendf.wiw$Measure=="class24.75"), 24.75, longdendf.wiw$Year)
longdendf.wiw$Type <- ifelse((longdendf.wiw$Measure=="fin.pop2.75"|
                                longdendf.wiw$Measure=="fin.pop4.75"|
                                longdendf.wiw$Measure=="fin.pop9.75"|
                                longdendf.wiw$Measure=="fin.pop14.75"|
                                longdendf.wiw$Measure=="fin.pop19.75"|
                                longdendf.wiw$Measure=="fin.pop24.75"), "Population", longdendf.wiw$Type)
longdendf.wiw$Type <- ifelse((longdendf.wiw$Measure=="fin.prev2.75"|
                                longdendf.wiw$Measure=="fin.prev4.75"|
                                longdendf.wiw$Measure=="fin.prev9.75"|
                                longdendf.wiw$Measure=="fin.prev14.75"|
                                longdendf.wiw$Measure=="fin.prev19.75"|
                                longdendf.wiw$Measure=="fin.prev24.75"), "Prevalence", longdendf.wiw$Type)
longdendf.wiw$Type <- ifelse((longdendf.wiw$Measure=="class2.75"|
                                longdendf.wiw$Measure=="class4.75"|
                                longdendf.wiw$Measure=="class9.75"|
                                longdendf.wiw$Measure=="class14.75"|
                                longdendf.wiw$Measure=="class19.75"|
                                longdendf.wiw$Measure=="class24.75"), "Class", longdendf.wiw$Type)
longdendf.wiw$Population <- ifelse((longdendf.wiw$Type=="Population"), as.numeric(as.character(longdendf.wiw$Value)), NA)
longdendf.wiw$Prevalence <- ifelse((longdendf.wiw$Type=="Prevalence"), as.numeric(as.character(longdendf.wiw$Value)), NA)
longdendf.wiw$Class <- ifelse((longdendf.wiw$Type=="Class"), longdendf.wiw$Value, NA)

unique(longdendf.wiw$Class)
order <- c("Survive", "Crash", "Disease", "Crash+Disease", "NA/Crash")
longdendf.wiw$Class  <- factor(longdendf.wiw$Class, levels = order)

names(myColors) <- levels(longdendf.wiw$Class)
colScale <- scale_fill_manual(name ="Class", values = myColors)

FMgtMF.den.class <- 
  ggplot(longdendf.wiw[which(longdendf.wiw$beta.fm.1=="FM Gamma = 2" &
                               longdendf.wiw$beta.mf.1=="MF Gamma = 0.5"&
                               longdendf.wiw$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Sex Dep. Trans. FM(2)>MF(.5) w/ Theta = 0")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

MFgtFM.den.class <- 
  ggplot(longdendf.wiw[which(longdendf.wiw$beta.fm.1=="FM Gamma = 0.5" &
                               longdendf.wiw$beta.mf.1=="MF Gamma = 2"&
                               longdendf.wiw$Type=="Class"),], 
         aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Sex Dep. Trans. FM(.5)<MF(2) w/ Theta = 0")) +
  facet_wrap(~Year, ncol=3) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

FMgtMF.den.class
MFgtFM.den.class





longfredf$relative <- rep(NA, length(longfredf$hunt.f))
longfredf$relative <- ifelse((longfredf$beta.f.1=="F Beta = 0.04" &
                  longfredf$beta.m.1=="M Beta = 0.12"), "Male-dominated", longfredf$relative)
longfredf$relative <- ifelse((longfredf$beta.f.1=="F Beta = 0.12" &
                                longfredf$beta.m.1=="M Beta = 0.04"), "Female-dominated", longfredf$relative)
longfredf$relative <- ifelse((longfredf$beta.f.1=="F Beta = 0.08" &
                                longfredf$beta.m.1=="M Beta = 0.08"), "Equal", longfredf$relative)

ggplot(longfredf[which(!is.na(longfredf$relative) &
                         longfredf$Type == "Class"),], 
       aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Beta F (0.04)=M (0.04) w/ Theta = 1")) +
  facet_grid(rows = vars(relative), cols = vars(Year)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale

longdendf$relative <- rep(NA, length(longdendf$hunt.f))
longdendf$relative <- ifelse((longdendf$beta.f.1=="F Beta = 5.0e-06" &
                                longdendf$beta.m.1=="M Beta = 1.5e-05"), "Male-dominated", longdendf$relative)
longdendf$relative <- ifelse((longdendf$beta.f.1=="F Beta = 1.5e-05" &
                                longdendf$beta.m.1=="M Beta = 5.0e-06"), "Female-dominated", longdendf$relative)
longdendf$relative <- ifelse((longdendf$beta.f.1=="F Beta = 1.0e-05" &
                                longdendf$beta.m.1=="M Beta = 1.0e-05"), "Equal", longdendf$relative)

ggplot(longdendf[which(!is.na(longdendf$relative) &
                         longdendf$Type == "Class"),], 
       aes(x=hunt.f, y=hunt.m, fill=Class)) +
  geom_tile() +
  theme_bw() +
  labs(title = paste("Classification of Populations over Time"), subtitle = paste("Theta = 0")) +
  facet_grid(rows = vars(relative), cols = vars(Year)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  colScale


