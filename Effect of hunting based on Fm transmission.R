require(dplyr)
require(ggplot2)
require(reshape2)

beta.ff = rep(.04, 400)
gamma.mm = rep(seq(from = .1, to = 1, length.out = 4), each = 100)
gamma.mf = rep(1, 400)
gamma.fm = rep(1, 400)
hunt.f <- rep(seq(0, .99, by= .01), 4)
df <- data.frame(beta.ff, gamma.mm, gamma.mf, gamma.fm, hunt.f)
#for sim - items to hold intermediates and output
df$fin.pop9.75   <- rep(NA, length(df$hunt.f))
df$fin.prev9.75  <- rep(NA, length(df$hunt.f))
df$fin.pop19.75  <- rep(NA, length(df$hunt.f))
df$fin.prev19.75 <- rep(NA, length(df$hunt.f))
df$fin.pop24.75  <- rep(NA, length(df$hunt.f))
df$fin.prev24.75 <- rep(NA, length(df$hunt.f))
df$total.deaths.f <- rep(NA, length(df$hunt.f))
df$total.deaths.m <- rep(NA, length(df$hunt.f))

for (i in 1:length(df$hunt.f)){
  out <- CWDsims::cwd_det_model_wiw(params = list(
    fawn.an.sur = fawn.an.sur,
    juv.an.sur = juv.an.sur,
    ad.an.f.sur = ad.an.f.sur,
    ad.an.m.sur = ad.an.m.sur,
    fawn.repro = fawn.repro,
    juv.repro = juv.repro,
    ad.repro = ad.repro,
    hunt.mort.fawn = hunt.mort.fawn,
    hunt.mort.juv.f = .1,
    hunt.mort.juv.m = .2,
    hunt.mort.ad.f = df$hunt.f[i],
    hunt.mort.ad.m = .2,
    ini.fawn.prev = ini.fawn.prev,
    ini.juv.prev = ini.juv.prev,
    ini.ad.f.prev = ini.ad.f.prev,
    ini.ad.m.prev = ini.ad.m.prev,
    n.age.cats = n.age.cats,
    p = p,
    env.foi = env.foi,
    beta.ff = df$beta.ff[i],
    gamma.mm = df$gamma.mm[i],
    gamma.mf = df$gamma.mf[i],
    gamma.fm = df$gamma.fm[i],
    theta = 1,
    n0 = 10000,
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
                            "beta.ff", "gamma.mm", "gamma.mf", "gamma.fm", 
                            "total.deaths.f", "total.deaths.m"))

# equal <- df. %>%
#   filter(
#     beta.ff == .08 &
#       gamma.mm == 1 &
#       gamma.mf == 1 &
#       gamma.fm == 1
#   )
# male.dom <- df. %>%
#   filter(
#     beta.ff == .04 &
#       gamma.mm == 3 &
#       gamma.mf == 3 &
#       gamma.fm == 1
#   )
# female.dom <- df. %>%
#   filter(
#     beta.ff == .12 &
#       gamma.mm == .99 &
#       gamma.mf == .99 &
#       gamma.fm == 1.01
#   )
# 
# equal <- equal [c(1:100,501:600,1101:1200,1401:1500),]
# equal$scenario <- "equal"
# male.dom <- male.dom [c(1:100,501:600,1101:1200,1401:1500),]
# male.dom$scenario <- "male.dom"
# female.dom <- female.dom [c(1:100,501:600,1101:1200,1401:1500),]
# female.dom$scenario <- "female.dom"
# rbind(equal,male.dom,female.dom)
# df.a <- rbind(equal,male.dom,female.dom)  
# 
# df. <- df.[c(1:100,501:600,1101:1200,1401:1500),]


a <- df. %>% 
  ggplot(aes(hunt.f, color = factor(gamma.mm))) + 
  geom_line(aes(y = fin.prev9.75)) +
  xlim(0, 0.5) +
  labs(x = "Female Harvest Rate",
       y = "Prevalence at 19.75 Years",
       color = "Male to Male Transmission") +
  theme_classic()
ggsave("mmtrans_femaleharveffect.png", dpi = 300, width = 5, height = 5)


hunt.m <- c(seq(0.01, .99, by= .01) )
lambda <- rep(NA, 400 )

beta.ff = rep(.08, each = 400)
gamma.mm = rep(.3, 400)
gamma.mf = rep(.3, 400)
gamma.fm = rep(c(.8, 1, 2, 3), each = 100)
hunt.m <- rep(seq(0, .99, by= .01), 4)
df <- data.frame(beta.ff, gamma.mm, gamma.mf, gamma.fm, hunt.m)
#for sim - items to hold intermediates and output
df$fin.pop9.75   <- rep(NA, length(df$hunt.m))
df$fin.prev9.75  <- rep(NA, length(df$hunt.m))
df$fin.pop19.75  <- rep(NA, length(df$hunt.m))
df$fin.prev19.75 <- rep(NA, length(df$hunt.m))
df$fin.pop24.75  <- rep(NA, length(df$hunt.m))
df$fin.prev24.75 <- rep(NA, length(df$hunt.m))
df$total.deaths.f <- rep(NA, length(df$hunt.m))
df$total.deaths.m <- rep(NA, length(df$hunt.m))

for (i in 1:length(df$hunt.m)){
  out <- CWDsims::cwd_det_model_wiw(params = list(
    fawn.an.sur = fawn.an.sur,
    juv.an.sur = juv.an.sur,
    ad.an.f.sur = ad.an.f.sur,
    ad.an.m.sur = ad.an.m.sur,
    fawn.repro = fawn.repro,
    juv.repro = juv.repro,
    ad.repro = ad.repro,
    hunt.mort.fawn = hunt.mort.fawn,
    hunt.mort.juv.f = .1,
    hunt.mort.juv.m = .2,
    hunt.mort.ad.f = .1,
    hunt.mort.ad.m = df$hunt.m[i],
    ini.fawn.prev = ini.fawn.prev,
    ini.juv.prev = ini.juv.prev,
    ini.ad.f.prev = ini.ad.f.prev,
    ini.ad.m.prev = ini.ad.m.prev,
    n.age.cats = n.age.cats,
    p = p,
    env.foi = env.foi,
    beta.ff = df$beta.ff[i],
    gamma.mm = df$gamma.mm[i],
    gamma.mf = df$gamma.mf[i],
    gamma.fm = df$gamma.fm[i],
    theta = 1,
    n0 = 10000,
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
                            "beta.ff", "gamma.mm", "gamma.mf", "gamma.fm", 
                            "total.deaths.f", "total.deaths.m"))

# equal <- df. %>%
#   filter(
#     beta.ff == .08 &
#       gamma.mm == 1 &
#       gamma.mf == 1 &
#       gamma.fm == 1
#   )
# male.dom <- df. %>%
#   filter(
#     beta.ff == .04 &
#       gamma.mm == 3 &
#       gamma.mf == 3 &
#       gamma.fm == 1
#   )
# female.dom <- df. %>%
#   filter(
#     beta.ff == .12 &
#       gamma.mm == .99 &
#       gamma.mf == .99 &
#       gamma.fm == 1.01
#   )
# 
# equal <- equal [c(1:100,501:600,1101:1200,1401:1500),]
# equal$scenario <- "equal"
# male.dom <- male.dom [c(1:100,501:600,1101:1200,1401:1500),]
# male.dom$scenario <- "male.dom"
# female.dom <- female.dom [c(1:100,501:600,1101:1200,1401:1500),]
# female.dom$scenario <- "female.dom"
# rbind(equal,male.dom,female.dom)
# df.a <- rbind(equal,male.dom,female.dom)  
# 
# df. <- df.[c(1:100,501:600,1101:1200,1401:1500),]


b <- df. %>% 
  ggplot(aes(hunt.m, color = factor(gamma.fm))) + 
  geom_line(aes(y = fin.prev19.75)) +
  xlim(0, 0.5) +
  labs(x = "Male Harvest",
       y = "Prevalence at 19.75 Years",
       color = "Female to Male Transmission") +
  theme_classic()
