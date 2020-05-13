# What does it look like when females dominate transmission but males are highly seropositive
##############################
require(CWDsims)
require(tidyverse)
params.a <- list(fawn.an.sur = 0.6, 
               juv.an.sur = 0.8, 
               ad.an.f.sur = 0.95, 
               ad.an.m.sur = 0.9, 
               fawn.repro = 0, 
               juv.repro = 0.6, 
               ad.repro = 1, 
               hunt.mort.fawn = 0.01, 
               hunt.mort.juv.f = 0.1, 
               hunt.mort.juv.m = 0.1,
               hunt.mort.ad.f = 0.1, 
               hunt.mort.ad.m = 0.2, 
               ini.fawn.prev = 0.02,
               ini.juv.prev = 0.03, 
               ini.ad.f.prev = 0.1,  
               ini.ad.m.prev = 0.2,
               n.age.cats = 12,  
               p = 0.43, 
               env.foi = 0.01, 
               # beta.f = .04,
               # beta.m = .12,
               beta.ff = 0.06,
               gamma.mm = 2,
               gamma.mf = 2,
               gamma.fm = 1,
               theta = 1, 
               n0 = 2000, 
               n.years = 25, 
               rel.risk = 1.0)
params.b <- list(fawn.an.sur = 0.6, 
               juv.an.sur = 0.8, 
               ad.an.f.sur = 0.95, 
               ad.an.m.sur = 0.9, 
               fawn.repro = 0, 
               juv.repro = 0.6, 
               ad.repro = 1, 
               hunt.mort.fawn = 0.01, 
               hunt.mort.juv.f = 0.1, 
               hunt.mort.juv.m = 0.1,
               hunt.mort.ad.f = 0.1, 
               hunt.mort.ad.m = 0.2, 
               ini.fawn.prev = 0.02,
               ini.juv.prev = 0.03, 
               ini.ad.f.prev = 0.1,  
               ini.ad.m.prev = 0.2,
               n.age.cats = 12,  
               p = 0.43, 
               env.foi = 0.01, 
               # beta.f = .12,
               # beta.m = .04,
               beta.ff = 0.12,
               gamma.mm = .01,
               gamma.mf = .01,
               gamma.fm = 2,
               theta = 1, 
               n0 = 2000, 
               n.years = 25, 
               rel.risk = 1.0)
params.c <- list(fawn.an.sur = 0.6, 
                 juv.an.sur = 0.8, 
                 ad.an.f.sur = 0.95, 
                 ad.an.m.sur = 0.9, 
                 fawn.repro = 0, 
                 juv.repro = 0.6, 
                 ad.repro = 1, 
                 hunt.mort.fawn = 0.01, 
                 hunt.mort.juv.f = 0.1, 
                 hunt.mort.juv.m = 0.1,
                 hunt.mort.ad.f = 0.1, 
                 hunt.mort.ad.m = 0.2, 
                 ini.fawn.prev = 0.02,
                 ini.juv.prev = 0.03, 
                 ini.ad.f.prev = 0.1,  
                 ini.ad.m.prev = 0.2,
                 n.age.cats = 12,  
                 p = 0.43, 
                 env.foi = 0.01,  
                 # beta.f = .08,
                 # beta.m = .08,
                 beta.ff = 0.06,
                 gamma.mm = 1,
                 gamma.mf = 1,
                 gamma.fm = 1,
                 theta = 1, 
                 n0 = 2000, 
                 n.years = 25, 
                 rel.risk = 1.0)

out.a <- cwd_det_model_wiw(params.a)
out.b <- cwd_det_model_wiw(params.b)
out.c <- cwd_det_model_wiw(params.c)

out.a$counts$age.cat <- "adult"
out.a$counts$age.cat[out.a$counts$age == 1] <- "fawn"
out.a$counts.sum <- out.a$counts %>% filter(month%%12 == 8) %>% group_by(year, 
                                                                         sex, age.cat) %>% summarize(n = sum(population)) %>% 
  unite(sex.age, sex, age.cat) %>% spread(key = sex.age, 
                                          value = n) %>% mutate(buck.doe = m_adult/f_adult)

out.b$counts$age.cat <- "adult"
out.b$counts$age.cat[out.b$counts$age == 1] <- "fawn"
out.b$counts.sum <- out.b$counts %>% filter(month%%12 == 8) %>% group_by(year, 
                                                       sex, age.cat) %>% summarize(n = sum(population)) %>% 
  unite(sex.age, sex, age.cat) %>% spread(key = sex.age, 
                                          value = n) %>% mutate(buck.doe = m_adult/f_adult)

out.c$counts$age.cat <- "adult"
out.c$counts$age.cat[out.c$counts$age == 1] <- "fawn"
out.c$counts.sum <- out.c$counts %>% filter(month%%12 == 8) %>% group_by(year, 
                                                                         sex, age.cat) %>% summarize(n = sum(population)) %>% 
  unite(sex.age, sex, age.cat) %>% spread(key = sex.age, 
                                          value = n) %>% mutate(buck.doe = m_adult/f_adult)

df <- rbind(data.frame(out.a$counts.sum),data.frame(out.b$counts.sum),data.frame(out.c$counts.sum))
df$type <- c(rep("male.dom",25),rep("female.dom",25),rep("equal",25))
ggplot(df, aes(x = year, y = buck.doe, color = type)) + 
  geom_line() +
  ylab("Buck:Doe ratio") + 
  xlab("Year") + 
  xlim(1,25) +
  theme_classic() 
ggsave("bdindex.tiff", dpi=300, width = 7, height = 5)

######################################
set.seed(873470)
params.a <- list(fawn.an.sur = 0.6, 
                 juv.an.sur = 0.8, 
                 ad.an.f.sur = 0.95, 
                 ad.an.m.sur = 0.9, 
                 fawn.repro = 0, 
                 juv.repro = 0.6, 
                 ad.repro = 1, 
                 hunt.mort.fawn = 0.01, 
                 hunt.mort.juv.f = 0.1, 
                 hunt.mort.juv.m = 0.1,
                 hunt.mort.ad.f = 0.1, 
                 hunt.mort.ad.m = 0.2, 
                 ini.fawn.prev = 0.02,
                 ini.juv.prev = 0.03, 
                 ini.ad.f.prev = 0.1,  
                 ini.ad.m.prev = 0.2,
                 n.age.cats = 12,  
                 p = 0.43, 
                 env.foi = 0.01, 
                 # beta.f = .04,
                 # beta.m = .12,
                 beta.ff = 0.04,
                 gamma.mm = 2,
                 gamma.mf = 2,
                 gamma.fm = 1,
                 theta = 1, 
                 n0 = 2000, 
                 n.years = 25, 
                 rel.risk = 1.0,
                 repro.var = 0.005, 
                 fawn.sur.var = 0.0025, 
                 sur.var = 0.0025, 
                 hunt.var = 0.0025)
params.b <- list(fawn.an.sur = 0.6, 
                 juv.an.sur = 0.8, 
                 ad.an.f.sur = 0.95, 
                 ad.an.m.sur = 0.9, 
                 fawn.repro = 0, 
                 juv.repro = 0.6, 
                 ad.repro = 1, 
                 hunt.mort.fawn = 0.01, 
                 hunt.mort.juv.f = 0.1, 
                 hunt.mort.juv.m = 0.1,
                 hunt.mort.ad.f = 0.1, 
                 hunt.mort.ad.m = 0.2, 
                 ini.fawn.prev = 0.02,
                 ini.juv.prev = 0.03, 
                 ini.ad.f.prev = 0.1,  
                 ini.ad.m.prev = 0.2,
                 n.age.cats = 12,  
                 p = 0.43, 
                 env.foi = 0.01, 
                 # beta.f = .12,
                 # beta.m = .04,
                 beta.ff = 0.08,
                 gamma.mm = .01,
                 gamma.mf = .01,
                 gamma.fm = 2,
                 theta = 1, 
                 n0 = 2000, 
                 n.years = 25, 
                 rel.risk = 1.0, 
                 rel.risk = 1.0,
                 repro.var = 0.0025, 
                 fawn.sur.var = 0.0025, 
                 sur.var = 0.0025, 
                 hunt.var = 0.0025)
params.c <- list(fawn.an.sur = 0.6, 
                 juv.an.sur = 0.8, 
                 ad.an.f.sur = 0.95, 
                 ad.an.m.sur = 0.9, 
                 fawn.repro = 0, 
                 juv.repro = 0.6, 
                 ad.repro = 1, 
                 hunt.mort.fawn = 0.01, 
                 hunt.mort.juv.f = 0.1, 
                 hunt.mort.juv.m = 0.1,
                 hunt.mort.ad.f = 0.1, 
                 hunt.mort.ad.m = 0.2, 
                 ini.fawn.prev = 0.02,
                 ini.juv.prev = 0.03, 
                 ini.ad.f.prev = 0.1,  
                 ini.ad.m.prev = 0.2,
                 n.age.cats = 12,  
                 p = 0.43, 
                 env.foi = 0.01,  
                 # beta.f = .08,
                 # beta.m = .08,
                 beta.ff = 0.06,
                 gamma.mm = 1,
                 gamma.mf = 1,
                 gamma.fm = 1,
                 theta = 1, 
                 n0 = 2000, 
                 n.years = 25, 
                 rel.risk = 1.0, 
                 rel.risk = 1.0,
                 repro.var = 0.0025, 
                 fawn.sur.var = 0.0025, 
                 sur.var = 0.0025, 
                 hunt.var = 0.0025)

a <- data.frame(year=c(),f_adult=c(),f_fawn=c(),m_adult=c(),m_fawn=c(),buck.doe=c(),type=c(),group=c())


for (i in 1:500){
  out.a <- cwd_stoch_model_wiw(params.a)
  out.a$counts$age.cat <- "adult"
  out.a$counts$age.cat[out.a$counts$age == 1] <- "fawn"
  out.a$counts.sum <- out.a$counts %>% filter(month %% 12 == 8) %>% group_by(year, 
                                                                           sex, age.cat) %>% summarize(n = sum(population)) %>% 
    unite(sex.age, sex, age.cat) %>% spread(key = sex.age, 
                                            value = n) %>% mutate(buck.doe = m_adult/f_adult)
  
  out.b <- cwd_stoch_model_wiw(params.b)
  out.b$counts$age.cat <- "adult"
  out.b$counts$age.cat[out.b$counts$age == 1] <- "fawn"
  out.b$counts.sum <- out.b$counts %>% filter(month%%12 == 8) %>% group_by(year, 
                                                                           sex, age.cat) %>% summarize(n = sum(population)) %>% 
    unite(sex.age, sex, age.cat) %>% spread(key = sex.age, 
                                            value = n) %>% mutate(buck.doe = m_adult/f_adult)
  
  out.c <- cwd_stoch_model_wiw(params.c)
  out.c$counts$age.cat <- "adult"
  out.c$counts$age.cat[out.c$counts$age == 1] <- "fawn"
  out.c$counts.sum <- out.c$counts %>% filter(month%%12 == 8) %>% group_by(year, 
                                                                           sex, age.cat) %>% summarize(n = sum(population)) %>% 
    unite(sex.age, sex, age.cat) %>% spread(key = sex.age, 
                                            value = n) %>% mutate(buck.doe = m_adult/f_adult)
  df <- rbind(data.frame(out.a$counts.sum),data.frame(out.b$counts.sum),data.frame(out.c$counts.sum))
  df$type <- c(rep("male.dom",25),rep("female.dom",25),rep("equal",25))
  df$group <- paste0(df$type, i)
  a <- rbind(a, df)
}
a <- a %>% 
  group_by(year, type) %>% 
  mutate(mean = mean(buck.doe), 
         sd = sd(buck.doe))
ggplot(a, aes(x = year, color = type)) + 
  geom_line(aes(y = buck.doe, group = group), alpha = 0.05) +
  geom_line(aes(y = mean), size = 1.25) + 
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = type), alpha = .25, color = NA) +
  ylab("Buck:Doe ratio") + 
  xlab("Year") + 
  xlim(0, 20) + 
  ylim(0.25, 1.25) +
  ggtitle("Stoc Sim. of B:D with Sex-based Trans.") +
  theme_classic() 
ggsave("bdindexstoch.png", dpi=300, width = 7, height = 5)
