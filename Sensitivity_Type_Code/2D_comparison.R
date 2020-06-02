require(readr)
require(CWDsims)
require(tidyverse)
require(ggpubr)

#calling the R0 validated scenarios
scenarios <- read_csv("R0_Scenarios.csv") 
scenarios$B.ff
#creating a dataframe with k*4 reps per scenario
k <- 25
df <- data.frame(beta.ff = rep(scenarios$B.ff, each = k*4)) 
df$gamma.mm <- rep(scenarios$G.mm, each = k*4)
df$gamma.mf <- rep(scenarios$G.mf, each = k*4)
df$gamma.fm <- rep(scenarios$G.fm, each = k*4)

#spreading 100 hunting levels for four categories between the three scenarios
df$hunt.f <- rep(c(seq(0, .99, length.out= k), rep(.1, k*3)), 3) 
df$hunt.m <- rep(c(rep(.2, k), seq(0, .99, length.out= k),rep(.2, k*2)), 3)
df$hunt.j.m <- rep(c(rep(.2, k*2), seq(0, .99, length.out= k),rep(.2, k)), 3)
df$hunt.j.f <- rep(c(rep(.1, k*3), seq(0, .99, length.out= k)), 3)

#for sim - items to hold intermediates and output
df$fin.pop9.75   <- rep(NA, length(df$hunt.f))
df$fin.prev9.75  <- rep(NA, length(df$hunt.f))
df$fin.pop19.75  <- rep(NA, length(df$hunt.f))
df$fin.prev19.75 <- rep(NA, length(df$hunt.f))
df$fin.pop24.75  <- rep(NA, length(df$hunt.f))
df$fin.prev24.75 <- rep(NA, length(df$hunt.f))
df$R0 <- rep(NA, length(df$hunt.f))

# running through all scenarios
for (i in 1:(nrow(df))){
  out <- cwd_det_model_wiw(params = list(
    fawn.an.sur = 0.6, 
    juv.an.sur = 0.8, 
    ad.an.f.sur = 0.95, 
    ad.an.m.sur = 0.9, 
    fawn.repro = 0, 
    juv.repro = 0.6, 
    ad.repro = 1, 
    hunt.mort.fawn = 0.01,
    ini.fawn.prev = 0.02,
    ini.juv.prev = 0.03, 
    ini.ad.f.prev = 0.04,  
    ini.ad.m.prev = 0.04,
    n.age.cats = 12,
    p = 0.43,
    env.foi = 0,  
    theta = 1, 
    n0 = 2000, 
    n.years = 25, 
    rel.risk = 1.0, 
    hunt.mort.juv.f = df$hunt.j.f[i],
    hunt.mort.juv.m = df$hunt.j.m[i],
    hunt.mort.ad.f = df$hunt.f[i],
    hunt.mort.ad.m = df$hunt.m[i],
    beta.ff = df$beta.ff[i],
    gamma.mm = df$gamma.mm[i],
    gamma.mf = df$gamma.mf[i],
    gamma.fm = df$gamma.fm[i]))
  
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

  df$fin.pop9.75[i]    <- pop.sum$n[pop.sum$year == 9.75]
  df$fin.pop19.75[i]   <- pop.sum$n[pop.sum$year == 19.75]
  df$fin.pop24.75[i]   <- pop.sum$n[pop.sum$year == 24.75]
  df$fin.prev9.75[i]   <- prev.sum$prev[prev.sum$year == 9.75]
  df$fin.prev19.75[i]  <- prev.sum$prev[prev.sum$year == 19.75]
  df$fin.prev24.75[i]  <- prev.sum$prev[prev.sum$year == 24.75]
}

#naming scenarios
df$scenario <- NA
df$scenario[1:(k*4)] <- "equal"
df$scenario[((k*4)+1):(k*4*2)] <- "male.dominated"
df$scenario[((k*4*2)+1):(k*4*3)] <- "female.dominated"

#naming the changing hunting parameter
df$rate.var <- rep(rep(c("af", "am", "jm", "jf"), each = k), 3)
df$rate.val <- NA
df$rate.val <- ifelse(df$rate.var == "af", df$hunt.f, df$rate.val)
df$rate.val <- ifelse(df$rate.var == "am", df$hunt.m, df$rate.val)
df$rate.val <- ifelse(df$rate.var == "jm", df$hunt.j.m, df$rate.val)
df$rate.val <- ifelse(df$rate.var == "jf", df$hunt.j.f, df$rate.val)

prev <- df %>% 
  ggplot(aes(rate.val, fin.prev9.75, color = rate.var)) + 
  geom_line() +
  facet_grid(.~scenario) +
  scale_y_log10() +
  theme_classic()

pop <- df %>% 
  ggplot(aes(rate.val, fin.pop9.75, color = rate.var)) + 
  geom_line() +
  facet_grid(.~scenario) +
  scale_y_log10() +
  theme_classic()

ggarrange(prev, pop, ncol=1, common.legend = T, legend = "right")
ggsave("Plots/2D_comparisons.tiff", dpi=300, width = 10, height = 7)

df %>% 
  ggplot(aes(rate.val, fin.prev24.75, color = rate.var, linetype = scenario)) + 
  geom_line() +
  geom_vline(aes(xintercept = .1)) +
  theme_classic()

df %>% 
  ggplot(aes(rate.val, fin.pop24.75, color = rate.var, linetype = scenario)) + 
  geom_line() +
  geom_vline(aes(xintercept = .1)) +
  theme_classic()

