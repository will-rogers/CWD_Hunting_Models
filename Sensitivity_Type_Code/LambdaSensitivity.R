require(CWDsims)
require(tidyverse)
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
n.years = 5
rel.risk = 1.0

beta.f            <- 0.08
beta.m            <- 0.08

hunt.mort.ad.f    <- 0
hunt.mort.ad.m    <- 0

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
                                   hunt.mort.ad.f = hunt.mort.ad.f,
                                   hunt.mort.ad.m = hunt.mort.ad.m,
                                   ini.fawn.prev = ini.fawn.prev,
                                   ini.juv.prev = ini.juv.prev,
                                   ini.ad.f.prev = ini.ad.f.prev,
                                   ini.ad.m.prev = ini.ad.m.prev,
                                   n.age.cats = n.age.cats,
                                   p = p,
                                   env.foi = env.foi,
                                   beta.f = beta.f,
                                   beta.m = beta.m,
                                   theta = 1,
                                   n0 = n0,
                                   n.years = n.years,
                                   rel.risk = rel.risk))

M <- out$matrix
require(popbio)
eigen.analysis(M)
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

?vitalsens
vitalrates <- list(
  S0 = 0.6,
  S1 = 0.8,
  SA.f = 0.95,
  SA.m = 0.9,
  b1 = 0.6,
  bA.f = 1
)

elements <- expression(
  0, .5*S0*b1, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  S1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  
  0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.f, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, .5*S0*b1, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, S1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.m, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.m, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.m, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.m, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.m, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.m, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.m, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.m, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.m, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.m, SA.m
)

# elements <- expression(
#   0, .5*S0*b1, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f, .5*S0*bA.f,
# S1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
# 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
# 0, 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
# 0, 0, 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 0, 
# 0, 0, 0, 0, SA.f, 0, 0, 0, 0, 0, 0, 0, 
# 0, 0, 0, 0, 0, SA.f, 0, 0, 0, 0, 0, 0,
# 0, 0, 0, 0, 0, 0, SA.f, 0, 0, 0, 0, 0, 
# 0, 0, 0, 0, 0, 0, 0, SA.f, 0, 0, 0, 0, 
# 0, 0, 0, 0, 0, 0, 0, 0, SA.f, 0, 0, 0, 
# 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.f, 0, 0,  
# 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, SA.f, SA.f)

vitalsens(elements, vitalrates)

n <- length(vitalrates)
vr <- seq(0,1,.01)
vrsen <- matrix(numeric(n*length(vr)), ncol=n, dimnames=list(vr, names(vitalrates)))
for (h in 1:n) {
  vitalrates2 <- list(  S0 = 0.6,
                        S1 = 0.8,
                        SA.f = 0.95,
                        SA.m = 0.9,
                        b1 = 0.6,
                        bA.f = 1)
  for (i in 1:length(vr))
  {
    vitalrates2[[h]] <- vr[i]
    A <- matrix(sapply(elements, eval,vitalrates2 , NULL), nrow=sqrt(length(elements)), byrow=TRUE)
    vrsen[i,h] <- max(Re(eigen(A)$values))
  }
}

df <- data.frame(vrsen)
df$id <- seq(0,1, by = .01)
require(reshape2)
df <- melt(df, id.vars = 'id')
a <- ggplot(df, aes(id, value, color= variable)) +
  geom_line() + 
  ylim(.9,1.25)+
  geom_hline(aes(yintercept=1), linetype="dotted") + 
  labs(x = "Demographic Parameter Range",
       y = expression(lambda)) +
  scale_color_discrete(labels = c(
    "Fawn Survival", 
    "Juvenile Survival",
    "Cow Survival",
    "Bull Survival",
    "Juvenilt Fecundity",
    "Cow Fecundity"
  ), 
  name = NULL) +
  theme_classic()
ggsave("sensitivityvr.png", dpi=300, width = 6, height = 5)

















