# This example is for NY DEC regarding new spsurvey code to perform their
# 2023-2027 rivers and streams design. In our meeting on 02-03-2023, we
# discussed specifying a design using stratifcation and unequal selection
# probabilities within each stratum. Each stratum has 10 desired sites, and
# within stratum, each of the four unequal probability categories should receive
# 3, 3, 2, and 2 expected samples, respectively. This example uses the
# NE_Lakes data in spsurvey and selects a sample with two strata, each receiving
# 5 samples. Within each stratum, each of the two unequal probability categories
# should receive 3 and 2 expected samples, respectively. Because there are the
# same samples per stratum and same expected sample sizes within stratum, some
# spsurvey grts() syntax can be simplified. Here we show how to specify
# caty_n using three ways, each of which requires more verbose syntax.
# Notice how all three sets of syntax yield the same design.

# load spsurvey
library(spsurvey)

# inspect NE_Lakes
NE_Lakes<-spsurvey::NE_Lakes

# stratification

## set strata variable
stratum_var <- "ELEV_CAT"
## set strata sample size
strata_n <- c("high" = 5, "low" = 5)
## could also run (useful if there are many names you don't want to type)
strata_n <- c(5, 5)
names(strata_n) <- unique(NE_Lakes[[stratum_var]])

# unequal selection probabilities

## set caty variable
caty_var <- "AREA_CAT"
## set caty sample sizes

### option 1: recycle caty vector
caty_n1 <- c("small" = 3, "large" = 2)
caty_n1

### option 2: recycle list structure
caty_n2 <- lapply(1:2, function(x) c("small" = 3, "large" = 2))
names(caty_n2) <- names(strata_n)
caty_n2

### option 3: by hand (avoid this if possible)
caty_n3 <- list(
  "high" = c("small" = 3, "large" = 2),
  "low" = c("small" = 3, "large" = 2)
)
caty_n3

## can run all three designs

### n1
samp_n1 <- spsurvey::grts(
  NE_Lakes,
  n_base = strata_n,
  stratum_var = stratum_var,
  caty_n = caty_n1,
  caty_var = caty_var
)
### look at caty_n structure
samp_n1$design$caty_n

### n2
samp_n2 <- grts(
  NE_Lakes,
  n_base = strata_n,
  stratum_var = stratum_var,
  caty_n = caty_n2,
  caty_var = caty_var
)
### look at caty_n structure
samp_n2$design$caty_n

### n3
samp_n3 <- grts(
  NE_Lakes,
  n_base = strata_n,
  stratum_var = stratum_var,
  caty_n = caty_n3,
  caty_var = caty_var
)
## look at caty_n structure
samp_n3$design$caty_n
