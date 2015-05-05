## Common Eider
load("../Data/data_coei.Rda")
coei_zerotrunc <- coei[coei$count > 0,]
coei_zero <- coei %>% mutate(count = factor(count > 0, labels = c("0", ">0")))

## Scoter
load("../Data/data_scot.Rda")
scot_zerotrunc <- scot[scot$count > 0,]
scot_zero <- scot %>% mutate(count = factor(count > 0, labels = c("0", ">0")))

## Long-tailed Duck
load("../Data/data_ltdu.Rda")
ltdu_zerotrunc <- ltdu[ltdu$count > 0,]
ltdu_zero <- ltdu %>% mutate(count = factor(count > 0, labels = c("0", ">0")))