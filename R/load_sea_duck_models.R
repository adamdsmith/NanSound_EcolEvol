## Load occupancy models for each species and rename to avoid repeatedly loading these models
load("C:/Users/Graduate_Student/Documents/Adam's_Stuff/NanSound/Results_coei/zero.rda")
#load("../Results_coei/zero.Rda")
COEIocc <- zero

load("C:/Users/Graduate_Student/Documents/Adam's_Stuff/NanSound/Results_scot/zero.rda")
#load("../Results_scot/zero.Rda")
SCOTocc <- zero

load("C:/Users/Graduate_Student/Documents/Adam's_Stuff/NanSound/Results_ltdu/zero.rda")
#load("../Results_ltdu/zero.Rda")
LTDUocc <- zero

## Load conditional count models for each species and rename to avoid repeatedly loading these models
load("C:/Users/Graduate_Student/Documents/Adam's_Stuff/NanSound/Results_coei/hurdle.rda")
#load("../Results_coei/hurdle.Rda")
COEIcc <- hurdle

load("C:/Users/Graduate_Student/Documents/Adam's_Stuff/NanSound/Results_scot/hurdle.rda")
#load("../Results_scot/hurdle.Rda")
SCOTcc <- hurdle

load("C:/Users/Graduate_Student/Documents/Adam's_Stuff/NanSound/Results_ltdu/hurdle.rda")
#load("../Results_ltdu/hurdle.Rda")
LTDUcc <- hurdle

rm(zero); rm(hurdle)