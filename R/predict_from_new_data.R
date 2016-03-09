## Generate final prediction data sets

# Get predicted occupancy and conditional abundance
# Technically, the `rowSums` function does not alter the outcome prediction and is not 
# needed but including it puts the result in a structure that is seemlessly incorporated 
# into the data frame

# COEI
## Load occupancy and conditional count models models for each species and rename
load("../Results_coei/zero.Rda")
COEIocc <- zero

load("../Results_coei/hurdle.Rda")
COEIcc <- hurdle

coei_pred$occ <- rowSums(predict(COEIocc, type="response", newdata = coei_pred))
coei_pred$abund <- rowSums(predict(COEIcc, parameter = "mu", type="response", newdata = coei_pred))
coei_pred$overd <- rowSums(predict(COEIcc, parameter = "sigma", type="response", newdata = coei_pred))
coei_pred$ltabund <- exp(predict.gamlssHurdle(COEIocc, COEIcc, coei_pred)$add_pred)

rm("zero", "hurdle", "COEIocc", "COEIcc")
gc(reset = TRUE)

# SCOT
load("../Results_scot/zero.Rda")
SCOTocc <- zero

load("../Results_scot/hurdle.Rda")
SCOTcc <- hurdle

scot_pred$occ <- rowSums(predict(SCOTocc, type="response", newdata = scot_pred))
scot_pred$abund <- rowSums(predict(SCOTcc, parameter = "mu", type="response", newdata = scot_pred))
scot_pred$overd <- rowSums(predict(SCOTcc, parameter = "sigma", type="response", newdata = scot_pred))
scot_pred$ltabund <- exp(predict.gamlssHurdle(SCOTocc, SCOTcc, scot_pred)$add_pred)

rm("zero", "hurdle", "SCOTocc", "SCOTcc")
gc(reset = TRUE)


# LTDU
load("../Results_ltdu/zero.Rda")
LTDUocc <- zero

load("../Results_ltdu/hurdle.Rda")
LTDUcc <- hurdle

ltdu_pred$occ <- rowSums(predict(LTDUocc, type="response", newdata = ltdu_pred))
ltdu_pred$abund <- rowSums(predict(LTDUcc, parameter = "mu", type="response", newdata = ltdu_pred))
ltdu_pred$overd <- rowSums(predict(LTDUcc, parameter = "sigma", type="response", newdata = ltdu_pred))
ltdu_pred$ltabund <- exp(predict.gamlssHurdle(LTDUocc, LTDUcc, ltdu_pred)$add_pred)

rm("zero", "hurdle", "LTDUocc", "LTDUcc")
gc(reset = TRUE)


# Calculate segment level medians and MADs
# COEI
coei_final <- ddply(coei_pred, .(seg, x, y, obs_window), summarise,
                    occup = median(occ),
                    occup_mad_m = mad(occ)/median(occ) * 100,
                    c_abund = median(abund),
                    c_abund_mad_m = mad(abund)/median(abund) * 100,
                    c_overd = median(overd),
                    c_overd_mad_m = mad(overd)/median(overd) * 100,
                    lt_abund = median(ltabund),
                    lt_abund_mad_m = mad(ltabund)/median(ltabund) * 100)

# SCOT
scot_final <- ddply(scot_pred, .(seg, x, y, obs_window), summarise,
                    occup = median(occ),
                    occup_mad_m = mad(occ)/median(occ) * 100,
                    c_abund = median(abund),
                    c_abund_mad_m = mad(abund)/median(abund) * 100,
                    c_overd = median(overd),
                    c_overd_mad_m = mad(overd)/median(overd) * 100,
                    lt_abund = median(ltabund),
                    lt_abund_mad_m = mad(ltabund)/median(ltabund) * 100)

# LTDU
ltdu_final <- ddply(ltdu_pred, .(seg, x, y, obs_window), summarise,
                    occup = median(occ),
                    occup_mad_m = mad(occ)/median(occ) * 100,
                    c_abund = median(abund),
                    c_abund_mad_m = mad(abund)/median(abund) * 100,
                    c_overd = median(overd),
                    c_overd_mad_m = mad(overd)/median(overd) * 100,
                    lt_abund = median(ltabund),
                    lt_abund_mad_m = mad(ltabund)/median(ltabund) * 100)