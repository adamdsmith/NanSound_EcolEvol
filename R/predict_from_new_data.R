## Generate final prediction data sets

# Get predicted occupancy and conditional abundance
# Technically, the `rowSums` function does not alter the outcome prediction and is not 
# needed but including it puts the result in a structure that is seemlessly incorporated 
# into the data frame

# COEI
coei_pred$occ <- rowSums(predict(COEIocc, type="response", newdata = coei_pred))
coei_pred$abund <- rowSums(predict(COEIcc, parameter = "mu", type="response", newdata = coei_pred))
coei_pred$overd <- rowSums(predict(COEIcc, parameter = "sigma", type="response", newdata = coei_pred))
coei_pred$ltabund <- exp(predict.gamlssHurdle(COEIocc, COEIcc, coei_pred)$add_pred)

# SCOT
scot_pred$occ <- rowSums(predict(SCOTocc, type="response", newdata = scot_pred))
scot_pred$abund <- rowSums(predict(SCOTcc, parameter = "mu", type="response", newdata = scot_pred))
scot_pred$overd <- rowSums(predict(SCOTcc, parameter = "sigma", type="response", newdata = scot_pred))
scot_pred$ltabund <- exp(predict.gamlssHurdle(SCOTocc, SCOTcc, scot_pred)$add_pred)

# LTDU
ltdu_pred$occ <- rowSums(predict(LTDUocc, type="response", newdata = ltdu_pred))
ltdu_pred$abund <- rowSums(predict(LTDUcc, parameter = "mu", type="response", newdata = ltdu_pred))
ltdu_pred$overd <- rowSums(predict(LTDUcc, parameter = "sigma", type="response", newdata = ltdu_pred))
ltdu_pred$ltabund <- exp(predict.gamlssHurdle(LTDUocc, LTDUcc, ltdu_pred)$add_pred)

# Calculate segment level medians and MADs
# COEI
coei_final <- ddply(coei_pred, .(seg, x, y, obs_window), summarise,
                    occup = median(occ),
                    occup_mad = mad(occ),
                    c_abund = median(abund),
                    c_abund_mad = mad(abund),
                    c_overd = median(overd),
                    c_overd_mad = mad(overd),
                    lt_abund = median(ltabund),
                    lt_abund_mad = mad(ltabund))

# SCOT
scot_final <- ddply(scot_pred, .(seg, x, y, obs_window), summarise,
                    occup = median(occ),
                    occup_mad = mad(occ),
                    c_abund = median(abund),
                    c_abund_mad = mad(abund),
                    c_overd = median(overd),
                    c_overd_mad = mad(overd),
                    lt_abund = median(ltabund),
                    lt_abund_mad = mad(ltabund))

# LTDU
ltdu_final <- ddply(ltdu_pred, .(seg, x, y, obs_window), summarise,
                    occup = median(occ),
                    occup_mad = mad(occ),
                    c_abund = median(abund),
                    c_abund_mad = mad(abund),
                    c_overd = median(overd),
                    c_overd_mad = mad(overd),
                    lt_abund = median(ltabund),
                    lt_abund_mad = mad(ltabund))