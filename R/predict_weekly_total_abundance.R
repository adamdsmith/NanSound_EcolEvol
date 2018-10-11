# Assumes the first two code chunkls from NanSound_main_document.Rmd have been sourced
library(ggplot2)
library(viridis)

theme_set(theme_classic(base_size = 15))
theme_update(legend.position = "top",
             legend.direction = "horizontal",
             axis.line.x = element_line(color="black"), # These two lines are workaround for ggplot2 bug
             axis.line.y = element_line(color="black"))

# Generate weekly data for the animation
source("../R/create_study_area_data.R")
dates03 <- mdy("11012003"); yday(dates03) <- 305 + seq(0, 150, 7)
dates04 <- mdy("11012004"); yday(dates04) <- 306 + seq(0, 150, 7)
dates05 <- mdy("11012005"); yday(dates05) <- 305 + seq(0, 150, 7)
pred_dates <- c(dates03, dates04, dates05)
weekly_data <- create_sa_data(pred_dates)

# Create some vectors useful for plotting dates across winters
plot_dates <- seq(mdy("11-01-2004"), mdy("03-28-2005"), by = "7 days")
label_dates <- format(plot_dates, format = "%d %b") 
label_dates[setdiff(seq_along(plot_dates), seq(1, length(plot_dates), by = 3))] <- ""
std_plot_dates <- as.integer(plot_dates - mdy("12-31-2004"))

# Load occupancy and conditional count models models for each species and rename
## COEI
load("../Results_coei/zero.Rda")
COEIocc <- zero

load("../Results_coei/hurdle.Rda")
COEIcc <- hurdle

coei_wkly <- weekly_data
coei_wkly$ltabund <- exp(predict.gamlssHurdle(COEIocc, COEIcc, weekly_data)$add_pred)
coei_plot <- coei_wkly %>% group_by(date, winter, time) %>%
    summarize(totalN = sum(ltabund)) %>%
    ungroup() %>%
    mutate(time = as.integer(time * covar_sds["time"] + covar_avgs["time"]))
coei_p <- ggplot(coei_plot, aes(x=time, y=totalN, group = winter)) +
    geom_line(aes(color = winter), lwd = 1.25) +
    geom_point(aes(fill = winter), shape = 21, size = 2.5) + 
    scale_x_continuous("Date", 
                       breaks = std_plot_dates,
                       labels = label_dates) +
    scale_y_continuous("") +
    scale_fill_manual("Winter", values = viridis(3)) +
    scale_color_manual("Winter", values = viridis(3)) +
    annotate("text", Inf, Inf, label = "COEI", hjust = 1, vjust = 1, size = 7) 

# SCOT
load("../Results_scot/zero.Rda")
SCOTocc <- zero

load("../Results_scot/hurdle.Rda")
SCOTcc <- hurdle

scot_wkly <- weekly_data
scot_wkly$ltabund <- exp(predict.gamlssHurdle(SCOTocc, SCOTcc, weekly_data)$add_pred)
scot_plot <- scot_wkly %>% group_by(date, winter, time) %>%
    summarize(totalN = sum(ltabund)) %>%
    ungroup() %>%
    mutate(time = as.integer(time * covar_sds["time"] + covar_avgs["time"]))
scot_p <- ggplot(scot_plot, aes(x=time, y=totalN, group = winter)) +
    geom_line(aes(color = winter), lwd = 1.25) +
    geom_point(aes(fill = winter), shape = 21, size = 2.5) + 
    scale_x_continuous("Date", 
                       breaks = std_plot_dates,
                       labels = label_dates) +
    scale_y_continuous("Predicted total abundance") +
    scale_fill_manual("Winter", values = viridis(3)) +
    scale_color_manual("Winter", values = viridis(3)) +
    annotate("text", Inf, Inf, label = "SCOT", hjust = 1, vjust = 1, size = 7) 

# LTDU
load("../Results_ltdu/zero.Rda")
LTDUocc <- zero

load("../Results_ltdu/hurdle.Rda")
LTDUcc <- hurdle

ltdu_wkly <- weekly_data
ltdu_wkly$ltabund <- exp(predict.gamlssHurdle(LTDUocc, LTDUcc, weekly_data)$add_pred)
ltdu_plot <- ltdu_wkly %>% group_by(date, winter, time) %>%
    summarize(totalN = sum(ltabund)) %>%
    ungroup() %>%
    mutate(time = as.integer(time * covar_sds["time"] + covar_avgs["time"]))
ltdu_p <- ggplot(ltdu_plot, aes(x=time, y=totalN, group = winter)) +
    geom_line(aes(color = winter), lwd = 1.25) +
    geom_point(aes(fill = winter), shape = 21, size = 2.5) + 
    scale_x_continuous("Date", 
                       breaks = std_plot_dates,
                       labels = label_dates) +
    scale_y_continuous("") +
    scale_fill_manual("Winter", values = viridis(3)) +
    scale_color_manual("Winter", values = viridis(3)) +
    annotate("text", Inf, Inf, label = "LTDU", hjust = 1, vjust = 1, size = 7) 

# Assemble figures
if (!requireNamespace("cowplot", quietly = TRUE))
    install.packages("cowplot", quiet = TRUE)
p <- cowplot::plot_grid(coei_p + theme(legend.position="none"), 
                        scot_p + theme(legend.position="none"), 
                        ltdu_p + theme(legend.position="none"), ncol = 1, align = 'v')
legend <- cowplot::get_legend(scot_p)

# add the legend to the row we made earlier. Give it one-third of the width
# of one plot (via rel_widths).
p <- cowplot::plot_grid(legend, p, ncol = 1, rel_heights = c(0.3, 3))

## CREATE FILE
# png(file = "./Figures/Predicted_weekly_abundance.png", height = 9, width = 6.5, units = "in", res = 600)
p
# dev.off()

