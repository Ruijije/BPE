library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(terra)
library(ggplot2)
library(viridis)
library(patchwork)
dataall<-read.csv("BPE_PFT_climate_allthree.csv")
##################################################################
#######################################plot distribution
plotdata1 <- filter(dataall, Forest_type == "deciduous" | Forest_type == "evergreen")
my_colors <- c("deciduous" = "cornflowerblue", "evergreen" = "deeppink2")
plotg_f <- ggplot(plotdata1, aes(x = f, fill = Forest_type)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.1, position = "identity", color = "black",alpha=0.7) +  # Filled histogram with borders
  geom_density(aes(color = Forest_type), fill = NA, size = 1.2) +  # Density curve without fill but with color
  ylab(expression('Density of f')) + 
  xlab(expression('f (fraction)')) + 
  scale_fill_manual(values = my_colors) +  # Consistent color for fill
  scale_color_manual(values = my_colors) +
  theme_bw() + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),
    axis.title.y=element_blank(),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text = element_text(size = rel(1.5)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) + 
  annotate("text", x = 0.5, y = 6, label = "(a)", size = 10)

plotg_f

plotTg <- ggplot(plotdata1, aes(x = Tg,fill = Forest_type)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, position = "identity", color = "black",alpha=0.7) +
  geom_density(aes(color = Forest_type), fill = NA, size = 1.2) + # Add density curve
  ylab(expression('Density of Tg')) +
  xlab(expression('Tg'*" ("*paste(degree, C)*")")) +
  scale_fill_manual(values = my_colors) +  # Consistent color for fill
  scale_color_manual(values = my_colors) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    axis.title.y=element_blank(),
    axis.text = element_text(size = rel(1.5)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  annotate("text", x = 18, y = 0.3, label = "(b)", size = 10)
plotTg

plotage <- ggplot(plotdata1, aes(x = Age,fill = Forest_type)) +
  geom_histogram(aes(y = ..density..), binwidth = 50, position = "identity", color = "black",alpha=0.7) +
  geom_density(aes(color = Forest_type), fill = NA, size = 1.2) +# Add density curve
  ylab(expression('Density of Age')) +
  xlab(expression('Age'*" (year)")) +
  scale_fill_manual(values = my_colors) +  # Consistent color for fill
  scale_color_manual(values = my_colors) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    axis.title.y=element_blank(),
    axis.text = element_text(size = rel(1.5)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  annotate("text", x = 400, y = 0.015, label = "(c)", size = 10)
plotage

plotvpd <- ggplot(plotdata1, aes(x = g_vpd,fill = Forest_type)) +
  geom_histogram(aes(y = ..density..), binwidth = 120, position = "identity", color = "black",alpha=0.7) +
  geom_density(aes(color = Forest_type), fill = NA, size = 1.2) +# Add density curve# Add density curve
  ylab(expression('Density of VPD')) +
  xlab(expression('VPD (Pa)')) +
  scale_fill_manual(values = my_colors) +  # Consistent color for fill
  scale_color_manual(values = my_colors) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    axis.title.y=element_blank(),
    axis.text = element_text(size = rel(1.5)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  annotate("text", x = 1000, y = 0.0035, label = "(d)", size = 10)
plotvpd


plotsoc <- ggplot(plotdata1, aes(x = soilCN,fill = Forest_type)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, position = "identity", color = "black",alpha=0.7) +
  geom_density(aes(color = Forest_type), fill = NA, size = 1.2) +
  ylab(expression('Density of CN')) +
  xlab(expression('CN (fraction)')) +
  scale_fill_manual(values = my_colors) +  # Consistent color for fill
  scale_color_manual(values = my_colors) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    axis.title.y=element_blank(),
    axis.text = element_text(size = rel(1.5)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  annotate("text", x = 15, y = 0.3, label = "(e)", size = 10)
plotsoc

plotBPE <- ggplot(plotdata1, aes(x = BPE,fill = Forest_type)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, position = "identity", color = "black",alpha=0.7) +
  geom_density(aes(color = Forest_type), fill = NA, size = 1.2) +
  ylab(expression('Density of BPE')) +
  xlab(expression('BPE')) +
  scale_fill_manual(values = my_colors) +  # Consistent color for fill
  scale_color_manual(values = my_colors) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.title.y=element_blank(),
    axis.text = element_text(size = rel(1.5)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  )+
  annotate("text", x = 0.5, y = 3.5, label = "(f)", size = 10)
plotBPE

plot_grid(plotg_f,plotTg,plotage,plotvpd, plotsoc,plotBPE, label_x = 0.2, nrow = 2,ncol=3)
####################################regression model
filterdata<-read.csv("/Users/echo/PhD_third/reading_list/New_data_sources/BPE_PFT_climate_allthree_climate_elevation_alldata_DE_EV.csv")##filter the non-pft
mean_bpe <- mean(filterdata$BPE, na.rm = TRUE)
sd_bpe <- sd(filterdata$BPE, na.rm = TRUE)

evergreendata<-filter(filterdata, Forest_type == "evergreen")
mean_bpe_ev <- mean(evergreendata$BPE, na.rm = TRUE)
sd_bpe_ev <- sd(evergreendata$BPE, na.rm = TRUE)
deciduousdata<-filter(filterdata, Forest_type == "deciduous")
mean_bpe_de <- mean(deciduousdata$BPE, na.rm = TRUE)
sd_bpe_de <- sd(deciduousdata$BPE, na.rm = TRUE)

model_xfactor9<-lm(logitBPE ~Tg+MTCO+lnage+lnCN+pH+sand+Forest_type, data = filterdata)
summary(model_xfactor9) ##this is the final model 
model_9evergreen <- lm(logitBPE ~Tg+MTCO+lnage+lnCN+pH+sand, data = evergreendata)
summary(model_9evergreen)
model_9deciduous <- lm(logitBPE ~Tg+MTCO+lnage+lnCN+pH+sand, data = deciduousdata)
summary(model_9deciduous)

library(visreg)
my_colors <- c("deciduous" = "cornflowerblue", "evergreen" = "deeppink2")
filterdata$rowid <- 1:nrow(filterdata)
f1t_allln<-visreg(model_xfactor9,"lnage",ylab="BPE",line=c(col="deeppink"))
# Merge based on rowid
f1t_allln$res$rowid <- as.numeric(rownames(f1t_allln$res))
f1t_allln$res <- merge(f1t_allln$res, filterdata[, c("rowid", "Forest_type")], by = "rowid")

f1t_alllnp <- ggplot(data = f1t_allln$res, aes(x = lnage, y = visregRes, color = Forest_type)) +  # <-- specify color by group here
  geom_point(alpha = 0.6) +   # points will use group colors
  geom_ribbon(data = f1t_allln$fit, aes(x = lnage, y = visregFit, ymin = visregLwr, ymax = visregUpr), inherit.aes = FALSE, alpha = 0.5) +
  geom_smooth(data = f1t_allln$fit, aes(x = lnage, y = visregFit), color = "darkorange", method = 'lm', se = TRUE, size = 2, inherit.aes = FALSE) +
  scale_color_manual(values = my_colors) +  # <-- apply your color palette
  xlab(expression('lnAge (year)')) +
  ylab(expression('logit BPE')) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 25),
    axis.title.y = element_blank(),
    axis.text = element_text(size = rel(1.5)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  annotate("text", x = 3.8, y = 3, label = "(c)", size = 10)
f1t_alllnp


# Re-run the visreg
f2t_allln <- visreg(model_xfactor9, "Tg", ylab = "BPE", line = c(col = "darkred"))

# Add rowid to visreg residuals
f2t_allln$res$rowid <- as.numeric(rownames(f2t_allln$res))

# Merge Forest_type into the visreg residuals
f2t_allln$res <- merge(f2t_allln$res, filterdata[, c("rowid", "Forest_type")], by = "rowid")

# Now plot
f2t_alllnp <- ggplot(data = f2t_allln$res, aes(x = Tg, y = visregRes, color = Forest_type)) +
  geom_point(alpha = 0.6) +
  geom_ribbon(data = f2t_allln$fit, aes(x = Tg, y = visregFit, ymin = visregLwr, ymax = visregUpr), inherit.aes = FALSE, alpha = 0.5) +
  geom_smooth(data = f2t_allln$fit, aes(x = Tg, y = visregFit), color = "darkorange", method = 'lm', se = TRUE, size = 2, inherit.aes = FALSE) +
  scale_color_manual(values = my_colors) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), limits = c(-3, 3)) +
  ylab(expression('logit BPE')) +
  xlab(expression('Tg' * " (" * paste(degree, C) * ")")) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 25),
    axis.title.y = element_blank(),
    axis.text = element_text(size = rel(1.3)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  annotate("text", x = 16, y = 3, label = "(a)", size = 10)

# Show the plot
f2t_alllnp


f22t_allln<-visreg(model_xfactor9,"MTCO",ylab="BPE",line=c(col="darkred"))
# Add rowid to visreg residuals
f22t_allln$res$rowid <- as.numeric(rownames(f22t_allln$res))

# Merge Forest_type into the visreg residuals
f22t_allln$res <- merge(f22t_allln$res, filterdata[, c("rowid", "Forest_type")], by = "rowid")

my_colors <- c("deciduous" = "cornflowerblue", "evergreen" = "deeppink2")

# Now plot
f22t_alllnp <- ggplot(data = f22t_allln$res, aes(x = MTCO, y = visregRes, color = Forest_type)) +
  geom_point(alpha = 0.6) +
  geom_ribbon(data = f22t_allln$fit, aes(x = MTCO, y = visregFit, ymin = visregLwr, ymax = visregUpr), inherit.aes = FALSE, alpha = 0.5) +
  geom_smooth(data = f22t_allln$fit, aes(x = MTCO, y = visregFit), color = "darkorange", method = 'lm', se = TRUE, size = 2, inherit.aes = FALSE) +
  scale_color_manual(values = my_colors) +
  ylab(expression('logit BPE')) +
  xlab(expression('MTCO' * " (" * paste(degree, C) * ")")) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 25),
    axis.title.y = element_blank(),
    axis.text = element_text(size = rel(1.3)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  annotate("text", x = 0, y = 3, label = "(b)", size = 10)

# Show plot
f22t_alllnp


f5t_allln <- visreg(model_xfactor9, "lnCN", ylab = "BPE", line = c(col = "deeppink"))

# Add rowid to visreg residuals
f5t_allln$res$rowid <- as.numeric(rownames(f5t_allln$res))

# Merge Forest_type into visreg residuals
f5t_allln$res <- merge(f5t_allln$res, filterdata[, c("rowid", "Forest_type")], by = "rowid")

# Set color
my_colors <- c("deciduous" = "cornflowerblue", "evergreen" = "deeppink2")

# Plot
f5t_alllnp <- ggplot(data = f5t_allln$res, aes(x = lnCN, y = visregRes, color = Forest_type)) +
  geom_point(alpha = 0.6) +
  geom_ribbon(data = f5t_allln$fit, aes(x = lnCN, y = visregFit, ymin = visregLwr, ymax = visregUpr), inherit.aes = FALSE, alpha = 0.5) +
  geom_smooth(data = f5t_allln$fit, aes(x = lnCN, y = visregFit), color = "darkorange", method = 'lm', se = TRUE, size = 2, inherit.aes = FALSE) +
  scale_color_manual(values = my_colors) +
  xlab(expression('lnC:N (fraction)')) +
  ylab(expression('BPE')) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 25),
    axis.title.y = element_blank(),
    axis.text = element_text(size = rel(1.3)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  annotate("text", x = 2.6, y = 3, label = "(d)", size = 10)

# Show plot
f5t_alllnp

# visreg for pH
f3t_allln <- visreg(model_xfactor9, "pH", ylab = "BPE", line = c(col = "darkred"))

# Add rowid to visreg residuals
f3t_allln$res$rowid <- as.numeric(rownames(f3t_allln$res))

# Merge Forest_type into visreg residuals
f3t_allln$res <- merge(f3t_allln$res, filterdata[, c("rowid", "Forest_type")], by = "rowid")

# Set colors
my_colors <- c("deciduous" = "cornflowerblue", "evergreen" = "deeppink2")

# Plot
f3t_alllnp <- ggplot(data = f3t_allln$res, aes(x = pH, y = visregRes, color = Forest_type)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = my_colors) +
  geom_ribbon(data = f3t_allln$fit, aes(x = pH, y = visregFit, ymin = visregLwr, ymax = visregUpr), inherit.aes = FALSE, alpha = 0.5) +
  geom_smooth(data = f3t_allln$fit, aes(x = pH, y = visregFit), color = "darkorange", method = 'lm', se = TRUE, size = 2, inherit.aes = FALSE) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), limits = c(-3, 3)) +
  xlab(expression('pH' * " (unitless)")) +
  ylab(expression('BPE')) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 25),
    axis.title.y = element_blank(),
    axis.text = element_text(size = rel(1.3)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  annotate("text", x = 5.7, y = 3, label = "(e)", size = 10)

# Show plot
f3t_alllnp



# visreg for sand
f6t_allln <- visreg(model_xfactor9, "sand", ylab = "BPE", line = c(col = "darkred"))

# Add rowid to visreg residuals
f6t_allln$res$rowid <- as.numeric(rownames(f6t_allln$res))

# Merge Forest_type into visreg residuals
f6t_allln$res <- merge(f6t_allln$res, filterdata[, c("rowid", "Forest_type")], by = "rowid")

# Set colors
my_colors <- c("deciduous" = "cornflowerblue", "evergreen" = "deeppink2")

# Plot
f6t_alllnp <- ggplot(data = f6t_allln$res, aes(x = sand, y = visregRes, color = Forest_type)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = my_colors) +
  geom_ribbon(data = f6t_allln$fit, aes(x = sand, y = visregFit, ymin = visregLwr, ymax = visregUpr), inherit.aes = FALSE, alpha = 0.5) +
  geom_smooth(data = f6t_allln$fit, aes(x = sand, y = visregFit), color = "darkorange", method = 'lm', se = TRUE, size = 2, inherit.aes = FALSE) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), limits = c(-3, 3)) +
  ylab(expression('BPE')) +
  xlab(expression('Sand' * " (% of weight)")) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 25),
    axis.title.y = element_blank(),
    axis.text = element_text(size = rel(1.3)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  annotate("text", x = 50, y = 3, label = "(f)", size = 10)

# Show plot
f6t_alllnp

# visreg for Forest_type
f7t_Forest_type1 <- visreg(model_xfactor9, "Forest_type", ylab = "BPE", line = c(col = "deeppink"))

# Convert factor to numeric for plotting
f7t_Forest_type1$res$x_numeric <- as.numeric(f7t_Forest_type1$res$Forest_type)
f7t_Forest_type1$fit$x_numeric <- as.numeric(f7t_Forest_type1$fit$Forest_type)

# Colors
# Define correct color mapping
my_colors <- c("deciduous" = "cornflowerblue", "evergreen" = "deeppink2")

f7t_Forest_typeplot1 <- ggplot(data = f7t_Forest_type1$res, aes(x = x_numeric, y = visregRes, color = Forest_type)) + 
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "darkorange", size = 1) +
  scale_color_manual(values = my_colors) +   # USE fixed my_colors
  scale_y_continuous(breaks = seq(-3, 3, by = 1), limits = c(-3, 3)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Evergreen","Deciduous")) +
  xlab("Forest Type") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 25),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = rel(1.3)),
    axis.text.y = element_text(size = rel(1.3)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  annotate("text", x = 1.5, y = 3, label = "(g)", size = 10)

f7t_Forest_typeplot1

plot_grid(f2t_alllnp,f22t_alllnp,f1t_alllnp,f5t_alllnp,f3t_alllnp,f6t_alllnp,f7t_Forest_typeplot1,
          ncol = 4, nrow = 2
)

#####################################global map 
evemap<-stack("Diaz_Median_CCI_we.tif")
plot(evemap)
decmap<-stack("Diaz_Median_CCI_wd.tif")
plot(decmap)

tempc<-stack("tempc12.nc")
tempc[tempc<0]<-NA
TgF <- calc(tempc, fun = mean, na.rm = TRUE)
plot(TgF)

tempmean<-stack("/Users/echo/PhD_second_year/chelsa_data/tempc12.nc")
MTCO <- calc(tempmean, fun = min, na.rm = TRUE)
plot(MTCO)

agedata<-stack("dominant_tree_age_distribution_new.nc")
plot(agedata)
lnageR<-log(agedata)
CN<-stack("CNrt.nc")
plot(CN)
lnCNR<-log(CN)

PH<-stack("PHH2O1.tif")
PH_new<-PH/10

solipath<-("/Volumes/rd619/projects/lab-prentice-realm-data/live/data/soil/soilgrids_v2")
sand <- list.files(solipath, pattern = "^sand_.*\\.tif$", full.names = TRUE)
sand_stack <- stack(sand)           # Load TIFF files into a RasterStack
sand_mean <- calc(sand_stack, fun = mean, na.rm = TRUE)           # Load TIFF files into a RasterStack
sand_mean1<-(sand_mean/1000)*100##(percent)

ref <- TgF  # choose a base raster (e.g. TgF)

# Align the rest to the reference
MTCO_res <- resample(MTCO, ref, method = "bilinear")
lnageR_res <- resample(lnageR, ref, method = "bilinear")
lnCNR_res <- resample(lnCNR, ref, method = "bilinear")
PH_new_res <- resample(PH_new, ref, method = "bilinear")
sand_mean1_res <- resample(sand_mean1, ref, method = "bilinear")


predictors <- stack(ref, MTCO_res, lnageR_res, lnCNR_res, PH_new_res, sand_mean1_res)

# Rename layers to match model terms
names(predictors) <- c("Tg", "MTCO", "lnage", "lnCN", "pH", "sand")

predicted_logitBPE_Decious <- overlay(predictors, fun = function(Tg, MTCO, lnage, lnCN, pH, sand) {
  3.355460 + 
    (-0.050215 * Tg) +
    (0.011538 * MTCO) +
    (-0.169381 * lnage) +
    (-0.532922 * lnCN) +
    (-0.147440 * pH) +
    (-0.006291 * sand) +
    0.202246 
})

BPE_deci <- exp(predicted_logitBPE_Decious) / (1 + exp(predicted_logitBPE_Decious))
writeRaster(BPE_deci, filename = "/Users/echo/PhD_third/CUE_Collalti/result_data/BPE_pre_De.tif",
            format = "GTiff", overwrite = TRUE)
predicted_logitBPE_evergreen <- overlay(predictors, fun = function(Tg, MTCO, lnage, lnCN, pH, sand) {
  3.355460 + 
    (-0.050215 * Tg) +
    (0.011538 * MTCO) +
    (-0.169381 * lnage) +
    (-0.532922 * lnCN) +
    (-0.147440 * pH) +
    (-0.006291 * sand) 
})
BPE_ever <- exp(predicted_logitBPE_evergreen) / (1 + exp(predicted_logitBPE_evergreen))
plot(BPE_ever)
writeRaster(BPE_ever, filename = "/Users/echo/PhD_third/CUE_Collalti/result_data/BPE_pre_Ev.tif",
            format = "GTiff", overwrite = TRUE)
# Make sure all rasters are aligned
evemap <- resample(evemap, BPE_ever, method = "bilinear")
decmap <- resample(decmap, BPE_ever, method = "bilinear")

# Create logical masks
ev_gt_dec <- evemap >= decmap
dec_gt_ev <- decmap > evemap  # you can use `>=` to ensure all areas are covered

# Initialize final BPE raster
BPE_final <- raster(BPE_ever)

# Assign BPE values based on dominance
BPE_final[ev_gt_dec] <- BPE_ever[ev_gt_dec]
BPE_final[dec_gt_ev] <- BPE_deci[dec_gt_ev]

# Optional: mask out NA values from both source BPEs
BPE_final[is.na(BPE_ever) & is.na(BPE_deci)] <- NA
plot(BPE_final)

############################################### Compare with trendy model

# Load each raster (replace these with your actual file paths or objects)
raster_list <- list(
  Empirical = BPE_final,
  CABLE = CABLE_POPbpe,
  CLASSIC = CLASS_CTEMbpe,
  ISAM = isambpe,
  `ISBA-CTRIP` = ISBA_CTRIPbpe,
  JSBACH = JSBACHbpe,
  `LPJ-GUESS` = LPJ_GUESSbpe,
  ORCHIDEE = ORCHIDEEbpe,
  SDGVM = SDGVMbpe
)

# Convert each raster to a long-format data.frame
df_list <- lapply(names(raster_list), function(name) {
  r <- raster_list[[name]]
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  names(df)[3] <- "BPE"
  df$Model <- name
  return(df)
})

# Combine all data.frames
bpe_all <- bind_rows(df_list)
bpe_all$Model <- factor(bpe_all$Model, levels = names(raster_list))

# Assuming bpe_all is your data frame with x, y, BPE, and Model
ggplot(bpe_all) +   
  geom_raster(aes(x = x, y = y, fill = BPE)) +   
  scale_fill_stepsn(
    limits = c(0, 1),     
    breaks = seq(0, 1, 0.1),     
    colors = viridis::plasma(11),     
    na.value = "transparent",     
    name = "BPE"   
  ) +
  facet_wrap(~ Model, scales = "free", ncol = 3) +   
  scale_x_continuous(expand = c(0, 0)) +   
  scale_y_continuous(expand = c(0, 0)) +   
  coord_quickmap() +   
  theme_minimal(base_size = 10) +   
  theme(     
    strip.text = element_text(face = "bold"),     
    legend.position = "bottom",     
    panel.grid = element_blank(),     
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),     
    axis.title = element_blank(),     
    axis.text = element_text(size = 8),     
    axis.ticks = element_line()   
  ) + 
  guides(
    fill = guide_colorbar(
      title.position = "top",  # Move title to the top of the color bar
      title.hjust = 0.5,  # Center the title
      barwidth = 20,  # Increase the width of the color bar
      barheight = 1,  # Adjust the height if needed
      label.position = "bottom",  # Move the labels to the bottom
      label.hjust = 0.5,  # Center the labels
      label.padding = unit(0.2, "lines")  # Add padding between labels and color bar
    )
  )
###################################################lm compare
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Metrics)
library(sf)

# Step 1: Create sf site points from LON and LAT
site_points <- cbind(filterdata$LON,filterdata$LAT)

# Step 2: Define model raster list
raster_list <- list(
  CABLE = CABLE_POPbpe,
  CLASSIC = CLASS_CTEMbpe,
  ISAM = isambpe,
  `ISBA-CTRIP` = ISBA_CTRIPbpe,
  JSBACH = JSBACHbpe,
  `LPJ-GUESS` = LPJ_GUESSbpe,
  ORCHIDEE = ORCHIDEEbpe,
  SDGVM = SDGVMbpe
)

for (model_name in names(raster_list)) {
  values <- raster::extract(raster_list[[model_name]], site_points)
  filterdata[[model_name]] <- values
}
# Step 4: Predict empirical BPE at sites using your linear model
filterdata$prelogitBPE <- predict(model_xfactor9, newdata = filterdata)
filterdata$Empirical <- exp(filterdata$prelogitBPE) / (1 + exp(filterdata$prelogitBPE))

library(dplyr)
library(broom)
library(Metrics)

library(dplyr)
library(tidyr)

model_names <- c("Empirical", "CABLE", "CLASSIC", "ISAM", "ISBA-CTRIP",
                 "JSBACH", "LPJ-GUESS", "ORCHIDEE", "SDGVM")

results <- lapply(model_names, function(model) {
  df <- filterdata %>% filter(!is.na(.data[[model]]), !is.na(BPE))
  
  # Create formula dynamically: BPE ~ model - 1
  formula <- as.formula(paste("BPE ~ `", model, "`", sep = ""))
  
  fit <- lm(formula, data = df)
  
  r2 <- summary(fit)$r.squared
  rmse_val <- rmse(df$BPE, df[[model]])
  
  tibble(Model = model, R2 = round(r2, 2), RMSE = round(rmse_val, 2))
}) %>% bind_rows()

library(dplyr)
library(tidyr)
plot_data <- filterdata %>%
  dplyr::select(BPE, all_of(model_names)) %>%   # Use all_of for model_names
  pivot_longer(cols = all_of(model_names), names_to = "Model", values_to = "Prediction") %>%
  filter(!is.na(Prediction), !is.na(BPE))

ggplot(plot_data, aes(x = Prediction, y = BPE)) +
  geom_point(color = "black", size = 0.7, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  facet_wrap(~ Model, ncol = 3, scales = "free") +
  geom_text(data = results, aes(x = -Inf, y = Inf,
                                label = paste0("R² = ", R2, "\nRMSE = ", RMSE, "\nSlope = ", slope)),
            hjust = -0.1, vjust = 1.2, size = 3, inherit.aes = FALSE) +
  theme_minimal(base_size = 10) +
  labs(x = "Predicted BPE", y = "Observed BPE") +
  theme(strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1))


########################add slope
model_names <- c("Empirical", "CABLE", "CLASSIC", "ISAM", "ISBA-CTRIP",
                 "JSBACH", "LPJ-GUESS", "ORCHIDEE", "SDGVM")

results <- lapply(model_names, function(model) {
  df <- filterdata %>% filter(!is.na(.data[[model]]), !is.na(BPE))
  
  # Create formula dynamically: BPE ~ model - 1
  formula <- as.formula(paste("BPE ~ `", model, "`", sep = ""))
  
  fit <- lm(formula, data = df)
  
  r2 <- summary(fit)$r.squared
  rmse_val <- rmse(df$BPE, df[[model]])
  slope <- coef(fit)[2]  # Extract the slope value from the model
  
  tibble(Model = model, R2 = round(r2, 2), RMSE = round(rmse_val, 2), Slope = round(slope, 2))
}) %>% bind_rows()

library(dplyr)
library(tidyr)
library(forcats)
plot_data <- filterdata %>%
  dplyr::select(BPE, all_of(model_names)) %>%   # Use all_of for model_names
  pivot_longer(cols = all_of(model_names), names_to = "Model", values_to = "Prediction") %>%
  filter(!is.na(Prediction), !is.na(BPE))
plot_data$Model <- fct_relevel(plot_data$Model, "Empirical", after = 0)  # Place Empirical first

ggplot(plot_data, aes(x = Prediction, y = BPE)) +
  geom_point(color = "black", size = 0.7, alpha = 0.6) +
  # Add 1:1 line (where predicted = observed)
  geom_abline(slope = 1, intercept = 0, color = "black",linetype="dashed") +
  # Add regression line for each model
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ fct_relevel(Model, "Empirical", after = 0), ncol = 3, nrow = 3, scales = "free") +  # Adjusted to 3x3 layout
  geom_text(data = results, aes(x = -Inf, y = Inf,
                                label = paste0("R² = ", R2, "\nRMSE = ", RMSE,"\nSlope = ", Slope)),
            hjust = -0.1, vjust = 1.2, size = 3, inherit.aes = FALSE) +
  theme_minimal(base_size = 10) +
  labs(x = "Predicted BPE", y = "Observed BPE") +
  theme(strip.text = element_text(face = "bold"),axis.title = element_text(size = 18),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

##########################################predict Biomass production
GPP<-stack("/Users/echo/PhD_second_year/Net carbon profit/0.05ddegree/pythonGPP0.05_sm.nc")
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
plot(GPP)
GPP<-calc(GPP,fun = mean,na.rm=TRUE)

library(terra)
library(ggplot2)
library(viridis)
library(patchwork)

# Mask GPP where BPE_final is NA
GPP_aligned <- resample(GPP, BPE_final, method = "bilinear")  # or "near" for categorical

GPP_masked <- mask(GPP_aligned, BPE_final)

# Step 2: Calculate BP = GPP * BPE_final
BP <- GPP_masked * BPE_final*365

# Step 3: Convert rasters to data frames for ggplot
GPP_aligned2<-GPP_aligned*365
df_gpp <- as.data.frame(GPP_aligned2, xy = TRUE, na.rm = FALSE)
colnames(df_gpp)[3] <- "value"
df_gpp$variable <- "GPP"

df_bpe <- as.data.frame(BPE_final, xy = TRUE, na.rm = FALSE)
colnames(df_bpe)[3] <- "value"
df_bpe$variable <- "BPE"

df_bp <- as.data.frame(BP, xy = TRUE, na.rm = FALSE)
colnames(df_bp)[3] <- "value"
df_bp$variable <- "BP"

# GPP plot
g1 <- ggplot(df_gpp) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(
    option = "viridis",
    name = NULL,
    na.value = "transparent"
  ) +
  labs(title = expression("GPP (gC " * m^{-2} * d^{-1} * ")")) +
  coord_quickmap() +
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 15),
    legend.position = "right",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 10)
  )

# BPE plot
g2 <- ggplot(df_bpe) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(
    option = "viridis",
    name = NULL,
    limits = c(0, 1),
    na.value = "transparent"
  ) +
  labs(title = "BPE") +
  coord_quickmap() +
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 15),
    legend.position = "right",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 10)
  )

# BP plot
g3 <- ggplot(df_bp) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(
    option = "viridis",
    name = NULL,
    na.value = "transparent"
  ) +
  labs(title = expression("BP (gC " * m^{-2} * d^{-1} * ")")) +
  coord_quickmap() +
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 15),
    legend.position = "right",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 10)
  )
plot_grid(
  g1, g2, g3,
  ncol = 1,nrow = 3
)

###########################lgm
library(relaimpo)
rel_imp_all <- calc.relimp(model_xfactor9, type = "lmg")
importance_df_all <- data.frame(
  Variable = names(rel_imp_all$lmg),
  Importance = rel_imp_all$lmg
)
importance_df_all$Variable <- c("Tg", "MTCO","lnAge", "lnCN","pH","Sand","Forest_type")

pall_trait <- ggplot(importance_df_all, aes(x = reorder(Variable, Importance), y = Importance)) +
  # Thin bars
  geom_bar(stat = "identity", fill = "cornflowerblue") +  # Adjust width for bar thickness
  # Add values at the end of the bars
  geom_text(aes(label = sprintf("%.2f", Importance)), 
            hjust = -0.2, size = 4, color = "black") +  # Adjust hjust for positioning
  ylim(0,0.09)+
  # Flip coordinates for horizontal bars
  coord_flip() +
  # Customize theme
  theme_minimal() +
  labs(title = "Relative Importance",
       x = "Predictor Variables",
       y = "Relative Importance (LMG)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = rel(1.2)),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))

# Display the plot
print(pall_trait)
##################################corplot and vif
corplot<-filterdata[, c("Tg","MTCO","lnage", "lnCN","pH", "sand")]
colnames(corplot)<- c("Tg", "MTCO","lnAge", "lnCN","pH","Sand")
corplot<- as.data.frame(sapply(corplot, as.numeric))
corplot<-na.omit(corplot)
sapply(corplot, class)
cormat <- round(cor(corplot),2)
head(cormat)

library(reshape2)
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri,na.rm = TRUE)
head(melted_cormat)


# Heatmap
library(ggplot2)
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "Orange", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  #ggtitle("Forest")+
  theme_minimal()+ # minimal theme
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, 
                                                            size = 12, hjust = 1),
        axis.text.y = element_text(size = 12))+
  coord_fixed()

coplot<-ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(legend.position = "right",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank())
coplot

vifgbpe<-vif(model_xfactor9)  # Check before and after adding lnRRD
vifgbpe<-data.frame(vifgbpe)

vifgbpe$row_names <-  c("Tg", "MTCO","lnAge", "lnCN","pH","Sand","Forest_type")

vifgbpeplot<-ggplot(vifgbpe, aes(x = row_names, y = vifgbpe)) +
  geom_bar(stat = "identity") +
  ylab(expression('VIF'))+
  # Rotate the X axis labels by 45 degrees
  theme_bw() +theme(axis.title=element_text(size=25),axis.text=element_text(size=rel(2)),axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x=element_blank(),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))
vifgbpeplot

###########################################################BP and GPP response
modelBP<-lm(log(BP) ~Tg+MTCO+lnage+lnCN+pH+sand+Forest_type, data = filterdata)
summary(modelBP)

modelgpp<-lm(log(GPP) ~Tg+MTCO+lnage+lnCN+pH+sand+Forest_type, data = filterdata)
summary(modelgpp)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Data for plotting (including stars based on significance)
coefficients_data <- data.frame(
  Predictor = rep(c('Tg', 'MTCO', 'lnAge', 'lnCN', 'pH', 'Sand'), 2),
  Model = rep(c('BP', 'GPP'), each = 6),
  Coeff = c(-0.0491, 0.0435, -0.0910, -0.3329, -0.1426, -0.0075,  # BP coefficients
            -0.0176, 0.0368, -0.0036, 0.0004, -0.0753, -0.0037), # GPP coefficients
  P_value = c(1.68e-09, 2e-16, 3.74e-05, 0.01271, 1.04e-06, 6.24e-07,  # BP p-values
              0.004652, 2e-16, 0.837970, 0.996978, 0.000887, 0.001952), # GPP p-values
  Significance = c('***', '***', '***', '*', '***', '***', # BP stars
                   '**', '***', '', '', '***', '**')) # GPP stars


# Add stars based on significance (based on P_value)
coefficients_data <- coefficients_data %>%
  mutate(Stars = case_when(
    P_value < 0.001 ~ "***",
    P_value < 0.01  ~ "**",
    P_value < 0.05  ~ "*",
    TRUE ~ ""
  ),
  Sign = ifelse(Coeff > 0, "Positive", "Negative"))

ggplot(coefficients_data, aes(x=Predictor, y=Coeff, fill=Model, group=Model, color=Model)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7) +
  geom_hline(yintercept = 0, linetype="solid", color="black", size=0.3) +
  geom_text(aes(y = ifelse(Coeff < 0, Coeff - 0.02, Coeff + 0.02), label = Stars, color = Model), 
            position = position_dodge(width = 0.8), size = 8, vjust = ifelse(coefficients_data$Coeff < 0, 1.5, -0.5)) +
  
  # Customize colors
  scale_fill_manual(values=c("BP"="cornflowerblue", "GPP"="darkseagreen")) +
  scale_color_manual(values=c("BP"="cornflowerblue", "GPP"="darkseagreen")) +
  labs(y="Coefficient Value", fill="Model", color="Model") +
  scale_y_continuous(labels = scales::comma) +
  ylim(-0.4,0.15)+
  theme_bw() +
  theme(
    legend.position = c(0.9, 0.1),  # Move the legend inside the plot at the bottom right
    legend.justification = c(1, 0),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(size = 25),
    axis.text = element_text(size = rel(1.5)),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) 


