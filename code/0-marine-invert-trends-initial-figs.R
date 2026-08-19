# data cleaning and generating initial figures for synthesis of trends 
# across marine invertebrate fisheries in BC

# loading packages
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(scales)

# set directory
setwd(here())

# helper: sum that returns NA (not 0) when every contributing value is NA/suppressed,
# rather than sum(..., na.rm=TRUE) which silently treats "all missing" as "zero"
sum_or_na = function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

### set up regional look-up table 

region_lookup = tibble(
  PFMA = as.character(c(1,2,3,4,5,101,102,103,104,105,106,142,
                        6,7,8,9,10,11,107,108,109,110,111,
                        25,26,27,121,123,124,125,126,127,130,
                        12,13,14,15,16,17,18,28,29,
                        19,20,21,22,23,24)),
  region = c(rep("Haida Gwaii / North Coast", 12),
             rep("Central Coast", 11),
             rep("West Coast Vancouver Island / Offshore", 10),
             rep("Strait of Georgia / NE Vancouver Island", 9),
             rep("South Coast / Juan de Fuca", 6))
)


################################################################################
#### crab 
################################################################################

# read in & clean data files
crab = as.data.frame(read_csv(here("data-generated","2026-jul-dfo-catch-data-crab.csv"), skip=20))
crab = crab[,1:9]
names(crab) = c("fishery","species","year","PFMA","subarea","vessel_count","num_traps_hauled","num_crabs_landed","weight_landed_lbs" )
crab = crab[which(crab$species=="XKG"),]

# when data are withheld due to privacy, the entry is "*" or "**"
# for ease of data handling, replace those values with NA
crab[which(crab$num_crabs_landed=="*" | crab$num_crabs_landed=="**"),]$num_crabs_landed = "NA"
crab[which(crab$weight_landed_lbs=="*" | crab$weight_landed_lbs=="**"),]$weight_landed_lbs = "NA"
# re-class columns as numeric
crab$num_crabs_landed = as.numeric(crab$num_crabs_landed)
crab$weight_landed_lbs = as.numeric(crab$weight_landed_lbs)

### --- figure 1: coast wide trends in catch and effort --- ###

coastwide_crab_catch = crab %>% 
  filter(subarea=="Total") %>% 
  group_by(year) %>% 
  summarise(coastwide_num_crabs=sum_or_na(num_crabs_landed),
            coastwide_weight_landed=sum_or_na(weight_landed_lbs),
            coastwide_num_traps_hauled=sum_or_na(num_traps_hauled),
            coastwide_vessel_count=sum_or_na(vessel_count)
  ) %>%
  complete(year = full_seq(year, 1))

png(here("figures","coastwide_trends_crab.png"),  width = 12, height = 8, units = "in", res = 300, pointsize=14)
par(mfrow = c(2, 2), mar = c(4, 5.5, 3, 1), mgp = c(4, 0.5, 0))
plot(coastwide_crab_catch$year, coastwide_crab_catch$coastwide_num_crabs,
     type = "o",lwd=2, pch = 16, col = "steelblue",
     xlab = "", ylab = "crabs landed", bty="l", las=1,
     main = "Coastwide landed catch")
plot(coastwide_crab_catch$year, coastwide_crab_catch$coastwide_weight_landed,
     type = "o",lwd=2,  pch = 16, col = "darkgreen",
     xlab = "", ylab = "weight landed (lbs)", bty="l", las=1,
     main = "Coastwide landed weight")
plot(coastwide_crab_catch$year, coastwide_crab_catch$coastwide_num_traps_hauled,
     type = "o",lwd=2,  pch = 16, col = "darkorange",bty="l", las=1,
     xlab = "year", ylab = "traps hauled",
     main = "Coastwide effort (traps hauled)")
plot(coastwide_crab_catch$year, coastwide_crab_catch$coastwide_vessel_count,
     type = "o",lwd=2,  pch = 16, col = "firebrick",bty="l", las=1,
     xlab = "year", ylab = "vessel count",
     main = "Coastwide vessel count")
dev.off()

### --- figure 2: crab trends by statistical area --- ###

### option 1: four panel plot with areas ranked highest to lowest average catch ###

# NOTE: this object was missing from the original script (referenced below but never
# defined) - added here, matching the pattern used for prawn/dive
catch_by_pfma = crab %>%
  filter(subarea == "Total") %>%
  group_by(PFMA, year) %>%
  summarise(weight_landed_lbs = sum_or_na(weight_landed_lbs), .groups = "drop") %>%
  complete(PFMA, year = full_seq(year, 1))

# rank PFMAs by average catch, split into 4 groups of 12
pfma_totals = catch_by_pfma %>%
  group_by(PFMA) %>%
  summarise(avg_weight = mean(weight_landed_lbs, na.rm = TRUE)) %>%
  arrange(desc(avg_weight)) %>%
  mutate(rank_group = ceiling(row_number() / 12))

catch_by_pfma_ranked = catch_by_pfma %>%
  left_join(pfma_totals %>% select(PFMA, rank_group), by = "PFMA")

panel_titles = c("Top 12 PFMAs (by avg. catch)", "PFMAs 13\u201324", "PFMAs 25\u201336", "PFMAs 37\u201348")

png(here("figures","crab_catch_by_pfma_ranked_groups.png"), width = 12, height = 7, units = "in", res = 300, pointsize = 14)
par(mfrow = c(2, 2), mar = c(2, 5.5, 3, 1), mgp = c(3.5, 0.7, 0))

for (i in 1:4) {
  d_group = catch_by_pfma_ranked %>% filter(rank_group == i)
  
  group_totals = pfma_totals %>% filter(rank_group == i) %>% arrange(avg_weight)
  pfmas_i = group_totals$PFMA
  cols = hcl.colors(length(pfmas_i), palette = "YlOrRd", rev = TRUE)
  
  plot(range(d_group$year), range(d_group$weight_landed_lbs, na.rm = TRUE),
       type = "n", xlab = "year", ylab = "weight landed (lbs)", bty = "l", las = 1,
       main = panel_titles[i])
  for (j in seq_along(pfmas_i)) {
    dd = d_group[d_group$PFMA == pfmas_i[j], ]
    lines(dd$year, dd$weight_landed_lbs, lwd = 1.5, col = cols[j])
    points(dd$year, dd$weight_landed_lbs, pch = 21, bg = cols[j], col = "black", cex = 0.7)
  }
  legend("topleft", legend = pfmas_i, pt.bg = cols, pch = 21, col = "black", lwd = 1,
         cex = 0.7, ncol = 2, bty = "n", title = "PFMA (low to high catch)")
}
dev.off()

### option 2: plotted by regions ###

catch_by_region_crab = crab %>%
  filter(subarea == "Total", PFMA != "0") %>%
  mutate(PFMA = as.character(PFMA)) %>%
  left_join(region_lookup, by = "PFMA") %>%
  group_by(region, year) %>%
  summarise(num_crabs = sum_or_na(num_crabs_landed),
            weight_landed = sum_or_na(weight_landed_lbs),
            num_traps_hauled = sum_or_na(num_traps_hauled),
            vessel_count = sum_or_na(vessel_count),
            .groups = "drop") %>%
  complete(region, year = full_seq(year, 1))

# fix region order & colours (based on avg. weight landed), reused across all panels
region_order = catch_by_region_crab %>%
  group_by(region) %>%
  summarise(avg_weight = mean(weight_landed, na.rm = TRUE)) %>%
  arrange(avg_weight) %>%
  pull(region)

n_extra = 3  # oversample, then drop the lightest colours
cols_full = hcl.colors(length(region_order) + n_extra, palette = "YlOrRd", rev = TRUE)
cols = cols_full[(n_extra + 1):length(cols_full)]
names(cols) = region_order

plot_region_panel = function(data, yvar, ylab, main, show_legend = FALSE, legend_pos = "topleft") {
  plot(range(data$year), range(data[[yvar]], na.rm = TRUE), ylim=c(0,1.2*max(data[[yvar]], na.rm=TRUE)),
       type = "n", xlab = "", ylab = ylab, bty = "l", las = 1, main = main)
  for (r in region_order) {
    dd = data[data$region == r, ]
    lines(dd$year, dd[[yvar]], lwd = 2, col = cols[r])
    points(dd$year, dd[[yvar]], pch = 21, bg = cols[r], col = "black", cex = 0.9)
  }
  if (show_legend) {
    legend(legend_pos, legend = region_order, pt.bg = cols, pch = 21, col = "black",
           lwd = 1, cex = 0.7, bty = "n", title = "Region (low to high avg. catch)")
  }
}

png(here("figures","crab_catch_by_region_panels.png"), width = 10, height = 8, units = "in", res = 300, pointsize = 12)
par(mfrow = c(2, 2), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))
plot_region_panel(catch_by_region_crab, "num_crabs", "crabs landed", "Regional landed catch", show_legend = TRUE)
plot_region_panel(catch_by_region_crab, "weight_landed", "weight landed (lbs)", "Regional landed weight")
plot_region_panel(catch_by_region_crab, "num_traps_hauled", "traps hauled", "Regional effort (traps hauled)")
plot_region_panel(catch_by_region_crab, "vessel_count", "vessel count", "Regional vessel count")
dev.off()

### --- figure 3: CPUE trends --- ###

### CPUE (weight landed / traps hauled) ###

# coastwide CPUE
coastwide_crab_catch = coastwide_crab_catch %>%
  mutate(cpue = coastwide_weight_landed / coastwide_num_traps_hauled)

# regional CPUE
catch_by_region_crab = catch_by_region_crab %>%
  mutate(cpue = weight_landed / num_traps_hauled)

png(here("figures","crab_cpue.png"), width = 10, height = 4.5, units = "in", res = 300, pointsize = 12)
par(mfrow = c(1, 2), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))

# panel 1: coastwide CPUE
plot(coastwide_crab_catch$year, coastwide_crab_catch$cpue,
     type = "o", lwd = 2, pch = 16, col = "steelblue",
     xlab = "", ylab = "CPUE (lbs / trap)", bty = "l", las = 1,
     main = "Coastwide CPUE")

plot_region_panel(catch_by_region_crab, "cpue", "CPUE (lbs / trap)", "Regional CPUE",
                  show_legend = TRUE, legend_pos = "topright")

dev.off()

################################################################################
#### prawn
################################################################################

# read in & clean data files
prawn = as.data.frame(read_csv(here("data-generated","2026-jul-dfo-catch-data-prawn.csv"), skip=20))
prawn = prawn[,1:10]
names(prawn) = c("fishery","species","year","PFMA","subarea","vessel_count","num_traps_hauled","prawn_landed_lbs","humpback_landed_lbs","coonstripe_landed_lbs")

# clean privacy-masked values
prawn[which(prawn$prawn_landed_lbs=="*" | prawn$prawn_landed_lbs=="**"),]$prawn_landed_lbs = "NA"
prawn$prawn_landed_lbs = as.numeric(prawn$prawn_landed_lbs)

### --- figure 1: coastwide trends in prawn catch and effort --- ###
coastwide_prawn_catch = prawn %>%
  filter(subarea=="Total") %>%
  group_by(year) %>%
  summarise(coastwide_weight_landed=sum_or_na(prawn_landed_lbs),
            coastwide_num_traps_hauled=sum_or_na(num_traps_hauled),
            coastwide_vessel_count=sum_or_na(vessel_count)
  ) %>%
  complete(year = full_seq(year, 1))

png(here("figures","prawn_coastwide_trends.png"), width = 9, height = 3, units = "in", res = 300, pointsize=14)
par(mfrow = c(1, 3), mar = c(4, 5.5, 3, 1), mgp = c(4, 0.5, 0))
plot(coastwide_prawn_catch$year, coastwide_prawn_catch$coastwide_weight_landed,
     type = "o",lwd=2,  pch = 16, col = "darkgreen",
     xlab = "year", ylab = "weight landed (lbs)", bty="l", las=1,
     main = "Coastwide landed weight")
plot(coastwide_prawn_catch$year, coastwide_prawn_catch$coastwide_num_traps_hauled,
     type = "o",lwd=2,  pch = 16, col = "darkorange",bty="l", las=1,
     xlab = "year", ylab = "traps hauled",
     main = "Coastwide effort (traps hauled)")
plot(coastwide_prawn_catch$year, coastwide_prawn_catch$coastwide_vessel_count,
     type = "o",lwd=2,  pch = 16, col = "firebrick",bty="l", las=1,
     xlab = "year", ylab = "vessel count",
     main = "Coastwide vessel count")
dev.off()

### --- figure 2: catch trends broken down by PFMA --- ###

## option 1: all the PFMAs ###

catch_by_pfma_prawn = prawn %>%
  filter(subarea == "Total") %>%
  group_by(PFMA, year) %>%
  summarise(prawn_landed_lbs = sum_or_na(prawn_landed_lbs), .groups = "drop") %>%
  complete(PFMA, year = full_seq(year, 1))

pfma_totals_prawn = catch_by_pfma_prawn %>%
  group_by(PFMA) %>%
  summarise(avg_weight = mean(prawn_landed_lbs, na.rm = TRUE)) %>%
  arrange(desc(avg_weight)) %>%
  mutate(rank_group = ceiling(row_number() / 12))

catch_by_pfma_prawn_ranked = catch_by_pfma_prawn %>%
  left_join(pfma_totals_prawn %>% select(PFMA, rank_group), by = "PFMA")

n_groups = max(pfma_totals_prawn$rank_group)
panel_titles = paste0("PFMAs ", (0:(n_groups-1))*12 + 1, "\u2013", pmin((1:n_groups)*12, nrow(pfma_totals_prawn)))
panel_titles[1] = "Top 12 PFMAs (by avg. catch)"

png(here("figures","prawn_catch_by_pfma_ranked_groups.png"), width = 10, height = 8, units = "in", res = 300, pointsize = 12)
par(mfrow = c(2, 2), mar = c(4, 5.5, 3, 1), mgp = c(3.5, 0.7, 0))

for (i in 1:n_groups) {
  d_group = catch_by_pfma_prawn_ranked %>% filter(rank_group == i)
  
  group_totals = pfma_totals_prawn %>% filter(rank_group == i) %>% arrange(avg_weight)
  pfmas_i = group_totals$PFMA
  cols = hcl.colors(length(pfmas_i), palette = "YlOrRd", rev = TRUE)
  
  plot(range(d_group$year), range(d_group$prawn_landed_lbs, na.rm = TRUE),
       type = "n", xlab = "", ylab = "weight landed (lbs)", bty = "l", las = 1,
       main = panel_titles[i])
  for (j in seq_along(pfmas_i)) {
    dd = d_group[d_group$PFMA == pfmas_i[j], ]
    lines(dd$year, dd$prawn_landed_lbs, lwd = 1.5, col = cols[j])
    points(dd$year, dd$prawn_landed_lbs, pch = 21, bg = cols[j], col = "black", cex = 0.7)
  }
  legend("topleft", legend = pfmas_i, pt.bg = cols, pch = 21, col = "black", lwd = 1,
         cex = 0.7, ncol = 2, bty = "n", title = "PFMA (low to high catch)")
}
dev.off()

### option 2: grouped into regions ###

catch_by_region_prawn = prawn %>%
  filter(subarea == "Total", PFMA != "0") %>%
  mutate(PFMA = as.character(PFMA)) %>%
  left_join(region_lookup, by = "PFMA") %>%
  group_by(region, year) %>%
  summarise(weight_landed = sum_or_na(prawn_landed_lbs),
            num_traps_hauled = sum_or_na(num_traps_hauled),
            vessel_count = sum_or_na(vessel_count),
            .groups = "drop") %>%
  complete(region, year = full_seq(year, 1))

# fix region order & colours (based on avg. weight landed), reused across all panels
region_order_p = catch_by_region_prawn %>%
  group_by(region) %>%
  summarise(avg_weight = mean(weight_landed, na.rm = TRUE)) %>%
  arrange(avg_weight) %>%
  pull(region)

n_extra = 3  # oversample, then drop the lightest colours
cols_full_p = hcl.colors(length(region_order_p) + n_extra, palette = "YlOrRd", rev = TRUE)
cols_p = cols_full_p[(n_extra + 1):length(cols_full_p)]
names(cols_p) = region_order_p

plot_region_panel_prawn = function(data, yvar, ylab, main, show_legend = FALSE) {
  plot(range(data$year), range(data[[yvar]], na.rm = TRUE), ylim=c(0,1.2*max(data[[yvar]], na.rm=TRUE)),
       type = "n", xlab = "", ylab = ylab, bty = "l", las = 1, main = main)
  for (r in region_order_p) {
    dd = data[data$region == r, ]
    lines(dd$year, dd[[yvar]], lwd = 2, col = cols_p[r])
    points(dd$year, dd[[yvar]], pch = 21, bg = cols_p[r], col = "black", cex = 0.9)
  }
  if (show_legend) {
    legend("topleft", legend = region_order_p, pt.bg = cols_p, pch = 21, col = "black",
           lwd = 1, cex = 0.7, bty = "n", title = "Region (low to high avg. catch)")
  }
}

png(here("figures","prawn_catch_by_region_panels.png"), width = 12, height = 4.5, units = "in", res = 300, pointsize = 12)
par(mfrow = c(1, 3), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))
plot_region_panel_prawn(catch_by_region_prawn, "weight_landed", "weight landed (lbs)", "Regional landed weight", show_legend = TRUE)
plot_region_panel_prawn(catch_by_region_prawn, "num_traps_hauled", "traps hauled", "Regional effort (traps hauled)")
plot_region_panel_prawn(catch_by_region_prawn, "vessel_count", "vessel count", "Regional vessel count")
dev.off()

### figure 3: CPUE trends ###
### CPUE (weight landed / traps hauled) - prawn ###

# coastwide CPUE
coastwide_prawn_catch = coastwide_prawn_catch %>%
  mutate(cpue = coastwide_weight_landed / coastwide_num_traps_hauled)

# regional CPUE
catch_by_region_prawn = catch_by_region_prawn %>%
  mutate(cpue = weight_landed / num_traps_hauled)

png(here("figures","prawn_cpue.png"), width = 10, height = 4.5, units = "in", res = 300, pointsize = 12)
par(mfrow = c(1, 2), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))

# panel 1: coastwide CPUE
plot(coastwide_prawn_catch$year, coastwide_prawn_catch$cpue,
     type = "o", lwd = 2, pch = 16, col = "steelblue",
     xlab = "", ylab = "CPUE (lbs / trap)", bty = "l", las = 1,
     main = "Coastwide CPUE")

# panel 2: regional CPUE
plot_region_panel_prawn(catch_by_region_prawn, "cpue", "CPUE (lbs / trap)", "Regional CPUE", show_legend = TRUE)

dev.off()

################################################################################
#### dive fisheries
################################################################################

# read in & clean data files
dive = as.data.frame(read_csv(here("data-generated","2026-jul-dfo-catch-data-dive.csv"), skip=25))
dive = dive[,1:8]
names(dive) = c("fishery","species","year","PFMA","subarea","vessel_count","dive_time_minutes","total_landing_lbs")

# fix inconsistent species code casing (e.g. "6bc" vs "6BC" for Red Urchin) for future use,
# even though we're grouping by fishery (not species) below
dive$species = toupper(dive$species)

# clean privacy-masked values (total landing) and NULL entries (dive time)
dive[which(dive$total_landing_lbs=="*" | dive$total_landing_lbs=="**"),]$total_landing_lbs = "NA"
dive$total_landing_lbs = as.numeric(dive$total_landing_lbs)

dive[which(dive$dive_time_minutes=="NULL"),]$dive_time_minutes = "NA"
dive$dive_time_minutes = as.numeric(dive$dive_time_minutes)

# filter to the four focal fisheries (drops Purple Urchin, Octopus by Dive, Scallop by Dive)
# "Geoduck and Horseclam" contains two species codes (84C, 76D); grouping by fishery
# rather than species keeps these combined, as intended
focal_fisheries = c("Geoduck and Horseclam", "Red Urchin", "Green Urchin", "Sea Cucumber")
dive = dive %>% filter(fishery %in% focal_fisheries)

# generic regional panel plotting function, reused across every fishery in the loop
plot_region_panel_generic = function(data, yvar, ylab, main, region_order, cols,
                                     show_legend = FALSE, legend_pos = "topleft") {
  plot(range(data$year), range(data[[yvar]], na.rm = TRUE),
       ylim = c(0, 1.2 * max(data[[yvar]], na.rm = TRUE)),
       type = "n", xlab = "", ylab = ylab, bty = "l", las = 1, main = main)
  for (r in region_order) {
    dd = data[data$region == r, ]
    lines(dd$year, dd[[yvar]], lwd = 2, col = cols[r])
    points(dd$year, dd[[yvar]], pch = 21, bg = cols[r], col = "black", cex = 0.9)
  }
  if (show_legend) {
    legend(legend_pos, legend = region_order, pt.bg = cols, pch = 21, col = "black",
           lwd = 1, cex = 0.7, bty = "n", title = "Region (low to high avg. catch)")
  }
}

for (f in focal_fisheries) {
  
  fishery_slug = tolower(gsub(" ", "_", f))
  d_species = dive %>% filter(fishery == f)
  
  ### --- coastwide trends --- ###
  coastwide = d_species %>%
    filter(subarea == "Total") %>%
    group_by(year) %>%
    summarise(coastwide_weight_landed = sum_or_na(total_landing_lbs),
              coastwide_dive_time = sum_or_na(dive_time_minutes),
              coastwide_vessel_count = sum_or_na(vessel_count)) %>%
    complete(year = full_seq(year, 1))
  
  png(here("figures", paste0("dive_", fishery_slug, "_coastwide_trends.png")),
      width = 9, height = 3, units = "in", res = 300, pointsize = 14)
  par(mfrow = c(1, 3), mar = c(4, 5.5, 3, 1), mgp = c(4, 0.5, 0))
  plot(coastwide$year, coastwide$coastwide_weight_landed,
       type = "o", lwd = 2, pch = 16, col = "darkgreen",
       xlab = "year", ylab = "weight landed (lbs)", bty = "l", las = 1,
       main = "Coastwide landed weight")
  plot(coastwide$year, coastwide$coastwide_dive_time,
       type = "o", lwd = 2, pch = 16, col = "darkorange", bty = "l", las = 1,
       xlab = "year", ylab = "dive time (minutes)",
       main = "Coastwide effort (dive time)")
  plot(coastwide$year, coastwide$coastwide_vessel_count,
       type = "o", lwd = 2, pch = 16, col = "firebrick", bty = "l", las = 1,
       xlab = "year", ylab = "vessel count",
       main = "Coastwide vessel count")
  dev.off()
  
  ### --- catch by PFMA (all areas, ranked groups of 12) --- ###
  catch_by_pfma = d_species %>%
    filter(subarea == "Total") %>%
    group_by(PFMA, year) %>%
    summarise(total_landing_lbs = sum_or_na(total_landing_lbs), .groups = "drop") %>%
    complete(PFMA, year = full_seq(year, 1))
  
  pfma_totals = catch_by_pfma %>%
    group_by(PFMA) %>%
    summarise(avg_weight = mean(total_landing_lbs, na.rm = TRUE)) %>%
    arrange(desc(avg_weight)) %>%
    mutate(rank_group = ceiling(row_number() / 12))
  
  catch_by_pfma_ranked = catch_by_pfma %>%
    left_join(pfma_totals %>% select(PFMA, rank_group), by = "PFMA")
  
  n_groups = max(pfma_totals$rank_group)
  ncol_panels = ceiling(sqrt(n_groups))
  nrow_panels = ceiling(n_groups / ncol_panels)
  panel_titles = paste0("PFMAs ", (0:(n_groups-1))*12 + 1, "\u2013", pmin((1:n_groups)*12, nrow(pfma_totals)))
  panel_titles[1] = "Top 12 PFMAs (by avg. catch)"
  
  png(here("figures", paste0("dive_", fishery_slug, "_catch_by_pfma_ranked_groups.png")),
      width = 5 * ncol_panels, height = 4 * nrow_panels, units = "in", res = 300, pointsize = 12)
  par(mfrow = c(nrow_panels, ncol_panels), mar = c(4, 5.5, 3, 1), mgp = c(3.5, 0.7, 0))
  for (i in 1:n_groups) {
    d_group = catch_by_pfma_ranked %>% filter(rank_group == i)
    group_totals = pfma_totals %>% filter(rank_group == i) %>% arrange(avg_weight)
    pfmas_i = group_totals$PFMA
    cols_pfma = hcl.colors(length(pfmas_i), palette = "YlOrRd", rev = TRUE)
    
    plot(range(d_group$year), range(d_group$total_landing_lbs, na.rm = TRUE),
         type = "n", xlab = "", ylab = "weight landed (lbs)", bty = "l", las = 1,
         main = panel_titles[i])
    for (j in seq_along(pfmas_i)) {
      dd = d_group[d_group$PFMA == pfmas_i[j], ]
      lines(dd$year, dd$total_landing_lbs, lwd = 1.5, col = cols_pfma[j])
      points(dd$year, dd$total_landing_lbs, pch = 21, bg = cols_pfma[j], col = "black", cex = 0.7)
    }
    legend("topleft", legend = pfmas_i, pt.bg = cols_pfma, pch = 21, col = "black", lwd = 1,
           cex = 0.7, ncol = 2, bty = "n", title = "PFMA (low to high catch)")
  }
  dev.off()
  
  ### --- catch by region --- ###
  catch_by_region = d_species %>%
    filter(subarea == "Total", PFMA != "0") %>%
    mutate(PFMA = as.character(PFMA)) %>%
    left_join(region_lookup, by = "PFMA") %>%
    group_by(region, year) %>%
    summarise(weight_landed = sum_or_na(total_landing_lbs),
              dive_time = sum_or_na(dive_time_minutes),
              vessel_count = sum_or_na(vessel_count),
              .groups = "drop") %>%
    complete(region, year = full_seq(year, 1))
  
  region_order = catch_by_region %>%
    group_by(region) %>%
    summarise(avg_weight = mean(weight_landed, na.rm = TRUE)) %>%
    arrange(avg_weight) %>%
    pull(region)
  
  n_extra = 3
  cols_full = hcl.colors(length(region_order) + n_extra, palette = "YlOrRd", rev = TRUE)
  cols = cols_full[(n_extra + 1):length(cols_full)]
  names(cols) = region_order
  
  png(here("figures", paste0("dive_", fishery_slug, "_catch_by_region_panels.png")),
      width = 12, height = 4.5, units = "in", res = 300, pointsize = 12)
  par(mfrow = c(1, 3), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))
  plot_region_panel_generic(catch_by_region, "weight_landed", "weight landed (lbs)",
                            "Regional landed weight", region_order, cols, show_legend = TRUE)
  plot_region_panel_generic(catch_by_region, "dive_time", "dive time (minutes)",
                            "Regional effort (dive time)", region_order, cols)
  plot_region_panel_generic(catch_by_region, "vessel_count", "vessel count",
                            "Regional vessel count", region_order, cols)
  dev.off()
  
  ### --- CPUE --- ###
  coastwide = coastwide %>% mutate(cpue = coastwide_weight_landed / coastwide_dive_time)
  catch_by_region = catch_by_region %>% mutate(cpue = weight_landed / dive_time)
  
  png(here("figures", paste0("dive_", fishery_slug, "_cpue.png")),
      width = 10, height = 4.5, units = "in", res = 300, pointsize = 12)
  par(mfrow = c(1, 2), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))
  plot(coastwide$year, coastwide$cpue,
       type = "o", lwd = 2, pch = 16, col = "steelblue",
       xlab = "", ylab = "CPUE (lbs / minute dive time)", bty = "l", las = 1,
       main = "Coastwide CPUE")
  plot_region_panel_generic(catch_by_region, "cpue", "CPUE (lbs / minute dive time)",
                            "Regional CPUE", region_order, cols, show_legend = TRUE)
  dev.off()
}