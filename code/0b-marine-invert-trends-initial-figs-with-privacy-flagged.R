# data cleaning and generating initial figures for synthesis of trends 
# across marine invertebrate fisheries in BC

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(scales)

setwd(here())

# sum that returns NA (not 0) when every contributing value is suppressed
sum_or_na = function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

# classifies a group of contributing values as:
#  "complete"   - nothing suppressed
#  "partial"    - some suppressed, some known (sum is a floor, not the true total)
#  "suppressed" - everything suppressed (no usable number at all)
status_flag = function(x) {
  if (all(is.na(x))) return("suppressed")
  if (any(is.na(x))) return("partial")
  "complete"
}

# draws a trend line with three-state suppression styling:
#  filled point  = complete data
#  hollow point  = partial suppression (value shown is an undercount)
#  small "x" tick near the bottom of the panel = fully suppressed year (no value plotted)
draw_status_line = function(x, y, status, col) {
  lines(x, y, lwd = 2, col = col)
  ok = !is.na(y)
  points(x[ok & status == "complete"], y[ok & status == "complete"],
         pch = 21, bg = col, col = "black", cex = 0.9)
  points(x[ok & status == "partial"], y[ok & status == "partial"],
         pch = 21, bg = "white", col = col, cex = 0.9, lwd = 1.5)
  suppressed_idx = !is.na(status) & status == "suppressed"
  if (any(suppressed_idx)) {
    usr = par("usr")
    tick_y = usr[3] + 0.03 * (usr[4] - usr[3])
    points(x[suppressed_idx], rep(tick_y, sum(suppressed_idx)), pch = 4, col = col, cex = 0.7)
  }
}

# adds a small legend explaining the point/tick styles; call once per figure
add_status_legend = function(pos = "topright") {
  legend(pos, legend = c("Complete", "Partial suppression", "Fully suppressed"),
         pch = c(16, 21, 4), pt.bg = c(NA, "white", NA), col = "black",
         bty = "n", cex = 0.6)
}

# range() with extra headroom on top, so a top-right status legend doesn't
# overlap the highest data points
buffered_range = function(y, buffer_frac = 0.18) {
  rng = range(y, na.rm = TRUE)
  span = diff(rng)
  if (span == 0) span = abs(rng[1]) + 1
  c(rng[1], rng[2] + buffer_frac * span)
}

# ranks groups (regions or PFMAs) by average value, low to high, WITHOUT letting
# groups with no data (NaN average, e.g. a species never fished in that region)
# get sorted to the "high" end just because dplyr::arrange() always puts NA/NaN
# last regardless of direction. Groups with no data are forced to sort first
# (treated as lowest), so they get the palest color and never look top-ranked.
rank_groups_low_to_high = function(data, group_col, value_col) {
  data %>%
    group_by(.data[[group_col]]) %>%
    summarise(avg_value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(avg_value_sort = ifelse(is.nan(avg_value) | is.na(avg_value), -Inf, avg_value)) %>%
    arrange(avg_value_sort)
}

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

crab = as.data.frame(read_csv(here("data-generated","2026-jul-dfo-catch-data-crab.csv"), skip=20))
crab = crab[,1:9]
names(crab) = c("fishery","species","year","PFMA","subarea","vessel_count","num_traps_hauled","num_crabs_landed","weight_landed_lbs" )
crab = crab[which(crab$species=="XKG"),]

crab[which(crab$num_crabs_landed=="*" | crab$num_crabs_landed=="**"),]$num_crabs_landed = "NA"
crab[which(crab$weight_landed_lbs=="*" | crab$weight_landed_lbs=="**"),]$weight_landed_lbs = "NA"
crab$num_crabs_landed = as.numeric(crab$num_crabs_landed)
crab$weight_landed_lbs = as.numeric(crab$weight_landed_lbs)

### --- figure 1: coastwide trends --- ###
coastwide_crab_catch = crab %>%
  filter(subarea=="Total") %>%
  group_by(year) %>%
  summarise(coastwide_num_crabs = sum_or_na(num_crabs_landed),
            num_crabs_status = status_flag(num_crabs_landed),
            coastwide_weight_landed = sum_or_na(weight_landed_lbs),
            weight_landed_status = status_flag(weight_landed_lbs),
            coastwide_num_traps_hauled = sum_or_na(num_traps_hauled),
            num_traps_hauled_status = status_flag(num_traps_hauled),
            coastwide_vessel_count = sum_or_na(vessel_count),
            vessel_count_status = status_flag(vessel_count)
  ) %>%
  complete(year = full_seq(year, 1))

png(here("figures","coastwide_trends_crab.png"),  width = 12, height = 8, units = "in", res = 300, pointsize=14)
par(mfrow = c(2, 2), mar = c(4, 5.5, 3, 1), mgp = c(4, 0.5, 0))

plot(range(coastwide_crab_catch$year), buffered_range(coastwide_crab_catch$coastwide_num_crabs),
     type = "n", xlab = "", ylab = "crabs landed", bty="l", las=1, main = "Coastwide landed catch")
draw_status_line(coastwide_crab_catch$year, coastwide_crab_catch$coastwide_num_crabs, coastwide_crab_catch$num_crabs_status, "steelblue")
add_status_legend()

plot(range(coastwide_crab_catch$year), buffered_range(coastwide_crab_catch$coastwide_weight_landed),
     type = "n", xlab = "", ylab = "weight landed (lbs)", bty="l", las=1, main = "Coastwide landed weight")
draw_status_line(coastwide_crab_catch$year, coastwide_crab_catch$coastwide_weight_landed, coastwide_crab_catch$weight_landed_status, "darkgreen")

plot(range(coastwide_crab_catch$year), buffered_range(coastwide_crab_catch$coastwide_num_traps_hauled),
     type = "n", xlab = "year", ylab = "traps hauled", bty="l", las=1, main = "Coastwide effort (traps hauled)")
draw_status_line(coastwide_crab_catch$year, coastwide_crab_catch$coastwide_num_traps_hauled, coastwide_crab_catch$num_traps_hauled_status, "darkorange")

plot(range(coastwide_crab_catch$year), buffered_range(coastwide_crab_catch$coastwide_vessel_count),
     type = "n", xlab = "year", ylab = "vessel count", bty="l", las=1, main = "Coastwide vessel count")
draw_status_line(coastwide_crab_catch$year, coastwide_crab_catch$coastwide_vessel_count, coastwide_crab_catch$vessel_count_status, "firebrick")

dev.off()

### --- figure 2 option 1: PFMA ranked groups --- ###
catch_by_pfma = crab %>%
  filter(subarea == "Total") %>%
  group_by(PFMA, year) %>%
  summarise(weight_landed_lbs = sum_or_na(weight_landed_lbs),
            weight_landed_status = status_flag(weight_landed_lbs),
            .groups = "drop") %>%
  complete(PFMA, year = full_seq(year, 1))

pfma_totals = catch_by_pfma %>%
  group_by(PFMA) %>%
  summarise(avg_weight = mean(weight_landed_lbs, na.rm = TRUE)) %>%
  arrange(desc(avg_weight)) %>%
  mutate(rank_group = ceiling(row_number() / 12))

catch_by_pfma_ranked = catch_by_pfma %>%
  left_join(pfma_totals %>% select(PFMA, rank_group), by = "PFMA")

panel_titles = c("Top 12 PFMAs (by avg. catch)", "PFMAs 13\u201324", "PFMAs 25\u201336", "PFMAs 37\u201348")

png(here("figures","crab_catch_by_pfma_ranked_groups.png"), width = 12, height = 7, units = "in", res = 300, pointsize = 14)
par(mfrow = c(2, 2), mar = c(4, 6.5, 3, 1), mgp = c(4.2, 0.7, 0))
for (i in 1:4) {
  d_group = catch_by_pfma_ranked %>% filter(rank_group == i)
  group_totals = pfma_totals %>% filter(rank_group == i) %>% arrange(avg_weight)
  pfmas_i = group_totals$PFMA
  
  if (all(is.na(d_group$weight_landed_lbs))) {
    plot.new(); title(main = panel_titles[i])
    text(0.5, 0.5, "No data available\n(fully suppressed)", cex = 1)
    next
  }
  
  cols = hcl.colors(length(pfmas_i), palette = "YlOrRd", rev = TRUE)
  plot(range(d_group$year), buffered_range(d_group$weight_landed_lbs),
       type = "n", xlab = "", ylab = "weight landed (lbs)", bty = "l", las = 1, main = panel_titles[i])
  for (j in seq_along(pfmas_i)) {
    dd = d_group[d_group$PFMA == pfmas_i[j], ]
    if (all(is.na(dd$weight_landed_lbs))) next
    draw_status_line(dd$year, dd$weight_landed_lbs, dd$weight_landed_status, cols[j])
  }
  legend("topleft", legend = pfmas_i, pt.bg = cols, pch = 21, col = "black", lwd = 1,
         cex = 0.6, ncol = 2, bty = "n", title = "PFMA (low to high catch)")
  if (i == 1) add_status_legend()
}
dev.off()

### --- figure 2 option 2: regions --- ###
catch_by_region_crab = crab %>%
  filter(subarea == "Total", PFMA != "0") %>%
  mutate(PFMA = as.character(PFMA)) %>%
  left_join(region_lookup, by = "PFMA") %>%
  group_by(region, year) %>%
  summarise(num_crabs = sum_or_na(num_crabs_landed),
            num_crabs_status = status_flag(num_crabs_landed),
            weight_landed = sum_or_na(weight_landed_lbs),
            weight_landed_status = status_flag(weight_landed_lbs),
            num_traps_hauled = sum_or_na(num_traps_hauled),
            num_traps_hauled_status = status_flag(num_traps_hauled),
            vessel_count = sum_or_na(vessel_count),
            vessel_count_status = status_flag(vessel_count),
            .groups = "drop") %>%
  complete(region, year = full_seq(year, 1))

region_totals_crab = rank_groups_low_to_high(catch_by_region_crab, "region", "weight_landed")
region_order = region_totals_crab$region

n_extra = 3
cols_full = hcl.colors(length(region_order) + n_extra, palette = "YlOrRd", rev = TRUE)
cols = cols_full[(n_extra + 1):length(cols_full)]
names(cols) = region_order

plot_region_panel = function(data, yvar, status_col, ylab, main, show_legend = FALSE,
                             legend_pos = "topleft", show_status_legend = FALSE) {
  if (all(is.na(data[[yvar]]))) {
    plot.new(); title(main = main)
    text(0.5, 0.5, "No data available\n(fully suppressed)", cex = 1)
    return(invisible(NULL))
  }
  plot(range(data$year), range(data[[yvar]], na.rm = TRUE), ylim = c(0, 1.2 * max(data[[yvar]], na.rm = TRUE)),
       type = "n", xlab = "", ylab = ylab, bty = "l", las = 1, main = main)
  for (r in region_order) {
    dd = data[data$region == r, ]
    if (all(is.na(dd[[yvar]]))) next
    draw_status_line(dd$year, dd[[yvar]], dd[[status_col]], cols[r])
  }
  if (show_legend) {
    legend(legend_pos, legend = region_order, pt.bg = cols, pch = 21, col = "black",
           lwd = 1, cex = 0.7, bty = "n", title = "Region (low to high avg. catch)")
  }
  if (show_status_legend) add_status_legend()
}

png(here("figures","crab_catch_by_region_panels.png"), width = 10, height = 8, units = "in", res = 300, pointsize = 12)
par(mfrow = c(2, 2), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))
plot_region_panel(catch_by_region_crab, "num_crabs", "num_crabs_status", "crabs landed", "Regional landed catch", show_legend = TRUE, show_status_legend = TRUE)
plot_region_panel(catch_by_region_crab, "weight_landed", "weight_landed_status", "weight landed (lbs)", "Regional landed weight")
plot_region_panel(catch_by_region_crab, "num_traps_hauled", "num_traps_hauled_status", "traps hauled", "Regional effort (traps hauled)")
plot_region_panel(catch_by_region_crab, "vessel_count", "vessel_count_status", "vessel count", "Regional vessel count")
dev.off()

### --- figure 3: CPUE --- ###
coastwide_crab_catch = coastwide_crab_catch %>%
  mutate(cpue = coastwide_weight_landed / coastwide_num_traps_hauled,
         cpue_status = case_when(
           is.na(cpue) ~ "suppressed",
           weight_landed_status == "partial" | num_traps_hauled_status == "partial" ~ "partial",
           TRUE ~ "complete"
         ))

catch_by_region_crab = catch_by_region_crab %>%
  mutate(cpue = weight_landed / num_traps_hauled,
         cpue_status = case_when(
           is.na(cpue) ~ "suppressed",
           weight_landed_status == "partial" | num_traps_hauled_status == "partial" ~ "partial",
           TRUE ~ "complete"
         ))

png(here("figures","crab_cpue.png"), width = 10, height = 4.5, units = "in", res = 300, pointsize = 12)
par(mfrow = c(1, 2), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))

plot(range(coastwide_crab_catch$year), buffered_range(coastwide_crab_catch$cpue),
     type = "n", xlab = "", ylab = "CPUE (lbs / trap)", bty = "l", las = 1, main = "Coastwide CPUE")
draw_status_line(coastwide_crab_catch$year, coastwide_crab_catch$cpue, coastwide_crab_catch$cpue_status, "steelblue")
add_status_legend()

plot_region_panel(catch_by_region_crab, "cpue", "cpue_status", "CPUE (lbs / trap)", "Regional CPUE",
                  show_legend = TRUE, legend_pos = "topright")
dev.off()

################################################################################
#### prawn
################################################################################

prawn = as.data.frame(read_csv(here("data-generated","2026-jul-dfo-catch-data-prawn.csv"), skip=20))
prawn = prawn[,1:10]
names(prawn) = c("fishery","species","year","PFMA","subarea","vessel_count","num_traps_hauled","prawn_landed_lbs","humpback_landed_lbs","coonstripe_landed_lbs")

prawn[which(prawn$prawn_landed_lbs=="*" | prawn$prawn_landed_lbs=="**"),]$prawn_landed_lbs = "NA"
prawn$prawn_landed_lbs = as.numeric(prawn$prawn_landed_lbs)

### --- figure 1: coastwide trends --- ###
coastwide_prawn_catch = prawn %>%
  filter(subarea=="Total") %>%
  group_by(year) %>%
  summarise(coastwide_weight_landed = sum_or_na(prawn_landed_lbs),
            weight_landed_status = status_flag(prawn_landed_lbs),
            coastwide_num_traps_hauled = sum_or_na(num_traps_hauled),
            num_traps_hauled_status = status_flag(num_traps_hauled),
            coastwide_vessel_count = sum_or_na(vessel_count),
            vessel_count_status = status_flag(vessel_count)
  ) %>%
  complete(year = full_seq(year, 1))

png(here("figures","prawn_coastwide_trends.png"), width = 9, height = 3, units = "in", res = 300, pointsize=14)
par(mfrow = c(1, 3), mar = c(4, 5.5, 3, 1), mgp = c(4, 0.5, 0))

plot(range(coastwide_prawn_catch$year), buffered_range(coastwide_prawn_catch$coastwide_weight_landed),
     type = "n", xlab = "year", ylab = "weight landed (lbs)", bty="l", las=1, main = "Coastwide landed weight")
draw_status_line(coastwide_prawn_catch$year, coastwide_prawn_catch$coastwide_weight_landed, coastwide_prawn_catch$weight_landed_status, "darkgreen")
add_status_legend()

plot(range(coastwide_prawn_catch$year), buffered_range(coastwide_prawn_catch$coastwide_num_traps_hauled),
     type = "n", xlab = "year", ylab = "traps hauled", bty="l", las=1, main = "Coastwide effort (traps hauled)")
draw_status_line(coastwide_prawn_catch$year, coastwide_prawn_catch$coastwide_num_traps_hauled, coastwide_prawn_catch$num_traps_hauled_status, "darkorange")

plot(range(coastwide_prawn_catch$year), buffered_range(coastwide_prawn_catch$coastwide_vessel_count),
     type = "n", xlab = "year", ylab = "vessel count", bty="l", las=1, main = "Coastwide vessel count")
draw_status_line(coastwide_prawn_catch$year, coastwide_prawn_catch$coastwide_vessel_count, coastwide_prawn_catch$vessel_count_status, "firebrick")

dev.off()

### --- figure 2 option 1: PFMA ranked groups --- ###
catch_by_pfma_prawn = prawn %>%
  filter(subarea == "Total") %>%
  group_by(PFMA, year) %>%
  summarise(prawn_landed_lbs = sum_or_na(prawn_landed_lbs),
            weight_landed_status = status_flag(prawn_landed_lbs),
            .groups = "drop") %>%
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
par(mfrow = c(2, 2), mar = c(4, 6.5, 3, 1), mgp = c(4.2, 0.7, 0))
for (i in 1:n_groups) {
  d_group = catch_by_pfma_prawn_ranked %>% filter(rank_group == i)
  group_totals = pfma_totals_prawn %>% filter(rank_group == i) %>% arrange(avg_weight)
  pfmas_i = group_totals$PFMA
  
  if (all(is.na(d_group$prawn_landed_lbs))) {
    plot.new(); title(main = panel_titles[i])
    text(0.5, 0.5, "No data available\n(fully suppressed)", cex = 1)
    next
  }
  
  cols = hcl.colors(length(pfmas_i), palette = "YlOrRd", rev = TRUE)
  plot(range(d_group$year), buffered_range(d_group$prawn_landed_lbs),
       type = "n", xlab = "", ylab = "weight landed (lbs)", bty = "l", las = 1, main = panel_titles[i])
  for (j in seq_along(pfmas_i)) {
    dd = d_group[d_group$PFMA == pfmas_i[j], ]
    if (all(is.na(dd$prawn_landed_lbs))) next
    draw_status_line(dd$year, dd$prawn_landed_lbs, dd$weight_landed_status, cols[j])
  }
  legend("topleft", legend = pfmas_i, pt.bg = cols, pch = 21, col = "black", lwd = 1,
         cex = 0.6, ncol = 2, bty = "n", title = "PFMA (low to high catch)")
  if (i == 1) add_status_legend()
}
dev.off()

### --- figure 2 option 2: regions --- ###
catch_by_region_prawn = prawn %>%
  filter(subarea == "Total", PFMA != "0") %>%
  mutate(PFMA = as.character(PFMA)) %>%
  left_join(region_lookup, by = "PFMA") %>%
  group_by(region, year) %>%
  summarise(weight_landed = sum_or_na(prawn_landed_lbs),
            weight_landed_status = status_flag(prawn_landed_lbs),
            num_traps_hauled = sum_or_na(num_traps_hauled),
            num_traps_hauled_status = status_flag(num_traps_hauled),
            vessel_count = sum_or_na(vessel_count),
            vessel_count_status = status_flag(vessel_count),
            .groups = "drop") %>%
  complete(region, year = full_seq(year, 1))

region_totals_prawn = rank_groups_low_to_high(catch_by_region_prawn, "region", "weight_landed")
region_order_p = region_totals_prawn$region

n_extra = 3
cols_full_p = hcl.colors(length(region_order_p) + n_extra, palette = "YlOrRd", rev = TRUE)
cols_p = cols_full_p[(n_extra + 1):length(cols_full_p)]
names(cols_p) = region_order_p

plot_region_panel_prawn = function(data, yvar, status_col, ylab, main, show_legend = FALSE, show_status_legend = FALSE) {
  if (all(is.na(data[[yvar]]))) {
    plot.new(); title(main = main)
    text(0.5, 0.5, "No data available\n(fully suppressed)", cex = 1)
    return(invisible(NULL))
  }
  plot(range(data$year), range(data[[yvar]], na.rm = TRUE), ylim=c(0,1.2*max(data[[yvar]], na.rm=TRUE)),
       type = "n", xlab = "", ylab = ylab, bty = "l", las = 1, main = main)
  for (r in region_order_p) {
    dd = data[data$region == r, ]
    if (all(is.na(dd[[yvar]]))) next
    draw_status_line(dd$year, dd[[yvar]], dd[[status_col]], cols_p[r])
  }
  if (show_legend) {
    legend("topleft", legend = region_order_p, pt.bg = cols_p, pch = 21, col = "black",
           lwd = 1, cex = 0.7, bty = "n", title = "Region (low to high avg. catch)")
  }
  if (show_status_legend) add_status_legend()
}

png(here("figures","prawn_catch_by_region_panels.png"), width = 12, height = 4.5, units = "in", res = 300, pointsize = 12)
par(mfrow = c(1, 3), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))
plot_region_panel_prawn(catch_by_region_prawn, "weight_landed", "weight_landed_status", "weight landed (lbs)", "Regional landed weight", show_legend = TRUE, show_status_legend = TRUE)
plot_region_panel_prawn(catch_by_region_prawn, "num_traps_hauled", "num_traps_hauled_status", "traps hauled", "Regional effort (traps hauled)")
plot_region_panel_prawn(catch_by_region_prawn, "vessel_count", "vessel_count_status", "vessel count", "Regional vessel count")
dev.off()

### --- figure 3: CPUE --- ###
coastwide_prawn_catch = coastwide_prawn_catch %>%
  mutate(cpue = coastwide_weight_landed / coastwide_num_traps_hauled,
         cpue_status = case_when(
           is.na(cpue) ~ "suppressed",
           weight_landed_status == "partial" | num_traps_hauled_status == "partial" ~ "partial",
           TRUE ~ "complete"
         ))

catch_by_region_prawn = catch_by_region_prawn %>%
  mutate(cpue = weight_landed / num_traps_hauled,
         cpue_status = case_when(
           is.na(cpue) ~ "suppressed",
           weight_landed_status == "partial" | num_traps_hauled_status == "partial" ~ "partial",
           TRUE ~ "complete"
         ))

png(here("figures","prawn_cpue.png"), width = 10, height = 4.5, units = "in", res = 300, pointsize = 12)
par(mfrow = c(1, 2), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))

plot(range(coastwide_prawn_catch$year), buffered_range(coastwide_prawn_catch$cpue),
     type = "n", xlab = "", ylab = "CPUE (lbs / trap)", bty = "l", las = 1, main = "Coastwide CPUE")
draw_status_line(coastwide_prawn_catch$year, coastwide_prawn_catch$cpue, coastwide_prawn_catch$cpue_status, "steelblue")
add_status_legend()

plot_region_panel_prawn(catch_by_region_prawn, "cpue", "cpue_status", "CPUE (lbs / trap)", "Regional CPUE", show_legend = TRUE)
dev.off()

################################################################################
#### dive fisheries
################################################################################

dive = as.data.frame(read_csv(here("data-generated","2026-jul-dfo-catch-data-dive.csv"), skip=25))
dive = dive[,1:8]
names(dive) = c("fishery","species","year","PFMA","subarea","vessel_count","dive_time_minutes","total_landing_lbs")
dive$species = toupper(dive$species)
dive[which(dive$total_landing_lbs=="*" | dive$total_landing_lbs=="**"),]$total_landing_lbs = "NA"
dive$total_landing_lbs = as.numeric(dive$total_landing_lbs)
dive[which(dive$dive_time_minutes=="NULL"),]$dive_time_minutes = "NA"
dive$dive_time_minutes = as.numeric(dive$dive_time_minutes)

focal_fisheries = c("Geoduck and Horseclam", "Red Urchin", "Green Urchin", "Sea Cucumber")
dive = dive %>% filter(fishery %in% focal_fisheries)

plot_region_panel_generic = function(data, yvar, status_col, ylab, main, region_order, cols,
                                     show_legend = FALSE, legend_pos = "topleft", show_status_legend = FALSE) {
  if (all(is.na(data[[yvar]]))) {
    plot.new(); title(main = main)
    text(0.5, 0.5, "No data available\n(fully suppressed)", cex = 1)
    return(invisible(NULL))
  }
  plot(range(data$year), range(data[[yvar]], na.rm = TRUE),
       ylim = c(0, 1.2 * max(data[[yvar]], na.rm = TRUE)),
       type = "n", xlab = "", ylab = ylab, bty = "l", las = 1, main = main)
  for (r in region_order) {
    dd = data[data$region == r, ]
    if (all(is.na(dd[[yvar]]))) next
    draw_status_line(dd$year, dd[[yvar]], dd[[status_col]], cols[r])
  }
  if (show_legend) {
    legend(legend_pos, legend = region_order, pt.bg = cols, pch = 21, col = "black",
           lwd = 1, cex = 0.7, bty = "n", title = "Region (low to high avg. catch)")
  }
  if (show_status_legend) add_status_legend()
}

for (f in focal_fisheries) {
  
  fishery_slug = tolower(gsub(" ", "_", f))
  d_species = dive %>% filter(fishery == f)
  
  ### --- coastwide trends --- ###
  coastwide = d_species %>%
    filter(subarea == "Total") %>%
    group_by(year) %>%
    summarise(coastwide_weight_landed = sum_or_na(total_landing_lbs),
              weight_landed_status = status_flag(total_landing_lbs),
              coastwide_dive_time = sum_or_na(dive_time_minutes),
              dive_time_status = status_flag(dive_time_minutes),
              coastwide_vessel_count = sum_or_na(vessel_count),
              vessel_count_status = status_flag(vessel_count)) %>%
    complete(year = full_seq(year, 1))
  
  png(here("figures", paste0("dive_", fishery_slug, "_coastwide_trends.png")),
      width = 9, height = 3, units = "in", res = 300, pointsize = 14)
  par(mfrow = c(1, 3), mar = c(4, 5.5, 3, 1), mgp = c(4, 0.5, 0))
  
  plot(range(coastwide$year), buffered_range(coastwide$coastwide_weight_landed),
       type = "n", xlab = "year", ylab = "weight landed (lbs)", bty = "l", las = 1, main = "Coastwide landed weight")
  draw_status_line(coastwide$year, coastwide$coastwide_weight_landed, coastwide$weight_landed_status, "darkgreen")
  add_status_legend()
  
  plot(range(coastwide$year), buffered_range(coastwide$coastwide_dive_time),
       type = "n", xlab = "year", ylab = "dive time (minutes)", bty = "l", las = 1, main = "Coastwide effort (dive time)")
  draw_status_line(coastwide$year, coastwide$coastwide_dive_time, coastwide$dive_time_status, "darkorange")
  
  plot(range(coastwide$year), buffered_range(coastwide$coastwide_vessel_count),
       type = "n", xlab = "year", ylab = "vessel count", bty = "l", las = 1, main = "Coastwide vessel count")
  draw_status_line(coastwide$year, coastwide$coastwide_vessel_count, coastwide$vessel_count_status, "firebrick")
  
  dev.off()
  
  ### --- catch by PFMA (all areas, ranked groups of 12) --- ###
  catch_by_pfma = d_species %>%
    filter(subarea == "Total") %>%
    group_by(PFMA, year) %>%
    summarise(total_landing_lbs = sum_or_na(total_landing_lbs),
              weight_landed_status = status_flag(total_landing_lbs),
              .groups = "drop") %>%
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
  par(mfrow = c(nrow_panels, ncol_panels), mar = c(4, 6.5, 3, 1), mgp = c(4.2, 0.7, 0))
  for (i in 1:n_groups) {
    d_group = catch_by_pfma_ranked %>% filter(rank_group == i)
    group_totals = pfma_totals %>% filter(rank_group == i) %>% arrange(avg_weight)
    pfmas_i = group_totals$PFMA
    
    if (all(is.na(d_group$total_landing_lbs))) {
      plot.new(); title(main = panel_titles[i])
      text(0.5, 0.5, "No data available\n(fully suppressed)", cex = 1)
      next
    }
    
    cols_pfma = hcl.colors(length(pfmas_i), palette = "YlOrRd", rev = TRUE)
    plot(range(d_group$year), buffered_range(d_group$total_landing_lbs),
         type = "n", xlab = "", ylab = "weight landed (lbs)", bty = "l", las = 1, main = panel_titles[i])
    for (j in seq_along(pfmas_i)) {
      dd = d_group[d_group$PFMA == pfmas_i[j], ]
      if (all(is.na(dd$total_landing_lbs))) next
      draw_status_line(dd$year, dd$total_landing_lbs, dd$weight_landed_status, cols_pfma[j])
    }
    legend("topleft", legend = pfmas_i, pt.bg = cols_pfma, pch = 21, col = "black", lwd = 1,
           cex = 0.6, ncol = 2, bty = "n", title = "PFMA (low to high catch)")
    if (i == 1) add_status_legend()
  }
  dev.off()
  
  ### --- catch by region --- ###
  catch_by_region = d_species %>%
    filter(subarea == "Total", PFMA != "0") %>%
    mutate(PFMA = as.character(PFMA)) %>%
    left_join(region_lookup, by = "PFMA") %>%
    group_by(region, year) %>%
    summarise(weight_landed = sum_or_na(total_landing_lbs),
              weight_landed_status = status_flag(total_landing_lbs),
              dive_time = sum_or_na(dive_time_minutes),
              dive_time_status = status_flag(dive_time_minutes),
              vessel_count = sum_or_na(vessel_count),
              vessel_count_status = status_flag(vessel_count),
              .groups = "drop") %>%
    complete(region, year = full_seq(year, 1))
  
  region_totals_dive = rank_groups_low_to_high(catch_by_region, "region", "weight_landed")
  region_order = region_totals_dive$region
  
  n_extra = 3
  cols_full = hcl.colors(length(region_order) + n_extra, palette = "YlOrRd", rev = TRUE)
  cols = cols_full[(n_extra + 1):length(cols_full)]
  names(cols) = region_order
  
  png(here("figures", paste0("dive_", fishery_slug, "_catch_by_region_panels.png")),
      width = 12, height = 4.5, units = "in", res = 300, pointsize = 12)
  par(mfrow = c(1, 3), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))
  plot_region_panel_generic(catch_by_region, "weight_landed", "weight_landed_status", "weight landed (lbs)",
                            "Regional landed weight", region_order, cols, show_legend = TRUE, show_status_legend = TRUE)
  plot_region_panel_generic(catch_by_region, "dive_time", "dive_time_status", "dive time (minutes)",
                            "Regional effort (dive time)", region_order, cols)
  plot_region_panel_generic(catch_by_region, "vessel_count", "vessel_count_status", "vessel count",
                            "Regional vessel count", region_order, cols)
  dev.off()
  
  ### --- CPUE --- ###
  coastwide = coastwide %>%
    mutate(cpue = coastwide_weight_landed / coastwide_dive_time,
           cpue_status = case_when(
             is.na(cpue) ~ "suppressed",
             weight_landed_status == "partial" | dive_time_status == "partial" ~ "partial",
             TRUE ~ "complete"
           ))
  catch_by_region = catch_by_region %>%
    mutate(cpue = weight_landed / dive_time,
           cpue_status = case_when(
             is.na(cpue) ~ "suppressed",
             weight_landed_status == "partial" | dive_time_status == "partial" ~ "partial",
             TRUE ~ "complete"
           ))
  
  png(here("figures", paste0("dive_", fishery_slug, "_cpue.png")),
      width = 10, height = 4.5, units = "in", res = 300, pointsize = 12)
  par(mfrow = c(1, 2), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))
  
  if (all(is.na(coastwide$cpue))) {
    plot.new(); title(main = "Coastwide CPUE"); text(0.5, 0.5, "No data available\n(fully suppressed)", cex = 1)
  } else {
    plot(range(coastwide$year), buffered_range(coastwide$cpue),
         type = "n", xlab = "", ylab = "CPUE (lbs / minute dive time)", bty = "l", las = 1, main = "Coastwide CPUE")
    draw_status_line(coastwide$year, coastwide$cpue, coastwide$cpue_status, "steelblue")
    add_status_legend()
  }
  plot_region_panel_generic(catch_by_region, "cpue", "cpue_status", "CPUE (lbs / minute dive time)",
                            "Regional CPUE", region_order, cols, show_legend = TRUE)
  dev.off()
}

################################################################################
#### shrimp trawl
################################################################################

shrimp = as.data.frame(read_csv(here("data-generated","2026-jul-dfo-catch-data-shrimp.csv"), skip=20))
shrimp = shrimp[,1:12]
names(shrimp) = c("fishery","species","year","PFMA","subarea","vessel_count","tow_duration_minutes",
                  "pink_lbs","sidestripe_lbs","humpback_lbs","coonstripe_lbs","prawn_lbs")

species_cols = c("pink_lbs","sidestripe_lbs","humpback_lbs","coonstripe_lbs","prawn_lbs")
for (col in species_cols) {
  shrimp[[col]][shrimp[[col]] == "*" | shrimp[[col]] == "**"] = NA
  shrimp[[col]] = as.numeric(shrimp[[col]])
}

shrimp$total_landing_lbs = apply(shrimp[species_cols], 1, sum_or_na)
shrimp$total_landing_status = apply(shrimp[species_cols], 1, status_flag)

### --- coastwide trends --- ###
coastwide_shrimp_catch = shrimp %>%
  filter(subarea == "Total") %>%
  group_by(year) %>%
  summarise(coastwide_weight_landed = sum_or_na(total_landing_lbs),
            weight_landed_status = status_flag(total_landing_lbs),
            coastwide_tow_duration = sum_or_na(tow_duration_minutes),
            tow_duration_status = status_flag(tow_duration_minutes),
            coastwide_vessel_count = sum_or_na(vessel_count),
            vessel_count_status = status_flag(vessel_count)) %>%
  complete(year = full_seq(year, 1))

png(here("figures","shrimp_coastwide_trends.png"), width = 9, height = 3, units = "in", res = 300, pointsize=14)
par(mfrow = c(1, 3), mar = c(4, 5.5, 3, 1), mgp = c(4, 0.5, 0))

plot(range(coastwide_shrimp_catch$year), buffered_range(coastwide_shrimp_catch$coastwide_weight_landed),
     type = "n", xlab = "year", ylab = "weight landed (lbs)", bty="l", las=1, main = "Coastwide landed weight")
draw_status_line(coastwide_shrimp_catch$year, coastwide_shrimp_catch$coastwide_weight_landed, coastwide_shrimp_catch$weight_landed_status, "darkgreen")
add_status_legend()

plot(range(coastwide_shrimp_catch$year), buffered_range(coastwide_shrimp_catch$coastwide_tow_duration),
     type = "n", xlab = "year", ylab = "tow duration (minutes)", bty="l", las=1, main = "Coastwide effort (tow duration)")
draw_status_line(coastwide_shrimp_catch$year, coastwide_shrimp_catch$coastwide_tow_duration, coastwide_shrimp_catch$tow_duration_status, "darkorange")

plot(range(coastwide_shrimp_catch$year), buffered_range(coastwide_shrimp_catch$coastwide_vessel_count),
     type = "n", xlab = "year", ylab = "vessel count", bty="l", las=1, main = "Coastwide vessel count")
draw_status_line(coastwide_shrimp_catch$year, coastwide_shrimp_catch$coastwide_vessel_count, coastwide_shrimp_catch$vessel_count_status, "firebrick")

dev.off()

### --- catch by PFMA (all areas, ranked groups of 12) --- ###
catch_by_pfma_shrimp = shrimp %>%
  filter(subarea == "Total") %>%
  group_by(PFMA, year) %>%
  summarise(total_landing_lbs = sum_or_na(total_landing_lbs),
            weight_landed_status = status_flag(total_landing_lbs),
            .groups = "drop") %>%
  complete(PFMA, year = full_seq(year, 1))

pfma_totals_shrimp = catch_by_pfma_shrimp %>%
  group_by(PFMA) %>%
  summarise(avg_weight = mean(total_landing_lbs, na.rm = TRUE)) %>%
  arrange(desc(avg_weight)) %>%
  mutate(rank_group = ceiling(row_number() / 12))

catch_by_pfma_shrimp_ranked = catch_by_pfma_shrimp %>%
  left_join(pfma_totals_shrimp %>% select(PFMA, rank_group), by = "PFMA")

n_groups = max(pfma_totals_shrimp$rank_group)
panel_titles = paste0("PFMAs ", (0:(n_groups-1))*12 + 1, "\u2013", pmin((1:n_groups)*12, nrow(pfma_totals_shrimp)))
panel_titles[1] = "Top 12 PFMAs (by avg. catch)"

png(here("figures","shrimp_catch_by_pfma_ranked_groups.png"), width = 10, height = 8, units = "in", res = 300, pointsize = 12)
par(mfrow = c(2, 2), mar = c(4, 6.5, 3, 1), mgp = c(4.2, 0.7, 0))
for (i in 1:n_groups) {
  d_group = catch_by_pfma_shrimp_ranked %>% filter(rank_group == i)
  group_totals = pfma_totals_shrimp %>% filter(rank_group == i) %>% arrange(avg_weight)
  pfmas_i = group_totals$PFMA
  
  if (all(is.na(d_group$total_landing_lbs))) {
    plot.new(); title(main = panel_titles[i])
    text(0.5, 0.5, "No data available\n(fully suppressed)", cex = 1)
    next
  }
  
  cols = hcl.colors(length(pfmas_i), palette = "YlOrRd", rev = TRUE)
  plot(range(d_group$year), buffered_range(d_group$total_landing_lbs),
       type = "n", xlab = "", ylab = "weight landed (lbs)", bty = "l", las = 1, main = panel_titles[i])
  for (j in seq_along(pfmas_i)) {
    dd = d_group[d_group$PFMA == pfmas_i[j], ]
    if (all(is.na(dd$total_landing_lbs))) next
    draw_status_line(dd$year, dd$total_landing_lbs, dd$weight_landed_status, cols[j])
  }
  legend("topleft", legend = pfmas_i, pt.bg = cols, pch = 21, col = "black", lwd = 1,
         cex = 0.6, ncol = 2, bty = "n", title = "PFMA (low to high catch)")
  if (i == 1) add_status_legend()
}
dev.off()

### --- catch by region --- ###
catch_by_region_shrimp = shrimp %>%
  filter(subarea == "Total", PFMA != "0") %>%
  mutate(PFMA = as.character(PFMA)) %>%
  left_join(region_lookup, by = "PFMA") %>%
  group_by(region, year) %>%
  summarise(weight_landed = sum_or_na(total_landing_lbs),
            weight_landed_status = status_flag(total_landing_lbs),
            tow_duration = sum_or_na(tow_duration_minutes),
            tow_duration_status = status_flag(tow_duration_minutes),
            vessel_count = sum_or_na(vessel_count),
            vessel_count_status = status_flag(vessel_count),
            .groups = "drop") %>%
  complete(region, year = full_seq(year, 1))

region_totals_shrimp = rank_groups_low_to_high(catch_by_region_shrimp, "region", "weight_landed")
region_order_s = region_totals_shrimp$region

n_extra = 3
cols_full_s = hcl.colors(length(region_order_s) + n_extra, palette = "YlOrRd", rev = TRUE)
cols_s = cols_full_s[(n_extra + 1):length(cols_full_s)]
names(cols_s) = region_order_s

plot_region_panel_shrimp = function(data, yvar, status_col, ylab, main, show_legend = FALSE, show_status_legend = FALSE) {
  if (all(is.na(data[[yvar]]))) {
    plot.new(); title(main = main)
    text(0.5, 0.5, "No data available\n(fully suppressed)", cex = 1)
    return(invisible(NULL))
  }
  plot(range(data$year), range(data[[yvar]], na.rm = TRUE), ylim=c(0,1.2*max(data[[yvar]], na.rm=TRUE)),
       type = "n", xlab = "", ylab = ylab, bty = "l", las = 1, main = main)
  for (r in region_order_s) {
    dd = data[data$region == r, ]
    if (all(is.na(dd[[yvar]]))) next
    draw_status_line(dd$year, dd[[yvar]], dd[[status_col]], cols_s[r])
  }
  if (show_legend) {
    legend("topleft", legend = region_order_s, pt.bg = cols_s, pch = 21, col = "black",
           lwd = 1, cex = 0.7, bty = "n", title = "Region (low to high avg. catch)")
  }
  if (show_status_legend) add_status_legend()
}

png(here("figures","shrimp_catch_by_region_panels.png"), width = 12, height = 4.5, units = "in", res = 300, pointsize = 12)
par(mfrow = c(1, 3), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))
plot_region_panel_shrimp(catch_by_region_shrimp, "weight_landed", "weight_landed_status", "weight landed (lbs)", "Regional landed weight", show_legend = TRUE, show_status_legend = TRUE)
plot_region_panel_shrimp(catch_by_region_shrimp, "tow_duration", "tow_duration_status", "tow duration (minutes)", "Regional effort (tow duration)")
plot_region_panel_shrimp(catch_by_region_shrimp, "vessel_count", "vessel_count_status", "vessel count", "Regional vessel count")
dev.off()

### --- CPUE --- ###
coastwide_shrimp_catch = coastwide_shrimp_catch %>%
  mutate(cpue = coastwide_weight_landed / coastwide_tow_duration,
         cpue_status = case_when(
           is.na(cpue) ~ "suppressed",
           weight_landed_status == "partial" | tow_duration_status == "partial" ~ "partial",
           TRUE ~ "complete"
         ))

catch_by_region_shrimp = catch_by_region_shrimp %>%
  mutate(cpue = weight_landed / tow_duration,
         cpue_status = case_when(
           is.na(cpue) ~ "suppressed",
           weight_landed_status == "partial" | tow_duration_status == "partial" ~ "partial",
           TRUE ~ "complete"
         ))

png(here("figures","shrimp_cpue.png"), width = 10, height = 4.5, units = "in", res = 300, pointsize = 12)
par(mfrow = c(1, 2), mar = c(4, 5.5, 3, 1), mgp = c(3.8, 0.7, 0))

plot(range(coastwide_shrimp_catch$year), buffered_range(coastwide_shrimp_catch$cpue),
     type = "n", xlab = "", ylab = "CPUE (lbs / minute tow duration)", bty = "l", las = 1, main = "Coastwide CPUE")
draw_status_line(coastwide_shrimp_catch$year, coastwide_shrimp_catch$cpue, coastwide_shrimp_catch$cpue_status, "steelblue")
add_status_legend()

plot_region_panel_shrimp(catch_by_region_shrimp, "cpue", "cpue_status", "CPUE (lbs / minute tow duration)", "Regional CPUE", show_legend = TRUE)
dev.off()