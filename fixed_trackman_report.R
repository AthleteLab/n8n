# Load required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)    # For color scales
library(cowplot)  # For plot composition
library(grid)     # For textGrob and gpar

# Function to convert spin axis to clock face tilt
spin_to_tilt <- function(spin_axis) {
  # Handle NA values
  if (any(is.na(spin_axis))) {
    return(ifelse(is.na(spin_axis), NA, spin_to_tilt(spin_axis[!is.na(spin_axis)])))
  }
  
  # Baseball Savant spin_axis: 180° = pure backspin (12:00), 0° = pure topspin (6:00)
  # From catcher's perspective: higher degrees go left (toward 11), lower go right (toward 1)
  
  # First normalize to 0-360 range
  normalized <- (spin_axis + 360) %% 360
  
  # Convert spin axis to clock hours
  # 180° = 12:00 (6 hours), so we need: (spin_axis/30) + 6
  # But wrap around 12-hour clock
  clock_decimal <- (normalized / 30 + 6) %% 12
  
  # Convert to 12-hour format
  clock_decimal <- ifelse(clock_decimal == 0, 12, clock_decimal)
  clock_decimal <- ifelse(clock_decimal > 12, clock_decimal - 12, clock_decimal)
  
  # Convert decimal to hours:minutes
  hours <- floor(clock_decimal)
  minutes <- round((clock_decimal - hours) * 60)
  
  # Handle minute overflow
  minutes <- ifelse(minutes >= 60, 0, minutes)
  hours <- ifelse(minutes == 0 & (clock_decimal - floor(clock_decimal)) * 60 >= 60, hours + 1, hours)
  hours <- ifelse(hours > 12, hours - 12, hours)
  hours <- ifelse(hours == 0, 12, hours)
  
  # Format as clock position
  sprintf("%d:%02d", hours, minutes)
}

# Function to create circle points
create_circle <- function(center = c(0,0), radius = 1, npoints = 100) {
  angles <- seq(0, 2*pi, length.out = npoints)
  
  data.frame(
    x = center[1] + radius * cos(angles),
    y = center[2] + radius * sin(angles)
  )
}

# Define consistent color scheme for pitch types
pitch_colors <- c(
  "FF" = "black",
  "SI" = "gray",
  "CT" = "darkgreen", 
  "CH" = "blue",
  "SP" = "lightblue",
  "SL" = "yellow",
  "SW" = "orange",
  "CB" = "red"
)

# Set working directory
setwd("C:/Users/cmurr/OneDrive/Desktop/Athlete Lab Showcase Reports")

# Read the data and print structure
data <- read.csv("TS.csv")

print("Column names in the data:")
print(names(data))

print("\nAvailable pitchers:")
print(unique(data$player_name))

# Helper functions for the comprehensive report
create_movement_plot <- function(data) {
  # Simplified version of existing movement plot
  avg_movement <- data %>%
    group_by(PitchType) %>%
    summarise(
      avg_horz = mean(HorzBreak, na.rm = TRUE),
      avg_vert = mean(InducedVertBreak, na.rm = TRUE),
      .groups = 'drop'
    )
  
  ggplot(data, aes(x = HorzBreak, y = InducedVertBreak, color = PitchType)) +
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    geom_vline(xintercept = 0, color = "black", size = 0.5) +
    geom_point(alpha = 0.6) +
    geom_point(data = avg_movement, aes(x = avg_horz, y = avg_vert, color = PitchType), 
               size = 4, inherit.aes = FALSE) +
    scale_color_manual(values = pitch_colors) +
    coord_fixed(xlim = c(-25, 25), ylim = c(-25, 25)) +
    theme_minimal() +
    labs(title = "Pitch Movement", subtitle = "Pitcher View")
}

create_release_plot <- function(data) {
  # Simplified version of existing release plot
  ggplot(data, aes(x = RelSide, y = RelHeight, color = PitchType)) +
    geom_point(alpha = 0.7, size = 2) +
    scale_color_manual(values = pitch_colors) +
    coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 7.5)) +
    theme_minimal() +
    labs(title = "Release Point", subtitle = "Pitcher View")
}

create_tilt_plot <- function(data) {
  # Simplified tilt plot
  clock_face <- create_circle(radius = 0.8)
  
  ggplot() +
    geom_polygon(data = clock_face, aes(x = x, y = y), fill = "gray95", color = "black") +
    coord_fixed(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
    theme_void() +
    labs(title = "Tilt Consistency")
}

create_velocity_plot <- function(data) {
  ggplot(data, aes(x = RelSpeed, fill = PitchType)) +
    geom_density(alpha = 0.6) +
    facet_grid(PitchType ~ .) +
    scale_fill_manual(values = pitch_colors) +
    theme_minimal() +
    labs(title = "Velocity Consistency") +
    theme(legend.position = "none")
}

create_heatmaps_by_pitch <- function(data) {
  ggplot(data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    geom_density_2d_filled(alpha = 0.7) +
    geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
              fill = NA, color = "black", size = 1) +
    facet_wrap(~ PitchType) +
    coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
    theme_minimal() +
    labs(title = "Location Heatmaps by Pitch Type") +
    theme(legend.position = "none")
}

# NEW COMPREHENSIVE MULTI-PAGE REPORT FUNCTION
create_comprehensive_pitching_report <- function(data, pitcher_name) {
  
  # Filter and prepare data
  pitcher_data <- data %>%
    filter(player_name == pitcher_name) %>%
    mutate(
      # Map Baseball Savant names to expected names
      PitchType = if("pitch_name" %in% names(.)) pitch_name else if("pitch_type" %in% names(.)) pitch_type else NA,
      RelSpeed = if("release_speed" %in% names(.)) release_speed else NA,
      SpinRate = if("release_spin_rate" %in% names(.)) release_spin_rate
      else if("release_spin" %in% names(.)) release_spin 
      else if("spin_rate" %in% names(.)) spin_rate 
      else if("spin_rate_deprecated" %in% names(.)) spin_rate_deprecated 
      else NA,
      InducedVertBreak = if("pfx_z" %in% names(.)) pfx_z * 12 else NA,
      HorzBreak = if("pfx_x" %in% names(.)) pfx_x * 12 else NA,
      SpinAxis = if("spin_axis" %in% names(.)) spin_axis else NA,
      Extension = if("release_extension" %in% names(.)) release_extension else NA,
      PlateLocSide = if("plate_x" %in% names(.)) plate_x else NA,
      PlateLocHeight = if("plate_z" %in% names(.)) plate_z else NA,
      RelSide = if("release_pos_x" %in% names(.)) release_pos_x else NA,
      RelHeight = if("release_pos_z" %in% names(.)) release_pos_z else NA,
      ArmAngle = if("arm_angle" %in% names(.)) arm_angle else NA,
      BatterHand = if("stand" %in% names(.)) stand else NA,
      # Combine events and description for result with proper labels
      Result = case_when(
        !is.na(events) & events != "" ~ case_when(
          # Strike outcomes
          events == "called_strike" ~ "Called Strike", 
          events == "swinging_strike" ~ "Swinging Strike",
          events == "swinging_strike_blocked" ~ "Swinging Strike",
          events == "missed_bunt" ~ "Swinging Strike",
          # Ball outcomes
          events == "ball" ~ "Ball",
          events == "blocked_ball" ~ "Ball",
          # Foul outcomes
          events == "foul" ~ "Foul",
          events == "foul_tip" ~ "Foul",
          events == "foul_bunt" ~ "Foul",
          events == "bunt_foul_tip" ~ "Foul",
          # Hit by pitch
          events == "hit_by_pitch" ~ "Hit By Pitch",
          # In play outcomes - outs
          events == "field_out" ~ "**In Play, Out(s)**",
          events == "force_out" ~ "**In Play, Out(s)**",
          events == "grounded_into_double_play" ~ "**In Play, Out(s)**",
          events == "fielders_choice_out" ~ "**In Play, Out(s)**",
          events == "double_play" ~ "**In Play, Out(s)**",
          events == "sac_fly_double_play" ~ "**In Play, Out(s)**",
          events == "triple_play" ~ "**In Play, Out(s)**",
          # In play outcomes - hits
          events == "home_run" ~ "**Home Run**",
          events == "single" ~ "**Single**", 
          events == "double" ~ "**Double**",
          events == "triple" ~ "**Triple**",
          events == "error" ~ "**Error**",
          events == "field_error" ~ "**Error**",
          # Final outcome - plate appearance results
          events == "walk" ~ "**Walk**",
          events == "strikeout" ~ "**Strikeout**",
          events == "strikeout_double_play" ~ "**Strikeout**",
          events == "sac_bunt" ~ "**Sac Bunt, Out**",
          events == "sac_fly" ~ "**Sac Fly, Out**",
          events == "fielders_choice" ~ "**Fielders Choice**",
          events == "truncated_pa" ~ "**Truncated PA**",
          events == "catcher_interf" ~ "**Catcher Interference**",
          TRUE ~ events
        ),
        !is.na(description) & description != "" ~ case_when(
          # Strike outcomes
          description == "called_strike" ~ "Called Strike", 
          description == "swinging_strike" ~ "Swinging Strike",
          description == "swinging_strike_blocked" ~ "Swinging Strike",
          description == "missed_bunt" ~ "Swinging Strike",
          # Ball outcomes
          description == "ball" ~ "Ball",
          description == "blocked_ball" ~ "Ball",
          # Foul outcomes
          description == "foul" ~ "Foul",
          description == "foul_tip" ~ "Foul",
          description == "foul_bunt" ~ "Foul",
          description == "bunt_foul_tip" ~ "Foul",
          # Hit by pitch
          description == "hit_by_pitch" ~ "Hit By Pitch",
          # In play outcomes - outs
          description == "field_out" ~ "**In Play, Out(s)**",
          description == "force_out" ~ "**In Play, Out(s)**",
          description == "grounded_into_double_play" ~ "**In Play, Out(s)**",
          description == "fielders_choice_out" ~ "**In Play, Out(s)**",
          description == "double_play" ~ "**In Play, Out(s)**",
          description == "sac_fly_double_play" ~ "**In Play, Out(s)**",
          description == "triple_play" ~ "**In Play, Out(s)**",
          # In play outcomes - hits
          description == "home_run" ~ "**Home Run**",
          description == "single" ~ "**Single**", 
          description == "double" ~ "**Double**",
          description == "triple" ~ "**Triple**",
          description == "error" ~ "**Error**",
          description == "field_error" ~ "**Error**",
          # Final outcome - plate appearance results
          description == "walk" ~ "**Walk**",
          description == "strikeout" ~ "**Strikeout**",
          description == "strikeout_double_play" ~ "**Strikeout**",
          description == "sac_bunt" ~ "**Sac Bunt, Out**",
          description == "sac_fly" ~ "**Sac Fly, Out**",
          description == "fielders_choice" ~ "**Fielders Choice**",
          description == "truncated_pa" ~ "**Truncated PA**",
          description == "catcher_interf" ~ "**Catcher Interference**",
          TRUE ~ description
        ),
        TRUE ~ "Unknown"
      ),
      Count = if("balls" %in% names(.) & "strikes" %in% names(.)) paste0(balls, "-", strikes) else NA,
      # Add fields needed for proper pitch numbering  
      Inning = if("inning" %in% names(.)) inning else NA,
      GamePK = if("game_pk" %in% names(.)) game_pk else NA,
      ThroughOrder = if("n_thruorder_pitcher" %in% names(.)) n_thruorder_pitcher else NA,
      PriorPA = if("n_priorpa_thisgame_player_at_bat" %in% names(.)) n_priorpa_thisgame_player_at_bat else NA,
      Pitcher = if("pitcher" %in% names(.)) pitcher else NA,
      Batter = if("batter" %in% names(.)) batter else NA,
      # Add outs field
      Outs = if("outs_when_up" %in% names(.)) outs_when_up else if("out_when_up" %in% names(.)) out_when_up else NA,
      # Add pitch sequence field if available
      PitchSequence = if("pitch_number" %in% names(.)) pitch_number else NA,
      # Create a proper datetime field for sorting
      game_datetime = if("game_date" %in% names(.)) as.POSIXct(game_date) else Sys.time()
    ) %>%
    # Convert pitch names to abbreviations
    mutate(
      PitchType = case_when(
        grepl("4-Seam|Four-Seam|Fastball", PitchType, ignore.case = TRUE) ~ "FF",
        grepl("Sinker|2-Seam|Two-Seam", PitchType, ignore.case = TRUE) ~ "SI", 
        grepl("Cutter", PitchType, ignore.case = TRUE) ~ "CT",
        grepl("Changeup|Change", PitchType, ignore.case = TRUE) ~ "CH",
        grepl("Slider", PitchType, ignore.case = TRUE) ~ "SL",
        grepl("Sweeper", PitchType, ignore.case = TRUE) ~ "SW",
        grepl("Curveball|Curve|Knuckle.*Curve|Slow.*Curve", PitchType, ignore.case = TRUE) ~ "CB",
        grepl("Split|Splitter", PitchType, ignore.case = TRUE) ~ "SP",
        TRUE ~ PitchType
      )
    ) %>%
    filter(!is.na(PitchType))
  
  # Detect handedness and adjust for lefties
  is_lefty <- mean(pitcher_data$RelSide, na.rm = TRUE) > 0
  
  if(is_lefty) {
    pitcher_data <- pitcher_data %>%
      mutate(
        HorzBreak = -HorzBreak,
        ArmAngle = 180 - ArmAngle,
        RelSide = -RelSide
      )
    print(paste("Detected left-handed pitcher:", pitcher_name))
  }
  
  # Get date range
  date_col <- NULL
  if ("game_date" %in% names(pitcher_data)) {
    date_col <- "game_date"
  } else if ("Date" %in% names(pitcher_data)) {
    date_col <- "Date"
  }
  
  if (!is.null(date_col)) {
    min_date <- min(as.Date(pitcher_data[[date_col]], format = "%Y-%m-%d"), na.rm = TRUE)
    max_date <- max(as.Date(pitcher_data[[date_col]], format = "%Y-%m-%d"), na.rm = TRUE)
    date_range <- paste(format(min_date, "%Y-%m-%d"), "to", format(max_date, "%Y-%m-%d"))
    filename_date <- paste0(format(min_date, "%Y-%m-%d"), "_to_", format(max_date, "%Y-%m-%d"))
  } else {
    date_range <- "Date Unknown"
    filename_date <- "Unknown_Date"
  }
  
  # Helper function to create arsenal table
  create_arsenal_table <- function(data, batter_filter = "all") {
    if(batter_filter != "all") {
      data <- data %>% filter(BatterHand == batter_filter)
    }
    
    # Add derived fields for calculations
    data <- data %>%
      mutate(
        # Zone determination (simplified - assumes strike zone exists)
        IsStrike = case_when(
          grepl("Called Strike|Foul|Swinging Strike", Result, ignore.case = TRUE) ~ TRUE,
          grepl("Ball", Result, ignore.case = TRUE) ~ FALSE,
          TRUE ~ NA
        ),
        IsInZone = case_when(
          !is.na(PlateLocSide) & !is.na(PlateLocHeight) ~ 
            (abs(PlateLocSide) <= 0.83 & PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5),
          TRUE ~ NA
        ),
        IsSwing = grepl("Foul|In Play|Swinging Strike", Result, ignore.case = TRUE),
        IsWhiff = grepl("Swinging Strike", Result, ignore.case = TRUE),
        IsCalledStrike = grepl("Called Strike", Result, ignore.case = TRUE)
      )
    
    # Calculate metrics
    arsenal_metrics <- data %>%
      group_by(PitchType) %>%
      summarise(
        Count = n(),
        `Max Velo` = round(max(RelSpeed, na.rm = TRUE), 1),
        `Avg Velo` = round(mean(RelSpeed, na.rm = TRUE), 1),
        `Spin Rate` = round(mean(SpinRate, na.rm = TRUE), 0),
        `Avg IVB` = round(mean(InducedVertBreak, na.rm = TRUE), 1),
        `Avg HB` = round(mean(HorzBreak, na.rm = TRUE), 1),
        Extension = round(mean(Extension, na.rm = TRUE), 1),
        `Avg Spin Axis` = mean(SpinAxis, na.rm = TRUE),
        # Advanced metrics
        `Strike%` = round(mean(IsStrike, na.rm = TRUE) * 100, 1),
        `Zone%` = round(mean(IsInZone, na.rm = TRUE) * 100, 1),
        `Whiff%` = round(sum(IsWhiff, na.rm = TRUE) / sum(IsSwing, na.rm = TRUE) * 100, 1),
        `CSW%` = round((sum(IsCalledStrike, na.rm = TRUE) + sum(IsWhiff, na.rm = TRUE)) / n() * 100, 1),
        .groups = 'drop'
      ) %>%
      mutate(
        `Usage%` = round((Count / sum(Count)) * 100, 1),
        Tilt = spin_to_tilt(`Avg Spin Axis`),
        Tilt = ifelse(substr(Tilt, 1, 2) == "0:", paste0("12", substr(Tilt, 2, nchar(Tilt))), Tilt)
      ) %>%
      select(-`Avg Spin Axis`) %>%
      select(PitchType, Count, `Usage%`, `Max Velo`, `Avg Velo`, `Spin Rate`, 
             `Avg IVB`, `Avg HB`, Extension, Tilt, `Strike%`, `Zone%`, `Whiff%`, `CSW%`)
    
    return(tableGrob(arsenal_metrics, rows = NULL))
  }
  
  # Helper function to create summary stats table
  create_summary_stats_table <- function(data) {
    # Add derived fields for calculations
    data <- data %>%
      mutate(
        IsStrike = case_when(
          grepl("Called Strike|Foul|Swinging Strike", Result, ignore.case = TRUE) ~ TRUE,
          grepl("Ball", Result, ignore.case = TRUE) ~ FALSE,
          TRUE ~ NA
        ),
        IsInZone = case_when(
          !is.na(PlateLocSide) & !is.na(PlateLocHeight) ~ 
            (abs(PlateLocSide) <= 0.83 & PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5),
          TRUE ~ NA
        ),
        IsSwing = grepl("Foul|In Play|Swinging Strike", Result, ignore.case = TRUE),
        IsWhiff = grepl("Swinging Strike", Result, ignore.case = TRUE),
        IsCalledStrike = grepl("Called Strike", Result, ignore.case = TRUE),
        IsHit = grepl("Single|Double|Triple|Home Run|In Play", Result, ignore.case = TRUE),
        IsWalk = grepl("walk", Result, ignore.case = TRUE),
        IsStrikeout = grepl("Swinging Strike", Result, ignore.case = TRUE),  # Simplified
        # Additional fields for traditional stats
        IsSingle = grepl("Single", Result, ignore.case = TRUE),
        IsDouble = grepl("Double", Result, ignore.case = TRUE),
        IsTriple = grepl("Triple", Result, ignore.case = TRUE),
        IsHomeRun = grepl("Home Run", Result, ignore.case = TRUE),
        IsAtBat = grepl("Single|Double|Triple|Home Run|In Play|Swing & Miss", Result, ignore.case = TRUE),
        IsRun = grepl("Home Run", Result, ignore.case = TRUE)  # Simplified - just home runs
      )
    
    # Calculate traditional stats
    total_hits <- sum(data$IsHit, na.rm = TRUE)
    total_walks <- sum(data$IsWalk, na.rm = TRUE) 
    total_strikeouts <- sum(data$IsStrikeout, na.rm = TRUE)
    total_at_bats <- sum(data$IsAtBat, na.rm = TRUE)
    estimated_runs <- sum(data$IsRun, na.rm = TRUE)  # Rough approximation
    
    # Estimate innings pitched (outs / 3)
    estimated_outs <- sum(data$IsStrikeout, na.rm = TRUE) + 
      sum(grepl("field_out|force_out|grounded_into_double_play", data$Result, ignore.case = TRUE), na.rm = TRUE)
    estimated_ip <- round(estimated_outs / 3, 1)
    
    # Calculate advanced stats
    whip <- round((total_hits + total_walks) / estimated_ip, 2)
    baa <- round(total_hits / total_at_bats, 3)
    
    # Approximate wOBA calculation (simplified)
    singles <- sum(data$IsSingle, na.rm = TRUE)
    doubles <- sum(data$IsDouble, na.rm = TRUE) 
    triples <- sum(data$IsTriple, na.rm = TRUE)
    home_runs <- sum(data$IsHomeRun, na.rm = TRUE)
    woba_numerator <- (0.89 * total_walks) + (1.27 * singles) + (1.62 * doubles) + (2.10 * triples) + (2.65 * home_runs)
    woba_denominator <- total_at_bats + total_walks
    woba_against <- round(woba_numerator / woba_denominator, 3)
    
    # Calculate CSW correctly: (Called Strikes + Swinging Strikes) / Total Pitches
    total_called_strikes <- sum(data$IsCalledStrike, na.rm = TRUE)
    total_whiffs <- sum(data$IsWhiff, na.rm = TRUE)
    csw_rate <- round((total_called_strikes + total_whiffs) / nrow(data) * 100, 1)
    
    summary_stats <- data.frame(
      IP = estimated_ip,
      R = estimated_runs,
      H = total_hits,
      K = total_strikeouts,
      BB = total_walks,
      WHIP = whip,
      `Strike%` = paste0(round(mean(data$IsStrike, na.rm = TRUE) * 100, 1), "%"),
      `Zone%` = paste0(round(mean(data$IsInZone, na.rm = TRUE) * 100, 1), "%"),
      `Whiff%` = paste0(round(sum(data$IsWhiff, na.rm = TRUE) / sum(data$IsSwing, na.rm = TRUE) * 100, 1), "%"),
      `CSW%` = paste0(csw_rate, "%")
    )
    return(tableGrob(summary_stats, rows = NULL))
  }
  
  # PAGE 1 - SUMMARY OVERVIEW
  page1_arsenal <- create_arsenal_table(pitcher_data, "all")
  page1_summary <- create_summary_stats_table(pitcher_data)
  
  # Reuse existing plot functions (simplified versions)
  page1_movement <- create_movement_plot(pitcher_data)
  page1_release <- create_release_plot(pitcher_data)
  page1_tilt <- create_tilt_plot(pitcher_data)
  page1_velocity <- create_velocity_plot(pitcher_data)
  
  page1 <- grid.arrange(
    # Name/Title (same as original)
    textGrob(paste(pitcher_name, "-", date_range), gp = gpar(fontsize = 20, fontface = "bold")),
    # Arsenal Table (same as original)  
    page1_arsenal,
    # NEW: Summary Stats Table
    page1_summary,
    # Movement Plot | Release Plot (same as original)
    arrangeGrob(page1_movement, page1_release, ncol = 2),
    # Clock | Velocity Consistency (same as original)
    arrangeGrob(page1_tilt, page1_velocity, ncol = 2),
    # Note: Removed heatmaps since original report didn't have them on page 1
    heights = c(0.5, 2, 1.5, 3, 3)
  )
  
  # PAGE 2 - vs LHB
  page2_arsenal <- create_arsenal_table(pitcher_data, "L")
  page2_heatmaps <- create_heatmaps_by_pitch(pitcher_data %>% filter(BatterHand == "L"))
  
  page2 <- grid.arrange(
    textGrob(paste("Page 2 - Arsenal vs LHB:", pitcher_name), gp = gpar(fontsize = 16, fontface = "bold")),
    page2_arsenal,
    page2_heatmaps,
    heights = c(0.5, 2, 4)
  )
  
  # PAGE 3 - vs RHB  
  page3_arsenal <- create_arsenal_table(pitcher_data, "R")
  page3_heatmaps <- create_heatmaps_by_pitch(pitcher_data %>% filter(BatterHand == "R"))
  
  page3 <- grid.arrange(
    textGrob(paste("Page 3 - Arsenal vs RHB:", pitcher_name), gp = gpar(fontsize = 16, fontface = "bold")),
    page3_arsenal,
    page3_heatmaps,
    heights = c(0.5, 2, 4)
  )
  
  # PAGE 4 - PITCH LOG (FIXED VERSION)
  # Create proper PA identifiers and ensure correct chronological ordering
  pitch_log <- pitcher_data %>%
    mutate(
      # Create unique PA identifier using game_pk + pitcher + batter + n_priorpa_thisgame_player_at_bat
      PA_UID = paste(GamePK, Pitcher, Batter, PriorPA, sep = "_")
    ) %>%
    # CRITICAL: Sort by pitcher, game_date, game_pk, PA identifier, then n_thruorder_pitcher (ASCENDING)
    arrange(Pitcher, game_datetime, GamePK, PA_UID, ThroughOrder) %>%
    group_by(Pitcher) %>%
    mutate(
      # Global pitch number for this pitcher
      `Pitch #` = row_number(),
      # Create sequential PA numbers based on PA_UID changes
      `PA #` = cumsum(c(1, PA_UID[-1] != PA_UID[-length(PA_UID)]))
    ) %>%
    group_by(Pitcher, `PA #`) %>%
    mutate(
      # Pitch number within this specific PA
      `Pitch # in PA` = row_number()
    ) %>%
    ungroup() %>%
    # CRITICAL: Recalculate balls and strikes from scratch based on pitch results
    group_by(Pitcher, `PA #`) %>%
    arrange(Pitcher, `PA #`, `Pitch # in PA`) %>%
    mutate(
      # Determine if this pitch is a ball based on Result
      Is_Ball = case_when(
        grepl("Ball|Hit By Pitch", Result, ignore.case = TRUE) ~ 1,
        TRUE ~ 0
      ),
      # Simple approach: Calculate strikes first, then handle fouls
      Temp_Strike = case_when(
        grepl("Called Strike|Swinging Strike", Result, ignore.case = TRUE) ~ 1,
        grepl("Foul", Result, ignore.case = TRUE) ~ 1,
        TRUE ~ 0
      ),
      # Calculate cumulative strikes up to previous pitch
      Cum_Strikes_Prev = lag(cumsum(Temp_Strike), default = 0),
      # Now determine if current pitch actually counts as a strike (handle 2-strike fouls)
      Is_Strike = case_when(
        grepl("Called Strike|Swinging Strike", Result, ignore.case = TRUE) ~ 1,
        grepl("Foul", Result, ignore.case = TRUE) & Cum_Strikes_Prev < 2 ~ 1,
        grepl("Foul", Result, ignore.case = TRUE) & Cum_Strikes_Prev >= 2 ~ 0,
        TRUE ~ 0
      ),
      # Calculate running count for display (what count was BEFORE this pitch)
      Running_Balls = lag(cumsum(Is_Ball), default = 0),
      Running_Strikes = lag(cumsum(Is_Strike), default = 0),
      # Create Count string - ALWAYS starts at 0-0 for first pitch in PA
      Count = paste0(Running_Balls, "-", Running_Strikes)
    ) %>%
    ungroup() %>%
    mutate(
      `Pitch Type` = PitchType,
      IVB = round(InducedVertBreak, 1),
      HB = round(HorzBreak, 1),
      `Spin Rate` = round(SpinRate, 0),
      Tilt = spin_to_tilt(SpinAxis),
      `Release Height` = round(RelHeight, 1),
      `Release Side` = round(RelSide, 1),
      Extension = round(Extension, 2)
    ) %>%
    # Remove temporary columns and select final columns
    select(`Pitch #`, `PA #`, `Pitch # in PA`, Inning, Outs, Count, `Pitch Type`, IVB, HB, `Spin Rate`, Tilt, 
           `Release Height`, `Release Side`, Extension, Result) %>%
    slice_head(n = 50)  # Limit for display
  
  # Debug: Print first 3 PAs to verify correct count progression
  debug_pas <- pitch_log %>%
    filter(`PA #` <= 3) %>%
    select(`Pitch #`, `PA #`, `Pitch # in PA`, Count, `Pitch Type`, Result)
  
  print("First 3 PAs for debugging:")
  print(debug_pas)
  
  # Create table with smaller font to fit more columns
  page4_table <- tableGrob(pitch_log, rows = NULL, 
                          theme = ttheme_default(
                            core = list(fg_params = list(cex = 0.7)),  # Smaller text
                            colhead = list(fg_params = list(cex = 0.7, fontface = "bold"))  # Smaller headers
                          ))
  
  page4 <- grid.arrange(
    textGrob(paste("Page 4 - Pitch Log (First 50 Pitches):", pitcher_name), gp = gpar(fontsize = 16, fontface = "bold")),
    page4_table,
    heights = c(0.5, 6)
  )
  
  # Save all pages
  clean_pitcher_name <- gsub("[, ]", "_", pitcher_name)
  
  ggsave(paste0(clean_pitcher_name, "_Page1_Summary_", filename_date, ".pdf"), page1, width = 12, height = 16)
  ggsave(paste0(clean_pitcher_name, "_Page2_vs_LHB_", filename_date, ".pdf"), page2, width = 12, height = 16)
  ggsave(paste0(clean_pitcher_name, "_Page3_vs_RHB_", filename_date, ".pdf"), page3, width = 12, height = 16)
  ggsave(paste0(clean_pitcher_name, "_Page4_PitchLog_", filename_date, ".pdf"), page4, width = 12, height = 16)
  
  print(paste("Generated comprehensive 4-page report for", pitcher_name))
}

# ORIGINAL SINGLE-PAGE FUNCTION (UNCHANGED)
create_trackman_report <- function(data, pitcher_name) {
  
  # Filter data for the specified pitcher using player_name instead of Pitcher
  pitcher_data <- data %>%
    filter(player_name == pitcher_name)
  
  # Map Baseball Savant column names to your expected names
  # You'll need to adjust these based on what columns are actually in your data
  pitcher_data <- pitcher_data %>%
    mutate(
      # Map Baseball Savant names to your code's expected names
      PitchType = if("pitch_name" %in% names(.)) pitch_name else if("pitch_type" %in% names(.)) pitch_type else NA,
      RelSpeed = if("release_speed" %in% names(.)) release_speed else NA,
      # Try multiple possible spin rate column names
      SpinRate = if("release_spin_rate" %in% names(.)) release_spin_rate
      else if("release_spin" %in% names(.)) release_spin 
      else if("spin_rate" %in% names(.)) spin_rate 
      else if("spin_rate_deprecated" %in% names(.)) spin_rate_deprecated 
      else NA,
      InducedVertBreak = if("pfx_z" %in% names(.)) pfx_z * 12 else NA,  # Convert feet to inches
      HorzBreak = if("pfx_x" %in% names(.)) pfx_x * 12 else NA,  # Convert feet to inches
      SpinAxis = if("spin_axis" %in% names(.)) spin_axis else NA,
      Extension = if("release_extension" %in% names(.)) release_extension else NA,
      PlateLocSide = if("plate_x" %in% names(.)) plate_x else NA,
      PlateLocHeight = if("plate_z" %in% names(.)) plate_z else NA,
      RelSide = if("release_pos_x" %in% names(.)) release_pos_x else NA,
      RelHeight = if("release_pos_z" %in% names(.)) release_pos_z else NA,
      ArmAngle = if("arm_angle" %in% names(.)) arm_angle else NA
    ) %>%
    # Convert pitch names to abbreviations
    mutate(
      PitchType = case_when(
        grepl("4-Seam|Four-Seam|Fastball", PitchType, ignore.case = TRUE) ~ "FF",
        grepl("Sinker|2-Seam|Two-Seam", PitchType, ignore.case = TRUE) ~ "SI", 
        grepl("Cutter", PitchType, ignore.case = TRUE) ~ "CT",
        grepl("Changeup|Change", PitchType, ignore.case = TRUE) ~ "CH",
        grepl("Slider", PitchType, ignore.case = TRUE) ~ "SL",
        grepl("Sweeper", PitchType, ignore.case = TRUE) ~ "SW",
        grepl("Curveball|Curve|Knuckle.*Curve|Slow.*Curve", PitchType, ignore.case = TRUE) ~ "CB",
        grepl("Split|Splitter", PitchType, ignore.case = TRUE) ~ "SP",
        TRUE ~ PitchType  # Keep original if no match
      )
    )
  
  # Debug: Check what spin rate values we actually have
  print("Spin Rate column check:")
  print(paste("Has release_spin_rate:", "release_spin_rate" %in% names(data)))
  print(paste("Has release_spin:", "release_spin" %in% names(data)))
  print(paste("Has spin_rate:", "spin_rate" %in% names(data)))
  print(paste("SpinRate values summary:"))
  print(summary(pitcher_data$SpinRate))
  
  # Remove rows where PitchType is NA
  pitcher_data <- pitcher_data %>%
    filter(!is.na(PitchType))
  
  # Detect handedness and adjust horizontal break for lefties
  # For left-handed pitchers, flip the horizontal break sign
  # You can determine this from release position or add it manually
  is_lefty <- mean(pitcher_data$RelSide, na.rm = TRUE) > 0  # Lefties typically release from positive x
  
  if(is_lefty) {
    pitcher_data <- pitcher_data %>%
      mutate(
        HorzBreak = -HorzBreak,  # Flip horizontal break for lefties
        ArmAngle = 180 - ArmAngle,  # Flip arm angle for lefties (mirror across 90°)
        RelSide = -RelSide  # Flip release side for lefties
      )
    print(paste("Detected left-handed pitcher:", pitcher_name, "- flipping horizontal break, arm angle, and release side values"))
  } else {
    print(paste("Detected right-handed pitcher:", pitcher_name))
  }
  
  # 1. Create pitch metrics summary table
  pitch_metrics <- pitcher_data %>%
    group_by(PitchType) %>%
    summarise(
      Count = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      `Usage%` = round((Count / sum(Count)) * 100, 1)
    ) %>%
    left_join(
      pitcher_data %>%
        group_by(PitchType) %>%
        summarise(
          `Max Velo` = round(max(RelSpeed, na.rm = TRUE), 1),
          `Avg Velo` = round(mean(RelSpeed, na.rm = TRUE), 1),
          `Spin Rate` = round(mean(SpinRate, na.rm = TRUE), 0),
          `Avg IVB` = round(mean(InducedVertBreak, na.rm = TRUE), 1),
          `Avg HB` = round(mean(HorzBreak, na.rm = TRUE), 1),
          `Avg Spin Axis` = mean(SpinAxis, na.rm = TRUE),
          Extension = round(mean(Extension, na.rm = TRUE), 1),
          .groups = 'drop'
        ) %>%
        mutate(
          Tilt = spin_to_tilt(`Avg Spin Axis`)
        ) %>%
        select(-`Avg Spin Axis`),
      by = "PitchType"
    ) %>%
    # Fix Tilt formatting and handle spin rate issues
    mutate(
      # Don't replace spin rate with 0 - let's see what the actual values are
      `Spin Rate` = ifelse(is.na(`Spin Rate`) | is.infinite(`Spin Rate`), NA, `Spin Rate`),
      Tilt = ifelse(substr(Tilt, 1, 2) == "0:", paste0("12", substr(Tilt, 2, nchar(Tilt))), Tilt)
    )
  
  # Create the plots using the helper functions
  movement_plot <- create_movement_plot(pitcher_data)
  release_plot <- create_release_plot(pitcher_data)
  tilt_plot <- create_tilt_plot(pitcher_data)
  velo_plot <- create_velocity_plot(pitcher_data)
  location_heatmap <- create_heatmaps_by_pitch(pitcher_data)
  
  # Get date range for the pitcher
  date_col <- NULL
  if ("game_date" %in% names(pitcher_data)) {
    date_col <- "game_date"
  } else if ("Date" %in% names(pitcher_data)) {
    date_col <- "Date"
  } else if ("GameDate" %in% names(pitcher_data)) {
    date_col <- "GameDate"
  }
  
  if (!is.null(date_col)) {
    min_date <- min(as.Date(pitcher_data[[date_col]], format = "%Y-%m-%d"), na.rm = TRUE)
    max_date <- max(as.Date(pitcher_data[[date_col]], format = "%Y-%m-%d"), na.rm = TRUE)
    
    if (min_date == max_date) {
      date_str <- format(min_date, "%y-%m-%d")
      filename_date <- format(min_date, "%Y-%m-%d")
    } else {
      date_str <- paste(format(min_date, "%y-%m-%d"), "to", format(max_date, "%y-%m-%d"))
      filename_date <- paste0(format(min_date, "%Y-%m-%d"), "_to_", format(max_date, "%Y-%m-%d"))
    }
  } else {
    date_str <- "(Date Unknown)"
    filename_date <- "Unknown_Date"
  }
  
  report_title <- paste(pitcher_name, "-", date_str)
  
  # Use normal table without row numbers
  colored_table <- tableGrob(pitch_metrics, rows = NULL)
  
  # NEW LAYOUT: Name, Table, Movement|Release, Clock|Velocity, Heatmaps
  final_plot <- grid.arrange(
    # Row 1: Title/Name
    textGrob(report_title, gp = gpar(fontsize = 20, fontface = "bold")),
    # Row 2: Table
    colored_table,
    # Row 3: Movement Plot | Release Plot
    arrangeGrob(movement_plot, release_plot, ncol = 2),
    # Row 4: Clock | Velocity Consistency
    arrangeGrob(tilt_plot, velo_plot, ncol = 2),
    # Row 5: Heatmaps
    location_heatmap,
    heights = c(0.5, 2, 3, 3, 3)
  )
  
  # Create filename with pitcher name and date(s)
  clean_pitcher_name <- gsub("[, ]", "_", pitcher_name)
  filename <- paste0(clean_pitcher_name, "_", filename_date, ".pdf")
  ggsave(filename, final_plot, width = 12, height = 16)
}

# Example usage for both functions:
# Single page report (existing):
create_trackman_report(data, "Skubal, Tarik")

# Multi-page comprehensive report (new):
create_comprehensive_pitching_report(data, "Skubal, Tarik")