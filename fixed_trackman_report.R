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

# Set working directory
setwd("C:/Users/cmurr/OneDrive/Desktop/Athlete Lab Showcase Reports")

# Read the data and print structure
data <- read.csv("statcast.csv")

print("Column names in the data:")
print(names(data))

print("\nAvailable pitchers:")
print(unique(data$player_name))

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
  
  # 2. Create velocity consistency plot
  velo_plot <- ggplot(pitcher_data, aes(x = RelSpeed, fill = PitchType)) +
    geom_density(alpha = 0.6) +
    facet_grid(PitchType ~ .) +
    theme_minimal() +
    labs(title = "Velocity Consistency") +
    theme(legend.position = "none")
  
  # 3. Create pitch locations plot
  location_plot <- ggplot(pitcher_data, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchType)) +
    geom_point() +
    geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
              fill = NA, color = "black") +
    coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
    theme_minimal() +
    labs(title = "Pitch Locations")
  
  # 4. Create pitch movement plot
  # Get the primary fastball (FF or SI - whichever is thrown more) and its average arm angle
  primary_fastball <- pitch_metrics %>%
    filter(PitchType %in% c("FF", "SI")) %>%
    arrange(desc(`Usage%`)) %>%
    slice(1) %>%
    pull(PitchType)
  
  # If no FF or SI found, fall back to most used pitch
  if(length(primary_fastball) == 0) {
    primary_fastball <- pitch_metrics %>%
      arrange(desc(`Usage%`)) %>%
      slice(1) %>%
      pull(PitchType)
  }
  
  avg_arm_angle <- pitcher_data %>%
    filter(PitchType == primary_fastball) %>%
    summarise(avg_angle = mean(ArmAngle, na.rm = TRUE)) %>%
    pull(avg_angle)
  
  # Create tick mark data for every 10 inches
  tick_marks <- data.frame(
    x_ticks = rep(seq(-20, 20, 10), each = 2),
    y_ticks = rep(c(-1, 1), times = 5),
    y_ticks_x = rep(seq(-20, 20, 10), each = 2),
    x_ticks_y = rep(c(-1, 1), times = 5)
  )
  
  # Calculate average movement for each pitch type
  avg_movement <- pitcher_data %>%
    group_by(PitchType) %>%
    summarise(
      avg_horz = mean(HorzBreak, na.rm = TRUE),
      avg_vert = mean(InducedVertBreak, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    left_join(pitch_metrics %>% select(PitchType, `Usage%`), by = "PitchType")
  
  movement_plot <- ggplot(pitcher_data, aes(x = HorzBreak, y = InducedVertBreak, color = PitchType)) +
    # Add intercept lines
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    geom_vline(xintercept = 0, color = "black", size = 0.5) +
    # Add tick marks every 10 inches
    geom_segment(data = tick_marks, aes(x = x_ticks, xend = x_ticks, y = y_ticks, yend = -y_ticks), 
                 color = "black", size = 0.3, inherit.aes = FALSE) +
    geom_segment(data = tick_marks, aes(x = x_ticks_y, xend = -x_ticks_y, y = y_ticks_x, yend = y_ticks_x), 
                 color = "black", size = 0.3, inherit.aes = FALSE) +
    # Add arm angle line if available
    {
      if (!is.na(avg_arm_angle)) {
        # Convert arm angle to radians and create line endpoints
        angle_rad <- avg_arm_angle * pi / 180
        line_length <- 25  # Extend to edge of plot
        x_end <- line_length * cos(angle_rad)
        y_end <- line_length * sin(angle_rad)
        geom_segment(aes(x = 0, y = 0, xend = x_end, yend = y_end), 
                     color = "red", size = 1, alpha = 0.7, inherit.aes = FALSE)
      }
    } +
    # Add the individual pitch points
    geom_point(alpha = 0.6) +
    # Add average movement dots with bold black borders - all same size
    # First layer: Bold black border (larger)
    geom_point(data = avg_movement, aes(x = avg_horz, y = avg_vert, color = PitchType), 
               color = "black", size = 6, stroke = 0, shape = 16, inherit.aes = FALSE) +
    # Second layer: Colored fill (smaller)
    geom_point(data = avg_movement, aes(x = avg_horz, y = avg_vert, color = PitchType), 
               size = 4, stroke = 0, shape = 16, inherit.aes = FALSE) +
    coord_fixed(xlim = c(-25, 25), ylim = c(-25, 25)) +
    theme_minimal() +
    labs(title = "Pitch Movements", 
         subtitle = if(!is.na(avg_arm_angle)) paste0("Red line: ", primary_fastball, " avg arm angle (", round(avg_arm_angle, 1), "°). Large dots = avg movement") else "Large dots = avg movement")
  
  # 5. Create pitch usage pie chart
  usage_plot <- pitcher_data %>%
    count(PitchType) %>%
    mutate(pct = n/sum(n)) %>%
    ggplot(aes(x = "", y = pct, fill = PitchType)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(title = "Pitch Usage %")
  
  # 6. Create release points plot
  release_plot <- ggplot(pitcher_data, aes(x = RelSide, y = RelHeight, color = PitchType)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Pitch Release Points")
  
  # 7. Create tilt consistency plot using base ggplot2
  # Create smaller clock face
  clock_face <- create_circle(radius = 0.8)
  
  # Numbers inside the clock face - 12 at top, going clockwise
  clock_angles <- seq(0, 2*pi, length.out = 13)[-13]  # 12 positions
  clock_angles <- -clock_angles + pi/2  # Rotate so 12 is at top and reverse direction for clockwise
  
  clock_labels <- data.frame(
    x = 0.65 * cos(clock_angles),
    y = 0.65 * sin(clock_angles),
    label = c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  )
  
  # Create tilt density data around the clock
  if ("SpinAxis" %in% names(pitcher_data)) {
    # Create density polygons for each pitch type
    tilt_density_data <- pitcher_data %>%
      filter(!is.na(SpinAxis)) %>%
      mutate(
        normalized_spin = (SpinAxis + 360) %% 360,
        # Convert to clock decimal for density calculation
        clock_decimal = (normalized_spin / 30 + 6) %% 12,
        clock_decimal = ifelse(clock_decimal == 0, 12, clock_decimal)
      )
    
    # Create density curves for each pitch type
    tilt_polygons <- list()
    
    for(pitch in unique(tilt_density_data$PitchType)) {
      pitch_data <- tilt_density_data %>% filter(PitchType == pitch)
      
      if(nrow(pitch_data) > 1) {
        # Create circular density
        dens <- density(pitch_data$clock_decimal, bw = 0.3, from = 0, to = 12, n = 100)
        
        # Extend density to wrap around (connect 12 to 0)
        angles <- c(dens$x, dens$x[1] + 12) * 30 - 90  # Convert to degrees, offset for 12 at top
        densities <- c(dens$y, dens$y[1])
        
        # Normalize density to reasonable size
        max_dens <- max(densities)
        densities <- densities / max_dens * 0.3  # Scale to max 0.3 units from clock edge
        
        # Create polygon points
        inner_radius <- 0.8
        outer_radius <- inner_radius + densities
        
        # Convert to x,y coordinates
        theta_rad <- angles * pi / 180
        
        # Create polygon (inner edge + outer edge)
        polygon_data <- data.frame(
          x = c(inner_radius * cos(theta_rad), rev(outer_radius * cos(theta_rad))),
          y = c(inner_radius * sin(theta_rad), rev(outer_radius * sin(theta_rad))),
          PitchType = pitch
        )
        
        tilt_polygons[[pitch]] <- polygon_data
      }
    }
    
    # Combine all polygons
    if(length(tilt_polygons) > 0) {
      tilt_density_polygons <- do.call(rbind, tilt_polygons)
    } else {
      tilt_density_polygons <- NULL
    }
  } else {
    tilt_density_polygons <- NULL
  }
  
  tilt_plot <- ggplot() +
    # Clock face background (light gray fill)
    geom_polygon(data = clock_face, aes(x = x, y = y), fill = "gray95", color = "black", size = 1) +
    # Hour marks (small lines from edge toward center)
    geom_segment(aes(x = 0.8 * cos(clock_angles),
                     y = 0.8 * sin(clock_angles),
                     xend = 0.7 * cos(clock_angles),
                     yend = 0.7 * sin(clock_angles)),
                 color = "black", size = 0.5) +
    # Clock numbers inside
    geom_text(data = clock_labels, aes(x = x, y = y, label = label), size = 4, fontface = "bold") +
    # Density polygons around the edge
    {
      if (!is.null(tilt_density_polygons) && nrow(tilt_density_polygons) > 0) {
        geom_polygon(data = tilt_density_polygons, aes(x = x, y = y, fill = PitchType), alpha = 0.6)
      }
    } +
    coord_fixed(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
    theme_void() +
    labs(title = "Tilt Consistency") +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          plot.margin = margin(10, 10, 10, 10),
          legend.position = "none")
  
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
  
  # Combine all plots using gridExtra
  final_plot <- grid.arrange(
    tableGrob(pitch_metrics),
    velo_plot,
    arrangeGrob(location_plot, movement_plot, ncol = 2),
    arrangeGrob(usage_plot, release_plot, tilt_plot, ncol = 3),
    heights = c(2, 2, 3, 3),
    top = textGrob(report_title, gp = gpar(fontsize = 20, fontface = "bold"))
  )
  
  # Create filename with pitcher name and date(s)
  clean_pitcher_name <- gsub("[, ]", "_", pitcher_name)
  filename <- paste0(clean_pitcher_name, "_", filename_date, ".png")
  ggsave(filename, final_plot, width = 12, height = 16)
}

# Example usage
create_trackman_report(data, "Skubal, Tarik")