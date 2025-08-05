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
  
  # 2. Create pitch movement plot
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
        annotate("segment", x = 0, y = 0, xend = x_end, yend = y_end, 
                 color = "red", size = 1, alpha = 0.7)
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
    scale_color_manual(values = pitch_colors) +
    theme_minimal() +
    labs(title = "Pitch Movement", 
         subtitle = "Pitcher View",
         x = "Horizontal Break (inches)", 
         y = "Induced Vertical Break (inches)")
  
  # 3. Create release point plot
  # Calculate average release point for each pitch type
  avg_release_points <- pitcher_data %>%
    group_by(PitchType) %>%
    summarise(
      avg_rel_side = mean(RelSide, na.rm = TRUE),
      avg_rel_height = mean(RelHeight, na.rm = TRUE),
      avg_arm_angle = mean(ArmAngle, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(!is.na(avg_arm_angle)) %>%
    mutate(
      # Calculate arm angle lines from central point to each pitch cluster
      # All lines start from the same central point (0, 4)
      line_start_x = 0,
      line_start_y = 4,
      # Lines extend to each pitch cluster center (showing arm angle trajectory)
      line_end_x = avg_rel_side,
      line_end_y = avg_rel_height
    )
  
    # Dynamic scaling for release plot
  rel_side_range <- range(pitcher_data$RelSide, na.rm = TRUE)
  rel_height_range <- range(pitcher_data$RelHeight, na.rm = TRUE)
  
  # Set x limits: default -2.5 to 2.5, expand to -4 to 4 if needed
  if (rel_side_range[1] < -2.5 || rel_side_range[2] > 2.5) {
    x_limits <- c(-4, 4)
  } else {
    x_limits <- c(-2.5, 2.5)
  }
  
  # Set y limits: default 0 to 7.5, expand to 0 to 10 if needed
  if (rel_height_range[2] > 7.5) {
    y_limits <- c(0, 10)
  } else {
    y_limits <- c(0, 7.5)
  }
  
  # Create realistic mound shape (10 inches high = 0.83 feet, 18 feet diameter)
  mound_angles <- seq(0, pi, length.out = 100)
  mound_data <- data.frame(
    x = 3 * cos(mound_angles),  # 18 feet diameter (9 foot radius)
    y = 0.83 * sin(mound_angles)  # 10 inch height with smooth curve
  )
  
  # Create pitching rubber inset into mound (24" x 6" regulation, slightly recessed)
  rubber_height <- 0.75  # Slightly below mound peak for realistic look
  rubber_data <- data.frame(
    x = c(-1, 1, 1, -1, -1),  # 24 inches = 2 feet long
    y = c(rubber_height, rubber_height, rubber_height + 0.05, rubber_height + 0.05, rubber_height)  # 6 inches = 0.5 feet wide, slightly raised
  )
  
  release_plot <- ggplot(pitcher_data, aes(x = RelSide, y = RelHeight, color = PitchType)) +
    # Add mound
    geom_polygon(data = mound_data, aes(x = x, y = y), 
                 fill = "#DEB887", color = "#8B7355", size = 0.8, inherit.aes = FALSE) +
    # Add pitching rubber (inset into mound)
    geom_polygon(data = rubber_data, aes(x = x, y = y), 
                 fill = "#F5F5F5", color = "#2F2F2F", size = 1.2, inherit.aes = FALSE) +
    # Add arm angle lines from central point to cluster centers
    geom_segment(data = avg_release_points, 
                 aes(x = line_start_x, y = line_start_y, 
                     xend = line_end_x, yend = line_end_y, color = PitchType),
                 size = 2, alpha = 0.8, inherit.aes = FALSE) +
    # Add the individual release points
    geom_point(alpha = 0.7, size = 2) +
    scale_color_manual(values = pitch_colors) +
    theme_minimal() +
    labs(title = "Release Point", 
         subtitle = "Pitcher View",
         x = "Release Side (feet)", 
         y = "Release Height (feet)") +
    coord_fixed(xlim = x_limits, ylim = y_limits)
  
  # 4. Create velocity consistency plot
  velo_plot <- ggplot(pitcher_data, aes(x = RelSpeed, fill = PitchType)) +
    geom_density(alpha = 0.6) +
    facet_grid(PitchType ~ .) +
    scale_fill_manual(values = pitch_colors) +
    theme_minimal() +
    labs(title = "Velocity Consistency",
         x = "Release Speed (mph)",
         y = "Density") +
    theme(legend.position = "none")
  
  # 5. Create tilt consistency plot using base ggplot2
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
        clock_positions <- c(dens$x, dens$x[1] + 12)
        densities <- c(dens$y, dens$y[1])
        
        # Convert clock positions to angles (same as corrected clock)
        angles <- (clock_positions - 3) * 30  # 3:00 = 0°, 12:00 = 270°
        
        # Normalize density to reasonable size
        max_dens <- max(densities)
        densities <- densities / max_dens * 0.3  # Scale to max 0.3 units from clock edge
        
        # Create polygon points
        inner_radius <- 0.8
        outer_radius <- inner_radius + densities
        
        # Convert to x,y coordinates (negative theta for clockwise)
        theta_rad <- angles * pi / 180
        
        # Create polygon (inner edge + outer edge)
        polygon_data <- data.frame(
          x = c(inner_radius * cos(-theta_rad), rev(outer_radius * cos(-theta_rad))),
          y = c(inner_radius * sin(-theta_rad), rev(outer_radius * sin(-theta_rad))),
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
    
    # Calculate average arm angle for each pitch type
    arm_angle_lines <- tilt_density_data %>%
      group_by(PitchType) %>%
      summarise(avg_arm_angle = mean(ArmAngle, na.rm = TRUE), .groups = 'drop') %>%
      filter(!is.na(avg_arm_angle)) %>%
      mutate(
        # Convert arm angle to radians (0° = right, 90° = up)
        theta = avg_arm_angle * pi / 180,
        # Create line from center to clock edge
        x_start = 0,
        y_start = 0,
        x_end = 0.75 * cos(theta),  # Stop just short of clock edge
        y_end = 0.75 * sin(theta)
      )
  } else {
    tilt_density_polygons <- NULL
    arm_angle_lines <- NULL
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
    # Average arm angle lines for each pitch type
    {
      if (!is.null(arm_angle_lines) && nrow(arm_angle_lines) > 0) {
        geom_segment(data = arm_angle_lines, aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = PitchType), 
                     size = 2, alpha = 0.8)
      }
    } +
    # Density polygons around the edge
    {
      if (!is.null(tilt_density_polygons) && nrow(tilt_density_polygons) > 0) {
        geom_polygon(data = tilt_density_polygons, aes(x = x, y = y, fill = PitchType), alpha = 0.6)
      }
    } +
    scale_color_manual(values = pitch_colors) +
    scale_fill_manual(values = pitch_colors) +
    coord_fixed(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
    theme_void() +
    labs(title = "Tilt Consistency") +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          plot.margin = margin(10, 10, 10, 10),
          legend.position = "none")
  
  # 6. Create location heatmap for each pitch type
  location_heatmap <- ggplot(pitcher_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    geom_density_2d_filled(alpha = 0.7) +
    geom_rect(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
              fill = NA, color = "black", size = 1) +
    facet_wrap(~ PitchType) +
    coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
    theme_minimal() +
    labs(title = "Location Heatmaps by Pitch Type",
         x = "Horizontal Location (feet)", 
         y = "Vertical Location (feet)") +
    theme(legend.position = "none")
  
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
  
  # Use normal table without any coloring attempts
  colored_table <- tableGrob(pitch_metrics)
  
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
  filename <- paste0(clean_pitcher_name, "_", filename_date, ".png")
  ggsave(filename, final_plot, width = 12, height = 16)
}

# Example usage
create_trackman_report(data, "Skubal, Tarik")