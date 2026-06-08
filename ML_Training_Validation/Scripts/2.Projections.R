#Get libraries and functions
source('C:/Users/User/Desktop/LaNE/Experimentos/ML_NTT/Model_Functions.R') 

# Set directory that the current file is in as the working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

set.seed(14)

# Getting RDS file containing training info
l.df.input.1 <- readRDS('C:/Users/User/Desktop/LaNE/Experimentos/ML_NTT/Data/CSVs/labeled_list.rds')
l.df.input.1 <- lapply(l.df.input.1, \(df) {colnames(df)[colnames(df) == "frame_row"] <- "X"; df}) #Making frame row called X

# Make sure the columns are with correct names
l.df.input.1 <- lapply(l.df.input.1, \(df) {df <- df[, c('X','x_head','y_head','x_body','y_body','x_tail','y_tail','Behavior.label')]; df})

# Make sure columns 1 to 7 are numeric 
l.df.input.1 <- lapply(l.df.input.1, function(x) {x[1:7] <- lapply(x[1:7], as.numeric); x})

# Convert pixel values to cm - from column 2 to 7
cm = 38
l.df.input.1 <- mapply(function(x, y) {x[2:7] <- x[2:7] * y; x}, l.df.input.1, cm, SIMPLIFY = FALSE)

# Remove column 1.frames and 8.behavior label to run the postural analysis
l.df.input.2 <- lapply(l.df.input.1, function(x) {as.data.frame(x[-c(1, 8)])})


# Now start creating list of dataframes with postural information
l.df.posture.info <- lapply(l.df.input.2, pairwise_distances, normalize=FALSE)
l.df.posture.info <- mapply(function(x, y) {out <- cbind(x, all_angle_combinations(y)); out}, l.df.posture.info, l.df.input.2, SIMPLIFY=FALSE)
l.df.posture.info <- mapply(function(x, y) {out <- cbind(x[1:(nrow(x)-1),], project_point_df(y, col.basis.start=c('x_body', 'y_body'), col.basis.end=c('x_head','y_head'))); out},
                            l.df.posture.info, l.df.input.2, SIMPLIFY=FALSE) 
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$angular.velocity <- c(0, diff(x$angle.x_head.x_body.x_tail)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$angular.acceleration <- c(0, diff(x$angular.velocity)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$x_head_acc <- c(0, diff(x$x_head.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$y_head_acc <- c(0, diff(x$y_head.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$x_body_acc <- c(0, diff(x$x_body.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$y_body_acc <- c(0, diff(x$y_body.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$x_tail_acc <- c(0, diff(x$x_tail.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$y_tail_acc <- c(0, diff(x$y_tail.proj)); x})

# Bring back the behavior label and X
l.df.input.3 <- lapply(l.df.input.1, function(x) {head(x, -1)})
l.df.input <- mapply(function(df, m) {df$behavior.label <- m$Behavior.label; df},
                     l.df.posture.info, l.df.input.3, SIMPLIFY=FALSE)
l.df.input <- mapply(function(df, m) {df$X <- m$X; df},
                     l.df.input, l.df.input.3, SIMPLIFY=FALSE)

# Organize order of columns
l.df.input <- lapply(l.df.input, function(x) {x[c(20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)]})

# Create dataframes w/ the sliding window of 10 frames (STANDARD)
n.frame.after <- 5
n.frame.before <- 5

# =========================================================
# WINDOW SIZES
# =========================================================

short.before <- 3
short.after  <- 3

medium.before <- 7
medium.after  <- 7

long.before <- 15
long.after  <- 15


# Make dataframes
l.df.sliding.window <- lapply(l.df.input, function(x) {out <- data.frame(frame=x$X); out})


# Count how many times animals were displaying angles below 90 or 270 between head, trunk and tail
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$n.turn.angle <- as.numeric(slide(df.input$angle.x_head.x_body.x_tail, function(y) {sum(y < 90, y > 270)}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

# Adding in angular information
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.turn.angle <- as.numeric(slide(df.input$angle.x_head.x_body.x_tail, function(y) {mean(abs(180-y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.turn.angle <- as.numeric(slide(df.input$angle.x_head.x_body.x_tail, function(y) {mean(180-y)}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.ang.vel <- as.numeric(slide(df.input$angular.velocity, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.ang.vel <- as.numeric(slide(df.input$angular.velocity, function(y) {mean(y)}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

# Add in distance between point information
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$dist.head.trunk <- as.numeric(slide(df.input$dist.x_head.x_body, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$dist.head.tail <- as.numeric(slide(df.input$dist.x_head.x_tail, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$dist.trunk.tail <- as.numeric(slide(df.input$dist.x_body.x_tail, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)


#Projections - tail, trunk, and head relative to fish heading
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.tail.vel.x <- as.numeric(slide(df.input$x_tail.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.tail.vel.x <- as.numeric(slide(df.input$x_tail.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.tail.vel.y <- as.numeric(slide(df.input$y_tail.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.tail.vel.y <- as.numeric(slide(df.input$y_tail.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.trunk.vel.x <- as.numeric(slide(df.input$x_body.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.trunk.vel.x <- as.numeric(slide(df.input$x_body.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.trunk.vel.y <- as.numeric(slide(df.input$y_body.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.trunk.vel.y <- as.numeric(slide(df.input$y_body.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.head.vel.x <- as.numeric(slide(df.input$x_head.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.head.vel.x <- as.numeric(slide(df.input$x_head.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.head.vel.y <- as.numeric(slide(df.input$y_head.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.head.vel.y <- as.numeric(slide(df.input$y_head.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)


#Acceleration for the angles, head, trunk,tail
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.tail.acc.x <- as.numeric(slide(df.input$x_tail_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.tail.acc.x <- as.numeric(slide(df.input$x_tail_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.tail.acc.y <- as.numeric(slide(df.input$y_tail_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.tail.acc.y <- as.numeric(slide(df.input$y_tail_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.trunk.acc.x <- as.numeric(slide(df.input$x_body_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.trunk.acc.x <- as.numeric(slide(df.input$x_body_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.trunk.acc.y <- as.numeric(slide(df.input$y_body_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.trunk.acc.y <- as.numeric(slide(df.input$y_body_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.head.acc.x <- as.numeric(slide(df.input$x_head_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.head.acc.x <- as.numeric(slide(df.input$x_head_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.head.acc.y <- as.numeric(slide(df.input$y_head_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.head.acc.y <- as.numeric(slide(df.input$y_head_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.ang.acc <- as.numeric(slide(df.input$angular.acceleration, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.ang.acc <- as.numeric(slide(df.input$angular.acceleration, function(y) {mean(y)}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)


#Variability in angle and velocity parameters to help understand HS and thrasing
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$sd.turn.angle <- as.numeric(slide(df.input$angle.x_head.x_body.x_tail, sd, .before=medium.before, .after=medium.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$sd.ang.vel <- as.numeric(slide(df.input$angular.velocity, sd, .before=long.before, .after=long.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$sd.ang.acc <- as.numeric(slide(df.input$angular.acceleration, sd, .before=medium.before, .after=medium.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$sum.abs.ang.vel <- as.numeric(slide(abs(df.input$angular.velocity), sum, .before=long.before, .after=long.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$n.direction.switch <- as.numeric(slide(df.input$angular.velocity, function(y) {sum(diff(sign(y)) != 0, na.rm=TRUE)}, .before=long.before, .after=long.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$tail.y.range <- as.numeric(slide(df.input$y_tail.proj, function(y) {diff(range(y, na.rm=TRUE))}, .before=long.before, .after=long.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

# =========================================================
# CENTROID VARIABLES
# =========================================================

l.df.input <- lapply(l.df.input, function(x) {
  x$centroid.x <- rowMeans(x[,c("x_head.proj","x_body.proj","x_tail.proj")], na.rm=TRUE)
  x
})

l.df.input <- lapply(l.df.input, function(x) {
  x$centroid.y <- rowMeans(x[,c("y_head.proj","y_body.proj","y_tail.proj")], na.rm=TRUE)
  x
})

l.df.input <- lapply(l.df.input, function(x) {
  x$centroid.speed <- c(0, sqrt(diff(x$centroid.x)^2 + diff(x$centroid.y)^2))
  x
})

l.df.input <- lapply(l.df.input, function(x) {
  x$centroid.acc <- c(0, diff(x$centroid.speed))
  x
})

l.df.input <- lapply(l.df.input, function(x) {
  x$centroid.jerk <- c(0, diff(x$centroid.acc))
  x
})

# CENTROID SPEED FEATURES
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$mean.speed <- as.numeric(slide(df.input$centroid.speed, mean, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$sd.speed <- as.numeric(slide(df.input$centroid.speed, sd, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$max.speed <- as.numeric(slide(df.input$centroid.speed, max, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

# CENTROID ACCELERATION / JERK
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$mean.acc <- as.numeric(slide(df.input$centroid.acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$sd.acc <- as.numeric(slide(df.input$centroid.acc, sd, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$mean.jerk <- as.numeric(slide(df.input$centroid.jerk, mean, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)


# =========================================================
# HEADING VARIABLES
# =========================================================

l.df.input <- lapply(l.df.input, function(x) {
  x$heading.angle <- c(0, atan2(diff(x$centroid.y), diff(x$centroid.x)))
  x
})

l.df.input <- lapply(l.df.input, function(x) {
  x$heading.change <- c(0, abs(diff(x$heading.angle)))
  x
})

# HEADING / TURNING FEATURES
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$mean.heading.change <- as.numeric(slide(df.input$heading.change, mean, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$sd.heading.change <- as.numeric(slide(df.input$heading.change, sd, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$cumulative.turning <- as.numeric(slide(df.input$heading.change, sum, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

# TAIL OSCILLATION FEATURES
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$tail.zero.crossings <- as.numeric(slide(df.input$y_tail.proj, function(y) {sum(diff(sign(y)) != 0, na.rm=TRUE)}, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$tail.energy <- as.numeric(slide(df.input$y_tail.proj, function(y) {sum(y^2, na.rm=TRUE)}, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$tail.sd <- as.numeric(slide(df.input$y_tail.proj, sd, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

# IMMOBILITY FEATURES
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$prop.low.motion <- as.numeric(slide(df.input$centroid.speed, function(y) {mean(y < 0.05, na.rm=TRUE)}, .before=long.before, .after=long.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

# BURST FEATURES
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$n.burst.events <- as.numeric(slide(df.input$centroid.speed, function(y) {sum(y > 0.5, na.rm=TRUE)}, .before=short.before, .after=short.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$max.burst.speed <- as.numeric(slide(df.input$centroid.speed, max, .before=short.before, .after=short.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

# SPATIAL / BOTTOM FEATURES
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$prop.bottom.time <- as.numeric(slide(df.input$centroid.y, function(y) {mean(y < 5, na.rm=TRUE)}, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$sd.vertical.position <- as.numeric(slide(df.input$centroid.y, sd, .before=n.frame.before, .after=n.frame.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)


#NEW BEHAVIORS TO HELP WITH HS AND TR
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$max.tail.vel.y <- as.numeric(slide(abs(df.input$y_tail.proj), max, .before=short.before, .after=short.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$max.head.vel.x <- as.numeric(slide(abs(df.input$x_head.proj), max, .before=short.before, .after=short.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$max.trunk.vel.x <- as.numeric(slide(abs(df.input$x_body.proj), max, .before=short.before, .after=short.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.input <- lapply(l.df.input, function(x) {x$tail.head.diff.y <- x$y_tail.proj - x$y_head.proj; x})
l.df.input <- lapply(l.df.input, function(x) {x$tail.body.diff.y <- x$y_tail.proj - x$y_body.proj; x})
l.df.input <- lapply(l.df.input, function(x) {x$body.curvature.index <- abs(x$y_head.proj - x$y_body.proj) + abs(x$y_tail.proj - x$y_body.proj); x})

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$mean.tail.head.diff.y <- as.numeric(slide(abs(df.input$tail.head.diff.y), mean, .before=medium.before, .after=medium.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$mean.tail.body.diff.y <- as.numeric(slide(abs(df.input$tail.body.diff.y), mean, .before=medium.before, .after=medium.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$max.body.curvature <- as.numeric(slide(df.input$body.curvature.index, max, .before=medium.before, .after=medium.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$sd.body.curvature <- as.numeric(slide(df.input$body.curvature.index, sd, .before=medium.before, .after=medium.after)); df.slide}, l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

#Add in mode of behavioral label
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$behavior.label <- as.character(slide(df.input$behavior.label, getmode, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)


#Now put all the data together and remove the NA frames to make a dataframe for training the model
df.training.data.labels <- bind_rows(l.df.sliding.window)

#Remove NA columns (only for training)
df.training.data.labels <- na.omit(df.training.data.labels)
data.frame(table(df.training.data.labels$behavior.label))

#Save file in RDS format
saveRDS(df.training.data.labels, 'C:/Users/User/Desktop/LaNE/Experimentos/ML_NTT/Data/Training_Data.21.05.rds')
df.training.data.labels <- readRDS('C:/Users/User/Desktop/LaNE/Experimentos/ML_NTT/Data/Training_Data.21.05.rds')

#Separate depending on behavior label
df.NB <- df.training.data.labels[which(df.training.data.labels$behavior.label == "NB"),]
df.IM <- df.training.data.labels[which(df.training.data.labels$behavior.label == "IM"),]
df.HS <- df.training.data.labels[which(df.training.data.labels$behavior.label == "HS"),]
df.TR <- df.training.data.labels[which(df.training.data.labels$behavior.label == "TR"),]
df.BS <- df.training.data.labels[which(df.training.data.labels$behavior.label == "BS"),]


#Splitting dataframes - Normal Behavior 80% for training 20% testing
data_set_size1= floor(nrow(df.NB)*0.80)
index.NB <- sample(1:nrow(df.NB), size = data_set_size1)
df.NB.Training80 <- df.NB[index.NB,]
df.NB.Testing20 <- df.NB[-index.NB,]


#Splitting dataframes - Immobility 80% for training 20% testing
data_set_size2= floor(nrow(df.IM)*0.80)
index.IM <- sample(1:nrow(df.IM), size = data_set_size2)
df.IM.Training80 <- df.IM[index.IM,]
df.IM.Testing20 <- df.IM[-index.IM,]

#Splitting dataframes - HS 80% for training 20% testing
data_set_size5= floor(nrow(df.HS)*0.80)
index.HS <- sample(1:nrow(df.HS), size = data_set_size5)
df.HS.Training80 <- df.HS[index.HS,]
df.HS.Testing20 <- df.HS[-index.HS,]

#Splitting dataframes - TR 80% for training 20% testing
data_set_size5= floor(nrow(df.TR)*0.80)
index.TR <- sample(1:nrow(df.TR), size = data_set_size5)
df.TR.Training80 <- df.TR[index.TR,]
df.TR.Testing20 <- df.TR[-index.TR,]

#Splitting dataframes - BS 80% for training 20% testing
data_set_size5= floor(nrow(df.BS)*0.80)
index.BS <- sample(1:nrow(df.BS), size = data_set_size5)
df.BS.Training80 <- df.BS[index.BS,]
df.BS.Testing20 <- df.BS[-index.BS,]


#Now lets combine the files and create the rds for training the model (max 1:1) - BURST WAS RHSOVED AS IT COOCURRED WITH HS AND TR
df.Training.Final <- rbind(df.NB.Training80, df.IM.Training80, df.TR.Training80, df.HS.Training80)

#Data for testing
df.Testing.Final <- rbind(df.NB.Testing20, df.IM.Testing20, df.TR.Testing20, df.HS.Testing20)

#Count to check amount of behaviors labelled 
data.frame(table(df.Training.Final$behavior.label))

#Save file in RDS format - using 14k max
saveRDS(df.Training.Final, 'C:/Users/User/Desktop/LaNE/Experimentos/ML_NTT/Data/df_training_15.rds')
saveRDS(df.Testing.Final, 'C:/Users/User/Desktop/LaNE/Experimentos/ML_NTT/Data/df_testing_15.rds')
