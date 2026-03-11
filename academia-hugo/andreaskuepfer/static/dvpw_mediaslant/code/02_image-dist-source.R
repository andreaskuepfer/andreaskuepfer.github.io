library(here)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(viridis)
library(scales)
library(RColorBrewer)
library(fields)
library(scales)
library(jpeg)

# baysian regression models using STAN
library(brms)
#remotes::install_github("stan-dev/cmdstanr")
library(cmdstanr)
#install_cmdstan()
library(posterior)
library(bayesplot)
library(diagram)


#################
### LOAD DATA ###
#################

load(here("Code/oli/scaling_refined/multilevel_scaling/dyad_sample_multilevel.Rdata")) # sample of 1,000 images
#load(here("Code/oli/scaling_refined/multilevel_scaling/dyad_sample_multilevel_N250.Rdata"))
#load(here("Code/oli/scaling_refined/multilevel_scaling/dyad_sample_multilevel_N500.Rdata"))

df <- dyad_sample

# N = 1,000
#fit <- readRDS(here("Code/oli/scaling_refined/estimated_models/fit_image_model_downsampled_N1000.Rds"))
#posterior_draws <- posterior::as_draws_matrix(fit$draws("z_img"))
#
#saveRDS(posterior_draws, 
#        file = here("Code/oli/scaling_refined/estimated_models/posterior_draws_image_model_downsampled_N1000.Rds"))

posterior_draws <- readRDS(here("Code/oli/scaling_refined/estimated_models/posterior_draws_image_model_downsampled_N1000.Rds"))

###################
### Sample Prep ###
###################

# undirected dyads
df$dyad <- apply(df[, c("source_A", "source_B")], 1, 
                 function(x) paste(sort(x), collapse = "_"))

# delete identical images
df <- df[df$eucl_dist != 0,]
df <- df[df$cos_sim != 1,]

# alphabetically ordered outlet-level data frame
outlets <- data.frame(outlet = sort(unique(c(df$source_A, df$source_B))),
                      num_index = NA)
outlets$num_index <- 1:nrow(outlets)

df$out_i <- outlets$num_index[match(df$source_A, outlets$outlet)]
df$out_j <- outlets$num_index[match(df$source_B, outlets$outlet)]

# alphabetically & numerically sorted image data frame
images <- data.frame(image = sort(unique(c(df$img_A, df$img_B))),
                     num_index = NA)
images$num_index <- 1:nrow(images)
images$outlet <- df$out_i[match(images$image, df$img_A)]
images$outlet[is.na(images$outlet)] <- 
  df$out_j[match(images$image[is.na(images$outlet)], df$img_B)]

outlet_of_image <- images$outlet

df$img_i <- images$num_index[match(df$img_A, images$image)]
df$img_j <- images$num_index[match(df$img_B, images$image)]

#posterior_draws <- 
#  readRDS(here("Code/oli/scaling_refined/estimated_models/posterior_draws_image_model_downsampled_N500.Rds"))




##########################
### Post-hoc alignment ###
##########################

# (resolve sign-flipping issue)

all_draws_df <- as_draws_df(posterior_draws)
all_draws_df <- as.data.frame(all_draws_df)

# Pick an anchor outlet: Fox News
anchor_image <- 1
unique(df$img_A[df$img_i == 1])

#plot(density(all_draws_df[[paste0("z_img[", anchor_image, ",1]")]]))

# Compute mean across outlets for each iteration
z_anchor <- all_draws_df[[paste0("z_img[", anchor_image, ",1]")]]

# Flip sign of entire latent vector for iterations where anchor < 0
table(z_anchor > 0)
flip_idx <- which(z_anchor < 0)
all_draws_df_aligned <- all_draws_df


z_vars <- grep("^z_", colnames(all_draws_df), value = TRUE)
all_draws_df_aligned[flip_idx, z_vars] <- -all_draws_df[flip_idx, z_vars]

#plot(density(all_draws_df_aligned[[paste0("z_img[", anchor_image, ",1]")]]),
#     xlim = c(-0.6, 0.6))

################################
### Organize posterior draws ###
################################

images$z_mean <- NA
images$z_lo95 <- NA
images$z_hi95 <- NA

images$z_posterior <- NA


for (i in 1:nrow(images)) {
  # z: outlet latent coordinate
  images$z_mean[i] <- mean(all_draws_df_aligned[[paste0("z_img[", i, ",1]")]])
  images$z_lo95[i] <- quantile(all_draws_df_aligned[[paste0("z_img[", i, ",1]")]], 0.025)
  images$z_hi95[i] <- quantile(all_draws_df_aligned[[paste0("z_img[", i, ",1]")]], 0.975)
  
  images$z_posterior[i] <- list(all_draws_df_aligned[[paste0("z_img[", i, ",1]")]])
}

images$outlet_name <- outlets$outlet[match(images$outlet, outlets$num_index)]

#####################################
### Descriptive plot of the scale ###
#####################################

# function to add images to plot
add_image <- function(img_name, 
                      x_center,
                      y_center,
                      num,
                      num_right = T){
  z_img_tmp <- images$z_mean[images$image == img_name]
  
  # plot point
  points(x = z_img_tmp,
         y = 0,
         pch = 21,
         lwd = 4,
         cex = 2,
         col = cividis(8, 1)[1],
         bg = cividis(8, 1)[5])
  text(x = z_img_tmp,
       y = 0.05,
       cex = 1,
       labels = as.character(num),
       font = 2,
       pos = 3)
  
  
  # plot image
  image_path <- here("Code/oli/text_image_scaling/x_presentation/img/")
  
  img_tmp <- readJPEG(paste0(image_path, img_name))
  
  # img ratio
  img_ratio  <- dim(img_tmp)[2] / dim(img_tmp)[1]  # width / height
  
  # plot region user coords and physical size
  usr <- par("usr")   # c(xmin, xmax, ymin, ymax)
  pin <- par("pin")   # c(width_in_inches, height_in_inches) of plotting region (not margins)
  
  # units-per-inch on each axis
  x_units_per_inch <- (usr[2] - usr[1]) / pin[1]
  y_units_per_inch <- (usr[4] - usr[3]) / pin[2]
  
  # choose desired image height as a FRACTION of plot height (0..1)
  img_height_frac <- 0.15                # <-- tweak this (fraction of plotting region height)
  img_height_in_inch <- img_height_frac * pin[2]
  
  # convert to user coordinates
  img_height_user <- img_height_in_inch * y_units_per_inch
  img_width_user  <- img_height_in_inch * img_ratio * x_units_per_inch
  
  img_xleft <- x_center - img_width_user / 2
  img_xright <- x_center + img_width_user / 2
  img_ybottom <- y_center - img_height_user / 2
  img_ytop <- y_center + img_height_user / 2
  
  rasterImage(img_tmp,
              xleft   = img_xleft,
              xright  = img_xright,
              ybottom = img_ybottom,
              ytop    = img_ytop,
              interpolate = TRUE)
  
  text(x = ifelse(num_right, img_xright, img_xleft),
       y = img_ytop-0.05,
       pos = ifelse(num_right, 4, 2),
       labels = as.character(num),
       font = 2)
}



### Select images to plot

images$image[which.min(images$z_mean)]

img_lo1 <- "G08897.jpg"
img_lo2 <- "G40107.jpg"
img_lo3 <- "G03981.jpg"
img_lo4 <- "G00184.jpg"
img_lo5 <- "G00518.jpg"

img_lo6 <- "G11826.jpg"
img_lo7 <- "G27494.jpg"
img_lo8 <- "G47178.jpg"

img_hi1 <- "G01855.jpg"
img_hi2 <- "G12010.jpg"
img_hi3 <- "G07375.jpg"
img_hi4 <- "G24764.jpg"
img_hi5 <- "G18200.jpg"

img_hi6 <- "G07404.jpg"
img_hi7 <- "G21221.jpg"
img_hi8 <- "G10100.jpg"
img_hi9 <- "G24764.jpg"

img_mid_m013 <- "G29963.jpg"
img_mid_m008 <- "G04292.jpg"
img_mid_m00 <- "G14687.jpg"
img_mid_p006 <- "G38548.jpg"
img_mid_p021 <- "G38696.jpg"





