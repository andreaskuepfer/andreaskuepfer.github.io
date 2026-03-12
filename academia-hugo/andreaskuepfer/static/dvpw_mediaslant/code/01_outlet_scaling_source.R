
library(here)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(viridis)
library(scales)
library(RColorBrewer)
library(fields)

# baysian regression models using STAN
library(brms)
#remotes::install_github("stan-dev/cmdstanr")
library(cmdstanr)
#install_cmdstan()
library(posterior)


#################
### LOAD DATA ###
#################

load(here("Data/oli/04_dyad_sample/dyad_sample.Rdata"))

# exclude BBC News and MSNBC (comparability with previous approach)
# -> already filtered out
dyad_sample <- dyad_sample[dyad_sample$source_A != "BBC News" &
                             dyad_sample$source_A != "MSNBC" &
                             dyad_sample$source_B != "BBC News" &
                             dyad_sample$source_B != "MSNBC",]

unique(dyad_sample$source_A)

set.seed(123)
df <- dyad_sample

# sample N
#set.seed(123)
#df <- df[sample(1:nrow(df), 100000),]

# no sampling: all 1M images

###################
### Sample Prep ###
###################

# undirected dyads
df$dyad <- apply(df[, c("source_A", "source_B")], 1, 
                 function(x) paste(sort(x), collapse = "_"))


outlets <- data.frame(outlet = unique(c(df$source_A, df$source_B)),
                      num_index = NA)
outlets$num_index <- 1:nrow(outlets)

df$out_i <- outlets$num_index[match(df$source_A, outlets$outlet)]
df$out_j <- outlets$num_index[match(df$source_B, outlets$outlet)]

# delete identical images (very few)
df <- df[df$eucl_dist_img != 0,]
df <- df[df$cos_sim_img != 1,]

img_posterior_draws <- readRDS(here("Code/oli/text_image_scaling/estimated_models/img_posterior_draws_baseline_model_downsampled_N1M.Rds"))

######################
### MODEL ANALYSIY ### 
######################

# function to plot z
plot_posterior_z <- function(z_draws, 
                             den_scale = 0.02,
                             xlim = c(-0.3, 0.3), cex = 2,
                             add = F,
                             dens_col = viridis(3, 0.4)[1],
                             shift_ci = 0,
                             empty = F,
                             subset = NA){
  # Organize data for plotting
  
  outlets$z_mean <- NA
  outlets$z_lo95 <- NA
  outlets$z_hi95 <- NA
  
  outlets$z_posterior <- NA
  
  for (i in 1:nrow(outlets)) {
    # z: outlet latent coordinate
    outlets$z_mean[i] <- mean(z_draws[[paste0("z[", i, ",1]")]])
    outlets$z_lo95[i] <- quantile(z_draws[[paste0("z[", i, ",1]")]], 0.025)
    outlets$z_hi95[i] <- quantile(z_draws[[paste0("z[", i, ",1]")]], 0.975)
    
    outlets$z_posterior[i] <- list(z_draws[[paste0("z[", i, ",1]")]])
  }
  
  outlets_ordered <- outlets[order(outlets$z_mean, decreasing = T),]
  
  outlets_ordered$y_val <- nrow(outlets_ordered):1
  
  if (!is.na(subset[1])){
    outlets_ordered <- outlets_ordered[outlets_ordered$outlet %in% subset,]
  }
  
  # plot posterior
  
  par(mar = c(c(5, 10, 1, 2) + 0.1),
      family = "serif")
  if(add == F){
    plot(x = outlets_ordered$z_mean,
         y = outlets_ordered$y_val,
         type = "n",
         yaxt = "n",
         ylab = "",
         xlab = "Outlet Latent Coordinate",
         pch = 19,
         cex = 2,
         xlim = xlim,
         ylim = c(1-.5, 10+.5))
  }
  #grid()
  abline(h = 10:1,
         col = "grey70",
         lwd = 1)
  abline(v = 0,
         lty = "dashed",
         col = "grey70",
         lwd = 1)
  
  if (!empty){
    for (i in 1:nrow(outlets_ordered)) {
      den_tmp <- density(unlist(outlets_ordered$z_posterior[i]))
      
      polygon(x = c(den_tmp$x, rev(den_tmp$x)),
              y = c(den_tmp$y*den_scale + outlets_ordered$y_val[i], 
                    rep(outlets_ordered$y_val[i], length(den_tmp$y))),
              border = NA,
              col = dens_col)
      
    }
    
    
    # Mean and 95% credibility interval
    segments(x0 = outlets_ordered$z_lo95,
             x1 = outlets_ordered$z_hi95,
             y0 = outlets_ordered$y_val + shift_ci,
             y1 = outlets_ordered$y_val + shift_ci,
             lwd = 3,
             lend = 1)
    points(x = outlets_ordered$z_mean,
           y = outlets_ordered$y_val + shift_ci,
           pch = 19, 
           cex = cex)
    
    # add y-axis
    if (add == F){
      axis(2,
           at = outlets_ordered$y_val,
           labels = outlets_ordered$outlet,
           las = 1)
    }
  }
  
  
}


# organize results


#z_draws <- as_draws_df(fit$draws("z"))
#a_draws <- as_draws_df(fit$draws("a"))
#mu_draws <- as_draws_df(fit$draws("mu"))

z_draws_img  <- as_draws_df(subset_draws(img_posterior_draws, variable = "z"))
a_draws_img  <- as_draws_df(subset_draws(img_posterior_draws, variable = "a"))
mu_draws_img <- as_draws_df(subset_draws(img_posterior_draws, variable = "mu"))

##########################
### Post-hoc alignment ###
##########################

# (resolve sign-flipping issue)

# Pick an anchor outlet: Fox News
anchor_outlet <- which(outlets$outlet == "CNN")

# Compute mean across outlets for each iteration
z_anchor_img <- z_draws_img[, which(outlets$outlet == "Al Jazeera English")]

# Flip sign of entire latent vector for iterations where anchor < 0
flip_idx_img <- which(z_anchor_img < 0)

z_draws_aligned_img <- z_draws_img

z_draws_aligned_img[flip_idx_img, ] <- -z_draws_img[flip_idx_img, ]

# flip all text positions to align with image scaling
z_draws_aligned_img <- -z_draws_aligned_img


### PLOT BOTH TEXT AND IMAGE RESULTS

plot_posterior_z_img_and_txt <- function(z_draws_img,
                                         z_draws_txt,
                                         den_scale = 0.02,
                                         xlim = c(-0.3, 0.3), 
                                         cex = 2){
  # Organize data for plotting
  
  outlets$z_mean_img <- NA
  outlets$z_lo95_img <- NA
  outlets$z_hi95_img <- NA
  outlets$z_posterior_img <- NA
  
  outlets$z_mean_txt <- NA
  outlets$z_lo95_txt <- NA
  outlets$z_hi95_txt <- NA
  outlets$z_posterior_txt <- NA
  
  for (i in 1:nrow(outlets)) {
    # z: outlet latent coordinate
    outlets$z_mean_img[i] <- mean(z_draws_img[[paste0("z[", i, ",1]")]])
    outlets$z_lo95_img[i] <- quantile(z_draws_img[[paste0("z[", i, ",1]")]], 0.025)
    outlets$z_hi95_img[i] <- quantile(z_draws_img[[paste0("z[", i, ",1]")]], 0.975)
    outlets$z_posterior_img[i] <- list(z_draws_img[[paste0("z[", i, ",1]")]])
    
    outlets$z_mean_txt[i] <- mean(z_draws_txt[[paste0("z[", i, ",1]")]])
    outlets$z_lo95_txt[i] <- quantile(z_draws_txt[[paste0("z[", i, ",1]")]], 0.025)
    outlets$z_hi95_txt[i] <- quantile(z_draws_txt[[paste0("z[", i, ",1]")]], 0.975)
    outlets$z_posterior_txt[i] <- list(z_draws_txt[[paste0("z[", i, ",1]")]])
  }
  
  # order according to image position
  outlets_ordered <- outlets[order(outlets$z_mean_img, decreasing = T),]
  
  outlets_ordered$y_val <- nrow(outlets_ordered):1
  
  # plot posterior
  
  par(mar = c(c(5, 10, 1, 2) + 0.1),
      family = "serif")
  plot(x = outlets_ordered$z_mean_img,
       y = outlets_ordered$y_val,
       type = "n",
       yaxt = "n",
       ylab = "",
       xlab = "Outlet Latent Coordinate",
       pch = 19,
       cex = 2,
       xlim = xlim,
       ylim = c(min(outlets_ordered$y_val)-.5, max(outlets_ordered$y_val)+.5))
  #grid()
  abline(h = outlets_ordered$y_val,
         col = "grey70",
         lwd = 1)
  abline(v = 0,
         lty = "dashed",
         col = "grey70",
         lwd = 1)
  
  ### 1) plot image positions
  for (i in 1:nrow(outlets_ordered)) {
    den_tmp <- density(unlist(outlets_ordered$z_posterior_img[i]))
    
    polygon(x = c(den_tmp$x, rev(den_tmp$x)),
            y = c(den_tmp$y*den_scale + outlets_ordered$y_val[i], 
                  rep(outlets_ordered$y_val[i], length(den_tmp$y))),
            border = NA,
            col = viridis(3, 0.4)[1])
    
  }
  
  # Mean and 95% credibility interval
  shift_ci <- 0
  segments(x0 = outlets_ordered$z_lo95_img,
           x1 = outlets_ordered$z_hi95_img,
           y0 = outlets_ordered$y_val + shift_ci,
           y1 = outlets_ordered$y_val + shift_ci,
           lwd = 3,
           lend = 1)
  points(x = outlets_ordered$z_mean_img,
         y = outlets_ordered$y_val + shift_ci,
         pch = 19, 
         cex = cex)
  
  ### 2) plot txt positions
  for (i in 1:nrow(outlets_ordered)) {
    den_tmp <- density(unlist(outlets_ordered$z_posterior_txt[i]))
    
    polygon(x = c(den_tmp$x, rev(den_tmp$x)),
            y = c(den_tmp$y*den_scale + outlets_ordered$y_val[i], 
                  rep(outlets_ordered$y_val[i], length(den_tmp$y))),
            border = NA,
            col = viridis(3, 0.4)[2])
    
  }
  
  # Mean and 95% credibility interval
  shift_ci <- -0.15
  segments(x0 = outlets_ordered$z_lo95_txt,
           x1 = outlets_ordered$z_hi95_txt,
           y0 = outlets_ordered$y_val + shift_ci,
           y1 = outlets_ordered$y_val + shift_ci,
           lwd = 3,
           lend = 1,
           col = "grey50")
  points(x = outlets_ordered$z_mean_txt,
         y = outlets_ordered$y_val + shift_ci,
         pch = 19, 
         cex = cex,
         col = "grey50")
  
  # add y-axis
  axis(2,
       at = outlets_ordered$y_val,
       labels = outlets_ordered$outlet,
       las = 1)
}
