
library(here)
library(viridis)

#image_data <- readRDS(here("Code/oli/text_image_scaling/oos_estimates/oos_est_N500.Rds"))
image_data <- readRDS(here("Code/oli/text_image_scaling/oos_estimates/oos_est_N1000.Rds"))

min(image_data$date)

# start on October 7
image_data <- image_data[image_data$date >= "2023-10-07",]

# flip sign
#image_data$z_mean_comb <- -image_data$z_mean_comb

# Function to plot loess fit
add_loess <- function(image_data,
                      col_ci = viridis(3, 0.4)[1],
                      col_line = viridis(3)[1],
                      outlet_label,
                      plot_ci = T,
                      label_yshift = 0,
                      loess_span = 0.75){
  
  # order by date
  image_data <- image_data[order(image_data$date),]
  # running days variable for date
  image_data$days <- 
    as.numeric(image_data$date - min(image_data$date))
  
  # loess fit
  loess_fit <- loess(z_mean_comb ~ days, 
                     data = image_data,
                     span = loess_span)
  
  # add date variable
  loess_fit$date <- image_data$date[match(loess_fit$x, image_data$days)]
  
  # predicted values and CI
  loess_fit_pred <- predict(loess_fit, se = T)
  loess_fit_pred$date <- loess_fit$date
  
  
  # polygon
  if (plot_ci){
    loess_fit_pred$ci_lo <- qnorm(0.025, loess_fit_pred$fit, loess_fit_pred$se.fit)
    loess_fit_pred$ci_hi <- qnorm(0.975, loess_fit_pred$fit, loess_fit_pred$se.fit)
    polygon(x = c(loess_fit_pred$date, rev(loess_fit_pred$date)),
            y = c(loess_fit_pred$ci_lo, rev(loess_fit_pred$ci_hi)),
            border = NA,
            col = col_ci)
  }
  
  
  lines(x = loess_fit_pred$date,
        y = loess_fit_pred$fit,
        col = col_line,
        lwd = 4)
  
  # label
  text(x = min(loess_fit_pred$date),
       y = loess_fit_pred$fit[which.min(loess_fit_pred$date)] + label_yshift,
       labels = outlet_label,
       pos = 2,
       col = col_line)
}



