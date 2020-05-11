library(tidyverse)
library(ggplot2)


# function to plot a counts histogram for a single feature and calculate the number of 
# missing values the variable
# TODO: if needed later, make the function work for categorical responses also
explore_psyCorona <- function(dat, var_name, n_bins = 30) {
  if (!(var_name %in% colnames(dat))){
    stop("The selected variable is not a column in the data")
  }
  # calculate NAs
  filt_df <- dat %>% dplyr::select(var_name)
  #print(paste("Filtered DF dimensions", (dim(filt_df))))
  n_na = sum(is.na(filt_df[ ,1]))
  # paste together a title for the histogram, that has the % of NAs in it  
  plot_title <- paste0(var_name, " distribution (", n_na, " NAs, ", round(100*n_na/nrow(dat),2), "%)")
  # plot the histogram
  ggplot(data=filt_df, aes_(x=rlang::sym(var_name))) +
    stat_bin(bins=n_bins, na.rm=TRUE) +
    ggtitle(plot_title)
}
