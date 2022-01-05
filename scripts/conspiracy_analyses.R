df_vars <- read.csv("scripts/df_training_labs.csv", stringsAsFactors = F)
df_vars$lab <- tolower(df_vars$lab)
df_vars <- df_vars[grepl("^c_", df_vars$lab), ]
df_vars <- df_vars[df_vars$lab %in% c("c_political stability", "c_govt. effectiveness", 
                                      "c_doctors per 10k", "c_control corruption", "c_govt. response", 
                                      "c_accountability", "c_containment health index", 
                                      "c_tourism expenditures", "c_rule of law"), ]


tmp <- df_training[, c("countryiso3", df_vars$X)]

names(tmp)[-1] <- df_vars$lab[match(names(tmp)[-1], df_vars$X)]
tmp <- tmp[!duplicated(tmp$countryiso3), ]

df_plot <- do.call(rbind, lapply(names(tmp)[-1], function(x){
  data.frame(Country = tmp[["countryiso3"]], Variable = x, Value = tmp[[x]])
}))

library(ggplot2)

p <- ggplot(df_plot, aes(x = Value, y = 1)) + ggrepel::geom_text_repel(aes(label = Country), angle = 90, size = 3, max.overlaps = 30, direction = "y") + facet_wrap(~Variable, nrow = 3, scales = "free") + theme_bw() + labs(x = NULL, y = NULL) + scale_y_continuous(labels = NULL, breaks = NULL)
ggsave("country_variables.png", p, "png", width = 8, height = 8)


df_plot <- do.call(rbind, lapply(names(tmp)[-1], function(x){
  data.frame(Country = tmp[["countryiso3"]], Variable = x, Value = tmp[[x]])
}))

library(ggplot2)

# df_training should be a data.table

df_plot <- lapply(c("deaths", "confirmed", "recovered", "governmentresponseindex", "stringencyindex", "closepublictransport_flag"), function(x){
  tmp <- df_training[, list(mean=mean(get(x), na.rm = TRUE), sd=sd(get(x), na.rm = TRUE)), by=countryiso3]
  tmp[, "variable" := x]
})
df_plot <- rbindlist(df_plot)
df_plot$variable <- df_vars$lab[match(df_plot$variable,  df_vars$X)]

p <- ggplot(df_plot, aes(y = countryiso3)) + 
  geom_errorbarh(aes(xmin = mean-sd, xmax = mean+sd)) +
  geom_point(aes(x = mean))+
  facet_wrap(~variable, nrow = 3, scales = "free") + theme_bw() +
  ylab("Country")
ggsave("country_variables_byday.png", p, "png", width = 8, height = 10)



# Check measurement invariance --------------------------------------------
library(lavaan)
df_inv <- df$consp01
df_inv <- df[, c("coded_country", grep("^consp0", names(df), value = T))]

df_tmp <- df_inv
df_tmp <- df_tmp[!rowSums(is.na(df_tmp)) == 3, ]
df_tmp <- df_tmp[!df_tmp$coded_country %in% c("Malaysia", "Philippines"), ]
mod <- paste0('F =~ ', paste0(names(df_tmp[-1]), collapse = " + "))
# configural invariance
# configural invariance
  fit <- cfa(mod, df_tmp)
  
  fit1 <- cfa(mod, data = df_tmp, group = "coded_country")
  # metric invariance
  fit2 <- cfa(mod, data = df_tmp, group = "coded_country",
              group.equal = "loadings")
  c(fitmeasures(fit1)[c("chisq", "df", "npar", "bic", "cfi", "tli", "rmsea")],
    fitmeasures(fit2)[c("chisq", "df", "npar", "bic", "cfi", "tli", "rmsea")],
    unlist(lavTestLRT(fit1, fit2)[2, 7]))
  anova(fit1, fit2)


df_tmp <- df_inv
names(df_tmp)[-1] <- paste0("consp_", 1:3)
library(tidySEM)
out <- sapply(unique(df_tmp$coded_country), function(i){
  #i = df_tmp$coded_country[1]
  tmp <- df_tmp[df_tmp$coded_country == i, -1]
  tmp <- tidy_sem(tmp)
  tmp <- create_scales(tmp)
  c(Country = i, unlist(tmp$descriptives))
})
desc <- data.frame(t(out))
write.csv(desc, "conspiracy_reliability_by_country.csv", row.names = F)
