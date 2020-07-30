list.files("results", ".csv")
tab <- read.csv("results/fit_table.csv", stringsAsFactors = F)
tab <- tab[, -grep("repres", names(tab))]
repr <-read.csv("results/representative_fit.csv", stringsAsFactors = F)
repr <- data.frame(t(repr))
names(repr) <- repr[1, ]
repr <- repr[-1, ]
match(gsub(".RData", "", gsub("isofriends_", "", gsub("results.res_", "", rownames(repr), fixed = TRUE), fixed=  TRUE), fixed  =TRUE), tab$DV)
tab <- cbind(tab, repr)
tab <- tab[, -grep("(lasso|mse|r_actu)", names(tab))]
tab[grep("r2", names(tab))] <- lapply(tab[grep("r2", names(tab))], function(x){formatC(as.numeric(x), digits = 3, format = "f")})
write.csv(tab, "results/fit_paper.csv", row.names = FALSE)