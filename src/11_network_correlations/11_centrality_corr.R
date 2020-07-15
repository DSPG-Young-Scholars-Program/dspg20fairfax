source("src/09_network_descriptives/network_functions.R")
library(Hmisc)


network_stats(vars = "S000", types = "all", years = 2017, centiserve = T, bottleneck = F)

cormatrix <-rcorr(as.matrix(nodelist_2017_S000_all[, -c(1, 2, 19)]))

# flattencorrmatrix function from here: http://www.sthda.com/english/wiki/correlation-matrix-formatting-and-visualization

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

corr_matrix_nodelist_2017_S000_all <- flattenCorrMatrix(cormatrix$r, cormatrix$P)
write.csv(corr_matrix_nodelist_2017_S000_all, "src/11_network_correlations/corr_matrix_nodelist_2017_S000_all.csv", row.names = F)

