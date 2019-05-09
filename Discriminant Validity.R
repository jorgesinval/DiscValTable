disc.val.table <- function(fit) {
  sq.corr=lavaan::inspect(fit,"std")$psi^2
  ave=semTools::reliability(fit)
  diag(sq.corr) <- round(unname(ave["avevar",1:length((abs(1-nrow(ave))))]),2)
    return(sq.corr)
}

library(lavaan)
HS.model <- ' visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HolzingerSwineford1939)

disc.table=disc.val.table(fit)
disc.table=round(disc.table,2)

disc.table<-disc.table
disc.table[upper.tri(disc.table)]<-""
disc.table<-as.data.frame(disc.table)
disc.table

library(kableExtra)
kable(disc.table) %>% kable_styling()
