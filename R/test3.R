add_adjacent_ints <- function(x, prefix){
  print("in adjacent")
  factors = paste("AF", seq(1, 5), sep="")
  factor_grid = expand.grid(factors, factors)
  print(factor_grid)
  for (i in 2:18){
    pos2 = paste(prefix, i, sep="")
    if (!(paste(pos2, "AF1", sep="_") %in% colnames(x))) { next }
    pos1 = paste(prefix, (i-1), sep="")
    for (j in 1:nrow(factor_grid)){
      ind2 = which(colnames(x)==paste(pos2, factor_grid$Var1[j], sep="_"))
      ind1 = which(colnames(x)==paste(pos1, factor_grid$Var2[j], sep="_"))
      new = x[,ind1] * x[,ind2]
      x = cbind(x, new)
      colnames(x)[ncol(x)] = paste(colnames(x)[ind1], colnames(x)[ind2], sep="_by_")
    }
  }
  return(x)
}
