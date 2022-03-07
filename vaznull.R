##codes 
##Vázquez DP, Melian CJ, Williams NM, Blüthgen N, Krasnov BR and Poulin R. 2007. Species abundance and asymmetric interaction strength in ecological networks. Oikos 116: 1120-1127.

vaznullmodif <-function (N, web, m.hyp) {
  web <- as.matrix(empty(web))
  vaznull.fast <- function(web) {
    rs.p <- rowSums(web)/sum(web)
    cs.p <- colSums(web)/sum(web)
    P <- P1 <- tcrossprod(rs.p, cs.p)*m.hyp
    finalmat <- matrix(0, nrow(web), ncol(web))
    n.int.finalmat <- 0
    while (n.int.finalmat < sum(dim(web))) {
      sel <- sample(1:length(web), 1, prob = P, replace = TRUE)
      selc <- floor((sel - 1)/(dim(web)[1])) + 1
      selr <- ((sel - 1)%%dim(web)[1]) + 1
      if (sum(finalmat[, selc]) == 0 | sum(finalmat[selr, ]) == 0) {
        finalmat[sel] <- 1
        P[sel] <- 0
      }
      n.int.finalmat <- sum(rowSums(finalmat) > 0) + sum(colSums(finalmat) > 0)
    }
    conn.remain <- sum(web > 0) - sum(finalmat > 0)
    if (conn.remain > 0) {
      add <- sample(which(finalmat == 0), conn.remain, prob = P1[finalmat == 0])
      finalmat[add] <- 1
    }
    int.remain <- sum(web) - sum(finalmat)
    if (int.remain > 0) {
      add <- sample(which(finalmat > 0), int.remain, prob = P1[finalmat > 0], replace = TRUE)
      finalmat[as.numeric(names(table(add)))] <- finalmat[as.numeric(names(table(add)))] + table(add)
    }
    finalmat
  }
  replicate(N, vaznull.fast(web), simplify = FALSE)
}


##
mo.dist <- function(mat, mat.dif){
  ntot <- dim(mat)[1]*dim(mat)[2]
  W <- list(length=ntot)
  for(i in 1:ntot) if(c(mat)[i] == 0) {W[[i]] <- c(NA)} else {W[[i]] <- rep(c(mat.dif)[i], c(mat)[i])}
  restas.obs <- unlist(W)
  ME <- mean(restas.obs, na.rm=T)
  SD <- sd(restas.obs, na.rm=T)
  c(ME, SD)
}