directory <- ""
setwd(directory)
### ------ list of functions needed ------ ###

# prob of rejection a treatment given p = prob of response
Rp <- function(p, r1, r, n1, n){
  x <- (r1 + 1):min(n1, r) 
  n2 <- n - n1
  R <- pbinom(r1, n1, p) + sum(dbinom(x, n1, p) * pbinom(r - x, n2, p))
  return(R)
}

TwoStage <- function(n1, n, r1, r, p0, p1){
  x <- (r1+1):min(n1, r) 
  n2 <- n - n1
  alpha <- 1 - Rp(p = p0, r1, r, n1, n) 
  beta <- Rp(p = p1, r1, r, n1, n) 
  Pet0 <- pbinom(r1, n1, p0); Pet1 <- pbinom(r1, n1, p1)
  EN <-  n1 * Pet0 + (1 - Pet0) * n
  #EN0 <- n1 + (1 - Pet0) * n2; EN1 <- n1 + (1 - Pet1) * n2
  return(data.frame(alpha, beta, Pet0, Pet1, EN))
}

# see at the end a faster function
SimonTwoSD <- function(p0, p1, alpha, beta, nmax, nmin, plot = TRUE){
  m_fin <- rep(0, 8)
  for(N in nmin:nmax){
    n1 <- 1:N
    for(i in 1:max(n1)){
      n1 <- rep(1:i, (1:i) + 1)
    }
    # r1 in 1:unique(n1)
    r1 <- rep(0, length(n1));  j <- 1
    for(i in 1:N) {
      r1[j + (0:i)] <- 0:i
      j <- j + i + 1
    }
    # r in (r1 + 1):nmax
    r <- 0; j <- 0; mm <- c(0, 0, 0)
    for(i in 1:nrow(cbind(n1, r1))){
      r_l <- (cbind(n1,r1)[i,2] + 1):N
      r[j + (1:length(r_l))] <- r_l
      M <- matrix(as.vector(as.vector(rep(cbind(n1,r1)[i,], length(r_l)))),ncol = 2, byrow = TRUE)
      M <- cbind(M, r_l)
      mm <- rbind(mm, M)
      j <- length(r)
    }
    mm2 <- mm[-(nrow(mm)+(-1:0)),]
    res <- rep(0, 7)
    for(i in 1:nrow(mm2)){
      obj <- TwoStage(n1 = mm2[i,1], n = N, p0 = p0, p1 = p1, r1 = mm2[i,2], r = mm2[i,3])
      if(obj$alpha <= alpha & obj$beta <= beta){
        res1 <- c(mm2[i,], obj$alpha, obj$beta, obj$Pet0, obj$EN)
        res <- rbind(res, res1) 
      }
      if(i == nrow(mm2) & length(res) != 7){
        #res2 <- apply(res, 2, function(x) round(x, 3))
        res <- res[-1,]
        if(length(res) == 7){
          fin <- c(res, N)
        }
        else{
          fin <- c(res[which.min(res[,7]),], N)
        }
        m_fin <- rbind(m_fin, fin)
      }
    }
  }
  colnames(m_fin) <- c("n1", "r1", "r", "a", "b", "Pet0", "EN", "N")
  m_fin <- m_fin[-1,]
  m_fin <- as.data.frame(m_fin)
  out <- list(values = m_fin, minmax = m_fin[1,], optimal = m_fin[which.min(m_fin$EN),])
  if(plot == TRUE){
    par(mar = c(4, 4, 1.5, 1.5))
    plot(out$values$N, out$values$EN, xlab = "N (total number of subjects)", 
         pch = 19, ylab = "EN (expected sample size under H0)", main = "Two stage Phase II Design")
    lines(out$values$N, out$values$EN, type = "l", lty = 1)
    points(x= c(out$optimal$N, out$minmax$N), y = c(out$optimal$EN, out$minmax$EN), col = c(4, 7), pch = 19)
    legend("topright", legend= c("minmax design", "optimal disign"), pch = rep(19, 2), col = c(7, 4))
    par(mar = rep(4, 4))
  }
  return(out)
}

fixedDesign <- function(alpha, beta, p0, p1, nmin, nmax){
  acc <- c(0, 0)
  for(N in nmin:nmax){
    for(R in 1:N){
      c1 <- 1 - pbinom(R - 1, size = N, prob = p0) 
      c2 <- 1 - pbinom(R - 1, size = N, prob = p1) 
      if(c1 <= alpha & c2 >= 1 - beta){
        acc1 <- c(R, N)
        acc <- rbind(acc, acc1)
      }
    }
  }
  acc <- acc[-1,]; colnames(acc) <- c("R", "N")
  return(list(N = acc[1,"N"], R = acc[1,"R"]))
}

alpha_1 <- function(m, n1, p0){
  1 - pbinom(m-1, size = n1, prob = p0)
}

# under current trend
adjIncreaseSample <- function(X, n1, p0, nmin, nmax, lev, R_old, N_old){
  acc <- c(0, 0)
  for(N in nmin:nmax){
    for(R in 1:N){
      c1 <- round(1 - pbinom(R - X - 1, size = N - n1, prob = p0) , 2)
      c1_old <- round(1 - pbinom(R_old - X - 1, size = N_old - n1, prob = p0), 2)
      c2 <- round(1 - pbinom(R - X - 1, size = N - n1, prob = X/n1) , 2)
      if(c1 <= c1_old & c2 >= lev){
        acc1 <- c(R, N)
        acc <- rbind(acc, acc1)
      }
    }
  }
  acc <- acc[-1,]; 
  colnames(acc) <- c("R", "N")
  return(acc)
}

# under alternative hypothesis
adjIncreaseSample_a <- function(X, n1, p0, p1, nmin, nmax, lev, R_old, N_old){
  acc <- c(0, 0)
  for(N in nmin:nmax){
    for(R in 1:N){
      c1 <- round(1 - pbinom(R - X - 1, size = N - n1, prob = p0) , 2)
      c1_old <- round(1 - pbinom(R_old - X - 1, size = N_old - n1, prob = p0), 2)
      c2 <- round(1 - pbinom(R - X - 1, size = N - n1, prob = p1) , 2)
      if(c1 <= c1_old & c2 >= lev){
        acc1 <- c(R, N)
        acc <- rbind(acc, acc1)
      }
    }
  }
  acc <- acc[-1,]; 
  colnames(acc) <- c("R", "N")
  return(acc)
}

CPc <- function(N, R, X, n1){
  return(round(1 - pbinom(R - X, N - n1, prob = X/n1), 3))
}

# under current trend
SimonAdj <- function(alpha, beta, p0, p1, nmin, nmax, n1, X, out, R, N){
  fixDes <- fixedDesign(alpha = alpha, beta = beta, p0 = p0, p1 = p1, nmin = nmin, nmax = nmax)
  R_old <- fixDes$R; N_old <- fixDes$N
  t <- n1/N_old
  #lev <- 0.9
  lev <- round(pnorm(2*(1-sqrt(t))/sqrt(t*(1-t))), 2)
  print(paste("R:",R, "N:", N))
  mi <- out$minmax$n1
  r1 <- 0 # set
  for(i in 1:mi){
    a1 <- alpha_1(m = i, n1 = n1, p0 = p0)
    cond <- a1 < alpha
    if(cond){
      m <- i
      a1 <- alpha_1(m = m, n1 = n1, p0 = p0) 
      x <- r1:(m - 1)
      condition <- FALSE
      R <- R_old
      while(condition != TRUE){
        condition <- sum(dbinom(x, n1, p0) * (1 - pbinom(R - x - 1, size = N - n1, prob = p0))) > alpha - a1
        if(condition){
          R <- R + 1
        }
        else break
      }
    }
    else R <- R_old
  }
  print(paste("R':",R)) # is the new R' (R' >= R)
  
  nmax <- nmax * 15
  NR_new <- adjIncreaseSample(X = X, n1 = n1, p0 = p0, nmax = nmax, nmin = N, lev = lev, R_old = R, N_old = N_old)
  return(list(R_old = R_old, R_ = R, N_old = N_old, R_new = NR_new[1,1], N_new = NR_new[1,2], lev = lev))
}

# under alternative ypothesis
SimonAdj_a <- function(alpha, beta, p0, p1, nmin, nmax, n1, X, lev, out, R, N){
  fixDes <- fixedDesign(alpha = alpha, beta = beta, p0 = p0, p1 = p1, nmin = nmin, nmax = nmax)
  R_old <- fixDes$R; N_old <- fixDes$N
  t <- n1/N_old
  #lev <- 0.9
  lev <- round(pnorm(2*(1-sqrt(t))/sqrt(t*(1-t))), 2)
  print(paste("R:",R, "N:", N))
  mi <- out$minmax$n1
  r1 <- 0 # set
  for(i in 1:mi){
    a1 <- alpha_1(m = i, n1 = n1, p0 = p0)
    cond <- a1 < alpha
    if(cond){
      m <- i
      a1 <- alpha_1(m = m, n1 = n1, p0 = p0) 
      x <- r1:(m - 1)
      condition <- FALSE
      R <- R_old
      while(condition != TRUE){
        condition <- sum(dbinom(x, n1, p0) * (1 - pbinom(R - x - 1, size = N - n1, prob = p0))) > alpha - a1
        if(condition){
          R <- R + 1
        }
        else break
      }
    }
    else R <- R_old
  }
  print(paste("R':",R)) # is the new R' (R' >= R)
  
  nmax <- nmax * 15
  NR_new <- adjIncreaseSample_a(X = X, n1 = n1, p1 = p1, p0 = p0, nmax = nmax, nmin = N,lev = lev, R_old = R, N_old = N_old)
  return(list(R_old = R_old, R_ = R, N_old = N_old, R_new = NR_new[1,1], N_new = NR_new[1,2], lev = lev))
}

CP <- function(N, R, p){
  obj <- rep(0, 3)
  for(n1 in 0:N){
    for(x in 0:R){
      obj1 <- c(n1, x, 1 - pbinom(R - x - 1, size = N - n1, prob = p))
      obj <- rbind(obj, obj1)
    }
  }
  obj <- obj[-1,]
  return(obj)
}

cp_orange <- function(N, R, n1, p){
  x <- 1:(R-1)
  return(sum(1 - pbinom(R - x - 1, N - n1, p = prob)))
}

print_regions_2 <- function(N, R, p, lev1, lev2, string, res = FALSE){
  obj_cp <- CP(N = N, R = R, p = p)
  CPa50 <- obj_cp[obj_cp[,3]<=0.55 & obj_cp[,3]>=0.45,]
  CPa50 <- CPa50[CPa50[,1]>=CPa50[,2],]
  
  hopeful <- cbind(obj_cp[obj_cp[,3]<lev1 & obj_cp[,3]>=lev2,1], obj_cp[obj_cp[,3]<lev1 & obj_cp[,3]>=lev2,2])
  hopeful <- hopeful[hopeful[,1]>=hopeful[,2],]
  favorable <- cbind(obj_cp[obj_cp[,3]>=lev1, 1], obj_cp[obj_cp[,3]>=lev1, 2])
  favorable <- favorable[favorable[,1]>=favorable[,2],]
  unfavorable <- cbind(obj_cp[ obj_cp[,3]<lev2,1], obj_cp[obj_cp[,3]<lev2,2])
  
  min_hope <- tapply(hopeful[,1], INDEX = hopeful[,2], FUN = min)
  max_hope <- tapply(hopeful[,1], INDEX = hopeful[,2], FUN = max)
  min_fav <- tapply(favorable[,1], INDEX = favorable[,2], FUN = min)
  max_fav <- tapply(favorable[,1], INDEX = favorable[,2], FUN = max)
  min_unfav <- tapply(unfavorable[,1], INDEX = unfavorable[,2], FUN = min)
  max_unfav <- tapply(unfavorable[,1], INDEX = unfavorable[,2], FUN = max)
  
  min_hope <- cbind(c(as.numeric(as.character(attr(min_hope, which = "dimnames")[[1]]))),
                       c(as.numeric(min_hope)))
  max_hope <- cbind(c(as.numeric(as.character(attr(max_hope, which = "dimnames")[[1]]))),
                       c(as.numeric(max_hope)))
  min_fav <- cbind(c(as.numeric(as.character(attr(min_fav, which = "dimnames")[[1]]))),
                       c(as.numeric(min_fav)))
  max_fav <- cbind(c(as.numeric(as.character(attr(max_fav, which = "dimnames")[[1]]))),
                       c(as.numeric(max_fav)))
  min_unfav <- cbind(c(as.numeric(as.character(attr(min_unfav, which = "dimnames")[[1]]))),
                       c(as.numeric(min_unfav)))
  max_unfav <- cbind(c(as.numeric(as.character(attr(max_unfav, which = "dimnames")[[1]]))),
                       c(as.numeric(max_unfav)))
  
  poly_x <- as.vector(c(min_hope[,2], sort(max_hope[,2], decreasing = TRUE)))
  poly_y <- as.vector(c(min_hope[,1], sort(max_hope[,1], decreasing = TRUE)))
  poly_x_fav <- as.vector(c(min_fav[,2], sort(max_fav[,2], decreasing = TRUE)))
  poly_y_fav <- as.vector(c(min_fav[,1], sort(max_fav[,1], decreasing = TRUE)))
  poly_x_unf <- as.vector(c(min_unfav[,2], sort(max_unfav[,2], decreasing = TRUE)))
  poly_y_unf <- as.vector(c(min_unfav[,1], sort(max_unfav[,1], decreasing = TRUE)))
  
  plot(obj_cp[,1], obj_cp[,2], xlim = c(0, N), ylim = c(0, R),
       xlab = "# Patients", ylab = "# Responses", main = string, pch = 19, cex = 0.5, col = "white")
  polygon(poly_x, poly_y, lwd = 2, border = "black", col = "#FD8204")
  polygon(poly_x_fav, poly_y_fav, lwd = 2, border = "black", col = "#8AB3B8")
  polygon(poly_x_unf, poly_y_unf, lwd = 2, border = "black", col = "#121FC5")
  lines(CPa50[,1], CPa50[,2], lty = 2, lwd = 2, col = "#E62020")
  #points(obj_cp[,1], obj_cp[,2], col = "#BEBEBE", pch = 19, cex = 0.5) # "#6E7F80"
  abline(v = c(0:N), lty = "solid", col = "#BEBEBE", lwd = 0.2)
  abline(h = c(0:R), lty = "solid", col = "#BEBEBE", lwd = 0.2)
  #grid(col = "#BEBEBE", lty = "solid", lwd = 0.2)
  if(res == TRUE){
    return(list(red = unfavorable, green = favorable))
  }
}

# to use in Rshiny app because is faster
fast_Simon2SD <- function(p0, p1, alpha, beta, nmax = 100){
  require(clinfun)
  val <- ph2simon(pu = p0, pa = p1, ep1 = alpha, ep2 = beta, nmax = nmax)
  colnames(val$out) <- c("r1", "n1", "r", "N", "EN", "Pet0")
  val$out <- as.data.frame(val$out)
  return(list(values = val$out, minmax = val$out[1,], optimal = val$out[which.min(val$out[,5]),]))
}

# slow
SimonTwoSD_2 <- function(p0, p1, alpha, beta, nmax, nmin){
  m_fin <- rep(0, 8)
  for(N in nmin:nmax){
    res <- rep(0, 8)
    for(n1 in 1:N){
      for(r1 in 1:n1){
        for(r in (r1+1):N){
          obj <- TwoStage(n1 = n1, n = N, p0 = p0, p1 = p1, r1 =r1, r = r)
          if(obj$alpha <= alpha & obj$beta <= beta){
            res1 <- c(N, n1, r, r1, obj$alpha, obj$beta, obj$Pet0, obj$EN)
            res <- rbind(res, res1) 
          }
        }
      }
    }
    if(length(res) != 8){
      res <- res[-1,]
      if(length(res) == 8){
        fin <- res
      }
      else{
        fin <- res[which.min(res[,8]),]
      }
      m_fin <- rbind(m_fin, fin)
    }
    print(N)
  }
  colnames(m_fin) <- c("N", "n1", "r1", "r", "a", "b", "Pet0", "EN")
  m_fin <- m_fin[-1,]
  m_fin <- as.data.frame(m_fin)
  
  out <- list(values = m_fin, minmax = m_fin[1,], optimal = m_fin[which.min(m_fin$EN),])
  
  return(out)
}