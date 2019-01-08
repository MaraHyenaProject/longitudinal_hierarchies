################################################################################
#                           Define functions                                   #
#                                                                              #
#                                                                              #
#                            By Eli Strauss                                    #
#                                                                              #
#                            November 2018                                     #
################################################################################

##Function to compile data for dot and whisker plots
dw.data.compiler <- function(mm){
  if(class(mm)[1] == 'glmerMod'){
    coefs <- fixef(mm)
    ci.dat <- confint(object= mm, parm = 'beta_', method = 'Wald')
  }else{
    ci.dat <- confint(mm)
    coefs <-summary(mm)$coefficients[,1]
  }
  
  dw.data <- rbind(data.frame(estimate = names(coefs),
                              ci = ci.dat[,2]),
                   data.frame(estimate = names(coefs),
                              ci = ci.dat[,1]))
  dw.data$u <- coefs
  return(dw.data)
}

desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}

lighten <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col*factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

pcomp_table <- function(mod, title){
  tab <- tidy(confint(mod))
  tab$rhs <- symnum(summary(mod)$test$pvalues, corr = FALSE, na = FALSE,
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("***", "**", "*", ".", " "))
  tab[,c(3,4,5)] <- round(tab[,c(3,4,5)], 2)
  tab$CI <- paste0('[',tab[,4], ', ', tab[,5], ']')
  tab <- tab[,c(1,2,3,6)]
  
  names(tab) <- c('Comparison', '', 'Estimate', 'CI')
  print(tab_df(tab,title = title))
  tab_df(tab, title = title, file = paste0('plots/tables/',title,'.doc'))
}

##redact an edgelist to match a given *percent unknowns*. Matching is not exact, but close##
pu_from_edgelist <- function(edgelist, order, pu){
  dyads <- expand.grid(winner = order, loser = order)
  dyads <- dyads[dyads$winner != dyads$loser,]
  dyads <- paste(dyads$winner,dyads$loser)
  obs_redacted <- edgelist_to_matrix(edgelist, identities = order)
  edgelist_redacted <- get_edgelist(obs_redacted)
  edges_present <- c(paste(edgelist_redacted$winner, edgelist_redacted$loser), paste(edgelist_redacted$loser, edgelist_redacted$winner))
  pu_obs <- 1 - length(unique(edges_present)) / length(dyads)
  while(pu_obs < pu){
    non_zero_obs <- which(obs_redacted+t(obs_redacted) != 0, arr.ind = T)
    ind <- non_zero_obs[sample(length(non_zero_obs[,1]), 1),]
    obs_redacted[ind[1],ind[2]] <- 0
    obs_redacted[ind[2],ind[1]] <- 0
    edgelist_redacted <- get_edgelist(obs_redacted)
    edges_present <- c(paste(edgelist_redacted$winner, edgelist_redacted$loser), paste(edgelist_redacted$loser, edgelist_redacted$winner))
    pu_obs <- 1 - length(unique(edges_present)) / length(dyads)
  }
  return(list(pu = pu_obs, edges = edgelist_redacted))
}

##Return bernouli trials for each dyad - did dyad change between two orders or not?
dyadic_similarity_model <- function(order1, order2){
  order1_grid <- rbind(data.frame(dyad = paste(t(combn(order1,2))[,1], t(combn(order1,2))[,2], sep = ','), Win = 1),
                       data.frame(dyad = paste(t(combn(order1,2))[,2], t(combn(order1,2))[,1], sep = ','), Win = 0))
  
  order2_grid <- rbind(data.frame(dyad = paste(t(combn(order2,2))[,1], t(combn(order2,2))[,2], sep = ','), Win = 1),
                       data.frame(dyad = paste(t(combn(order2,2))[,2], t(combn(order2,2))[,1], sep = ','), Win = 0))
  sim_list <- order2_grid[match(order1_grid$dyad, order2_grid$dyad),]$Win == order1_grid$Win
  return(sim_list[1:(length(sim_list)/2)])
}


#Return future observations. If none, current observations are returned
get_future_obs <- function(period, pu){
  future_obs <- obs_current_period
  if(period != num_periods){
    future_obs[] <- rep(0)
    for(p in (period+1):num_periods){
      fobs <- get(paste0('obs_', pu, '_', p))
      future_obs <- future_obs + fobs[dimnames(future_obs)[[1]], dimnames(future_obs)[[1]]]
    } 
  }
  return(future_obs)
}

# ###function adapted from user caracal on stackexchange. Generate orders that with certain
# ###spearman's correlation to true order
generate_trait_orders <- function(n_orders, rho_seq = runif(n_orders, 0,1), true_order = 1:20){
  trait_orders <- list(rho = NA,
                       order = list(NA))
  for(i in 1:n_orders){
    n     <- length(true_order)                    # length of vector                   # desired correlation = cos(angle)
    rho   <- rho_seq[i]
    theta <- acos(rho)             # corresponding angle
    x1    <- true_order        # fixed given data
    x2    <- sample(true_order, replace = F)      # new random data
    X     <- cbind(x1, x2)         # matrix
    Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
    
    Id   <- diag(n)                               # identity matrix
    Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
    P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
    x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
    Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
    Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
    
    x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
    
    trait_orders$rho[i] <- cor(x1, order(x), method = 'spearman')
    trait_orders$order[[i]] <- order(x)
  }
  return(trait_orders)
}


# Multiple plot function from R Cookbook
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

