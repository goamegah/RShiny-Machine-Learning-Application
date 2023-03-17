
### check if two objects are identical and print differences else
isequal <- function(a, b) {
    attributes(a) <- NULL
    attributes(b) <- NULL
    if (!isTRUE(all.equal(a, b))) {
        print(a, digits = 10)
        print(b, digits = 10)
        return(FALSE)
    } else {
        return(TRUE) 
    }
}

logchisq.test <- function(x) {
    cs <- colSums(x) > 0
    rs <- rowSums(x) > 0
    if (sum(cs) < 2 || sum(rs) < 2) return(0)
    if (min(x) < 10 && sum(x) < 100) {
        ctest <- chisq.test(x[rs, cs], correct = FALSE, 
                            simulate.p.value = TRUE, B = 9999)
        X2 <- ctest$statistic
        ret <- log(ctest$p.value)
    } else {
        suppressWarnings(ctest <- chisq.test(x[rs, cs], correct = FALSE))
        X2 <- ctest$statistic
        df <- ctest$parameter
        ret <- pchisq(X2, df = df, lower.tail = FALSE, log.p = TRUE)
    }
    attr(ret, "Chisq") <- X2
    ret
}

### merge two levels of a factor
mergelevels <- function(index, merge) {

    stopifnot(length(unique(merge)) == 2)
    stopifnot(all(merge %in% index))

    ### which _original levels_ are to be merged
    ml <- index %in% merge
    ### which levels must be relabeled
    gr <- index > max(merge)

    ### merge levels
    index[ml] <- min(index[ml])
    ### relabel
    index[gr] <- index[gr] - 1
    
    return(index)
}

### split a merger in two groups
splitlevels <- function(index, level, split) {

    stopifnot(sum(index == level) > length(split))

    ### which levels must be relabeled
    gr <- index > level
    ### relabel
    index[gr] <- index[gr] + 1
    ### split
    index[split] <- level + 1

    return(index)
}

### actually merge factor levels
mergex <- function(x, index) {

    ### extract levels and save observations as character
    lev <- levels(x)
    if (is.ordered(x))
        stopifnot(all(diff(index) %in% c(0, 1)))
    chrx <- as.character(x)
    newlev <- rep("", length(unique(index)))
    for (i in unique(index)) {
        indx <- index == i
        ### assign merged levels to observations
        chrx[chrx %in% lev[indx]] <- paste(lev[indx], collapse = "+")
        ### merge levels itself
        newlev[i] <- paste(lev[indx], collapse = "+")
    }
    return(factor(chrx, levels = newlev, ordered=is.ordered(x)))
}



step1internal <- function(response, x, weights, index = NULL, ctrl) {
  
  
  
  alpha2 <- ctrl$alpha2
  
  alpha3 <- ctrl$alpha3
  
  stopifnot(alpha2 > alpha3)
  
  stopifnot(is.factor(x))
  
  if (is.null(index))
    
    index <- 1:nlevels(x)
  
  state <- NULL
  
 
  while(TRUE) {
    
  
    ### nothing to do for two categories
    
    if (max(index) == 2){break} 
    
    
    
    ### merge levels
    
    result <- step2(response, x, weights, index, ctrl, state)
    
    mlev <- result$mlev
    
    state <- result$state
    
    
    
    ### nothing to merge, return index
    
    if (is.null(mlev)) break()
    
    
    
    ### step 3 necessary? 
    
    runstep3 <- sum(mlev[1] == index) > 1 ||
      
      sum(mlev[2] == index) > 1
    
    runstep3 <- runstep3 && (alpha3 > 0)
    
    
    
    ### actually merge levels
    
    kati <- index %in% mlev
    
    index <- mergelevels(index, mlev)
    
    kat <- unique(index[kati])
    
    
    
    ### perform step 3 if necessary
    
    if (runstep3) {
      
      
      
      index <- step3(x,response, weights, alpha3 = alpha3, index, kat)
      
      #state <- NULL
      
      
      
      if(is.ordered(x)){
        
        
        
        if(!(all(diff(index) %in% c(0, 1)))){
          
          
          
          dum <- aggregate(index,by=list(index),FUN=length)
          
          colnames(dum) <- c("index","times")
          
          dum$ind <- unique(index)
          
          
          
          dum1 <- dum[,c("times","ind")]
          
          dum1 <- dum1[order(dum1$ind),]
          
          
          index <- rep(dum1$ind,dum1$times)
        }
        
      }
      
    
    }
    
    
  }
  
  
  
  attr(index, "state") <- state
  
  return(index)
  
}

step1 <- function(response, xvars, weights, indices = NULL, ctrl) {

    ret <- vector(mode = "list", length = length(xvars))
    for (i in 1:length(xvars))
        ret[[i]] <- step1internal(response, xvars[[i]], weights, indices[[i]], ctrl)
    ret
}

step2 <- function(response, x, weights, index = 1:nlevels(x), ctrl, state=NULL) {
  
  stopifnot(is.factor(response))
  
  stopifnot(is.factor(x))
  
  if (nlevels(response[, drop = TRUE]) < 2) return(NULL)
  
  
  
  if (is.null(state)) {
    
    mergedx <- x
    
    if (nlevels(mergedx[, drop = TRUE]) < 3) return(NULL)
    
    xytab <- xtabs(weights ~ mergedx + response)
    
    logpmaxs <- matrix(NA, nrow=nrow(xytab), ncol=nrow(xytab))
    
  }
  
  else {
    
    xytab <- state$xytab
    
    mergedx <- state$mergedx
    
    logpmaxs <- state$logpmaxs
    
  }
  
  
  
  if(nlevels(mergedx)>1){
    
    
    
    comb <- switch(class(mergedx)[1], 
                   
                   "factor" = lapply(1:(nlevels(mergedx) - 1), function(i) (i + 1):nlevels(mergedx)),
                   
                   "ordered" = lapply(1:(nlevels(mergedx) - 1), function(i) i + 1),
                   
                   stop("unknown class")
                   
    )
    
    
    
    
    
    for (i in 1:length(comb)) {
      
      for (j in comb[[i]]) {
        
        if (is.na(logpmaxs[i, j])) {
          
          X <- xytab[c(i, j), ]
          
          
          
          if(length(dim(X)) > 1){
            
            logpmaxs[i, j] <- logchisq.test(X)
            
          }else{logpmaxs[i, j] <- NA}
          
          
          
        }
        
      }
      
    }
    
    
    
    logpmax <- max(logpmaxs, na.rm=TRUE)
    
    pos <- which.max(logpmaxs)
    
    levindx <- c(pos %% nrow(logpmaxs), as.integer(pos / nrow(logpmaxs)) + 1)
    
    
    
    ### sample size stopping criteria
    
    nmin <- min(c(ceiling(ctrl$minprob * sum(weights)), ctrl$minbucket))
    
    
    
    if (exp(logpmax) > ctrl$alpha2 || any(rowSums(xytab) < nmin)) {
      
      xytab[min(levindx),] <- colSums(xytab[levindx,])
      
      mergedx[mergedx==rownames(xytab)[max(levindx)]] <- rownames(xytab)[min(levindx)]
      
      xytab <- xytab[-max(levindx),]
      
      
      
      if(is.null(rownames(xytab))){
        
        mergedx <- factor(mergedx, levels=1,
                          
                          ordered=is.ordered(mergedx))  
        
      }else{
        
        mergedx <- factor(mergedx, levels=rownames(xytab),
                          
                          ordered=is.ordered(mergedx))
        
      }
      
      
      
      logpmaxs[levindx,] <- NA
      
      logpmaxs[,levindx] <- NA        
      
      logpmaxs <- logpmaxs[-min(levindx), -max(levindx)]
      
      return(list(mlev=levindx, state=list(xytab=xytab, mergedx=mergedx,
                                           
                                           logpmaxs=logpmaxs)))
      
    }
    
    
    
  }
  
  
  
  return(NULL)
  
}


step3 <- function(x, y, weights, alpha3 = 0.049, index, kat) {

    split_indx <- index
    if (sum(index == kat) > 2) {
        sp <- step3intern(x, y, weights, alpha3, index, kat)
        ### compute minimum p-value and split
        if(!is.null(sp))
            split_indx <- splitlevels(index, level = kat, sp$split)
    }
    ## return new index 
    return(split_indx)
}

step3intern <- function(x, y, weights, alpha3=0.05, index, kat){
  
  ### determine all admissible combinations
  
  foo <- function(nll) {
    
    if (is.ordered(x)){
      
      ret<-matrix(FALSE,ncol=nll,nrow=nll-1)
      
      for(i in 1:(nll-1)) {
        
        for(j in 1:(nll-1))  {
          
          if(i<=j){
            
            ret[j,i]<-TRUE}}}}
    
    else{
      
      indl <- rep(FALSE, nll)
      
      indl[1] <- TRUE
      
      mi <- 2^(nll - 1)
      
      ret <- matrix(FALSE, ncol = nll, nrow = mi - 1)
      
      for (i in 1:(mi - 1)) {
        
        ii <- i
        
        for(l in 1:(nll-1)) {
          
          indl[l] <- as.logical(ii%%2)
          
          ii <- ii %/% 2
          
        }
        
        ret[i,] <- indl
        
      }
      
    }
    
    return(ret)
    
  }
  
  subsetx <- x %in% levels(x)[index == kat]
  
  ytmp <- y[subsetx, drop = TRUE]
  
  xtmp <- x[subsetx, drop = TRUE]
  
  wtmp <- weights[subsetx]
  
  xlev <- levels(xtmp)
  
  if(nlevels(xtmp) > 1){  
    
    ret <- foo(nlevels(xtmp))
    
    logp <- numeric(nrow(ret))
    
    for (i in 1:nrow(ret)){
      
      tmpx <- as.factor(xtmp %in% xlev[ret[i, ]])
      
      mat <- xtabs(wtmp ~ tmpx + ytmp)
      
      
      
      if(length(dim(mat)) >1){
        
        logp[i] <- logchisq.test(mat)
        
      }else{ logp[i] <- NA }
      
      
      
    }
    
    logp_min <- min(logp)
    
    if (exp(logp_min) > alpha3) return(NULL)
    
    splitlev <- xlev[ret[which.min(logp),]]
    
    return(list(logp = logp_min, split = which(levels(x) %in% splitlev)))
    
  } else { return(NULL) }  
  
  
  
}

step4internal <- function(response, x, weights, index, ctrl) {

    if (nlevels(response[, drop = TRUE]) < 2) return(0)
    state <- attr(index, "state")
    if (is.null(state))
        mx <- mergex(x, index)
    else
        mx <- state$mergedx

    nmin <- min(c(ceiling(ctrl$minprob * sum(weights)), ctrl$minbucket))
    if (any(table(mx[weights > 0]) < nmin)) return(0)

    c_levels <- nlevels(x[weights > 0, drop = TRUE])
    r_levels <- nlevels(mx)

    if (is.null(state))
        xytab <- xtabs(weights ~ response + mx)
    else
        xytab <- state$xytab

    ### p-value on log-scale!

    if(length(dim(xytab)) > 1){
    logp <- logchisq.test(xytab)
    }else{logp = 0}
    
    if (logp == 0) return(0)

    if (is.ordered(x)) {
        ### formula (3.1) in Kass (1980)
        ret <- logp + lchoose(c_levels - 1,r_levels - 1)
    } else {
        i <- 0:(r_levels - 1) ### formula (3.2)
        fact <- sum((-1)^i * ((r_levels - i)^c_levels) / 
                              (factorial(i) * factorial(r_levels - i)))
        ret <- logp + log(fact)
    }
    attr(logp, "Chisq") <- attr(logp, "Chisq")
    return(ret)
}

step4 <- function(response, xvars, weights, indices, ctrl, states) {
    states <- attr(indices, "state")
    p <- numeric(length(xvars))
    X2 <- rep(NA, length(xvars))
    for (i in 1:length(xvars)) {
        tmp <- step4internal(response, xvars[[i]], weights, indices[[i]], ctrl)
        p[i] <- tmp
        if (!is.null(attr(tmp, "Chisq")))
            X2[i] <- attr(tmp, "Chisq")
    }
    names(p) <- names(xvars)
    attr(p, "Chisq") <- X2
    return(p)
}

step5 <- function(id = 1L, response, x, weights = NULL, indices = NULL, 
                  ctrl = chaid_control(), height = 0) {

    if (is.null(weights)) weights <- rep.int(1, length(response))
    if (sum(weights) < ctrl$minsplit) 
        return(partynode(id = id))

    if (ctrl$stump && id > 1)
        return(partynode(id = id))

    if (height == ctrl$maxheight) 
        return(partynode(id = id))
    height <- height + 1
    indices <- step1(response, x, weights, indices = indices, ctrl)

    logpvals <- step4(response, x, weights, indices, ctrl)
    info <- list(adjpvals = exp(logpvals))

    if (exp(min(logpvals)) > ctrl$alpha4) return(partynode(id = id, info = info))

    sp <- partysplit(varid = which.min(logpvals), 
                     index = as.integer(indices[[which.min(logpvals)]]))

    ### FIXME: remove???
    ### retain index only for split variable
    newindices <- lapply(x, function(x) 1:nlevels(x))
    ### newindices[[which.min(logpvals)]] <- indices[[which.min(logpvals)]]

    kidids <- kidids_split(sp, data = x)

    kids <- vector(mode = "list", length = max(sp$index))
    for (kidid in 1:max(sp$index)) {
          w <- weights
          w[kidids != kidid] <- 0
          if (kidid > 1) {
              myid <- max(nodeids(kids[[kidid - 1]]))
          } else {
              myid <- id
          }
          kids[[kidid]] <- step5(id = as.integer(myid + 1), response, x, 
                                 weights = w, newindices, ctrl, height)
    }
    return(partynode(id = as.integer(id), split = sp, kids = kids, info = info))
}

chaid_control <- function(alpha2 = 0.05, alpha3 = -1, alpha4 = 0.05,
                          minsplit = 20, minbucket = 7, minprob = 0.01, 
                          stump = FALSE, maxheight = -1) {

    ret <- list(alpha2 = alpha2, alpha3 = alpha3, alpha4 = alpha4,
         minsplit = minsplit, minbucket = minbucket, minprob = minprob,
         stump = stump, maxheight = maxheight)
    class(ret) <- "chaid_control"
    return(ret)
}


chaid <- function(formula, data, subset, weights, na.action = na.omit, 
                  control = chaid_control())
{
      mf <- match.call(expand.dots = FALSE)
      m <- match(c("formula", "data", "subset", "weights", "na.action"),
               names(mf), 0)
      mf <- mf[c(1, m)]
      mf$drop.unused.levels <- FALSE
      mf[[1]] <- as.name("model.frame") 
      m <- eval.parent(mf)
      y <- model.response(m)
      x <- m[, c(-1, -which(names(m) == "(weights)")), drop = FALSE]
      w <- model.weights(m)
      chaidtree <- step5(1L, y, x, weights = w, ctrl = control)
      tree <- party(chaidtree, data = x, 
                    fitted = data.frame("(fitted)" = fitted_node(chaidtree, data = x),
                                        "(response)" = y, check.names = FALSE),
                    terms = terms(formula, data = data))
      if (!missing(weights))
          tree$fitted[["(weights)"]] <- w
      class(tree) <- c("constparty", "party")
      tree
}
