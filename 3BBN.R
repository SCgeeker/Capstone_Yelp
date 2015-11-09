
## Split the variables
id <- names(yelp_FastFood)[1]
info <- names(yelp_FastFood)[2:6]
attributes <- names(yelp_FastFood)[7:43]



## Codes of fit.the.model and xval.the.model from http://www.bnlearn.com/research/genetics14/

fit.the.model = function(data, alpha) {
  
  cpc = vector(length(info), mode = "list")
  names(cpc) = info
  
  # find the parents of each trait (may be genes or other traits).
  for (t in seq_along(info)) {
    
    # BLUP away the family structure.
    ## m = lmer(as.formula(paste(info[t], "~ (1|FUNNEL:PLANT)")), data = data)
    m = lmer(as.formula(info[t]), data = data)
    ## data[!is.na(data[, info[t]]), info[t]] =
    ##      data[, info[t]] - ranef(m)[[1]][paste(data$FUNNEL, data$PLANT, sep = ":"), 1]
    data[!is.na(data[, info[t]]), info[t]] =
      data[, info[t]] - ranef(m)[[1]]
    # find out the parents.
    cpc[[t]] = learn.nbr(data[, c(info, attributes)], node = info[t], debug = FALSE,
                         method = "si.hiton.pc", test = "cor", alpha = alpha)
    
  }#FOR
  
  # merge the relevant variables to use for learning.
  nodes = unique(c(info, unlist(cpc)))
  # yield has no children, and genes cannot depend on traits.
  ## blacklist = tiers2blacklist(list(nodes[!(nodes %in% info)],
  ##                                 info[traits != "YLD"], "YLD"))
  
  blacklist = tiers2blacklist(list(nodes[!(nodes %in% info)],
                                   info[info != "neighborhoods"], "neighborhoods"))
  
  # build the Bayesian network.
  bn = hc(data[, nodes], blacklist = blacklist)
  
  return(bn)
  
}#FIT.THE.MODEL

xval.the.model = function(data, k = 10, cluster, alpha, ridge) {
  
  n = nrow(data)
  predcor = numeric(length(info))
  names(predcor) = info
  postcor = numeric(length(info))
  names(postcor) = info
  
  # shuffle the data to get unbiased splits.
  kcv = split(sample(n), seq_len(k))
  # store the length of each test set.
  kcv.length = sapply(kcv, length)
  
  predicted = parLapply(kcv, cl = cluster, function(test) {
    
    # create a matrix to store the predicted values.
    pred = matrix(0, nrow = length(test), ncol = length(info))
    colnames(pred) = info
    # create a matrix to store posterior estimates.
    post = matrix(0, nrow = length(test), ncol = length(info))
    colnames(post) = info
    
    cat("* beginning cross-validation fold.\n")
    
    # split training and test.
    dtraining = data[-test, ]
    dtest = data[test, ]
    # fit the model on the training data.
    model = fit.the.model(dtraining, alpha = alpha)
    fitted = bn.fit(model, dtraining[, nodes(model)])
    # maybe re-fit with ridge regression.
    if (ridge) {
      
      library(penalized)
      
      for (no in nodes(fitted)) {
        
        node.parents = parents(fitted, no)
        
        if (length(node.parents) < 3)
          next
        
        opt.lambda = optL2(response = dtraining[, no],
                           penalized = dtraining[, node.parents],
                           model = "linear", trace = FALSE,
                           minlambda2 = 10e-5, maxlambda = 500)$lambda
        fitted[[no]] = penalized(response = dtraining[, no],
                                 penalized = dtraining[, node.parents],
                                 model = "linear", trace = FALSE,
                                 lambda1 = 0, lambda2 = opt.lambda)
        
      }#FOR
      
    }#THEN
    # subset the test data.
    dtest = dtest[, nodes(model)]
    
    cat("  model has", length(nodes(model)), "nodes.\n")
    
    # predict each trait in turn, given all the parents.
    for (t in traits)
      pred[, t] = predict(fitted, node = t, data = dtest[, nodes(model)])
    
    for (i in seq(nrow(dtest)))
      post[i, traits] = colMeans(cpdist(fitted, nodes = traits,
                                        evidence = as.list(dtest[i, names(dtest) %in% genes]),
                                        method = "lw", n = 1000))
    
    return(list(model = fitted, pred = pred, post = post))
    
  })
  
  # merge all the predicted values.
  posterior = do.call(rbind, lapply(predicted, `[[`, "post"))
  causal = do.call(rbind, lapply(predicted, `[[`, "pred"))
  
  cat("* overall cross-validated correlations:\n")
  for (t in traits) {
    
    predcor[t] = cor(causal[, t], data[unlist(kcv), t])
    cat("  PREDCOR(", t, "):", predcor[t], "\n")
    postcor[t] = cor(posterior[, t], data[unlist(kcv), t])
    cat("  POSTCOR(", t, "):", postcor[t], "\n")
    
  }#FOR
  
  return(list(predicted = causal, posterior = posterior,
              observed = data[unlist(kcv), t], predcor = predcor, postcor = postcor,
              models = lapply(predicted, `[[`, "model")))
  
}#XVAL.THE.MODEL




cl = makeCluster(10)
invisible(clusterEvalQ(cl, library(bnlearn)))
invisible(clusterEvalQ(cl, library(lme4)))
clusterExport(cl = cl, c("info", "attributes", "id", "fit.the.model"))
FastFood001 = vector(10, mode = "list")

for (i in 1:10)
  FastFood001[[i]] = xval.the.model(yelp_FastFood, cluster = cl, alpha = 0.01, ridge = FALSE)

stopCluster(cl)
