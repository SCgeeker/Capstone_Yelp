load("../Yelp_Data/Model.RData")

## BBN Graphs
### Fast Food
#### Normal BBN plot
names(yelp_FastFood) <- c("ID", "L", "S", "H", "R", "N", paste0("A",1:37))
yelp_FastFood.gs <- gs(yelp_FastFood[,c(3,2,4:43)])
yelp_FastFood.hc <- hc(yelp_FastFood[,c(3,2,4:43)])
compare(yelp_FastFood.gs, yelp_FastFood.hc)
plot(yelp_FastFood.gs,  main  =  "Constraint-based  algorithms",  highlight  =  c("S",  "L"))
plot(yelp_FastFood.hc,  main  =  "Hill-Climbing",  highlight  =  c("S",  "L"))

#### BBN Graphviz
gs.opt <- list(nodes = c("S", "L"), arcs = c("S", "L"), col = "red", fill = "grey")
graphviz.plot(yelp_FastFood.gs, highlight = gs.opt)
hc.opt <- list(nodes = c("S", "L"), arcs = c("S", "L"), col = "red", fill = "grey")
graphviz.plot(yelp_FastFood.hc, highlight = hc.opt)

##### In the case of fast food, both "Constraint-based  algorithms" and "Hill-Climbing" plot the Location as the prior event of stars.
##### Constraint-based algorithms pick the direct events to 'stars': 'review count', 'Outdoor.Seating'and 'dessert'. 'Loc' is the prior event of 'review count'

##### "Loc" is the top node that directly influence "Accepts.Credit.Cards", "Good.For.Groups", "Price.Range", and "Good.for.Kids".

### Chinese
names(yelp_Chinese) <- c("ID", "L", "S", "H", "R", "N", paste0("A",1:37))
yelp_Chinese.gs <- gs(yelp_Chinese[,c(3,2,4:43)])
yelp_Chinese.hc <- hc(yelp_Chinese[,c(3,2,4:43)])
plot(yelp_Chinese.gs,  main  =  "Chinese resturants: Constraint-based  algorithms",  highlight  =  c("stars",  "Loc"))
plot(yelp_Chinese.hc,  main  =  "Chinese resturants: Hill-Climbing",  highlight  =  c("stars",  "Loc"))
gs.opt <- list(nodes = c("stars", "Loc"), arcs = c("stars", "Loc"), col = "red", fill = "grey")
graphviz.plot(yelp_Chinese.gs, highlight = gs.opt)
hc.opt <- list(nodes = c("stars", "Loc"), arcs = c("stars", "Loc"), col = "red", fill = "grey")
graphviz.plot(yelp_Chinese.hc, highlight = hc.opt)

#####  "Loc" decide the hours of open of Chinese resturants. Hours of open directly influence "stars". "Loc" and "stars" decide the other attributes of Chinese resturants.




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
