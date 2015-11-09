## Example from http://www.bnlearn.com/research/genetics14/

## Analysis of the MAGIC population in Scutari et al., Genetics (2014)
## This is a short HOWTO describing fit the averaged Bayesian network in “Multiple Quantitative Trait Analysis Using Bayesian Networks” by Scutari, Howell, Balding, Mackay (Genetics, 2014).

download.file(url = "http://www.bnlearn.com/research/genetics14/prepd-magic.txt.xz", destfile = "prepd-magic.txt.xz")

library(lme4)
library(bnlearn)
library(parallel)
 
# load the data.
# xzfd = xzfile("prepd-magic.txt", open = "r")
magic = read.table("prepd-magic.txt", header = TRUE,
       colClasses = c(rep("factor", 4), rep("numeric", 3164)))
# close(xzfd)

## Afterwards, we substitute short labels to the marker names, which are quite long and make plotting problematic. In addition we identify which variables in the data are traits, which are markers, which contain variety IDs and pedigree information.

# code the gene names.
names(magic)[12:ncol(magic)] = paste("G", 12:ncol(magic) - 11, sep = "")
 
# split the variables.
ids = names(magic)[1:4]
traits = names(magic)[5:11]
genes = names(magic)[12:ncol(magic)]

## Finally, we extract observations with incomplete data (in the traits and variety information, missing values in the markers have all been imputed).
# separate observations with missing data from the rest.
partial = magic[!complete.cases(magic), ]
magic = magic[complete.cases(magic), ]

## Performing cross-validation
## The Bayesian networks model is fitted by the fit.the.model() function below, which takes the data and the type I error threshold alpha to use for structure learning as arguments.

## The general layout of fit.the.model() is as follows:
##
##  First, we remove family structure using the pedigree information stored in the FUNNEL and PLANT. This is done for each trait in turn using lmer() to fit a linear mixed model with a random effect for the combination of these variables; the estimated effect is then subtracted from the raw value of the trait. Note that this procedure is specific to the data, others will require different adjustments using either pedigree- or marker-based measures of kinship.
##  Subsequently, we use learn.nbr() to run SI-HITON-PC on each of the traits and identify their parents and children in the Bayesian network. We save them in the cpc variable. The parents of a trait can be either markers or other traits; children can only be traits. The test used to assess (conditional) independence is the exact t test for Pearson' correlation (test = "cor") with the specified alpha threshold.
##  We then merge all the elements of cpc and the traits into a vector of relevant nodes to be used in learning the Bayesian networks. Markers that are not included in this list have not been found to be directly related to any trait and are therefore discarded from the analysis.
##  The nodes are then partitioned in three subsets: markers, traits other than yield, and yield. The tiers2blacklist() function creates a blacklist that prevents arcs from going to yield to any other node, and from traits to the markers. These restrictions restrict the space of the candidate models further and have the aim of forcing causal relationships to flow in the right direction while learning them from the data.
##  Finally, we learn the structure of the Bayesian network by maximising BIC with hill-climbing given the blacklist we just generated.
##
## In order to have multiple models to average and to assess the predictive power of the Bayesian network model, we run fit.the.model() under 10-fold cross-validation for 10 times. Since structure learning has been customised for the analysis, we cannot use bn.cv() from bnlearn. Instead, we implement a custom xval.the.model() function, shown below.

fit.the.model = function(data, alpha) {
  
   cpc = vector(length(traits), mode = "list")
   names(cpc) = traits
  
   # find the parents of each trait (may be genes or other traits).
     for (t in seq_along(traits)) {
      
         # BLUP away the family structure. (We may be unable to do this in the yelp case)
           m = lmer(as.formula(paste(traits[t], "~ (1|FUNNEL:PLANT)")), data = data)
           data[!is.na(data[, traits[t]]), traits[t]] =
               data[, traits[t]] - ranef(m)[[1]][paste(data$FUNNEL, data$PLANT, sep = ":"), 1]
           # find out the parents.
             cpc[[t]] = learn.nbr(data[, c(traits, genes)], node = traits[t], debug = FALSE,
                                                      method = "si.hiton.pc", test = "cor", alpha = alpha)
          
           }#FOR
  
   # merge the relevant variables to use for learning.
     nodes = unique(c(traits, unlist(cpc)))
     # yield has no children, and genes cannot depend on traits. (We have to avoid "stars" has children)
       blacklist = tiers2blacklist(list(nodes[!(nodes %in% traits)],
                                                          traits[traits != "YLD"], "YLD"))
      
       # build the Bayesian network.
         bn = hc(data[, nodes], blacklist = blacklist)
        
         return(bn)
        
       }#FIT.THE.MODEL

## The general layout of xval.the.model() is as follows:
##  
##  We split() the sample into 10 folds, allocating observations at random. Folds are saved in a list, kcv, which then passed to parLapply() to run the analysis over multiple processors.
##  Each fold is used as the test data set (dtest) while the rest of the data set is used as the training set (dtrain). We learn the structure of the Bayesian network on dtrain with fit.the.model, the we fit its parameters with bn.fit(). This produces ordinary least squares estimates of the regression coefficients in the local distributions.
##  As an alternative, if ridge = TRUE the parameters using the ridge regression implementation in the penalized package: optL2() finds the optimal λ2 penalty and penalized() estimates the regression coefficients using that λ2. bnlearn takes care of extracting the coefficients from the models and storing them in the fitted Bayesian network.
##  Subsequently, we produce predictions for the traits in dtest. Causal predictions are computed with predict() conditional on the parents of the trait, which can be considered putative direct causal effects. Genetic predictions are the expected values of the posterior density of the traits given the markers, as computed using colMeans(), cpdist() and likelihood weighting. The former are used to compute ρC, the latter for ρG. Note that in general the average should be weighted, but in this case all the weights are identical due to the blacklist used in fit.the.model().

xval.the.model = function(data, k = 10, cluster, alpha, ridge) {
  
   n = nrow(data)
   predcor = numeric(length(traits))
   names(predcor) = traits
   postcor = numeric(length(traits))
   names(postcor) = traits
  
   # shuffle the data to get unbiased splits.
     kcv = split(sample(n), seq_len(k))
     # store the length of each test set.
       kcv.length = sapply(kcv, length)
      
       predicted = parLapply(kcv, cl = cluster, function(test) {
        
           # create a matrix to store the predicted values.
             pred = matrix(0, nrow = length(test), ncol = length(traits))
             colnames(pred) = traits
             # create a matrix to store posterior estimates.
               post = matrix(0, nrow = length(test), ncol = length(traits))
               colnames(post) = traits
            
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
clusterExport(cl = cl, c("traits", "genes", "ids", "fit.the.model"))
pr001 = vector(10, mode = "list")
for (i in 1:10)
   pr001[[i]] = xval.the.model(magic, cluster = cl, alpha = 0.01, ridge = FALSE)
stopCluster(cl)

pred.summary = sapply(pr001, `[[`, "predcor")
print(rowMeans(pred.summary))

post.summary = sapply(pr001, `[[`, "postcor")
print(rowMeans(post.summary))


# gather all the arc lists.
arclist = list()

  for (i in seq_along(pr001)) {
    
   # extract the models.
     run = pr001[[i]]$models
      
     for (j in seq_along(run))
         arclist[[length(arclist) + 1]] = arcs(run[[j]])
        
     }#FOR

# compute the arc strengths.
nodes = unique(unlist(arclist))
strength = custom.strength(arclist, nodes = nodes)
# estimate the threshold and average the networks.
averaged = averaged.network(strength)


# subset the network to remove isolated nodes.
relevant.nodes = nodes(averaged)[sapply(nodes, degree, object = averaged) > 0]
averaged2 = subgraph(averaged, relevant.nodes)
strength2 = strength[(strength$from %in% relevant.nodes) &
                      (strength$to %in% relevant.nodes), ]
gR = strength.plot(averaged2, strength2, shape = "rectangle", layout = "fdp")

## From http://stackoverflow.com/questions/18023300/is-rgraphviz-no-longer-available-for-r
## You need to install it directly from the bioconductors site.
## source("http://bioconductor.org/biocLite.R")
## biocLite("Rgraphviz")

require("Rgraphviz")
nodeRenderInfo(gR)$fill = "lightblue"
nodeRenderInfo(gR)$fill = "lightblue"
nodeRenderInfo(gR)$col = "darkblue"
nodeRenderInfo(gR)$fill[traits] = "limegreen"
nodeRenderInfo(gR)$col[traits] = "darkgreen"
a = arcs(subgraph(averaged, traits))
a = as.character(interaction(a[, "from"], a[, "to"], sep = "~"))
edgeRenderInfo(gR)$col = "grey"
edgeRenderInfo(gR)$col[a] = "darkgreen"
renderGraph(gR)
