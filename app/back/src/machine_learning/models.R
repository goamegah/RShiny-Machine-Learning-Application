plot_tree_model=function(model,name){
  if(name=="Arbre de décision CART"){
    return(rpart.plot(model,roundint = FALSE))
  }else if(name=="Arbre de décision CHAID"){
    return(plot(model,gp=gpar(fontsize = 10, lwd = 3,fontface="bold")))
  }

}

plot_roc_auc_curve= function(labels,predictions){
  return (roc(labels,predictions,smoothed = TRUE,  plot=TRUE,print.auc=TRUE))
}
decision_tree_model = function(df, outcome,col_categories, method, prop=0.7, prune = FALSE,nbins=10) {
  #hypothesis: if method=="CHAID", all predictors variable are qualitative

  # Split data into training and test sets
  if(method == "CHAID") df[,-match(outcome,colnames(df))]=discretization_cols(df[-match(outcome,colnames(df))],col_categories[-match(outcome,colnames(df))],nbins)
  train = sample(nrow(df), round(prop*nrow(df)), replace = FALSE)
  train_df = df[train,]
  test_df = df[-train,]

  # Build decision tree using specified method
  if(method == "CHAID") {
    train_df = train_df %>%
      mutate_at(colnames(train_df), as.factor)
    test_df = test_df %>%
      mutate_at(colnames(test_df), as.factor)
    tree_model = chaid(as.formula(paste(outcome,"~.")), data = train_df)
  } else if (method == "CART") {
    tree_model = rpart(as.formula(paste(outcome, "~.")), data = train_df,method = "class")
    if(prune) {
      # Get the index of this lowest xerror
      opt = which.min(tree_model$cptable[,"xerror"])
      cp = tree_model$cptable[opt, "CP"]
      tree_model = prune(tree_model, cp)  #pruned tree
    }
  } else {
    stop("Method not recognized. Choose CHAID or CART.")
  }
  # Get predicted probabilities
  pred_prob = predict(tree_model, newdata = test_df[!names(test_df) %in% c(outcome)], type = "prob")[,2]
  true_labels = test_df[[outcome]]
  # Return decision tree model, predicted labels, predicted probabilities, and relevant metrics
  return(list(model = tree_model,true_labels=true_labels, pred_prob = pred_prob))
}

logistic_regression = function(df, outcome,col_categories, prop=0.7){
  #hypothesis features and outcome quantitative
  if ("qualitative nominale" %in% col_categories[!names(col_categories) %in% c(outcome)] || "qualitative ordinale" %in% col_categories[!names(col_categories) %in% c(outcome)]){
    stop("All predictors variables for Logistic Regression should be quantitative.")
  }
  train = sample(nrow(df), round(prop*nrow(df)), replace = FALSE)
  train_df = df[train,]
  test_df = df[-train,]
  true_labels = test_df[[outcome]]
  reg_model=glm(as.formula(paste(outcome,"~.")),data=train_df,family=binomial)
  pred_prob = reg_model %>% predict(test_df[!names(test_df) %in% c(outcome)], type = "response")
  return(list(model = reg_model,true_labels=true_labels, pred_prob = pred_prob))
}


display_informations=function(model,name){
  if(name=="Arbre de décision CART"){
    return(model$cptable)
  }else if(name=="Arbre de décision CHAID"){
    return(NULL)
  }else if(name == "Régression Logistique"){
    return(summary(model)$coef)
  }
}

calculate_metrics = function(actual, predicted) {
  #hypothesis: boolean vector (TRUE or FALSE) for actual and predicted
  tp = sum(predicted == TRUE & actual == TRUE)
  fp = sum(predicted == TRUE & actual == FALSE)
  fn = sum(predicted == FALSE & actual == TRUE)
  tn = sum(predicted == FALSE & actual == FALSE)
  accuracy = (tp + tn) / (tp + tn + fp + fn)
  error_rate = (fp + fn) / (tp + tn + fp + fn)
  precision = tp / (tp + fp)
  recall = tp / (tp + fn)
  f1 = 2 * precision * recall / (precision + recall)
  metrics = data.frame(accuracy = accuracy, error_rate = error_rate, precision = precision, recall = recall, f1 = f1)
  confusion_matrix = matrix(c(tp, fp, fn, tn), nrow = 2, dimnames = list(c("Actual TRUE", "Actual FALSE"), c("Predicted TRUE", "Predicted FALSE")))
  return(list(metrics = metrics, confusion_matrix = confusion_matrix))
}


