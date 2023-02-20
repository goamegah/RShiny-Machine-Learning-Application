statistiques=function(df_col){
  col_name=colnames(df_col)[1]
  vec_col=df_col[,1]
  return(tibble(
    Nom=col_name,
    Minimum = min(vec_col),
    "Quantile 25%"=quantile(vec_col,probs = .25),
    "Quantile 50%"=quantile(vec_col,probs = .5),
    "Quantile 75%"=quantile(vec_col,probs = .75),
    Maximum = max(vec_col),
    Moyenne = mean(vec_col),
    Médiane = median(vec_col),
    "Écart-type" = sd(vec_col),
    Variance = var(vec_col)
  ))
}

#------------------------------------------------------------------discreet quantitative part
create_freq_table_discreet = function(df_col) {
  table = table(df_col)
  table_data = as.data.frame(table)
  colnames(table_data) = c("Valeur", "Effectif")
  table_data$Fréquence = round(table_data$Effectif / sum(table_data$Effectif), 5)
  table_data = table_data[order(table_data$Valeur),]
  return(table_data)
}


create_bar_plot_discreet = function(df_col) {
  freq_table = create_freq_table_discreet(df_col)
  barplot(freq_table$Effectif, names.arg = freq_table$Valeur, xlab = "Catégories", ylab = "Effectif")
}

create_ecdf_plot_discreet = function(df_col) {
  col_name=colnames(df_col)[1]
  df_col=df_col[,1]
  plot(ecdf(df_col),main="Fonction de répartition empirique", xlab = col_name, ylab = "Fréquence cumulée")
}

#-------------------------------------------------------------------


#------------------------------------------------------------------continous quantitative part
create_freq_table_continous = function(df_col, nbins = 10) {
  df_col=df_col[,1]
  breaks = seq(min(df_col), max(df_col), length.out = nbins + 1)
  table_data = cut(df_col, breaks, include.lowest = TRUE, right = FALSE)
  table_data = as.data.frame(table(table_data))
  colnames(table_data) = c("Intervalle", "Effectif")
  table_data$frequence = round(table_data$Effectif / sum(table_data$Effectif) , 5)
  colnames(table_data)[3]="Fréquence"
  return(table_data)
}

create_histogram_continous = function(df_col, nbins = 10, ylab = "Fréquence") {
  col_name=colnames(df_col)[1]
  df_col=df_col[,1]
  breaks = seq(min(df_col), max(df_col), length.out = nbins + 1)
  hist_data = hist(df_col, breaks = breaks, main = paste("Histogram de", col_name), xlab = col_name, ylab = ylab)
  return(hist_data)
}


create_ecdf_plot_continous = function(df_col, ylab = "Fréquences Cumulées") {
  xlab=colnames(df_col)[1]
  df_col=df_col[,1]
  ecdf_data = ecdf(df_col)
  plot(ecdf_data, main = paste("Courbe des Fréquences Cumulées de", xlab), xlab = xlab, ylab = ylab)
  abline(h = 0, col = "gray")
}

#-------------------------------------------------------------------


#------------------------------------------------------------------- qualitative part
tableau_effectifs_freq_qualitative = function(df_col) {
  table_df = table(df_col)
  table_df = as.data.frame(table_df)
  colnames(table_df) = c(colnames(df_col)[1], "Effectif")
  table_df$Fréquence = table_df$Effectif / sum(table_df$Effectif)
  return(table_df)
}

diag_barres_qualitative = function(df_col) {
  count_df = as.data.frame(table(df_col))
  return(
    ggplot(count_df, aes(x = as.factor(count_df[,1]), y = count_df[,2])) +
      geom_bar(stat="identity",width = 0.8) +
      ggtitle("Diagramme en barres") +
      xlab(colnames(df_col)[1]) + ylab("Fréquence")
  )
}

diag_circulaire_qualitative = function(df_col) {
  count_df = as.data.frame(table(df_col))
  if (length(count_df[, 1]) < 7){
    label=paste(count_df[, 1], "\n", round(count_df[, 2] / sum(count_df[, 2]) * 100, 5), "%", sep = "")
    return(
      ggplot(count_df, aes(x = "", y = count_df[,2], fill = count_df[,1],label = label )) +
        geom_bar(width = 0.8, stat = "identity") +
        geom_text(position = position_stack(vjust = 0.5), color = "white", size = 4) +
        ggtitle("Diagramme circulaire") +
        scale_fill_brewer(palette = "Set1") +
        coord_polar("y", start = 0) +
        theme(legend.position = "bottom")+
        xlab(NULL) + ylab(NULL) +
        guides(fill = guide_legend(title = "Catégories"))
    )
  } else {
    return(
      ggplot(count_df, aes(x = "", y = count_df[,2], fill = count_df[,1])) +
        geom_bar(width = 0.8, stat = "identity") +
        ggtitle("Diagramme circulaire") +
        scale_fill_brewer(palette = "Set1") +
        coord_polar("y", start = 0) +
        theme(legend.position = "bottom")+
        xlab(NULL) + ylab(NULL) +
        guides(fill = guide_legend(title = "Catégories"))
    )


  }

}

#------------------------------------------------------------------- 2 variables

two_var_contingency_table = function(df_cols,type_cols,nbins=10,with_freq=FALSE) {
  # Convert variables to categorical if they are quantitative
  col_names=colnames(df_cols)
  var1=col_names[1]
  var2=col_names[2]
  if (type_cols[1]=="quantitative continue" || type_cols[1]=="quantitative discrète") {
    df_cols[[var1]]=as.numeric(df_cols[[var1]])
    breaks = seq(min(df_cols[var1]), max(df_cols[var1]), length.out = nbins + 1)
    df_cols[[var1]] = cut(df_cols[[var1]], breaks = breaks,include.lowest = TRUE, right = FALSE)
  }
  if (type_cols[2]=="quantitative continue" || type_cols[2]=="quantitative discrète") {
    df_cols[[var2]]=as.numeric(df_cols[[var2]])
    breaks = seq(min(df_cols[var2]), max(df_cols[var2]), length.out = nbins + 1)
    df_cols[[var2]] = cut(df_cols[[var2]], breaks = breaks,include.lowest = TRUE, right = FALSE)
  }
  ct=table(df_cols[[var1]], df_cols[[var2]])
  if (! with_freq){
    # Create contingency table
    return(cbind(ct))
  }else{
    # Compute frequencies
    return(cbind(prop.table(ct)))
  }
}


two_var_boxplot_qual_var_cond = function(df_cols, type_cols,nbins=10){
  #hypothesis: var2:quantitative
  var1 = colnames(df_cols)[1]
  var2 = colnames(df_cols)[2]
  if(type_cols[1] == "quantitative continue" || type_cols[1] == "quantitative discrète"){
    breaks = seq(min(df_cols[var1]), max(df_cols[var1]), length.out = nbins + 1)
    df_col1 = cut(df_cols[[var1]], breaks = breaks,include.lowest = TRUE, right = FALSE)
    df_cols[var1]=df_col1

  }
  return(
    ggplot(df_cols, aes(x=!!sym(var1), y=!!sym(var2),fill=var1)) +
    geom_boxplot(fill="slateblue", alpha=0.2)
  )
}


two_var_barplot_cond = function(df_cols, type_cols,nbins=10){
  var1 = colnames(df_cols)[1]
  var2 = colnames(df_cols)[2]
  if(type_cols[1] == "quantitative continue" || type_cols[1] == "quantitative discrète"){
    breaks = seq(min(df_cols[var1]), max(df_cols[var1]), length.out = nbins + 1)
    df_col1 = cut(df_cols[[var1]], breaks = breaks,include.lowest = TRUE, right = FALSE)
    df_cols[var1]=df_col1
  }
  if(type_cols[2] == "quantitative continue" || type_cols[2] == "quantitative discrète"){
    breaks = seq(min(df_cols[var2]), max(df_cols[var2]), length.out = nbins + 1)
    df_col2 = cut(df_cols[[var2]], breaks = breaks,include.lowest = TRUE, right = FALSE)
    df_cols[var2]=df_col2
  }
  df_counts = df_cols %>%
    group_by(!!sym(var1), !!sym(var2)) %>% #transform var1,var2 string to symbol for dplyr library
                                            #lien:https://larmarange.github.io/analyse-R/manipuler-les-donnees-avec-dplyr.html
    summarize(count = n())
  return(
    ggplot(df_counts, aes(x = !!sym(var2), y = count, fill = !!sym(var1))) +
      geom_bar(position = "dodge", stat = "identity")+
      ylab("Effectif")
  )

}



two_var_treemap_2_var = function(df_cols, type_cols,nbins=10){
  #hypothesis: var2:quantitative
  var1 = colnames(df_cols)[1]
  var2 = colnames(df_cols)[2]
  if(type_cols[1] == "quantitative continue" || type_cols[1] == "quantitative discrète"){
    breaks = seq(min(df_cols[var1]), max(df_cols[var1]), length.out = nbins + 1)
    df_col1 = cut(df_cols[[var1]], breaks = breaks,include.lowest = TRUE, right = FALSE)
    df_cols[var1]=df_col1

  }

  df_tree = df_cols %>%
    group_by(!!sym(var1)) %>% #transform var1,var2 string to symbol for dplyr library
    summarize(value = round((mean(!!sym(var2))),5))
  return(
    ggplot(df_tree, aes(area = !!sym("value"), fill = !!sym(var1), label = paste(!!sym(var1),"\n",!!sym("value")))) +
      geom_treemap() +
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 15)+
      ggtitle(paste("TreeMap représentant les Moyennes conditionnées de ",var2," sachant ",var1))+
      theme_minimal()

  )
}

two_var_scatterplot = function(df_cols, type_cols){
  #hypothesis: var1:quantitative AND var2:quantitative

  var1 = colnames(df_cols)[1]
  var2 = colnames(df_cols)[2]
  ggplot(df_cols, aes(x=!!sym(var1), y=!!sym(var2))) +
    geom_point() +
    labs(x = var1, y = var2) +
    theme_minimal()
}

two_var_statistics= function(df_cols,type_cols,nbins=10){
  #hypotesis : df_cols[2] / df_cols[1]
  #df_cols[2] :quantitative
  var1 = colnames(df_cols)[1]
  var2 = colnames(df_cols)[2]
  if(type_cols[1] == "quantitative continue" || type_cols[1] == "quantitative discrète"){
    breaks = seq(min(df_cols[var1]), max(df_cols[var1]), length.out = nbins + 1)
    df_col1 = cut(df_cols[[var1]], breaks = breaks,include.lowest = TRUE, right = FALSE)
    df_cols[var1]=df_col1
  }
  df_summarize = df_cols %>%
    group_by(!!sym(var1)) %>%
    summarize(
      mean_cond = mean(!!sym(var2)),
      var_cond=var(!!sym(var2))
    )
  df_final=df_summarize
  colnames(df_final)= c(var1,paste("Moyenne Conditionée de ",var2),paste("Variance Conditionée de ",var2))
  return(
    df_final
  )

}