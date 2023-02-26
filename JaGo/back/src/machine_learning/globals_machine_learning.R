MODELES_BIN=c("Régression Logistique","Arbre de décision CART","Arbre de décision CHAID")

discretization_col=function(col_vec,nbins){
  col_vec=as.numeric(col_vec)
  breaks = seq(min(col_vec), max(col_vec), length.out = nbins + 1)
  col_vec = cut(col_vec, breaks = breaks,include.lowest = TRUE, right = FALSE)
  return(col_vec)
}

discretization_cols=function(df_cols,col_categories,nbins=10){
    nbins_vec=rep(nbins,length(
      col_categories[col_categories %in% c("quantitative continue","quantitative discrète")])
    )

  j=1
  for (i in seq_along(df_cols)) {
    # Get the category of the current column
    cat <- col_categories[i]
    if (cat == "quantitative continue" || cat == "quantitative discrète"  ) {
      df_cols[[i]] = discretization_col(df_cols[[i]],nbins_vec[j])
      j=j+1
    }
  }
    return(df_cols)
}
