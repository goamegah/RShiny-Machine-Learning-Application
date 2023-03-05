


process_type = function(df, col_categories) {

  # Loop over each column in the dataframe
  for (i in seq_along(df)) {

    # Get the category of the current column
    cat = col_categories[i]

    # Clean and transform the column based on its category
    if (cat == "quantitative continue") {
      # Clean the column (remove missing values, convert to numeric)
      df[[i]] = as.numeric(as.character(df[[i]]))
      df[[i]][is.na(df[[i]])] = mean(df[[i]], na.rm = TRUE)

    } else if (cat == "quantitative discrète") {
      # Clean the column (remove missing values, convert to integer)
      df[[i]] = as.integer(as.character(df[[i]]))
      df[[i]][is.na(df[[i]])] = as.integer(round(mean(df[[i]], na.rm = TRUE)))

    } else if (cat == "qualitative nominale") {
      # Clean the column (remove missing values, convert to factor)
      x=df[[i]][!is.na(df[[i]])]
      ux <- unique(x)
      val_mostfreq=ux[which.max(tabulate(match(x, ux)))]
      df[[i]][is.na(df[[i]])] =as.character(val_mostfreq)
      df[[i]] = as.factor(as.character(df[[i]]))


    } else if (cat == "qualitative ordinale") {
      # Clean the column (remove missing values, convert to factor)
      x=df[[i]][!is.na(df[[i]])]
      ux <- unique(x)
      val_mostfreq=ux[which.max(tabulate(match(x, ux)))]
      df[[i]][is.na(df[[i]])] =as.character(val_mostfreq)
      df[[i]] = as.factor(as.character(df[[i]]))
      df[[i]] = factor(df[[i]], ordered = TRUE)
    } else {
      stop("Unknown category, skip the column")
      next
    }
  }
  # Return the cleaned and transformed dataframe
  return(df)
}


process_outliers = function(df,df_all,df_types, col_name, k = 1.5) {
  # Extract column and remove NAs
  col = df[[col_name]]
  col = col[!is.na(col)]

  # Calculate lower and upper limits for outliers
  q1 = quantile(col, probs = 0.25)
  q3 = quantile(col, probs = 0.75)
  iqr = q3 - q1
  lower_limit = q1 - k * iqr
  upper_limit = q3 + k * iqr
  col_names=colnames(df)
  # Remove outliers
  df = as.data.frame(df[col >= lower_limit & col <= upper_limit,])
  colnames(df)=col_names
  col_names_all=colnames(df_all)
  df_all=as.data.frame(df_all[col >= lower_limit & col <= upper_limit,])
  colnames(df_all)=col_names_all
  df_types[,"Number of Modalities"]=sapply(df,function(x){length(unique(x))})
  return(list(table=df,table_all=df_all,type_table=df_types))
}


process_normalize = function(df,df_all,df_types,col_categories_all,col_name, method = "standardize") {
  # Extract column and remove NAs
  col = df[[col_name]]
  col = col[!is.na(col)]

  # Normalize column
  if (method == "standardize") {
    col = (col - mean(col)) / sd(col)
  } else if (method == "normalize") {
    col = (col - min(col)) / (max(col) - min(col))
  } else {
    stop("Invalid normalization method. Choose 'standardize' or 'normalize'.")
  }
  df[[col_name]] = as.numeric(col)
  df_all[[col_name]] = as.numeric(col)
  df_types[df_types["variable"]==col_name,]=c(col_name,class(df[[col_name]]),
                                              "quantitative continue",length(unique(df[[col_name]])))
  col_categories_all[col_name]="quantitative continue"
  return(list(table=df,table_all=df_all,type_table=df_types,col_categories_all=col_categories_all))
}



process_dummy = function(df,df_all,df_types,col_categories_all,col_name) {
  before_colnames=colnames(df_all)
  df_all=dummy_cols(df_all, select_columns = col_name)
  new_colnames=setdiff(colnames(df_all),before_colnames)
  df_all[new_colnames]=sapply(df_all[new_colnames],as.numeric)
  df[new_colnames]=as.data.frame(df_all[new_colnames])
  names(df)[names(df) %in% new_colnames]=gsub(" ","",new_colnames)
  names(df_all)[names(df_all) %in% new_colnames]=gsub(" ","",new_colnames)
  new_colnames=gsub(" ","",new_colnames)
  df_newcols=data.frame(variable=new_colnames,type=sapply(df[new_colnames],function(x){get_type_columns(x)}),
                        category=rep("quantitative discrète",length(new_colnames)),
                        modality=sapply(df[new_colnames],function(x){length(unique(x))}))
  colnames(df_newcols)[4]="Number of Modalities"
  rownames(df_newcols)=NULL
  df_types=rbind(df_types,df_newcols)
  col_categories_all[new_colnames]=rep("quantitative discrète",length(new_colnames))
  return(list(table=df,table_all=df_all,type_table=df_types,col_categories_all=col_categories_all))
}
