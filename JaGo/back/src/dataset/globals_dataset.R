#global variables for dataset panel
number_click_process=0

get_categories = function(df){
  return(sapply(df, function(x) {
    if (is.numeric(x) || is.integer(x)) {
      if (length(unique(x)) > 11) {
        "quantitative continue"
      } else {
        "quantitative discrète"
      }
    } else if (is.character(x)) {
      "qualitative nominale"
    } else if (is.logical(x)) {
      "qualitative ordinale"
    } else if (is.factor(x)) {
      if (is.ordered(x)) {
        "qualitative ordinale"
      } else {
        "qualitative nominale"
      }
    } else {
      "Inconnu"
    }
  }))
}


get_type_columns=function(x) {
  types = class(x)
  if (length(types) > 1) {
    return(paste(types, collapse = "/"))
  } else {
    return(types[1])
  }
}

get_type_col_by_name=function(df,var_name){
  return(df[df["variable"]==var_name,"category"])
}