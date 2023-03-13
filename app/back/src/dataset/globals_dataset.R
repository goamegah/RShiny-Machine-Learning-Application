#global variables for dataset panel
number_click_process=0
PARAMS=c("Type de variable","Outliers","Normalisation","Dummification")

get_categories = function(df){
  return(sapply(df, function(x) {
    if (is.integer(x)){
      "quantitative discrète"
    }
    if (is.numeric(x)) {
      x_who_na=x[!is.na(x)]
      if (sum(x_who_na-floor(x_who_na)==0)/length(x_who_na)>.99) {
        "quantitative discrète"
      } else {
        "quantitative continue"
      }
    } else if (is.character(x)) {
      "qualitative nominale"
    } else if (is.logical(x)) {
      "qualitative nominale"
    } else if (is.factor(x)) {
      if (is.ordered(x)) {
        "qualitative ordinale"
      } else {
        "qualitative nominale"
      }
    }else{
     "qualitative nominale"
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