visualization_types = list(
  quantitative_discreet = c("Tableau des Effectifs/Fréquence", "Diagramme en bâtons","Fonction de répartition empirique"),
  quantitative_continous = c("Tableau des Effectifs/Fréquence par classe", "Histogramme","Fonction de répartition empirique"),
  qualitative = c("Tableau des Effectifs/Fréquence", "Diagramme en barres" ,"Diagramme circulaire")
)

get_vizualizations=function(cat){
  if (cat == "quantitative continue") {
    return(c(c("Statistiques"),visualization_types$quantitative_continous))
  } else if (cat == "quantitative discrète") {
    return(c(c("Statistiques"),visualization_types$quantitative_discreet))
  } else if (cat == "qualitative nominale" || cat == "qualitative ordinale") {
    return(visualization_types$qualitative)
  } else{
    stop("A problem occur!")
  }

}

bins_require_viz_uni=function(viz){
  if (viz == "Histogramme" || viz == "Tableau des Effectifs/Fréquence par classe"){
    return(TRUE)
  } else{
    return(FALSE)
  }
}




visualization_types_bi = function(cat){
  if (cat =="quantitative continue" || cat =="quantitative discrète"){
    return("quantitative")
  }
  if (cat == "qualitative nominale" || cat =="qualitative ordinale" ){
    return("qualitative")
  } else{
    stop(paste("A problem occur from visualization_types_bi:",cat))
  }


}


get_vizualizations_bi=function(cats){
  viz_global=c("Tableau de Contingence en Effectif","Tableau de Contingence en Fréquence","Diagramme en Barres")
  catgen1=visualization_types_bi(cats[1])
  catgen2=visualization_types_bi(cats[2])
  if (catgen1 == "quantitative" && catgen2 == "quantitative"){
    return(c(viz_global,c("Statistiques","Boite à Moustaches","TreeMap","Nuage de points")))
  }
  if (catgen1 == "qualitative" && catgen2 == "qualitative"){
    return(viz_global)

  }
  if (catgen1 == "quantitative" && catgen2 == "qualitative"){
    return(viz_global)

  }
  if (catgen1 == "qualitative" && catgen2 == "quantitative"){
    return(c(viz_global,c("Statistiques","Boite à Moustaches","TreeMap")))

  }
  else{
    stop(paste("A problem occur from get_vizualizations_bi:",cats))
  }
}

bins_require_viz_bi=function(viz,cats){
  cat1=visualization_types_bi(cats[1])
  cat2=visualization_types_bi(cats[2])
  if (viz == "Tableau de Contingence en Effectif" || viz == "Tableau de Contingence en Fréquence" || viz == "Diagramme en Barres"){
    return (cat1 == "quantitative" || cat2 == "quantitative")
  }
  if (viz== "Boite à Moustaches"  || viz == "TreeMap" || viz == "Statistiques"){
    return(cat1 == "quantitative")
  }
  return(FALSE)
}

