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
    print("A problem occur!")
  }

}





visualization_types_bi = function(cat){
  if (cat =="quantitative continue" || cat =="quantitative discrète"){
    return("quantitative")
  }
  if (cat == "qualitative nominale" || cat =="qualitative ordinale" ){
    return("qualitative")
  } else{
    print(paste("A problem occur from visualization_types_bi:",cat))
  }


}


get_vizualizations_bi=function(cats){
  viz_global=c("Tableau de Contingence en Effectif","Tableau de Contingence en Fréquence")
  catgen1=visualization_types_bi(cats[1])
  catgen2=visualization_types_bi(cats[2])
  if (catgen1 == "quantitative" && catgen2 == "quantitative"){
    return(c(viz_global,c("Boite à Moustaches")))
  }
  if (catgen1 == "qualitative" && catgen2 == "qualitative"){
    return(viz_global)

  }
  if (catgen1 == "quantitative" && catgen2 == "qualitative"){
    return(viz_global)

  }
  if (catgen1 == "qualitative" && catgen2 == "quantitative"){
    return(c(viz_global,c("Boite à Moustaches")))

  }
  else{
    print(paste("A problem occur from get_vizualizations_bi:",cats))
  }
}

