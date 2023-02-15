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
    return(c(c("Statistiques"),visualization_types$qualitative))
  } else{
    print("A problem occur!")
  }

}
get_vizualization=function(cat,index_viz){
    return(get_vizualizations(cat)[index_viz])
}

