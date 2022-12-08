#--------------------------/PREPARATION/--------------------------#

# Génération d'une carte vierge jolie pour ensuite y ajouter des données.
# Utilisation de GGplot2 pour les cartes.

world_map <- ggplot2::map_data("world") %>% 
  dplyr::filter(region != "Antarctica")


basic4map <- ggplot2::ggplot() + 
  ggplot2::coord_fixed() +
  ggplot2::xlab("") +
  ggplot2::ylab("")

#Add map to base plot
base_world <- basic4map +
  ggplot2::geom_polygon(data=world_map,
                        ggplot2::aes(x=long,
                                     y=lat,
                                     group=group), 
                        colour="gray",
                        fill="gray") +
  ggplot2::theme(panel.background = ggplot2::element_rect(
    size = 0.5,
    linetype = "solid"),
    panel.grid.major = ggplot2::element_line(
      size = 0.5,
      linetype = 'solid',
      colour = "gray90"), 
    panel.grid.minor = ggplot2::element_line(
      size = 0.25,
      linetype = 'solid',
      colour = "gray90")) 

rm(basic4map)

#--------------------------/FUNCTION 1/--------------------------#

# Génère une carte montrant la repartition d'un phonème à travers
# le monde, avec différentes couleurs pour representer les
# différentes familles.
#
# ARGUMENTS :
# phoneme : phonème d'interet pour l'analyse
# data_frame : table de données qui doit contenir les colonnes
#              suivantes :
#              - Phoneme
#              - longitude
#              - latitude
#              - Family_name
#              Par défaut phoible
# map : carte vierge, par défaut base_world
#
# VALEUR RETOURNEE :
# Un graphique map-monde pour la répartition globale d'un phonème

one.phoneme_to_map <- function(phoneme,
                               data_frame = phoible,
                               map = base_world) {
  
  map_data <- base_world +
    ggplot2::geom_point(data=data_frame %>%
                          dplyr::filter(
                            Phoneme==phoneme) %>% 
                          dplyr::distinct(Glottocode,
                                          longitude,
                                          latitude,
                                          Family_name), 
                        ggplot2::aes(x=longitude,
                                     y=latitude,
                                     colour=Family_name,
                                     fill=Family_name),
                        pch=21,
                        size=2,
                        alpha=I(0.7),
                        show.legend = FALSE)
  
  return(map_data)
  
}

#--------------------------/FUNCTION 2/--------------------------#

# Génère une carte montrant la repartition de deux phonème à travers
# le monde. Le cercle vide est utilisé pour montrer la présence 
# d'un premier phonème et la croix pour un deuxième phonème.
# Lorsque les deux phonèmes sont présents dans la langue alors la
# croix est dans le cercle.
#
# ARGUMENTS :
# phoneme1 : phonème d'interet pour l'analyse
# phoneme2 : deuxième phoneme d'interet pour l'analyse
# data_frame : table de données qui doit contenir les colonnes
#              suivantes :
#              - Phoneme
#              - longitude
#              - latitude
#              - Family_name
# 
#
# VALEUR RETOURNEE :
# Un graphique map-monde pour la répartition global deux de
# phonèmes. Utile pour voir une possible corrélation.

two.phonemes_to_map <- function(phoneme1,
                                phoneme2,
                                data_frame = phoible) {
  
  plot <- one.phoneme_to_map(phoneme1)
  
  second.map <- plot +
    ggplot2::geom_point(data=data_frame %>%
                          dplyr::filter(
                            Phoneme==phoneme2) %>% 
                          dplyr::distinct(Glottocode,
                                          longitude,
                                          latitude,
                                          Family_name,
                                          ), 
                        ggplot2::aes(x=longitude,
                                     y=latitude),
                        pch=3,
                        size=1,
                        alpha=I(0.7),
                        show.legend = FALSE)
  
  return(second.map)
  
}

#--------------------------/FUNCTION 3/--------------------------#

# Génère une carte montrant la repartition d'un groupe de phonèmes à 
# travers le monde vs les langues qui n'ont pas ce phonème.
#
# ARGUMENTS :
# Group : vecteur de phonèmes d'interet 
# data_frame : table de données qui doit contenir les colonnes
#              suivantes :
#              - Phoneme
#              - Glottocode
#              - longitude
#              - latitude
#              - Family_name
#              Par défaut phoible
# map : carte vierge, par défaut base_world
#
# VALEUR RETOURNEE :
# Un graphique map-monde pour la répartition globale d'un groupe
# de phoneme

group.phoneme_to_map <- function(Group,
                               data_frame = phoible,
                               map = base_world) {
  

map_groups <- base_world +
          
          ggplot2::geom_point(data= phoible %>%
                                dplyr::filter(
                                  Phoneme %in% Group)  %>% 
                                dplyr::distinct(Glottocode,
                                                longitude,
                                                latitude,
                                                Family_name), 
                              ggplot2::aes(x=longitude,
                                           y=latitude),
                              pch=3,
                              size=2,
                              alpha=I(0.7),
                              show.legend = FALSE) + 
          
          ggplot2::geom_point(data= phoible %>% 
                                dplyr::setdiff(phoible %>% 
                                                 dplyr::filter((
                                                   Phoneme %in% Group)) %>% 
                                                 dplyr::distinct(Glottocode) %>% 
                                                 dplyr::inner_join(phoible,by='Glottocode'),
                                               by='Glottocode')  %>% 
                                dplyr::distinct(Glottocode,
                                                longitude,
                                                latitude,
                                                Family_name),  
                              ggplot2::aes(x=longitude,
                                           y=latitude,
                                           colour=Family_name,
                                           fill=Family_name),
                              pch=4,
                              size=2,
                              alpha=I(0.7),
                              show.legend = FALSE)

  return(map_groups)

}





#--------------------------/FUNCTION 3/--------------------------#






fil_weight_el <- function(el, w) {
  
  new.el <- dplyr::filter(el,weight > w)
  
  return(new.el)
  
}

graph_dir <- function(el,dir=FALSE) {
  
  graph <- igraph::graph.data.frame(el,directed=dir)
  
  return(graph)
  
}


main_graph <- function(el) {
  
  main <- igraph::induced_subgraph(el,
                                   igraph::V(
                                     el)[igraph::components(
                                       el)$membership == which.max(igraph::components(
                                         el)$csize)])
  
  return(main)
  
}

save.graph <- function(graph, file, table.attribute) {
  
  cairo_pdf(file)
  plot(graph,
       #layout=igraph::layout_nicely,
       vertex.shape="none",
       vertex.label.color="black",
       vertex.label.cex=0.3,
       vertex.label = table.attribute[igraph::V(graph)$name,1],
       vertex.size = 2,
       #vertex.size = igraph::degree(allophone_light, mode = "in"),
       #edge.width=log10(igraph::E(main.dir.5)$weight),
       edge.arrow.size=0.10,
       edge.color=colorRampPalette(c("grey","orange","red"))(
         max(igraph::E(
           graph)$weight))[igraph::E(
             graph)$weight],
       edge.curved = 0.2)
  dev.off()  
  
}



#--------------------------/FUNCTION 4/--------------------------#



# heatmap_weight.trans.area <- function(EDGELIST,NAME){
#   
#   df_1 <- EDGELIST %>% 
#     dplyr::filter(macroarea==NAME
#                   & (
#                     stringr::str_detect(source.name,"TRILL") |
#                       stringr::str_detect(target.name,"TRILL"))
#     )%>% 
#     dplyr::select(source,target,weight.trans.area) %>% 
#     dplyr::distinct() %>% 
#     dplyr::group_by(source) %>%
#     #dplyr::filter(source!=target) %>%
#     dplyr::mutate(
#       weight.trans.area = weight.trans.area/sum(
#         weight.trans.area))
#   
#   
#   
#   df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
#   
#   df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
#                      target=colnames(df_2)[col(df_2)],
#                      weight=c(df_2)) %>% 
#     dplyr::left_join(df_1,
#                      by=c("source","target"))
#   
#   
#   p <- ggplot2::ggplot(data=df_3,
#                        ggplot2::aes(x=source,y=target,fill=weight.trans.area))+
#     ggplot2::geom_tile(color = "white")+
#     ggplot2::scale_fill_gradient2(low = "skyblue3",
#                                   high = "red3",midpoint = 0.5,
#                                   na.value = 'gray')  +
#     ggplot2::labs(caption=NAME)
#   
#   return(p)
#   
# }
# heatmap_weight.area <- function(EDGELIST,NAME){
#   
#   df_1 <- EDGELIST %>% 
#     dplyr::filter(macroarea==NAME
#                   & (
#                     stringr::str_detect(source.name,"TRILL") |
#                       stringr::str_detect(target.name,"TRILL"))
#     )%>% 
#     dplyr::select(source,target,weight.area) %>% 
#     dplyr::distinct()
#   
#   
#   
#   df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
#   
#   df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
#                      target=colnames(df_2)[col(df_2)],
#                      weight=c(df_2)) %>% 
#     dplyr::left_join(df_1,
#                      by=c("source","target"))
#   
#   
#   p <- ggplot2::ggplot(data=df_3,
#                        ggplot2::aes(x=source,y=target,fill=weight.area))+
#     ggplot2::geom_tile(color = "white")+
#     ggplot2::scale_fill_gradient2(low = "red3",
#                                   high = "skyblue3",midpoint = 0,
#                                   na.value = 'gray',limits=c(-1,1))  +
#     ggplot2::labs(caption=NAME)
#   
#   return(p)
#   
# }
# heatmap_weight.trans.fam <- function(EDGELIST,NAME){
#   
#   df_1 <- EDGELIST %>% 
#     dplyr::filter(Family_name==NAME
#                   & (
#                     stringr::str_detect(source.name,"TRILL") |
#                       stringr::str_detect(target.name,"TRILL"))
#     )%>% 
#     dplyr::select(source,target,weight.trans.fam) %>% 
#     dplyr::distinct() %>% 
#     dplyr::group_by(source) %>%
#     #dplyr::filter(source!=target) %>%
#     dplyr::mutate(
#       weight.trans.fam = weight.trans.fam/sum(
#         weight.trans.fam))
#   
#   
#   
#   df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
#   
#   df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
#                      target=colnames(df_2)[col(df_2)],
#                      weight=c(df_2)) %>% 
#     dplyr::left_join(df_1,
#                      by=c("source","target"))
#   
#   
#   p <- ggplot2::ggplot(data=df_3,
#                        ggplot2::aes(x=source,y=target,fill=weight.trans.fam))+
#     ggplot2::geom_tile(color = "white")+
#     ggplot2::scale_fill_gradient2(low = "skyblue3",
#                                   high = "red3",midpoint = 0.5,
#                                   na.value = 'gray')  +
#     ggplot2::labs(caption=NAME)
#   
#   return(p)
#   
# }
# heatmap_weight.fam <- function(EDGELIST,NAME){
#   
#   df_1 <- EDGELIST %>% 
#     dplyr::filter(Family_name==NAME
#                   & (
#                     stringr::str_detect(source.name,"TRILL") |
#                       stringr::str_detect(target.name,"TRILL"))
#     )%>% 
#     dplyr::select(source,target,weight.fam) %>% 
#     dplyr::distinct()
#   
#   
#   
#   df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
#   
#   df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
#                      target=colnames(df_2)[col(df_2)],
#                      weight=c(df_2)) %>% 
#     dplyr::left_join(df_1,
#                      by=c("source","target"))
#   
#   
#   p <- ggplot2::ggplot(data=df_3,
#                        ggplot2::aes(x=source,y=target,fill=weight.fam))+
#     ggplot2::geom_tile(color = "white")+
#     ggplot2::scale_fill_gradient2(low = "red3",
#                                   high = "skyblue3",midpoint = 0,
#                                   na.value = 'gray',limits=c(-1,1))  +
#     ggplot2::labs(caption=NAME)
#   
#   return(p)
#   
# }
# 
# create_graph.area <- function(EDGELIST,NAME){
#   df_edgelist <- EDGELIST %>% 
#     dplyr::filter(macroarea==NAME
#                   & (
#                     stringr::str_detect(source.name,"TRILL") |
#                       stringr::str_detect(target.name,"TRILL"))
#     ) %>% 
#     dplyr::select(source,target,weight.area)
#   
#   
#   df_edgelist_1 <- as.matrix(igraph::as_adjacency_matrix(
#     igraph::graph_from_data_frame(df_edgelist)))
#   
#   df_edgelist_2 <- data.frame(source=rownames(df_edgelist_1)[row(df_edgelist_1)],
#                               target=colnames(df_edgelist_1)[col(df_edgelist_1)],
#                               weight=c(df_edgelist_1)) %>% 
#     dplyr::left_join(df_edgelist,
#                      by=c("source","target")) %>% 
#     dplyr::distinct() %>% 
#     dplyr::filter(!is.na(weight.area))
#   
#   
#   GRAPH <- igraph::graph.data.frame(df_edgelist_2)
#   GRAPH <- igraph::simplify(GRAPH, remove.loops = TRUE)
#   
#   return(GRAPH)
#   
# }
# 
# create_graph.fam <- function(EDGELIST,NAME){
#   df_edgelist <- EDGELIST %>% 
#     dplyr::filter(Family_name==NAME
#                   & (
#                     stringr::str_detect(source.name,"TRILL") |
#                       stringr::str_detect(target.name,"TRILL"))
#     ) %>% 
#     dplyr::select(source,target,weight.fam)
#   
#   
#   df_edgelist_1 <- as.matrix(igraph::as_adjacency_matrix(
#     igraph::graph_from_data_frame(df_edgelist)))
#   
#   df_edgelist_2 <- data.frame(source=rownames(df_edgelist_1)[row(df_edgelist_1)],
#                               target=colnames(df_edgelist_1)[col(df_edgelist_1)],
#                               weight=c(df_edgelist_1)) %>% 
#     dplyr::left_join(df_edgelist,
#                      by=c("source","target")) %>% 
#     dplyr::distinct() %>% 
#     dplyr::filter(!is.na(weight.fam))
#   
#   
#   GRAPH <- igraph::graph.data.frame(df_edgelist_2)
#   GRAPH <- igraph::simplify(GRAPH, remove.loops = TRUE)
#   
#   return(GRAPH)
#   
# }





#-------//--------

heatmap_weight.trans.area <- function(EDGELIST,NAME){
  
  df_1 <- EDGELIST %>% 
    dplyr::filter(macroarea==NAME
    )%>% 
    dplyr::select(source,target,weight.trans.area) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(source) %>%
    #dplyr::filter(source!=target) %>%
    dplyr::mutate(
      weight.trans.area = weight.trans.area/sum(
        weight.trans.area))
  
  
  
  df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
  
  df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
                     target=colnames(df_2)[col(df_2)],
                     weight=c(df_2)) %>% 
    dplyr::left_join(df_1,
                     by=c("source","target"))
  
  
  p <- ggplot2::ggplot(data=df_3,
                       ggplot2::aes(x=source,y=target,fill=weight.trans.area))+
    ggplot2::geom_tile(color = "white")+
    ggplot2::scale_fill_gradient2(low = "skyblue3",
                                  high = "red3",midpoint = 0.5,
                                  na.value = 'gray')  +
    ggplot2::labs(caption=NAME,
                  subtitle = "Source")
  
  return(plot(p))
  
}
heatmap_weight.trans.fam <- function(EDGELIST,NAME){
  
  df_1 <- EDGELIST %>% 
    dplyr::filter(Family_name==NAME
    )%>% 
    dplyr::select(source,target,weight.trans.fam) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(source) %>%
    #dplyr::filter(source!=target) %>%
    dplyr::mutate(
      weight.trans.fam = weight.trans.fam/sum(
        weight.trans.fam))
  
  
  
  df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
  
  df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
                     target=colnames(df_2)[col(df_2)],
                     weight=c(df_2)) %>% 
    dplyr::left_join(df_1,
                     by=c("source","target"))
  
  
  p <- ggplot2::ggplot(data=df_3,
                       ggplot2::aes(x=source,y=target,fill=weight.trans.fam))+
    ggplot2::geom_tile(color = "white")+
    ggplot2::scale_fill_gradient2(low = "skyblue3",
                                  high = "red3",midpoint = 0.5,
                                  na.value = 'gray')  +
    ggplot2::labs(caption=NAME,
                  subtitle = "Source")
  
  return(plot(p))
  
}

heatmap_weight.target.area <- function(EDGELIST,NAME){
  
  df_1 <- EDGELIST %>% 
    dplyr::filter(macroarea==NAME
    )%>% 
    dplyr::select(source,target,weight.trans.area) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(target) %>%
    #dplyr::filter(source!=target) %>%
    dplyr::mutate(
      weight.trans.area = weight.trans.area/sum(
        weight.trans.area))
  
  
  
  df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
  
  df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
                     target=colnames(df_2)[col(df_2)],
                     weight=c(df_2)) %>% 
    dplyr::left_join(df_1,
                     by=c("source","target"))
  
  
  p <- ggplot2::ggplot(data=df_3,
                       ggplot2::aes(x=source,y=target,fill=weight.trans.area))+
    ggplot2::geom_tile(color = "white")+
    ggplot2::scale_fill_gradient2(low = "skyblue3",
                                  high = "red3",midpoint = 0.5,
                                  na.value = 'gray')  +
    ggplot2::labs(caption=NAME,
                  subtitle = "Target")
  
  return(plot(p))
  
}
heatmap_weight.target.fam <- function(EDGELIST,NAME){
  
  df_1 <- EDGELIST %>% 
    dplyr::filter(Family_name==NAME
    )%>% 
    dplyr::select(source,target,weight.trans.fam) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(target) %>%
    #dplyr::filter(source!=target) %>%
    dplyr::mutate(
      weight.trans.fam = weight.trans.fam/sum(
        weight.trans.fam))
  
  
  
  df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
  
  df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
                     target=colnames(df_2)[col(df_2)],
                     weight=c(df_2)) %>% 
    dplyr::left_join(df_1,
                     by=c("source","target"))
  
  
  p <- ggplot2::ggplot(data=df_3,
                       ggplot2::aes(x=source,y=target,fill=weight.trans.fam))+
    ggplot2::geom_tile(color = "white")+
    ggplot2::scale_fill_gradient2(low = "skyblue3",
                                  high = "red3",midpoint = 0.5,
                                  na.value = 'gray')  +
    ggplot2::labs(caption=NAME,
                  subtitle = "Target")
  
  return(plot(p))
  
}

heatmap_weight.trans.MANNER.area <- function(EDGELIST,NAME,MANNER){
  
  df_1 <- EDGELIST %>% 
    dplyr::filter(macroarea==NAME
                  & (
                    stringr::str_detect(source.name,MANNER) |
                      stringr::str_detect(target.name,MANNER))
    )%>% 
    dplyr::select(source,target,weight.trans.area) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(source) %>%
    #dplyr::filter(source!=target) %>%
    dplyr::mutate(
      weight.trans.area = weight.trans.area/sum(
        weight.trans.area))
  
  
  
  df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
  
  df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
                     target=colnames(df_2)[col(df_2)],
                     weight=c(df_2)) %>% 
    dplyr::left_join(df_1,
                     by=c("source","target"))
  
  
  p <- ggplot2::ggplot(data=df_3,
                       ggplot2::aes(x=source,y=target,fill=weight.trans.area))+
    ggplot2::geom_tile(color = "white")+
    ggplot2::scale_fill_gradient2(low = "skyblue3",
                                  high = "red3",midpoint = 0.5,
                                  na.value = 'gray')  +
    ggplot2::labs(caption=NAME,
                  subtitle = "Source")
  
  return(plot(p))
  
}
heatmap_weight.target.MANNER.area <- function(EDGELIST,NAME,MANNER){
  
  df_1 <- EDGELIST %>% 
    dplyr::filter(macroarea==NAME
                  & (
                    stringr::str_detect(source.name,MANNER) |
                      stringr::str_detect(target.name,MANNER))
    )%>% 
    dplyr::select(source,target,weight.trans.area) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(target) %>%
    #dplyr::filter(source!=target) %>%
    dplyr::mutate(
      weight.trans.area = weight.trans.area/sum(
        weight.trans.area))
  
  
  
  df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
  
  df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
                     target=colnames(df_2)[col(df_2)],
                     weight=c(df_2)) %>% 
    dplyr::left_join(df_1,
                     by=c("source","target"))
  
  
  p <- ggplot2::ggplot(data=df_3,
                       ggplot2::aes(x=source,y=target,fill=weight.trans.area))+
    ggplot2::geom_tile(color = "white")+
    ggplot2::scale_fill_gradient2(low = "skyblue3",
                                  high = "red3",midpoint = 0.5,
                                  na.value = 'gray')  +
    ggplot2::labs(caption=NAME,
                  subtitle = "Target")
  
  return(plot(p))
  
}
heatmap_weight.MANNER.area <- function(EDGELIST,NAME,MANNER){
  
  df_1 <- EDGELIST %>% 
    dplyr::filter(macroarea==NAME
                  & (
                    stringr::str_detect(source.name,MANNER) |
                      stringr::str_detect(target.name,MANNER))
    )%>% 
    dplyr::select(source,target,weight.area) %>% 
    dplyr::distinct()
  
  
  
  df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
  
  df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
                     target=colnames(df_2)[col(df_2)],
                     weight=c(df_2)) %>% 
    dplyr::left_join(df_1,
                     by=c("source","target"))
  
  
  p <- ggplot2::ggplot(data=df_3,
                       ggplot2::aes(x=source,y=target,fill=weight.area))+
    ggplot2::geom_tile(color = "white")+
    ggplot2::scale_fill_gradient2(low = "red3",
                                  high = "skyblue3",midpoint = 0,
                                  na.value = 'gray',limits=c(-1,1))  +
    ggplot2::labs(caption=NAME,
                  subtitle = "Source/Target")
  
  return(plot(p))
  
}

heatmap_weight.trans.MANNER.fam <- function(EDGELIST,NAME,MANNER){
  
  df_1 <- EDGELIST %>% 
    dplyr::filter(Family_name==NAME
                  & (
                    stringr::str_detect(source.name,MANNER) |
                      stringr::str_detect(target.name,MANNER))
    )%>% 
    dplyr::select(source,target,weight.trans.fam) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(source) %>%
    #dplyr::filter(source!=target) %>%
    dplyr::mutate(
      weight.trans.fam = weight.trans.fam/sum(
        weight.trans.fam))
  
  
  
  df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
  
  df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
                     target=colnames(df_2)[col(df_2)],
                     weight=c(df_2)) %>% 
    dplyr::left_join(df_1,
                     by=c("source","target"))
  
  
  p <- ggplot2::ggplot(data=df_3,
                       ggplot2::aes(x=source,y=target,fill=weight.trans.fam))+
    ggplot2::geom_tile(color = "white")+
    ggplot2::scale_fill_gradient2(low = "skyblue3",
                                  high = "red3",midpoint = 0.5,
                                  na.value = 'gray')  +
    ggplot2::labs(caption=NAME,
                  subtitle = "Source")
  
  return(plot(p))
  
}
heatmap_weight.target.MANNER.fam <- function(EDGELIST,NAME,MANNER){
  
  df_1 <- EDGELIST %>% 
    dplyr::filter(Family_name==NAME
                  & (
                    stringr::str_detect(source.name,MANNER) |
                      stringr::str_detect(target.name,MANNER))
    )%>% 
    dplyr::select(source,target,weight.trans.fam) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(target) %>%
    #dplyr::filter(source!=target) %>%
    dplyr::mutate(
      weight.trans.fam = weight.trans.fam/sum(
        weight.trans.fam))
  
  
  
  df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
  
  df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
                     target=colnames(df_2)[col(df_2)],
                     weight=c(df_2)) %>% 
    dplyr::left_join(df_1,
                     by=c("source","target"))
  
  
  p <- ggplot2::ggplot(data=df_3,
                       ggplot2::aes(x=source,y=target,fill=weight.trans.fam))+
    ggplot2::geom_tile(color = "white")+
    ggplot2::scale_fill_gradient2(low = "skyblue3",
                                  high = "red3",midpoint = 0.5,
                                  na.value = 'gray')  +
    ggplot2::labs(caption=NAME,
                  subtitle = "Target")
  
  return(plot(p))
  
}
heatmap_weight.MANNER.fam <- function(EDGELIST,NAME,MANNER){
  
  df_1 <- EDGELIST %>% 
    dplyr::filter(Family_name==NAME
                  & (
                    stringr::str_detect(source.name,MANNER) |
                      stringr::str_detect(target.name,MANNER))
    )%>% 
    dplyr::select(source,target,weight.fam) %>% 
    dplyr::distinct()
  
  
  
  df_2 <- as.matrix(igraph::as_adjacency_matrix(igraph::graph_from_data_frame(df_1)))
  
  df_3 <- data.frame(source=rownames(df_2)[row(df_2)],
                     target=colnames(df_2)[col(df_2)],
                     weight=c(df_2)) %>% 
    dplyr::left_join(df_1,
                     by=c("source","target"))
  
  
  p <- ggplot2::ggplot(data=df_3,
                       ggplot2::aes(x=source,y=target,fill=weight.fam))+
    ggplot2::geom_tile(color = "white")+
    ggplot2::scale_fill_gradient2(low = "red3",
                                  high = "skyblue3",midpoint = 0,
                                  na.value = 'gray',limits=c(-1,1))  +
    ggplot2::labs(caption=NAME,
                  subtitle = "Source/Target")
  
  return(plot(p))
  
}

create_graph.MANNER.area <- function(EDGELIST,NAME,MANNER){
    df_edgelist <- EDGELIST %>% 
      dplyr::filter(macroarea==NAME
                    & (
                      stringr::str_detect(source.name,MANNER) |
                        stringr::str_detect(target.name,MANNER))
      ) %>% 
      dplyr::select(source,target,weight.area)
    
    
    df_edgelist_1 <- as.matrix(igraph::as_adjacency_matrix(
      igraph::graph_from_data_frame(df_edgelist)))
    
    df_edgelist_2 <- data.frame(source=rownames(df_edgelist_1)[row(df_edgelist_1)],
                                target=colnames(df_edgelist_1)[col(df_edgelist_1)],
                                weight=c(df_edgelist_1)) %>% 
      dplyr::left_join(df_edgelist,
                       by=c("source","target")) %>% 
      dplyr::distinct() %>% 
      dplyr::filter(!is.na(weight.area))
    
    
    GRAPH <- igraph::graph.data.frame(df_edgelist_2)
    GRAPH <- igraph::simplify(GRAPH, remove.loops = TRUE)
    
    return(GRAPH)
    
  }
create_graph.MANNER.fam <- function(EDGELIST,NAME,MANNER){
  df_edgelist <- EDGELIST %>% 
    dplyr::filter(Family_name==NAME
                  & (
                    stringr::str_detect(source.name,MANNER) |
                      stringr::str_detect(target.name,MANNER))
    ) %>% 
    dplyr::select(source,target,weight.fam)
  
  
  df_edgelist_1 <- as.matrix(igraph::as_adjacency_matrix(
    igraph::graph_from_data_frame(df_edgelist)))
  
  df_edgelist_2 <- data.frame(source=rownames(df_edgelist_1)[row(df_edgelist_1)],
                              target=colnames(df_edgelist_1)[col(df_edgelist_1)],
                              weight=c(df_edgelist_1)) %>% 
    dplyr::left_join(df_edgelist,
                     by=c("source","target")) %>% 
    dplyr::distinct() %>% 
    dplyr::filter(!is.na(weight.fam))
  
  
  GRAPH <- igraph::graph.data.frame(df_edgelist_2)
  GRAPH <- igraph::simplify(GRAPH, remove.loops = TRUE)
  
  return(GRAPH)
  
}

colors_graph.area <- function(GRAPH,EDGELIST,NAME){
  
  df_edgelist <- EDGELIST %>% 
    dplyr::filter(macroarea==NAME) %>%  
    dplyr::select(source,target,weight.area)
  
  max_min <- df_edgelist %>% 
    dplyr::summarise(max = max(weight.area),
                     min = min(weight.area))
  
  
  scale_range <- c(max_min$min, max_min$max)
  pal <- leaflet::colorNumeric("RdBu", domain = scale_range)
  
  palette <- pal((dplyr::as_data_frame(igraph::as_edgelist(
    GRAPH)) %>% 
      dplyr::rename(source = V1, target = V2) %>%
      dplyr::bind_cols(dplyr::as_data_frame(
        igraph::E(GRAPH)$weight)) %>%
      dplyr::rename(weight=value) %>% 
      dplyr::left_join(df_edgelist %>%
                         dplyr::select(source,target,weight.area) %>%
                         dplyr::distinct(),
                       by=c("source","target")
      ) %>% 
      dplyr::select(weight.area) %>%
      dplyr::rename(weight = weight.area))$weight)
  
  return(palette)
}
colors_graph.fam <- function(GRAPH,EDGELIST,NAME){
  
  df_edgelist <- EDGELIST %>% 
    dplyr::filter(Family_name==NAME) %>%  
    dplyr::select(source,target,weight.fam)
  
  max_min <- df_edgelist %>% 
    dplyr::summarise(max = max(weight.fam),
                     min = min(weight.fam))
  
  
  scale_range <- c(max_min$min, max_min$max)
  pal <- leaflet::colorNumeric("RdBu", domain = scale_range)
  
  palette <- pal((dplyr::as_data_frame(igraph::as_edgelist(
    GRAPH)) %>% 
      dplyr::rename(source = V1, target = V2) %>%
      dplyr::bind_cols(dplyr::as_data_frame(
        igraph::E(GRAPH)$weight)) %>%
      dplyr::rename(weight=value) %>% 
      dplyr::left_join(df_edgelist %>%
                         dplyr::select(source,target,weight.fam) %>%
                         dplyr::distinct(),
                       by=c("source","target")
      ) %>% 
      dplyr::select(weight.fam) %>%
      dplyr::rename(weight = weight.fam))$weight)
  
  return(palette)
}

generate_graph.area <- function(GRAPH,EDGELIST,NAME){
  plot(GRAPH,
       vertex.shape="none",
       vertex.label.color="black",
       vertex.label.cex=0.3,
       vertex.size = 2,
       edge.arrow.size=0.20,
       edge.curved = 0.2,
       edge.color = colors_graph.area(GRAPH,EDGELIST,NAME),
       main=NAME
  )
}
generate_graph.fam <- function(GRAPH,EDGELIST,NAME){
  plot(GRAPH,
       vertex.shape="none",
       vertex.label.color="black",
       vertex.label.cex=0.3,
       vertex.size = 2,
       edge.arrow.size=0.20,
       edge.curved = 0.2,
       edge.color = colors_graph.fam(GRAPH,EDGELIST,NAME),
       main=NAME
  )
}

permutation_offdiag.AREA <- function(AREA,MANNER){
  N_SAMPLES <- 1000
  statistics <- rep(0, N_SAMPLES)
  for (i in 1:N_SAMPLES) {
    
    fake <- Edgelist %>% 
      dplyr::filter(macroarea==AREA & (
        stringr::str_detect(source.name,MANNER) |
          stringr::str_detect(target.name,MANNER))
      ) %>%
      dplyr::select(macroarea,source.name,target.name) %>% 
      dplyr::distinct()
    
    
    fake$target.name <- sample(fake$target.name)
    
    fake_graph <- fake %>% 
      dplyr::select(source.name,target.name) %>% 
      graph_dir(TRUE)
    
    statistics[i] <- offDiag_compl(fake_graph)
  }
  return(list(statistics))
}
permutation_offdiag.FAM <- function(FAM,MANNER){
  N_SAMPLES <- 1000
  statistics <- rep(0, N_SAMPLES)
  for (i in 1:N_SAMPLES) {
    
    fake <- Edgelist %>% 
      dplyr::filter(Family_name==FAM & (
        stringr::str_detect(source.name,MANNER) |
          stringr::str_detect(target.name,MANNER))
      ) %>%
      dplyr::select(Family_name,source.name,target.name) %>% 
      dplyr::distinct()
    
    
    fake$target.name <- sample(fake$target.name)
    
    fake_graph <- fake %>% 
      dplyr::select(source.name,target.name) %>% 
      graph_dir(TRUE)
    
    statistics[i] <- offDiag_compl(fake_graph)
  }
  return(list(statistics))
}

#------------//------------

plot_hist <- function(DF,NAME,BIN=50){
  
  df <- DF %>% 
    dplyr::filter(AREA == NAME |
                    FAMILY == NAME)
  
  hist(DF$PermTest[df$ID][[1]][[1]],breaks=BIN,main=NAME,xlab="Offdiagonal complexity")
  abline(v = DF$OffDial[df$ID], col="red")
  
  
}

