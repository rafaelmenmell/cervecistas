#funciones basicas

library(readxl)
library(plotly)

leer_dataset <- function(){
  beer_data <- read_excel(path = "dataset-datathon.xlsx")
  beer_data$id <- 1:nrow(beer_data)
  return(beer_data)
}

crear_brujula <- function(idb,beer_data=leer_dataset()){
  columnas_brujula <- c("color","maltoso","licoroso","afrutado","especias","acidez","lupulo_afrutado_citrico","lupulo_floral_herbal","amargor")
  datos <- as.numeric(beer_data[beer_data$id==idb,columnas_brujula])
  datos[is.na(datos)] <- 0
  p <- plot_ly(
    type="scatterpolar",
    mode="lines+markers",
    r = c(datos,datos[1]),
    theta = c(columnas_brujula,"color"),
    fill = 'none',
    opacity=0.8,
    line = list(
      color = "#A68041",
      width=5
    ),
    marker = list(
      opacity=0
    ),
    hoverinfo = "r",
    connectgaps=TRUE
  ) %>% layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,5),
        ticklen = 0,
        showticklabels=FALSE,
        layer="below traces",
        showline=FALSE,
        color="#27292C"
      ),
      angularaxis = list(
        color="black",
        linewidth=1,
        gridcolor="black",
        ticklen=0,
        direction="clockwise",
        rotation="0",
        layer="below traces"
      )
    ),
    showlegend = F
  )

  return(p)
}

crear_clusters_v1 <- function(Ncl=5){
  beer_data <- leer_dataset()
  beer_data_matrix <- as.matrix(beer_data[,6:15])
  beer_data_matrix[is.na(beer_data_matrix)] <- 0
  d <- dist(beer_data_matrix[,-10],method="euclidean")
  fit <- hclust(d,method="ward.D")
  groups <- cutree(fit,k = Ncl)
  beer_data$cl <- groups
  beer_data[,6:15] <- beer_data_matrix
  beer_data_medias <- beer_data[,6:16]
  beer_data_medias <- split(beer_data_medias,beer_data$cl)
  beer_data_medias <- as.data.frame(t(sapply(beer_data_medias, function(x) colMeans(x))))
  # beer_data_medias$id <- 10000 + 1:Ncl
  # beer_data_medias$cl <- NULL
  # beer_data_cl <- bind_rows(beer_data,beer_data_medias)
  return(list(cluster=beer_data_medias,groups=groups))
}

crear_brujula_con_rumbo <- function(idb,Ncl=5){
  beer_data <- leer_dataset()
  beer_data_cl <- crear_clusters_v1(Ncl = Ncl)
  cl <- beer_data_cl$groups[idb]
  columnas_brujula <- c("color","maltoso","licoroso","afrutado","especias","acidez","lupulo_afrutado_citrico","lupulo_floral_herbal","amargor")
  datos <- as.numeric(beer_data[beer_data$id==idb,columnas_brujula])
  datos[is.na(datos)] <- 0
  datos2 <- as.numeric(beer_data_cl$cluster[beer_data_cl$cluster$cl==cl,columnas_brujula])
  p <- plot_ly(
    type="scatterpolar"
  ) %>% add_trace(
    mode="lines+markers",
    r = c(datos2,datos2[1]),
    theta = c(columnas_brujula,"color"),
    fill = 'toself',
    fillcolor = "#F81818",
    opacity=0.6,
    line = list(
      color = "#F81818",
      width=5
    ),
    marker = list(
      opacity=0
    ),
    hoverinfo = "r",
    connectgaps=TRUE
  ) %>% add_trace(
    mode="lines+markers",
    r = c(datos,datos[1]),
    theta = c(columnas_brujula,"color"),
    fill = 'none',
    opacity=0.8,
    line = list(
      color = "#A68041",
      width=5
    ),
    marker = list(
      opacity=0
    ),
    hoverinfo = "r",
    connectgaps=TRUE
  )  %>% layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,5),
        ticklen = 0,
        showticklabels=FALSE,
        layer="below traces",
        showline=FALSE,
        color="#27292C"
      ),
      angularaxis = list(
        color="black",
        linewidth=1,
        gridcolor="black",
        ticklen=0,
        direction="clockwise",
        rotation="0",
        layer="below traces"
      )
    ),
    showlegend = F
  )
  
  return(p)
}

crear_mapa_rumbos <- function(Ncl=5){
  rumbos <- crear_clusters_v1(Ncl = 5)
  grafico_rumbos <- vector("list",Ncl)
  columnas_brujula <- c("color","maltoso","licoroso","afrutado","especias","acidez","lupulo_afrutado_citrico","lupulo_floral_herbal","amargor")
  for (n in 1:Ncl){
    datos2 <- as.numeric(rumbos$cluster[rumbos$cluster$cl==n,columnas_brujula])
    p <- plot_ly(
      type="scatterpolar"
    ) %>% add_trace(
      mode="lines+markers",
      r = c(datos2,datos2[1]),
      theta = c(columnas_brujula,"color"),
      fill = 'toself',
      fillcolor = "#F81818",
      opacity=0.6,
      line = list(
        color = "#F81818",
        width=5
      ),
      marker = list(
        opacity=0
      ),
      hoverinfo = "r",
      connectgaps=TRUE
    )  %>% layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,5),
          ticklen = 0,
          showticklabels=FALSE,
          layer="below traces",
          showline=FALSE,
          color="#27292C"
        ),
        angularaxis = list(
          color="black",
          linewidth=1,
          gridcolor="black",
          ticklen=0,
          direction="clockwise",
          rotation="0",
          layer="below traces"
        )
      ),
      showlegend = F
    )
    grafico_rumbos[[n]] <- p 
  }
  return(grafico_rumbos)
}