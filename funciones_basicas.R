#funciones basicas

library(readxl)
library(plotly)

leer_dataset <- function(){
  beer_data <- read_excel(path = "dataset-datathon.xlsx")
  beer_data$id <- 1:nrow(beer_data)
  return(beer_data)
}

crear_brujula <- function(idb,beer_data){
  columnas_brujula <- c("color","maltoso","licoroso","afrutado","especias","acidez","lupulo_afrutado_citrico","lupulo_floral_herbal","amargor")
  datos <- as.numeric(beer_data[beer_data$id==idb,columnas_brujula])
  # datos[is.na(datos)] <- 0
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