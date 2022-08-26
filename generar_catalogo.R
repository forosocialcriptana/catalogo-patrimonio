# Script para crear crear el catálogo de patrimonio de Campo de Criptana
# Author: Alfredo Sánchez Alberca (asalber@gmail.com)

library(googlesheets4)
library(rmarkdown)
library(yaml)
library(dplyr)
library(googledrive)

# URL de la hoja de cálculo con los datos
url.df <- "https://docs.google.com/spreadsheets/d/1hWyDiPwVU1oUflqVSC6iSveP-NigMpZhh9e7RKXZpJk/edit?usp=sharing"

# Carga de los datos
df <- read_sheet(url.df)
df <- df %>% arrange(Título)

#' Title
#' Función que crea un fichero con el contenido de una ficha de un lugar en formato markdown. 
#' El nombre del fichero en formato Rmarkdown se toma del segundo campo que se supone es el título de ficha.
#' @param item Vector con los campos de la ficha.
#'
#' @return None
#' @export
#'
#' @examples
render.record <- function(item){
  # Primero eliminar tildes, espacios y pasar a minuscula
  name <- gsub(" ", "-", tolower(iconv(item$Título, to='ASCII//TRANSLIT')))
  name <- gsub("/", "-", name)
  file.name <- paste("content/lugares/", name, ".md", sep="")
  file.create(file.name)
  # Descargar fotos
  url.photos <- trimws(unlist(strsplit(gsub("open\\?", "uc?export=download&", item$Fotos), ",")))
  photos <- NULL
  if (length(url.photos)>0) {
    for (i in 1:length(url.photos)) {
      photos[i] <- paste("img/", name, "-", i, ".jpg", sep="")
      #drive_download(url.photos[i], path=paste0("/static/", photos[i]), overwrite = T)
    }
    banner = photos[1]
  } else {
    banner="img/fondo-azul.png"
  }
  yamlheader <- as.yaml(list(title=as.character(item$Título), date=as.character(item$`Marca temporal`[[1]]), description=paste0(item$`Descripción breve`), categories=as.character(item$`Tipo de valor`), featured_image = paste("/", banner, sep="")))
  write(paste("---\n", yamlheader,"---\n\n", sep=""), file=file.name, append=T)
  write(unlist(item$`Descripción completa`), file=file.name, append=T)
  if (length(photos)>0) {
    write('\n<div id="myCarousel" class="carousel slide" df-ride="carousel">
  <!-- Indicators -->
  <ol class="carousel-indicators">
    <li df-target="#myCarousel" df-slide-to="0" class="active"></li>',
    file=file.name, append=T)
    for (i in seq_along(photos[-1])) {
      write(paste('    <li df-target="#myCarousel" df-slide-to="', i, '"></li>', sep=""), file = file.name, append = T)
    }
    write('  </ol>
  <!-- Wrapper for slides -->
  <div class="carousel-inner" role="listbox">
    <div class="item active">',
    file = file.name, append=T)
    write(paste('      <img src="/', photos[1], '" alt="', item$Título, '">', sep=""), file = file.name, append = T)
    write('    </div>', file= file.name, append = T)
    for (i in photos[-1]) {
      write('    <div class="item">', file= file.name, append = T)
      write(paste('      <img src="/', i, '" alt="', item$Título, '">', sep=""), file = file.name, append = T)
      write('    </div>', file= file.name, append = T)    
    }
    write('  <!-- Left and right controls -->
  <a class="left carousel-control" href="#myCarousel" role="button" df-slide="prev">
    <span class="glyphicon glyphicon-chevron-left" aria-hidden="true"></span>
    <span class="sr-only">Previous</span>
  </a>
  <a class="right carousel-control" href="#myCarousel" role="button" df-slide="next">
    <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>
    <span class="sr-only">Next</span>
  </a>
</div>', file = file.name, append=T)
  }
  return(paste0(name, ".md"))
}


#' Función que genera todas las fichas de los lugares en formato markdown.
#'
#' @param df df frame con los registros de las fichas de los lugares.
#'
#' @return None
#' @export
#'
#' @examples
render.all.records <- function(df){
  # Generar el índice
  file.name = "content/lugares/index.md"
  file.create(file.name)
  yamlheader <- "---
layout: page
title: Índice de lugares
header-img: /img/sierra-de-los-molinos-1.jpg
---\n\n"
  write(yamlheader, file=file.name, append=T)
  write('## Índice por categorías\n
<i class="fa fa-tag"></i> &nbsp;<a href="{{ site.github.url }}/lugares/categorias/index.html#ambiental">Ambiental</a> &nbsp;&nbsp;&nbsp;
<i class="fa fa-tag"></i> &nbsp;<a href="{{ site.github.url }}/lugares/categorias/index.html#artístico">Artístico</a> &nbsp;&nbsp;&nbsp;
<i class="fa fa-tag"></i> &nbsp;<a href="{{ site.github.url }}/lugares/categorias/index.html#etnográfico">Etnográfico</a> &nbsp;&nbsp;&nbsp;
<i class="fa fa-tag"></i> &nbsp;<a href="{{ site.github.url }}/lugares/categorias/index.html#histórico">Histórico</a> &nbsp;&nbsp;&nbsp;

## Índice alfabético\n', file=file.name, append=T)
  write(unlist(lapply(df[,2], function(x) paste("- [", x, "](", gsub(" ", "-", tolower(iconv(x, to='ASCII//TRANSLIT'))), "/index.html)", sep=""))), file=file.name, append=T)
  
  # Generar estadísticas
  
  write("\n## Estadísticas de lugares\n", file = file.name, append = T)
  write(paste("Número de lugares catalogados: <b>", nrow(df), "</b>\n", sep=""), file = file.name, append = T)
  write('<img src="estadisticas.png" alt="Estadística de lugares catalogados">', file= file.name, append = T)
  table <- table(df$Tipo.de.valor)
  labels <- paste(names(table), "\n", table, sep="")
  png("static/img/estadisticas.png")
  pie(table, labels = labels, main="Lugares catalogados por categorías", col=c("#00FF00FF", "#FFFF00FF", "#FF0000FF", "#0000FFFF"))
  dev.off()
  # Generar las fichas
  lapply(1:nrow(df), function(i) render.record(df[i,]))
}

render.all.records(df)
