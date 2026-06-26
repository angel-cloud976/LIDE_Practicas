
setwd("C:/Users/angel/OneDrive/Documentos/Escritorio/graficos-el-quantificador")
source("C:/Users/angel/OneDrive/Documentos/Escritorio/graficos-el-quantificador/scripts/utils.R")
source("C:/Users/angel/OneDrive/Documentos/Escritorio/graficos-el-quantificador/scripts/packages.R")

ensure_packages(c("dplyr", "ggplot2", "stringr", "ggtext", "ragg", "readxl", "scales", "tidyr", "magick", "cowplot"))


base <- read_xlsx("C:/Users/angel/OneDrive/Documentos/Escritorio/Pre-profesional/Grafica de linea de tiempo/COL_completo_con_arancel.xlsx")

datos3  <- read_xlsx("C:/Users/angel/OneDrive/Documentos/Escritorio/Pre-profesional/Grafica de linea de tiempo/lista-de-ecuador.xlsx")


base <- base %>%
  mutate(
    año = str_extract(Período, "^\\d{4}"),
    mes = str_extract(Período, "(?<=/)\\s*\\d{1,2}") |> str_trim(),
    fecha_date = as.Date(paste(año, mes, "01", sep = "-"))
  )
base <- base %>%
  filter(mes %in% c("01", "02", "03"))

base <- base %>%
  left_join(datos3, by = "codigo_subpartida" )


otro <- base %>%
  filter(!is.na(Arancel)) %>%
  arrange(desc(Arancel)) %>%
  filter(mes %in% c("03"))%>%
  slice_head(n = 5)

otro$Arancel <- otro$Arancel*100

grafico <- otro %>%
  select(Descripción, Arancel, Arancel_Base) %>%
  pivot_longer(
    cols = c(Arancel, Arancel_Base),
    names_to = "Tipo",
    values_to = "Arancel"
  )

build_chart <- function(orientation) {
  
  spec <- house_spec(orientation)
  
  if (orientation == "landscape") {
    
    title_txt <- stringr::str_wrap(
      "Los mayores incrementos arancelarios aplicados por Ecuador",
      width = 100
    )
    
    subtitle_txt <- stringr::str_wrap(
      "Comparación entre el arancel anterior y el de Noboa para los cinco productos con mayor diferencia",
      width = 109
    )
    
    caption_txt <- stringr::str_wrap(
      paste(
        "Fuente: COMEX, SENAE y Arancel Nacional Integrado.",
        "Elaboración: Ángel Alava para El Quantificador de Laboratorio LIDE.",
        "Nota: Los aranceles se basaron en la publicación del SENAE 'Arancel-del-Ecuador-Seccion-XVI'.",
        "Los aranceles impuestos por Noboa son del mes de mayo antes de eliminarlos y fueron obtenidos de un artículo de Redacción Primicias.",
        sep = "\n"
      ),
      width = 160
    )
    
  } else {
    
    title_txt <- paste(
      "Los mayores incrementos",
      "arancelarios aplicados por Ecuador",
      sep = " "
    )
    
    subtitle_txt <- paste(
      "Comparación entre el arancel anterior y el de Noboa",
      "para los cinco productos con mayor diferencia",
      sep = " "
    )
    
    caption_txt <- stringr::str_wrap(
      paste(
        "Fuente: COMEX, SENAE y Arancel Nacional Integrado.",
        "Elaboración: Ángel Alava para El Quantificador.",
        "Nota: Los aranceles se basaron en la publicación del SENAE 'Arancel-del-Ecuador-Seccion-XVI'. Los aranceles impuestos por Noboa son del mes de mayo antes de eliminarlos y fueron obtenidos de un artículo de Redacción Primicias.",
        sep = " "
      )
    )
  }
  
  ggplot(grafico,
         aes(x = Descripción,
             y = Arancel,
             fill = Tipo)) +
    geom_col(position = position_dodge(width = 0.5), width = 0.5) +
    
    geom_text(
    data = filter(grafico, Tipo == "Arancel_Base"),
    aes(label = paste0(Arancel, "%")),
    hjust = -0.1,  
    vjust = -0.4,  
    color = "black", size = 3
  ) +
    
    geom_text(
      data = filter(grafico, Tipo == "Arancel"),
      aes(label = paste0(Arancel, "%")),
      hjust = -0.1,  
      vjust = 1.5, 
      color = "black", size = 3
    )  +
    scale_y_continuous(limits = c(0, 88), expand = c(0, 0)) + 
    coord_flip() +
    scale_fill_manual(
      values = c("steelblue", "orange"),
      labels = c(
        "Arancel" = "Arancel de Noboa",
        "Arancel_Base" = "Arancel anterior"
      )
    ) +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      caption = caption_txt,
      x = "Producto",
      y = "Porcentaje de arancel"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold",hjust = 0, size = 16),
      plot.subtitle = element_text(hjust = 0),
      plot.caption = element_text( size = 6,hjust = 0)

    ) +
    scale_x_discrete(labels = c(
      "Azúcar y melaza caramelizados" = "Azúcar y melaza",
      "Extractos, esencias y concentrados" = "Extractos de café",
      "Preparaciones a base de extractos, esencias o concentrados o a base de café" = "Preparaciones\na base de café",
      "Que contengan como ingrediente principal uno\r\no más extractos vegetales, partes de plantas, semillas\r\no frutos, incluidas las mezclas entre sí." = "Extractos y preparaciones\nvegetales",
      "Agua, incluidas el agua mineral y la gaseada, con adición de azúcar u otro edulcorante o aromatizada" = "Agua"
    )) +
    theme_minimal(base_size = 9.5) +
    
    theme(
      plot.caption.position = "plot",
      plot.title.position = "plot",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "#CCCCCC", size = 0.5),
      axis.ticks = element_line(color = "#777777", size = 0.6),
      axis.ticks.length = unit(0.15, "cm"),  
      axis.title.y = element_blank(),
      plot.title = ggtext::element_textbox_simple(
        face = "bold",
        size = 13, 
        margin = margin(b = 4)
      ),
      
      plot.subtitle = ggtext::element_textbox_simple(
        size = 10,
        color = "gray30",
        margin = margin(b = 10)
      ),
      
      plot.caption = ggtext::element_textbox_simple(size = 6.5,
        hjust = 0, 
        width = grid::unit(1, "npc"),
      margin = margin(6, 36, 6, 16)
      ),
    
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.box = "horizontal",
      legend.title = element_text(size = 7), 
      legend.text = element_text(size = 6.5),
      legend.margin = margin(t = 0, r = 0, b = 0, l = -80),
    )
}

p_landscape <- build_chart("landscape")
p_portrait  <- build_chart("portrait")


out_path <- ("C:/Users/angel/OneDrive/Documentos/Escritorio/Pre-profesional/Imagenes de graficos/Tasa_arancel_2017_vs_2026")

for (orientation in c("portrait", "landscape")) {
  spec <- house_spec(orientation)
  p_final <- house_apply_logo(build_chart(orientation), orientation, x = 0.89, y = 0.05)
  dest <- paste0(house_out_path(out_path, orientation), ".png")

ggsave(
    filename = dest,
    plot = p_final,
    width = 4,   
    height = 5,  
    units = "in",
    dpi = 300,            
    device = ragg::agg_png,
    bg = "white"
  )
  message("Guardado: ", dest)
}
print(dest)
