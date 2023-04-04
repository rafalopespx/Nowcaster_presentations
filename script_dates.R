## Cleaning
rm(list = ls())
gc()

## Loading packs
packs<-c("tidyverse", "vroom", "ggplot2", "gganimate")

lapply(packs, require, character.only = TRUE)

## Loading the data

path<-"~/Desktop/central_covid/sumario_SIVEP/"

data_cases<-list.files(path, 
                       full.names = T)

## Taking out first two positions, are README and LICENSE
data_cases<-data_cases[-c(1,2)]

extract_col<-function(x, col.name){
  ## extract dates
  name_x<-stringr::str_extract(string = x, 
                      pattern = "\\d{4}_\\d{2}_\\d{2}")
  ## Reading .csv.xz
  x<-vroom::vroom(x)
  
  ## Summarizing cases over the column selected via col.names
  y<-x |>
    dplyr::select(dt_evento, covid) |> 
    dplyr::mutate(dt_notific = as.Date(name_x, "%Y_%m_%d")) |> 
    dplyr::group_by(dt_evento, dt_notific) |> 
    dplyr::summarise(n = sum({{ col.name }}))
  
  ## Removing objetcts to lightweight the process
  rm(x)
  gc()
  
  ## Returning y
  return(y)
  
}

## lapplying
data_covid<-lapply(data_cases, extract_col, covid)


## Binding rows
data_covid<-data_covid |> 
  bind_rows()

## ggplot animate
dates_succession<-data_covid |> 
  filter(dt_evento >= "2020-01-01") |> 
  ggplot(aes(x = dt_evento, y = n, col = as.factor(dt_notific))) +
  geom_line()+
  scale_x_date(date_labels = "%d/%b", 
               date_breaks = "4 months") +
  scale_color_viridis_d(direction = 1,
                        aesthetics = "colour",
                        option = "turbo") +
  labs(x = "Dates of onset of symptoms", y = "Number of Cases", 
       colour = "Dates of database", 
       # title = "Evolution on number of cases", 
       # subtitle = "Observatório COVID19 BR; covid19br.github.io",
       caption = paste0("Source: SIVEP-Gripe surveillance system")) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 22),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(size = 16, angle = 90),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"))+
  transition_states(factor(dt_notific))+
  shadow_mark(colour = "lightgrey",
              alpha = 0.5,
              size = .7,
              exclude_layer = c(2, 3))

dates_succession

animate(dates_succession,
        duration = 30,
        end_pause = 5,
        width = 16,
        height = 9)
