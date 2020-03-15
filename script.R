library(tidyverse)
library(leaflet)

data <- read_csv('./data.csv')

getColor <- function(data) {
  map_chr(data$category, function(category) {
    case_when(
      category == 'カフェ' ~ 'cadetblue',
      category == 'うどん・そば' ~ 'darkgreen',
      category == '中華料理' ~ 'blue',
      category == 'ラーメン' ~ 'darkgreen',
      category == 'カレー' ~ 'darkgreen',
      category == '和食' ~ 'darkgreen',
      category == 'イタリア料理' ~ 'blue',
      category == '焼肉・ステーキ' ~ 'cadetblue',
      category == 'その他' ~ 'green',
      category == 'インド料理' ~ 'blue',
      category == 'アジア料理' ~ 'blue',
      category == 'ハンバーガー' ~ 'cadetblue',
      category == 'ファストフード' ~ 'cadetblue'
    )
  })
}

getIcon <- function(data) {
  map_chr(data$category, function(category) {
    case_when(
      category == 'カフェ' ~ 'coffee',
      category == 'うどん・そば' ~ 'cutlery',
      category == '中華料理' ~ 'fire',
      category == 'ラーメン' ~ 'cutlery',
      category == 'カレー' ~ 'fire',
      category == '和食' ~ 'cutlery',
      category == 'イタリア料理' ~ 'cutlery',
      category == '焼肉・ステーキ' ~ 'cutlery',
      category == 'その他' ~ 'dot-circle-o',
      category == 'インド料理' ~ 'fire',
      category == 'アジア料理' ~ 'globe',
      category == 'ハンバーガー' ~ 'cutlery',
      category == 'ファストフード' ~ 'cutlery'
    )
  })
}

icons <- awesomeIcons(
  icon = getIcon(data),
  iconColor = '#FFFFFF',
  library = 'fa',
  markerColor = getColor(data)
)

leaflet(data) %>% 
  addTiles() %>% 
  setView(lng = 139.723, lat = 35.626, zoom = 15) %>%
  addAwesomeMarkers(lng = ~lng,
             lat = ~lat,
             icon=icons,
             popup = sprintf("<span style='font-weight: bold;'>%s</span>（%s）<br />%s", data$name, data$category, data$address)) %>%
  addMarkers(lng = 139.721629, lat = 35.626823, popup = '<span style="font-weight: bold;">freee株式会社</span>')
