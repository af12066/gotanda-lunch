library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)

data <- read_csv('./data.csv')

# カテゴリとマーカのカラーの対応
# ref. https://github.com/lvoogdt/Leaflet.awesome-markers#properties
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

# カテゴリとIconの対応
# ref. https://github.com/lvoogdt/Leaflet.awesome-markers#properties
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

icons <- function(data) {
  awesomeIcons(
    icon = getIcon(data),
    iconColor = '#FFFFFF',
    library = 'fa',
    markerColor = getColor(data)
  )
}

# addAwesomeMarkersをカテゴリ名で絞る
genMarkers <- function(map, categoryName) {
  addAwesomeMarkers(
    data = dplyr::filter(.data=data, category == categoryName),
    lng = ~lng,
    lat = ~lat,
    icon = icons(dplyr::filter(.data=data, category == categoryName)),
    group = categoryName,
    popup = sprintf(
      "<span style='font-weight: bold;'>%s</span>（%s）<br />%s<br />%s",
      dplyr::filter(.data = data, category == categoryName)$name,
      dplyr::filter(.data = data, category == categoryName)$category,
      dplyr::filter(.data = data, category == categoryName)$address,
      ifelse(dplyr::filter(.data = data, category == categoryName)$closed == TRUE, '閉店済み', '')
    ),
    map
  )
}

# マップ上に店舗をプロット
ll <- leaflet(data) %>% 
  addTiles() %>% 
  setView(lng = 139.723, lat = 35.626, zoom = 15) %>%
  genMarkers(categoryName = 'カフェ') %>%
  genMarkers(categoryName = 'うどん・そば') %>%
  genMarkers(categoryName = '中華料理') %>%
  genMarkers(categoryName = 'ラーメン') %>%
  genMarkers(categoryName = 'カレー') %>%
  genMarkers(categoryName = '和食') %>%
  genMarkers(categoryName = 'イタリア料理') %>%
  genMarkers(categoryName = '焼肉・ステーキ') %>%
  genMarkers(categoryName = 'その他') %>%
  genMarkers(categoryName = 'インド料理') %>%
  genMarkers(categoryName = 'アジア料理') %>%
  genMarkers(categoryName = 'ハンバーガー') %>%
  genMarkers(categoryName = 'ファストフード') %>%
  addMarkers(lng = 139.721629, lat = 35.626823, popup = '<span style="font-weight: bold;">freee株式会社</span>') %>%
  addLayersControl(
    overlayGroups = data %>% dplyr::select(category) %>% distinct() %>% pull(),
    options = layersControlOptions(collapsed = FALSE)
  )

saveWidget(ll, file="index.html", title = "五反田ランチスポット")

gg <- data %>%
  count(category) %>%
  ggplot(aes(x = reorder(category, -n), y = n)) + geom_bar(stat = 'identity') +
  geom_text(aes(label = n), vjust = 1.1, color = "white") +
  labs(x = 'カテゴリ', y = '店舗数') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = 'Hiragino Kaku Gothic Pro W3')
       )
ggsave('bar.png', gg, dpi = 200, width = 7, height = 4.5)

# 西五反田 vs 東五反田の図を生成する
gg2 <- data %>%
  mutate(east_west_other = case_when(
    grepl('東五反田', address) ~ '東五反田',
    grepl('西五反田', address) ~ '西五反田',
    grepl('[^(東|西)五反田]', address) ~ 'その他'
  )) %>%
  count(east_west_other) %>%
  ggplot(aes(x = '', y = n, fill = east_west_other)) + geom_bar(stat = 'identity') + coord_polar('y') +
  # ref. https://seesaawiki.jp/w/kou1okada/d/20191205%3A%20GNU%20R%20-%20ggplot2%20-%20%B1%DF%A5%B0%A5%E9%A5%D5%A4%CB%B0%FA%A4%AD%BD%D0%A4%B7%C0%FE%A4%CB%A4%E8%A4%EB%CB%DE%CE%E3%A4%F2%C9%D5%A4%B1%A4%EB
  geom_text(aes(y = rev(cumsum(rev(n))) - n/2,label=sprintf("%2.1f%%\n（%d件）", 100*n/sum(n), n)), color = 'black', family = 'Hiragino Kaku Gothic Pro W3', size = 6.5) +
  guides(fill = guide_legend(reverse = TRUE)) + # 凡例並び替え
  theme(
    axis.text =  element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    # 不要な枠を消す
    panel.grid = element_blank(),
    rect = element_blank(),
    text = element_text(family = 'Hiragino Kaku Gothic Pro W3', size = 16),
  ) +
  scale_fill_brewer(palette = "RdYlBu")
ggsave('circle.png', gg2, dpi = 200, width = 7, height = 4.5)