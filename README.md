# gotanda-lunch

五反田のランチ情報を可視化するR script。以下の記事用です。

[五反田のランチ事情について考える（2020年版） - freee Developers Blog](https://developers.freee.co.jp/entry/gotanda-lunch-2020)

## Usage

R 3.6以降をインストールしておき、必要なパッケージもインストールします。

```R
install.packages(c('tidyverse', 'leaflet', 'RColorBrewer', 'htmlwidgets'))
```

スクリプトを上から順番にすべて実行すれば、地図`index.html`とPNG形式のグラフが出力されます。
