Corona-19 from Jan-22 to Mar-01
================

-   [Libraries](#libraries)
-   [Dataset](#dataset)
-   [Preprocessing](#preprocessing)
    -   [Cleaning Data](#cleaning-data)
    -   [Derived Tables](#derived-tables)
    -   [Complete Data](#complete-data)
    -   [Country Wise Latest Data](#country-wise-latest-data)
    -   [Chines Province Wise Latest Data](#chines-province-wise-latest-data)
-   [Map](#map)
    -   [Reported cases over the world](#reported-cases-over-the-world)
    -   [Reported cases in China](#reported-cases-in-china)
    -   [Cases in Countries](#cases-in-countries)
    -   [Spread over the time](#spread-over-the-time)
    -   [Countries with deaths reported](#countries-with-deaths-reported)
    -   [Countries and Provinces with no recovered cases](#countries-and-provinces-with-no-recovered-cases)
    -   [Countries and Provinces with no affected case anymore](#countries-and-provinces-with-no-affected-case-anymore)
    -   [Countries and Provinces with all the cases recovered](#countries-and-provinces-with-all-the-cases-recovered)
    -   [Diamond Princess Cruise ship Status](#diamond-princess-cruise-ship-status)
-   [Hubei - China - World](#hubei---china---world)
-   [Number of new cases everyday](#number-of-new-cases-everyday)
    -   [Number of Cases](#number-of-cases)
-   [Number of Places to which COVID-19 Spread](#number-of-places-to-which-covid-19-spread)
-   [Recovery and Mortality Rate Over The Time](#recovery-and-mortality-rate-over-the-time)
-   [Proportion of Cases](#proportion-of-cases)
-   [Number of cases](#number-of-cases-1)
-   [Number of new cases](#number-of-new-cases)
-   [Number of Countries with new cases](#number-of-countries-with-new-cases)
-   [refer](#refer)
    -   [What i have to study later.](#what-i-have-to-study-later.)
    -   [What i solved the above problem.](#what-i-solved-the-above-problem.)

Libraries
---------

``` r
library(data.table)
library(tidyverse)
library(lubridate) # to manipulate Date class
library(stringr)
library(scales)   # for using scale_x_date


library(DT)
library(plotly)
library(treemap)  # for drawing treemap
library(viridis) # nice color palette
library(RColorBrewer) # for color palettes
library(purrr) # for mapping over a function
library(magick) # this is call to animate/read pngs
#devtools::install_github("gluc/data.tree")
#devtools::install_github("timelyportfolio/d3treeR")
library(d3treeR)


library(shiny)
library(rgeos)
library(maps)
library(maptools)
library(leaflet)   # interative map
```

Dataset
-------

``` r
full_table <-fread("covid_19_clean_complete.csv", data.table = F)
```

Preprocessing
-------------

### Cleaning Data

``` r
full_table$`Country/Region` <- str_replace(full_table$`Country/Region`,'Mainland China','China')

# filling missing values with NA
full_table$`Province/State`[full_table$`Province/State`==""] <- NA
full_table$index <- 1:nrow(full_table)
```

``` r
full_table$Date <- str_replace(full_table$Date,"2002","2020")
full_table$Date <- gsub(pattern = "-20$", replacement = "-02",x = full_table$Date)
full_table$Date <- gsub(pattern = "2003-01-20", replacement ="2020-01-03",x = full_table$Date)

datatable(full_table, caption = "Table : test data",
          filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-7-1.png)

[View the full\_table](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/full_table.html)

``` r
temp<-full_table %>% filter(str_length(full_table$Date)==10)
temp2<-full_table %>% filter(str_length(full_table$Date)!=10)


temp$Date<-ydm(temp$Date)
temp2$Date<-mdy(temp2$Date)
```

``` r
full_table<-rbind(temp,temp2) %>% arrange(index)
```

### Derived Tables

``` r
`%notin%` <- Negate(`%in%`)  # user function

ship <- full_table  %>% filter(`Province/State`=="Diamond Princess cruise ship")
full_table <- full_table %>% filter(full_table$index %notin% ship$index)
# full_table <- full_table %>% filter(!(full_table$index %in% ship$index))


china <- full_table %>% filter(`Country/Region`=="China") 
row <- full_table %>% filter(`Country/Region`!="China") 


full_latest <- full_table %>% filter(full_table$Date==max(full_table$Date))
china_latest <- full_latest %>% filter(full_latest$`Country/Region`=="China")
row_latest <- full_latest %>% filter(full_latest$`Country/Region`!="China")

full_latest_grouped <- full_latest %>% 
  group_by(`Country/Region`) %>% 
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered))

china_latest_grouped <- china_latest %>% 
  group_by(`Province/State`) %>% 
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered))

row_latest_grouped <- row_latest %>% 
  group_by(`Country/Region`) %>% 
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered))
```

``` r
tmp<- full_table %>% 
      group_by(Date) %>% 
      summarise(Confirmed = sum(Confirmed),
                Deaths = sum(Deaths),
                Recovered = sum(Recovered)) %>% 
      arrange(Date)

tmp %>% tail(1) %>% data.frame
```

    ##         Date Confirmed Deaths Recovered
    ## 1 2020-03-01     87666   2990     42706

### Complete Data

``` r
tmp<-full_latest %>% 
  group_by(`Country/Region`, `Province/State`) %>% 
  summarise(Confirmed = max(Confirmed),
            Deaths = max(Deaths),
            Recovered = max(Recovered))

datatable(tmp, caption = "Table : test data",
          filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-13-1.png)

[View the Complete Data](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/completedata.html)

### Country Wise Latest Data

``` r
tmp<-full_latest_grouped %>% 
  arrange(-Confirmed)
  
datatable(tmp, caption = "Table : test data",
          filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-15-1.png)

[View the Country wise latest data](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/country_wise_latest_data.html)

### Chines Province Wise Latest Data

``` r
tmp <- china_latest_grouped %>% 
  arrange(-Confirmed)

datatable(tmp, caption = "Table : test data",
          filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-17-1.png)

[View the Chines Province Wise Latest Data](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/chinese_province_wise_latest_data.html)

Map
---

### Reported cases over the world

``` r
# World wide - except for ship
not_china <- full_table %>% filter(full_table$`Country/Region` != "China")

pal <- colorNumeric(palette = colorRampPalette(c("blue", "red"))(10),
                    domain = 0:max(not_china$Confirmed))


mnpopup1 <- paste0("<li>Country: ", as.character(not_china$`Country/Region`), "<br>",
                   "<li>Province: ", as.character(not_china$`Province/State`), "<br>",
                   "<li>ConFirmed: ", as.character(not_china$Confirmed), "<br>",
                   "<li>Deaths: ", as.character(not_china$Deaths), "<br>",
                   "<li>Recovered: ", as.character(not_china$Recovered), "<br>")




labs1 <- lapply(seq(nrow(not_china)), function(i) {
  paste0( '<b>',"Country: ","</b>" , not_china[i, "Country/Region"], '<br>', 
          '<b>',"Province: ","</b>" , not_china[i, "Province/State"], '<br>',
          '<b>',"ConFirmed: ","</b>" , not_china[i, "Confirmed"], '<br>',
          '<b>',"Deaths: ","</b>", not_china[i, "Deaths"], '<br>', 
          '<b>',"Recovered: ","</b>", not_china[i, "Recovered"],'<br>') 
})






leaflet(data=not_china) %>% addTiles() %>%  
  
  addCircleMarkers(data=not_china,
                   ~Long, ~Lat, 
                   popup = mnpopup1, label = lapply(labs1, htmltools::HTML),
                   radius = ~sqrt(Confirmed)-10, opacity = 0.3, 
                   fillOpacity = 0.2, color= ~pal(Confirmed)) %>%       
  addLegend("bottomright", pal = pal, values = ~Confirmed,
            title = "Num of <br> Confirmed",
            opacity = 1,
            bins = 7)
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-19-1.png)

[View the Reported cases over the world map](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/all_over_map.html)

### Reported cases in China

``` r
china_latest_other<-china_latest %>% filter(`Province/State`!="Hubei")
china_latest_Hubei<-china_latest %>% filter(`Province/State`=="Hubei")



mnpopup1 <- paste0("<b>","Province: ","</b>", as.character(china_latest_other$`Province/State`), "<br>",
                   "<b>","ConFirmed: ","</b>", as.character(china_latest_other$Confirmed), "<br>",
              "<b>", "Deaths: ", "</b>", as.character(china_latest_other$Deaths), "<br>",
              "<b>", "Recovered: ", "</b>", as.character(china_latest_other$Recovered), "<br>")

mnpopup2 <- paste0("<b>","Province: ","</b>", as.character(china_latest_Hubei$`Province/State`), "<br>",
                   "<b>","ConFirmed: ","</b>", as.character(china_latest_Hubei$Confirmed), "<br>",
              "<b>", "Deaths: ", "</b>", as.character(china_latest_Hubei$Deaths), "<br>",
              "<b>", "Recovered: ", "</b>", as.character(china_latest_Hubei$Recovered), "<br>")


labs1 <- lapply(seq(nrow(china_latest_other)), function(i) {
  paste0( '<b>',"Province: ","</b>" , china_latest_other[i, "Province/State"], '<br>',
          '<b>',"ConFirmed: ","</b>" , china_latest_other[i, "Confirmed"], '<br>', 
          '<b>',"Deaths: ","</b>", china_latest_other[i, "Deaths"], '<br>', 
          '<b>',"Recovered: ","</b>", china_latest_other[i, "Recovered"],'<br>') 
})

labs2 <- lapply(seq(nrow(china_latest_Hubei)), function(i) {
  paste0( '<b>',"Province: ","</b>" , china_latest_Hubei[i, "Province/State"], '<br>',
          '<b>',"ConFirmed: ","</b>" , china_latest_Hubei[i, "Confirmed"], '<br>', 
          '<b>',"Deaths: ","</b>", china_latest_Hubei[i, "Deaths"], '<br>', 
          '<b>',"Recovered: ","</b>", china_latest_Hubei[i, "Recovered"],'<br>') 
})






leaflet(data=china_latest) %>% addTiles() %>% 
  setView(109, lat=38, zoom = 4 ) %>% 
  
  addCircleMarkers(data=china_latest_other,
                   ~Long, ~Lat, 
                   popup = mnpopup1, label = lapply(labs1, htmltools::HTML),
                   radius = ~Confirmed/140, color="blue") %>% 
  
  addCircleMarkers(data = china_latest_Hubei,
                   ~Long, ~Lat, 
                   popup = mnpopup2, label = lapply(labs2, htmltools::HTML),
                   radius = ~Confirmed/3000, color = "red")
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-21-1.png)

[View the Reported cases in China map](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/china_map.html)

In here, i didn't use the below map. just refer.
[Download the world map](http://thematicmapping.org/downloads/world_borders.php)

### Cases in Countries

``` r
df1 <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
names(df1)[1]<-"Country/Region"

df<-full_latest_grouped

df2<-left_join(df,df1,by="Country/Region")
df2$CODE[57]<-"KOR"
df2$CODE[64]<-"GBR"
df2$CODE[66]<-"USA"
df2$CODE[47]<-"MKD"


# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = T,
  showcoastlines = T,
  projection = list(type = 'Mercator')
)


plot_geo(df2) %>% 
  add_trace(
    z = ~log(Confirmed), colors ="YlGn",
    text = ~paste(`Country/Region`,"\n Confirmed : ", Confirmed), locations = ~CODE, marker = list(line = l),
    showscale = FALSE, hoverinfo = 'text') %>% 
  colorbar(title = 'log(Confirmed) :', tickprefix = '->') %>% 
  layout(title = "Countries with Confirmed Cases",
         geo = g) 
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-23-1.png)

[View the Confirmed Cases on the map](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/Confirmed_Cases_on_the_map.html)

``` r
plot_geo(df2 %>% filter(Deaths>0)) %>% 
  add_trace(
    z = ~log(Deaths), colors ="YlOrRd",
    text = ~paste(`Country/Region`,"\n Deaths : ", Deaths), locations = ~CODE, marker = list(line = l),
    showscale = FALSE, hoverinfo = 'text') %>% 
  colorbar(title = 'log(Deaths) :', tickprefix = '->') %>% 
  layout(title = "Countries with Deaths Cases",
         geo = g) 
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-25-1.png)

[View the Deaths Cases on the map](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/Deaths_Cases_on_the_map.html)

``` r
temp <- full_table %>% group_by(`Country/Region`, Date) %>%
  summarise(Confirmed=sum(Confirmed),
            Deaths=sum(Deaths),
            Recovered=sum(Recovered))

fig1 <- temp %>%
        ggplot(aes(x=Date, y=Confirmed, fill=`Country/Region`)) +
        geom_bar(stat = "identity") +
        ggtitle("Cases")


ggplotly(fig1, dynamicTicks = T)
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-27-1.png)

[View the Cases in differnt Countries - Cases](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/case_in_difference_country.html)

``` r
fig2 <- temp %>% 
        ggplot(aes(x=Date, y=Deaths, color=`Country/Region`)) + 
        geom_bar(stat="identity") +
        ggtitle("Deaths")

ggplotly(fig2, dynamicTicks = T)
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-29-1.png)

[View the Cases in different Countrires - Deaths](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/case_in_difference_country2.html)

``` r
temp <- full_latest_grouped %>% 
  dplyr::select(`Country/Region`, Deaths) %>%
  arrange(-Deaths) %>% 
  filter(Deaths>0)


temp %>% data.frame
```

    ##    Country.Region Deaths
    ## 1           China   2870
    ## 2            Iran     54
    ## 3           Italy     34
    ## 4     South Korea     17
    ## 5           Japan      6
    ## 6          France      2
    ## 7       Hong Kong      2
    ## 8       Australia      1
    ## 9     Philippines      1
    ## 10         Taiwan      1
    ## 11       Thailand      1
    ## 12             US      1

### Spread over the time

If you excute Shiny app with above code, you can see like the below.
<!-- ![](https://raw.githubusercontent.com/koojaekwan/Corona-19/master/html_files/shiny_gif.gif) -->

``` r
china_map <- china %>% group_by(Date, `Province/State`) %>%
  summarise(Confirmed=max(Confirmed),
            Deaths=max(Deaths),
            Recovered=max(Recovered),
            Lat=max(Lat),
            Long=max(Long))

china_map$Date<-format(china_map$Date, format='%m/%d/%Y')


plot_geo(china_map,x = ~Long, y = ~Lat,  showscale=FALSE, showlegend = FALSE,
         frame=~Date, mode = 'markers',color=~Confirmed,
         text = ~paste0(`Province/State`, "\n", "Comfirmed :",Confirmed),
         hoverinfo = 'text') %>% 
  
  add_markers(size = ~Confirmed,
              showlegend = FALSE, showscale=FALSE,
              marker=list(sizeref=0.07, sizemode="area")) %>% 
  
  layout(title = 'Spread in China over time',showlegend = FALSE, showscale=FALSE,
         geo = list ( scope = 'asia' ))
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-33-1.png)

[Spread in China over time](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/China_timeline.html)

``` r
full_map <- full_table %>% group_by(Date, `Country/Region`) %>%
  summarise(Confirmed=max(Confirmed),
            Deaths=max(Deaths),
            Recovered=max(Recovered),
            Lat=max(Lat),
            Long=max(Long))


full_map$Date<-format(full_map$Date, format='%m/%d/%Y')


plot_geo(full_map %>% filter(`Country/Region` %notin% "China"),
         x = ~Long, y = ~Lat,  showscale=FALSE, showlegend = FALSE,
         frame=~Date, mode = 'markers',color=~Confirmed,
         text = ~paste0(`Country/Region`, "\n", "Comfirmed :",Confirmed),
         hoverinfo = 'text') %>% 
  
  add_markers(size = ~Confirmed,
              showlegend = FALSE, showscale=FALSE,
              marker=list(sizeref=0.1)) %>% 
  
  layout(title = 'Spread outside China over time',showlegend = FALSE, showscale=FALSE,
         geo = list ( scope = 'world' ))
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-35-1.png)

[Spread outside China over time](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/world_timeline.html)

### Countries with deaths reported

``` r
temp <- full_latest_grouped %>% 
  dplyr::select(`Country/Region`, Deaths) %>% 
  filter(Deaths>0) %>% arrange(-Deaths)

temp %>% data.frame
```

    ##    Country.Region Deaths
    ## 1           China   2870
    ## 2            Iran     54
    ## 3           Italy     34
    ## 4     South Korea     17
    ## 5           Japan      6
    ## 6          France      2
    ## 7       Hong Kong      2
    ## 8       Australia      1
    ## 9     Philippines      1
    ## 10         Taiwan      1
    ## 11       Thailand      1
    ## 12             US      1

### Countries and Provinces with no recovered cases

``` r
temp <- row_latest_grouped %>% 
  filter(Recovered==0) %>% 
  dplyr::select(`Country/Region`, Confirmed, Deaths, Recovered) %>% 
  arrange(-Confirmed)

datatable(temp)
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-38-1.png)

[View the Countries and Provinces with no recovered cases](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/Countries%20and%20Provinces%20with%20no%20recovered%20cases.html)

``` r
# Provinces with no cases recovered

temp <- china_latest_grouped %>% 
  filter(Recovered==0) %>% 
  dplyr::select(`Province/State`, Confirmed, Deaths, Recovered) %>% 
  arrange(-Confirmed)


temp
```

    ## # A tibble: 0 x 4
    ## # ... with 4 variables: `Province/State` <chr>, Confirmed <int>,
    ## #   Deaths <int>, Recovered <int>

### Countries and Provinces with no affected case anymore

``` r
temp <- row_latest_grouped %>% 
  filter(Confirmed==Deaths+Recovered) %>% 
  dplyr::select(`Country/Region`, Confirmed, Deaths, Recovered) %>% 
  arrange(-Confirmed)


temp %>% data.frame
```

    ##   Country.Region Confirmed Deaths Recovered
    ## 1        Vietnam        16      0        16
    ## 2          India         3      0         3
    ## 3         Russia         2      0         2
    ## 4       Cambodia         1      0         1
    ## 5          Nepal         1      0         1
    ## 6      Sri Lanka         1      0         1

``` r
temp <- china_latest_grouped %>% 
  filter(Confirmed==Deaths+Recovered) %>% 
  dplyr::select(`Province/State`, Confirmed, Deaths, Recovered) %>% 
  arrange(-Confirmed)


temp %>% data.frame
```

    ##   Province.State Confirmed Deaths Recovered
    ## 1        Qinghai        18      0        18
    ## 2          Tibet         1      0         1

### Countries and Provinces with all the cases recovered

``` r
temp <- row_latest_grouped %>% 
  filter(Confirmed==Recovered) %>% 
  dplyr::select(`Country/Region`, Confirmed, Recovered) %>% 
  arrange(-Confirmed)

temp %>% data.frame
```

    ##   Country.Region Confirmed Recovered
    ## 1        Vietnam        16        16
    ## 2          India         3         3
    ## 3         Russia         2         2
    ## 4       Cambodia         1         1
    ## 5          Nepal         1         1
    ## 6      Sri Lanka         1         1

``` r
temp <- china_latest_grouped %>% 
  filter(Confirmed==Recovered) %>% 
  dplyr::select(`Province/State`, Confirmed, Recovered) %>% 
  arrange(-Confirmed)

temp %>% data.frame
```

    ##   Province.State Confirmed Recovered
    ## 1        Qinghai        18        18
    ## 2          Tibet         1         1

### Diamond Princess Cruise ship Status

``` r
temp <- ship %>% 
  arrange(Date) %>% 
  tail(1)

temp %>% data.frame
```

    ##                  Province.State Country.Region     Lat    Long       Date
    ## 40 Diamond Princess cruise ship         Others 35.4437 139.638 2020-03-01
    ##    Confirmed Deaths Recovered index
    ## 40       705      6        10  4947

``` r
temp <- ship %>% filter(Date==max(ship$Date))



mnpopup1 <- paste0("<b>","Province: ","</b>", as.character(temp$`Province/State`), "<br>",
                   "<b>","ConFirmed: ","</b>", as.character(temp$Confirmed), "<br>",
              "<b>", "Deaths: ", "</b>", as.character(temp$Deaths), "<br>",
              "<b>", "Recovered: ", "</b>", as.character(temp$Recovered), "<br>")


labs1 <- lapply(seq(nrow(temp)), function(i) {
  paste0( '<b>',"Province: ","</b>" , temp[i, "Province/State"], '<br>',
          '<b>',"ConFirmed: ","</b>" , temp[i, "Confirmed"], '<br>', 
          '<b>',"Deaths: ","</b>", temp[i, "Deaths"], '<br>', 
          '<b>',"Recovered: ","</b>", temp[i, "Recovered"],'<br>') 
})





leaflet(data=temp) %>% addTiles() %>% 
  setView(139.677376, lat=35.442777, zoom = 11 ) %>% 

  addCircleMarkers(data = temp,
                   ~Long, ~Lat, 
                   popup = mnpopup1, label = lapply(labs1, htmltools::HTML),
                   radius = ~17, color = "red") %>% 
  addProviderTiles(providers$CartoDB.Positron)
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-46-1.png)

[View the Diamond Princess Cruise ship Status map](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/ship.html)

-   The Diamond Princess cruise ship was carrying 3,711 passengers and crew on board
-   Ship arrived in Yokohama, near Tokyo, on February 3
-   <https://www.princess.com/news/notices_and_advisories/notices/diamond-princess-update.html>

Hubei - China - World
---------------------

``` r
temp <- full_latest %>% 
  mutate(location = case_when(
    `Country/Region`=="China" & `Province/State`=="Hubei" ~ "Hubei",
    `Country/Region`=="China" & `Province/State`!="Hubei" ~ "Other Chinese Provinces",
    `Country/Region`!="China" & `Province/State` %notin% "Hubei" ~ "Rest of the World"))



temp <- temp %>% group_by(location) %>% summarise(Confirmed=sum(Confirmed),
                                          Deaths=sum(Deaths),
                                          Recovered=sum(Recovered))


temp <- temp %>% gather(key=case,value=val,2:4)



fig <- temp %>% ggplot(aes(x=location, y=val, fill=case)) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip()

#levels 변경해야될까?
ggplotly(fig)
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-48-1.png)

[View the Hubei - China - World](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/Hubei_China_World.html)

``` r
DT::saveWidget(ggplotly(fig), "Hubei_China_World.html")
```

Number of new cases everyday
----------------------------

``` r
temp <- china %>% 
  group_by(Date) %>% 
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered)) %>% 
  mutate(Confirmed_date = c(min(Confirmed), diff(Confirmed)),
         Deaths_date = c(min(Deaths), diff(Deaths)),
         Recovered_date = c(min(Recovered), diff(Deaths)))


temp <- dplyr::select(temp,-c(2:4))
temp <- rename(temp, Confirmed = Confirmed_date,
               Deaths = Deaths_date,
               Recovered = Recovered_date)

temp <- temp %>% gather(key=Case, value=val,-1)



fig <- temp %>% ggplot(aes(x=Date, y=val, fill=Case)) +
         geom_bar(stat="identity") + ggtitle("Number of new cases in China everyday")
  
ggplotly(fig) %>% 
  layout(hovermode = "x")
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-50-1.png) [View the Number of new cases everyday in China](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/Number%20of%20new%20cases%20everyday.html)

``` r
temp <- row %>% 
  group_by(Date) %>% 
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered)) %>% 
  mutate(Confirmed_date = c(min(Confirmed), diff(Confirmed)),
         Deaths_date = c(min(Deaths), diff(Deaths)),
         Recovered_date = c(min(Recovered), diff(Deaths)))


temp <- dplyr::select(temp,-c(2:4))
temp <- rename(temp, Confirmed = Confirmed_date,
               Deaths = Deaths_date,
               Recovered = Recovered_date)

temp <- temp %>% gather(key=Case, value=val,-1)


fig <- temp %>% ggplot(aes(x=Date, y=val, fill=Case)) +
         geom_bar(stat="identity") + ggtitle("Number of new cases outside China everyday")
  
ggplotly(fig) %>% 
  layout(hovermode = "x")
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-52-1.png)

[View the Number of new cases everyday outside China](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/Number%20of%20new%20cases%20everyday2.html)

2월 7일 확진이 -인것은 뭐지?

### Number of Cases

``` r
temp <- full_table %>% 
          group_by(Date, `Country/Region`) %>% 
          summarise(Confirmed=max(Confirmed),
                    Deaths=max(Deaths),
                    Recovered=max(Recovered)) 


temp <- temp %>% filter(`Country/Region` %in% "China")
temp <- temp %>% gather(key=Case, value=Count, -c(1:2))



fig <- temp %>% ggplot(aes(x=Date, y=Count, fill=Case)) +
        geom_bar(stat="identity") +
        facet_grid(~Case) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        ggtitle("Cases in China") + xlab("")

ggplotly(fig) %>% 
    layout(hovermode = "x")
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-54-1.png)

[View the Number of Cases in China](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/Number%20of%20Cases.html)

``` r
temp <- full_table %>% 
          group_by(Date, `Country/Region`) %>% 
          summarise(Confirmed=max(Confirmed),
                    Deaths=max(Deaths),
                    Recovered=max(Recovered)) 


temp <- temp %>% filter(`Country/Region` %notin% "China")
temp <- temp %>% gather(key=Case, value=Count, -c(1:2))



fig <- temp %>% ggplot(aes(x=Date, y=Count, fill=Case)) +
        geom_bar(stat="identity") +
        facet_grid(~Case) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("Cases outside China") + xlab("")

ggplotly(fig) %>% 
    layout(hovermode = "x")
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-56-1.png)

[View the Number of Cases outside China](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/Number%20of%20Cases2.html)

``` r
DT::saveWidget(ggplotly(fig) %>% 
    layout(hovermode = "x"),
               "Number of Cases2.html")
```

Number of Places to which COVID-19 Spread
-----------------------------------------

``` r
spread_china <- china %>% 
  filter(Confirmed!=0) %>% 
  group_by(Date) %>% 
  distinct(`Province/State`) %>% 
  summarise("Province/State"=n())



spread_china %>% ggplot(aes(x=Date, y=`Province/State`)) +
  geom_line(col="blue",lwd=1) +
  scale_y_continuous(breaks=seq(22,31)) +
  scale_x_date(date_breaks = "4 day", 
              labels=date_format("%m-%d"),
              limits = as.Date(c('2020-01-22','2020-03-01'))) +
  ggtitle("Number of Provinces/States/Region of China \n to which COVID-19 spread over the time")
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-58-1.png)

``` r
spread_out <- full_table %>% 
  filter(Confirmed!=0) %>% 
  group_by(Date) %>% 
  distinct(`Country/Region`) %>% 
  summarise("Country/Region"=n())



spread_out %>% ggplot(aes(x=Date, y=`Country/Region`)) +
  geom_line(col="blue",lwd=1) +
  scale_y_continuous(breaks=seq(5,70,5)) +
  scale_x_date(date_breaks = "4 day", 
              labels=date_format("%m-%d"),
              limits = as.Date(c('2020-01-22','2020-03-02'))) +
  ggtitle("Number of Country/Region \n to which COVID-19 spread over the time")
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-59-1.png)

-   COVID-19 spread to all the provinces of the China really fast and early
-   Number of countries to which COVID-19 spread hasn't increased that much after first few weeks

Recovery and Mortality Rate Over The Time
-----------------------------------------

``` r
temp <- full_table %>% group_by(Date) %>% summarise(Confirmed=sum(Confirmed),
                                                    Deaths=sum(Deaths),
                                                    Recovered=sum(Recovered))

temp[, "# of Deaths to 100 Confirmed Cases"] <- round(temp[,"Deaths"]/temp[,"Confirmed"],3) * 100
temp[, "# of Recovered to 100 Confirmed Cases"] <- round(temp[,"Recovered"]/temp[,"Confirmed"],3) * 100
temp[, "# of Recovered to 1 Death Cases"] <- round(temp[,"Recovered"]/temp[,"Deaths"],3)

temp <- temp %>% gather(key=Ratio, value=Value, 5:7)



fig <- temp %>% ggplot(aes(x=Date, y=Value, col=Ratio)) + 
          geom_line(lwd=1) +
          ggtitle("Recovery and Mortaliry Rate Over The Time")

ggplotly(fig)
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-60-1.png)

[View the Recovery and Mortality Rate Over The Time](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/Recovery_and_Mortality.html)

-   During the first few weeks the there were more Deaths reported per day than Recoverd cases
-   Over the time that has changed drastically
-   Although the death rate hasn't come down, the number of recovered cases has defenitly increased

Proportion of Cases
-------------------

``` r
temp <- row_latest %>% 
  group_by(`Country/Region`) %>% 
  summarise(Confirmed=sum(Confirmed),
            Deaths=sum(Deaths),
            Recovered=sum(Recovered)) %>% 
  arrange(-Confirmed)


temp <- temp %>% gather(key=Case, value=Value,-1)

fig <- temp %>% ggplot(aes(x=reorder(`Country/Region`,Value), y=Value, fill=Case)) +
          geom_bar(stat="identity") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            theme( # remove the vertical grid lines
                   panel.grid.major.x = element_blank() ,
                   # explicitly set the horizontal lines (or they will disappear too)
                   panel.grid.major.y = element_line()) + 
          ggtitle("Number of Cases outside China") + 
          xlab("Country/Region")

ggplotly(fig)
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-62-1.png)

[View the Proportion of Cases outside China](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/Proportion_of_Cases.html)

``` r
temp <- china_latest %>% 
  group_by(`Province/State`) %>% 
  summarise(Confirmed=sum(Confirmed),
            Deaths=sum(Deaths),
            Recovered=sum(Recovered)) %>% 
  arrange(-Confirmed)


temp <- temp %>% gather(key=Case, value=Value,-1)

fig <- temp %>% ggplot(aes(x=reorder(`Province/State`,Value), y=Value, fill=Case)) +
          geom_bar(stat="identity") +
          coord_flip() +
            theme( # remove the vertical grid lines
                   panel.grid.major.x = element_blank() ,
                   # explicitly set the horizontal lines (or they will disappear too)
                   panel.grid.major.y = element_blank()) + 
          ggtitle("Number of Cases in China") +
          xlab("Province/State")

ggplotly(fig)
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-64-1.png)

[View the Proportion of Cases in China](https://github.com/koojaekwan/Corona-19/blob/master/html_files/Proportion_of_Cases2.html)

``` r
treemap1<-
treemap(china_latest,
        index = c("Province/State","Confirmed"),
        vSize = "Confirmed",
        vColor="Confirmed",
        type = "index",
        title="Number of Confirmed Cases in Chinese Provinces")


treemap2<-
treemap(china_latest,
        index = c("Province/State", "Deaths"),
        vSize = "Deaths",
        vColor = "Deaths",
        type = "index",
        title="Number of Deaths Reported in Chinese Provinces")


treemap3<-
treemap(china_latest,
        index = c("Province/State", "Recovered"),
        vSize = "Recovered",
        vColor = "Recovered",
        type = "index",
        title="Number of Recovered Cases in Chinese Provinces")


treemap4<-
treemap(row_latest,
        index = c("Country/Region", "Confirmed"),
        vSize = "Confirmed",
        vColor = "Confirmed",
        type = "index",
        title="Number of Confirmed Cases outside China") 


treemap5<-
treemap(row_latest,
        index = c("Country/Region", "Deaths"),
        vSize = "Deaths",
        vColor = "Deaths",
        type = "index",
        title="Number of Deaths Reported outside China")


treemap6<-
treemap(row_latest,
        index = c("Country/Region", "Recovered"),
        vSize = "Recovered",
        vColor = "Recovered",
        type = "index",
        title="Number of Recovered Cases outside China") 
```

``` r
#palette = "Set2",
#bg.labels=c("white")

fig1<-d3tree3(treemap1, rootname = "Number of Confirmed Cases in Chinese Provinces : ")

fig2<-d3tree3(treemap2, rootname = "Number of Deaths Reported in Chinese Provinces : ")

fig3<-d3tree3(treemap3, rootname = "Number of Recovered Cases in Chinese Provinces : ")

fig4<-d3tree3(treemap4, rootname = "Number of Confirmed Cases outside China : ")

fig5<-d3tree3(treemap5, rootname = "Number of Deaths Reported outside China : ")

fig6<-d3tree3(treemap6, rootname = "Number of Recovered Cases outside China : ")
```

-   If you want to view activative treemap, click the hyperlink!. treemap can be adjusted by you. If you want to return to original position, click the title.

[Number of Confirmed Cases in Chinese Provinces :](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/treemap_fig1.html)

[Number of Deaths Reported in Chinese Provinces :](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/treemap_fig2.html)

[Number of Recovered Cases in Chinese Provinces :](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/treemap_fig3.html)

[Number of Confirmed Cases outside China :](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/treemap_fig4.html)

[Number of Deaths Reported outside China :](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/treemap_fig5.html)

[Number of Recovered Cases outside China :](https://raw.githack.com/koojaekwan/Corona-19/master/html_files/treemap_fig6.html)

Number of cases
---------------

``` r
temp <- china %>% group_by(Date) %>% summarise(Confirmed=sum(Confirmed),
                                               Deaths=sum(Deaths),
                                               Recovered=sum(Recovered)) 


temp <- temp %>% gather(key=Cases, value=val, 2:4)


temp %>% ggplot(aes(x=Date, y=val, col=Cases)) + 
  geom_line(lwd=1) + 
  theme_classic() +
  theme(legend.background = element_rect(fill="white",
                                  size=0.5, linetype="solid", 
                                  colour ="black"),
        legend.position = c(0.15, 0.8))
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-70-1.png)

``` r
temp <- row %>% group_by(Date) %>% summarise(Confirmed=sum(Confirmed),
                                     Deaths=sum(Deaths),
                                     Recovered=sum(Recovered)) 

temp <- temp %>% gather(key=Cases, value=val, 2:4)

temp %>% ggplot(aes(x=Date, y=val, col=Cases)) + 
  geom_line(lwd=1) + 
  theme_classic() +
  theme(legend.background = element_rect(fill="white",
                                  size=0.5, linetype="solid", 
                                  colour ="black"),
        legend.position = c(0.15, 0.8))
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-71-1.png)

Number of new cases
-------------------

``` r
temp <- full_table %>% group_by(`Country/Region`,Date) %>% 
  summarise(Confirmed=sum(Confirmed),
            Deaths=sum(Deaths),
            Recovered=sum(Recovered)) %>% 
  filter(Confirmed!=0)
```

Number of Countries with new cases
----------------------------------

``` r
temp_c <- temp %>% group_by(Date) %>% summarise(n=n())

ggplot(temp_c, aes(x=Date, y=n)) + 
  geom_line(col="blue", lwd=1) + 
  theme_classic() +
  theme(legend.background = element_rect(fill="white",
                                  size=0.5, linetype="solid", 
                                  colour ="black"))
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-73-1.png)

``` r
# Number of new Countries
temp_c <- temp %>% group_by(Date) %>% summarise(n=n()) %>% mutate("Number of new Countries"=c(min(n),diff(n)))


ggplot(temp_c, aes(x=Date, y=`Number of new Countries`)) + 
  geom_bar(stat="identity", fill="skyblue") + 
  theme_classic() + 
  scale_y_continuous(breaks=seq(0,8,1)) +
  scale_x_date(date_breaks = "1 day", 
              labels=date_format("%m-%d"),
              limits = as.Date(c('2020-01-21','2020-03-01'))) +
  ggtitle("Number of new Countries") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Coronavirus_files/figure-markdown_github/unnamed-chunk-74-1.png)

refer
-----

-   \[treemap\]<https://www.r-graph-gallery.com/236-custom-your-treemap.html>

-   \[date range in ggplot\]<https://stackoverflow.com/questions/14162829/set-date-range-in-ggplot>

-   \[Multi popup in leaflet\]<https://www.nickconti.io/interactive_election_R.html>
-   \[Multi label in leaflet\]<https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines>
-   \[leaflet tiles\]<https://rstudio.github.io/leaflet/basemaps.html>

-   \[How to add code folding\]<https://stackoverflow.com/questions/37755037/how-to-add-code-folding-to-output-chunks-in-rmarkdown-html-documents>

### What i have to study later.

-   \[treemap plotly\]<https://plot.ly/r/treemaps/> - i don't know well. is it possible?

-   \[bar chart on the leaflet map\]<https://stackoverflow.com/questions/45538831/creating-a-leaflet-map-with-custom-labels-in-r>

-   \[Cumulative Lines Animation with ggplot2\]<https://plot.ly/ggplot2/cumulative-animations/>

### What i solved the above problem.

-   \[treemap d3tree\]<https://www.r-graph-gallery.com/237-interactive-treemap.html>
-   \[treemap d3tree\]<http://www.buildingwidgets.com/blog/2015/7/22/week-29-d3treer-v2>
