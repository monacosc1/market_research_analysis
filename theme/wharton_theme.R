library(ggplot2)
library(scales)
library(extrafont)
library(magick)

wharton_theme<-theme_grey()+
  theme(
    panel.background = element_blank()
    ,axis.line = element_line(color = '#666666')
    ,axis.text.x = element_text(angle = 0, vjust = .5, family = "Source Sans Pro")
    ,panel.grid.major.x = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.grid.major.y = element_line(color='#999999', linetype="dotted")
    ,plot.title = element_text(family="Panton Bold", size = 16)
    ,axis.title = element_text(family="Source Sans Pro", size=14)
    ,axis.text = element_text(family="Source Sans Pro", size=12)
    ,legend.text = element_text(family="Source Sans Pro", size=12)
    ,legend.title = element_text(family="Source Sans Pro", size=12)
    ,plot.caption = element_text(family="Source Sans Pro", size=10, margin = margin(10,2,2,0))
    ,legend.position = "right"
  ) 

wharton_theme_dark<-theme_grey()+
  theme(
    panel.background = element_rect(fill = '#08080D')
    ,plot.background = element_rect(fill = '#08080D')
    ,legend.background = element_rect(fill = '#08080D', color = '#08080D')
    ,legend.key = element_rect(fill = '#08080D', color = '#08080D')
    ,axis.line = element_line(color = '#EBEBEB')
    ,axis.text.x = element_text(angle = 0, vjust = .5, family = "Source Sans Pro")
    ,panel.grid.major.x = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.grid.major.y = element_line(color='#EBEBEB', linetype="dotted")
    ,plot.title = element_text(family="Panton Bold", size = 16, color = "#FFFFFF")
    ,axis.title = element_text(family="Source Sans Pro", size=14, color = "#FFFFFF")
    ,axis.text = element_text(family="Source Sans Pro", size=12, color = "#FFFFFF")
    ,legend.text = element_text(family="Source Sans Pro", size=12, color = "#FFFFFF")
    ,legend.title = element_text(family="Source Sans Pro", size=12, color = "#FFFFFF")
    ,plot.caption = element_text(family="Source Sans Pro", size=10, color = "#FFFFFF")
    ,legend.position = "right"
  ) 

x_vertical <- theme(axis.text.x = element_text(angle = 90))

wharton_palette <- c('#5064FA', '#08080D', '#001E46', '#01BAEF', '#00F0B4', '#F2FF49',
                     '#FF4365', '#676767')

whapal_02 <- c(wharton_palette[c(1,2,5,7)])
whapal_21 <- c(wharton_palette[c(1,4,5,6,7,8)])

logo <- image_read("../theme/main-wharton-logo.png")
logo_dark <- image_read("../theme/main-wharton-logo.png")


addlogo <- function(){
  grid::grid.raster(logo, x = 0.07, y = 0.01, just = c('left', 'bottom'), width = unit(1, 'inches'))
}
addlogo_dark <- function(){
  grid::grid.raster(logo_dark, x = 0.07, y = 0.01, just = c('left', 'bottom'), width = unit(1, 'inches'))
}

comprss <- function(tx) { 
  div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                      c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
  paste0('$',round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), 
        c("","K","M","B","T")[div] )}

