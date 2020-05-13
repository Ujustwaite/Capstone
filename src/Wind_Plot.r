#Be sure to set path appropriately to source and data
setwd("C:/Users/ujust/Documents/Github/WindPrediction/")

#Import the April 20, 2016 into train and the April 21, 2016 data into test
df_other <- read.csv("data/Met_Data/nj_other.csv")
df_other  <- df_other[df_other$file_date =="4/20/2016" | df_other$file_date == "4/21/2016",]

#Bring the data in
df <- read.csv("data/nj_col4_1yr_at_1min.csv")
df <- df[df$file_date == '2016-04-20' | df$file_date == '2016-04-21',]

#Bring the exogenous variables into the dataframe
df$avg_pressure <- df_other$avg_pressure
df$avg_tempr <-df_other$avg_temp
df$avg_humid <- df_other$avg_humidity


#convert the negative wind direction values to positive from 0 to 360. Adding because the wind values are already negative if greater than 180. 
df[which(df$horizontal_wdir < 0),]$horizontal_wdir = 360 + df[which(df$horizontal_wdir < 0),]$horizontal_wdir

df$dir_bin <- cut(df$horizontal_wdir, breaks = c(seq(0,360,30)))

#Get average windspeed by direction
library(dplyr)
by_dir <- df %>% group_by(dir_bin)
wspd_grouped <- by_dir %>% summarise(
  wspd = mean(horizontal_wspd),
)

wspd_grouped$lower_bin <- seq(0,330,30)
wspd_grouped$upper_bin <- seq(30,360,30)


library(plotly)
library(RColorBrewer)

dir_colors = colorRampPalette(brewer.pal(n = 8, name = "RdBu"))(12)

fig <- plot_ly(
  type = 'scatterpolar',
  mode = 'lines'
) 

for (i in 1:12) { 
  fig <- fig %>%
    add_trace(
      r = c(0, wspd_grouped$wspd[i], wspd_grouped$wspd[i], 0),
      theta = c(0, wspd_grouped$lower_bin[i], wspd_grouped$upper_bin[i], 0),
      fill = 'toself',
      fillcolor = dir_colors[i],
      line = list(
        color = 'black'
      )
    ) 
  
  }

fig <- fig %>%
  layout(
    title = list(text = "Wind Speed Average by Direction", y = 1, yanchor = "top"),
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,max(wspd_grouped$wspd)+1)
      ),
      angularaxis = list(
        nticks = 16,
        tickfont = list(
          size = 8
        ),
        rotation = 90,
        direction = 'clockwise'
      )
    ),
    showlegend = F
  )

fig
