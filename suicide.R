#stable version of the package can be installed using 
#install.packages("gganimate") 
#latest development version can be obtained by 
#devtools::install_github(“thomasp85/gganimate”).

#loading required libraries
library(tidyverse)
library(reshape2)
library(ggthemes)
library(gganimate)
library(gifski)
#loading dataset
suicide_data<-read.csv("./data/master.csv",header = TRUE,stringsAsFactors = FALSE)

#selecting columns to work with
suicide_sub<-suicide_data %>%
  select("country","year" ,"sex","suicides_no") # %>%

#rename("country"="ï..country" )

#function to sum the total suicide per country
n<-unique(suicide_sub$country)

country<-function(x){
  suicide2<-suicide_sub %>% filter(country==x)
  sum(suicide2$suicides_no)
}

#return a list with all total deaths per country
country_total<-sapply(n,function(x) country(x))

#creating a dataframe with top 10 total suicides per country
df<-do.call(rbind,Map(data.frame,Country=n,Total_Suicides=country_total))
df2<-df %>% arrange(desc(Total_Suicides))
df3<-head(df2,n=10)

write.csv(df3,"./data/total_suicide.csv")

#plotting the top 10 countries leading in the total suicide rates
ggplot(df3,aes(reorder(Country,Total_Suicides),Total_Suicides,fill=as.factor(Country)))+
  geom_col()+
  coord_flip(clip = "off", expand = FALSE)+
  guides( fill = FALSE) +
  labs(title="TOTAL SUICIDE DEATHS PER COUNTRY FROM 1985-2016", 
       y="Total Suicides Per Country", x="Country")+
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = paste(Total_Suicides,"")), hjust = 1)




#subset initial data with top 10 countries
top_suicide<-suicide_sub %>%
  filter(country==c("Russian Federation","United States","Japan","France","Ukraine","Germany","Republic of Korea","Brazil","Poland","United Kingdom"))

#filtering years with consistent data
top_suicide2<-top_suicide %>%
  filter(year %in%c(1990:2014)) 
top_suicide2$sex <- as.factor(top_suicide2$sex)
#summing the total male & female suicides per country for each year
sm3<-aggregate(suicides_no~country+year,top_suicide2,sum)
#* 1 ensures we have non-integer ranks while sliding
sm4<-sm3 %>%
  group_by(year) %>%
  mutate(rank = min_rank(-suicides_no) * 1) %>%
  ungroup()








#plotting static plot
static_plot<-ggplot(sm4,aes(rank,group=country,fill=as.factor(country),color=as.factor(country))) +
  geom_tile(aes(y = suicides_no/2,
                height = suicides_no,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=suicides_no,label = paste(" ",suicides_no)), hjust=0)+
  coord_flip(clip = "off", expand = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme_minimal() +
  theme(
    plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
    plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
    plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
    axis.ticks.y = element_blank(), 
    axis.text.y = element_blank(), 
    plot.margin = margin(1,1,1,4, "cm")
  )




#creating final animation
plt<-static_plot + transition_states(states = year, transition_length = 4, state_length = 1) + 
  ease_aes('cubic-in-out') +
  #view_follow(fixed_x = TRUE) +
  labs(title = 'Total Suicides per Year : {closest_state}', 
       subtitle = "Top 10 Countries",
       caption = "Data Source: World Bank Data",
       x="Countries", y="Total Suicides per year")


#rendering the animation for gif
final_animation<-animate(plt,100,fps = 20,duration = 30, width = 950, height = 750, renderer = gifski_renderer())
#rendering the animation for mp4
#https://www.rdocumentation.org/packages/ndtv/versions/0.13.0/topics/install.ffmpeg
#https://ffmpeg.zeranoe.com/builds/win64/static/
animate(plt,100,fps = 20,duration = 30, width = 950, height = 750, renderer = ffmpeg_renderer())


#saving the animation
anim_save("./data/suicide_animate.gif",animation=final_animation)


