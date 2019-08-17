library(dplyr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(leaflet)
library(ggthemes)
library(rgdal)
library(caret)
library(randomForest)
library(xgboost)
library(keras)
library(dplyr)

a <- 0
b <- 1
scd <- read.csv("master.csv")
scd$age <- gsub("[[:alpha:]]", "", scd$age)
scd$generation <- gsub("Generation ", "", scd$generation)
scd$generation <- gsub(" Generation", "", scd$generation)
world_spdf=readOGR(dsn= getwd() , layer="TM_WORLD_BORDERS_SIMPL-0.3")

theme1 <- theme(panel.grid.major.y = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major.x = element_line(colour = "#F1F1F1"),
               panel.background = element_rect(colour = "white",
                                               fill = "white"))

theme2 <- theme(panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_line(colour = "#F1F1F1"),
                panel.background = element_rect(colour = "white",
                                                fill = "white"))

theme3 <- theme(panel.grid.minor = element_blank(),
                panel.grid.major.y = element_line(colour = "#F1F1F1"),
                panel.grid.major.x = element_line(colour = "#F1F1F1"),
                panel.background = element_rect(colour = "white",
                                                fill = "white"))

#chart 1 - X Country with Most Suicide Cases
spprt1 <- scd %>% 
  group_by(country) %>% 
  summarise(total_cases = sum(suicides_no)) %>% 
  arrange(desc(total_cases))

plot1 <- function(data1){
spprt1x <- spprt1 %>% 
  top_n(n = as.integer(data1))
spprt1x$country <- factor(spprt1x$country)

plttab1 <- scd %>%
  filter(country %in% spprt1x$country) %>% 
  group_by(country,sex) %>% 
  summarise(total_cases = sum(suicides_no)) %>% 
  arrange(desc(total_cases))

plot1 <- ggplot(plttab1)+
  geom_bar(aes(x=reorder(country, total_cases), y=total_cases, fill=sex,
               text=paste("Country : ",country, "<br>","Total Cases: ",total_cases,"<br>","Sex :",sex)),
           stat="identity", position="stack")+
  coord_flip()+
  xlab("")+
  ylab("Total Suicide Case")+
  theme(legend.title = element_blank())+
  scale_fill_manual(values = c("#F84D54","#00BFAC"))+
  theme1

return(plot1)
}
#chart2 - Suicide Cases by Age Range
plttab2 <- scd%>%
  group_by(age,sex) %>% 
  summarise(total_cases = sum(suicides_no)) %>% 
  arrange(desc(total_cases))

plttab2$age <- factor(plttab2$age, levels=c("5-14 ", "15-24 ", "25-34 ", "35-54 ", "55-74 ", "75+ "))

plot2 <- ggplot(plttab2)+
  geom_bar(aes(x=age, y=total_cases, fill=sex, text=paste("Age Group : ",age,"<br>","Total Cases: ",total_cases,"<br>","Sex :",sex))
           ,stat="identity", position="stack")+
  ylab("Total Suicide Case")+
  xlab("Age Group")+
  theme(legend.title = element_blank())+
  theme2+
  scale_fill_manual(values = c("#F84D54","#00BFAC"))

#Chart 3 - Total Suicide Over The Year
plot3 <- function(data){
  if (data=="1"){
     plttab3 <- scd %>% 
      group_by(year,age,sex) %>% 
      summarise(total_cases = sum(suicides_no)) %>% 
       ungroup() %>% 
       mutate()
     
    levels(plttab3$sex) <- c("Female", "Male")
    plttab3$age <- as.factor(factor(plttab3$age, levels=c("15-24 ", "25-34 ", "35-54 ", "55-74 ", "75+ ","5-14 ")))
    
    plt3 <- ggplot(plttab3,aes(x=year,y=total_cases,group = age,fill=age))+
      geom_area(aes(text=paste("Year : ",year,"<br>","Total Case : ",total_cases,"Age :",age),alpha = 0.9))+
      facet_wrap(plttab3$sex,ncol=2)+
      ylab("Total Case")+
      xlab("Year")+
      theme2+
      theme(strip.background =element_rect(fill="white"))+
      guides(alpha=FALSE)+
      scale_fill_manual(values = c("#00BFAC","#3A484B","#F84D54","#FE9556","#AA4F96","#EBDA00"))
    plt3
  }
  
  else if (data=="2"){
    plttab3 <- scd %>% 
      group_by(year,generation,sex) %>% 
      summarise(total_cases = sum(suicides_no)) %>% 
      ungroup() %>% 
      mutate()
    
    first_years <- plttab3 %>% 
      group_by(generation,sex) %>%
      summarise(year = min(year) - 1) %>%
      filter(year > 1984) %>%
      ungroup() %>% 
      mutate(total_cases = 0)
    
    last_years <- plttab3 %>% 
      group_by(generation,sex) %>%
      summarise(year = max(year) + 1) %>%
      filter(year < 2017) %>%
      ungroup() %>% 
      mutate(total_cases = 0)
    
    plttab3 <- bind_rows(plttab3, first_years,last_years) %>%
      arrange(year, total_cases)

    levels(plttab3$sex) <- c("Female", "Male")
    
    plttab3$generation <- as.factor(factor(plttab3$generation, levels=c("Boomers", "X", "Silent", "G.I.", "Millenials","Z")))
    
    plt3 <- ggplot(plttab3,aes(x=year,y=total_cases,group = generation, fill = generation))+
      geom_area(aes(text=paste("Year : ",year,"<br>","Total Case : ",total_cases,"Generation :",generation)),alpha = .7, position = "identity")+
      facet_wrap(plttab3$sex,ncol=2)+
      ylab("Total Case")+
      xlab("Year")+
      labs(fill = "Generation")+
      scale_alpha_continuous(guide='none')+
      theme2+
      theme(strip.background =element_rect(fill="white"))+
      guides(alpha=FALSE)+
      scale_fill_manual(values = c("#00BFAC","#3A484B","#F84D54","#FE9556","#AA4F96","#EBDA00"))
  }
  else {
    plttab3 <- scd %>% 
      group_by(year,sex) %>% 
      summarise(total_cases = sum(suicides_no)) %>% 
      ungroup() %>% 
      mutate()
    
    levels(plttab3$sex) <- c("Female", "Male")
    
    plt3 <- ggplot(plttab3,aes(x=year,y=total_cases,group = sex,fill = sex))+
      geom_area(aes(text=paste("Year : ",year,"<br>","Total Case : ",total_cases,"Sex :",sex),alpha = 1.0))+
      facet_wrap(plttab3$sex,ncol=2)+
      ylab("Total Case")+
      xlab("Year")+
      theme2+
      theme(strip.background =element_rect(fill="white"))+
      guides(alpha=FALSE)
  }
    return(plt3)
}

#Chart 4 - Map
##Menyamakan Nama Negara
plot4 <- function(data1,data2) {
spprt2 <- scd %>% 
  filter(year>=data1 & year<=data2) %>% 
  group_by(country) %>% 
  summarise(total_cases = sum(suicides_no)) %>% 
  arrange(desc(total_cases))

spprt2$country <- as.character(spprt2$country)
spprt2[spprt2$country=="Cabo Verde",1] <- "Cape Verde"
spprt2[spprt2$country=="Republic of Korea",1] <- "Korea, Republic of"
spprt2[spprt2$country=="Russian Federation",1] <- "Russia"
spprt2[spprt2$country=="Saint Vincent and Grenadines",1] <- "Saint Vincent and the Grenadines"
world_spdf@data <- left_join(world_spdf@data, spprt2, by = c("NAME" = "country"))
spprt2$country <- as.factor(spprt2$country)


##Text untuk di hover
mytext=paste("Country: ", world_spdf@data$NAME,"<br/>", "Total Suicide Case : ", world_spdf@data$total_cases,"<br/>") %>%
  lapply(htmltools::HTML)

#choropleth with leaflet
mybins=c(0,1000,5000,10000,50000,100000,500000,1000000,2000000)
mypalete <- colorBin(palette=c("#c6dcff","#75aaff","#3582ff","#0061ff","#003387","#001a47"), domain=world_spdf@data$total_cases, na.color="transparent", bins = mybins)
#Create Map
plot4 <- leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons(color = ~mypalete(total_cases), weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 1.0,
              label = mytext,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "13px", direction = "auto")
  )  %>% 
  addLegend(pal=mypalete, values=~total_cases, opacity=1.0, title = "Suicide Case", position = "bottomleft" )
return(plot4)
}

#Plot 5 dan 6
plot5 <- function(data1){
  if (data1 == "1") {
    #chart 5 - HDI, GDP per Capita and total scd
    plttab5 <- scd %>% 
      group_by(country,year,country.year,HDI.for.year,gdp_per_capita....) %>% 
      summarise(totalscd = sum(suicides_no))
    
    plot5 <- ggplot(plttab5, aes(x = HDI.for.year, y = gdp_per_capita...., size = totalscd, colour = country))+
      geom_point(aes(text=paste("Country Year: ",country.year,"<br>","Total Cases: ",totalscd,"<br>","Human Development Index: ",
                                HDI.for.year,"GDP per Capita: ",gdp_per_capita....)))+
      ylab("GDP per Capita")+
      xlab("HDI for Year")+
      scale_y_continuous(labels = scales::comma)+
      theme(legend.position = "none")+
      theme3
    plot5
  }
  else{
    #chart 6 - total scd by country using mean HDI and GDP per Capita
    plttab6 <- scd %>% 
      group_by(country) %>% 
      summarise(totalscd = sum(suicides_no), HDI = mean(HDI.for.year,na.rm=TRUE),GDP = mean(gdp_per_capita....,na.rm=TRUE))
    
    plot6 <- ggplot(plttab6, aes(x = HDI, y = GDP, size = totalscd, colour = country))+
      geom_point(aes(text=paste("Country: ",country,"<br>","Total Cases: ",totalscd,"<br>","Mean Human Development Index: ",
                                HDI,"Mean GDP per Capita: ",GDP)))+
      ylab("GDP per Capita")+
      xlab("HDI for Year")+
      scale_y_continuous(labels = scales::comma)+
      theme(legend.position = "none")+
      theme3
  }
}

#chart7 - Suicide Cases by Generation
plttab7 <- scd%>%
  group_by(generation,sex) %>% 
  summarise(total_cases = sum(suicides_no)) %>% 
  arrange(desc(total_cases))

plot7 <- ggplot(plttab7)+
  geom_bar(aes(x=generation, y=total_cases, fill=sex, text=paste("Generation : ",generation,"<br>","Total Cases: ",total_cases,"<br>","Sex :",sex))
           ,stat="identity", position="stack")+
  ylab("Total Suicide Case")+
  xlab("Generation")+
  theme(legend.title = element_blank())+
  theme2+
  scale_fill_manual(values = c("#F84D54","#00BFAC"))

#chart 8 - scatterlot for correlation
plot8 <- function(category){
  if (category == "1"){x <- aes(x = HDI.for.year, y = suicides_no)
  xlab <- "HDI for Year"
  ylim <-c(0,12000)}
  else {x<- aes(x = gdp_per_capita...., y = suicides_no)
  xlab <- "GDP per Capita"
  ylim <- c(0,20000)}
plt8 <- ggplot(scd, x)+
  geom_point(colour = "#F84D54")+
  ylab("Number of Cases")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  xlab(xlab)+
  ylim(ylim)+
  theme(legend.position = "none")+
  theme3
plt8
}