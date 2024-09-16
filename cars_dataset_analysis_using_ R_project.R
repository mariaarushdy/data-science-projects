#readxl reads xl file
library(readxl)
cars <- read_excel("D:/studing/Advanced cars.xlsx")
View(cars)
library(tidyverse)
advancedcars <- na.omit(cars)
advcars <- advancedcars[sample(1:nrow(advancedcars),10000),]

#the higher the MPG the smaller the HP , inverse relation
ggplot(data= advcars) + geom_smooth(mapping = aes(x=HP,y=highwayMPG),color="brown")
#the price increases every year 
ggplot(data= advcars) + geom_smooth(mapping = aes(x=Year,y=MSRP))
#
ggplot(data= advcars) + geom_smooth(mapping = aes(x=Year,y=MSRP ,linetype=Driven_Wheels))
ggplot(data= advcars) + geom_smooth(mapping = aes(x=Year,y=MSRP ,group=Driven_Wheels))
ggplot(data= advcars) + geom_smooth(mapping = aes(x=Year,y=MSRP ,color=Driven_Wheels),show.legend = FALSE) #false

ggplot(data= advcars) + geom_point(mapping = aes(x=HP,y=citympg,size=Cylinders))
ggplot(data= advcars) + geom_point(mapping = aes(x=HP,y=highwayMPG,color=FuelType))
ggplot(data= advcars) + geom_point(mapping = aes(x=HP,y=highwayMPG,alpha=Cylinders))
ggplot(data= advcars) + geom_point(mapping = aes(x=HP,y=highwayMPG))+facet_grid(Cylinders~ Driven_Wheels)
ggplot(data= advcars) + geom_point(mapping = aes(x=HP,y=highwayMPG))+facet_grid(Cylinders~FuelType)
ggplot(data= advcars) + geom_point(mapping = aes(x=HP,y=highwayMPG))+facet_grid(.~Cylinders)
ggplot(data= advcars) + geom_point(mapping = aes(x=Popularity,y=MSRP))+facet_wrap(~VehicleStyle,nrow=4)
ggplot(data= advcars) + geom_point(mapping = aes(x=Popularity,y=MSRP, shape=TransmissionType))
ggplot(data= advcars) + geom_point(mapping = aes(x=Popularity,y=MSRP, color=NumberofDoors==2))
# adding random noise
ggplot(data= advcars) + geom_point(
  mapping = aes(x=HP,y=highwayMPG),
  position = "jitter"
)
ggplot(data= advcars) + geom_bar(mapping = aes(x=Driven_Wheels,fill=FuelType)) 
ggplot(data= advcars) + geom_bar(mapping = aes(x=TransmissionType,fill=Make)) 
ggplot(data= advcars) + geom_bar(mapping = aes(x=MarketCategory,y=..prop..,fill=Make)) 
ggplot(data= advcars, mapping = aes(x=Driven_Wheels,fill=FuelType))+geom_bar(alpha=1/5,position = "identity") 
ggplot(data= advcars) + geom_bar(mapping = aes(x=Driven_Wheels,fill=FuelType),position = "dodge") 

ggplot(data= advcars) + geom_histogram(mapping = aes(x=Popularity,fill=VehicleSize),binwidth = 1000) 
ggplot(data= advcars) + geom_histogram(mapping = aes(x=Popularity,fill=Driven_Wheels),binwidth = 500) 
ggplot(data= advcars) + geom_histogram(mapping = aes(x=MSRP,fill=Driven_Wheels),binwidth = 30000) 

ggplot(data= advcars,mapping = aes(x=highwayMPG , y=FuelType)) + geom_boxplot() #+ coord_flip()
#bar chart and a Coxcomb
bar <- ggplot(data= advcars) + geom_bar(mapping = aes(x=VehicleStyle,fill=VehicleStyle),
                                        show.legend = FALSE,
                                        width = 1
                                        ) + theme(aspect.ratio = 1) + labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()

ggplot(data = advcars) +
  stat_summary(
    mapping = aes(x=highwayMPG, y=FuelType),
    fun.ymin = min,
    fun.ymax = max, 
    fun.y = median
)


#< multiple geoms
ggplot(data= advcars,mapping = aes(x=HP,y=highwayMPG))+geom_smooth()+ geom_point()
ggplot(data= advcars,mapping = aes(x=HP,y=highwayMPG))+geom_smooth(data=filter(advcars,FuelType=="electric"))+ geom_point(mapping=aes(color=FuelType))
ggplot(data= advcars,mapping = aes(x=HP,y=highwayMPG))+geom_smooth(data=filter(advcars,FuelType=="natural gas"))+ geom_point(mapping=aes(color=FuelType))
ggplot(data= advcars,mapping = aes(x=HP,y=highwayMPG))+geom_smooth(data=filter(advcars,FuelType=="electric"))+ geom_point(mapping=aes(color=FuelType))
#<the outlair caused by electric fuel type
ggplot(data= advcars,mapping = aes(x=HP,y=highwayMPG))+geom_smooth(data=filter(advcars,Cylinders=="0"),se=FALSE)+ geom_point(mapping=aes(color=Cylinders))
#<the outlair caused by zero cylinders


