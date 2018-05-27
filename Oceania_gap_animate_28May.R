###########################################################################################################
##This R code creates an animated plot of Life Expectancy vs. GDP with bubble size baased on U5 mort rate
###########################################################################################################

library(ggplot2) #ggplot2 package is used to plot the scatter plot
library(magick)
library(readr) #to read the data from the CSV
combined <- read_csv("~/combined.csv") #combined.csv created used python wrangling of gapminder data

img <- image_graph(res = 96)
datalist <- split(combined, combined$Year)
out <- lapply(datalist, function(data){
  p <- ggplot(data, aes(GDP_PPP, LifeExp, size = Mortality_U5, color = geo.name)) +
    scale_size("Mortality_U5", limits = range(combined$Mortality_U5)) +
    scale_x_log10(limits = c(1e+03,0.5e+06)) + # approachnot used - "limits = range(combined$GDP_PPP)"
    geom_text(aes(x=GDP_PPP,y=LifeExp,label = geo.name),size=3.1, color = "black", hjust=-0.12, vjust=0.5) +
    geom_point(alpha=0.65) + ylim(55, 90) + theme(plot.title = element_text(hjust=0.5)) + ggtitle(paste("Year", data$Year, "     Oceania Life Expectancy vs. GDP (Animation covers 2000-2030)")) + theme_classic() +
    labs(y = "Life Expectancy (Years)", x = "GDP per capita")
    
  print(p)
})
dev.off()
animation <- image_animate(img, fps = 5)
image_write(animation, "oceania_u5.gif")

###Shows the header of the data for the datalist used to create the snapshot images in the animation
head(datalist)#so if I can get the data into the same format then I can use it for this animation in R
