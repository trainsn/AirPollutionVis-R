library(igraph)
library(XML)

StartDate = "2017-03-06"
EndDate = "2017-04-04"

if(version$os=="mingw32")
{
     cityInclude = read.table("E:/Rstudy/PM25Plot-master/Province.txt",header = T)[,3]
}else
     load("E:/Rstudy/PM25Plot-master/cityInclude.RData")

aqi = read.csv(file = "E:/Rstudy/PM25Plot-master/aqi.csv", header = T)
numCity = 184
numDate = 24

cities = aqi[[1]]
aqidata = aqi[1:numDate+1]
pmMean = apply(data,1,mean)

city1 = NULL  
city2 = NULL
R2 = NULL
for (i in 1:(length(cities)-1)){
    cat(i,":")
    for (j in (i+1):length(cities)){
        city1 = c(city1, i)
        city2 = c(city2, j)
        aqi1 = as.vector(t(aqidata[i,])) 
        aqi2 = as.vector(t(aqidata[j,])) 
        R2 = c(R2,cor(aqi1,aqi2))
        cat(".")
    }
    cat("\n")
}

pmNetwork = function(cityInclude,cities,city1,city2,R2,pmMean,cutoff=0.5,StartDate,EndDate)
{
    cityInclude = gsub(pattern = "\\s",replacement = "",cityInclude)
    cityIncludeIndex = match(cityInclude,cities)
    cat("Delete cities",cityInclude[is.na(cityIncludeIndex)],"because of lack of data ...\n")
    cityInclude = cityInclude[!is.na(cityIncludeIndex)]
    cityIncludeIndex = cityIncludeIndex[!is.na(cityIncludeIndex)]
    
    city1Child = city1[city1 %in% cityIncludeIndex]
    city2Child = city2[city1 %in% cityIncludeIndex]
    R2Child = R2[city1 %in% cityIncludeIndex]
    city1Child = city1Child[city2Child %in% cityIncludeIndex]
    R2Child = R2Child[city2Child %in% cityIncludeIndex]
    city2Child = city2Child[city2Child %in% cityIncludeIndex]
    
    data = data.frame(city1=city1Child,city2=city2Child,R2=R2Child)
    data = data[complete.cases(data),]
    data = data[abs(data$R2)>=cutoff,]
    
    CitySet = unique(c(data[,1],data[,2]))
    CityLabelSet = as.character(cities)[CitySet]
    CityMean = pmMean[CitySet]
    data$city1 = match(data$city1,CitySet)
    data$city2 = match(data$city2,CitySet)
    #cat (class(data))
    if(nrow(data)<2)
        return(data)
    g = graph(as.vector(rbind(data$city1,data$city2)),directed = T)
    direction = ifelse(data$R2>0,1,2)
    data$R2 = abs(data$R2)
    lwd = (data$R2 - min(data$R2))/(max(data$R2)-min(data$R2)) * 4 + 4
    E(g)$arrow.mode = 0
    E(g)$width = lwd
    E(g)$color = heat.colors(10)[10-floor(data$R2*10)]
    E(g)$label = round(data$R2,digits = 2)
    E(g)$label.color = "slategrey"
    E(g)$label.size = 0.8
    
    colorSeq = seq(0,1,0.1)
    col = rgb(red = 1,green = 1-colorSeq,blue = 0)
    V(g)$color = col[ceiling(CityMean/30)]
    V(g)$frame.color = "white"
    V(g)$label = CityLabelSet
    V(g)$label.color = "black"
    plot(g,layout = layout.fruchterman.reingold)
    
    title(main = paste("全国省会城市之间aqi相关系数网络关系图 R2>=",cutoff,sep=""),
          sub=paste("收据收集时间段",StartDate,"至",EndDate," @conda",sep=""))
    g
}

jpeg("pm2.5Network.%d.jpeg",width = 1000,height = 1000,quality = 100)
g = pmNetwork(cityInclude,cities,city1,city2,R2,pmMean,cutoff=0,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,pmMean,cutoff=0.1,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,pmMean,cutoff=0.2,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,pmMean,cutoff=0.3,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,pmMean,cutoff=0.4,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,pmMean,cutoff=0.5,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,pmMean,cutoff=0.6,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,pmMean,cutoff=0.7,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,pmMean,cutoff=0.8,StartDate,EndDate)
dev.off()
  