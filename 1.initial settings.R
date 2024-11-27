# directory <- "D:/shibofeng/SMA NSGA-batch/loop initial/"
# directory <- "D:/DATA/SMA NSGA-batch/loop initial/"
# directory <- "D:/DATA/Cu-MOO/Ms-HMA/"
directory <- paste(getwd(),"/",sep="")
setwd(directory)

source("0.library.R")
source("2.1.functions for fs and ml.R")
source("acquisition function.R")

## set file path
data.file <- paste(directory,"Cu-SMA data.csv",sep = "") 
feature.file <- paste(directory,"elefeature.csv",sep = "")
H.file <- paste(directory,"H.csv",sep = "")

write.csv(data.Ms,"data.Ms.csv")
write.csv(data.HMA,"data.HMA.csv")



data_column <- data.HMA$HMA  # 生成一些正态分布的随机数据作为示例

# 计算均值和标准差
mean_value <- mean(data_column)
sd_value <- sd(data_column)

# 绘制频率分布直方图
hist(data_column, freq = FALSE, col = 'lightblue', border = 'blue', main = '频率分布直方图与正态分布拟合', xlab = '数据值', ylab = '密度')

# 添加正态分布曲线
curve(dnorm(x, mean = mean_value, sd = sd_value), add = TRUE, col = 'red')




## elements
data.elements <- c("Cu","Al","Mn","Ni","Co","Zn","Si","Be","Hf","Fe","Ti",
                   "V","Mg","Cr","Ag","Nb","Zr")
data.prop <- c("Ms","Mp","Mf","As","Ap","Af","HMA","HAM")
data.proc <- c("Crlrat","Ttemp1","Ttime1","Ttemp2","Ttime2")

vir.elements<-c("Cu","Al","Mn","Ni","Co","Zn","Si","Be","Hf","Fe","Ti",
                "V","Mg","Cr","Ag","Nb","Zr")
## raw data and features of elements
## 154 data
rawdata <- read.csv(data.file)
# rawdata <- rawdata[which(rawdata$Mn+rawdata$Ni!=0),]
# 
# rawdata <- rawdata[which(rowSums(rawdata[,5:17])==0),]
rawdata <- rawdata[!duplicated(rawdata[,1:17]),]
rawdata <- rawdata[!is.na(rawdata$Ms),]

# summary(rawdata)
rawdata <- rawdata[which(rawdata$Ms>=200&rawdata$Ms<=600),]
rawdata <- rawdata[which(rawdata$Ttime2==0),]

rawdata.aval <- rawdata[!is.na(rawdata$HMA),]
rawdata.aval <- rawdata.aval[which(rawdata.aval$Ms>=333&rawdata.aval$Ms<=353),]
write.csv(rawdata.aval,"raw.available.csv")

library(VIM)
library(mice)
rawdata1 <- rawdata[order(rawdata$Ms,decreasing = F),]
matrixplot(rawdata1[,c("Ms","HMA")])


write.csv(rawdata,"rawdata.csv")



p1 <- ggplot(rawdata, aes(x = Ms)) +
  geom_histogram(binwidth = 20, fill = "grey", color = "black") +
  geom_vline(xintercept = c(333, 353), col = "red", linetype = "dashed") +
  geom_rect(aes(xmin = 333, xmax = 353, ymin = 0, ymax = Inf),
            fill = "red", alpha = 0.2) +
  labs(title = "Distribution of Ms", x = "Ms", y = "Frequency")

# 计算 HMA 的缺失值数量
missing_count <- sum(is.na(rawdata$HMA))
missing_data <- data.frame(Feature = "HMA", MissingValues = missing_count)

# 创建一个仅包含缺失值数量的条形图
p2 <- ggplot(missing_data, aes(x = Feature, y = MissingValues, fill = Feature)) +
  geom_bar(stat = "identity") +
  labs(title = "Missing Values in HMA", x = "", y = "Number of Missing Values")

# 组合两个图表
grid_plot <- gridExtra::grid.arrange(p1, p2, nrow = 2)



library(ggplot2)

library(ggplot2)

# 假设你的数据已经被读入名为 rawdata 的数据框中
# rawdata <- read.csv("path/to/your/file.csv")

# 使用 ggplot2 绘制散点图
p <- ggplot(rawdata, aes(x = Ms, y = HMA)) +
  geom_point(aes(color = HMA), size = 3, alpha = 0.6) +  # 绘制散点
  scale_color_gradient(low = "blue", high = "red") +     # 设置颜色渐变
  geom_rect(aes(xmin = 333, xmax = 353, ymin = min(rawdata$HMA, na.rm = TRUE), ymax = max(rawdata$HMA, na.rm = TRUE)),
            fill = "yellow", alpha = 0.2) +              # 高亮333到353范围
  labs(title = "Matrix Plot of Ms vs. HMA",
       x = "Ms",
       y = "HMA",
       color = "HMA Value") +
  theme_minimal()

# 显示图形
print(p)





# which(!is.na(rawdata$HMA))

# rawdata <- rawdata[which(rawdata$Ms>=330&rawdata$Ms<=370),]
# colnames(rawdata)
# rawdata <- rawdata[,-ncol(rawdata)]
# rawdata <- as.numeric(rawdata[-1,])
# rawdata <- data.frame(ID=c(1:nrow(rawdata)),rawdata[,c(data.elements)],rawdata[,c("hysteresis","enthalpy")])
# sum(rowSums(rawdata[,2:11])!=100)

features <- read.csv(feature.file)
rownames(features) <- features[,1]
features <- features[,c(-1)]

# data.at <- data.frame(matrix(nrow = nrow(rawdata),ncol = length(data.elements)))
fn.at <- function(data,feature){
  
  mole <- t(t(data)/feature)
  at <- mole/rowSums(mole)*100
  
  return(at)
}
data.at <- fn.at(rawdata[data.elements],features[data.elements,"amass"])


# colnames(data.at) <- data.elements


# summary(rawdata)


## H of elements
H <- read.csv(H.file)
H <- H[,-1]
H[1,1]<-0
row.names(H) <- H[,1]
H <- H[,-1]
colnames(H) <- H[1,]
H <- H[-1,]


raw.frequency <- rbind(max=rep(1,times=length(data.elements),each=1),
                       min=rep(0,times=length(data.elements),each=1),
                       (nrow(rawdata)-colSums(rawdata[,data.elements]==0))/nrow(rawdata))
raw.frequency <- data.frame(raw.frequency)


vir.range <- rbind(max=rep(100,times=length(vir.elements),each=1),
                   min=rep(0,times=length(vir.elements),each=1),
                   apply(vir[,vir.elements], 2, max))
vir.range <- data.frame(vir.range)

radarchart(raw.frequency,
           axistype = 1,
           # Customize the polygon
           pcol = "#00AFBB", pfcol = scales::alpha("#00AFBB", 0.3), plwd = 2, plty = 2,
           # Customize the grid
           cglcol = "gray", cglty = 1, cglwd = 0.8,
           # Customize the axis
           axislabcol = "black", 
           # Variable labels
           vlcex = 1.3, vlabels = colnames(raw.frequency),caxislabels = c("0%", "25%", "50%", "75%", "100%"))



legend(x=0.8,y=1.3,legend=c("Frequency"),text.font = 2,
       pch=16, col="#00AFBB",pt.cex = 1.5,bty = "n")




boxplot(rawdata$Ms,xlim=c(0.6,1.4),ylim=c(100,600),col="lightgray",
        cex=2,lwd=3,outcol="red",pch=21,bg=rgb(255,0,0,100,max=255),
        cex.lab=1.2,tcl=0.4,ann=F,yaxt="n")
title(main="Ms",line=0.5)
axis(side=2,lwd.ticks=3,tick=T,at = seq(0,900,100),mgp=c(1,0.5,0),las=1,tck=0.04,cex.axis=1.2,font=2,font.lab=2)
box(lwd=3)

stripchart(list(rawdata$Ms),vertical = T,method = "overplot",cex = 1.5,
           pch = 20,col = "#0099ff",add=T)


boxplot(rawdata$As,xlim=c(0.6,1.4),ylim=c(0,900),col="lightgray",
        cex=2,lwd=3,outcol="red",pch=21,bg=rgb(255,0,0,100,max=255),
        cex.lab=1.2,tcl=0.4,ann=F,yaxt="n")
title(main="As",line=0.5)
axis(side=2,lwd.ticks=3,tick=T,at = seq(0,900,100),mgp=c(1,0.5,0),las=1,tck=0.04,cex.axis=1.2,font=2,font.lab=2)
box(lwd=3)

stripchart(list(rawdata$As),vertical = T,method = "overplot",cex = 1.5,
           pch = 20,col = "#0066df",add=T)


boxplot(rawdata$HMA,xlim=c(0.6,1.4),ylim=c(0,12),col="lightgray",
        cex=3,lwd=3,outcol="red",pch=21,bg=rgb(255,0,0,100,max=255),
        cex.lab=1.2,tcl=0.4,ann=F,yaxt="n")
title(main="HMA",line=0.5)
axis(side=2,lwd.ticks=3,tick=T,at = seq(0,12,3),mgp=c(1,0.5,0),las=1,tck=0.04,cex.axis=1.2,font=2,font.lab=2)
box(lwd=3)

stripchart(list(rawdata$HMA),vertical = T,method = "overplot",cex = 1.5,
           pch = 20,col = "#0033df",add=T)

# stripchart(list(rawdata$HMA),vertical = T,method = "jitter",cex = 1,
#            pch = 20,col = "#6666cc",add=T)

