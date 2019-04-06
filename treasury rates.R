install.packages("quantmod")
install.packages("mondate")
install.packages("tidyr")
install.packages("dplyr")

library(quantmod)
library(ggplot2)
library(mondate)
library(tidyr)
library(dplyr)


tickers = c("DGS3MO","DGS6MO","DGS1","DGS2","DGS3","DGS5","DGS7","DGS10")
to=Sys.Date()
from = mondate(to)-12
getSymbols(tickers, src = "FRED", auto.assign = TRUE, from=from,to=to)

treasury <- merge(DGS3MO,DGS6MO,DGS1,DGS2,DGS3,DGS5,DGS7,DGS10)['2018-04-03::2019-04-03']

data = na.omit(treasury)

maturity <- c(3/12, 0.5, 1,2,3,5,7,10)
plot(maturity,data["2019-04-03"],main="US Treasury Constant Maturity Yield Curve", xlab="Maturity (yr)",
     ylab="Yield (%)", type="o",col="red",lty=1,ylim=c(2,4))

points(maturity,data['2019-01-03'], col="blue", pch="*")
lines(maturity,data['2019-01-03'], col="blue",lty=2)

points(maturity,data['2018-10-03'], col="dark red",pch="+")
lines(maturity,data['2018-10-03'], col="dark red", lty=3)

  legend(x="topright",legend=c("current","3-mo ago","6-mo ago"), col=c("red","blue","dark red"),
       pch=c("o","*","+"),lty=c(1,2,3), ncol=1)

plot(data$DGS3MO,main="3-month U.S. Treasury rates in the past year")
abline(h=mean(data$DGS3MO),col="blue")


df <- data.frame(
  index(data),
  coredata(data),
  stringsAsFactors=FALSE
)
colnames(df)[1]="date"
head(df)

df.tidy <- df %>%
  gather(key="maturities", value="rates", -date) 
df.tidy$maturities = as.factor(df.tidy$maturities)

up=ifelse(df$DGS10>df$DGS3MO,df$DGS10-df$DGS3MO,0)
down=ifelse(df$DGS10<df$DGS3MO,df$DGS10-df$DGS3MO,0)
  
ggplot((data=df.tidy),aes(x=date,y=rates,color=maturities))+
geom_line() 

ggplot(data=df,aes(x=date,y=DGS10-DGS3MO))+
geom_ribbon(aes(ymin=0,ymax=down),fill="#d73027",alpha=0.5)+
geom_ribbon(aes(ymin=up,ymax=0),fill="#4575b4",alpha=0.5)+  
  scale_color_manual(values=c("#d73027","#4575b4"),name="Yield Curve Slope ")+
geom_rug(aes(color=ifelse(DGS10>=DGS3MO,">=0","<0")),sides="b")+
  labs(x="",y="Difference in percentage points",title="Slope of Yield Curve: 10-year minus 3-month U.S. Treasury rates",
       subtitle="Difference in percentage points, daily")+
  geom_hline(yintercept=0,color="black") +
theme(legend.position="top",
      plot.caption=element_text(hjust=0),
      plot.subtitle=element_text(face="italic",size=9),
      plot.title=element_text(face="bold",size=14))
