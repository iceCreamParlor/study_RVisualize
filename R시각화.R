library(ggmap)

library(stringr)

#공영주차장1

loc<-read.csv("서울_강동구_공영주차장_위경도.csv",header=T)

kd<-get_map("Amsa-dong",zoom=13,maptype = "roadmap")

kor.map<-ggmap(kd)+geom_point(data=loc, aes(x=LON, y=LAT),
                              
                              size=3,alpha=0.9,color="red")

kor.map+geom_text(data=loc, aes(x=LON, y=LAT+0.001, label=주차장명),
                  
                  size=3)

ggsave("c:\\r_temp/kd.png",dpi=500)



#공영주차장2

ggmap(kd)

loc2<-str_sub(loc$주차장명,start=-2,end=-2)

loc2

colors<-c()

for (i in 1:length(loc2)){
  
  if (loc2[i]=='구'){
    
    colors<-c(colors,'red')}
  
  else{
    
    colors<-c(colors,'blue')}
  
}

kd<-get_map("Amsa-dong",zoom=13,maptype = "roadmap")

kor.map<-ggmap(kd)+geom_point(data=loc, aes(x=LON, y=LAT),
                              
                              size=3,alpha=0.7,color=colors)

kor.map+geom_text(data=loc, aes(x=LON, y=LAT+0.001, label=주차장명),
                  
                  size=3)



#장애인 도서관

loc<-read.csv("지역별장애인도서관정보.csv",header=T)

loc

kor<-get_map("seoul",zoom=11,maptype="roadmap")

kor.map<-ggmap(kor)+geom_point(data=loc,aes(x=LON, y=LAT),size=5,alpha=0.7)

kor.map+geom_text(data=loc,aes(x=LON,y=LAT+0.01,label=자치구명),size=3)



#인구데이터 버블차트

librRY(grid)

library(grid)

pop<-read.csv("지역별인구현황_2014_4월기준.csv",header=T)

pop

lon<-pop$LON

lat<-pop$LAT

data<-pop$총인구수

df<-data.frame(lon,lat,data)

df

map1<-get_map("Jeonju",zoom=7,maptype='roadmap')

map1<-ggmap(map1)

map1+geom_point(aes(x=lon,y=lat,colour=data,fill=factor(data),size=data),data=df)

ggsave("c:\\r_temp/pop5.png",scale=1,width=7,height=4,dpi=1000)



#서울시구청 위치 정보

gu<-read.csv("서울시구청위치정보_new.csv",header=T)

map<-get_map("seoul",zoom=11,maptype="roadmap")

map.map<-ggmap(map)+geom_point(data=gu,aes(x=LON, y=LAT),size=5,alpha=0.7,color="green")

map.map+geom_text(data=gu,aes(x=LON,y=LAT+0.01,label=name),size=3)

ggsave("c:\\r_temp/구청위치.png",dpi=500)



#2호선 위치 정보

ln2<-read.csv("서울지하철2호선위경도정보.csv",header=T)

met<-get_map("seoul",zoom=11,maptype="roadmap")

met.map<-ggmap(met)+geom_point(data=ln2,aes(x=LON, y=LAT),size=3,alpha=0.9,color="blue")

met.map+geom_text(data=ln2,aes(x=LON,y=LAT+0.01,label=역명),size=3)

ggsave("c:\\r_temp/2호선.png",dpi=500)



#장난감 도서관

toy<-read.csv("서울시장난감도서관위치현황.csv",header=T)

libb<-get_map("seoul",zoom=11,maptype="roadmap")

libb.map<-ggmap(libb)+geom_point(data=toy,aes(x=LON, y=LAT),size=3,alpha=0.9,color="black")

libb.map+geom_text(data=toy,aes(x=LON,y=LAT+0.01,label=이름),size=3)

ggsave("c:\\r_temp/장난.png",dpi=500)



#장난감+장애인 도서관

toy<-read.csv("서울시장난감도서관위치현황.csv",header=T)

pa<-read.csv("지역별장애인도서관정보.csv",header=T)

libb<-get_map("seoul",zoom=11,maptype="roadmap")

libb.map<-ggmap(libb)+geom_point(data=toy,aes(x=LON, y=LAT),size=3,alpha=0.9,color="black")+
  
  geom_point(data=pa,aes(x=LON, y=LAT),size=3,alpha=0.9,color="green")

libb.map+geom_text(data=toy,aes(x=LON,y=LAT+0.01,label=이름),size=3)+
  
  geom_text(data=pa,aes(x=LON,y=LAT+0.01,label=도서관명),size=3)

ggsave("c:\\r_temp/장난+도서.png",dpi=500)



#2호선+3호선

ln2<-read.csv("서울지하철2호선위경도정보.csv",header=T)

ln3<-read.csv("서울지하철3호선역위경도정보.csv",header=T)

met<-get_map("seoul",zoom=11,maptype="roadmap")

met.map<-ggmap(met)+geom_point(data=ln2,aes(x=LON, y=LAT),size=3,alpha=0.9,color="green")+
  
  geom_point(data=ln3,aes(x=LON, y=LAT),size=3,alpha=0.9,color="red")

met.map+geom_text(data=ln2,aes(x=LON,y=LAT+0.01,label=역명),size=3)+
  
  geom_text(data=ln3,aes(x=LON,y=LAT+0.01,label=역명),size=3)

ggsave("c:\\r_temp/2호선+3호선.png",dpi=500)



#경로연결

library(ggplot2)

jeju<-read.csv("제주도여행코스_1일차.csv",header=T)

jeju1<-get_map("Hallasan",zoom=10,maptype="hybrid")

jeju.map<-ggmap(jeju1)+geom_point(data=jeju,
                                  
                                  aes(x=LON,y=LAT),size=3,alpha=0.7,col="red")

jeju.map+geom_path(data=jeju,aes(x=LON,y=LAT),size=1,linetype=8,
                   
                   col="yellow")+
  
  geom_text(data=jeju,aes(x=LON,y=LAT+0.01,label=장소),size=3)

ggsave("c:\\r_temp/jeju_1.png",dpi=700)



#3호선 경로

ln3<-read.csv("서울지하철3호선역위경도정보.csv",header=T)

met<-get_map("seodaemun_gu",zoom=11,maptype="roadmap")

met.map<-ggmap(met)+geom_point(data=ln3,aes(x=LON, y=LAT),size=3,alpha=0.9,color="red")

met.map+geom_text(data=ln3,aes(x=LON,y=LAT+0.01,label=역명),size=3)+
  
  geom_path(data=ln3,aes(x=LON,y=LAT),size=1,linetype=8,col="green")

ggsave("c:\\r_temp/3호선경로.png",dpi=500)



#서울 관광지

se<-read.csv("서울관광명소.csv",header=T)

pla<-get_map("seoul",zoom=12,maptype="roadmap")

pla.map<-ggmap(pla)+geom_point(data=se,aes(x=LON, y=LAT),size=3,alpha=0.9,color="red")

pla.map+geom_text(data=se,aes(x=LON,y=LAT+0.01,label=이름),size=3)+
  
  geom_path(data=se,aes(x=LON,y=LAT),size=1,linetype=8,col="blue")

ggsave("c:\\r_temp/서울관광지도.png",dpi=500)



#인천 관광지

inc<-read.csv("인천 관광지.csv",header=T)

pla<-get_map("incheon",zoom=12,maptype="roadmap")

pla.map<-ggmap(pla)+geom_point(data=inc,aes(x=LON, y=LAT),size=3,alpha=0.9,color="red")

pla.map+geom_text(data=inc,aes(x=LON,y=LAT+0.01,label=이름),size=3)+
  
  geom_path(data=inc,aes(x=LON,y=LAT),size=1,linetype=8,col="blue")

ggsave("c:\\r_temp/인천관광지도.png",dpi=500)