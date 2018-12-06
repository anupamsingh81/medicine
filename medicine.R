history= "medicine.txt"
chat <- readr::read_lines(history, locale = readr::locale(encoding = "UTF-8"))

chat[4]
library(lubridate)

###helper functions

# time extraction

timex= function(x){
  
  raw.time  <- stringr::str_split_fixed(x, "- ", n=2)[1]
  
 time= tibble(time= dmy_hm(raw.time))
time }


# content extraction
content = function(x){
  raw.data1 <- stringr::str_split_fixed(x, ": ", n=2)[2]
  
  if(raw.data1=="")(raw.data1=x)
  raw.data1}

# author extraction

authorize= function(x){
  gsub(".*- |:.*", "", x)
}

#run check
stringr::str_split_fixed(chat[1], "- ", n=2)[1]

##df

moment = map_df(chat,timex)

time = as.data.frame(moment) %>% gather()

moment$content=map_chr(chat,content)

moment=moment %>% fill(time)

opd=moment %>% filter(str_detect(content,"Today's OPD|Todays Opd")) %>% mutate(opd=abs(parse_number(content))) %>% select(-content)
admission=moment %>% filter(str_detect(content,"Today's Adm|Todays Adm")) %>% mutate(admission=abs(parse_number(content)))%>% select(-content)

#Present IPD 

ipd=moment %>% filter(str_detect(content,"Present IPD")) %>% mutate(ipd=abs(parse_number(content)))%>% select(-content)


#Todays Dis -1

discharge=moment %>% filter(str_detect(content,"Today's Dis|Todays Dis")) %>% mutate(discharge=abs(parse_number(content)))%>% select(-content)

opd %>% left_join(admission) %>%  left_join(ipd) %>% left_join(discharge) -> census

census %>% mutate(conversion_ratio= round(100*admission/opd,2) ,
                    weekday=wday(time,label = TRUE)) -> census





#%>% 
  #View()

##plots
census %>%   ggplot(aes(y=opd,x=weekday,group=1))+geom_point()+geom_smooth()

census %>%   ggplot(aes(y=admission,x=weekday,group=1))+geom_point()+geom_smooth()

census %>%   ggplot(aes(y=discharge,x=weekday,group=1))+geom_point()+geom_smooth()

census %>%   ggplot(aes(y=conversion_ratio,x=weekday,group=1))+geom_point()+geom_smooth()



census %>% gather(metric,value,opd:conversion_ratio) %>% mutate(time=as.Date(time)) %>%  #as.Date important .for scale_x_date
  ggplot(aes(x=time,y=value,color=metric,group=metric))+geom_smooth(se=FALSE) +scale_x_date(date_breaks = "1 week")+
  scale_y_continuous(breaks=seq(0,80,by=5))+theme_light()

census %>% gather(metric,value,opd:conversion_ratio) %>% 
  ggplot(aes(x=weekday,y=value,color=metric,group=metric))+geom_smooth()


census %>% gather(metric,value,opd:conversion_ratio) %>% group_by(metric) %>% summarise(mean=mean(value),median=median(value),sd=sd(value))

##calculate summary statistics all#######


namer= names(summary(census$opd))

census %>% select_if(is.numeric) %>% map_df(~summary(.)) %>% add_column(summary=namer) %>% 
  select(summary,everything()) 


namer
