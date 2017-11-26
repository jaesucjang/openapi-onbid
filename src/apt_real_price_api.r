## Sample 

http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=gfwgG1b5KAPvJxW7eJdsRtBg3487NmkTs5vZrIBNDxCMQ5Sfz9iWR6ORO8hqWiWlSvQhQsQimjOSarxpEJdz9A%3D%3D&LAWD_CD=11290&DEAL_YMD=201512

require(plyr)
install.packages("plyr")

library(plyr)
library(httr)
library(rvest)

air_data = NULL
i=1
for (i in 1:100000){
  url = paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/",
               "rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?",
               "&LAWD_CD=",11290,
               "&DEAL_YMD=",201512,
               "&serviceKey=",service_key)

  #write.csv(pblntfpclnd, paste0("pnu_result_data_",i,".csv"), row.names = T)

  a=read_html(url)
  xml_name(a)
  xml_children(a)
  xml_path(a)
  item_list=html_nodes(a, 'items item')
  b=html_text(item_list)

  library(stringr)
  
  #### Real ####
  
  r=data.frame()


  colnames(r)=c('거래금액','건축년도','년','법정동','아파트','월','일','전용면적','지번','지역코드','층','기타')
  
  
  i=1
  j=1
  for(i in 1:length(b)){
    for(j in 1:12){
      r[i,j]=str_split(b[i], '>')[[1]][j]
    }
  }
  

  i=1
  j=1
  
  for (i in 1:length(b)){
    for(j in 1:11){
      #a=colnames(r)[j]
      r[i,j]=gsub(colnames(r)[j],'',r[i,j])
    }
  }
  
  
  
