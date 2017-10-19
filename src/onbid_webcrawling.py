##################################################################
# Author.............: JJS
# File name..........: webcrawling.py
# Written Date.......: 2017.10.18
# Program Description: Onbid Site search 
##################################################################


from selenium import webdriver
from bs4 import BeautifulSoup
import pandas as pd
from pandas import DataFrame
import pandas as pd
from pandas import DataFrame
import re


driver = webdriver.Chrome()
driver.implicitly_wait(3)

#driver.get('http://www.onbid.co.kr/op/cta/cltrdtl/collateralDetailMoveableAssetsList.do')
driver.get('http://www.onbid.co.kr/op/bda/bidrslt/collateralRealEstateBidResultList.do?q=3BBA7FC2189F003D0A021CFCED9D4141BF41F40EFAF43D&charset=UTF-8')

DataFrame = []

import csv
matrix = []
f = open('C:/Users/User/Downloads/공공데이타분석_장재석/M_number_2.csv', 'r')
f1 = open('C:/Users/User/Downloads/공공데이타분석_장재석/M_number_result.csv','w')

csvReader = csv.reader(f)
for row in csvReader:
    matrix.append(row)
f.close()

# 9508 
i=0
for i in range(0,9508):
	nmid=matrix[i]
	elem=driver.find_element_by_name('searchCltrMnmtNo')
	elem.clear()
	elem.send_keys(nmid)
	elem.submit()
	html = driver.page_source
	soup = BeautifulSoup(html, 'html.parser')
	tbl1 = soup.find("table", {"class" : "op_tbl_type1"})
	new_table = pd.DataFrame(columns=range(0,7), index =range(0,20)) # I know the size 

	row_marker = 0
	for row in tbl1.find_all('tr'):
		column_marker = 0
		row_marker += 1
		columns = row.find_all('td')
		for column in columns:
        
			#print(column.get_text())
			temp=column.get_text()
			#print(type(temp))

			nospace = re.sub('&nbsp;| |\t|\r|\n', '', temp)
			replace_temp2=nospace
			replace_temp= temp.replace(" ","")
			new_table.iat[row_marker,column_marker] = replace_temp2
			column_marker += 1
    
	new_table=new_table.dropna()
	rows_temp = zip(new_table[0],new_table[1],new_table[2],new_table[3],new_table[4],new_table[5],new_table[6])
	
	csvWriter = csv.writer(f1)   
	for row in rows_temp:
		csvWriter.writerow(row)

f1.close()



'''
row_marker = 0
for row in tbl1.find_all('tr'):
    column_marker = 0
    columns = row.find_all('td')
    for column in columns:
        
        #print(column.get_text())
        temp=column.get_text()
        #print(type(temp))

        nospace = re.sub('&nbsp;| |\t|\r|\n', '', temp)
        replace_temp2=nospace
        replace_temp2
        replace_temp= temp.replace(" ","")
        #replace_temp2=nospace.splitlines()
        print(replace_temp2)
        print(type(replace_temp2))
        print(len(replace_temp2))
        #replace_temp4= replace_temp4.replace(" ","")
        new_table.iat[row_marker,column_marker] = replace_temp2
        
        column_marker += 1
    
new_table
'''