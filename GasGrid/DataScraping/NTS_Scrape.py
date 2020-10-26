from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as ec
import io
import os
import bs4 as bs 
import pandas as pd
import time
import re


def get_data(from_date,to_date,data):
    driver = webdriver.Firefox()
    driver.get("https://mip-prd-web.azurewebsites.net/DataItemExplorer")
    wait = WebDriverWait(driver,10)
    base_text = '/html/body/div[1]/div/div[2]/div[2]/div/div'
    end_text = '/span/span[1]'
    add_text = ''
    for i in range(len(data)):
        if i == len(data)-1:
            end_text = '/span/span[2]'
        add_text += '/ul/li['+data[i]+']' 
        complete_text = base_text + add_text + end_text 
        wait.until(ec.visibility_of_element_located\
            ((By.XPATH,complete_text))).click()
    wait.until(ec.element_to_be_clickable((By.ID,"applicableForRadioButton"))).click()
    wait.until(ec.element_to_be_clickable((By.ID,"FromDateTime"))).clear()
    wait.until(ec.element_to_be_clickable((By.ID,"FromDateTime"))).send_keys(from_date)
    wait.until(ec.element_to_be_clickable((By.ID,"ToDateTime"))).clear()
    wait.until(ec.element_to_be_clickable((By.ID,"ToDateTime"))).send_keys(to_date)
    wait.until(ec.visibility_of_element_located((By.ID,"viewReportButton"))).click()
    header = []
    for i in range(6):
       header.append(wait.until(ec.visibility_of_element_located\
           ((By.XPATH,"/html/body/div[1]/div[2]/table/thead/tr/th["+str(i+1)+"]"))).text)
    html = driver.page_source
    soup = bs.BeautifulSoup(html,'lxml')
    driver.quit()
    table = []
    for tr in soup.find_all('tr')[1:]:
        tds = tr.find_all('td')
        row = []
        for i in tds:
            row.append(i.text)
        table.append(row)
    table = pd.DataFrame(table)
    table.columns = header
    return table
'''
EXAMPLE CODE HERE FOR USE
'''
# actual_flows = ['14','4']
# comp_weather_var_actual = ['18','1','1']
# comp_weather_table = get_data('01/10/2020','20/10/2020',comp_weather_var_actual)
# print(comp_weather_table)

def instantaneous_flows():
    driver = webdriver.Firefox()
    driver.get("http://mip-prd-web.azurewebsites.net/InstantaneousView")
    wait = WebDriverWait(driver,10)
    return 

# instantaneous_flows()






