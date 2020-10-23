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

driver = webdriver.Firefox()
driver.get("https://mip-prd-web.azurewebsites.net/DataItemExplorer")

def get_actual_offtake_flows(from_date,to_date):
    wait = WebDriverWait(driver,10)
    reports_element = wait.until(ec.visibility_of_element_located\
        ((By.XPATH,"/html/body/div[1]/div/div[2]/div[2]/div/div/ul/li[14]/span/span[1]")))
    reports_element.click()
    offtake_element = wait.until(ec.visibility_of_element_located\
        ((By.XPATH,"/html/body/div[1]/div/div[2]/div[2]/div/div/ul/li[14]/ul/li[4]/span/span[2]")))
    offtake_element.click()
    wait.until(ec.element_to_be_clickable((By.ID,"applicableForRadioButton"))).click()
    wait.until(ec.element_to_be_clickable((By.ID,"FromDateTime"))).clear()
    wait.until(ec.element_to_be_clickable((By.ID,"FromDateTime"))).send_keys(from_date)
    wait.until(ec.element_to_be_clickable((By.ID,"ToDateTime"))).clear()
    wait.until(ec.element_to_be_clickable((By.ID,"ToDateTime"))).send_keys(to_date)
    csv = wait.until(ec.visibility_of_element_located((By.ID,"viewReportButton"))).click()
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

offtake_flows = get_actual_offtake_flows('01/10/2020','03/10/2020')
print(offtake_flows['Data Item'])








# for i in range(1,19):
#     element = driver.find_elements_by_xpath\
#         ("/html/body/div[1]/div/div[2]/div[2]/div/div/ul/li["+str(i)+"]/span/span[1]")[0]
#     element.click()
#     element_text = driver.find_elements_by_xpath\
#         ("/html/body/div[1]/div/div[2]/div[2]/div/div/ul/li["+str(i)+"]/span/span[4]")[0]
#     print(element_text.get_attribute("innerHTML"))