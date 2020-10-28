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



def offtake_names():
    data = ['14','4']
    driver = webdriver.Firefox()
    driver.get("https://mip-prd-web.azurewebsites.net/DataItemExplorer")
    wait = WebDriverWait(driver,10)
    base_text = '/html/body/div[1]/div/div[2]/div[2]/div/div'
    end_text = '/span/span[1]'
    add_text = ''
    for i in range(len(data)):
        if i == len(data)-1:
            end_text = '/span/span[1]'
        add_text += '/ul/li['+data[i]+']' 
        complete_text = base_text + add_text + end_text 
        wait.until(ec.visibility_of_element_located\
            ((By.XPATH,complete_text))).click()
    i = 1
    offtakes = []
    while True:
            try:
                base_xpath = "/html/body/div[1]/div/div[2]/div[2]/div/div/ul/li[14]/ul/li[4]/ul/li["+str(i)+"]/span/span[4]"
                new_offtake = wait.until(ec.visibility_of_element_located\
                    ((By.XPATH,base_xpath))).text
                offtakes.append(new_offtake)
                i += 1
            except:
                break 
    j = 0
    for i in range(len(offtakes)):
        offtakes[i] = ','.join(offtakes[i].split(',',2)[1:])
        offtakes[i] = ','.join(offtakes[i].split(',')[:-1])
    return offtakes

offtakes = pd.DataFrame(offtake_names())
df = pd.read_csv(r'C:\Users\trs53\Dropbox\Documents\Cambridge\PopulatingTools\list.csv')
df = pd.concat([df,offtakes],ignore_index=True,axis=1)
df.to_csv('list.csv')

