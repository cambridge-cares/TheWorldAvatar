from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as ec
import io
from tabulate import tabulate
import datetime
import os
import numpy as np 
import bs4 as bs 
import pandas as pd
from tqdm import tqdm
import time
import re


def get_data(from_date,to_date,data):
    '''
    DESCRIPTION:
    get_data will obtain data between two defined dates from the national grid's online database.
    Taking advantage of the Selenium library for interaction with webpages and Beautiful Soup for 
    the extraction of html tables

    INPUTS:
    from_date:      A date that the start of data to be obtained is required in the form 'DD,MM,YYYY'
    to_date:        A date that the end of data to be obtained is required in the form 'DD,MM,YYYY'
    data:           An array containing values of consecutuve folder numbers to click on/obtain data from

    OUTPUTS:
    table:          A pandas dataframe containing all information specified 

    NOTES:
    the 'data' array is currently the least 'flexible' aspect of this code, for example it is obtained 
    through manual interaction with the online database, however once obtained for the data needed
    it can be stored and reused. For now is relatively fine as only certain information is required from 
    the database.
    '''

    #--- opening the database in a firefox window ---#
    driver = webdriver.Firefox() 
    driver.get("https://mip-prd-web.azurewebsites.net/DataItemExplorer")
    wait = WebDriverWait(driver,10)
    #--- clicking through the file tree ---#
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
    #--- inserting dates required ---#
    wait.until(ec.element_to_be_clickable((By.ID,"applicableForRadioButton"))).click()
    wait.until(ec.element_to_be_clickable((By.ID,"FromDateTime"))).clear()
    wait.until(ec.element_to_be_clickable((By.ID,"FromDateTime"))).send_keys(from_date)
    wait.until(ec.element_to_be_clickable((By.ID,"ToDateTime"))).clear()
    wait.until(ec.element_to_be_clickable((By.ID,"ToDateTime"))).send_keys(to_date)
    wait.until(ec.visibility_of_element_located((By.ID,"viewReportButton"))).click()
    #--- creation of pandas dataframe ---#
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
EXAMPLE CODE FOR get_data
'''
# actual_flows = ['14','4']
# comp_weather_var_actual = ['18','1','1']
# comp_weather_table = get_data('01/10/2020','20/10/2020',comp_weather_var_actual)
# print(comp_weather_table)



def real_time_intakes():
    '''
    DESCRIPTION:
    Calls the National Grid online publication of incoming flows to the NTS and produces
    two numpy tables, one with zonal intakes and one with terminal intakes.
    Units are mcm/day.
    '''
    #--- opening intakes webpage ---#
    os.system('cls' if os.name == 'nt' else 'clear')
    while True:
        driver = webdriver.Firefox()
        try:
            driver.get("https://mip-prd-web.azurewebsites.net/InstantaneousView")
        except:
            print('ONLINE DATABASE UNAVAILABLE, CHECK NETWORK CONNECTION')
            break 
        wait = WebDriverWait(driver,10)
        html = driver.page_source

        #--- converting all the information to a table ---#
        #--- as all data presented in a large html table ---#
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
        #--- ontaining only the required values ---#
        table = table.to_numpy()[4:,1:]
        zone_names = table[1:29,0]
        latest_zone_value = table[1:29,6]
        latest_zone_value[6] = latest_zone_value[6][1:]
        latest_zone_value = latest_zone_value.astype(np.float)
        zone_supply = np.concatenate(([zone_names],[latest_zone_value]),axis=0).T

        zone_supply_pd = pd.DataFrame(zone_supply)

        terminal_names = table[47:59,0]
        latest_terminal_value = table[47:59,6].astype(np.float)
        terminal_supply = np.concatenate(([terminal_names],[latest_terminal_value]),axis=0).T

        terminal_supply_pd = pd.DataFrame(terminal_supply)

        overall_df = pd.concat((zone_supply_pd,terminal_supply_pd),axis=1,ignore_index=True)

        header = ['Zone Supply','Instananeous Flow (mcm/day)','Terminal Supply','Instananeous Flow (mcm/day)']
        print(tabulate(overall_df,headers=header,showindex="never"))
        for i in reversed(range(120)):
            print('TIME FOR NEXT UPDATE: ',i,' SECONDS',end='\r')
            time.sleep(1)
        os.system('cls' if os.name == 'nt' else 'clear')
    return 



def terminal_intakes():
    driver = webdriver.Firefox()
    try:
        driver.get("https://mip-prd-web.azurewebsites.net/InstantaneousView")
    except:
        print('ONLINE DATABASE UNAVAILABLE, CHECK NETWORK CONNECTION')
    wait = WebDriverWait(driver,10)
    html = driver.page_source

    #--- converting all the information to a table ---#
    #--- as all data presented in a large html table ---#
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
    #--- ontaining only the required values ---#
    table = table.to_numpy()[4:,1:]
    time = table[0,1:7]
    dt = datetime.datetime.now()
    datetime_addition = str(dt.year)+'-'+str(dt.month)+'-'+str(dt.day)+' '
    for i in range(len(time)):
        time[i] = datetime_addition+time[i]+':00'
    time = np.insert(time,0,'time')
    terminal_names = table[43:52,0]
    terminal_values = table[43:52,1:7]
    for i in range(len(terminal_values[:,0])):
        for j in range(len(terminal_values[0,:])):
            if list(terminal_values[i,j])[0] == 'i':
                string = ''.join(list(terminal_values[i,j])[1:])
                terminal_values[i,j] = string 
                
    terminal_values = terminal_values.astype(float)
    terminal_supply = np.concatenate((np.array([terminal_names]).T,terminal_values),axis=1)
    terminal_supply = np.insert(terminal_supply,0,[time],axis=0)
    return terminal_supply