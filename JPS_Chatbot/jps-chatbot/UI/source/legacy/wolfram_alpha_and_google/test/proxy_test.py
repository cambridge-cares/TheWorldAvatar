import re
import time

from selenium import webdriver
from selenium.common.exceptions import UnexpectedAlertPresentException
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.common.proxy import Proxy, ProxyType
from selenium.webdriver.support.expected_conditions import alert_is_present

from bs4 import BeautifulSoup


# what is the heat capacity of h2so4


class Proxy:

    def __init__(self):
        self.options = Options()
        self.options.add_argument('--headless')
        self.options.add_argument('--disable-logging')
        address = '185.141.58.109:19596'
        webdriver.DesiredCapabilities.FIREFOX['proxy'] = {
            "httpProxy": address,
            "sslProxy": address,
            "proxyType": "MANUAL"
        }

        self.driver = webdriver.Firefox(options=self.options)
        self.url_template = 'https://www.google.com/search?q=what is the weight of ch4'

    # due to possible restriction from google, we implement a proxy to enable robust request to google.
    def make_proxy_requst(self):
        start_time = time.time()
        ip_list = ['185.141.58.109:19596']
        ip = ip_list.pop()
        # prox = Proxy()
        # prox.proxy_type = ProxyType.MANUAL
        # prox.http_proxy = ip

        print('Sending request...')
        
        self.driver.get("http://www.whatsmyip.org/")
        
        html_source="None"
        try:
            html_source = self.driver.find_element_by_tag_name('html').find_element_by_id('ip').get_attribute('innerHTML')
        except UnexpectedAlertPresentException as ex:
                print("***Request caused an alert [%s]***" % ex.__dict__["msg"])
        print(' Response: ' + html_source)
        end_time = time.time()
        print('Time elapsed: %f s' % (end_time - start_time))



    def make_request(self, question):
        url = self.make_url(question)
        self.options = Options()
        self.options.add_argument('--headless')
        self.options.add_argument('--disable-logging')
        self.driver = webdriver.Firefox(options=self.options)
        # self.driver = webdriver.Firefox(options=self.options)
        try:
            self.driver.get(url)
            html_source = self.driver.find_element_by_tag_name('html').get_attribute('innerHTML')
            self.driver.quit()
            html = BeautifulSoup(html_source, 'html.parser')
            return html

        except:
            return 'Time out'


p = Proxy()
p.make_proxy_requst()
