import re

import time

from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from bs4 import BeautifulSoup


# what is the heat capacity of h2so4


class GoogleAPI:

    def __init__(self):
        self.options = Options()
        self.options.add_argument('--headless')
        self.options.add_argument('--disable-logging')
        address = '185.141.58.109:19596'
        webdriver.DesiredCapabilities.CHROME['proxy'] = {
            "httpProxy": address,
            "sslProxy": address,
            "proxyType": "MANUAL"
        }

        self.driver = webdriver.Chrome(options=self.options)

        self.url_template = 'https://www.google.com/search?hl=en&q=%s'

    # due to possible restriction from google, we implement a proxy to enable robust request to google.
    def test_proxy_ip(self):
        start_time = time.time()
        self.driver.get("http://www.whatsmyip.org/")
        html_source = self.driver.find_element_by_tag_name('html').find_element_by_id('ip').get_attribute('innerHTML')
        print('got response')
        print(html_source)
        end_time = time.time()
        print('it took ', end_time - start_time)
        return html_source

    def combine_key_components(self, key_components):
        pass

    # to separate the key components in the html

    def extract_key_components(self, html):

        result = {}
        div_ifM = html.find_all('div', class_='ifM9O')

        # if ifM90 begins with 'People also search ...', then use wp-tabs
        if len(div_ifM) > 0:
            # a result is returned
            div_result = div_ifM[0]
        else:
            print('There is no ifM9O component, should turn to wp-tabs')
            return None
        # print('div result', div_result)
        # find the key components, only extract the content of them. Discard all the wrappers
        # find the result heading, it is the first element with the role heading
        headings = div_result.find_all('div', role='heading')
        if len(headings) > 0:
            key_head = headings[0]
            # change its class to one of ours:
            key_head['class'] = 'border rounded head'
            # print('the key head extracted', key_head)
            result['head'] = key_head
        else:
            print('There is no heading')

        # ================ get the key value of the result ============
        kp_header = html.find_all('div', class_='kp-header')
        # print('kp_header \n ======================= \n', kp_header)

        if len(kp_header) > 0:
            result['type'] = 'standard'
            kp_header = kp_header[0]
            #            value = kp_header.findChild().findChild()
            value = kp_header.text
            # print('the value extracted', value)
            result['value'] = value
            # =============== get the key image if there is any ============

            image = kp_header.find_all('g-img')
            if len(image) > 0:
                image = image[0]
                image = image.find_all('img')[0]
                image['style'] = ''
                image['class'] = 'key_img'
                # print('the image extracted is', image)
                result['img'] = image

            # ================ get the siblings of the kp-head =========
            description = div_result.find_all('div', class_='i4J0ge')
            if len(description) > 0:
                # this means the result page comes with a description
                description = description[0]
                description['class'] = 'description border rounded'
                # print('the description is \n =================== \n', description)
                result['description'] = description

            extra = div_result.find_all(text=re.compile('People also search for'))
            if len(extra) > 0:
                extra = extra[0].parent.parent.parent
                # find all the data-reltype="sideways" components, change their class
                sideways = extra.find_all('div', {'data-reltype': 'sideways'})
                for s in sideways:
                    s['class'] = 'sideways'
                    sideways_container = s.parent
                    sideways_container['class'] = 'sideways-container'
                extra['class'] = 'extra border rounded'
                result['extra'] = extra
                # print('here is the people also search for \n ================ \n', extra)
        else:
            # there are two cases: wp-container or table
            # try to find 'kp-wholepage'

            # kp_wholepage = div_result.find_all('div', class_='kp-wholepage')
            kp_wholepage = html.find_all('div', attrs={'class': re.compile('^kp-wholepage.*')})

            if len(kp_wholepage) > 0:
                kp_wholepage = kp_wholepage[0]
                # find all the data-reltype="sideways" components, change their class
                sideways = kp_wholepage.find_all('div', {'data-reltype': 'sideways'})
                for s in sideways:
                    s['class'] = 'sideways'
                    sideways_container = s.parent
                    sideways_container['class'] = 'sideways-container'

                result['type'] = 'table'
                result['main_result'] = kp_wholepage

            else:
                print('There is no kp-header but with ifM9O')

                # if the first child of ifM9O is h2 with People also ask
                children = div_ifM[0].findChildren()
                if len(children) > 0:
                    first_child = children[0]
                    if first_child.name == 'h2' and first_child.text == 'People also ask':
                        return None

                result['type'] = 'table'
                # the main content will be under class 'mod'
                result['main_result'] = div_result

        return result

    def create_new_node(self, content, class_):
        new_div = BeautifulSoup('<div class="%s">%s</div>' % (class_, content), 'html.parser').div
        return new_div

    def create_visualization_table(self, key_components):
        if 'main_result' in key_components:
            result = key_components['main_result']
            google_result = self.create_new_node('', 'google_result')
            main_result = self.create_new_node('', 'main_result border')
            main_result.append(result)
            google_result.append(main_result)
            if 'extra' in key_components:
                extra = key_components['extra']
                print('==================\n', extra)
                google_result.append(extra)
            return google_result

    def create_visualization_standard(self, key_components):
        # the 'google_result' contains all the components
        # the 'head' comes first
        # the 'main_result' contains the value and image
        # then the description
        google_result = self.create_new_node('', 'google_result')
        main_result = self.create_new_node('', 'main_result border')

        if 'head' in key_components:
            head = key_components['head']
            google_result.append(head)

        if 'value' in key_components:
            value = key_components['value']
            main_result.append(self.create_new_node(value, 'value'))

        if 'img' in key_components:
            img = key_components['img']
            main_result.append(img)

        google_result.append(main_result)

        if 'description' in key_components:
            description = key_components['description']
            google_result.append(description)

        if 'extra' in key_components:
            extra = key_components['extra']
            google_result.append(extra)

        # print(main_result)

        return google_result

    # make the http request and parse the source code with beautiful soup
    def make_request(self, question):
        url = self.make_url(question)
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
        # self.driver = webdriver.Firefox(options=self.options)

        # self.test_proxy_ip()

        try:
            self.driver.get(url)
            html_source = self.driver.find_element_by_tag_name('html').get_attribute('innerHTML')
            self.driver.quit()
            html = BeautifulSoup(html_source, 'html.parser')
            return html

        except:
            return 'Time out'

    # this function will remove all the hidden and non-essential text returned by google

    def super_filter(self, html):

        if type(html) is not str:
            stop_words = ['Featured snippet from the web', 'Click on the error', 'Claim this knowledge panel',
                          'Feedback']
            for s_w in stop_words:
                tmp = html.find_all(text=re.compile(s_w))
                for t in tmp:
                    tag_name = t.parent.name
                    try:
                        t.decompose()
                    except:
                        t.parent.decompose()

        svgs = html.find_all('svg')
        for svg in svgs:
            svg.decompose()

        return html
        #  extra = div_result.find_all(text=re.compile('People also search for'))

    # encode the question and format the url for google request
    def make_url(self, question):
        tmp = question.split(' ')
        encoded_question = '+'.join(tmp)
        return self.url_template % encoded_question

    def run(self, question):
        # make request
        print('processing question', question)
        html = self.make_request(question)
        # print('the html is ', html)
        key_components = self.extract_key_components(html)
        if key_components is not None:
            type_ = key_components['type']
            result = 'Google failed to provide a direct answer'
            if type_ == 'standard':
                result = self.create_visualization_standard(key_components)
            elif type_ == 'table':
                result = self.create_visualization_table(key_components)

            result = self.super_filter(result)
            return result
        else:
            return ""

    # def run(self, question):
    #     ip = self.test_proxy_ip()
    #     with open('iplog', 'w') as f:
    #         f.write(ip)
    #         f.close()
    #     print('=======================')
    #     print(ip)
    #     print('here is the ip')


if __name__ == '__main__':
    g = GoogleAPI()
    g.run('what is the molecular weight of CH4')
