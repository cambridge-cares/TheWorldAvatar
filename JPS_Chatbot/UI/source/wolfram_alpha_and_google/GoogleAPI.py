import re

import time

from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from bs4 import BeautifulSoup


# what is the heat capacity of h2so4

# create new div to hold result from google
def create_new_node(content, class_):
    new_div = BeautifulSoup('<div class="%s">%s</div>' % (class_, content), 'html.parser').div
    return new_div


# put the created div in one div
def create_visualization_table(key_components):
    if 'main_result' in key_components:
        result = key_components['main_result']
        google_result = create_new_node('', 'google_result')
        main_result = create_new_node('', 'main_result border')
        main_result.append(result)
        google_result.append(main_result)
        if 'extra' in key_components:
            extra = key_components['extra']
            google_result.append(extra)
        return google_result


# in the case that google gives the standard info box
def create_visualization_standard(key_components):
    # the 'google_result' contains all the components
    # the 'head' comes first
    # the 'main_result' contains the value and image
    # then the description
    google_result = create_new_node('', 'google_result')
    main_result = create_new_node('', 'main_result border')

    if 'head' in key_components:
        head = key_components['head']
        google_result.append(head)

    if 'value' in key_components:
        value = key_components['value']
        main_result.append(create_new_node(value, 'value'))

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

    return google_result


# remove all redundant information returned by google
def super_filter(html):
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


# extract components from
def extract_key_components(html):
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

    kp_header = html.find_all('div', class_='kp-header')
    if len(kp_header) > 0:
        result['type'] = 'standard'
        kp_header = kp_header[0]
        value = kp_header.text
        result['value'] = value

        image = kp_header.find_all('g-img')
        if len(image) > 0:
            image = image[0]
            image = image.find_all('img')[0]
            image['style'] = ''
            image['class'] = 'key_img'
            # print('the image extracted is', image)
            result['img'] = image

        description = div_result.find_all('div', class_='i4J0ge')
        if len(description) > 0:
            description = description[0]
            description['class'] = 'description border rounded'
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


class GoogleAPI:

    def __init__(self):
        self.url_template = 'https://www.google.com/search?hl=en&q=%s'
        self.options = Options()
        self.options.add_argument('--headless')
        self.options.add_argument('--disable-logging')

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

    # to separate the key components in the html
    # make the http request and parse the source code with beautiful soup
    def make_request(self, question):
        url = self.make_url(question)
        address = '185.141.58.109:19596'
        webdriver.DesiredCapabilities.FIREFOX['proxy'] = {
            "httpProxy": address,
            "sslProxy": address,
            "proxyType": "MANUAL"
        }

        driver = webdriver.Firefox(options=self.options)

        try:
            driver.get(url)
            html_source = driver.find_element_by_tag_name('html').get_attribute('innerHTML')
            driver.close()
            html = BeautifulSoup(html_source, 'html.parser')
            return html

        except:
            return 'Time out'

    # this function will remove all the hidden and non-essential text returned by google
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
        key_components = extract_key_components(html)
        if key_components is not None:
            type_ = key_components['type']
            result = 'Google failed to provide a direct answer'
            if type_ == 'standard':
                result = create_visualization_standard(key_components)
            elif type_ == 'table':
                result = create_visualization_table(key_components)

            result = super_filter(result)
            return result
        else:
            return ""
