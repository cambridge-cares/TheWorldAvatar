from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from bs4 import BeautifulSoup

# what is the heat capacity of h2so4



class GoogleAPI:

    def __init__(self):
        options = Options()
        options.add_argument('--headless')
        self.driver = webdriver.Firefox(options=options)
        self.url_template = 'https://www.google.com/search?q=%s'

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
            return None
        # print('div result', div_result)
        # find the key components, only extract the content of them. Discard all the wrappers
        # find the result heading, it is the first element with the role heading
        headings = div_result.find_all('div', role='heading')
        if len(headings) > 0:
            key_head = headings[0]
            # change its class to one of ours:
            key_head['class'] = 'border rounded head'
            print('the key head extracted', key_head)
            result['head'] = key_head
        else:
            print('There is no heading')

        # ================ get the key value of the result ============
        kp_header = html.find_all('div', class_='kp-header')
        if len(kp_header) > 0:
            kp_header = kp_header[0]
            #            value = kp_header.findChild().findChild()
            value = kp_header.text
            print('the value extracted', value)
            result['value'] = value
            # =============== get the key image if there is any ============
            image = kp_header.find_all('g-img')
            if len(image) > 0:
                image = image[0]
                image = image.find_all('img')[0]
                image['class'] = 'key_img'
                # print('the image extracted is', image)
                result['img'] = image
            # ================ get the siblings of the kp-head =========
            description = div_result.find_all('div', class_='i4J0ge')
            description['class'] = 'description border rounded'
            if len(description) > 0:
                # this means the result page comes with a description
                description = description[0]
                print('the description is \n =================== \n', description)
                result['description'] = description


            print('the siblings found \n ====================== \n')

            with open('siblings', 'w') as f:
                f.write(description.encode('utf-8').decode('utf-8'))

        return result

    def create_new_node(self, content, class_):
        new_div = BeautifulSoup('<div class="%s">%s</div>' % (class_, content), 'html.parser').div
        return new_div

    def create_visualization(self, key_components):
        # the 'main result'
        img = key_components['img']
        value = key_components['value']
        head = key_components['head']
        description = key_components['description']



        main_result = self.create_new_node('', 'main_result border')
        main_result.append(self.create_new_node(value, 'value'))
        main_result.append(img)
        # print(main_result)

    # make the http request and parse the source code with beautiful soup
    def make_request(self, question):
        url = self.make_url(question)
        self.driver.get(url)
        html_source = self.driver.find_element_by_tag_name('html').get_attribute('innerHTML')
        self.driver.quit()
        html = BeautifulSoup(html_source, 'html.parser')
        return html

    # encode the question and format the url for google request
    def make_url(self, question):
        tmp = question.split(' ')
        encoded_question = '+'.join(tmp)
        return self.url_template % encoded_question

    def run(self, question):
        # make request
        html = self.make_request(question)
        # print('the html is ', html)
        key_components = self.extract_key_components(html)
        self.create_visualization(key_components)
