# in one hour, after one c
#
import time
import urllib
import urllib.parse
import urllib.request

import re
from datetime import datetime
import random

from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from bs4 import BeautifulSoup


def fire_query_to_chatbot(question):

    # x = input()
    values = {'question': question}
    tail = (urllib.parse.urlencode(values))
    url = "http://127.0.0.1:5000/query?question="  + tail
    #


    r = urllib.request.urlopen(url)
    return r

# def make_request(question):
#     url = "http://127.0.0.1:5000/query?question=" + question
#     options = Options()
#     options.add_argument('--headless')
#     driver = webdriver.Firefox(options=self.options)
#     driver.get(url)
#     html_source = self.driver.find_element_by_tag_name('html').get_attribute('innerHTML')
#     self.driver.quit()
#     html = BeautifulSoup(html_source, 'html.parser')
#     return html
now = datetime.now()
current_time = now.strftime("%H:%M:%S")
print("Current Time =", current_time)

with open('test_log', 'w') as f:
    f.write('test begins at' + current_time + '\n')
    f.close()

with open('error_log', 'w') as f:
    f.write('test begins at' + current_time + '\n')
    f.close()


with open('page_questions') as f:
    page_questions = [q.replace('\n', '') for q in f.readlines()]
    # print(page_questions)
    print('number of questions in total', len(page_questions))

with open('verified_questions', 'w') as f:
    f.write(' ')
    f.close()

verified_questions = []
counter = 0
good_counter = 0
with open('page_questions') as f:
    counter = counter + 1
    print('we are at number ', counter)
    page_questions = [q.replace('\n', '') for q in f.readlines()]
    # for q in random.choices(page_questions, k= 10):
    for q in page_questions:
        # print(q)
        r = fire_query_to_chatbot(q)

        content = r.read().decode('utf-8')
        # print('content:\n', content)
        flag = 'Nothing' in content
        if flag:
            print('failure')
            with open('error_log', 'a') as f:
                f.write(q)
                f.write('\n ------------ \n')
                f.close()
        else:
            good_counter = good_counter + 1
            print('good counter', good_counter)
            verified_questions.append(q)

            with open('verified_questions', 'a') as f:
                f.write(q)
                f.write('\n')
                f.close()

            with open('test_log', 'a') as f:
                f.write('question:\n' + q)
                f.write('\nresult:\n' + content)
                f.write('\n ------------ \n')
                f.close()

        print('flag', flag)
        print('---------------')
        time.sleep(1)




# http://127.0.0.1:5000/query_wolfram?question=%C2%A0%20mass%20of%20aromatic%20hydrocarbons