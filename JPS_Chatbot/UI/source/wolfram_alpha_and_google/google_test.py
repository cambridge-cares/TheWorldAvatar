# # TODO:  allow google to show the image
#
# from WolframGoogle import WolframGoogle
#
# google = WolframGoogle()
#
# r = google.get_result_from_google_directly('What is the molecular weight of Ch4')
# print(r)
import re

html_template = '''
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css" integrity="sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2" crossorigin="anonymous">

    <title>Test HTML for google visual results</title>
</head>
<style>
.kp-header { font-size:20px; font-family: arial; width: 600px }
.headings { font-size:20px; font-family: arial; width: 600px}

</style>
<body>

<span class="rounded">%s</div> 


<br/>

 

</body>
</html>

'''

# Fix the "president of china problem"

#question = 'who is the president of china'
# question = 'where is the capital of china'
# question = 'what is the molecular weight of benzene'
question = 'what is the boiling point of water'
# question = 'what is the heat capacity of H2SO4'
def encode_question(q):
    tmp = q.split(' ')
    return '+'.join(tmp)


encoded_question = encode_question(question)
print('encoded question', encoded_question)



def get_wp_tabs_container(html):
    tabs = html.find_all('div', id='wp-tabs-container')
    # print('id="wp-tabs-container"', tabs)
    if len(tabs) > 0:
        return tabs[0]
    else:
        return ''

def replace_text(html):
    findtoure = html.find_all(text=re.compile('Featured snippet from the web'))
    for comment in findtoure:
        fixed_text = comment.replace('Featured snippet from the web', '')
        comment.replace_with(fixed_text)
    return html


def extract_key_components(html):
    # find the key components, only extract the content of them. Discard all the wrappers
    # find the result heading, it is the first element with the role heading
    headings = html.find_all('div', role='heading')
    if len(headings) > 0:
        key_head = headings[0]
        # change its class to one of ours:
        key_head['class'] = key_head


    print('the key head extracted', key_head)










def process_head_result(html):
    head = html.findChild().text.strip()
    # print(head)
    if 'People also ask' == head: # It means we have a problem. The main content comes from the wp-content
        return None
    else:

        headings = html.find_all('div', role='heading')
        for h in headings:
            # print(h)
            h['class'] = 'border headings'

        kp_header = html.find_all('div', class_='kp-header')
        if len(kp_header) > 0:
            kp_header = kp_header[0]
            kp_header['style'] = '{ font-size:20px; font-family: arial }'

            value = kp_header.findChild().findChild()
            value['style'] = '{ font-family: arial; font-size: 40px}'

            # =============== find the key image =================
        key_image = html.find_all('')


            # value.decompose()
        # remove this part first ...

        # find the key image if there is one, first remove it






url = 'https://www.google.com/search?q=%s' % encoded_question


from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from bs4 import BeautifulSoup

# TODO: hide the firefox browser
# driver = webdriver.Firefox()
options = Options()
options.add_argument('--headless')
driver = webdriver.Firefox(options=options)
driver.get(url)

html = driver.find_element_by_tag_name('html').get_attribute('innerHTML')
# print(html)

soup = BeautifulSoup(html, 'html.parser')
div_result = soup.find_all('div', class_='ifM9O')[0]
extract_key_components(div_result)


# tbs = get_wp_tabs_container(soup)
# soup = replace_text(soup)
#
# div_result = soup.find_all('div', class_='ifM9O')[0]
#
# html = process_head_result(div_result)
#
# # html_content = html_template % div_result.encode('utf-8').decode('utf-8')
# html_content = html_template % (div_result)
# with open('show.html', 'w', encoding='utf-8') as f:
#     f.write(html_content)



# images = soup.find_all('g-img')
# rst = ''
# for img in images:
#     print(img)

driver.quit()