# # TODO:  allow google to show the image
#
# from WolframGoogle import WolframGoogle
#
# google = WolframGoogle()
#
# r = google.get_result_from_google_directly('What is the molecular weight of Ch4')
# print(r)
html_template = '''
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Test HTML for google visual results</title>
</head>
<body>

 

%s

</body>
</html>

'''

url = 'https://www.google.com/search?q=What+is+the+molecular+weight+of+CO2'
from selenium import webdriver
from bs4 import BeautifulSoup

driver = webdriver.Firefox()
driver.get(url)

html = driver.find_element_by_tag_name('html').get_attribute('innerHTML')
# print(html)
soup = BeautifulSoup(html, 'html.parser')
div_result = soup.find_all('div', class_='ifM9O')[0]
# html_content = html_template % div_result.encode('utf-8').decode('utf-8')
html_content = html_template % div_result.encode('utf-8').decode('utf-8')
with open('show.html', 'w') as f:
    f.write(html_content)



# images = soup.find_all('g-img')
# rst = ''
# for img in images:
#     print(img)

driver.quit()