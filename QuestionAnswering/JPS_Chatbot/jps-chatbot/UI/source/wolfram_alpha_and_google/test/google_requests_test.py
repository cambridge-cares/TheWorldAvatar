# import requests
# from bs4 import BeautifulSoup
# from pprint import pprint
# headers_Get = {
#         'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:49.0) Gecko/20100101 Firefox/49.0',
#         'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
#         'Accept-Language': 'en-US,en;q=0.5',
#         'Accept-Encoding': 'gzip, deflate',
#         'DNT': '1',
#         'Connection': 'keep-alive',
#         'Upgrade-Insecure-Requests': '1'
#     }
#
#
# def google(q):
#     s = requests.Session()
#     q = '+'.join(q.split())
#     url = 'https://www.google.com/search?q=' + q + '&ie=utf-8&oe=utf-8'
#     r = s.get(url, headers=headers_Get)
#
#     soup = BeautifulSoup(r.text, "html.parser")
#     output = []
#
#     pprint(soup)
#
#     #
#     # for searchWrapper in soup.find_all('h3', {'class':'r'}):
#     #     url = searchWrapper.find('a')["href"]
#     #     text = searchWrapper.find('a').text.strip()
#     #     result = {'text': text, 'url': url}
#     #     output.append(result)
#     #
#     # return output
#
#
# rst = google('what is the molecular weight of CH4')
# pprint(rst)

# Import the beautifulsoup
# and request libraries of python.
import requests
import bs4

# Make two strings with default google search URL
# 'https://google.com/search?q=' and
# our customized search keyword.
# Concatenate them
text = "c++ linear search program"
url = 'https://google.com/search?q=' + text

# Fetch the URL data using requests.get(url),
# store it in a variable, request_result.
request_result = requests.get(url)

# Creating soup from the fetched request
soup = bs4.BeautifulSoup(request_result.text, "html.parser")
print(soup)

filter = soup.find_all("h3")
for i in range(0, len(filter)):
    print(filter[i].get_text())