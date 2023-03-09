import urllib.request
from threading import Thread 

# declare the class 'BMSDataStream'
opener = urllib.request.FancyURLopener({})

result = []

with open('workingdir/Urls.txt') as file:
	urls = file.readlines()
	
def fetch_data(url):
	with opener.open(url) as f:
		read = f.read().decode('utf-8','ignore')
		print(read)
		result.append(read)
		
threads = [Thread(target=fetch_data, args=(url,)) for url in urls]
for thread in threads:
    thread.start()
for thread in threads:
    thread.join()
	
	
with open('workingdir/bmsResult.html','w') as file:
	file.write('')

for section in result:
	with open('workingdir/bmsResult.html','a') as file:
		file.write(section)