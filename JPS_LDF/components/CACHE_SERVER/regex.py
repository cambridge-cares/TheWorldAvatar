import re, time


start = time.time()

with open('Result') as f:
    content = f.read().replace('\n', '')
    print(content)
    # x = re.findall("\"@id.*\n.*\n.*", content)
    # print(x[0])
    
print(time.time() -start)