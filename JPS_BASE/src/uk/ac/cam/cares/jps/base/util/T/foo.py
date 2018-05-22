import sys
content = sys.argv[1]
with open('bar.txt', 'w') as f:
    f.write(str(content)) 
