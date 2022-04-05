import sys
argsString = sys.argv[1]
with open('result.txt','w') as file:
	file.write(argsString)