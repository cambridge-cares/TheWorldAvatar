# Python script for PythonHelper unit test
# Print Hello World + input arguments

import sys

if __name__ == '__main__':
			
	result = "Hello World!" 
	
	parameters = sys.argv[1:]
	
	for p in parameters :
		result = result + " " + p
		
	print(result)