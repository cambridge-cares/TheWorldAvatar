import sys
sys.path.append('/UI')

from UI.source.CoordinateAgent import CoordinateAgent

ca = CoordinateAgent()
result = ca.run('What is the heat capacity of CH4')
print(result)