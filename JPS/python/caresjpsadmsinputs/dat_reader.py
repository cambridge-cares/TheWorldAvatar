import pandas as pd
import re, csv, json, sys
from io import StringIO
import json


    
def convert_dat(filepath):
    with open(filepath) as f:
        content = re.sub(r'[ ]+',',',re.sub(r'[ ]+\n','\n',f.read()))
        f.close()
    content = StringIO(content) 
    data = pd.read_csv(content, delimiter=',')
    data.sort_values(by=['X(m)', 'Y(m)'])
    pollutants = list(data.columns)[7:]  
    heights = sorted(set(list(data['Z(m)'])))
    num_heights = len(heights)
    num_interval=heights[2]-heights[1]
    initial_height=heights[0]
    num_pollutant = len(pollutants)
    result = []
    for height in heights:
        height_list = []
        # 1. seperate the rows with different heights first
        data_at_x_m = data.loc[data['Z(m)'] == height]
        # 2. seperate each column after 7 and make each of them a list
        list_at_x_m = data_at_x_m.iloc[:,-num_pollutant:]
        for col in list_at_x_m:
            height_list.append(list(list_at_x_m[col].values))
        result.append(height_list)

        
    print(json.dumps({'grid': result, 'numheight': num_heights, 'listofpol': pollutants, 'numpol': num_pollutant, 'numinterval':num_interval, 'initialheight':initial_height}))
    with open('data.json','w') as f:
        json.dump({'grid': result, 'numheight': num_heights, 'listofpol': pollutants, 'numpol': num_pollutant, 'numinterval':num_interval, 'initialheight':initial_height},f)


if __name__ == "__main__":#test
    filepath = sys.argv[1]
    convert_dat(filepath)