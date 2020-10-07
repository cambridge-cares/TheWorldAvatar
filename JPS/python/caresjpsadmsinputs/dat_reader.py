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
    xx = data["X(m)"][:100].values.tolist() 
    yy = data["Y(m)"][:100].values.tolist() 
    data.sort_values(by=['X(m)', 'Y(m)'])
    #grab all 100 of the coordinates
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
            single_array = list(list_at_x_m[col].values)
            n = 10
            x = [single_array[i:i + n] for i in range(0, len(single_array), n)] 
            transpose_single_array = []
            
            for k in range(n):
                for sub_list in x:
                    transpose_single_array.append(sub_list[k])
            height_list.append(transpose_single_array)
 
        result.append(height_list)


        
    print(json.dumps({'grid': result, 'numheight': num_heights, 'listofpol': pollutants, 'numpol': num_pollutant, 'numinterval':num_interval, 'initialheight':initial_height,"x_coord": xx, "y_coord":yy}))
    with open('data.json','w') as f:
        json.dump({'grid': result, 'numheight': num_heights, 'listofpol': pollutants, 'numpol': num_pollutant, 'numinterval':num_interval, 'initialheight':initial_height},f)


if __name__ == "__main__":#test
    filepath = sys.argv[1]
    convert_dat(filepath)