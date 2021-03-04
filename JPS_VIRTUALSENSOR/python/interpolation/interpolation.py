try: 
    import pandas as pd
    import numpy as np
    import re, json, sys
    from scipy.interpolate import RegularGridInterpolator
    from io import StringIO
    import json
except Exception as e:
    print(e)
    exit(1)

# this function will interpolate the emissions at the given latlng coordinates
# returns values at each height within the grid
def interpolate(filepath,x,y):
    with open(filepath) as f:
        content = re.sub(r'[ ]+',',',re.sub(r'[ ]+\n','\n',f.read()))
        f.close()
    content = StringIO(content)
    data = pd.read_csv(content, delimiter=',')

    # collect coordinates
    x_all = data["X(m)"]
    y_all = data["Y(m)"]
    z_all = data["Z(m)"]

    x_set = sorted(set(x_all))
    y_set = sorted(set(y_all))
    z_set = sorted(set(z_all))

    # create interpolation point at each height
    interp_point = np.empty((len(z_set),3));
    for i in range(len(z_set)) :
        interp_point[i] = [x,y,z_set[i]]

    # create a matrix for each pollutant, then find where the input is located within the matrix
    pollutants = list(data.columns)[7:]

    # initialise result
    result = {}
    result["height"] = z_set
    for pol in pollutants:
        # initialise array
        pol_matrix = np.empty((len(x_set),len(y_set),len(z_set)))
        # collect data in a list first
        pol_list = data[pol]
        # transform list into a matrix
        for i in range(len(pol_list)) :
            x_index = x_set.index(x_all[i])
            y_index = y_set.index(y_all[i])
            z_index = z_set.index(z_all[i])
            pol_matrix[x_index,y_index,z_index] = pol_list[i]

        interp_fn = RegularGridInterpolator((x_set,y_set,z_set), pol_matrix)
        result[pol] = interp_fn(interp_point).tolist()

    print(json.dumps(result))
    with open('result.json','w') as f:
        json.dump(result,f)

if __name__ == "__main__":
    filepath = sys.argv[1] # "3D_instantanous_mainconc_center.dat"
    x = sys.argv[2] # x coordinate to interpolate
    y = sys.argv[3] # y coordinate to interpolate
    try:
        interpolate(filepath,x,y)
    except Exception as e:
        print(e)
        exit(1)
