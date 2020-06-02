import tarfile
import os
path = './'
files = os.listdir(path)
paths = [os.path.join(path, basename) for basename in files]
file_name = max(paths, key=os.path.getctime)
print(file_name)
tf = tarfile.open(file_name)
tf.extractall()