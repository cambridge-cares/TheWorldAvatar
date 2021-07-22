import numpy as np 
import matplotlib.pyplot as plt 

ax = plt.subplot()
im = ax.imshow(np.linspace(5,10,100).reshape((10, 10)),cmap='BuPu')
plt.colorbar(im)

plt.show()