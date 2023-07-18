from PIL import Image

# # Open the image file and convert it to RGB format
# image = Image.open("/mnt/d/wx243/FromTWA/background.png").convert('RGB')
# # Get the dominant color in the image
# dominant_color = max(image.getcolors(image.size[0]*image.size[1]), key=lambda x: x[0])[1]
# # Print the RGB value of the dominant color
# print(dominant_color)

##  = (219, 219, 220)


# Open the image file and convert it to RGBA format
img = Image.open("/mnt/d/wx243/FromTWA/netDemanding.png").convert('RGBA')

# Set the RGB color to be transparent
r, g, b = 219, 219, 220 # set the color you want to be transparent
data = img.getdata()
new_data = []
for item in data:
    if item[0] == r and item[1] == g and item[2] == b:
        new_data.append((255, 255, 255, 0))
    else:
        new_data.append(item)

# Create a new image with the transparent color
img.putdata(new_data)

# # Set the region to be transparent 1
# x, y, size1, size2 = 448, 645, 39, 75 # set the x and y coordinates and the size of the square region
# x1, y1, x2, y2 = x, y, x+size1, y+size2
# for i in range(x1, x2):
#     for j in range(y1, y2):
#         data = img.getpixel((i,j))
#         new_data = (data[0], data[1], data[2], 0)
#         img.putpixel((i,j), new_data)

# # Set the region to be transparent 2
# x, y, size = 408, 675, 45# set the x and y coordinates and the size of the square region
# x1, y1, x2, y2 = x, y, x+size, y+size
# for i in range(x1, x2):
#     for j in range(y1, y2):
#         data = img.getpixel((i,j))
#         new_data = (data[0], data[1], data[2], 0)
#         img.putpixel((i,j), new_data)

img.save("/mnt/d/wx243/FromTWA/transparent_netDemanding.png")

