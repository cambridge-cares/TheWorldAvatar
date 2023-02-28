# The purpose of this script is to re-color the initial reporting station icons to
#      1) use different colors to represent the different data providers
#      2) use different icons to represent the different types of stations

import os

from PIL import Image
from pathlib import Path


def recolor_icon(icon_path, source_rgb, target_hex):
    
    def _replace_color(pixel, source_rgb, target_rgb):
        # Color replacement function that preserves the transition between color and white
        # Check if the pixel is white or transparent
        if pixel[3] == 0 or (pixel[0] == 255 and pixel[1] == 255 and pixel[2] == 255):
            return pixel
        if pixel[:3] == source_rgb:
            # Set full saturation target color for pixels that match the source color
            return target_rgb + (pixel[3],)
        else:
            # Adjust color/saturation for transition pixels
            pixel_new = []
            for p in range(3):
                # Interpolate between white and the target color
                # Relative interpolation factor
                #adj = (pixel[p] - source_rgb[p]) / source_rgb[p]
                #adj = int(min(255, (1 + adj) * target_rgb[p]))
                # Absolute interpolation increment
                adj = round((pixel[p] - source_rgb[p]) + target_rgb[p])
                pixel_new.append(adj)
            # Create RGBA tuple from RGB color and alpha value of the original pixel
            return (*pixel_new, pixel[3])

    # convert target hexcolor to RGB tuple
    c = target_hex.lstrip('#')
    target_rgb = tuple(int(c[i:i+2], 16) for i in (0, 2, 4))

    # load the icon image
    icon = Image.open(icon_path)
    # convert the image to RGBA mode
    icon = icon.convert('RGBA')
    # get the image data as a list of pixels
    pixels = icon.getdata()
    # create a new list of pixels where non-white parts are replaced with the defined color
    new_pixels = [_replace_color(pixel, source_rgb, target_rgb) for pixel in pixels]
    # update the image with the new pixel data
    icon.putdata(new_pixels)
    return icon


#
# 1) Create colored default icons to distinguish between data providers
# 

# Define input icon
icon = 'ea-empty.png'
fp1 = os.path.join(Path(__file__).parent.parent.parent, 'DTVF', 'data', 'icons', icon)
# Define colors and labels for output icons
# 1) Metoffice weather stations
# 2) Air quality stations
# 3) Environment Agency flood monitoring stations
names = ['metoffice', 'airquality', 'floodmonitoring']
target_colors = ['#2b6cb0', '#7b2cbf', '#29a745']
# Define "key" color of input icon to be replaced with the target colors
source_color = (104, 191, 86)

for i in range(len(names)):
    f = names[i] + '.png'
    fp2 = os.path.join(Path(__file__).parent.parent.parent, 'DTVF', 'data', 'icons', f)
    color_icon = recolor_icon(fp1, source_color, target_colors[i])
    # save the recolored icon with a new filename
    color_icon.save(fp2)
