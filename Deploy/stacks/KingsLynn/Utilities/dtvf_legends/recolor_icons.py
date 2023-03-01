# The purpose of this script is to re-color the initial reporting station icons to
#      1) use different colors to represent the different data providers
#      2) use different icons to represent the different types of stations

import os
import codecs
import fitz
import math

from pathlib import Path
from svglib.svglib import svg2rlg
from reportlab.graphics import renderPDF

# Define target icon details
res = 64  # 64 x 64 pixels
dpi = 32

#
# 1) Create colored default icons to distinguish between data providers
# 

# Define input icon
icon = 'circle.svg'
fp_in = os.path.join(Path(__file__).parent, icon)
input_color = "#68BF56"
# Define colors and names for output icons
# 1) Metoffice weather stations
# 2) Air quality stations
# 3) Environment Agency flood monitoring stations
names = ['metoffice', 'ukair', 'floodmonitoring']
target_colors = ['#7b2cbf', '#2b6cb0', '#29a745']

for i in range(len(names)):
    # Create output file path
    fp_out = os.path.join(Path(__file__).parent, '{}.png'.format(names[i]))
    temp = os.path.join(Path(__file__).parent, 'tmp.svg')

    # Read SVG file
    with codecs.open(fp_in, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    # Change color
    new_SVG = content.replace(input_color, target_colors[i])

    # Save updated .svg as temporary file (deleted later)
    with codecs.open(temp, 'w', encoding='utf-8', errors='ignore') as f:
        f.write(new_SVG)
    
    # Convert svg to png: directly rendering to png does not support transparency
    # https://github.com/deeplook/svglib/issues/171
    # Convert svg to pdf in memory with svglib+reportlab    
    drawing = svg2rlg(path=temp)
    pdf = renderPDF.drawToString(drawing)

    # Open pdf with fitz (pyMuPdf) to convert to PNG
    doc = fitz.Document(stream=pdf)
    pix = doc.load_page(0).get_pixmap(alpha=True, dpi=100)

    # Adjust and save output png
    n = int(pix.height/64)
    n = math.floor(math.log(n)/math.log(2))
    pix.shrink(n)
    pix.set_dpi(dpi, dpi)
    pix.save(fp_out)

    # Delete temporary file
    os.remove(temp)
    

# #
# # 2) Create gray default icons to distinguish between station types
# # 

# # Define input icons
# icons = ['airquality.png', 'flow.png', 'rainfall.png', 'temperature.png', 'water-level.png', 
#          'weather.png', 'weather-for.png', 'weather-obs.png', 'wind.png']
# target_color = '#808080'
# # Define "key" color of input icon to be replaced with the target colors
# source_color = (104, 191, 86)

# for icon in icons:
#     fp1 = os.path.join(Path(__file__).parent, icon)
#     fp2 = os.path.join(Path(__file__).parent.parent.parent, 'DTVF', 'data', 'icons', icon)
#     color_icon = recolor_icon(fp1, source_color, target_color)
#     # save the recolored icon with a new filename
#     color_icon.save(fp2)
