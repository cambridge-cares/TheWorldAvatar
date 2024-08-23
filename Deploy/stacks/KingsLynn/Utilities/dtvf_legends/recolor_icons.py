# The purpose of this script is to re-color the initial reporting station icons to
#      1) use different colors to represent the different data providers
#      2) use different icons to represent the different types of stations

import os
import re
import codecs
import fitz
import math

from pathlib import Path
from svglib.svglib import svg2rlg
from reportlab.graphics import renderPDF

# Define target icon details
res = 64  # 64 x 64 pixels
dpi = 32

# Define input icon
icon = 'circle.svg'
fp_in = os.path.join(Path(__file__).parent, icon)
input_color = "#68BF56"
# Define colors and names for output icons
# 1) Metoffice weather stations
# 2) Air quality stations
# 3) Environment Agency flood monitoring stations
names = ['metoffice', 'ukair', 'floodmonitoring']
target_colors = ['#29a745', '#7b2cbf', '#2b6cb0']


def recolor_and_save(input_fp, output_fp, input_color, target_color):
    # Read SVG file
    with codecs.open(input_fp, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    # Change color
    pattern = re.compile(input_color, re.IGNORECASE)
    new_SVG = pattern.sub(target_color, content)

    # Save updated .svg as temporary file (deleted later)
    temp = os.path.join(Path(__file__).parent, 'tmp.svg')
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
    pix.save(output_fp)

    # Delete temporary file
    os.remove(temp)


#
# 1) Create colored default icons to distinguish between data providers
# 
for i in range(len(names)):
    # Create output file path
    fp_out = os.path.join(Path(__file__).parent, '{}.png'.format(names[i]))

    recolor_and_save(input_fp=fp_in, output_fp=fp_out, 
                     input_color=input_color, target_color=target_colors[i])


#
# 2) Create gray default icons to distinguish between station types
# 
# Define input icons
icons = ['airquality', 'flow', 'rainfall', 'temperature', 'water-level', 
         'weather', 'weather-for', 'weather-obs', 'wind']
target_color = '#808080'

for icon in icons:
    # Create input/output file paths
    fp_in = os.path.join(Path(__file__).parent, '{}.svg'.format(icon))
    fp_out = os.path.join(Path(__file__).parent, '{}.png'.format(icon))

    recolor_and_save(input_fp=fp_in, output_fp=fp_out, 
                     input_color=input_color, target_color=target_color)
    

#
# 3) Create colored map icons for station types
# 
# Define relevant icons and colors for data providers
providers = {
    'metoffice': ('#29a745', 'met', ['weather', 'weather-for', 'weather-obs']),
    'floodmonitoring': ('#2b6cb0', 'ea', ['flow', 'rainfall', 'temperature', 'water-level', 
                                          'wind']),
    'ukair': ('#7b2cbf', 'air', ['airquality']),
}

for k, v in providers.items():    
    for icon in v[2]:
        # Create input/output file paths
        fp_in = os.path.join(Path(__file__).parent, '{}.svg'.format(icon))
        fp_out = os.path.join(Path(__file__).parent, '{}_{}.png'.format(v[1], icon))

        recolor_and_save(input_fp=fp_in, output_fp=fp_out, 
                        input_color=input_color, target_color=v[0])

