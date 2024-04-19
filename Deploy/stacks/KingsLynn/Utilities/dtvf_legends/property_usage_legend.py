import os
import math
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

# Dictionary of building usages with respective colors
#NOTE: Align values and colors with data.json
building_usages = {
    "Domestic": "#808080",
    "Emergency\nServices": "#D62728",
    "Medical Care": "#FF7F0E",
    "Education": "#2CA02C",
    "Non-Domestic": "#1F77B4"
}
# Specify input icon
icon = "building_icon.png"
icon_path = os.path.join(Path(__file__).parent, icon)
# Specify filepath to store legend
f = 'usage_colorbar.png'
fp = os.path.join(Path(__file__).parent.parent.parent, 'StackDeployment', 'inputs', 
                  'stack-manager', 'inputs', 'data', 'visualisation', 'data', 'icons', f)

# Set image size and font size
size = 150
font_size = 50
cols = 3

# Calculate number of rows and columns
num_rows = math.ceil(len(building_usages)/cols)
num_cols = min(len(building_usages), cols)

# Create an empty image and a draw object to draw on it
legend_img = Image.new('RGBA', (size*num_cols*2, size*num_rows*2), color=(255, 255, 255, 0))
draw = ImageDraw.Draw(legend_img)

# Load font
font = ImageFont.truetype('arial.ttf', font_size)

# Set label padding
label_padding = 25

# Loop through the building usages and draw each one on the legend
for i, (usage, color) in enumerate(building_usages.items()):
    # Load building icon image and resize
    icon = Image.open(icon_path).resize((size, size))
    
    # Apply color to icon
    colored_icon = Image.new('RGBA', icon.size, color=color)
    building_icon = Image.composite(colored_icon, icon, icon)
    
    # Calculate row and column indices and paste building icon on legend
    row_idx = i // num_cols
    col_idx = i % num_cols
    icon_x = size*col_idx*2+size//2
    icon_y = size*row_idx*2+size//2
    legend_img.paste(building_icon, (icon_x, icon_y), building_icon)
    
    # Add label text centered below the building icon
    label_width, label_height = draw.textsize(usage, font=font)
    label_x = icon_x + size//2 - label_width//2
    label_y = icon_y + size + size//2 - label_height//2 - label_padding
    draw.text((label_x, label_y), usage, fill='black', font=font)

# Save legend image
legend_img.save(fp, dpi=(300, 300))