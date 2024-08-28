import os
import math
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

# Dictionary of flood warning types with respective colors
#NOTE: Align values and colors with data.json
flood_alert_types = {
    "Severe\nFlood\nWarning": "#c90a0a",
    "Flood\nAlert": "#ebc244",
    "Flood\nWarning": "#ff8800",
    "Inactive\nFlood\nAlert": "#add8e6"
}
opacity = 0.3                   # 0...1 (Mapbox)
opacity = int((1-opacity)*255)  # 0...255 (PIL)
# Specify input icon
icon = "flood_area.png"
icon_path = os.path.join(Path(__file__).parent, icon)
# Specify filepath to store legend
f = 'flood_warning_types.png'
fp = os.path.join(Path(__file__).parent.parent.parent, 'StackDeployment', 'inputs', 
                  'stack-manager', 'inputs', 'data', 'visualisation', 'data', 'icons', f)

# Set image size and font size
size = 150
font_size = 50
cols = 2

# Calculate number of rows and columns
num_rows = math.ceil(len(flood_alert_types)/cols)
num_cols = min(len(flood_alert_types), cols)

# Create an empty image and a draw object to draw on it
legend_img = Image.new('RGBA', (size*num_cols*3, round(size*num_rows*1.2)), color=(255, 255, 255, 0))
draw = ImageDraw.Draw(legend_img)

# Load font
font = ImageFont.truetype('arial.ttf', font_size)

# Set label padding
spacing = size//2
padding = size//4

# Loop through the flood warning types and draw each one on the legend
for i, (usage, color) in enumerate(flood_alert_types.items()):
    # Load flood icon image and resize
    icon = Image.open(icon_path).convert("RGBA").resize((size, size))
    # Extract alpha channel
    alpha = icon.getchannel("A")

    # Create a new alpha channel with the desired opacity
    new_alpha = Image.new("L", alpha.size, 0)
    # Replace the alpha value only where it is not fully transparent
    for x in range(alpha.width):
        for y in range(alpha.height):
            if alpha.getpixel((x, y)) > 0:
                new_alpha.putpixel((x, y), opacity)
    
    # Apply color to icon
    colored_icon = Image.new('RGBA', icon.size, color=color)
    flood_icon = Image.composite(colored_icon, icon, icon)
    # Apply the new alpha channel to the image
    flood_icon.putalpha(new_alpha)
    
    # Calculate row and column indices and paste flood icon on legend
    row_idx = i // num_cols
    col_idx = i % num_cols
    icon_x = (size+spacing)*col_idx*2+size//4
    icon_y = size*row_idx + size//4 * row_idx
    legend_img.paste(flood_icon, (icon_x, icon_y), flood_icon)
    
    # Add label text on the right of each flood warning icon
    label_width, label_height = draw.textsize(usage, font=font)
    label_x = icon_x + size + padding
    label_y = icon_y + (size - label_height)//2
    draw.text((label_x, label_y), usage, fill='black', font=font)

# Save legend image
legend_img.save(fp, dpi=(300, 300))