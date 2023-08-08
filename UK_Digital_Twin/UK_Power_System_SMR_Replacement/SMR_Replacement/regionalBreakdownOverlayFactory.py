from PIL import Image

def energyBreakdownOverlayerFactory(base_image_path, overlay_image_folderPath, arrow_image_path, legend_path):

    LACodeList = ['E12000001', 'E12000002', 'E12000003', 'E12000004', 'E12000005', 'E12000006', 'E12000007', 'E12000008', 'E12000009', 'S92000003', 'W92000004']
    locationList = [[800, 100], [600, 400], [950, 780], [1050, 1200], [700, 1350], [1370, 1480], [1470, 1850], [1020, 1580], [310, 1900], [320, 190], [410, 1050]]
    
    # Open the base image
    base_image = Image.open(base_image_path)
    # Open the arrow image
    arrow_image = Image.open(arrow_image_path)
    arrow_image = arrow_image.convert("RGBA")
    # Open the legend image
    legend_image = Image.open(legend_path)

    # Open the image to be added
    for i, LACode in enumerate(LACodeList):
        overlay_image = Image.open(overlay_image_folderPath + "RegionalEnergyBreakdown_PieChart_%s.png"%LACode)

        # Resize the overlay image
        width, height = overlay_image.size
        new_size = (int(width * 0.04), int(height * 0.04))
        resized_overlay_image = overlay_image.resize(new_size)
        resized_overlay_image = resized_overlay_image.convert("RGBA")
        # Set the coordinates for where to place the overlay
        x, y = locationList[i][0], locationList[i][1]

        # Add the overlay to the base image
        base_image.alpha_composite(resized_overlay_image, dest=(x, y))
    
    # Add the arrow to the base image
    base_image.alpha_composite(arrow_image, dest=(1360, 1720))

    # # Add the legend to the base image
    # width, height = legend_image.size
    # new_size = (int(width * 2.6), int(height * 2.6))
    # legend_image = legend_image.resize(new_size)
    # base_image.alpha_composite(legend_image, dest=(440, 2150))

    # Save the new image
    base_image.save(overlay_image_folderPath + "OVERLAY_RegionalAreaEnergyBreakdown.png")

    print(overlay_image_folderPath + "OVERLAY_RegionalAreaEnergyBreakdown is created. ")
    return


if __name__ == '__main__':  
    base_image_path = "/mnt/d/wx243/FromTWA/RegionalBreakdown_images/UKMapWithRegionalBoundaries.png"
    arrow_image_path = "/mnt/d/wx243/FromTWA/RegionalBreakdown_images/arrow.png"
    legend_path = "/mnt/d/wx243/FromTWA/RegionalBreakdown_images/breakdownLegend.png"

    overlay_image_folderPath = "/mnt/d/wx243/FromTWA/regionalEnergyBreakdownPieChart/20230628-0326/RegionalAreaEnergyBreakdown/SMR_32_CarbonTax_100_weatherCondition_WBSB_weight_0.5/"
    energyBreakdownOverlayerFactory(base_image_path, overlay_image_folderPath, arrow_image_path, legend_path)



