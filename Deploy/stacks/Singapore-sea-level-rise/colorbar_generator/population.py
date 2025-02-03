import matplotlib.pyplot as plt
import matplotlib as mpl
from mycolorpy import colorlist as mcp


def create_colors(bounds, name):
    fig2, ax = plt.subplots(figsize=(6, 1))
    fig2.subplots_adjust(bottom=0.5)
    color1 = mcp.gen_color(cmap="Reds", n=len(bounds))

    colour_pairs = []
    for i in range(len(bounds)):
        colour_pairs.append((color1[i], bounds[i]))

    sld_content = generate_sld(colour_pairs)

    f = open("population_{name}.sld".format(name=name), "w")
    f.write(sld_content)
    f.close()

    cmap = mpl.colors.ListedColormap(color1)

    norm = mpl.colors.BoundaryNorm(bounds, cmap.N)

    cb2 = mpl.colorbar.ColorbarBase(ax, cmap=cmap,
                                    norm=norm,
                                    orientation='horizontal')
    cb2.set_label(
        'Population density ({name}) [10$^4$#/km$^2$]'.format(name=name.replace('_', '/')))
    plt.savefig('population_{name}.png'.format(name=name), bbox_inches='tight',
                transparent=True, dpi=300)


def generate_sld(color_quantity_pairs):
    sld_template = """<?xml version="1.0" encoding="UTF-8"?>

<StyledLayerDescriptor
    xmlns="http://www.opengis.net/sld"
    xmlns:ogc="http://www.opengis.net/ogc"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.0.0/StyledLayerDescriptor.xsd"
    version="1.0.0">

    <NamedLayer>
        <Name>uk-population-style</Name>
        <UserStyle>
            <Name>uk-population-style</Name>
            <Title>uk-population-style</Title>
            <Abstract>Raster style for UK Population data.</Abstract>
            <FeatureTypeStyle>
                <FeatureTypeName>Feature</FeatureTypeName>
                <Rule>
                    <RasterSymbolizer>
                        <ColorMap>
                            {colormap_entries}
                        </ColorMap>
                    </RasterSymbolizer>
                </Rule>
            </FeatureTypeStyle>
        </UserStyle>
    </NamedLayer>
</StyledLayerDescriptor>
"""

    colormap_entry_template = '<ColorMapEntry color="{color}" quantity="{quantity}" label="{quantity}" opacity="0.5"/>'

    colormap_entries = "\n                            ".join(
        [colormap_entry_template.format(
            color=color, quantity=quantity) for color, quantity in color_quantity_pairs]
    )

    sld_content = sld_template.format(colormap_entries=colormap_entries)
    return sld_content


if __name__ == "__main__":
    # general population
    bounds = range(0, 101, 10)
    create_colors(list(bounds), 'general')

    # men and women
    bounds = range(0, 51, 5)
    create_colors(list(bounds), 'men_women')

    bounds = range(0, 11, 1)
    create_colors(list(bounds), 'children_youth_elderly')
