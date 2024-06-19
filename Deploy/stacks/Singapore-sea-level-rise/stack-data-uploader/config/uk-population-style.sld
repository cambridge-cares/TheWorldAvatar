<?xml version="1.0" encoding="UTF-8"?>

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
                            <!-- Define discrete ColorMapEntries for values from 0 to 50 in steps of 5 with opacity 0.5 -->
                            <ColorMapEntry color="#fff5f0" quantity="0" label="0" opacity="0.5"/>
                            <ColorMapEntry color="#fee5d8" quantity="5" label="5" opacity="0.5"/>
                            <ColorMapEntry color="#fdcab5" quantity="10" label="10" opacity="0.5"/>
                            <ColorMapEntry color="#fcab8f" quantity="15" label="15" opacity="0.5"/>
                            <ColorMapEntry color="#fc8a6a" quantity="20" label="20" opacity="0.5"/>
                            <ColorMapEntry color="#fb694a" quantity="25" label="25" opacity="0.5"/>
                            <ColorMapEntry color="#f14432" quantity="30" label="30" opacity="0.5"/>
                            <ColorMapEntry color="#d92523" quantity="35" label="35" opacity="0.5"/>
                            <ColorMapEntry color="#bc141a" quantity="40" label="40" opacity="0.5"/>
                            <ColorMapEntry color="#980c13" quantity="45" label="45" opacity="0.5"/>
                            <ColorMapEntry color="#67000d" quantity="50" label="50" opacity="0.5"/>
                        </ColorMap>
					</RasterSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>
