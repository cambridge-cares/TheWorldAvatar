<?xml version="1.0" encoding="UTF-8"?>

<StyledLayerDescriptor
    xmlns="http://www.opengis.net/sld"
    xmlns:ogc="http://www.opengis.net/ogc"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.0.0/StyledLayerDescriptor.xsd"
    version="1.0.0">

	<NamedLayer>
		<Name>uk-dem-style</Name>
		<UserStyle>
			<Name>uk-dem-style</Name>
			<Title>uk-dem-style</Title>
			<Abstract>Raster style for DEM data.</Abstract>
			<FeatureTypeStyle>
				<FeatureTypeName>Feature</FeatureTypeName>
				<Rule>
					<RasterSymbolizer>
						<ColorMap>
							<ColorMapEntry color="#AAFFAA" quantity="0" label="values" />
							<ColorMapEntry color="#00FF00" quantity="40"/>
							<ColorMapEntry color="#FFFF00" quantity="80" label="values" />
							<ColorMapEntry color="#FF7F00" quantity="120" label="values" />
							<ColorMapEntry color="#BF7F3F" quantity="160" label="values" />
							<ColorMapEntry color="#000000" quantity="200" label="values" />
						</ColorMap>
						<ContrastEnhancement>
						<Normalize />
							<GammaValue>0.5</GammaValue>
						</ContrastEnhancement>
					</RasterSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>
