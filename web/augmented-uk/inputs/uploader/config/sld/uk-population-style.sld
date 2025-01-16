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
							<ColorMapEntry color="#000000" opacity="0.0" quantity="0.99"/>
							<ColorMapEntry color="#BF94BC" opacity="0.25" quantity="1.00"/>
							<ColorMapEntry color="#513452" opacity="0.85" quantity="500.00"/>
						</ColorMap>
						<ContrastEnhancement/>
					</RasterSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>
