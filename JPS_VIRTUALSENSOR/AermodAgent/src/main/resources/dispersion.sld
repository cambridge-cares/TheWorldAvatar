<?xml version="1.0" encoding="ISO-8859-1"?>
<StyledLayerDescriptor version="1.0.0" xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.0.0/StyledLayerDescriptor.xsd" xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>dispersion_style</Name>
    <UserStyle>
      <Title>dispersion_style</Title>
      <FeatureTypeStyle>
        <Rule>
          <PolygonSymbolizer>
            <Fill>
              <CssParameter name="fill"><ogc:PropertyName>fill</ogc:PropertyName></CssParameter>
              <CssParameter name="fill-opacity"><ogc:PropertyName>fill-opacity</ogc:PropertyName></CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke"><ogc:PropertyName>stroke</ogc:PropertyName></CssParameter>
              <CssParameter name="stroke-width"><ogc:PropertyName>stroke-width</ogc:PropertyName></CssParameter>
			        <CssParameter name="stroke-opacity"><ogc:PropertyName>stroke-opacity</ogc:PropertyName></CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>