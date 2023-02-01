<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="1.0.0" xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.0.0/StyledLayerDescriptor.xsd">
  <NamedLayer>
    <Name>elevation</Name>
    <UserStyle>
      <Name>elevation</Name>
      <Title>Simple DEM style</Title>
      <Abstract>Classic elevation color progression</Abstract>
      <FeatureTypeStyle>
        <Rule>
          <RasterSymbolizer>
            <Opacity>0.3</Opacity>
            <ColorMap>
              <ColorMapEntry color="#AAFFAA" quantity="-100" />
              <ColorMapEntry color="#AAFFAA" quantity="-0.00000002" />
              <ColorMapEntry color="#AAFFAA" quantity="-0.00000001" opacity="0" />
              <ColorMapEntry color="#AAFFAA" quantity="0.0" opacity="0" />
              <ColorMapEntry color="#AAFFAA" quantity="0.00000001" opacity="0" />
              <ColorMapEntry color="#AAFFAA" quantity="0.00000002" />
              <ColorMapEntry color="#00FF00" quantity="400" />
              <ColorMapEntry color="#FFFF00" quantity="800" />
              <ColorMapEntry color="#FF7F00" quantity="1200" />
              <ColorMapEntry color="#BF7F3F" quantity="1600" />
              <ColorMapEntry color="#000000" quantity="2000" />
            </ColorMap>
          </RasterSymbolizer>
        </Rule>
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>