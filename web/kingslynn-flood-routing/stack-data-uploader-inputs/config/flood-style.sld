<?xml version="1.0" encoding="UTF-8"?>
<sld:StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:sld="http://www.opengis.net/sld"
  xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" version="1.0.0">
  <sld:NamedLayer>
    <sld:Name>Pluvial Style</sld:Name>
    <sld:UserStyle>
      <sld:Name>Pluvial Style</sld:Name>
      <sld:Title>Flood map raster</sld:Title>
      <sld:Abstract>A sample style for rasters</sld:Abstract>
      <sld:FeatureTypeStyle>
        <sld:Name>name</sld:Name>
        <sld:FeatureTypeName>Feature</sld:FeatureTypeName>
        <sld:Rule>
          <sld:RasterSymbolizer>
            <sld:ColorMap>
              <ColorMapEntry color="#d4fffb" quantity="0" label="shallow" opacity="0.4" />
              <ColorMapEntry color="#0000FF" quantity="0.5" label="deep" opacity="0.6" />
            </sld:ColorMap>
            <sld:ContrastEnhancement />
          </sld:RasterSymbolizer>
        </sld:Rule>
      </sld:FeatureTypeStyle>
    </sld:UserStyle>
  </sld:NamedLayer>
</sld:StyledLayerDescriptor>