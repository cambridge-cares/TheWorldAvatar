<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" version="1.0.0" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:sld="http://www.opengis.net/sld">
  <UserLayer>
    <sld:LayerFeatureConstraints>
      <sld:FeatureTypeConstraint/>
    </sld:LayerFeatureConstraints>
    <sld:UserStyle>
      <sld:Name>atlas_elevationWithBathymetry_style</sld:Name>
      <sld:FeatureTypeStyle>
        <sld:Rule>
          <sld:RasterSymbolizer>
            <sld:ChannelSelection>
              <sld:GrayChannel>
                <sld:SourceChannelName>1</sld:SourceChannelName>
              </sld:GrayChannel>
            </sld:ChannelSelection>
            <sld:ColorMap type="ramp">
              <sld:ColorMapEntry label="-8250" quantity="-8250" color="#9c0b0b"/>
              <sld:ColorMapEntry label="-6000" quantity="-6000" color="#ee0000"/>
              <sld:ColorMapEntry label="-4000" quantity="-4000" color="#fa4e33"/>
              <sld:ColorMapEntry label="-2000" quantity="-2000" color="#ff8945"/>
              <sld:ColorMapEntry label="0" quantity="0" color="#ffe5bf"/>
              <sld:ColorMapEntry label="2000" quantity="2000" color="#74adb4"/>
              <sld:ColorMapEntry label="4000" quantity="4000" color="#0c51b7"/>
            </sld:ColorMap>
          </sld:RasterSymbolizer>
        </sld:Rule>
      </sld:FeatureTypeStyle>
    </sld:UserStyle>
  </UserLayer>
</StyledLayerDescriptor>
