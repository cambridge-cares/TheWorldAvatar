<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" version="1.0.0" xmlns:gml="http://www.opengis.net/gml" xmlns:sld="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
  <UserLayer>
    <sld:LayerFeatureConstraints>
      <sld:FeatureTypeConstraint/>
    </sld:LayerFeatureConstraints>
    <sld:UserStyle>
      <sld:Name>atlas_wind_100m0_025</sld:Name>
      <sld:FeatureTypeStyle>
        <sld:Rule>
          <sld:RasterSymbolizer>
            <sld:ChannelSelection>
              <sld:GrayChannel>
                <sld:SourceChannelName>1</sld:SourceChannelName>
              </sld:GrayChannel>
            </sld:ChannelSelection>
            <sld:ColorMap type="ramp">
              <sld:ColorMapEntry color="#ffffff" quantity="0" opacity="0"/>
              <sld:ColorMapEntry color="#2b83ba" quantity="0.0000000000000001"/>
              <sld:ColorMapEntry label="1.000" quantity="1" color="#2b83ba"/>
              <sld:ColorMapEntry label="4.125" quantity="4.125" color="#abdda4"/>
              <sld:ColorMapEntry label="7.250" quantity="7.25" color="#ffffbf"/>
              <sld:ColorMapEntry label="10.375" quantity="10.375" color="#fdae61"/>
              <sld:ColorMapEntry label="13.500" quantity="13.5" color="#d7191c"/>
            </sld:ColorMap>
          </sld:RasterSymbolizer>
        </sld:Rule>
      </sld:FeatureTypeStyle>
    </sld:UserStyle>
  </UserLayer>
</StyledLayerDescriptor>
