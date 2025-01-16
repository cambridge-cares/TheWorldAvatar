<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" version="1.0.0" xmlns:gml="http://www.opengis.net/gml" xmlns:sld="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
  <UserLayer>
    <sld:LayerFeatureConstraints>
      <sld:FeatureTypeConstraint/>
    </sld:LayerFeatureConstraints>
    <sld:UserStyle>
      <sld:Name>atlas_capacity_factor1Normal</sld:Name>
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
              <sld:ColorMapEntry color="#000004" quantity="0.0000000000000001"/>
              <sld:ColorMapEntry label="0.25" quantity="0.25" color="#000004"/>
              <sld:ColorMapEntry label="0.45" quantity="0.45264423076923077" color="#6a1c81"/>
              <sld:ColorMapEntry label="0.65" quantity="0.64807692307692299" color="#e75263"/>
              <sld:ColorMapEntry label="0.85" quantity="0.84999999999999998" color="#fcfdbf"/>
            </sld:ColorMap>
          </sld:RasterSymbolizer>
        </sld:Rule>
      </sld:FeatureTypeStyle>
    </sld:UserStyle>
  </UserLayer>
</StyledLayerDescriptor>

