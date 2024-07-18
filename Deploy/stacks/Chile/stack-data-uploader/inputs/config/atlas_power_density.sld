<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" version="1.0.0" xmlns:gml="http://www.opengis.net/gml" xmlns:sld="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
  <UserLayer>
    <sld:LayerFeatureConstraints>
      <sld:FeatureTypeConstraint/>
    </sld:LayerFeatureConstraints>
    <sld:UserStyle>
      <sld:Name>power_density</sld:Name>
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
              <sld:ColorMapEntry color="#fff5f0" quantity="0.000000000000001" label="0.0000"/>
              <sld:ColorMapEntry label="260.0" quantity="260" color="#fee0d2"/>
              <sld:ColorMapEntry label="520.0" quantity="520" color="#fcbba1"/>
              <sld:ColorMapEntry label="780.0" quantity="780" color="#fc9272"/>
              <sld:ColorMapEntry label="1040.0" quantity="1040" color="#fb6a4a"/>
              <sld:ColorMapEntry label="1300.0" quantity="1300" color="#ef3b2c"/>
              <sld:ColorMapEntry label="1560.0" quantity="1560" color="#cb181d"/>
              <sld:ColorMapEntry label="1800.0" quantity="1800" color="#a50f15"/>
              <sld:ColorMapEntry label="2000.0" quantity="2000" color="#67000d"/>
            </sld:ColorMap>
          </sld:RasterSymbolizer>
        </sld:Rule>
      </sld:FeatureTypeStyle>
    </sld:UserStyle>
  </UserLayer>
</StyledLayerDescriptor>
