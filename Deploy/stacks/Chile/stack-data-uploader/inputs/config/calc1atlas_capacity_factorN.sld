<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" version="1.0.0" xmlns:gml="http://www.opengis.net/gml" xmlns:sld="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
  <UserLayer>
    <sld:LayerFeatureConstraints>
      <sld:FeatureTypeConstraint/>
    </sld:LayerFeatureConstraints>
    <sld:UserStyle>
      <sld:Name>calc1_atlas_capacity_factor1Normal</sld:Name>
      <sld:FeatureTypeStyle>
        <sld:Rule>
          <sld:RasterSymbolizer>
            <sld:ChannelSelection>
              <sld:GrayChannel>
                <sld:SourceChannelName>1</sld:SourceChannelName>
              </sld:GrayChannel>
            </sld:ChannelSelection>
            <sld:ColorMap type="intervals">
              <sld:ColorMapEntry color="#d71bb5" quantity="0.29999999999999999" label="&lt;= 0.3" opacity="0.33"/>
              <sld:ColorMapEntry color="#ffff00" quantity="0.45000000000000001" label="0.3332 - 0.45" opacity="0.33"/>
              <sld:ColorMapEntry color="#14fced" quantity="0.75" label="0.4500 - 0.75" opacity="0.33"/>
              <sld:ColorMapEntry color="#d71bb5" quantity="1" label="> 0.7500" opacity="0.33"/>
            </sld:ColorMap>
          </sld:RasterSymbolizer>
        </sld:Rule>
      </sld:FeatureTypeStyle>
    </sld:UserStyle>
  </UserLayer>
</StyledLayerDescriptor>

