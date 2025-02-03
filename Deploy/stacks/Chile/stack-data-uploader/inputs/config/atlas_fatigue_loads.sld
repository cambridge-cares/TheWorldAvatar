<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:sld="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc" version="1.0.0">
  <UserLayer>
    <sld:LayerFeatureConstraints>
      <sld:FeatureTypeConstraint/>
    </sld:LayerFeatureConstraints>
    <sld:UserStyle>
      <sld:Name>atlas_fatigue_loads</sld:Name>
      <sld:FeatureTypeStyle>
        <sld:Rule>
          <sld:RasterSymbolizer>
            <sld:ChannelSelection>
              <sld:GrayChannel>
                <sld:SourceChannelName>1</sld:SourceChannelName>
              </sld:GrayChannel>
            </sld:ChannelSelection>
            <sld:ColorMap type="values">
              <sld:ColorMapEntry color="#ffffff" quantity="255.0" opacity="0"/>
              <sld:ColorMapEntry color="#30123b" label="1A+" quantity="0.0"/>
              <sld:ColorMapEntry color="#702b8c" label="1A" quantity="1.0"/>
              <sld:ColorMapEntry color="#d140de" label="1B" quantity="2.0"/>
              <sld:ColorMapEntry color="#ff95e3" label="1C" quantity="3.0"/>
              <sld:ColorMapEntry color="#005c0c" label="2A+" quantity="4.0"/>
              <sld:ColorMapEntry color="#00a816" label="2A" quantity="5.0"/>
              <sld:ColorMapEntry color="#00c71b" label="2B" quantity="6.0"/>
              <sld:ColorMapEntry color="#3dd873" label="2C" quantity="7.0"/>
              <sld:ColorMapEntry color="#f80004" label="3A+" quantity="8.0"/>
              <sld:ColorMapEntry color="#eacb3f" label="3A" quantity="9.0"/>
              <sld:ColorMapEntry color="#fff45d" label="3B" quantity="10.0"/>
              <sld:ColorMapEntry color="#fff45d" label="3C" quantity="11.0"/>
              <sld:ColorMapEntry color="#f77a21" label="C" quantity="12.0"/>
            </sld:ColorMap>
            <sld:VendorOption name="brightness">0.519608</sld:VendorOption>
          </sld:RasterSymbolizer>
        </sld:Rule>
      </sld:FeatureTypeStyle>
    </sld:UserStyle>
  </UserLayer>
</StyledLayerDescriptor>

