<?xml version="1.0" encoding="UTF-8"?>
<sld:StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:sld="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" version="1.0.0">
  <sld:NamedLayer>
    <sld:Name>Forestry Styler</sld:Name>
    <sld:UserStyle>
      <sld:Title>Forestry Reduced Style</sld:Title>
      <sld:FeatureTypeStyle>
        <sld:Rule>
          <sld:Name>Light Woodland</sld:Name>
          <ogc:Filter>
            <ogc:Or>
              <ogc:PropertyIsEqualTo>
                <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                <ogc:Literal>Low density</ogc:Literal>
              </ogc:PropertyIsEqualTo>
              <ogc:PropertyIsEqualTo>
                <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                <ogc:Literal>Shrub</ogc:Literal>
              </ogc:PropertyIsEqualTo>
            </ogc:Or>
          </ogc:Filter>
          <sld:PolygonSymbolizer>
            <sld:Fill>
              <sld:CssParameter name="fill">
              #ced4ea
            </sld:CssParameter>
            </sld:Fill>
          </sld:PolygonSymbolizer>
        </sld:Rule>
        <sld:Rule>
          <sld:Name>Woodland</sld:Name>
          <ogc:Filter>
            <ogc:Or>
              <ogc:PropertyIsEqualTo>
                <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                <ogc:Literal>Mixed mainly broadleaved</ogc:Literal>
              </ogc:PropertyIsEqualTo>
              <ogc:Or>
                <ogc:PropertyIsEqualTo>
                  <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                  <ogc:Literal>Mixed mainly conifer</ogc:Literal>
                </ogc:PropertyIsEqualTo>
                <ogc:Or>
                  <ogc:PropertyIsEqualTo>
                    <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                    <ogc:Literal>Broadleaved</ogc:Literal>
                  </ogc:PropertyIsEqualTo>
                  <ogc:PropertyIsEqualTo>
                    <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                    <ogc:Literal>Conifer</ogc:Literal>
                  </ogc:PropertyIsEqualTo>
                </ogc:Or>
              </ogc:Or>
            </ogc:Or>
          </ogc:Filter>
          <sld:PolygonSymbolizer>
            <sld:Fill>
              <sld:CssParameter name="fill">
              #83baa6
            </sld:CssParameter>
            </sld:Fill>
          </sld:PolygonSymbolizer>
        </sld:Rule>
        <sld:Rule>
          <sld:Name>Young Woodland</sld:Name>
          <ogc:Filter>
            <ogc:Or>
              <ogc:PropertyIsEqualTo>
                <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                <ogc:Literal>Coppice with standards</ogc:Literal>
              </ogc:PropertyIsEqualTo>
              <ogc:Or>
                <ogc:PropertyIsEqualTo>
                  <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                  <ogc:Literal>Young trees</ogc:Literal>
                </ogc:PropertyIsEqualTo>
                <ogc:PropertyIsEqualTo>
                  <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                  <ogc:Literal>Coppice</ogc:Literal>
                </ogc:PropertyIsEqualTo>
              </ogc:Or>
            </ogc:Or>
          </ogc:Filter>
          <sld:PolygonSymbolizer>
            <sld:Fill>
              <sld:CssParameter name="fill">
              #cadea5
            </sld:CssParameter>
            </sld:Fill>
          </sld:PolygonSymbolizer>
        </sld:Rule>
        <sld:Rule>
          <sld:Name>Uncertain</sld:Name>
          <ogc:Filter>
            <ogc:Or>
              <ogc:PropertyIsEqualTo>
                <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                <ogc:Literal>Uncertain</ogc:Literal>
              </ogc:PropertyIsEqualTo>
              <ogc:Or>
                <ogc:PropertyIsEqualTo>
                  <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                  <ogc:Literal>Assumed woodland</ogc:Literal>
                </ogc:PropertyIsEqualTo>
                <ogc:PropertyIsEqualTo>
                  <ogc:PropertyName>IFT_IOA</ogc:PropertyName>
                  <ogc:Literal>Cloud \ shadow</ogc:Literal>
                </ogc:PropertyIsEqualTo>
              </ogc:Or>
            </ogc:Or>
          </ogc:Filter>
          <sld:PolygonSymbolizer>
            <sld:Fill>
              <sld:CssParameter name="fill">
              #b3b3b3
            </sld:CssParameter>
            </sld:Fill>
          </sld:PolygonSymbolizer>
        </sld:Rule>
      </sld:FeatureTypeStyle>
    </sld:UserStyle>
  </sld:NamedLayer>
</sld:StyledLayerDescriptor>