<?xml version="1.0" encoding="UTF-8"?><sld:StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:sld="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" version="1.0.0">
<sld:NamedLayer>
  <sld:Name>Cropmap Styler</sld:Name>
  <sld:UserStyle>
    <sld:Title>Cropmap Reduced Style</sld:Title>
    <sld:FeatureTypeStyle>
      <sld:Rule>
        <sld:Name>Heathland and Bracken</sld:Name>
        <ogc:Filter>
          <ogc:PropertyIsEqualTo>
            <ogc:PropertyName>lucode</ogc:PropertyName>
            <ogc:Literal>HE02</ogc:Literal>
          </ogc:PropertyIsEqualTo>
        </ogc:Filter>
        <sld:PolygonSymbolizer>
          <sld:Fill>
            <sld:CssParameter name="fill">
              #ba8591
            </sld:CssParameter>
          </sld:Fill>
        </sld:PolygonSymbolizer>
      </sld:Rule>
      <sld:Rule>
        <sld:Name>Perennial Crops and Isolated Trees</sld:Name>
        <ogc:Filter>
          <ogc:PropertyIsEqualTo>
            <ogc:PropertyName>lucode</ogc:PropertyName>
            <ogc:Literal>TC01</ogc:Literal>
          </ogc:PropertyIsEqualTo>
        </ogc:Filter>
        <sld:PolygonSymbolizer>
          <sld:Fill>
            <sld:CssParameter name="fill">
              #945200
            </sld:CssParameter>
          </sld:Fill>
        </sld:PolygonSymbolizer>
      </sld:Rule>
      <sld:Rule>
        <sld:Name>Trees, Woody Plants, Hedgerows</sld:Name>
        <ogc:Filter>
          <ogc:PropertyIsEqualTo>
            <ogc:PropertyName>lucode</ogc:PropertyName>
            <ogc:Literal>WO12</ogc:Literal>
          </ogc:PropertyIsEqualTo>
        </ogc:Filter>
        <sld:PolygonSymbolizer>
          <sld:Fill>
            <sld:CssParameter name="fill">
              #375623
            </sld:CssParameter>
          </sld:Fill>
        </sld:PolygonSymbolizer>
      </sld:Rule>
      <sld:Rule>
        <sld:Name>Short Rotation Coppice</sld:Name>
        <ogc:Filter>
          <ogc:PropertyIsEqualTo>
            <ogc:PropertyName>lucode</ogc:PropertyName>
            <ogc:Literal>SR01</ogc:Literal>
          </ogc:PropertyIsEqualTo>
        </ogc:Filter>
        <sld:PolygonSymbolizer>
          <sld:Fill>
            <sld:CssParameter name="fill">
              #a7ab3d
            </sld:CssParameter>
          </sld:Fill>
        </sld:PolygonSymbolizer>
      </sld:Rule>
      <sld:Rule>
        <sld:Name>Unknown or Mixed Vegetation</sld:Name>
        <ogc:Filter>
          <ogc:PropertyIsEqualTo>
            <ogc:PropertyName>lucode</ogc:PropertyName>
            <ogc:Literal>AC00</ogc:Literal>
          </ogc:PropertyIsEqualTo>
        </ogc:Filter>
        <sld:PolygonSymbolizer>
          <sld:Fill>
            <sld:CssParameter name="fill">
              #ff9300
            </sld:CssParameter>
          </sld:Fill>
        </sld:PolygonSymbolizer>
      </sld:Rule>
    </sld:FeatureTypeStyle>
  </sld:UserStyle>
</sld:NamedLayer>
</sld:StyledLayerDescriptor>
