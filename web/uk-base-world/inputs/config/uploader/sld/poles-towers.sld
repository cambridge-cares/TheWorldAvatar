<?xml version="1.0" encoding="UTF-8"?>
<sld:StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:sld="http://www.opengis.net/sld"
  xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" version="1.0.0">
  <sld:NamedLayer>
    <sld:Name>Default Styler</sld:Name>
    <sld:UserStyle>
      <sld:FeatureTypeStyle>
        <sld:Name>name</sld:Name>
        <sld:Rule>
          <sld:PointSymbolizer>
            <sld:Graphic>
              <ExternalGraphic xmlns="http://www.opengis.net/sld"
                xmlns:xlink="http://www.w3.org/1999/xlink">
                <OnlineResource xlink:type="simple"
                  xlink:href="http://localhost:8080/geoserver/www/icons/${icon}.png" />
                <Format>image/png</Format>
              </ExternalGraphic>
              <sld:Size>
                <ogc:PropertyName>icon_size</ogc:PropertyName>
              </sld:Size>
            </sld:Graphic>
          </sld:PointSymbolizer>
        </sld:Rule>
      </sld:FeatureTypeStyle>
    </sld:UserStyle>
  </sld:NamedLayer>
</sld:StyledLayerDescriptor>