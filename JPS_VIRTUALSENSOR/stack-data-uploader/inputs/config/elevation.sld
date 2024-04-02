<?xml version="1.0" encoding="ISO-8859-1"?>
   <StyledLayerDescriptor version="1.0.0"
     xsi:schemaLocation="http://www.opengis.net/sld StyledLayerDescriptor.xsd"
     xmlns="http://www.opengis.net/sld"
     xmlns:ogc="http://www.opengis.net/ogc"
     xmlns:xlink="http://www.w3.org/1999/xlink"
     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
     <NamedLayer>
       <Name>elevation_style</Name>
       <UserStyle>
         <Title>elevation_style</Title>
         <Abstract>Extracts contours from DEM</Abstract>
         <FeatureTypeStyle>
           <Transformation>
             <ogc:Function name="ras:Contour">
               <ogc:Function name="parameter">
                 <ogc:Literal>data</ogc:Literal>
               </ogc:Function>
               <ogc:Function name="parameter">
                 <ogc:Literal>levels</ogc:Literal>
                 <ogc:Literal>200</ogc:Literal>
                 <ogc:Literal>220</ogc:Literal>
                 <ogc:Literal>240</ogc:Literal>
                 <ogc:Literal>260</ogc:Literal>
                 <ogc:Literal>280</ogc:Literal>
                 <ogc:Literal>300</ogc:Literal>
                 <ogc:Literal>320</ogc:Literal>
                 <ogc:Literal>340</ogc:Literal>
				 <ogc:Literal>360</ogc:Literal>
				 <ogc:Literal>380</ogc:Literal>
				 <ogc:Literal>400</ogc:Literal>
               </ogc:Function>
             </ogc:Function>
           </Transformation>
           <Rule>
             <Name>rule1</Name>
             <Title>Contour Line</Title>
             <LineSymbolizer>
               <Stroke>
                 <CssParameter name="stroke">#000000</CssParameter>
                 <CssParameter name="stroke-width">1</CssParameter>
               </Stroke>
             </LineSymbolizer>
             <TextSymbolizer>
               <Label>
                 <ogc:PropertyName>value</ogc:PropertyName>
               </Label>
               <Font>
                 <CssParameter name="font-family">Arial</CssParameter>
                 <CssParameter name="font-style">Normal</CssParameter>
                 <CssParameter name="font-size">10</CssParameter>
               </Font>
               <LabelPlacement>
                 <LinePlacement/>
               </LabelPlacement>
               <Halo>
                 <Radius>
                   <ogc:Literal>2</ogc:Literal>
                 </Radius>
                 <Fill>
                   <CssParameter name="fill">#FFFFFF</CssParameter>
                   <CssParameter name="fill-opacity">0.6</CssParameter>
                 </Fill>
               </Halo>
               <Fill>
                 <CssParameter name="fill">#000000</CssParameter>
               </Fill>
               <Priority>2000</Priority>
               <VendorOption name="followLine">true</VendorOption>
               <VendorOption name="repeat">100</VendorOption>
               <VendorOption name="maxDisplacement">50</VendorOption>
               <VendorOption name="maxAngleDelta">30</VendorOption>
             </TextSymbolizer>
           </Rule>
         </FeatureTypeStyle>
       </UserStyle>
     </NamedLayer>
    </StyledLayerDescriptor>