// Import necessary Mapbox GL JS types for TypeScript
import mapboxgl from 'mapbox-gl';

/**
 * Function to add a click event listener to a Mapbox map.
 * @param map The Mapbox map object to attach the event listener to.
 */
export function addMapClickListener(map: mapboxgl.Map): void {
    map.on('click', (event) => {
      const features = map.queryRenderedFeatures(event.point, {
        // Specify layers if you know which layers contain your icons
        // layers: ['your-icon-layer-id']
      });
  
      if (features.length > 0) {
        // Assuming your icons have properties you want to log or use
        console.log('Clicked feature:', features[0].properties);
  
        // For example, displaying a popup with feature's name
        new mapboxgl.Popup()
          .setLngLat(event.lngLat)
          .setHTML(`<h1>${features[0].properties.name}</h1>`) // Replace 'name' with the relevant property key
          .addTo(map);
      } else {
        // Log lat, lng if no features were clicked
        console.log(`Map clicked at latitude: ${event.lngLat.lat}, longitude: ${event.lngLat.lng}`);
      }
    });
  }