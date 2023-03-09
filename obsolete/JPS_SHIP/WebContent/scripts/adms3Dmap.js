const listGeoJsonAddedToOSMB = [] // edit

const controlButtonsSetter = osmb => {
  var controlButtons = document.querySelectorAll('.control button')
  for (var i = 0, il = controlButtons.length; i < il; i++) {
    controlButtons[i].addEventListener('click', function (e) {
      var button = this
      var parentClassList = button.parentNode.classList
      var direction = button.classList.contains('inc') ? 1 : -1
      var increment
      var property

      if (parentClassList.contains('tilt')) {
        property = 'Tilt'
        increment = direction * 10
      }
      if (parentClassList.contains('rotation')) {
        property = 'Rotation'
        increment = direction * 10
      }
      if (parentClassList.contains('zoom')) {
        property = 'Zoom'
        increment = direction * 1
      }
      if (property) {
        osmb['set' + property](osmb['get' + property]() + increment)
      }
    })
  }
}
//let sensorIRIs = [{x:103.83143122477935 , y:1.2429458210894155, name:"test"}];
//todo: add render sensor function
const initadms3dmap = (
  list, range, osmb, location, coordinatesMid, cityiri, shipList, folder) => {
  //TODO:add: initi render sensor

  for (obj of listGeoJsonAddedToOSMB) {
    obj.destroy()
  }

  proj4.defs('EPSG:3857',
    '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs')
  proj4.defs('WGS84',
    '+title=WGS 84 (long/lat) +proj=longlat +ellps=WGS84 +datum=WGS84 +units=degrees')

  const parsedLowLeft = proj4('EPSG:3857', 'WGS84', [range[0], range[2]])
  const parsedTopRight = proj4('EPSG:3857', 'WGS84', [range[1], range[3]])
  let lowLeft = [], topRight = []
  lowLeft[0] = Math.min(parsedLowLeft[0], parsedTopRight[0])
  lowLeft[1] = Math.min(parsedLowLeft[1], parsedTopRight[1])
  topRight[0] = Math.max(parsedLowLeft[0], parsedTopRight[0])
  topRight[1] = Math.max(parsedLowLeft[1], parsedTopRight[1])

  const position = {}

  position.latitude = coordinatesMid[0]
  position.longitude =coordinatesMid[1]

  osmb.setPosition(position)
  osmb.setZoom(15.7)
  osmb.setTilt(45.0)

  // --- Rendering 3D building models --- //
  console.log('START')

  //TODO: modify back later
  $.getJSON('/JPS/ADMSHelper',
  //$.getJSON('/geo',
      {
      listOfIRIs: JSON.stringify(list),
      cityiri,
    },
    function (data) {
      var geojson = data
      var arrayLength = geojson.length
      console.log(data)

      for (var i = 0; i < arrayLength; i++) {

        try {
          listGeoJsonAddedToOSMB.push(osmb.addGeoJSON(geojson[i])) // edit
          console.log(JSON.stringify(geojson[i], null, 4))
        } catch (err) {
          console.log(err.name)
        }
      }
    })

  // --- Rendering 3D ship models --- //
  for (ship of shipList.collection.items) {
    let geoShip = {
      type: 'FeatureCollection',
      features: [
        {
          type: 'Feature',
          properties: {
            color: 'black',
            roofColor: 'black',
            height: 5,
            minHeight: 0,
          },
          geometry: {
            type: 'Polygon',
            coordinates: [
              [
                [ship.lon + 0.0001, ship.lat],
                [ship.lon, ship.lat - 0.0001],
                [ship.lon - 0.0001, ship.lat],
                [ship.lon, ship.lat + 0.0001],
                [ship.lon + 0.0001, ship.lat],
              ],
            ],
          },
        }],
    }
    listGeoJsonAddedToOSMB.push(osmb.addGeoJSON(geoShip))
  }

  // --- Rendering 3D layer --- //
  let optionWrapperNode = document.getElementById('optionwrapper')
  while (optionWrapperNode.firstChild) {
    optionWrapperNode.removeChild(optionWrapperNode.firstChild)
  }

  var geojson = {
    type: 'FeatureCollection',
    features: [
      {
        type: 'Feature',
        properties: {
          height: 0,
          minHeight: 0,
        },
        geometry: {
          type: 'Polygon',
          coordinates: [//TODO:ã€€LINK THIS TO USER INPUT
            [
              [topRight[0], lowLeft[1]],
              [topRight[0], topRight[1]],
              [lowLeft[0], topRight[1]],
              [lowLeft[0], lowLeft[1]],
              [topRight[0], lowLeft[1]],
            ],
          ],
        },
      }],
  }
//change back after local test: '/JPS/ADMSOutputAllForShips'
  //local test: /result
  getContourMaps('/JPS/ADMSOutputAllForShips', folder).then(data => {
	const dataurls = data[0]
	const POL_LIST = data[1]
	const POL_NUM = data[2]
	const HEIGHT_NUM = data[3]
	const HEIGHT_GAP=data[4]
	const INITIAL_HEIGHT=data[5]
	makeRadios('optionwrapper', POL_LIST, 'Select a pollutant:')
    const LEGEND_WRAPPER = 'legendwrapper'
    const SLIDER_WRAPPER = 'sliderwrapper'
    let legendWrapperNode = document.getElementById(LEGEND_WRAPPER)
    let sliderWrapperNode = document.getElementById(SLIDER_WRAPPER)
    let idxSrc = 0, idxH = 0, preObj
    let image = dataurls[0][0]
    let thresholds = dataurls[0][1], color = dataurls[0][2]

    while (legendWrapperNode.firstChild) {
      legendWrapperNode.removeChild(legendWrapperNode.firstChild)
    }
    makeLegend(LEGEND_WRAPPER, thresholds, color)

    //  var dataurls = data.dataurls, heights =data.heights
    preObj = osmb.addGeoJSON(geojson,
      { elevation: 0, hasTexture: dataurls[0][0] })
    listGeoJsonAddedToOSMB.push(preObj) // edit

    while (sliderWrapperNode.firstChild) {
      sliderWrapperNode.removeChild(sliderWrapperNode.firstChild)
    }

    $('.radiogroup').change(function () {
      let radioValue = $('input[name=\'radio\']:checked').val()
      idxSrc = POL_LIST.indexOf(radioValue)
      thresholds = dataurls[idxH * POL_NUM + idxSrc][1]
      color = dataurls[idxH * POL_NUM + idxSrc][2]
      image = dataurls[idxH * POL_NUM + idxSrc][0]

      if (preObj) preObj.destroy()

      while (legendWrapperNode.firstChild) {
        legendWrapperNode.removeChild(legendWrapperNode.firstChild)
      }
      makeLegend(LEGEND_WRAPPER, thresholds, color)

      preObj = osmb.addGeoJSON(geojson, {
        elevation: HEIGHT_INTERVAL * (idxH),
        hasTexture: image,
      })
      listGeoJsonAddedToOSMB.push(preObj) // edit
    })

    //init at zero position
    makeSlider(SLIDER_WRAPPER, HEIGHT_NUM, function (event, ui) {
      if (preObj) preObj.destroy()
      idxH = ui.value
      $('#height-show').val(INITIAL_HEIGHT+idxH * HEIGHT_GAP)
      thresholds = dataurls[idxH * POL_NUM + idxSrc][1]
      color = dataurls[idxH * POL_NUM + idxSrc][2]
      image = dataurls[idxH * POL_NUM + idxSrc][0]

      while (legendWrapperNode.firstChild) {
        legendWrapperNode.removeChild(legendWrapperNode.firstChild)
      }
      makeLegend(LEGEND_WRAPPER, thresholds, color)

      preObj = osmb.addGeoJSON(geojson, {
        elevation: HEIGHT_INTERVAL * (idxH),
        hasTexture: image,
      })
      listGeoJsonAddedToOSMB.push(preObj)
    })
  }, err => {console.log(err)})

}