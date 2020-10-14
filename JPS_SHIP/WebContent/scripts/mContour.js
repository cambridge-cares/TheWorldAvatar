//Concentration contour map threshould number
const THRESHOULD_NUM = 8

//Height is faked, each plane displayed between an interval
const HEIGHT_INTERVAL = 5

function getContourMaps (address, folder) {

  return new Promise((resolve, reject) => {

    $.ajax({
      url: address,
      data: {
        folder: folder,
      },
      dataType: 'text',
    }).done(function (data) {
    	
    	data = JSON.parse(data)
	let d2result = data['grid']
	let POL_LIST = data['listofpol']  
    	let POL_NUM = data['numpol']
    	let HEIGHT_NUM = data['numheight']
    	let HEIGHT_GAP=data['numinterval']
    	let INITIAL_HEIGHT=data['initialheight']
    	
    	
      
      let bands = []
      //calculate global min max per polutant
      d2result.forEach(
        (level) => {
          let levbands = []
          let count = 0
          level.forEach(
            (polutant) => {
              let plmax = polutant.reduce(
                (max, num) => Math.max(max, num)
              )
              let plmin = polutant.reduce(
                (min, num) => {
                  if (min > 0 && num > 0) {
                    if (min > num) min = num
                  } else {
                    if (num > 0) min = num
                  }
                  return min
                },
                0
              )
              levbands.push([plmin, plmax])
            }
          )
          levbands.forEach((bandvals) => {
            let pvals = bands[count]
            if (!pvals) {
              bands.push(bandvals)
            } else {
              let min = Math.min(pvals[0], bandvals[0])
              let max = Math.max(pvals[1], bandvals[1])
              bands[count] = [min, max]
            }
            count++
          })
        }
      )

      //=============contour consts======================//
      d2result = d2Arr21d(d2result)//to 1d

      //=============contour map as svg for each  ======================//
      let level = 0
      let svgstrs = d2result.map((output) => {
        let svg = d3.select('#svgwrapper svg'),
          width = +svg.attr('width')
        let range = THRESHOULD_NUM
        let sqr_size = Math.sqrt(output.length)
        let ROW_NUM = sqr_size
        let COL_NUM = sqr_size
        let values = output
        let ubound = bands[level][1]
        let lbound = bands[level][0]
        if (!ubound > 0) {
          ubound = 1
          range = 1
        } else if (ubound === lbound) {
          ubound = ubound * THRESHOULD_NUM
        }
        level++
        if (level > POL_NUM - 1) level = 0

        const thresholdsC = d3.scaleLog() // [0, 1, 2, 3, 4, 5, 6, 7, 8]
          .domain([lbound, ubound]).range([0, range])
        let ticks = numberarray(range + 1)
        let thresholds = ticks.map((tik) => {return thresholdsC.invert(tik)})
        for (let i = thresholds.length; i--;) {
          if (thresholds[i] === 0) thresholds.splice(i, 1)
        }

        let color = d3.scaleLog(d3.interpolateRdYlBu).
          domain(thresholds).
          range([
            '#b0d6f9',
            '#3986ce',
            'rgba(216,217,162,0.62)',
            'rgba(255,254,78,0.65)',
            '#fee91c',
            '#ffce11',
            '#fc9708',
            '#d73027']).
          interpolate(d3.interpolateLab)

        //values = values.map((v) => enlarge(v))
        let contours = d3.contours().
          size([COL_NUM, ROW_NUM]).
          thresholds(thresholds)

        svg.selectAll('path').
          data(contours(values)).
          enter().
          append('path').
          attr('d', d3.geoPath(d3.geoIdentity().scale(width / COL_NUM))).
          attr('fill', function (d) {
            return color(d.value)
          })
        let svgstr = $('#svgwrapper').html()
        $('#svgwrapper svg').empty()//clear up and start again
        return [svgstr, thresholds, color]

      })

      //=========set up canvas for image conversion=========================//

      let canvas = $('#drawcanvas')[0]
      let context = canvas.getContext('2d')
      context.save();
      context.translate(0, canvas.height);
      context.scale(1, -1/originRatio);
      //========convert all svg strs to png images=============//

      let futureImages = svgstrs.map((svgstr) => {
        return svgToImagePromise(svgstr)
      })

      //======parse all image to dataurls=====================/

      Promise.all(futureImages).then(images => {
        let dataurls = images.map((image) => {

          context.fillStyle = 'white'

          context.fillRect(0, 0, canvas.width, canvas.height)
          context.drawImage(image[0], 0, 0)
          let dataurl = context.canvas.toDataURL('image/png')
          context.clearRect(0, 0, canvas.width, canvas.height)

          return [dataurl, image[1], image[2]]
        })
        console.log(dataurls)
        context.restore();
        resolve([dataurls,POL_LIST,POL_NUM,HEIGHT_NUM,HEIGHT_GAP,INITIAL_HEIGHT])

      }, err => {//todo: err handling
        reject(err)
      })

      //todo: this is the command for actualluy draw on map
      //osmb.addGeoJSON(geojson,{ elevation: HEIGHT_INTERVAL * ++numCount, hasTexture:dataurl});

    }).fail(function (err) {
        //todo: err handling
        reject(err)
      }
    )

  })

}

function numberarray (len) {
  let arr = []

  for (let i = 0; i < len; i++) {
    arr.push(i)
  }
  return arr
}


function svgToImagePromise (svgstr) {
  return new Promise((resolve, reject) => {
    svgToImage(svgstr[0], function (err, image) {
      if (err) {
        reject(err)
        return
      }
      resolve([image, svgstr[1], svgstr[2]])
    })
  })

}

function d2Arr21d (d2array) {
  let coned = []
  d2array.forEach((row) => {
    coned = coned.concat(row)
  })
  return coned

}

function makeLegend (selector_id, thresholds, color) {
  let range = thresholds.length - 1

  let thresholdScale = d3.scaleThreshold().
    domain(thresholds).
    range(['rgb(244, 244, 244)'].concat(
      d3.range(range).map(function (i) { return color(thresholds[i])})))


  let svg = d3.select('#' + selector_id).
    append('svg').
    style('height', '155px').
    style('width', '160px')

  svg.append('g').attr('class', 'legendQuant')

  let legend = d3.legendColor().
    labelFormat(d3.format('.3e')).
    labels(d3.legendHelpers.thresholdLabels).
    scale(thresholdScale)

  svg.select('.legendQuant').call(legend)

  $('text.label').append(' \u03BCg/m\u00B3') // micrograms/meter^3
}

function makeSlider (selector_id, levelnum, callback) {
  let parent = $('#' + selector_id)

  parent.append(
    '<p><label for="amount">Height:</label><input class="readonlyinput" type="text" id="height-show" readonly></p>')

  let slider = $('<div id=\'slider\'></div>').appendTo(parent).slider({
    min: 0,
    max: levelnum - 1,
    range: 'min',
    value: 0,
    orientation: 'vertical',

    slide: callback,
  })
  $('#minbeds').on('change', function () {
    slider.slider('value', this.selectedIndex + 1)

  })
}

function makeRadios (selector_id, list, legend) {
  let set = $('<form class=\'radiogroup\'></form>').
    appendTo($('#' + selector_id))
  if (legend) {
    set.append('<legend>' + legend + '</legend>')
  }
  list.forEach((item) => {
    set.append(
      $('<input type=\'radio\'  value =\'' + item + '\' name=\'radio\' >' +
        '<label>' + item + '&nbsp;</label>'))
  })

  $('.radiogroup').children('input').first().attr('checked', true)

}

//***********This part is main call*****************************//

//make radio group

//const POL_LIST = ['CO2', 'CO', 'NO2', 'HC', 'NOx', 'Particulate001','SO2','O3']
//const POL_LIST = ['CO2', 'CO', 'NO2', 'HC', 'NOx','SO2','O3', 'PM10', 'PM2.5']
//const POL_NUM = POL_LIST.length
//const HEIGHT_NUM = 4

