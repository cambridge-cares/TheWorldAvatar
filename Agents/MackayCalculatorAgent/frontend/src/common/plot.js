import React , {useEffect} from "react";
import Plot from 'react-plotly.js';
import Icons from 'react-plotly.js';
//https://github.com/plotly/react-plotly.js#state-management
function MyPlot(props) {
        const index = props.index
        //filter chart to show by category
        const filteredData = props.fulldata.plotdata.filter(
        (item) => item.category === props.fulldata.display
        )[0];

        //Cut to 8th entry if mode 2050
        if (props.mode2050) {
        filteredData.data.forEach((chartdata)=>{
        chartdata.forEach((linedata)=>{
                      if ('x' in linedata) {linedata.x = linedata.x.slice(0,8)};
                    if ('y' in linedata) {linedata.y = linedata.y.slice(0,8)};
        })
        })
        //cut axis range if mode 2050
        filteredData.layout.forEach((chartlayout)=>{
        if('xaxis' in chartlayout) chartlayout.xaxis.range = [2015,2050]
        })
         }

        //make height larger for single page plot(flow)
        /**
        let height = filteredData.data.length>1?"500px":"800px"
          useEffect(() => {
               setTimeout(function() {
                 window.dispatchEvent(new Event("resize"));
                 console.log("fired resize");
               }, 5);
          });
       */


        let config = {
              modeBarButtonsToAdd: [
                {
                  name: 'Tips: Double Click to zoom back to original',
                  icon: Icons.tooltip,
                  click: function(gd) {//alert('Double Click to zoom back to original')
                  }}]
                  };

        return index in filteredData.data?(
            <Plot
                data={filteredData.data[index]}
                layout={{...filteredData.layout[index]}}
                config = {{...config, responsive: true, displayModeBar: true}}
                    style={{width: '100%', height:"500px"}}
            />
        ):('');
        
}
export default MyPlot;