import React from "react";
import Header from './../common/header';
import Sidebar from './../common/sidebar';
import MyPlot from './../common/plot';
import axios from "axios";
import { Preloader, Bars } from 'react-preloader-icon';
import { useState, useEffect } from 'react';
import initialdata from './../assets/json/initial_plotdata.json';

function CalculatorApp()  {
    
    const [fulldata, setFullData] = useState({...initialdata
        ,display:'overview0'
    });
    const [mode2050, setMode2050] = useState(true);
    const [loading, setLoading] = useState(false);
    const [reductionPercent, setReductionPercent] = useState(0.46);
    const [warn4, setWarn4] = useState(0);
    const [warn4Text, setWarn4Text] = useState("0 lever is set to level 4, considered by experts to be the limit of what is plausible");
    const [warnFast, setWarnFast] = useState(0);
    const [warnFastText, setWarnFastText] = useState("0 lever has a rate of increase in ambition that is higher than Level 4 ambition over the default deployment time, considered by experts to be the fastest plausible.");
    /**
    useEffect(() => {//Fetch when page loads
        fetch('./initial_plotdata.json')
            .then((response) => response.json())
            .then((json) => setFullData({...fulldata, ...json}));
    }, []);
    **/
    const updateDataArr = (dataarr, newvalues)=>{
        dataarr.forEach((pageobj, idxP)=>{
            
            pageobj.data[0].forEach((lineobj, idxL)=>{//first graph
              if ('link' in lineobj ){lineobj.link.value =newvalues[idxP][0][0] }
               else {
              lineobj.y  = newvalues[idxP][0][idxL]
              if (lineobj.y.every((x)=> x==0)) {lineobj.visible = 'legendonly'}
              else {lineobj.visible = 'true'}

              }
          })
            if (pageobj.data.length > 1){
            pageobj.data[1].forEach((lineobj, idxL)=>{//second graph
                lineobj.y  = newvalues[idxP][1][idxL]
                  if (lineobj.y.every((x)=> x==0)) {lineobj.visible = 'legendonly'}
                  else {lineobj.visible = 'true'}
            })}
            /**
            graphobj.data[1].forEach((lineobj, idx)=>{//first graph
                lineobj.x  = newvalues[idxG][1][idx]
            })
             **/
        })
        
    }
    
    
        const fetchData = (datadict) => {
           let levers = Object.values(datadict);
            const sendReq = async () => {
                setLoading(true);
                const  resdata  = await axios.post("/data", JSON.stringify({levers:levers}),  {headers:{
                    'Content-Type': 'application/json'
                }});
                setLoading(false);
                let newValues = resdata['data']['values']
                let newReduct = resdata['data']['reduction2100']
                setReductionPercent(newReduct)
                setWarn4(resdata['data']['warn4'])
                setWarn4Text(resdata['data']['warn4Text'])
                setWarnFast(resdata['data']['warnFast'])
                setWarnFastText(resdata['data']['warnFastText'])
                let updated = [...fulldata['plotdata']];
                updateDataArr(updated, newValues)
                setFullData({...fulldata, plotdata:updated});
            };
            sendReq()
            
        };
 
        /**
         * Filter category for display
         * */
        const setDisplay = (button) => {
            setFullData({...fulldata, display:button});

        };

    return(
            <div className="d-flex" id="wrapper">
            {/* <!-- Sidebar--> */}
                <Sidebar fetchData={fetchData} reductionPercent={reductionPercent} warn4={warn4} warn4Text={warn4Text} warnFast={warnFast} warnFastText={warnFastText}/>
            {/* <!-- Page content wrapper--> */}
            <div className="main" id="page-content-wrapper">
            {/* <!-- Top navigation--> */}
            <Header setDisplay={setDisplay} display={fulldata.display}/>
            {/* <!-- Page content--> */}
            <div className="container-fluid content-container">
                <div class={loading?"overlay customshow":"overlay"}></div>
                <div class={loading?"spanner customshow":"spanner"}>
                    <div class="loader"></div>
                    <p>Calculating, please be patient.</p>
                </div>
                <div id="plot-container1">
                    <MyPlot fulldata={fulldata} mode2050={mode2050} index={0}/></div>
                <div id="plot-container2">
                    <MyPlot fulldata={fulldata} mode2050={mode2050} index={1}/></div>
            </div>
            </div>
            </div>);
    
    }
export default CalculatorApp;