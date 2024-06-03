import React from "react";
import 'react-perfect-scrollbar/dist/css/styles.css';
import PerfectScrollbar from 'react-perfect-scrollbar'
import { Link } from 'react-router-dom';
import btndescriptions from './../assets/json/descriptions.json';

import { useState, useEffect } from 'react';
import { InfoCircle, ExclamationSquareFill,Icon4SquareFill, InfoCircleFill} from "react-bootstrap-icons";



//TODO: set the API route here
function Sidebar(topprops) {
    //TOP LEVEL NAMES
    const topiclabels = {"Transport":['SG Transport Demand', 'International Aviation', 'Light Vehicles - Electric', 'Light Vehicles - Hydrogen', 'Light Vehicles - Hybrid', 'Light Vehicles - Biofuel', 'Heavy Vehicles - Electric', 'Heavy Vehicles - Hydrogen', 'Heavy Vehicles - Hybrid', 'Heavy Vehicles - Biofuel', 'Aviation Efficiency', 'Aviation Biofuel'],
        "Buildings":['Buildings Temperature', 'Buildings Insulation', 'District Heat Share', 'Heat Pump Share', 'Hybrid Heat Share', 'Network - Heat Pump', 'Heat Network - Biomass', 'Lighting and Appliances'],
    "Industry":['Industrial Efficiency', 'Industry Electrification', 'Industry Shift to Biomass', 'Industry Shift to Gas', 'Industry CCS'],
    "CO2 Removal & Gases":['Hydrogen Gas Grid Share', 'Biomethane Gas Grid Share', 'Hydrogen - Biomass CCS', 'Hydrogen - Methane CCS', 'Hydrogen - Imports', 'Greenhouse Gas Removal', 'Bio-Conversion with CCS', 'CCS Capture Rate' ],
    "Electricity":[ 'Seasonal Storage', 'Short Term Balancing', 'Biomass with CCS', 'Nuclear', 'Offshore & Onshore Wind', 'Solar', 'Wave & Tidal', 'Gas with CCS'],
    "Land Use & Biofuels":['Farming Yield & Efficiency', 'Forestry', 'Land for Bioenergy', 'Waste Reduction']};
    const toplabels =  Object.keys(topiclabels);
    const alllabels = toplabels.map(item=>topiclabels[item]).flat();
    const initalConfig = {};
    const intialTopConfig = {};
    for (const l of alllabels) {
        initalConfig[l] = 1;
    }
    for (const l of toplabels) {
        intialTopConfig[l] = 1;
    }
    const [configs, setConfigs] = useState(initalConfig);
    const [topConfigs, setTopConfigs] = useState(intialTopConfig);
    
    const intialCollapse = {};
    
    for (const l of toplabels) {
        intialCollapse[l] = false;
    }
    const [collapses, setCollapses] = useState(intialCollapse);//To remember what is collapsed after re-render
    
    
    
    function LevelButtons(props) {
        const label = props.label;
        
        function handleValueChange(event) {
            let prev = {...configs};
            let prevTop = {...topConfigs};
            if (toplabels.includes(label)){//is top label=>
                //set sublabel config
                for (let sub of topiclabels[label]){
                    prev[sub] = parseInt(event.target.value);
                }
                //set toplabel config
                prevTop[label] = parseInt(event.target.value);
                setTopConfigs({...prevTop});
                
            } else {//is sub label
                prev[label] = parseInt(event.target.value);
    
            }
            setConfigs({...prev});
            topprops.fetchData(prev);
        }
        let configDict = toplabels.includes(label)?topConfigs:configs;
        let isTop = toplabels.includes(label);
        return <div className="btn-group" id={"btn-group-"+label} role="group" aria-label="Basic radio toggle button group">
            <input type="radio" className="btn-check" checked={configDict[label] === 1} value="1" name={"btnradio"+label} id={"btnradio1"+label} onChange={handleValueChange}></input>
            <label data-bs-toggle="tooltip"  title={isTop?undefined:btndescriptions[label]['d1']} className={configDict[label]>=1?"btn btn-outline-primary active":"btn btn-outline-primary"} htmlFor={"btnradio1"+label}>1</label>
            <input type="radio" className="btn-check" checked={configDict[label] === 2} value="2" name={"btnradio"+label} id={"btnradio2"+label} onChange={handleValueChange}></input>
            <label data-bs-toggle="tooltip"  title={isTop?undefined:btndescriptions[label]['d2']} className={configDict[label]>=2?"btn btn-outline-primary active":"btn btn-outline-primary"} htmlFor={"btnradio2"+label}>2</label>
            <input type="radio" className="btn-check" checked={configDict[label] === 3} value="3" name={"btnradio"+label} id={"btnradio3"+label} onChange={handleValueChange}></input>
            <label data-bs-toggle="tooltip"  title={isTop?undefined:btndescriptions[label]['d3']} className={configDict[label]>=3?"btn btn-outline-primary active":"btn btn-outline-primary"} htmlFor={"btnradio3"+label}>3</label>
            <input type="radio" className="btn-check" checked={configDict[label] === 4} value="4" name={"btnradio"+label} id={"btnradio4"+label} onChange={handleValueChange}></input>
            <label data-bs-toggle="tooltip"  title={isTop?undefined:btndescriptions[label]['d4']} className="btn btn-outline-primary" htmlFor={"btnradio4"+label}>4</label>
        </div>
    }
//get-childitem *.pdf | foreach {rename-item $_ $_.name.replace(" ","")}
    function removeSpace(name){
    return name.replace(/\s/g, '').replace('-','').replaceAll('&','and');
    }

    function CollapseLevelButtons(props) {
        const  topLabel = props.labels;
        const subLabels = topiclabels[topLabel];
        const subbuttons = subLabels.map((label) =>
            <li className="d-flex justify-content-between" ><Link target="_blank" to={removeSpace("/guides/"+removeSpace(topLabel)+'/'+topLabel+label+'.pdf')}><span data-bs-toggle="tooltip" title={btndescriptions[label]['d0']}>{label}</span></Link>
                <LevelButtons label={label}/>
            </li>
        )
        
        function handleCollapse(event) {
            let prev = Object.assign({}, collapses);
            prev[topLabel] = !collapses[topLabel];
            setCollapses({...prev})
        }
    
        let isExpanded = collapses[topLabel];
        return <li className="mb-1 me-4">
    
            <div class="d-flex justify-content-between">
                <button id={topLabel+"-collapse-control"} className="btn btn-toggle rounded collapsed" data-bs-toggle="collapse" data-bs-target={"#"+topLabel+"-collapse"} onClick={handleCollapse} aria-expanded="true" >
                    {topLabel}
                </button>
                <LevelButtons label={topLabel} />
            </div>
            <div className={isExpanded?"collapse show":"collapse"} id={topLabel+"-collapse"}>
                <ul className="btn-toggle-nav list-unstyled fw-normal pb-1">
                    {subbuttons}
                </ul>
            </div>

    </li>
    }
    
    function setAllLevels(event) {
        let level = parseInt(event.target.value);
        let prev = {...configs};
        let prevTop = {...topConfigs};
        for (let key in prev){
            prev[key] = level ;
        }
        for (let key in prevTop){
            prevTop[key] = level ;
        }
        setConfigs({...prev});
        setTopConfigs({...prevTop});
        topprops.fetchData(prev);
    }
    
    function selectDropdown(props){
    
    
    
    return <div>
        <select onChange={setAllLevels} class="form-select my-1 mx-2 customSelect" aria-label=".form-select-lg example">
        <option selected disabled>Example pathways</option>
            <option value="1">All Level 1</option>
            <option value="2">All Level 2</option>
        <option value="3">All Level 3</option>
            <option value="4">All Level 4</option>
    </select>
        </div>
    
    }


    function reductionBar(props){
    let percent = topprops.reductionPercent;
    const displayPercent = (percent) => `${(percent * 100).toFixed(0)}%`;
    const displayPixel  = (percent) => `${percent}px`;
    return             <div class={percent-0<0?"progress":"progress inverted"} style={percent-0<0?{"width":"300px"}:{"width":displayPixel(300*(1+percent))}}>
                           <div class="progress-bar" style={percent-0<0?{"width":displayPercent(Math.max(1+percent, 0.20)), "background":"#6EC1E4"}:{"width":"300px", "background":"rgb(255,255,255)"}}>
                               <h3 class={percent-0<0?"progress-title":"progress-title invert"}>{percent-0>0?1990:2050}</h3>
                               {percent-0<0 &&
                               <div class="progress-value">{displayPercent(percent)}</div>
                               }
                           </div>
                           <div>
                               {percent-0>0 &&
                               <div class="progress-value-invert">{'+'+displayPercent(percent)}</div>
                               }
                               <h3 class="progress-rightlabel text-end" >{percent-0>0?2050:1990}</h3>
                           </div>
                       </div>

    }

    
    //define config buttons by label list
    const topLevelBtns = toplabels.map((eachlabels) =>
        <CollapseLevelButtons labels={eachlabels} />);

        return <div>

        <div className="border-end sidenav" id="sidebar-wrapper">
            <div className="sidebar-heading border-bottom ">
                <div class="container">
                    <div class="row">
                        <h1 className="col-12">SINGAPORE MACKAY CARBON CALCULATOR</h1>
                    </div>
                     <div class="row">
                        <div class="col-1">
                            <Link to="https://dev.theworldavatar.io/">
                                <img alt="Alt content" src={require('./../assets/images/logo_sm.png')} />
                            </Link>
                        </div>
                        <div class="col-8 m-3">
                             <h2>Powered by The World Avatar</h2>
                        </div>

                                                <div className="col-1 m-2">
                                                      <Link
                                                        to="/guidance"
                                                        target="_blank"
                                                        rel="noreferrer"
                                                      >
                                                <InfoCircle size={25}/>
                                                </Link>
                                                </div>
                    </div>

                </div>
            </div>
            <PerfectScrollbar className="sidebar-items">
                <ul className="list-unstyled ps-0">
                     <li className="mx-2">
                            {reductionBar()}
                            <p className="mx-2">CO<sub>2</sub>e reduction in 2050 compared to 1990</p>
                     </li>
                     <li className="border-top my-3"></li>
                     <li className="row">
                         <div className="col-5 mx-2">
                         {selectDropdown()}
                         </div>
                                 <div className="col-1 my-2 me-2">
                                                 <span data-bs-toggle="modal" data-bs-target="#selectModal">
                                                 <InfoCircleFill size={30} className="infoicon"/>
                                                 </span>
                                 </div>
                         <div className="col-1 my-1">
                          <Icon4SquareFill className={topprops.warn4?"red warnicon":"grey warnicon"} size={35}  data-bs-toggle="tooltip"  title={topprops.warn4Text}/>
                          </div>
                          <div className="col-1 my-1">
                          <ExclamationSquareFill className={topprops.warnFast?"red warnicon":"grey warnicon"}  size={35} data-bs-toggle="tooltip" title={topprops.warnFastText}/>
                         </div>
                     </li>
                     <li className="border-top my-3"></li>
                     <li className="row">
                     <div className="col-4 ms-1 pe-1">Lever settings: </div>
                     <div className="col-1 me-4 ps-1" data-bs-toggle="modal" data-bs-target="#leverModal"><InfoCircleFill size={30} className="infoicon"/></div>
                     <div className="col-5 ms-5 ps-4">Level of ambition</div>
                     </li>
                     <li className="border-top my-3"></li>
                     {topLevelBtns}
                     <li className="border-top my-3"></li>
                </ul>
            </PerfectScrollbar>
        </div>


                                     <div class="modal fade" id="selectModal" tabindex="-1" role="dialog" aria-labelledby="selectModalLabel" aria-hidden="true">
                                       <div class="modal-dialog" role="document">
                                         <div class="modal-content">
                                           <div class="modal-header">
                                             <h5 class="modal-title" id="termModalLabel">Pathway descriptions</h5>
                                             <button type="button" class="close" data-bs-dismiss="modal" aria-label="Close">
                                               <span aria-hidden="true">&times;</span>
                                             </button>
                                           </div>
                                           <div class="modal-body">
                                        <p>All Level 1:
                                            Ambition levels for all levers are set to Level 1.</p>
                                           <p>
                                             All Level 2:
                                             Ambition levels for all levers are set to Level 2.</p>
                                             <p>All Level 3:
                                             Ambition levels for all levers are set to Level 3.</p>
                                             <p>All Level 4:
                                             Ambition levels for all levers are set to Level 3.</p>
                                           </div>
                                         </div>
                                       </div>
                                     </div>


                                     <div class="modal fade" id="leverModal" tabindex="-1" role="dialog" aria-labelledby="leverModalLabel" aria-hidden="true">
                                       <div class="modal-dialog" role="document">
                                         <div class="modal-content">
                                           <div class="modal-header">
                                             <h5 class="modal-title" id="termModalLabel">Lever table</h5>
                                             <button type="button" class="close" data-bs-dismiss="modal" aria-label="Close">
                                               <span aria-hidden="true">&times;</span>
                                             </button>
                                           </div>
                                           <div class="modal-body">
                                                <p>Each lever can be assigned an ambition level. High levels reflect high ambition:</p>

                                                <p>1: The least effort possible</p>
                                                <p>2: Ambitious but reasonable</p>
                                                <p>3: Very ambitious needing technical breakthroughs</p>
                                                <p>4: The upper limit of what is considered plausible</p>
                                           </div>
                                         </div>
                                       </div>
                                     </div>

        <div className="fixed-bottom-custom">
                        <p data-bs-toggle="modal" data-bs-target="#termModal">
                        Conditions of Use
                        </p>
        </div>
        <div class="modal fade" id="termModal" tabindex="-1" role="dialog" aria-labelledby="termModalLabel" aria-hidden="true">
          <div class="modal-dialog" role="document">
            <div class="modal-content">
              <div class="modal-header">
                <h5 class="modal-title" id="termModalLabel">Terms and Conditions</h5>


                <button type="button" class="close" data-bs-dismiss="modal" aria-label="Close">
                  <span aria-hidden="true">&times;</span>
                </button>
              </div>
              <div class="modal-body">
                Cambridge CARES provides no express or implied warranties concerning this tool and its content and, accordingly, accepts no liability arising from use of the tool or its content.


              </div>
            </div>
          </div>
        </div>

</div>
    
}

export default Sidebar;