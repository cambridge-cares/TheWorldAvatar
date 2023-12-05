import React from "react";
import { useState, useEffect } from 'react';

function Header(topprops) {
    
    
    const [active, setActive] = useState('Overview');
    const navLabels = {"Overview":['Emissions / Primary Energy','Cumulative Emissions / Final Energy'],
        "Transport":["Energy & Emissions","Demand & Technology"],
        "Buildings":["Energy & Emissions","Heat"],
        "Industry":["Energy & Emissions"],
        "CO2 Removal & Gases":["Emissions Removal","Gas Grid & Hydrogen"],
        "Electricity":["Emissions/Generation","Capacity & Peak"],
        "Land Use & Bioenergy":["Emissions & Land","Bioenergy"],
        "Imports & Flows":["Imports"]
    };
    
    function SubTabs(props){
        const subLabels = navLabels[props.topLabel]
        const subItems = subLabels.map((label, idx) =>
                <li className="nav-item"  onClick={()=>{topprops.setDisplay(props.topLabel.toLowerCase()+idx)}}>
                    <a className={topprops.display===(props.topLabel.toLowerCase()+idx)?"nav-link active":"nav-link"} href="#">{label}</a>
                </li>
            )
        return <ul className="nav nav-tabs">
            {subItems}
        </ul>
    }

    const topLabels = Object.keys(navLabels);
    /**
    <li class="nav-item active" onClick={()=>{props.setDisplay('overview')}}>
        <a class="nav-link" href="#">Overview<span class="sr-only">(current)</span></a>
    </li>
     **/
    return (
        <div className="d-flex flex-column">
            <div>
         <nav className="navbar nav-pills navbar-expand-lg navbar-dark  border-bottom">
            <div className="container-fluid">
                <div className="collapse navbar-collapse" id="navbarSupportedContent">
                    <ul className="navbar-nav ms-auto mt-2 mt-lg-0">
                        {/* <li className="nav-item"><a data-bs-toggle="modal" data-bs-target="#add-lead-modal"  className="nav-link highlighted-text" href="#!">Add lead</a></li> */}
                        {
                            topLabels.map((label)=>
                                <li class="nav-item" onClick={()=>{setActive(label);topprops.setDisplay(label.toLowerCase()+0);}}  >
                                    <a class={active===label?"nav-link active":"nav-link"} href="#">{label}</a>
                                </li>
                            )
                            
                        }
                        
                    </ul>
                </div>
            </div>
        </nav>
            </div>
            <div className="container-fluid" id="sub-nav">
             <SubTabs topLabel={active} />
            </div>
        </div>
);
}

export default Header;