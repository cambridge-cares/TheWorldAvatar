test_data = [
    {
        "node": "ontothermoagent",
        "domain": "ontoagent",
        "score": 0.5,
        "target": {
            "Heat capacity at constant pressure": {
                "value": [
                    "65.17",
                    "65.51",
                    "82.54",
                    "96.74",
                    "108.19",
                    "117.53",
                    "125.32",
                    "131.93",
                    "137.61",
                    "146.76",
                    "156.47",
                    "161.08",
                    "166.15",
                    "171.51",
                    "174.72",
                    "176.76",
                    "178.14",
                    "179.10",
                    "179.81"
                ],
                "unit": "J/mol/K"
            },
            "Heat capacity at constant volume": {
                "value": [
                    "56.86",
                    "57.19",
                    "74.22",
                    "88.43",
                    "99.87",
                    "109.21",
                    "117.00",
                    "123.62",
                    "129.29",
                    "138.44",
                    "148.15",
                    "152.77",
                    "157.84",
                    "163.20",
                    "166.40",
                    "168.45",
                    "169.82",
                    "170.79",
                    "171.49"
                ],
                "unit": "J/mol/K"
            }
        }
    },
    {
        "node": "EMPTY SLOT",
        "domain": "ontoagent",
        "score": -499.5,
        "target": "EMPTY SLOT"
    },
    {
        "node": "EMPTY SLOT",
        "domain": "ontoagent",
        "score": -499.5,
        "target": "EMPTY SLOT"
    },
    {
        "node": "EMPTY SLOT",
        "domain": "ontoagent",
        "score": -499.5,
        "target": "EMPTY SLOT"
    },
    {
        "node": "EMPTY SLOT",
        "domain": "ontoagent",
        "score": -499.5,
        "target": "EMPTY SLOT"
    }
]

test_data_2  = {"result":{"Thermodynamic data for a single T, P point":{"Temperature":{"value":"298.15","unit":"K"},"Pressure":{"value":"101325.00","unit":"Pa"},"Enthalpy":{"value":"16.00","unit":"kJ/mol"},"Internal energy":{"value":"13.52","unit":"kJ/mol"},"Entropy":{"value":"271.54","unit":"J/mol/K"},"Gibbs energy":{"value":"-64.96","unit":"kJ/mol"},"Heat capacity at constant pressure":{"value":"65.17","unit":"J/mol/K"},"Heat capacity at constant volume":{"value":"56.86","unit":"J/mol/K"}},"Thermodynamic data over a selected T range at a single P point":{"Temperature":{"value":["298.15","300.00","400.00","500.00","600.00","700.00","800.00","900.00","1000.00","1200.00","1500.00","1700.00","2000.00","2500.00","3000.00","3500.00","4000.00","4500.00","5000.00"],"unit":"K"},"Pressure":{"value":"101325.00","unit":"Pa"},"Enthalpy":{"value":["16.00","16.12","23.54","32.53","42.80","54.10","66.25","79.12","92.61","121.09","166.67","198.45","247.58","332.11","418.74","506.64","595.39","684.71","774.45"],"unit":"kJ/mol"},"Internal energy":{"value":["13.52","13.63","20.22","28.37","37.81","48.28","59.60","71.64","84.29","111.11","154.20","184.31","230.95","311.33","393.79","477.54","562.13","647.30","732.88"],"unit":"kJ/mol"},"Entropy":{"value":["271.54","271.95","293.18","313.18","331.87","349.27","365.48","380.64","394.84","420.78","454.64","474.52","501.12","538.83","570.41","597.51","621.20","642.24","661.15"],"unit":"J/mol/K"},"Gibbs energy":{"value":["-64.96","-65.46","-93.73","-124.06","-156.32","-190.39","-226.14","-263.45","-302.23","-383.84","-515.29","-608.24","-754.67","-1014.96","-1292.49","-1584.63","-1889.43","-2205.39","-2531.32"],"unit":"kJ/mol"},"Heat capacity at constant pressure":{"value":["65.17","65.51","82.54","96.74","108.19","117.53","125.32","131.93","137.61","146.76","156.47","161.08","166.15","171.51","174.72","176.76","178.14","179.10","179.81"],"unit":"J/mol/K"},"Heat capacity at constant volume":{"value":["56.86","57.19","74.22","88.43","99.87","109.21","117.00","123.62","129.29","138.44","148.15","152.77","157.84","163.20","166.40","168.45","169.82","170.79","171.49"],"unit":"J/mol/K"}},"Fitted NASA polynomials":{"Pressure":{"value":"101325.00","unit":"Pa"},"NASA low temperature":{"value":"298.15","unit":"K"},"NASA medium temperature":{"value":"1000.00","unit":"K"},"NASA high temperature":{"value":"5000.00","unit":"K"},"NASA low temperature coefficients":["-1.21838e+00","3.88845e-02","-3.22553e-05","1.23322e-08","-1.18670e-12","8.20167e+02","2.93334e+01"],"NASA high temperature coefficients":["7.50406e+00","1.34254e-02","-5.18190e-06","9.20977e-10","-6.17855e-14","-1.56334e+03","-1.54677e+01"],"Chemkin-style NASA polynomials data":"C3H4O           STHD    C   3H   4O   1     G    298.15   5000.00 1000.00      1\n-1.21838491e+00 3.88845353e-02-3.22552930e-05 1.23321505e-08-1.18670362e-12    2\n 8.20167317e+02 2.93334196e+01 7.50406187e+00 1.34254377e-02-5.18189977e-06    3\n 9.20976506e-10-6.17855136e-14-1.56334298e+03-1.54676888e+01                   4\n"},"Energy reference point information":{"type":"Standard enthalpy of formation at 298.15 K","value":"16.00","unit":"kJ/mol","provenance":"Rodriguez; Chang; et al.; 1976"}}}



function make_rows(x_data, y_data){
    let rows = [];
    for (let i = 0; i < y_data.length; i++) {
        rows.push([parseFloat(x_data[i]), parseFloat(y_data[i])]);
    }
    return rows
}

function prepare_chart_data(elements){
    // find the value, find the other array
    // y, x
    console.log("preparing chart data", elements)
    elements.forEach((element)=>{
        console.log("element", element)
        for (let attr in element){
            element = element[attr]
            let y_data = element['value']
            let y_unit = element['unit']

        }

    });



    // elements.forEach(function (element){
    for (attribute in elements[0]) {
        console.log("attribute", attribute)
        let y_data = element['value']
        // let y_data = element['value']
        // let y_unit = element['unit']



        // let x_data = []
        // let x_data_title = ''
        // let x_data_unit = ''
        // let attribute = element['attribute']
        // let fixed_data_title = ''
        // let fixed_data = ''
        // let fixed_data_unit = ''
        // find the fixed value and the serial value
        // find the key that is neither value nor attribute
    //     Object.keys(element).forEach(function (k) {
    //         if(k!== 'value' && k!== 'attribute'){
    //             // see whether this is single value or serial
    //             if(typeof element[k]['value'] === "object"){
    //                 // this is serial, x axis
    //                 let x_data_object = element[k]; // containing values and a unit
    //                 x_data_unit = x_data_object['unit'];
    //                 x_data = x_data_object['value'];
    //                 x_data_title = k; // store the title of x axis too
    //             }else{
    //                 // this is the fixed value
    //                 fixed_data_title = k;
    //                 let fixed_data_object = element[k];
    //                 fixed_data_unit = fixed_data_object['unit'];
    //                 fixed_data = fixed_data_object['value'];
    //             }
    //         }
    //     });
    //     let rows = make_rows(x_data, y_data);
    //     drawLineChart(rows, attribute, y_unit, x_data_title, x_data_unit, fixed_data_title, fixed_data, fixed_data_unit);
    }
}


function process_matrix_data(matrix){

    console.log("matrix", matrix)

    let elements = [];
    if ("multiple_results" in matrix){
        console.log("multi reuslt in matrix")
        elements = matrix['multiple_results'];
        prepare_chart_data(elements);
        makeTable(elements);
    }
    else{
        if(typeof matrix['value'] === "string"){
            // single value, table only
            console.log("multi reuslt in matrix")
            elements = [matrix]
            makeTable(elements)
        }else{
            // serial value, use chart to visualise
            elements = [matrix];
            prepare_chart_data(elements);
            makeTable(elements)
        }
    }
}

function split_ontoagent_results(data){

    let all_agent_results = []
    let all_other_results = []
    console.log(data)

    data.forEach((item) => {
       domain = item.domain
       score = item.score
       if (score >= 0){
         console.log(domain)
         if (domain === "ontoagent"){
            all_agent_results.push(item.target)
         }
         else{
           all_other_results.push(item)
         }
       }
    });

    return {"agent": {"multiple_results": all_agent_results}, "others": all_other_results}

}

rst = split_ontoagent_results(test_data_2)
// console.log(rst)
agent_result = rst.agent
console.log(agent_result)
process_matrix_data(agent_result)


