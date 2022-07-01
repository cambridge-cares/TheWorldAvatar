const axios = require('axios').default;

const qstrBgAllSensors = `
select * {
?S <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#hasTimeSeries> ?o.}`;


//connect to postgresql f//connect to postgresql for read the timeseries data
const bgep = 'http://10.25.188.4:9999/blazegraph/namespace/bmstest/sparql';
const pg = require('pg');

function queryBgUnit(qstrBg, cb) {
    const searchParams = new URLSearchParams();
    searchParams.set('query', qstrBg);
    
    axios.post(bgep, searchParams.toString())
        .then(function (response) {
            resultJ = response.data;
            unitResultList = resultJ['results']['bindings'];
            let units = {};
            console.log(resultJ)
            for(let name of resultJ['head']['vars']){
                units[name] = null;
                console.log(units);
            }
    
            for (let unitResultObj of unitResultList){
                let objectKey = Object.keys(unitResultObj);
                console.log(objectKey)
                if (objectKey in units){
                    units[objectKey]=unitResultObj[objectKey]["value"];
                    console.log(unitResultObj[objectKey]["value"])
                }
            }
            cb(null, Object.values(units));
        })
        .catch(function (error) {
            console.log(error);
        });
}

function getTSDataUnitQstr(sensorIRIs) {
    let namestr = "";
    let qnames = [...sensorIRIs.keys()].map((id)=>`?o${id} `);
    namestr.concat()
    let qsecs = '';
    for (let idx in [...sensorIRIs.keys()]){
        qsecs+=`OPTIONAL {{<${sensorIRIs[idx]}> system:hasUnitOfMeasure ${qnames[idx]}. }}`;
    }
    
    
    let template = `
prefix system:<http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>

    select * where{
${qsecs}
}
    `;
    return template;
}

function isotoMs(isostring) {
    let date = new Date(isostring); // some mock date
    let milliseconds = date.getTime();
    return milliseconds;
}

function  getColPg(listIRI) {
    let head = listIRI[0];
    if (listIRI.length > 2) {
        for (let idx = 1; idx < listIRI.length; idx++) {
            head = head + "," + listIRI[idx];
        }
    }
    return `SELECT "dataIRI", "tableName", "columnName" FROM "dbTable" WHERE "dataIRI"='${head}'`;
}

function  getTSPg(colnames, tablename) {
    let head = 'time';
        for (let idx = 0; idx < colnames.length; idx++) {
            head = head + "," + colnames[idx];
        }
    return `SELECT ${head} FROM "${tablename}"`;
}


function getShortUnit(unitIRI){
    let sec =unitIRI.split('#')
    return sec[sec.length-1];}

function getShortName(nameIRI){
    let sec =nameIRI.replace('#', '_').split('/')
    return sec[sec.length-1];
}

 function getTSData(urls, opts, cb) {
     const pool = new pg.Pool({
         user: 'postgres',
         host: '10.25.188.4',
         database: 'bms',
         password: '111111',
         port: '5432'});
    
     //query for unitdata from blazegraph
    pool.query(getColPg(urls), (err, resTableCol) => {
        if(err) cb(err);
       //get tablename, colum
        let colnames = resTableCol.rows.map((row) => row['columnName']);
        console.log(colnames)
        const tablename = resTableCol.rows[0]['tableName'];
        console.log(tablename)
        console.log(getTSPg(colnames, tablename))
    
        pool.query(getTSPg(colnames, tablename), (err, resTSData) => {
            if(err) console.log(err);
            //{unit, name, value:{time, value}}
            console.log(resTSData)
            let tsSeries = [];
            let qstr = getTSDataUnitQstr(urls)
            queryBgUnit(qstr, function (err, units) {
                console.log("result units")
                console.log(units)
                for (let idx = 0; idx<colnames.length;idx++){//for each queried sensor
                    let sensoriri = urls[idx];
                    let sensorname = getShortName(sensoriri);
                    let unit = units[idx]?getShortUnit(units[idx]):"";
                    let timevaluepair = resTSData.rows.map(( row ) => ({time:isotoMs(row['time']), value:row[colnames[idx]]}));
                    tsDatum = tsSeries.push({value: timevaluepair, name:sensorname, unit:unit});
                }
                console.log(JSON.stringify(tsSeries))
                cb(null, tsSeries);
                pool.end();
            });

    });
})
}



module.exports = getTSData;