var Obj = { "http://www.theworldavatar.com/WeatherOfShenzhen-567658699" : { "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" : [ { "type" : "literal" , "value" : "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState" } ] , "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasWind" : [ { "type" : "uri" , "value" : "http://www.theworldavatar.com/WindOfShenzhen-567658699" } ] , "http://www.theworldavatar.com/weather.owl#hasIcon" : [ { "type" : "literal" , "value" : "02d" } ] , "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasWeatherCondition" : [ { "type" : "uri" , "value" : "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Sun" } ] , "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasHumidity" : [ { "type" : "literal" , "value" : "62" } ] , "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Temperature" : [ { "type" : "literal" , "value" : "30.96" } ] } , "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Sun" : { "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" : [ { "type" : "uri" , "value" : "http://www.theworldavatar.com/WeatherConditionOfShenzhen-567658699" } ] } , "http://www.theworldavatar.com/WindOfShenzhen-567658699" : { "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasSpeed" : [ { "type" : "literal" , "value" : "4.6" } ] , "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" : [ { "type" : "literal" , "value" : "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Wind" } ] , "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasDirection" : [ { "type" : "literal" , "value" : "210" } ] }};

var icon = '';



var initValue = getParameter();
console.log(initValue);
if(initValue === 'Error' || initValue === ''){
	alert('Error: CITY_NOT_FOUND');
}
else
{
populateTable(processResult(initValue));
}
function getParameter() {
    var url_string = window.location.href;
    var url = new URL(url_string.replace(/#/g, '$'));
    var value = url.searchParams.get("value");
    return value
}

function processResult(testObj) {
	testObj = JSON.parse(testObj);
    var keys = Object.keys(testObj);

    var mainKey = '';
    for(var idx in keys){
        var key = keys[idx];
        if(key.includes("WeatherOf")){
            mainKey = key;
        }
    }


    var weatherOntologyBaseUrl = 'https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl$';
    var jpsWeatherOntolgy = 'http://www.theworldavatar.com/weather.owl$';
    var weatherObj = testObj[mainKey];

    var wind = weatherObj[weatherOntologyBaseUrl + 'hasWind'];
    icon = 'http://openweathermap.org/img/w/' + weatherObj[jpsWeatherOntolgy + 'hasIcon'][0]['value'] + '.png';
    var weatherCondition = weatherObj[weatherOntologyBaseUrl+ 'hasWeatherCondition'][0]['value'].split('$')[1];
    var humidity = weatherObj[weatherOntologyBaseUrl + 'hasHumidity'][0]['value'] + ' %';
    var temperature = weatherObj[weatherOntologyBaseUrl + 'hasTemperature'][0]['value'] + ' &#8451;';
    var windIRI = wind[0]['value'];
    wind = testObj[windIRI];
    var windSpeed = wind[weatherOntologyBaseUrl + 'hasSpeed'][0]['value'] + ' m/s';
    var windDirection = wind[weatherOntologyBaseUrl + 'hasDirection'][0]['value'] + ' &deg;';
    var city = '<a href= ' + weatherObj['http://dbpedia.org/ontology/locationCity'][0]['value'] + '>' + weatherObj['http://dbpedia.org/ontology/locationCity'][0]['value'] + '</a>';
    
    
    
    var result = {'description': weatherCondition, 'temperature': temperature, 'windSpeed': windSpeed, 'windDirection': windDirection, 'humidity': humidity, 'city': city};
    return result;
}

function populateTable(result) {

    for(var idx in Object.keys(result)){
        var key = Object.keys(result)[idx];
        console.log(key);
        document.getElementById(key).innerHTML = result[key];
    }
    document.getElementById('icon').src = icon;
}