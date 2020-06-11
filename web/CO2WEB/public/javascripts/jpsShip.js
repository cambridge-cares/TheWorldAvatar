var prefix = "http://localhost:8080";
var markers = []
var listOfIRIs = [];
var metaEndpoint = prefix + "/rdf4j-server/repositories/airqualitystation";
var heatmap = null;
var arrXYPollutant;

var NO2json = [{"X(m)":1.1921157116,"Y(m)":103.7832792019,"NO2":1.758644},{"X(m)":1.2102061519,"Y(m)":103.7832711996,"NO2":2.70028},{"X(m)":1.2282965896,"Y(m)":103.7832630768,"NO2":5.278008},{"X(m)":1.2463870249,"Y(m)":103.7832548333,"NO2":0.04759246},{"X(m)":1.2644774576,"Y(m)":103.7832464692,"NO2":15.56921},{"X(m)":1.2825678877,"Y(m)":103.7832379845,"NO2":20.39414},{"X(m)":1.3006583152,"Y(m)":103.7832293792,"NO2":6.240888},{"X(m)":1.3187487401,"Y(m)":103.7832206532,"NO2":0.1043798},{"X(m)":1.3368391622,"Y(m)":103.7832118066,"NO2":0.0},{"X(m)":1.3549295816,"Y(m)":103.7832028394,"NO2":0.0007031696},{"X(m)":1.1921236463,"Y(m)":103.8012525414,"NO2":0.7650797},{"X(m)":1.210214207,"Y(m)":103.8012446573,"NO2":3.997098},{"X(m)":1.2283047652,"Y(m)":103.8012366543,"NO2":17.96103},{"X(m)":1.2463953209,"Y(m)":103.8012285326,"NO2":31.77187},{"X(m)":1.2644858741,"Y(m)":103.801220292,"NO2":20.07102},{"X(m)":1.2825764247,"Y(m)":103.8012119325,"NO2":3.685356},{"X(m)":1.3006669726,"Y(m)":103.8012034542,"NO2":0.09420924},{"X(m)":1.3187575179,"Y(m)":103.8011948571,"NO2":0.01199669},{"X(m)":1.3368480605,"Y(m)":103.8011861411,"NO2":0.01622258},{"X(m)":1.3549386004,"Y(m)":103.8011773062,"NO2":0.00172995},{"X(m)":1.192131463,"Y(m)":103.8192259997,"NO2":0.001312677},{"X(m)":1.2102221423,"Y(m)":103.8192182337,"NO2":1.878788},{"X(m)":1.2283128192,"Y(m)":103.8192103507,"NO2":16.34919},{"X(m)":1.2464034936,"Y(m)":103.8192023506,"NO2":21.67351},{"X(m)":1.2644941654,"Y(m)":103.8191942335,"NO2":16.75356},{"X(m)":1.2825848346,"Y(m)":103.8191859993,"NO2":4.220244},{"X(m)":1.3006755013,"Y(m)":103.8191776481,"NO2":0.04344807},{"X(m)":1.3187661652,"Y(m)":103.8191691798,"NO2":0.0},{"X(m)":1.3368568265,"Y(m)":103.8191605944,"NO2":0.002501672},{"X(m)":1.354947485,"Y(m)":103.8191518919,"NO2":0.0005850521},{"X(m)":1.1921391617,"Y(m)":103.837199575,"NO2":0.0},{"X(m)":1.2102299579,"Y(m)":103.8371919272,"NO2":0.7992001},{"X(m)":1.2283207516,"Y(m)":103.8371841641,"NO2":2.542465},{"X(m)":1.2464115429,"Y(m)":103.8371762858,"NO2":7.517437},{"X(m)":1.2645023315,"Y(m)":103.8371682921,"NO2":24.09183},{"X(m)":1.2825931176,"Y(m)":103.8371601832,"NO2":8.602631},{"X(m)":1.3006839011,"Y(m)":103.837151959,"NO2":0.04613145},{"X(m)":1.3187746819,"Y(m)":103.8371436195,"NO2":0.0},{"X(m)":1.3368654601,"Y(m)":103.8371351648,"NO2":0.0000590878},{"X(m)":1.3549562355,"Y(m)":103.8371265947,"NO2":0.0001573054},{"X(m)":1.1921467423,"Y(m)":103.8551732656,"NO2":0.0},{"X(m)":1.2102376536,"Y(m)":103.855165736,"NO2":0.05783296},{"X(m)":1.2283285624,"Y(m)":103.8551580928,"NO2":0.619101},{"X(m)":1.2464194687,"Y(m)":103.8551503362,"NO2":0.02089391},{"X(m)":1.2645103725,"Y(m)":103.855142466,"NO2":18.2236},{"X(m)":1.2826012736,"Y(m)":103.8551344824,"NO2":21.83832},{"X(m)":1.3006921722,"Y(m)":103.8551263852,"NO2":2.589856},{"X(m)":1.3187830681,"Y(m)":103.8551181746,"NO2":0.0},{"X(m)":1.3368739613,"Y(m)":103.8551098504,"NO2":0.001974617},{"X(m)":1.3549648518,"Y(m)":103.8551014128,"NO2":0.0002526548},{"X(m)":1.1921542049,"Y(m)":103.8731470697,"NO2":1.334192},{"X(m)":1.2102452295,"Y(m)":103.8731396582,"NO2":0.7048039},{"X(m)":1.2283362516,"Y(m)":103.873132135,"NO2":2.006457},{"X(m)":1.2464272712,"Y(m)":103.8731245001,"NO2":0.0},{"X(m)":1.2645182882,"Y(m)":103.8731167534,"NO2":37.7027},{"X(m)":1.2826093027,"Y(m)":103.8731088951,"NO2":48.75678},{"X(m)":1.3007003145,"Y(m)":103.873100925,"NO2":5.256254},{"X(m)":1.3187913237,"Y(m)":103.8730928432,"NO2":0.4017018},{"X(m)":1.3368823302,"Y(m)":103.8730846496,"NO2":0.02389445},{"X(m)":1.354973334,"Y(m)":103.8730763444,"NO2":0.0000057832},{"X(m)":1.1921615495,"Y(m)":103.8911209855,"NO2":0.06663989},{"X(m)":1.2102526856,"Y(m)":103.8911136922,"NO2":0.1407793},{"X(m)":1.2283438192,"Y(m)":103.8911062889,"NO2":0.3614089},{"X(m)":1.2464349502,"Y(m)":103.8910987757,"NO2":2.615068},{"X(m)":1.2645260788,"Y(m)":103.8910911525,"NO2":13.91094},{"X(m)":1.2826172047,"Y(m)":103.8910834195,"NO2":35.17267},{"X(m)":1.3007083281,"Y(m)":103.8910755764,"NO2":10.03782},{"X(m)":1.3187994487,"Y(m)":103.8910676235,"NO2":0.07940782},{"X(m)":1.3368905667,"Y(m)":103.8910595606,"NO2":0.001038789},{"X(m)":1.354981682,"Y(m)":103.8910513877,"NO2":0.0},{"X(m)":1.1921687761,"Y(m)":103.9090950112,"NO2":0.0},{"X(m)":1.2102600218,"Y(m)":103.909087836,"NO2":0.04295366},{"X(m)":1.2283512651,"Y(m)":103.9090805527,"NO2":0.0},{"X(m)":1.2464425059,"Y(m)":103.9090731612,"NO2":2.895215},{"X(m)":1.2645337441,"Y(m)":103.9090656616,"NO2":17.67259},{"X(m)":1.2826249798,"Y(m)":103.9090580538,"NO2":26.59106},{"X(m)":1.3007162128,"Y(m)":103.9090503378,"NO2":16.80966},{"X(m)":1.3188074432,"Y(m)":103.9090425137,"NO2":2.274314},{"X(m)":1.3368986709,"Y(m)":103.9090345814,"NO2":0.008024883},{"X(m)":1.3549898959,"Y(m)":103.909026541,"NO2":0.0},{"X(m)":1.1921758846,"Y(m)":103.9270691451,"NO2":0.0},{"X(m)":1.2102672383,"Y(m)":103.9270620881,"NO2":0.0002838454},{"X(m)":1.2283585894,"Y(m)":103.9270549247,"NO2":0.6349015},{"X(m)":1.2464499381,"Y(m)":103.9270476549,"NO2":0.001335126},{"X(m)":1.2645412842,"Y(m)":103.9270402788,"NO2":14.26682},{"X(m)":1.2826326278,"Y(m)":103.9270327963,"NO2":19.74097},{"X(m)":1.3007239688,"Y(m)":103.9270252074,"NO2":16.98232},{"X(m)":1.3188153071,"Y(m)":103.9270175121,"NO2":2.22069},{"X(m)":1.3369066427,"Y(m)":103.9270097105,"NO2":0.0},{"X(m)":1.3549979756,"Y(m)":103.9270018025,"NO2":0.005256226},{"X(m)":1.1921828751,"Y(m)":103.9450433853,"NO2":0.003944732},{"X(m)":1.2102743348,"Y(m)":103.9450364464,"NO2":0.0003045356},{"X(m)":1.2283657921,"Y(m)":103.945029403,"NO2":1.304565},{"X(m)":1.2464572469,"Y(m)":103.945022255,"NO2":0.2675311},{"X(m)":1.2645486992,"Y(m)":103.9450150024,"NO2":2.949056},{"X(m)":1.2826401489,"Y(m)":103.9450076452,"NO2":14.81825},{"X(m)":1.3007315959,"Y(m)":103.9450001834,"NO2":49.53163},{"X(m)":1.3188230403,"Y(m)":103.9449926169,"NO2":15.88286},{"X(m)":1.3369144821,"Y(m)":103.9449849459,"NO2":0.9250311},{"X(m)":1.3550059211,"Y(m)":103.9449771703,"NO2":0.005183576}];
var NO2json2 = [{"X(m)":11553101.7936108839,"Y(m)":132715.2898235318,"NO2":1.758644},{"X(m)":11553100.9028050341,"Y(m)":134729.5510449489,"NO2":2.70028},{"X(m)":11553099.9985729679,"Y(m)":136743.8254259113,"NO2":5.278008},{"X(m)":11553099.0809142403,"Y(m)":138758.1131632826,"NO2":0.04759246},{"X(m)":11553098.1498284005,"Y(m)":140772.4144539303,"NO2":15.56921},{"X(m)":11553097.2053149864,"Y(m)":142786.7294947262,"NO2":20.39414},{"X(m)":11553096.2473735362,"Y(m)":144801.058482555,"NO2":6.240888},{"X(m)":11553095.2760035731,"Y(m)":146815.4016143039,"NO2":0.1043798},{"X(m)":11553094.2912046239,"Y(m)":148829.7590868686,"NO2":0.0},{"X(m)":11553093.2929762006,"Y(m)":150844.1310971508,"NO2":0.0007031696},{"X(m)":11555102.5766090527,"Y(m)":132716.1733022057,"NO2":0.7650797},{"X(m)":11555101.6989544053,"Y(m)":134730.447940213,"NO2":3.997098},{"X(m)":11555100.8080717549,"Y(m)":136744.7357382033,"NO2":17.96103},{"X(m)":11555099.9039606657,"Y(m)":138759.0368930454,"NO2":31.77187},{"X(m)":11555098.9866206907,"Y(m)":140773.3516016149,"NO2":20.07102},{"X(m)":11555098.0560513772,"Y(m)":142787.6800607924,"NO2":3.685356},{"X(m)":11555097.1122522671,"Y(m)":144802.0224674681,"NO2":0.09420924},{"X(m)":11555096.1552228965,"Y(m)":146816.3790185327,"NO2":0.01199669},{"X(m)":11555095.1849627905,"Y(m)":148830.7499108913,"NO2":0.01622258},{"X(m)":11555094.2014714759,"Y(m)":150845.1353414531,"NO2":0.00172995},{"X(m)":11557103.3728347924,"Y(m)":132717.0436406413,"NO2":0.001312677},{"X(m)":11557102.5083317775,"Y(m)":134731.3314956871,"NO2":1.878788},{"X(m)":11557101.6307989825,"Y(m)":136745.6325111493,"NO2":16.34919},{"X(m)":11557100.7402359769,"Y(m)":138759.9468839019,"NO2":21.67351},{"X(m)":11557099.8366423175,"Y(m)":140774.2748108269,"NO2":16.75356},{"X(m)":11557098.9200175628,"Y(m)":142788.6164888104,"NO2":4.220244},{"X(m)":11557097.9903612584,"Y(m)":144802.9721147476,"NO2":0.04344807},{"X(m)":11557097.0476729479,"Y(m)":146817.3418855426,"NO2":0.0},{"X(m)":11557096.0919521675,"Y(m)":148831.7259981001,"NO2":0.002501672},{"X(m)":11557095.123198444,"Y(m)":150846.1246493359,"NO2":0.0005850521},{"X(m)":11559104.1820898838,"Y(m)":132717.9008384085,"NO2":0.0},{"X(m)":11559103.3307389282,"Y(m)":134732.20171094,"NO2":0.7992001},{"X(m)":11559102.4665564224,"Y(m)":136746.5157443114,"NO2":2.542465},{"X(m)":11559101.5895419363,"Y(m)":138760.8431354061,"NO2":7.517437},{"X(m)":11559100.6996950414,"Y(m)":140775.1840811099,"NO2":24.09183},{"X(m)":11559099.7970152963,"Y(m)":142789.5387783184,"NO2":8.602631},{"X(m)":11559098.8815022558,"Y(m)":144803.9074239305,"NO2":0.04613145},{"X(m)":11559097.953155471,"Y(m)":146818.2902148543,"NO2":0.0},{"X(m)":11559097.0119744856,"Y(m)":148832.687348008,"NO2":0.0000590878},{"X(m)":11559096.057958832,"Y(m)":150847.0990203097,"NO2":0.0001573054},{"X(m)":11561105.0041761063,"Y(m)":132718.7448950854,"NO2":0.0},{"X(m)":11561104.1659776326,"Y(m)":134733.058585539,"NO2":0.05783296},{"X(m)":11561103.315145839,"Y(m)":136747.3854372545,"NO2":0.619101},{"X(m)":11561102.4516803063,"Y(m)":138761.7256471134,"NO2":0.02089391},{"X(m)":11561101.5755806118,"Y(m)":140776.0794120168,"NO2":18.2236},{"X(m)":11561100.6868463214,"Y(m)":142790.4469288598,"NO2":21.83832},{"X(m)":11561099.7854769975,"Y(m)":144804.8283945511,"NO2":2.589856},{"X(m)":11561098.8714721967,"Y(m)":146819.2240060065,"NO2":0.0},{"X(m)":11561097.9448314663,"Y(m)":148833.6339601455,"NO2":0.001974617},{"X(m)":11561097.0055543538,"Y(m)":150848.0584538958,"NO2":0.0002526548},{"X(m)":11563105.8388952352,"Y(m)":132719.5758102555,"NO2":1.334192},{"X(m)":11563105.0138496533,"Y(m)":134733.9021190652,"NO2":0.7048039},{"X(m)":11563104.1763689928,"Y(m)":136748.2415895445,"NO2":2.006457},{"X(m)":11563103.3264528401,"Y(m)":138762.5944185916,"NO2":0.0},{"X(m)":11563102.4641007781,"Y(m)":140776.9608031045,"NO2":37.7027},{"X(m)":11563101.5893123802,"Y(m)":142791.3409399894,"NO2":48.75678},{"X(m)":11563100.7020872142,"Y(m)":144805.73502616,"NO2":5.256254},{"X(m)":11563099.8024248481,"Y(m)":146820.1432585351,"NO2":0.4017018},{"X(m)":11563098.8903248329,"Y(m)":148834.5658340435,"NO2":0.02389445},{"X(m)":11563097.9657867234,"Y(m)":150849.0029496195,"NO2":0.0000057832},{"X(m)":11565106.6860490367,"Y(m)":132720.3935835092,"NO2":0.06663989},{"X(m)":11565105.8741567526,"Y(m)":134734.7323110996,"NO2":0.1407793},{"X(m)":11565105.0500276368,"Y(m)":136749.0842007671,"NO2":0.3614089},{"X(m)":11565104.2136612814,"Y(m)":138763.4494494127,"NO2":2.615068},{"X(m)":11565103.3650572766,"Y(m)":140777.8282539398,"NO2":13.91094},{"X(m)":11565102.5042152032,"Y(m)":142792.2208112656,"NO2":35.17267},{"X(m)":11565101.6311346348,"Y(m)":144806.6273183052,"NO2":10.03782},{"X(m)":11565100.7458151449,"Y(m)":146821.0479719869,"NO2":0.07940782},{"X(m)":11565099.8482562937,"Y(m)":148835.4829692448,"NO2":0.001038789},{"X(m)":11565098.9384576418,"Y(m)":150849.9325070188,"NO2":0.0},{"X(m)":11567107.5454392694,"Y(m)":132721.1982144452,"NO2":0.0},{"X(m)":11567106.7467006836,"Y(m)":134735.5491612355,"NO2":0.04295366},{"X(m)":11567105.9359235186,"Y(m)":136749.9132705034,"NO2":0.0},{"X(m)":11567105.1131073739,"Y(m)":138764.2907391526,"NO2":2.895215},{"X(m)":11567104.2782518435,"Y(m)":140778.6817640975,"NO2":17.67259},{"X(m)":11567103.4313565176,"Y(m)":142793.0865422554,"NO2":26.59106},{"X(m)":11567102.5724209771,"Y(m)":144807.505270551,"NO2":16.80966},{"X(m)":11567101.7014447991,"Y(m)":146821.9381459182,"NO2":2.274314},{"X(m)":11567100.8184275553,"Y(m)":148836.3853652964,"NO2":0.008024883},{"X(m)":11567099.9233688097,"Y(m)":150850.8471256311,"NO2":0.0},{"X(m)":11569108.4168676864,"Y(m)":132721.9897026662,"NO2":0.0},{"X(m)":11569107.6312831938,"Y(m)":134736.352669069,"NO2":0.0002838454},{"X(m)":11569106.8338583782,"Y(m)":136750.7287983441,"NO2":0.6349015},{"X(m)":11569106.0245928504,"Y(m)":138765.1182873995,"NO2":0.001335126},{"X(m)":11569105.2034862079,"Y(m)":140779.5213331537,"NO2":14.26682},{"X(m)":11569104.3705380466,"Y(m)":142793.938132531,"NO2":19.74097},{"X(m)":11569103.5257479548,"Y(m)":144808.3688824646,"NO2":16.98232},{"X(m)":11569102.6691155192,"Y(m)":146822.8137798894,"NO2":2.22069},{"X(m)":11569101.8006403185,"Y(m)":148837.2730217536,"NO2":0.0},{"X(m)":11569100.9203219209,"Y(m)":150851.7468050093,"NO2":0.005256226},{"X(m)":11571109.3001360372,"Y(m)":132722.7680477803,"NO2":0.003944732},{"X(m)":11571108.5277060252,"Y(m)":134737.1428342058,"NO2":0.0003045356},{"X(m)":11571107.7436339539,"Y(m)":136751.5307838884,"NO2":1.304565},{"X(m)":11571106.9479194395,"Y(m)":138765.9320937445,"NO2":0.2675311},{"X(m)":11571106.1405620873,"Y(m)":140780.3469606957,"NO2":2.949056},{"X(m)":11571105.3215615023,"Y(m)":142794.7755816744,"NO2":14.81825},{"X(m)":11571104.4909172766,"Y(m)":144809.2181536196,"NO2":49.53163},{"X(m)":11571103.6486290079,"Y(m)":146823.6748734707,"NO2":15.88286},{"X(m)":11571102.7946962733,"Y(m)":148838.1459381811,"NO2":0.9250311},{"X(m)":11571101.9291186575,"Y(m)":150852.6315447088,"NO2":0.005183576}];
//first call to initMap. Determine center of map by url

function initMap() {
    //array of pathName
    var arrUrl = window.location.pathname.split('/');
    var location = arrUrl[3];
    var center;
    map = new google.maps.Map(document.getElementById('map'));
    if (location.toLowerCase() == "singapore"){
        center = new google.maps.LatLng(1.367165198,103.801163462);
        map.setZoom(10);
        getRelevantFolder(arrUrl[2], "Singapore");
    } else if (location.toLowerCase() == "hongkong"){
        center = new google.maps.LatLng(22.28911086466781,114.1491155592187);
        map.setZoom(16);
        getRelevantFolder(arrUrl[2], "Hong_Kong");
    }
    map.setCenter(center);
    
  }
function getRelevantFolder(typeOfEmission, city){
    var locationIRI = "http://dbpedia.org/resource/"+city;
    var agentScenario = prefix +  "/JPS_DISPERSION/" + typeOfEmission + "/results/latest";
    document.getElementById("loader").style.display = "block"; 
    //Part 1: get relevant folder
    $.get(agentScenario, {city:locationIRI}).done(function (data) {
        console.log('requested Scenario Agent for folder: '+data);
    }).then(function(data){
        var agentInfo = prefix +  "/JPS_SHIP/GetExtraInfo";
        //Part 2: get the relevant IRIs for ship, as well as for airStationIRIs
        // $.get(agentInfo, {path:data}).done(function (data) {
        //     var info=JSON.parse(data);
        //     //Part 3: Handle Ships if they are there
        //     var shipsIRI = info.ship.collection.items; 
        //     placeShips(shipsIRI);
        //     querySensor(city, function (sensorData) {
        //         renderSensorStations(sensorData);
        //     });
        //     document.getElementById("loader").style.display = "none"; 
        // })
        let agentInformation = prefix + "/JPS/ADMSOutputAllForShips";//"/info"
        console.log(agentInformation);
        $.get(agentInformation, {folder:data}).done(function (info) {
            setUpSlider(info.numheight, info.initialheight, info.numinterval);
            console.log(info.listofpol);
            populateDropDown(info.listofpol);
            arrXYPollutant = [info.x_coord, info.y_coord, info.grid]; //grid = noOfPollutantx(X*Y)
            //Part 4: Concentration data
            document.getElementById("loader").style.display = "none"; 
        })
    })
}
function setUpSlider(numOfLevel, initHeight, numInterval){
    var slider = document.getElementById("myRange");
    slider.min = initHeight;
    slider.step = numInterval;
    slider.max = initHeight + (numOfLevel-1)* numInterval;
}
function populateDropDown(listofpollutants){
    let dropdown = document.getElementById('locality-dropdown');
    dropdown.length = 0;

    let defaultOption = document.createElement('option');
    defaultOption.text = 'Choose Pollutant type';

    dropdown.add(defaultOption);
    dropdown.selectedIndex = 0;
    for (poll in listofpollutants){
        option = document.createElement('option');
        option.text = listofpollutants[poll];
        option.value = poll; //let the index be the value selected
        dropdown.add(option);
    }
}
function addheatmap(){
    map = new google.maps.Map(document.getElementById('map'));
    heatmap = new google.maps.visualization.HeatmapLayer({
        data: getPollutantAndHeight(),
        map: map
      });
}
function getPollutantAndHeight(){
    var arrUrl = window.location.pathname.split('/');
    var firstProjection;
    proj4.defs("EPSG:32648","+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs");
    if (arrUrl[3].toLowerCase()== "singapore"){
        if (arrUrl[2].toLowerCase()== "adms" ){
            firstProjection = "EPSG:3414";
        }else{
            firstProjection = "EPSG:32648";
        }
    }else{
        if (arrUrl[2].toLowerCase()== "adms" ){
            firstProjection = "EPSG:2326";
        }else{
            firstProjection = "EPSG:32650";
        }
    }
    heightLevel = slider.value;
    let dropD= document.getElementById("locality-dropdown");
    let pollutantIndex = dropD.options[dropD.selectedIndex].value;
    let sideLength = arrXYPollutant[0].length;
    let typeOfPollutant = arrXYPollutant[2][heightLevel][pollutantIndex];
    console.log(typeOfPollutant)
    //so length of Gases is a squared integer, pollutant index is an integer
    lotsOfMarkers = [];
    for (var i = 0; i < sideLength; i++) {
        console.log(i);
        var coordinate = proj4(firstProjection).inverse([arrXYPollutant[0][i],arrXYPollutant[1][i]]);
        var random = {location: new google.maps.LatLng(coordinate[0], coordinate[1]),
        weight: typeOfPollutant[i] } ;
        lotsOfMarkers.push(random);
    }
    return lotsOfMarkers;
}

var slider = document.getElementById("myRange");
var output = document.getElementById("demo");
output.innerHTML = slider.value;

slider.oninput = function() {
    output.innerHTML = this.value;
}
document.getElementById("myList").onchange = function() {
    addheatmap();
};

function placeShips(shipsIRI){
    for (ship of shipsIRI) {
        console.log(ship.lat, ship.lon, ship.name);
        var image = {
            url: 'https://sites.google.com/site/kmlfilescares/jsonsample/ship.png',
            scaledSize : new google.maps.Size(10, 10)
          };
        
        var marker = new google.maps.Marker({
            position: new google.maps.LatLng(ship.lat, ship.lon),
            map: map,
            title: ship.name,
            icon: image, 
            opacity: 0.5
          });
        
          listOfIRIs.push(marker);
      }
}
function renderSensorStations(sensorLocs) {
    //TODO: mock data
    for (let sIRI of sensorLocs){
        createMarker(sIRI);
    }
}

/** creates a single marker and places it on the google map
 * @param {List} lst of generators at that location
 */
function createMarker(lst){
    console.log(lst[2], lst[1]);
    var marker = new google.maps.Marker({
        position: new google.maps.LatLng(lst[2], lst[1]),
        map: map,
        title: lst[0], 
      });
    marker.addListener('click', function(){
        querySensorAttributes(lst[0], function (err, sensorAttributes) {
            if (err){console.log(err)}
            document.getElementById("loader").style.display = "block"; 
            console.log('got sensor attributes to show');
            console.log(sensorAttributes);
            sensorAttributes.names= ['pollutant', 'concentration','time','allpsi','mean','max','min','individualpsi']
            sensorAttributes.data.forEach(item=>{
                let name = item[0].split('/');
                name = name[name.length-1]
                name = name.split('.owl')[0]
                item[0] = name
                let unit = item.splice(-1)[0]
                let unitArr = unit.split('#')
                unit = unitArr.splice(-1)
                item[1] = parseFloat(item[1]).toFixed(2)+' '+unit
                item[4] = parseFloat(item[4]).toFixed(2)+' '+unit
                item[5] = parseFloat(item[5]).toFixed(2)+' '+unit
                item[6] = parseFloat(item[6]).toFixed(2)+' '+unit
                item[7] = parseFloat(item[7]).toFixed(2)

            })
            sensorAttributes.data.sort(function(a, b) {
                var nameA = a[0].toUpperCase(); // ignore upper and lowercase
                var nameB = b[0].toUpperCase(); // ignore upper and lowercase
                if (nameA < nameB) {
                    return -1;
                }
                if (nameA > nameB) {
                    return 1;
                }
            });
            renderAttributeTable(sensorAttributes);
        })
    });
    markers.push(marker);
}
function renderAttributeTable(attrs){
    let tableDiv = $("#sensorTable").empty();
    let tableStr= "<table class='table'><tr>";
    for (let tab of attrs.names){
        tableStr+="<th>"+tab+"</th>";
    }
    tableStr+="</tr>";
    for(let row of attrs.data){
        tableStr+="<tr>"
        for(let col of row){
            tableStr+="<td>"+col+"</td>"
        }
        tableStr+="</tr>"
    };
    tableStr+="</table>"
    tableDiv.append(tableStr);
    
    document.getElementById("loader").style.display = "none"; 
}
function querySensor(city, callback){
    let qstr = `
        PREFIX s:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>
        PREFIX t:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
        PREFIX sys:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        SELECT Distinct ?graph ?x ?y 
        {graph ?graph {
            ?s t:hasGISCoordinateSystem ?gs.
            ?gs t:hasProjectedCoordinate_y ?cy.
            ?cy sys:hasValue ?yv.
            ?yv sys:numericalValue ?y.
            ?gs t:hasProjectedCoordinate_x ?cx.
            ?cx sys:hasValue ?xv.
            ?xv sys:numericalValue ?x.
        }
        }
        `;

    $.get({
        url:metaEndpoint,
        'Content-Type':"application/json",
        data: { query: qstr,format:'json'}
    })
        .done(function( msg ) {
            let result =queryProcessor(msg).data
            let search = []
            for (let item of result){
                console.log(item[0])
                if(item[0].includes(city.toLowerCase())){
                    search.push(item);
                }
            }
            callback(search)
        }).fail(function(){
            alert("Search Query failed!" );
        });
}

function querySensorAttributes(stationIRI, callback) {
   let qstrT = `PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#>
        PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>
        PREFIX j6:<http://www.w3.org/2006/time#>
        PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
        SELECT Distinct ?prop ?propval  ?proptimeval ?allpsi ?mean ?max ?min ?individualpsi ?unit
        {graph stationIRI
        {
        ?graph j4:hasOverallPSI ?allpsi .
        ?prop   j2:hasValue ?vprop .
        ?prop j4:hasMeasuredPropertyMean ?mean .
        ?prop j4:hasMeasuredPropertyMax ?max .
        ?prop j4:hasMeasuredPropertyMin ?min .
        ?prop j4:hasPSI ?individualpsi .
        ?vprop   j4:prescaledNumValue ?propval .
        ?vprop   j2:hasUnitOfMeasure ?unit .
        ?vprop   j6:hasTime ?proptime .
        ?proptime   j6:inXSDDateTime ?proptimeval .
        }}
        ORDER BY DESC(?proptimeval) LIMIT10`;
    let qstr = qstrT.replace('stationIRI', '<'+stationIRI+'>');
    console.log(qstr);
    $.get({
        url:metaEndpoint,
        'Content-Type':"application/json",
        data: { query: qstr,format:'json'}
    })
        .done(function( strresult ) {
            console.log( "query sensor station result: " );
            console.log(strresult);
            console.log(typeof  strresult);
            let processed = queryProcessor(strresult);
            callback(null, processed);
        })
        .fail(function(err) {
            console.log( "query sensor attributes failed bc: ");
            console.log(err);
        });
}
function queryProcessor(str){
   let lines = str.split('\n');
   let results = [];
    let names = lines[0].split(',');
    for (let i =1; i< lines.length-1;i++){//remove last one which should be empty
        let vs = lines[i].split(',')
        results.push(vs)
    }
    return {data:results, names:names};
}
// select appropriate gas emission sample
function getPoints() {
    var size = NO2json.length;
    for (var i = 0; i < size; i++) {
        var random = {location: new google.maps.LatLng(NO2json[i]["X(m)"],NO2json[i]["Y(m)"]),
        weight: NO2json[i]["NO2"] } ;
            lotsOfMarkers.push(random);
    }

  return lotsOfMarkers;
}
function changeRadius(numeral) {
    heatmap.set('radius', heatmap.get('radius') ? null : numeral);
}
function getLegends(){

    var container = d3.select("#chart");
    var colourScale = d3
        .scaleSequential(d3.interpolateRdYlGn)
        .domain([46,0]);
    var domain = colourScale.domain();
    
    var width = 100;
    var height = 500;
    var  paddedDomain = fc.extentLinear()
    .pad([0.05, 0.05])
    .padUnit("percent")(domain);
  var [min, max] = paddedDomain;
  var expandedDomain = d3.range(min, max, (max - min) / height);
    var xScale = d3
        .scaleBand()
        .domain([0, 1])
        .range([0, width]);
    
    var yScale = d3
        .scaleLinear()
        .domain(paddedDomain)
        .range([height, 0]);
    
    var svgBar = fc
      .autoBandwidth(fc.seriesSvgBar())
      .xScale(xScale)
      .yScale(yScale)
      .crossValue(0)
      .baseValue((_, i) => (i > 0 ? expandedDomain[i - 1] : 0))
      .mainValue(d => d)
      .decorate(selection => {
        selection.selectAll("path").style("fill", d => colourScale(d));
      });
    
    var axisLabel = fc
      .axisRight(yScale)
      .tickValues([...domain, (domain[1] + domain[0]) / 2,
       (domain[1] + domain[0]) / 5, (domain[1] + domain[0]) / 5*2,
       (domain[1] + domain[0]) / 5*3,(domain[1] + domain[0]) / 5*4 ]);
    
    var legendSvg = container.append("svg")
        .attr("height", height)
        .attr("width", width);
    
    var legendBar = legendSvg
        .append("g")
        .datum(expandedDomain)
        .call(svgBar);
    
    var barWidth = Math.abs(legendBar.node().getBoundingClientRect().x);
    legendSvg.append("g")
        .attr("transform", `translate(${barWidth})`)
      .datum(expandedDomain)
      .call(axisLabel)
      .select(".domain")
      .attr("visibility", "hidden");
    
    container.style("margin", "1em");
}

 
/** sleep function for javascript
 * 
 * @param {Integer} ms time in miliseconds 
 */
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
/**async function that would provide five minutes to complete simulation and then run callback function
 * @param {Function} callback
 */
async function delayedCallback(callback) 
    {
    var dt = Date();
    console.log("Wait for simulation to finish: "+dt);
    await sleep(300*1000);//five minutes?
    dt = Date();
    console.log("Check callback "+dt);
    callback();
  }

/** clears all markers on the page
 * 
 */
function clearMarkers() {
    if(!markers){
        return;
    }
    for(marker of markers){
        marker.setMap(null);
        marker=null;
    }
}



/** constructs and calls upon openWindow for foodcourts and 
 * 
 * @param {String} id iri of line
 * @param {function} callback displays content of infowindow as set in drawLines in PopupMap
 */
function setMarkerMen(id, callback){
    if (id.includes("FoodCourt")){
        typeInfo = FCQuery;
    }else if (id.includes("OnSite")){
        typeInfo = OnWQuery;   
    }else{
        typeInfo = WTQuery;
    }
    var promise1 = new Promise(function (resolve, reject){
        resolve(openWindow(id, typeInfo, callback));
    }); 
    promise1.catch(alert);
}

/** creates new scenario through ScenarioModifier.java agent
     * @param scenarioname the name of the scenario, be it base or specific folder 
     * @param agenturl: GET request to Java Backend Servlet
     * @param sparql: JSON packets or what not that the Java backend could request. 
     * @returns modified url for future use. 
     */
    function createNewUrlForAgent(scenarioname, agenturl, agentparams) {

        var url;
        if ((scenarioname == null) || scenarioname == "base") {
            url = agenturl;
        } else {
            agentparams['scenarioagentoperation'] = agenturl;
            var scenariourl = prefix + '/jps/scenariomod/' + scenarioname + '/call';
            url = scenariourl;
        }

        return url + "?query=" + encodeURIComponent(JSON.stringify(agentparams));
    }
/** accesses parallel scenarios through these helper functions
 * @param scenarioname the name of the scenario, be it base or specific folder 
 * @param iri: iri of resource to be queried. 
 * @param sparql: the sparql update to be fired
 * @returns modified url for query
 */
function createUrlForSparqlUpdate(scenarioname, iri, sparql) {

    var url2 = prefix + '/jps/scenario/' + scenarioname + '/update?query=';
    urljson = {"scenarioresource":iri,"sparqlupdate":sparql};
    url2 += encodeURIComponent(JSON.stringify(urljson)); 
    //url2 += JSON.stringify(urljson); 
    return url2;    
}
/*** accesses parallel scenarios through these helper functions
 * @param scenarioname the name of the scenario, be it base or specific folder 
 * @param iri: iri of resource to be queried. 
 * @param sparql: the sparql query to be fired
 * @returns modified url for update
 */
function createUrlForSparqlQuery(scenarioname, iri, sparql) {

    var url2 = prefix + '/jps/scenario/' + scenarioname + '/query?query=';
    urljson = {"scenarioresource":iri,"sparqlquery":sparql};
    url2 += encodeURIComponent(JSON.stringify(urljson)); 
    //url2 += JSON.stringify(urljson); 
    return url2;    
}
/*** accesses parallel scenarios through these helper functions
 * @param scenarioname the name of the scenario, be it base or specific folder 
 * @param agenturl: GET request to Java Backend Servlet
 * @param sparql: JSON packets or what not that the Java backend could request. 
 * @returns modified url for update
 */
function createUrlForAgent(scenarioname, agenturl, agentparams) {

    var url;
    if ((scenarioname == null) || scenarioname == "base") {
        url = agenturl;
    } else {
        agentparams['scenarioagentoperation'] = agenturl;
        var scenariourl = prefix + '/jps/scenario/' + scenarioname + '/call';
        url = scenariourl;
    }

    return url + "?query=" + encodeURIComponent(JSON.stringify(agentparams));
}

