/**
 * Created by MASTE on 9/18/2017.
 */

var theScene;
var showLayout = true;
var enablePop = true;
var singleClick = true;



var init = function() {

    // prepare all important html elements
    // 1. popup includes both the popup window and its background
    // 2. span is the close button on the popup window
    // 3. popupContent is the popup window
    var popup = document.getElementsByClassName('modal')[0];
    var span = document.getElementsByClassName("close")[0];
    var popupContent = document.getElementById('popupContent');


    // when span is clicked, hide the popup window and remove all its content
    span.onclick = function () {

        popup.style.display = "none";

        // clean up the element within popup window
        while (popupContent.firstChild) {
            popupContent.removeChild(popupContent.firstChild);
        }


        // enable popup window 100 ms after closing the popup window.
        // if not, when close button overlaps with some device, trouble appears
        setTimeout(function () {
            enablePop = true;
        },100)    };



    var canvas = document.getElementById('renderCanvas');
    var engine = new BABYLON.Engine(canvas,true);
    var createScene = function () {
        var scene = new BABYLON.Scene(engine);

        //Create a light
        var light1 = new BABYLON.HemisphericLight("light1", new BABYLON.Vector3(1, 1, 1), scene);
        var light2 = new BABYLON.HemisphericLight("light2", new BABYLON.Vector3(1, 1, 1), scene);
        light2.intensity = 0.5;
        //Create an Arc Rotate Camera - aimed negative z this time
        var camera = new BABYLON.ArcRotateCamera("Camera", Math.PI * 3 / 2 , 1, 220, BABYLON.Vector3.Zero(), scene);
       // camera.rotation.y = Math.PI / 2;






        camera.attachControl(canvas, true);





        //Creation of a plane
        var plane = BABYLON.MeshBuilder.CreatePlane("plane_0", {width:329.1, height:122.2}, scene);
        plane.position.y = 0;
        plane.rotation.x = Math.PI / 2;



        var options = {
            width: 1,
            height: 20,
            depth: 78.1
        };



        // create the Material for the plane(Floor layout plane)
        var materialPlane = new BABYLON.StandardMaterial("texturePlane", scene);


        // prepared for option checkboxes.
        if(showLayout)
        {
            materialPlane.diffuseTexture = new BABYLON.Texture("texture/BMSLayout.JPG", scene);
        }
        else
        {
            materialPlane.diffuseTexture = new BABYLON.Texture("texture/white.bmp", scene);
        }
        materialPlane.diffuseTexture.uScale = 1.0;//Repeat 5 times on the Vertical Axes
        materialPlane.diffuseTexture.vScale = 1.0;//Repeat 5 times on the Horizontal Axes
        materialPlane.backFaceCulling = false;//Always show the front and the back of an element
        materialPlane.specularPower = 1132;



        //Apply the materials to meshes

        plane.material = materialPlane;
        scene.clearColor =  new BABYLON.Color3.White;



        // 1.   read JSON file, get name, dimensions, positions
        // 2.   create meshes, attach material


        var loader = new BABYLON.AssetsManager(scene);

        var position = 10;
        var pos = function(t) {
            t.loadedMeshes.forEach(function(m) {
                m.position.x -= position;
                m.rotation.y = Math.PI ;
                m.scaling.x = 5;
                m.scaling.y = 5;
                m.scaling.z = 5;
                m.id = 'FH';

            });
            position += 1;

        };

        //var bunny = loader.addMeshTask("bunny", "", "models/", "FH.obj");
        //bunny.onSuccess = pos;


        loader.onFinish = function() {
            engine.runRenderLoop(function() {
                scene.render();
            });
        };

        loader.load();
        return scene;
    };




    var scene = createScene();
    theScene = scene;
    readJSON(scene);




    engine.runRenderLoop(function() {
        scene.render();
    });



// When the user clicks on the button, open the modal

// When the user clicks on <span> (x), close the modal

// When the user clicks anywhere outside of the modal, close it


    window.addEventListener('resize',function () {
        engine.resize();
    });
  
    
    window.addEventListener("dblclick",function (evt) {
        console.log(evt.detail);




        singleClick = false;
        var pickResult = scene.pick(evt.clientX, evt.clientY);

        if (pickResult.hit) {
            id = pickResult.pickedMesh.id;
            if (id == 'plane_0' || id.includes('Wall')) {

            }
            else {

                var url = "http://www.theworldavatar.com/BMS/" + id + '.owl';
                var win = window.open(url, '_blank');
                win.focus();

            }

        }


    });
    
    window.addEventListener("click", function (evt) {
        // We try to pick an object

        console.log('event',evt);

        if(enablePop && evt.detail <= 1 ){
        var pickResult = scene.pick(evt.clientX, evt.clientY);

        if (pickResult.hit) {


            id = pickResult.pickedMesh.id;
            if (id == 'plane_0') {

            }
            else {
                switch (id) {
                    case 'Door':
                        openTheDoor();
                        break;

                    default:
                        if(id.includes("Wall"))
                        {

                        }
                        else
                        {

                            setTimeout(function () {

                                if(singleClick)
                                {
                                    fillPopup(id,popup,popupContent);
                                }

                                singleClick = true;


                            },200);



                        }

                        break;
                }
            }
        }

    }


    });


//When click event is raised


};

window.addEventListener('DOMContentLoaded',init);

function fillPopup(id,popup,popupContent)
{
    var muri = "http://www.theworldavatar.com/BMS/" + id + '.owl';
    console.log('muri', muri);
    var foundIRI = false;
    move(0,40); // move the progress bar to 40%
    $.ajax({
        url: "http://www.theworldavatar.com:82/getAttrList",
        method: "POST",
        data: JSON.stringify({uri: muri}),
        contentType: "application/json; charset=utf-8",
        success: function (attrPairs) {

            move(40,80);
            enablePop = false;  // when popup window appears, disable popup to avoid triggering other popup window by accident
            var tableBody = document.createElement('table');
            var headRow = document.createElement('tr');
            var head1 = document.createElement('th');
            head1.innerText = 'Name';

            var head2 = document.createElement('th');
            head2.innerText = 'Value';
            var head3 = document.createElement('th');
            head3.innerText = 'Unit';
 
			
            headRow.appendChild(head1);
            headRow.appendChild(head2);
			headRow.appendChild(head3);
            tableBody.appendChild(headRow);

            attrPairs = sortByKey(attrPairs,'name').reverse();


            var prevName = 'xxx';
            for (var i = 0; i < attrPairs.length; i++) {

                var row = document.createElement('tr');
                var td1 = document.createElement('td');
                td1.innerText = attrPairs[i].name;
                var td2 = document.createElement('td');



                if(Array.isArray(attrPairs[i].value))
                {
                    var tempString = '';
                    for(var k = 0; k < attrPairs[i].value.length; k++)
                    {
                        var value = attrPairs[i].value[k].value;
                        var time = attrPairs[i].value[k].time;



                        time = new Date(time).getTime();

                        var formattedTime = new Date(time).toLocaleTimeString();


                        tempString = tempString + value + ' --- ' + formattedTime + '\n';

                    }

                    td2.innerText = tempString;

                 }
                else
                {
                    td2.innerText = attrPairs[i].value;

                }



				var td3 = document.createElement('td');
				if(attrPairs[i].unit)
				{
					td3.innerText = attrPairs[i].unit;	
				}
				else
				{
					if(attrPairs[i].name.includes('DamperState'))
					{
						td3.innerText = '%';
					}
					else
					{
						td3.innerText = 'nil';
					}
				}
				
				
				
                if(attrPairs[i].name.includes(prevName))
                {

                }
                else
                {
                row.appendChild(td1);
                row.appendChild(td2);
				row.appendChild(td3);

                }
                var value = attrPairs[i].value;




                var name  = attrPairs[i].name;
				var unit = attrPairs[i].unit;
				console.log('---- attrPairs ---',attrPairs);
                prevName = name;


                if(value.includes('http://www.theworldavatar.com') && !(value.includes('#')))
                {
                    foundIRI = true;
                    console.log('id--',id);
                    td2.onclick = function () {
                        fillPopup(this.innerText.split('BMS/')[1].split('.owl')[0],popup,popupContent); };
                }
                // filter out object relations
                // should be solved at the backend


                if(value.includes('theworldavatar') && (value.includes('#')))
                {

                }
                else
                {
                    tableBody.appendChild(row);
                }
            }


            // Due to the exception of declaring 'hasIRI' triples, some of the hasIRI pairs can not be found by
            // searching Literals.
            $.ajax({
                url: "http://www.theworldavatar.com:82/getChildrenSingle",
                method: "POST",
                data: JSON.stringify({uri: muri}),
                contentType: "application/json; charset=utf-8",
                success: function (_attrPairs) {
                    console.log('muri requested',muri);
                    console.log('from getchildren ---',_attrPairs);
                    move(80,100);

                    if(foundIRI)
                    {

                    }
                    else
                    {
                        for (var i = 0; i < _attrPairs.length; i++) {

                            var row = document.createElement('tr');
                            var td1 = document.createElement('td');
                            if(_attrPairs[i].includes('/BMS/'))
                            {
                                td1.innerText = _attrPairs[i].split('/BMS/')[1];

                            }
                            else
                            {

                            }
                            var td2 = document.createElement('td');
                            td2.innerText = _attrPairs[i];

					 
					 
				
							
                            row.appendChild(td1);
                            row.appendChild(td2);
 
				
				
							
                            if(_attrPairs[i].includes('http://www.theworldavatar.com') )
                            {
                                console.log('id--',id);
                                td2.onclick = function () {


                                    fillPopup(this.innerText.split('BMS/')[1].split('.owl')[0],popup,popupContent); };
                            }
                            tableBody.appendChild(row);


                        }

                    }


                        popup.style.display = 'block';
                        document.getElementById("myBar").style.display = 'none';

                }}
                );


            if(popupContent.childNodes.length >= 2)
            {
                popupContent.removeChild(popupContent.lastChild);
            }



            popupContent.appendChild(tableBody);





        }
    });
}


function sortByKey(array, key) {
    return array.sort(function(a, b) {
        var x = a[key]; var y = b[key];
        return ((x < y) ? -1 : ((x > y) ? 1 : 0));
    });
}




function readJSON(scene)
{


    $.getJSON("./layout", function( data ) {
        console.log('data --- ',data);
        var devices = data['Devices'];
        for (var i = 0; i < devices.length; i ++)
        {
            var device = devices[i];
            //console.log('device name',device);
            //function createItem(name,type,color,dimensions,positions,scene)
            // basing on layout file, create items accordingly.
            createItem(device.name,device.type,device.color,device.dimensions,device.positions,scene);

        }
    });

}

// move the progress bar to limit%
function move(start,limit) {
    var elem = document.getElementById("myBar");
    elem.style.display = 'block';
    var width = start;
    var id = setInterval(frame, 10);
    function frame() {
        if (width >= 100) {
            clearInterval(id);
        } else {
            if(width <= limit)
            {
                width++;
                elem.style.width = width + '%';
            }

        }
    }
}

function openTheDoor() {
    alert('Opening the Door')
}

// item,x,y,z,dimensions.
function placeItem(item,x,y,z,dimensions)
{
    var width = dimensions.width;       //x
    var height = dimensions.height;     //y
    var depth = dimensions.depth;       //z



    // adjust object's position basing on layout plane's dimensions

    item.position.x = x - 163.44 + width/2;
    item.position.y = y + height/2;
    item.position.z = z - 61.1 + depth/2;

}

function createItem(name,type,color,dimensions,positions,scene)
{
        // get the dimension of object to be create
        var options =
        {
            width: dimensions[0],
            height: dimensions[1],
            depth: dimensions[2],
            color: color
        };

    var width = options.width;       //x
    var height = options.height;     //y
    var depth = options.depth;       //z

    if(type == 'Sensor')
    {

    }
    else
    {
    var newDevice = BABYLON.MeshBuilder.CreateBox(name,options,scene);

    newDevice.position.x = positions[0] - 163.44 + width/2;
    newDevice.position.y = positions[1] + height/2;
    newDevice.position.z = positions[2] - 61.1 + depth/2;
    }

    switch(type)
    {
        case 'OuterWall':
            var wallMaterial = new BABYLON.StandardMaterial("texturePlane", scene);
            wallMaterial.diffuseTexture = new BABYLON.Texture("texture/white.bmp", scene);
            wallMaterial.diffuseTexture.uScale = 1.0;//Repeat 5 times on the Vertical Axes
            wallMaterial.diffuseTexture.vScale = 1.0;//Repeat 5 times on the Horizontal Axes
            wallMaterial.backFaceCulling = false;//Always show the front and the back of an element
            wallMaterial.diffuseColor  = new BABYLON.Color3(0, 0, 0.5);
            wallMaterial.alpha = 0.2;
            newDevice.material = wallMaterial;
            break;
        case 'Device':
            var deviceMaterial = new BABYLON.StandardMaterial("texturePlane", scene);


            var textLabelTexture = new BABYLON.DynamicTexture("textLabelTexture", 50, scene, true);
            textLabelTexture.hasAlpha = true;


            var axisHeight = 2;
            var labelHeight = 10;
            var color = 'red';
            if(name.includes('VAV'))
            {
                axisHeight = 2;
                labelHeight = 10;
                color = 'red'
            }
            else
            {
                axisHeight = 3;
                labelHeight = 10;
                color = 'green'

            }
            textLabelTexture.drawText(name, 5, 40, "bold 12px Arial", color , "transparent", true);
            var textLabel = new BABYLON.Mesh.CreatePlane(name, 5, scene, true);
            textLabel.position.x = positions[0] - 163.44 + width/2;
            textLabel.position.y = positions[1] + height/2 + labelHeight;
            textLabel.position.z = positions[2] - 61.1 + depth/2;
            textLabel.material = new BABYLON.StandardMaterial("TextPlaneMaterial", scene);
            textLabel.material.backFaceCulling = false;
            textLabel.material.specularColor = new BABYLON.Color3(0, 1, 0);
            textLabel.material.diffuseTexture = textLabelTexture;
            textLabel.rotation.x = Math.PI / 4;




            var axisX = BABYLON.Mesh.CreateLines(name, [
                    new BABYLON.Vector3(newDevice.position.x,newDevice.position.y,newDevice.position.z),
                    new BABYLON.Vector3(textLabel.position.x,textLabel.position.y - 2,textLabel.position.z)
            ], scene);


            axisX.color = new BABYLON.Color3(0, 0, 0.5);

            deviceMaterial.diffuseTexture = new BABYLON.Texture("texture/white.bmp", scene);
            deviceMaterial.diffuseTexture.uScale = 1.0;//Repeat 5 times on the Vertical Axes
            deviceMaterial.diffuseTexture.vScale = 1.0;//Repeat 5 times on the Horizontal Axes
            deviceMaterial.backFaceCulling = false;//Always show the front and the back of an element
            deviceMaterial.diffuseColor  = new BABYLON.Color3(180/255, 207/255,235/255);
            deviceMaterial.alpha = 0.6;
            newDevice.material = deviceMaterial;
            break;


        case 'InnerWall':

            var wallMaterial = new BABYLON.StandardMaterial("texturePlane", scene);
            wallMaterial.diffuseTexture = new BABYLON.Texture("texture/white.bmp", scene);
            wallMaterial.diffuseTexture.uScale = 1.0;//Repeat 5 times on the Vertical Axes
            wallMaterial.diffuseTexture.vScale = 1.0;//Repeat 5 times on the Horizontal Axes
            wallMaterial.backFaceCulling = false;//Always show the front and the back of an element
            wallMaterial.diffuseColor  = new BABYLON.Color3(1, 1, 1);
            wallMaterial.alpha = 0.4;
            newDevice.material = wallMaterial;
            break;


        case 'Door':



            var doorMaterial = new BABYLON.StandardMaterial("doorTexture", scene);
            doorMaterial.diffuseTexture = new BABYLON.Texture("texture/woodDoor.jpg", scene);
            doorMaterial.diffuseTexture.uScale = 1.0;//Repeat 5 times on the Vertical Axes
            doorMaterial.diffuseTexture.vScale = 1.0;//Repeat 5 times on the Horizontal Axes
            doorMaterial.backFaceCulling = false;//Always show the front and the back of an element
            doorMaterial.diffuseColor  = new BABYLON.Color3(1, 1, 1);
            doorMaterial.alpha = 0.2;
            newDevice.material = doorMaterial;
            break;

        case 'Sensor':
            newDevice = null;
            newDevice = BABYLON.Mesh.CreateSphere(name,1,1,scene);
            //var newDevice = BABYLON.MeshBuilder.Create(name,options,scene);


            var textLabelTexture = new BABYLON.DynamicTexture("textLabelTexture", 50, scene, true);
            textLabelTexture.hasAlpha = true;


            var axisHeight = 2;
            var labelHeight = 10;
            var _color = 'red';
            if(name.includes('VAV'))
            {
                axisHeight = 2;
                labelHeight = 10;
                _color = 'red'
            }
            else
            {
                axisHeight = 3;
                labelHeight = 10;
                _color = 'red'
            }

            textLabelTexture.drawText(name, 5, 40, "bold 12px Arial", _color , "transparent", true);
            var textLabel = new BABYLON.Mesh.CreatePlane(name, 5, scene, true);
            textLabel.position.x = positions[0] - 163.44 + width/2;
            textLabel.position.y = positions[1] + height/2 + labelHeight;
            textLabel.position.z = positions[2] - 61.1 + depth/2;
            textLabel.material = new BABYLON.StandardMaterial("TextPlaneMaterial", scene);
            textLabel.material.backFaceCulling = false;
            textLabel.material.specularColor = new BABYLON.Color3(0, 1, 0);
            textLabel.material.diffuseTexture = textLabelTexture;
            textLabel.rotation.x = Math.PI / 4;
            newDevice.position.x = positions[0] - 163.44 + width/2;
            newDevice.position.y = positions[1] + height/2;
            newDevice.position.z = positions[2] - 61.1 + depth/2;

            var axisX = BABYLON.Mesh.CreateLines(name, [
                new BABYLON.Vector3(newDevice.position.x,newDevice.position.y,newDevice.position.z),
                new BABYLON.Vector3(textLabel.position.x,textLabel.position.y - 2,textLabel.position.z)
            ], scene);

            axisX.color = new BABYLON.Color3(0, 0, 0.5);

            var ThermometerMaterial = new BABYLON.StandardMaterial("", scene);
            ThermometerMaterial.diffuseTexture = new BABYLON.Texture("texture/" + color + ".bmp", scene);
            ThermometerMaterial.diffuseTexture.uScale = 1.0;//Repeat 5 times on the Vertical Axes
            ThermometerMaterial.diffuseTexture.vScale = 1.0;//Repeat 5 times on the Horizontal Axes
            ThermometerMaterial.backFaceCulling = false;//Always show the front and the back of an element
            ThermometerMaterial.diffuseColor  = new BABYLON.Color3(1, 1, 1);
            ThermometerMaterial.alpha = 1;
            newDevice.material = ThermometerMaterial;


        default:

    }



    console.log('Created New Item');





 //   var StandardDeviceMaterial;
}

function checkboxLogic(e)
{
    console.log('checkbox',e.name);

    var name = e.name;
    switch(name)
    {
        case 'showLayout':

            showLayout = e.checked;
            init();
            console.log('show layout',showLayout);
            break;

        case 'showSensor':
            break;

    }

}