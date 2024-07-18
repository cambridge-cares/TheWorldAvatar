    

    /**
const data = {"tupleNumber": 120, "flowNumber": "10", "flowList": [[1.0, 2.0], [1.0, 3.0], [1.0, 5.0], [3.0, 2.0], [4.0, 1.0], [4.0, 2.0], [4.0, 3.0], [4.0, 5.0], [5.0, 2.0], [5.0, 3.0]], "modifyList": [[895.0, 1.0, 2.0], [1512.0, 1.0, 5.0], [638.0, 4.0, 1.0]], "saveNumber": 3045.0};
     **/
/***
for heatwaste, implements popupMap
***/
   var plants = ["Zeon", "Exxonmobil","Singapore refining","Lanxess BTR Plant", "Chang Chun"]
var hwMap = new PopupMap({useCluster:false});

    var FileLinkMap = function (data, options) {
        var width = options.width ||$(document).width(),
            height = options.height || 2000,
            charge = options.charge || -3000,
            distance = options.distance || 100,
            nodeR = options.nodeR || 15,
            textSize = options.textSize || 5;

        var colorList = ["#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
            "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
            "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
            "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
            "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
            "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
            "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
            "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
            "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
            "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
            "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
            "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329"];
        var colorList2 = [


            "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C", "#000000"];
        var colorMap = {};
        var mapSize = 0;

        var bubbleMap = {};
        bubbleMap.nodesArr = [];

        function packNodesArr(links, coords, serviceUrls) {
            console.log("@@@@@@@@@@@@@@@@@@@"+JSON.stringify(serviceUrls))
            var nodes = {};
            var nodesArr = [];


            function getDomain(str) {
                if (!str) {
                    return null;
                }

                if(typeof str !== "string"){

                    return str.toString()
                }
                str = str.replace("theworldavatar", "jparksimulator");
                str = str.replace("file:/C:/", "http://www.jparksimulator.com/");
                var arr = str.split("/");


                console.log(arr);
                let domain = "";
                if (arr.length < 3) {
                    domain += arr[2];
                }
                for (let i = 2; i < arr.length - 1; i++) {
                    domain += "/" + arr[i];
                }
                return domain;
            }

            function getSimpleName(url) {
                if (!url) {
                    return "";
                }
                if(typeof url !== "string"){

                    return url.toString()
                }
                var arr = url.split("/");

                //  return (arr[arr.length - 1] === "")? arr[arr.length-3]+'/'+arr[arr.length-2] : arr[arr.length-2]+"/"+arr[arr.length -1];
                if(arr.length < 2){
                    console.log("undefined name :" + url)
                    return "";
                }
                return (arr[arr.length - 1] === "") ? arr[arr.length - 2] : arr[arr.length - 1];

                return url;
            }

            /*search for coordinates in coordinate array by uri*/
            function getCoord(uri) {
                if(!coords) return
                for (let i = 0; i < coords.length; i++) {
                    let coord = coords[i];
                    //   console.log("url in packed coords+ " +coord.url);
                    //console.log("coord in packed coords+ " +JSON.stringify(coord.coord));

                    if (coord.url == uri) {
                        return coord.coord;
                    }
                }

                return null;
            }

            /***
             * search for serivceUrl in service array by uri
             */
            function  getServiceUrl(uri) {
                console.log("search service url for "+uri)

                if(!serviceUrls||serviceUrls.length < 1){
                    console.log("Did not find any service urls")
                    return null;
                }
                for (let i = 0; i < serviceUrls.length; i++) {
                    let serviceUrl = serviceUrls[i];
                    console.log("comparing :"+ serviceUrl.url)
                    if (serviceUrl&& serviceUrl.url == uri) {
                        console.log("!!!!!!!!!! found service Url node for "+serviceUrl )
                        return serviceUrl.serviceUrl;
                    }
                }

                return null;
            }

            // Compute the distinct nodes from the links.
            links.forEach(function (link) {



                link.source = nodes[link.source] || (nodes[link.source] = {
                        url: link.source,

                        name: link.source,
                        domain: link.source,
                        count: 0,


                    });

                if (nodes[link.target]) {
                    nodes[link.target].level = parseInt(link.level) + 1;
                }
                link.target = nodes[link.target] || (nodes[link.target] = {
                        url: link.target,
                        name: link.target,
                        domain: link.target,
                        count: 0
                                  //add to node attri: geo coordinates


                    });


            });

            let index = 0;
            console.log("@@@@@@@@@@@@@@@@@@@@@@");
            for (let link of links) {

                console.log(index + "  :" + JSON.stringify(link));
                index++;
            }
            //packs object :nodes into array
            for (var URI in nodes) {
                if (nodes.hasOwnProperty(URI)) {

                    if (nodes[URI].name.toLowerCase().indexOf("world") !== -1)//manually add level to world
                    {
                        nodes[URI].level = 0;
                    }

                    nodesArr.push(nodes[URI]);
                    console.log("packed node: " + JSON.stringify(nodes[URI]))
                    //console.log("count" + nodes[attr].count);
                }
            }
            return nodesArr;
        }

        function setBodyS(node) {
            return -350;

            //return charge;
        }



        /**
         * Allocate color for nodes of each domain
         * @param d  datum
         * @returns one color defined in colormap
         */
        function allocateColor(d) {
            // is this exist in color map?

            if (d.level !== undefined && d.level !== null && !isNaN(d.level)) {

                return colorList2[d.level];
            }

            colorMap[d.url] = colorMap[d.url] || (colorList[mapSize++]);

            if (mapSize >= mapSize.length) {
                mapSize = 0;
                console.log("WARNING: DOMAIN NUMBER EXISTS COLOR MAP NUMBER!");
            }

            return colorMap[d.url];
        }

        function sortOrder(d, i) {

            if (d.level !== undefined && d.level !== null && !isNaN(d.level)) {

                return d.level * 1000 + i;
            }

            return 100000 + i;
        }

        function defineLegend(d) {
            if (d.level !== undefined && d.level !== null && !isNaN(d.level)) {

                return d.level;
            }

            return d.domain;
        }

        var svg = d3.select("#draw-panel").append("svg")
            .attr("width", width)
            .attr("height", height);
        var container = svg.append("g");
        var gP = container.append("g").attr("class", "pathsg");
        var gN = container.append("g").attr("class", "nodesg");

        var gT = container.append("g").attr("class", "textsg");

        function clear() {
            g.selectAll("line").data([]).exit().remove();
            g.selectAll("circle").data([]).exit().remove();
            g.selectAll("text").data([]).exit().remove();
        }

        bubbleMap.update = function (links, coords, serviceUrls) {
            bubbleMap.nodesArr = packNodesArr(links, coords, serviceUrls);


            console.log(links)
            console.log(bubbleMap.nodesArr)
            //set force simulationf
            var simulation = d3.forceSimulation()
                .force("link", d3.forceLink(links).distance(100))
                .force("charge", d3.forceManyBody().strength(setBodyS))
                .force("center", d3.forceCenter(width / 2, height / 2));

            var path = gP.selectAll("line") //linking lines
                .data(links, function (d) {
                    return d.source.domain + d.source.name + d.target.domain + d.target.name;
                });

            path.exit().remove();


            path.enter().append("line")

                .attr("stroke-width" ,function (d) {
                    console.log(Math.sqrt(d.value))
                    return d.value/100

                })
                .attr("stroke", function () {
                    return "#666"
                })

            ;
            // path.exit().remove();
            path = gP.selectAll("line");

            console.log(JSON.stringify(bubbleMap.nodesArr))
            var circle = gN.selectAll("a.cir") //node bubbles
                .data(bubbleMap.nodesArr, function (d) {
                    return d.url;
                })

            circle.exit().remove();
            console.log(circle.enter());

            let timer = 0;
            let sglclickPrevent = false;
            circle.enter().append("a")
                .attr('class', 'cir')
                //.append("a")
                .attr("xlink:href", function (d) {
                    console.log("@@@@@@@@@@@@draw node: " + d.name);
                    return d.url;
                })
                .on("click", function (d) {//update this infor in
                    d3.event.preventDefault();
                    d3.event.stopPropagation();

                    timer = setTimeout(function() {
                        if (!sglclickPrevent) {
                            clickAction();
                        }
                        sglclickPrevent = false;
                    }, 200);


                    function clickAction() {
                        if(d.serviceUrl && d.serviceUrl!==""){
                            console.log(d.serviceUrl);
                            window.open(d.serviceUrl);
                        }

                        console.log(d3.select(this));
                        let d3node = d3.select(this)
                        unHighLightAll();
                        hightLightResult(d.name, d3node);
                        if (d.coord) {
                            $("#search-center-x").val(d.coord.x);
                            $("#search-center-y").val(d.coord.y);
                        }
                    }
                })
                .on("dblclick", function (d) {
                    clearTimeout(timer);
                    sglclickPrevent = true;
                    d3.event.stopPropagation()
                    window.open(d.url);

                })

                .append("circle")
                .attr("id", function (d) {
                    console.log("!!!!!!!!!!!!!!")
                    console.log(d);
                    return d.name;
                })
                .attr("r", nodeR)
                .attr("class", "nodes")
                .attr("fill", allocateColor)
                .attr("data-legend-pos", sortOrder)
                .attr("data-legend", defineLegend)
                .on("mouseover", function (d) {
                    handleMouseOver(d3.select(this), d);
                })         //highlight all links when mouse over
                .on("mouseout", function (d) {
                    handleMouseOut(d3.select(this), d);

                })
                .call(d3.drag()                         //enable user to drag
                    .on("start", dragstarted)
                    .on("drag", dragged)
                    .on("end", dragended)
                );


            circle = gN.selectAll("a.cir");
            var circleDraw = gN.selectAll("a.cir").select("circle.nodes");

            var text = gT.selectAll("text.nodeTag")  //node tags
                .data(bubbleMap.nodesArr, function (d) {
                    return d.domain + d.name;
                });
            text.exit().remove();


            text.enter().append("text")
                .attr("class", "nodeTag")
                .attr("x", textSize)
                .attr("y", "0.31em")
                .text(function (d) {
                    return d.name;
                });
            text = gT.selectAll("text.nodeTag");


            var textL = gT.selectAll("text.lineTag")  //node tags
                .data(links, function (d) {
                    return  d.source.name + d.target.name ;
                });
            textL.exit().remove();


            textL.enter().append("text")
                .attr("class", "lineTag")
                .attr("x", textSize)
                .attr("y", "0.31em")
                .text(function (d) {
                    return d.value;
                });
            textL = gT.selectAll("text.lineTag");

            simulation
                .nodes(bubbleMap.nodesArr)
                .on("tick", ticked);

            simulation.force("link")
                .links(links);




            function ticked() {

                path
                    .attr("x1", function (d, i) {
                        if (d === undefined) {
                            //	console.log("@@@@@@@@@@@@@"+i);
                            return 0;
                        }
                        return d.source.x;
                    })
                    .attr("y1", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.source.y;
                    })
                    .attr("x2", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.target.x;
                    })
                    .attr("y2", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.target.y;
                    });

                circle
                    .attr("x", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.x;
                    })
                    .attr("y", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.y;
                    });

                circleDraw
                    .attr("cx", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.x;
                    })
                    .attr("cy", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.y;
                    });
                text
                    .attr("x", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.x;
                    })
                    .attr("y", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.y;
                    });
                textL
                    .attr("x", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return (d.source.x + d.target.x)/2;
                    })
                    .attr("y", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return (d.source.y + d.target.y)/2;
                    });

            }

            function newTick() {

                path
                    .attr("x1", function (d, i) {
                        if (d === undefined) {
                            //	console.log("@@@@@@@@@@@@@"+i);
                            return 0;
                        }
                        return d.source.fx;
                    })
                    .attr("y1", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.source.fy;
                    })
                    .attr("x2", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.target.fx;
                    })
                    .attr("y2", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        return d.target.fy;
                    });

                circle
                    .attr("x", function (d, i) {

                        if (i === 2) {
                            //    console.log("!!!!!!!!!!!!!!!!!!!!!!!" + d.fx + "!!!!" + d.x)
                        }
                        if (d === undefined) {
                            return 0;
                        }
                        d.fx = d.x;
                        return d.fx;
                    })
                    .attr("y", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        d.fy = d.y;
                        return d.fy;
                    });

                circleDraw
                    .attr("cx", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        d.fx = d.x;
                        return d.fx;
                    })
                    .attr("cy", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        d.fy = d.y;

                        return d.fy;
                    });
                text
                    .attr("x", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        d.fx = d.x;

                        return d.x;
                    })
                    .attr("y", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        d.fy = d.y;

                        return d.y;
                    });

                textL
                    .attr("x", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        d.fx = (d.source.x + d.target.x)/2;
                        return (d.source.x + d.target.x)/2;
                    })
                    .attr("y", function (d) {
                        if (d === undefined) {
                            return 0;
                        }
                        d.fy = (d.source.y + d.target.y)/2;

                        return (d.source.y + d.target.y)/2;
                    });

            }

            function dragstarted(d) {
                if (!d3.event.active) simulation.alphaTarget(0.3).restart();
                console.log("DRag")
                d.fx = d.x;
                d.fy = d.y;
            }

            function dragged(d) {
                d.fx = d3.event.x;
                d.fy = d3.event.y;

                d.x = d3.event.x;
                d.y = d3.event.y;
            }

            function dragended(d) {
                if (!d3.event.active) simulation.alphaTarget(0);
                //d.fx = null;
                //d.fy = null;
            }

            function handleMouseOver(nodeCircle, dCircle) {
                //node name : d.name
                gP.selectAll("line")
                    .style("stroke", highlightPath);

                nodeCircle.attr("class", "hovered");

                function highlightPath(dLink) {
                    // console.log("source: "+JSON.stringify(dLink.source)+"  target: "+ JSON.stringify(dLink.target) +" node name: "+dCircle.name);
                    if ((dLink.source && dLink.source.name === dCircle.name) || ( dLink.target && dLink.target.name === dCircle.name)) {
                        //     console.log("change color");


                        return "#ffff00";
                    }
                    return "#666";
                }
            }

            function handleMouseOut(nodeCircle, dCircle) {
                //node name : d.name
                // console.log("change back");

                gP.selectAll("line").style("stroke", "#666");
                //    nodeCircle.style("stroke-width","4px");
                // nodeCircle.style("stroke","#666");
                nodeCircle.attr("class", "nodes");


            }

            bubbleMap.defaultOpa();
            /*freeze**/


            setTimeout(function () {
                console.log("Stop moving!")

                simulation.stop();


                simulation
                    .nodes(bubbleMap.nodesArr)
                    .on("tick", newTick);

            }, 1000)
            svg.call(d3.zoom()
                .scaleExtent([1 / 2, 8])
                .on("zoom", zoomed));

        }


        function zoomed() {
            container.attr("transform", d3.event.transform);
            // link.attr("transform", d3.event.transform);

        }

        //TODO: add coordinates to nodes data
        bubbleMap.updateByCoord = function updateByCoord(center, radius) {

            var resultArr = [];
            gN.selectAll("a.cir").select('circle.nodes').attr("opacity", function (d) {
                if (d.coord) {
                    console.log("@@@@@@@@@@@in d3 node dta: " + JSON.stringify(d.coord))
                    let dx = d.coord.x - center.x;
                    let dy = d.coord.y - center.y;
                    let eps = Number.EPSILON;
                    //console.log(radius * radius - dx * dx - dy * dy);
                    if (radius * radius - dx * dx - dy * dy > eps) {
                        resultArr.push({name: d.name, x: d.coord.x, y: d.coord.y});
                        console.log("!!!!!!!!!!" + d.name)
                        return 1;
                    } else {
                        return 0.25;
                    }
                } else {//This node does not have coordinates
                    // console.log(" node: "+d.url+" does not have coordinates");
                    return 0.25;
                }
            });


            //TODO: deal with path

            // deal with label
            gT.selectAll("text.nodeTag").attr("visibility", function (d) {
                if (d.coord) {
                    console.log("@@@@@@@@@@@in d3 node dta: " + JSON.stringify(d.coord))
                    let dx = d.coord.x - center.x;
                    let dy = d.coord.y - center.y;
                    let eps = Number.EPSILON;
                    console.log(radius * radius - dx * dx - dy * dy);
                    return (radius * radius - dx * dx - dy * dy > eps) ? "visible" : "hidden";
                } else {//This node does not have coordinates
                    // console.log(" node: "+d.url+" does not have coordinates");
                    return "hidden";
                }
            });

            return resultArr;
        }

        bubbleMap.highlightNode = function hlNode(node, name) {
            if (!node) {
                gN.selectAll("a.cir").select('circle.nodes').attr("class", function (d) {
                    if (d.name == name) {
                        return "nodes selected"
                    }
                    return "nodes"
                })

                return;
            }

            node.attr("class", function (d) {
                return "nodes selected"
            })
        };

        bubbleMap.unhighlightAll = function () {
            gN.selectAll("a.cir").select('circle.nodes').attr("class", function (d) {
                return "nodes";
            })
        }
        bubbleMap.defaultOpa = function () {
            gN.selectAll("a.cir").select('circle.nodes').attr("opacity", function (d) {
                return 1;
            })
        };

        var links = data.connections;
        var coords = data.geoCoords;
        var serviceUrls = data.serviceUrls;
        bubbleMap.update(links, coords, serviceUrls);
        return bubbleMap;

    };



    //when button clicked, run simulation
$(document).ready(function () {
    InitialInputs();
    $("#run-btn").click(function () {
        console.log("Start sim")
         HWSimulation();

    });
});


/***
ajax to get initial inputs
***/
function InitialInputs(){
    $.ajax({
        url: window.location.href + "/initial",
        method: "GET",
        //  contentType: "application/json; charset=utf-8",
        success: function (data) {
            console.log(data)
            addInitialInputsDisplay(data);
        },
        error: function () {
            console.log("Can not get location")
        }
    });

}


    const textPanel = $("#text-panel")
    const rePanel = $("#result-panel")
    const drawPanel = $("#draw-panel")

/**
ajax to get simulation result and draw it as a bubble map
***/
function HWSimulation() {
    //todo:need refresh map?
    //first, ajax
    rePanel.empty()
    drawPanel.empty()
    let data = getInputs();
    console.log("inputs:")
    console.log(data)
    $.ajax({
        url: window.location.href + "/simulation",
        method: "POST",
          contentType: "application/json; charset=utf-8",
        data:JSON.stringify({plantData: data}),
        success: function (data) {
            console.log("success!")
            console.log(data)
            hwMap.clearAnimatedLines()
            hwMap.drawAnimatedLines(data.modifyList);
     let packed = packList(data.modifyList);
             console.log(data.flowList)
              console.log("packed list!")
             console.log(packed)

            addTextDisplay(data);
     var linkmap   = FileLinkMap({connections : packed}, {width: 500, height:500});//draw result as a filelinkmap
        },
        error: function () {
            console.log("Can not get location")
        }
    });

}


function getInputs() {


    let sourceQuan = [], sinkQuan = [], sourceQual = [], sinkQual = [];
    $("table#quantity tr").each(function () {
        var row = $(this);
        let cols = row.find("input");
        console.log(cols)
        if(cols.length<1){
            return;
        }
        sourceQuan.push($(cols[0]).val())
        sinkQuan.push($(cols[1]).val())

    })

    $("table#quality tr").each(function () {
        var row = $(this);
        let cols = row.find("input");
        console.log(cols)
        if(cols.length<1){
            return;
        }
        sourceQual.push($(cols[0]).val())
        sinkQual.push($(cols[1]).val())

    })

    return sourceQual.concat(sinkQual.concat(sourceQuan.concat(sinkQuan)));
}

function  packList(list) {

    return list.map((item)=>{
        console.log(plants[item[0]-1])
        return {source:plants[item[0]-1], target:plants[item[1]-1], value:item[2]}})
}
//TODO:input window
  //TODO: check networkx, maybe could just use directed graph for the graph






function addTextDisplay(data){
    //console.log(p(textTupleNum(data.tupleNumber)))

    rePanel.append(p(textFlowNum(data.flowNumber)))
    rePanel.append(p(textFlowList(data.flowList)))
    rePanel.append(p(textOpHeading()))

    rePanel.append(p(textFlowList(data.modifyList)))
}

function addInitialInputsDisplay(data){
    textPanel.append(p(textHeading()))
    textPanel.append(table(tableContentPQuan(data.SourcePlantQuantities, data.SinkPlantQuantities), "quantity"))
    textPanel.append(table(tableContentQual(data.SourcePlantQualities, data.SinkPlantQualities), "quality"))
}
function p(text) {
    return `<p>${text}</p>`;
}

function textHeading(){
    return 'After parsing the waste heat recovery network owl file, we find that the waste heat quantity(kW) and quality( °C) are:'
}

function textOpHeading() {
    return "After network optimization, we find that the optimal waste heat recovery network is:"
}


function textFlowNum(flow) {
    return `After transportation network modelling, we find that:\n There are ${flow} total possible waste heat recovery energy flows in the network`;
}
    function li(content) {
        return `<li>${content}</li>`;
    }
    function ul(content){
        return `<ul>${content}</ul>`;
    }


function textFlowList(list){
function template(plant1name, plant2name,value) {
    let T =  `from plant ${plant1name} to ${plant2name}`;
    let valueT = value?`${value} kW waste heat `:"";
    return valueT+T;
}


return ul(list.map((item)=>li(template(getPlantName(item[0] - 1), getPlantName(item[1]-1), item.length>2?item[2]:null))).join(''))

}

function getPlantName(id) {
    return plants[id];

}

function tableContentPQuan(p1quantities, p2quantities){

    return [["","Source quantity(kw)","Sink quantity(kw)"]]
        .concat(p1quantities.map((value, i)=> [value, p2quantities[i]]));
}

function tableContentQual(p1qualities, p2qualities){
    return [["","Source quality(°C)","Sink quality(°C)"]]
        .concat(p1qualities.map((value, i)=> [value, p2qualities[i]]));
}

function table(lists, id){
    let tableStr = "<table id='"+id+"'>";
    let headerRowD = lists.splice(0,1)[0];
    let headerRow = "";
    console.log(headerRowD)
    headerRowD.forEach((cell => {headerRow+=th(cell)}));

    tableStr+=tr(headerRow)

    lists.forEach((row, idx)=>{
        let rowStr ="";
        rowStr+= td(plants[idx]);
        row.forEach((cell => {rowStr+=td(txtinput(cell))}));
        tableStr+=tr(rowStr)
    })

    tableStr+="</table>";
    console.log(tableStr)
    return tableStr;
    function tr(content){
        return `<tr>${content}</tr>`;

    }
    function td(content){
        return `<td>${content}</td>`;
    }

    function th(content){
        return `<th>${content}</th>`;
    }
    function txtinput(content, id){
        return `<input id='${id}' type='text' value='${content}'>`;
    }


}















//for each in modify list, draw a line with animation








