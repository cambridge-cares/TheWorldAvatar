// http://blog.thomsonreuters.com/index.php/mobile-patent-suits-graphic-of-the-day/

var socket = io();

let resultArr;
var shapeSaved = {};

let defaultLocation = location;

function getIdFromNameInResults(name) {
    if (!resultArr || resultArr.length < 1) {
        return null;
    }
    
    for (let idx = 0; idx < resultArr.length; idx++) {
        if (resultArr[idx].name == name) {
            return idx;
        }
    }
}
/*constructor: d3 link graph***********************************************/
var FileLinkMap = function (options) {
    var width = $(document).width(),
        height = $(document).height() > 2000 ? $(document).height() : 2000,
        charge = options.charge || -500,
        distance = options.distance || 50,
        nodeR = options.nodeR || 15,
        textSize = options.textSize || 5;
    
    var colorList2 = [
        
        
        "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C","#922329" ];
    var colorMap = {};
    var mapSize = 0;
    
    var bubbleMap = {};
    bubbleMap.nodesArr = [];
    var maxLevel = 0;

    /**
    pack connection into node objects
    @params:
    links: [{target, source,level},]
    ***/
    function packNodesArr(links, coords, serviceUrls) {
        var nodes = {};
        var nodesArr = [];
        
        
        function getDomain(str) {
            if (!str) {
                return null;
            }
            
            str = str.replace("theworldavatar", "jparksimulator");
            str = str.replace("C:\\TOMCAT\\webapps\\ROOT\\", "http://www.jparksimulator.com/");
            var arr = str.split("/");
            
            
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
            if(url.name){
                return url.name
            }
            if (!url) {
                return "";
            }
            var arr = url.split("/");
            
            //  return (arr[arr.length - 1] === "")? arr[arr.length-3]+'/'+arr[arr.length-2] : arr[arr.length-2]+"/"+arr[arr.length -1];
            if (arr.length < 2) {
                console.log("undefined name :" + url)
                return "";
            }
            return (arr[arr.length - 1] === "") ? arr[arr.length - 2] : arr[arr.length - 1];
            
            return url;
        }
        
        /*search for coordinates in coordinate array by uri*/
        function getCoord(uri) {
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
        function getServiceUrl(uri) {
            //   console.log("search service url for "+uri)
            
            if (!serviceUrls || serviceUrls.length < 1) {
                console.log("Did not find any service urls")
                return null;
            }
            for (let i = 0; i < serviceUrls.length; i++) {
                let serviceUrl = serviceUrls[i];
                //console.log("comparing :"+ serviceUrl.url)
                if (serviceUrl && serviceUrl.url == uri) {
                    //   console.log("!!!!!!!!!! found service Url node for "+serviceUrl )
                    return serviceUrl.serviceUrl;
                }
            }
            
            return null;
        }
        
        // Compute the distinct nodes from the links.
        links.forEach(function (link) {
            
          //   if (nodes[link.source] != null) {
           //     nodes[link.source].count = nodes[link.source].count + 1;
           //     console.log(link.source + "++");
           // }
           //  if (nodes[link.target] != null) {
          //      nodes[link.target].count = nodes[link.target].count + 1;
             //   console.log("++");

           // }
            
            if (link.source) {
                link.source = nodes[link.source] || (nodes[link.source] = {
                        url: link.source,
            
                        name: getSimpleName(link.source),
                        domain: getDomain(link.source),
                        clustersize: link.source in bubbleMap.subconMap ? bubbleMap.subconMap[link.source].connections.length : null,
                        coord: getCoord(link.source),
                        serviceUrl: getServiceUrl(link.source)
            
                    });
            }
            
            var thisLevel = parseInt(link.level) + 1;
            if(maxLevel < thisLevel){
                maxLevel = thisLevel;
            }
            if (nodes[link.target]) {//if target already exists in node list, then increase level by
                nodes[link.target].level = thisLevel;
            }
            if(link.target) {
                link.target = nodes[link.target] || (nodes[link.target] = {
                        url: link.target,
                        name: getSimpleName(link.target),
                        domain: getDomain(link.target),
                        clustersize: link.target in bubbleMap.subconMap ?bubbleMap.subconMap[link.target].connections.length : null,
                        level: thisLevel
                        ,                //add to node attri: geo coordinates
                        coord: getCoord(link.target),
                        serviceUrl: getServiceUrl(link.target)
            
                    });
            }
            
        });
        
        let index = 0;
        //   console.log("@@@@@@@@@@@@@@@@@@@@@@");
        for (let link of links) {
            
            //   console.log(index + "  :" + JSON.stringify(link));
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
                // console.log("packed node: " + JSON.stringify(nodes[URI]))
                //console.log("count" + nodes[attr].count);
            }
        }
        return nodesArr;
    }
    
    function setBodyS(node) {
        
        return charge;
    }
    
    function setD(link) {
        // console.log(link.source.count);
        var nodeNThre = 5
        if (link.source.count > nodeNThre || link.target.count > nodeNThre) {
            return 200;
        }
        return 1;
    }
    
    /**
     * Allocate color for nodes of each domain
     * @param d  datum
     * @returns one color defined in colormap
     */
    function allocateColor(d) {
        // is this exist in color map?
        
        if (d.level !== undefined && d.level !== null && !isNaN(d.level)) {
            
            return colorList2[d.level+1];
        }
        
        
        // colorMap[d.domain] = colorMap[d.domain] || (colorList[mapSize++]);
        
        if (mapSize >= mapSize.length) {
            mapSize = 0;
            console.log("WARNING: DOMAIN NUMBER EXISTS COLOR MAP NUMBER!");
        }
        
        return colorList2[0];
    }
    
    function sortOrder(d, i) {
        
        if (d.level !== undefined && d.level !== null && !isNaN(d.level)) {
            
            return d.level * 1000 + i;
        }
        
        return 0;
    }
    
    function defineLegend(d) {
        if (d.level !== undefined && d.level !== null && !isNaN(d.level)) {
            
            return d.level;
        }
        
        return 0;
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
    
    var simulation;
    var circle = gN.selectAll("a.cir");
    var path = gP.selectAll("line");
    var text = gT.selectAll("text.nodeTag");
    var circleDraw
    var newPath, newCircle, newText
    bubbleMap.nodesArr = []
    bubbleMap.links = []
    bubbleMap.selected = null;
    var filteredNodes = [], filteredLinks = []
    let subscribeList = []
    
    

    /***
    function to draw d3 bubble gragph
    @params:
    dnodes node objects
    dlinks link objects
    retainSim boolean if retain previous simulation
    ***/
    bubbleMap.drawBubbles = function (dnodes, dlinks, retainSim) {
        //node bubbles
    
        if (!retainSim) {
            
             console.log("not retaining previous sim")
         
             simulation = d3.forceSimulation()
             .force("link", d3.forceLink().strength(setD).id(function (d) {
                    return d.id;
                }))
             .force("charge", d3.forceManyBody().strength(setBodyS))
             .force("center", d3.forceCenter(width / 2, height / 2))
             .on("tick", ticked);
             
        }
        let timer = 0;
        let sglclickPrevent = false;
    
        let circleUp = circle.data(dnodes)//: FILTERed
    
    
        console.log(circleUp.exit().select("circle"))
        circleUp.exit().selectAll("circle").remove();//REMOVE CHILDREN
        circleUp.exit().remove();
    
        newCircle = circle = circleUp.enter().append("a")
            .attr('class', 'cir')
            //.append("a")
            .attr("xlink:href", function (d) {
                //   console.log("@@@@@@@@@@@@draw node: " + d.name);
                return d.url;
            })
            .on("click", function (d) {//update this infor in
                d3.event.preventDefault();
                d3.event.stopPropagation();
                let d3node = d3.select(this).select('circle');
            
                if(bubbleMap.selected && bubbleMap.selected.node()!=d3node.node() || bubbleMap.selected ===null) {//not same one selected
                    d3node.attr("class", "selected")
                    if (bubbleMap.selected)
                        bubbleMap.selected.attr("class","nodes")
                    bubbleMap.selected = d3node
                } else{//same node selected
                    bubbleMap.selected.attr("class","nodes")
                    bubbleMap.selected = null
                
                }
                console.log(bubbleMap.selected)
            
                timer = setTimeout(function () {
                    if (!sglclickPrevent) {
                        clickAction();
                    }
                
                    sglclickPrevent = false;
                }, 200);
            
                function clickAction() {
                    if (d.serviceUrl && d.serviceUrl !== "") {
                        //    console.log(d.serviceUrl);
                        window.open(d.serviceUrl);
                    }
                
                    //  console.log(d3.select(this));
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
            
            });
    
    
        circle.append("circle")
            .attr("id", function (d) {
                //   console.log("!!!!!!!!!!!!!!")
                //    console.log(d);
                return d.name;
            })
            .attr("r", function(d){
                console.log(d.clustersize)
                return d.clustersize? (7+Math.log10(d.clustersize+1) * nodeR ): nodeR
                }
            )
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
    
    
        circle = circle.merge(circleUp)
    
        circleDraw = circle.select("circle")
    
        path = path //linking lines
            .data(dlinks
                //function (d) {
                //return d.source.domain + d.source.name + d.target.domain + d.target.name;}
            );
    
        path.exit().remove();
    
    
        newPath = path.enter().append("line")
            .attr("class", function (d) {
                // console.log("@@@@@@@@@@@@draw link : " + d.source.name + '--------' + d.target.name);
                return "link " + "licensing";
            })
        path = newPath.merge(path);
    
        //   console.log(newPath)
    
    
        text = text //node tags
            .data(dnodes, function (d) {
                return d.domain + d.name;
            });
        text.exit().remove();
    
    
        newText = text.enter().append("text")
            .attr("class", "nodeTag")
            .attr("x", textSize)
            .attr("y", "0.31em")
            .text(function (d) {
                return d.name;
            })
        text = newText.merge(text)
    
    
        simulation
            .nodes(dnodes)
    
    
        simulation.force("link").links(dlinks)
    
    
        simulation.alphaTarget(0.3).restart();
    
        var preLengend = svg.selectAll("g.legend").remove();
    
        var legend = svg.append("g")
            .attr("class", "legend")
            .attr("transform", "translate(50,30)")
            .style("font-size", "12px")
            .call(d3.legend);
    
    
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
                    let el = d3.select(this.parentNode)
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
        
        
        }
    
        function dragended(d) {
            if (!d3.event.active) simulation.alphaTarget(0);
            d.fx = null;
            d.fy = null;
        }
    
        function handleMouseOver(nodeCircle, dCircle) {
            //node name : d.name
            gP.selectAll("line")
                .style("stroke", highlightPath);
            if(nodeCircle.attr("class")!== "selected")
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
            if(nodeCircle.attr("class")!== "selected"){
            
                nodeCircle.attr("class", "nodes");}
        
        
        }
    
        bubbleMap.defaultOpa();
    
    
        svg.call(d3.zoom()
            .scaleExtent([1 / 2, 8])
            .on("zoom", zoomed));
        
    }
    

    bubbleMap.showHidden = function (newnodes, newlinks) {

        bubbleMap.drawBubbles(bubbleMap.nodesArr.concat(newnodes), bubbleMap.links.concat(newlinks), true)
     // console.log(filteredNodes.concat(newnodes).length)
    }
    
    /***
    update d3 directed graph with new data
    ***/
	bubbleMap.subscribe = function(){
		if(subscribeList.length > 0){
			console.log('resubscribe, list length: '+ subscribeList.length)
        socket.emit("join", JSON.stringify(subscribeList));
		}
	}
    bubbleMap.update = function (links, coords, serviceUrls, retainSim) {
        coords = coords || []
        serviceUrls = serviceUrls || []
        console.log(typeof link)
        links = deepcopyObjArr(links)
        let newNodes = packNodesArr(links, [], []);
        
        for (let link of links) {
            if (!includeLink(bubbleMap.links, link) && link.source && link.target) {
                console.log("new links: ")
                console.log(link)
                let existTarget = searchNodes(link.target.url)
                let existSource = searchNodes(link.source.url)
                link["target"] = existTarget ? existTarget : link["target"]//if target node exists, replace it with existingone
                link["source"] = existSource ? existSource : link["source"]
                bubbleMap.links.push(link)
            }
        }
        
        function searchNodes(url) {
            for (let node of bubbleMap.nodesArr) {
                if (node.url === url) {
                    return node
                }
            }
            
            return null
        }
        
        
        function deleteFromArr(arr, idx2Del) {
            idx2Del = idx2Del.sort().reverse()
            for (let idx of idx2Del) {
                arr.splice(idx, 1)
            }
        }
        
        
        
        function filterlevel2nodes(nodes, links) {
            let oriLink = Object.assign([], links);
            let oriNodes = Object.assign([], nodes);
            for (let node of oriNodes) {
                if (node.level === undefined || node.level< 2) {
                    filteredNodes.push(node)
                }
                
            }
            for (let link of oriLink) {
                if ((link["target"].level === undefined || link["target"].level < 2 )&& (link["source"].level === undefined ||link["source"].level < 1)) {
                    filteredLinks.push(link)
                }
            }
        }
        
        
        //delete update links
        let idx2Del = []
        for (let idx = 0; idx < bubbleMap.links.length; idx++) {
            let oriLink = bubbleMap.links[idx];
            if (!includeLink(links, oriLink)) {
                //note down this idx
                idx2Del.push(idx);
            }
        }
        
        deleteFromArr(bubbleMap.links, idx2Del);
        
        
        //delete update nodes
        for (let node of newNodes) {
            if (!includeobj(bubbleMap.nodesArr, node, ['url'])) {
                bubbleMap.nodesArr.push(node)
            } 
        }
        let idx2DelNodes = []
        for (let idx = 0; idx < bubbleMap.nodesArr.length; idx++) {
            let oriNode = bubbleMap.nodesArr[idx];
            if (!includeobj(newNodes, oriNode, ['url'])) {
                //note down this idx
                idx2DelNodes.push(idx)
            }
        }
        console.log(idx2DelNodes)
        deleteFromArr(bubbleMap.nodesArr, idx2DelNodes)
        console.log(bubbleMap.nodesArr)
        //get nodes to shown

            //filterlevel2nodes(bubbleMap.nodesArr, bubbleMap.links);
        
        
        
        function includeLink(arr, obj) {
            for (let item of arr) {
                if (obj.source.url === item.source.url && obj.target.url === item.target.url) {
                    return true
                }
            }
            return false
        }
        
        function includeobj(arr, obj, idnames) {
            for (let item of arr) {
                let allTrue = idnames.length, aggregateTrue = 0
                for (let attrname of idnames) {
                    aggregateTrue += (item[attrname] === obj[attrname])
                }
                if (aggregateTrue === allTrue) {
                    return true;
                }
            }
            return false;
        }
        
        //console.log(bubbleMap.links)

        /*subscribe**********/
        subscribeList = bubbleMap.nodesArr.map(function (node) {
            return {uri: node.url, withData: false};
        });
        
        console.log(subscribeList);
	     
        socket.emit("join", JSON.stringify(subscribeList));
        /******************/
        
        
        //set force simulationf

        
       bubbleMap.drawBubbles(bubbleMap.nodesArr, bubbleMap.links, retainSim)
        
    }
    
    
    bubbleMap.addnew = function (newlinks) {
        bubbleMap.update(newlinks, [], [], true)
        
    }
    
    /**
    zoom handler
    ***/
    function zoomed() {
        if(bubbleMap.selected){//special case: zoom nodes
            var url = bubbleMap.selected._groups[0][0].__data__.url
            console.log("url:" +url)
            var newlinks = bubbleMap.expandCluster(url)
            console.log("expand")
            console.log(newlinks)
            if(!newlinks){return}
           map.update(newlinks, null, null, true)
         
          // bubbleMap.showHidden(...newlinks)
            return
        }
        
        container.attr("transform", d3.event.transform);
        // link.attr("transform", d3.event.transform);
        
    }
    
    //add coordinates to nodes data
    bubbleMap.updateByCoord = function updateByCoord(center, radius) {
        
        var resultArr = [];
        gN.selectAll("a.cir").select('circle.nodes').attr("opacity", function (d) {
            if (d.coord) {
                //    console.log("@@@@@@@@@@@in d3 node dta: " + JSON.stringify(d.coord))
                let dx = d.coord.x - center.x;
                let dy = d.coord.y - center.y;
                let eps = Number.EPSILON;
                //console.log(radius * radius - dx * dx - dy * dy);
                if (radius * radius - dx * dx - dy * dy > eps) {
                    resultArr.push({name: d.name, x: d.coord.x, y: d.coord.y});
                    //     console.log("!!!!!!!!!!" + d.name)
                    return 1;
                } else {
                    return 0.25;
                }
            } else {//This node does not have coordinates
                // console.log(" node: "+d.url+" does not have coordinates");
                return 0.25;
            }
        });
        
        
        
        // deal with label
        gT.selectAll("text.nodeTag").attr("visibility", function (d) {
            if (d.coord) {
                //console.log("@@@@@@@@@@@in d3 node dta: " + JSON.stringify(d.coord))
                let dx = d.coord.x - center.x;
                let dy = d.coord.y - center.y;
                let eps = Number.EPSILON;
                //console.log(radius * radius - dx * dx - dy * dy);
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
    

    
    bubbleMap.loadData = function (url) {
        $.ajax({ //ajax to get links again
            url: url + '/links',
            type: 'GET',
        
            statusCode: {
                200: function (data) {
                    console.log(data)
                    console.log(typeof(data.connections))
                    bubbleMap.initLinks = deepcopyObjArr(data.connections);
                    //console.log(data.connections)
                    //console.log(data.subconnections)
    
                    
                    bubbleMap.subconMap = data.subconnections;
                    let topurl =data.connections[0].source
                    bubbleMap.subconMap[topurl] = {connections:data.connections}
                    bubbleMap.update(
                        [{source: data.connections[0].source, target:null}] , [], [], false, true);
                }
            },
            error: function (err) {
                console.log(err);
            }
        });
    }
    
    /***
    expand a clustered node
    @params:
    url: url of the clustered nodes
    ***/
        bubbleMap.expandCluster = function (url) {
            if(!bubbleMap.subconMap[url]) return null;
       // console.log(bubbleMap.subconMap[url].connections)
        //console.log(bubbleMap.initLinks)
		            bubbleMap.initLinks =  bubbleMap.initLinks.concat(bubbleMap.subconMap[url].connections)


        return bubbleMap.initLinks;
    
    };

/**
expand all clustered nodes
***/
    bubbleMap.expandAll = function(){
            for(let url in bubbleMap.subconMap )     {
          bubbleMap.initLinks =  bubbleMap.initLinks.concat(bubbleMap.subconMap[url].connections)
            }   
    return bubbleMap.initLinks;
    }
    //this is not used, but kept for reference purpose
    bubbleMap.noClusterExpand= function(){
       // bubbleMap.update(bubbleMap.initLinks, [], [], true);
    
    }

    return bubbleMap;
    
};

function deepcopyObjArr(arr) {
return JSON.parse(JSON.stringify(arr));
}

function combineArrMap(obja, objb){

    let copya = Object.assign({}, obja);
    for (let attr in objb){
        if(attr && (objb[attr].connections instanceof Array)) {
        if(attr in copya){
            if(attr && copya[attr].connections instanceof Array  ) {
                let union = new Set(copya[attr].connections.concat(objb[attr].connections));
                copya[attr].connections = [...union];
            }
        } else {
            copya[attr] = {connections:[]};
           objb[attr].connections.forEach((item)=>{copya[attr].connections.push(item)})
        }
    }
    }
    return copya
}

/*END constructor: d3 link graph*********************************************/



/*DOM LOGIC**************************************************************/

var map;

$(window).load(function () {// when web dom ready
    /*init d3 map*/
    let url = window.location.href;     // Returns full URL
    
    map = FileLinkMap({});
    map.loadData(url)
	socket.on('connect', function(){
		console.log('reconnect socket, try subscribe again')
		map.subscribe();
	})
    $("#geo-search-result-panel").hide()
    
    
    let btn = $("#menu-toggle");
    
    btn.click(function (e) {
        console.log("btn clicked")
        e.preventDefault();
        e.stopImmediatePropagation();
        $("#sidebar-wrapper").toggleClass("toggled");
        /*settings menu*****/
        $('#sidebar-wrapper').on('clickout', function (e) {
            console.log('!!!!!!!!!!!!!!!Outside element click')
            $("#sidebar-wrapper").toggleClass("toggled");
            $("#sidebar-wrapper").off("clickout");
            
        })
        
    });
    
    
    
    /*choice panel*************************************************/
    /*button : show Import **/
    $("#checkShowImport").change(function () {
        if ($('#checkShowImport').prop('checked')) {
            disableThenEnableAfterTimeout();
            
            $.ajax({
                url: url + '/includeImport',
                type: 'GET',
                
                statusCode: {
                    200: function (data) {
                        let links = data.connections;
                        let coords = data.geoCoords;
                        console.log('ajax successful!\n');
                        
                        
                        console.log(JSON.stringify(links));
                        if (LinkRightFormat(links)) {
                            
                            for (let link of links) {
                                if (link === undefined || link === null) {
                                    console.log("!!!!!!!!!!");
                                    
                                }
                                
                            }
                            map.update(links, coords, [],true);
                            //concate subconnection
                                                        console.log(data.subconnections)
                           map.subconMap = combineArrMap(map.subconMap,data.subconnections);
                        console.log(map.subconMap);               
                        map.expandAll()             
                        }
                        
                    }
                },
                error: function (err) {
                    console.log(err);
                    
                    
                }
            });
            
        } else {
            window.location.href = defaultLocation;
        }
        
        
    });
    /*button : show Service Only **/
    $("#checkShowServiceOnly").change(function () {
        if ($('#checkShowServiceOnly').prop('checked')) {
            disableThenEnableAfterTimeout();
            $.ajax({
                url: url + '/showServiceOnly',
                type: 'GET',
                
                statusCode: {
                    200: function (data) {
                        
                        let links = data.connections;
                        let coords = data.geoCoords;
                        let serviceUrls = data.serviceUrls
                        
                        console.log('ajax successful!\n');
                        
                        
                        //     console.log(JSON.stringify(links));
                        if (LinkRightFormat(links)) {
                            clearSelectBar();
                            map.update(links, coords, serviceUrls);
                            
                            
                        }
                        
                    }
                },
                error: function (err) {
                    console.log(err);
                    
                    
                }
            });
            
        } else {
            window.location.href = defaultLocation;
        }
    })
    /*button : show Default **/
    $("#checkdefault").change(function () {
        if ($('#checkdefault').prop('checked')) {
            
            window.location.href = defaultLocation; //return to default
            
            //use this for a test
            //var data = JSON.parse($("#data").val());
            // var newlinks = data.connections;
            
            // newlinks.push({source:"http://www.theworldavatar.com/EBus-203.owl", target:"http://www.theworldavatar.com/test"})
            
            //  newlinks.push({source:"Myriel", target:"test"})
            //  map.update(newlinks, [], [], true)
            
        }
    });
    /*check box back to default when unload*/
    $(window).unload(function () {
        console.log("unload");
        
        if ($("#checkShowImport").prop('checked')) {
            $("#checkShowImport").prop('checked', false);
        }
        if ($("#checkShowServiceOnly").prop('checked')) {
            $("#checkShowServiceOnly").prop('checked', false);
        }
    });
    /*utility functions*********/
    /**
     * utility function, check if link is of correct format
     * @param links
     * @returns {boolean}
     * @constructor
     */
    function LinkRightFormat(links) {
        if (!links || links.length < 1) {
            return false;
        }
        for (let link of links) {
            if (!link.hasOwnProperty("target") || !link.hasOwnProperty("source")) {
                return false;
            }
            
        }
        return true;
    }
    
    /**
     * Utility function: disable option buttons
     * @param disable
     * @returns {boolean}
     * @constructor
     */
    function DisableOptionButton(disable) {
        //  console.log(disable);
        
        if ($("#checkShowImport").prop("disabled") === !disable && $("#checkShowServiceOnly").prop("disabled") === !disable) {
            $("#checkShowImport").prop("disabled", disable);
            $("#checkShowServiceOnly").prop("disabled", disable);
            console.log("now :" + (disable === true ? "disable" : "enable"));
            return true;
        }
        return false;
    }
    
    /**
     * utility function : disable all buttons then enable all after timeout
     */
    function disableThenEnableAfterTimeout() {
        if (DisableOptionButton(true)) {
            
            setTimeout(function () {
                DisableOptionButton(false);
            }, 5000);
        }
        
    }
    
    /*END  choice panel***************************************************/
    
    /*GEO Search Panel***************************************************************/
    
    function updateSelectBar() {
        //initiate select with data
        clearSelectBar();
        map.nodesArr.forEach(function (item) {    // for each in data list
            if (item.coord) {        //only do this if has a coord'
                $("#device-select").append("<option value='" + item.coord.x + "/" + item.coord.y + "'>" + item.name + "</option>");
            }
        });
    }
    
    function clearSelectBar() {
        $("#device-select").html("");
    }
    
    updateSelectBar();
    
    

    
    //when choose from select, update coord to x,y input
    $('#device-select').on('change', function () {
        // alert( this.value );
        let coordStr = $("select#device-select option:selected").val();
        let coordArr = coordStr.split("/");
        $("#search-center-x").val(coordArr[0]);
        $("#search-center-y").val(coordArr[1]);
    });
    
    
    //when user clicked submit
    $("#search-submit").click(function () {
        //retreive search center and radius
        let x = parseFloat($("#search-center-x").val());
        let y = parseFloat($("#search-center-y").val());
        
        // check validity, display err msg
        let radius = parseFloat($("#search-radius").val());
        if (x === null || y === null || radius === null || isNaN(x) || isNaN(y) || isNaN(radius)) {
            displayMsg("Input parameters are not number", "danger");
            return;
        }
        /**
         //truncate radius to 7 digits, show warning
         if(decimalPlaces(x) > 7){
            x= x.toFixed(7);
            $("#search-center-x").val(x);
            displayMsg("Numbers beyond 7 decimal places will be truncated");
        }
         if(decimalPlaces(y) > 7){
            y= x.toFixed(7);
            $("#search-center-y").val(y);
            displayMsg("Numbers beyond 7 decimal places will be truncated");
        }
         if(decimalPlaces(y) > 7){
            radius = radius.toFixed(7);
            $("#search-center-y").val(radius);
            displayMsg("Numbers beyond 7 decimal places will be truncated");
        }
         **/
        
        console.log("CX:" + x + " Y:" + y + "  R:" + radius);
        //pack x, y into object
        //pack x, y into object
        //pack x, y into object
        //pack x, y into object
        //pack x, y into object
        resultArr = map.updateByCoord({x, y}, radius);
        
        console.log(resultArr.toString())
        
        //clear everything on map
        deleteAllShapes();
        let resultPanel = $("#geo-search-result-panel");
        
        
        //check if result panel is show
        if (resultPanel.css('display').toLowerCase() == 'none') {
            resultPanel.css({"display": "block"});//show the result panel
            
        }
        
        //scroll down to result panel
        
        let mwindow = $(window)
        mwindow.scrollTop(
            resultPanel.offset().top
        );
        //clear previous results, if any
        $("#result-table-wrapper").html("");
        // /append a result table
        var template = "<table>";
        
        resultArr.forEach(function (item) {
            let noAffixName = item.name.split('.')[0];
            console.log("@@@@@@@@@@@@@" + noAffixName)
            template += "<tr id='tr" + noAffixName + "'><td>" + item.name + "</td></tr>";
            //create this shape on map
            placeShape(new google.maps.LatLng(item.y, item.x), extractTypeInName(item.name), item.name);
            
        });
        template += "</table>";
        $("#result-table-wrapper").append(template);
        //add listeners
        resultArr.forEach(function (item) {
            let noAffixName = item.name.split('.')[0];
            console.log("@@@@@@@@@@@@@" + noAffixName);
            //add listener
            $("tr#tr" + noAffixName).click(function () {
                unHighLightAll();
                hightLightResult(item.name);
            })
            
        });
        
        
        displayMsg("Search for device near center point :(" + x + " ," + y + ") with radius : " + radius, "success");
        
        //Draw all devices on map
        
        function extractTypeInName(name) {
            var arr = name.split("-");
            if (arr.length < 2) {
                console.log("not correct device name")
                return null;
            }
            return arr[0];
        }
        
        
        function decimalPlaces(num) {
            var match = ('' + num).match(/(?:\.(\d+))?(?:[eE]([+-]?\d+))?$/);
            if (!match) {
                return 0;
            }
            return Math.max(
                0,
                // Number of digits right of decimal point.
                (match[1] ? match[1].length : 0)
                // Adjust for scientific notation.
                - (match[2] ? +match[2] : 0));
        }
        
    });
    
    /*END GEO Search Panel***************************************************************/
    
    /*Err Msg Bar************************************************************************/
    var template = function (msg, type) {
        
        return "<p class='alert alert-" + type + "'>" + msg + "</p>";
    };
    
    /**
     *
     * @param msg
     * @param type  [success/info/warning/danger]
     */
    function displayMsg(msg, type) {
        
        $("#err-msg-panel").html("");
        $("#err-msg-panel").append(template(msg, type));
        
    }
    
    
    /*End Err Msg Bar*******************************************************************/
    
    /*socket**************************************************************/
    //blink any updated data
    let blinkTimerList = {};
    let preTime = Date.now();
    let rerequestTimer, allowRe = true;
    socket.on('update', function (data) {
        //console.log("Socket event!!!!!!!!!!!")
        //console.log(data)
        //check if time less than 1s, if true, do not do anything
        // if (Date.now() - preTime > 1000) {
        preTime = Date.now();
        //  let parsedData = JSON.parse(data);
        let name = data.uri;
        console.log(name)
        //search name among nodes to grab the correct one
        // console.log($('circle'));
        // console.log($('#FH-01.owl'));
        // console.log($("circle[id='FH-01.owl']"));
        let simpleNameArr = data.filename.split('\\');
        
        let simpleName = simpleNameArr.length > 1 ? simpleNameArr[simpleNameArr.length - 1] : data.filename;
        let node = $("circle[id='" + simpleName + "']");
        
        let oriColor = node.css("fill");
        
        let orSize = node.attr("r");
        //console.log($('circle#FH-01.owl'));
        //console.log(data.filename)
        
        if (simpleName in blinkTimerList) {
            return;
        }
        
        node.animate({'r': 20}, 500)
            .css({'fill': '#FFFC94', 'transition': 'fill 0.5s'})
            .css({'stroke': '#FFF700', 'transition': 'stroke 0.5s'});
        ;
        let mTimer = setTimeout(function () {
            node.animate({'r': orSize}, 500)
                .css({'stroke': '#000000', 'transition': 'stroke 0.5s'})
                .css({'fill': oriColor, 'transition': 'fill 0.5s'});
            delete blinkTimerList[simpleName];
        }, 500);//on 0.5s, back to normal
        blinkTimerList[simpleName] = mTimer;
        
        
        // check frequency
        // this will fire for every update!!!!!! What if the update is rather frequent and is related to
        let url = window.location.href;     // Returns full URL
        
        //console.log(data)
        if (allowRe && data.filename == "NuclearPlants\.owl") { //
            console.log("request links again")
            $.ajax({ //ajax to get links again
                url: url + '/links',
                type: 'GET',
                
                statusCode: {
                    200: function (redata) {
                        let links = redata.connections;
                        
                        map.update(links, [], [], true)
                        
                    }
                },
                error: function (err) {
                    console.log(err);
                }
            });
            allowRe = false // disallow re
            if (!rerequestTimer) {//don't regenerate if existing timer
                rerequestTimer = setTimeout(function () {
                    allowRe = true
                    rerequestTimer = null
                }, 2000);//time allowed for a rerequest call
            }
            
        } else {
            console.log("Rereequest not allowed for now. Too frequent")
        }
        
        
        // }
    });
    /*END socket **********************************************************/
    
});
/*END DOM LOGIC**************************************************************/

/*Map************************/
var googleMap;


function initMap() {
    console.log("init map")
    //initiate map, set center on Jurong
    var jurong = {lat: 1.276, lng: 103.677};
    googleMap = new google.maps.Map(document.getElementById('map'), {
        zoom: 14
        , center: jurong
    });
    
}


let deviceMap = (function initDeviceMap() {
    let deviceMap = new Map();
    $.getJSON("JSON/prototypeDevices.json", function (data) {
        for (let device of Object.keys(data)) {
            console.log("loading device: " + device);
            deviceMap.set(device, data[device]);
        }
        
    });
    
    return deviceMap;
    
})();


function placeShape(position, type, name) {
    //  console.log("lng:" + position.lng());
    // console.log("lat:" + position.lat());
    
    let defaultType = "blower";
    if (!deviceMap) {
        console.log("device type Map not set, can not retrieve prototypes");
        return null;
    }
    
    let shape;
    
    if (!type) {
        return null;
    }
    
    shape = deviceMap.get(type);
    console.log("shape:" + shape);
    if (!shape) {//everything not defined in Map
        console.log("prototype device coordinates : " + type + " can not be find, use " + defaultType + " to draw instead");
        //shape = deviceMap.get(defaultType);
        return null;
    }
    
    /*____Construct coordinates from Shape Str_______*/
    
    let coordis = shape.coordinates;
    
    let newCoor = [];
    //for each pair in coordinates, add position
    
    let dLng = position.lng() - coordis[0].lng;
    let dLat = position.lat() - coordis[0].lat;
    
    newCoor.push({lng: position.lng(), lat: position.lat()});
    for (let idx = 1; idx < coordis.length - 1; idx++) {
        
        newCoor.push({lng: coordis[idx].lng + dLng, lat: coordis[idx].lat + dLat});
    }
    
    let pl = createPolygon(newCoor);
    
    pl.addListener('click', function () {//when clicked, hightlight both bubble node and rd row
        unHighLightAll();
        hightLightResult(name);
    });
    
    shapeSaved[name] = pl;
    
}


function hightLightResult(name, d3node) {
    //hightlight select
    //hightlight this shape
    let noaffixname = name.split('.')[0];
    
    if (shapeSaved[name]) {
        shapeSaved[name].setOptions({fillOpacity: 0.8});
        //hightlight corresponding row
        let row = $("#tr" + noaffixname);
        
        if (row) {
            console.log(row);
            row.addClass("selected");
        }
        console.log("!!!!!!!!!!!!!now hightlight " + name);
        //:hightlight bubble
        
        map.highlightNode(d3node, name);
    }
    
}


function unHighLightAll() {
    //unhightlight all pre
    for (let shape in shapeSaved) {
        if (shapeSaved.hasOwnProperty(shape)) {
            
            shapeSaved[shape].setOptions({fillOpacity: 0.35});
            let noaffixname = shape.split('.')[0];
            
            $("tr#tr" + noaffixname).removeClass("selected");
            
        }
    }
    
    map.unhighlightAll();
}

function deleteAllShapes() {
    
    setMapOnAllShape(null);
    shapeSaved = {};
}

function createPolygon(coords) {
    let poly = new google.maps.Polygon({
        paths: coords,
        strokeColor: '#FF0000',
        strokeOpacity: 0.8,
        strokeWeight: 3,
        fillColor: '#FF0000',
        fillOpacity: 0.35,
    });
    
    poly.setMap(googleMap);
    return poly;
}

function setMapOnAllShape(map) {
    
    for (let shape in shapeSaved) {
        if (shapeSaved.hasOwnProperty(shape)) {
            shapeSaved[shape].setMap(map);
            
        }
    }
}

/*END MAP****************/
//module.exports = FileLinkMap;