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
         this.MAXAGENTCONN = options.maxAgentConn || 20,
         this.AGENTTIME= options.agentShowDuration||10000;
    let self = this;
    
    var colorList = [ "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
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
        
        
        "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C","#922329" ];
    var colorMap = {};
    var mapSize = 0;
    
    var bubbleMap = {};
    bubbleMap.nodesArr = [];
    var maxLevel = 0;
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
                        clustersize: link.target in bubbleMap.subconMap ? bubbleMap.subconMap[link.target].connections.length : null,
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
        var nodeNThre = 5;
        if (link.source.count > nodeNThre || link.target.count > nodeNThre) {
            return 100;
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
    var path = gP.selectAll("line.link");
    var text = gT.selectAll("text.nodeTag");
    var circleDraw;
    var newPath, newCircle, newText;
    bubbleMap.nodesArr = []
    bubbleMap.links = []
    bubbleMap.selected = null;
    var filteredNodes = [], filteredLinks = []
    var agentNodes = gN.selectAll("a.agent");
    var agentPath= gP.selectAll("line.agent");
    var agentData = [], agentLinkData = [];
    var agentColorMap = [];
    

    
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
    
        let circleUp = circle.data(dnodes)//TODO: FILTERed
    
    
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
            
                //todo:set selected
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
                return d.clustersize?Math.log10(d.clustersize+1) * nodeR : nodeR
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
            .data(dlinks//todo:filtered
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
               // return d.name;
            })
        text = newText.merge(text)
    
    
        simulation
            .nodes(dnodes)
    
    
        simulation.force("link").links(dlinks)//todo:filtered
    
    
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
    
    //TODO: handling node removal
    bubbleMap.update = function (links, coords, serviceUrls, retainSim) {
        coords = coords || []
        serviceUrls = serviceUrls || []
        console.log(typeof link)
        links = deepcopyObjArr(links)
        let newNodes = packNodesArr(links, [], []);
        console.log(links)
        
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
            //todo:filter out nodes level larger than 2, and all links contain these nodes
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

//TODO: find node list, subscribe change event through sockets
        /*subscribe**********/
        let subscribeList = bubbleMap.nodesArr.map(function (node) {
            return {uri: node.url, withData: false};
        });
    
    
        socket.emit("join", JSON.stringify(subscribeList));
        console.log('sockeet connect')
        //set force simulationf

        
       bubbleMap.drawBubbles(bubbleMap.nodesArr, bubbleMap.links, retainSim)
        
    }
    
    //update and expand cluster
    /******************/
    bubbleMap.findLinks2Node = function (url) {
        
        /**
         *         var childNodes = [], childLinks = [];
 
         for (let link of bubbleMap.links) {
                //console.log(link)
                if (link["source"].url == url) {
                    childNodes.push(link['target']);
                    childLinks.push(link)
                }
            }
         return [childNodes, childLinks]
 
         **/
        
    }
    
    
    bubbleMap.addnew = function (newlinks) {
        bubbleMap.update(newlinks, [], [], true)
        
    }
    
    function zoomed() {
        if(bubbleMap.selected){//special case: zoom nodes
            var url = bubbleMap.selected._groups[0][0].__data__.url
            console.log("url:" +url)
            var newlinks = bubbleMap.expandCluster(url)
            console.log("expand")
            console.log(newlinks)
           map.update(newlinks, null, null, true)
         
          // bubbleMap.showHidden(...newlinks)
            return
        }
        
        container.attr("transform", d3.event.transform);
        // link.attr("transform", d3.event.transform);
        
    }
    
    //TODO: add coordinates to nodes data
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
        
        
        //TODO: deal with path
        
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
    
    let selected = [];
    bubbleMap.highlightNode = function hlNode(node, name) {
        
        if (!node) {
            gN.selectAll("a.cir").select('circle.nodes').attr("class", function (d) {
                if (d.name == name) {
                    console.log('found node to hightlight')
                    return "nodes selected"
                }
               return "nodes"
            })
            
            return;
        }
        
        node.select('circle.nodes').attr("class","nodes selected");
    };
    bubbleMap.unhighlightNode = function hlNode(node, url) {
        if (!node) {
            gN.selectAll("a.cir").filter(function(d) {
                return d.url === url }).select('circle.nodes').attr("class", function (d) {
                return "nodes"
            });
            return;
        }
        
        node.select('circle.nodes').attr("class", function (d) {
            return "nodes"
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
                    console.log(data.connections)
                    console.log(data.subconnections)
                    
                    bubbleMap.subconMap = data.subconnections;
                    let topurl =data.connections[0].source;
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
    
        bubbleMap.expandCluster = function (url) {
        console.log(bubbleMap.subconMap[url].connections)
        console.log(bubbleMap.initLinks);
            bubbleMap.initLinks =  bubbleMap.initLinks.concat(bubbleMap.subconMap[url].connections)
        return bubbleMap.initLinks
    
    };
    bubbleMap.noClusterExpand= function(){
        bubbleMap.update(bubbleMap.initLinks, [], [], true);
    
    };
    
    bubbleMap.setAgentDisp = function (dur, num) {
        this.AGENTTIME = parseInt(Math.min(1000,Math.max(dur, 10000)));
        this.MAXAGENTCONN = parseInt(num);
        console.log('link show for '+this.AGENTTIME)
        console.log('show agentlink number: '+ this.MAXAGENTCONN)
    }
    
    let     gA = container.append("g").attr("class", "agentsg");
    
    let agentSimNodes = [];
    
    bubbleMap.simpleName = function (iri) {
      let arr = iri.split()
      
    };
    bubbleMap.drawAgent = function (datum) {
        //if can not find - not expand - don't do anything
        datum.plantId = normalUrl(datum.plantId);
       console.log(datum.plantId)
        let theNode = d3.selectAll("a.cir")
            .filter(function(d) {
                return d.url === datum.plantId });
        console.log('length of existing liks :'+agentLinkData.length);
        if(!theNode || theNode.empty() ){
            console.log('do nothing')
            return;
        }
        
        if(agentLinkData.length> self.MAXAGENTCONN){//exceed max length
            return datum;
        }
        console.log(getSimpleName(datum.plantId));
        this.highlightNode(theNode);
        
        /*draw agent********************************/
        datum.id =  datum.agentId+datum.scenarioId ;
        let existN = agentData.find((ag)=>{ return ag.id === datum.id });//if node exists, don't add again
        if(!existN) {
            console.log('node not exists')
            agentData.push(datum);
        }
        //add plant node
        agentLinkData.push({target:{id:datum.plantId+datum.scenarioId, plantId: datum.plantId
        }, source: datum });
        agentSimNodes.push({id:datum.plantId+datum.scenarioId, plantId: datum.plantId});
        console.log('nodes')
       console.log(agentData)
        console.log('links:')
        console.log(agentLinkData)
        simulation = d3.forceSimulation()
            .force("link", d3.forceLink())
            .on("tick", aticked);
    
    
        let timera = 0;
        let agentUp = agentNodes.data(agentData, d=>d.id);//TODO: FILTERed
    
        agentUp.exit().selectAll("rect").remove();//REMOVE CHILDREN
       agentUp.exit().remove();
        // agentNodes.data(agentData, d=>d.id).exit().selectAll("rect").remove();
        //   agentNodes.data(agentData, d=>d.id).exit().remove();
        agentNodes = agentUp.enter().append("a")
            .attr('class', 'agent')
            //.append("a")
            .attr("xlink:href", function (d) {
                //   console.log("@@@@@@@@@@@@draw node: " + d.name);
                return d.agentId;
            })
            .on("click", function (d) {//update this infor in
                d3.event.preventDefault();
                d3.event.stopPropagation();
        
                timera = setTimeout(function () {
                    if (!sglclickPrevent) {
                        clickAction();
                    }
            
                    sglclickPrevent = false;
                }, 200);
        
                //todo:set selected
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
                clearTimeout(timera);
    
                sglclickPrevent = true;
                d3.event.stopPropagation()
                window.open(d.agentId);
            
            });
    
       // agentNodes.append("rect")
        let symbol =  d3.symbol().size([1000]);
        let count = 0;
    
        let dx = 300;//theNode.attr("x") -500;
        let dy = 300;
        agentNodes.append("path")
            .attr("d", symbol.type(d3.symbolTriangle))

            .attr("class", "agent")
         //   .attr("width", d => 20)
       // .attr("height", d =>  20)
            .attr("x", (d,i) =>  dx+ i*50 -5 )
            .attr("y", (d,i) =>  dy -5)
            .attr("transform", (shape, i ) => "translate(" + (dx+ i*50) + ", "+dy+")")
    
            .attr("fill", function (d) {
                if( !agentColorMap.includes(d.agentId)){
                    let idx = agentColorMap.length;
                    if(idx >colorList.length){
                        return colorList[0];
                    }
                        agentColorMap.push(d.agentId);
                   return    colorList[idx];
                }
                
                let idx = agentColorMap.indexOf(d.agentId);
                if(idx){return colorList[idx];}
            });
            //.call(drag(simulationAgent));
        
    
    
    
        agentNodes = agentNodes.merge(agentUp)
    
    
        agentPath = agentPath //linking lines
            .data(agentLinkData, d=>d.target.id);
    
        agentPath.exit().remove();
    
    
        newAgentPath = agentPath.enter().append("line")
            .attr("class", function (d) {
                // console.log("@@@@@@@@@@@@draw link : " + d.source.name + '--------' + d.target.name);
                return "link agent";
            });
           
        
        agentPath = newAgentPath.merge(agentPath);
    
    
        console.log('simed targets')
        console.log(agentSimNodes)
        simulation
            .nodes(agentSimNodes.concat(agentData));
    
        simulation.force("link").links(agentLinkData);
        simulation.alphaTarget(0.3).restart();
    
        function aticked() {
             if(agentPath === null) return;
             try {
                 agentPath
                     .attr("x1", d => {
            
                         let theNode = d3.selectAll("path.agent")
                             .filter(function (r) {
                                 return r.agentId + r.scenarioId === d.source.agentId + d.source.scenarioId
                             });
                         return theNode ? theNode.attr('x') : null
                     })
                     .attr("y1", d => {
            
                         let theNode = d3.selectAll("path.agent")
                             .filter(function (r) {
                                 return r.agentId + r.scenarioId === d.source.agentId + d.source.scenarioId
                             });
                         return theNode ? theNode.attr('y') : null
                     })
                     .attr("x2", d => {
                         "use strict";
                         let theNode = d3.selectAll("a.cir")
                             .filter(function (t) {
                                 return t.url === d.target.plantId
                             });
                         return theNode ? theNode.attr('x') : null;
                     })
                     .attr("y2", d => {
                         "use strict";
                         let theNode = d3.selectAll("a.cir")
                             .filter(function (t) {
                                 return t.url === d.target.plantId
                             });
                         return theNode ? theNode.attr('y') : null;
                     });
             } catch(e){
             
             }
        

        }
        //delete agent nodes after display time
        setTimeout(()=>{
            "use strict";
            console.log('now delete');
    
            //delete links no longer in graph
    


           let tod = agentLinkData.filter((link)=>{ return link.target.id === (datum.plantId+datum.scenarioId) && link.source.agentId === datum.agentId });
            console.log('link to delete')
            console.log(tod)
            agentLinkData.splice(agentLinkData.indexOf(tod[0]),1)
    
          //  let plantstillIn = agentLinkData.find((link)=>{return link.target.plantId === datum.plantId && link.target.scenarioId === datum.scenarioId});
            let agentstillIn = agentLinkData.find((link)=>{return link.source.id === datum.id});
            //if(!plantstillIn){//plant node no long has link, delete
                let todpn = agentSimNodes.filter(item=> item.id ===datum.scenarioId+datum.plantId )
                agentSimNodes.splice(agentSimNodes.indexOf(todpn[0]),1);
               this.unhighlightNode(null, datum.plantId);
           // }
            if(!agentstillIn)//agent node no longer has link ,delete
            {
                let todan = agentData.filter(item=> item.agentId===datum.agentId && item.scenarioId === datum.scenarioId)
                agentData.splice(agentData.indexOf(todan[0]),1);
                
            }
    
    
            //remove svg presentation of data 2 remove
            // agentNodes.data(agentData, d=>d.id).exit().selectAll("rect").remove();.s
         //   agentNodes.data(agentData, d=>d.id).exit().remove();
    
         //   agentPath.data(agentLinkData, d=>d.id).exit().remove();
    
            simulation
                .nodes(agentSimNodes.concat(agentData));
    
            simulation.force("link").links(agentLinkData);
        },self.AGENTTIME);
        
    };


    return bubbleMap;
    
};

function deepcopyObjArr(arr) {
return JSON.parse(JSON.stringify(arr));
}


/*END constructor: d3 link graph*********************************************/



/*DOM LOGIC**************************************************************/

var map;

$(window).load(function () {// when web dom ready
    /*init d3 map*/
    let url = window.location.href;     // Returns full URL
    
    map = FileLinkMap({});
    map.loadData(url)
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
            
            //TODO:ajax, only change data, but with d3...how? should use angular instead?
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
                            map.update(links, coords, [],true );
                            
                            
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
        let agentDur = parseFloat($("#agent-duration").val());
        let agentNum = parseFloat($("#agent-number").val());
        
        // check validity, display err msg
      //  let radius = parseFloat($("#search-radius").val());
        if ( isNaN(agentDur) || isNaN(agentNum) || agentNum<0 || agentDur <0 ) {
            displayMsg("Input parameters are not number", "danger");
            return;
        }
        
         //todo:
        
        map.setAgentDisp(agentDur, agentNum);
  
        
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
    let qTime = 1000;
    let rerequestTimer, allowRe = true;
    let queue = [], qManager = null;
    
    //todo: discerning update with node addition!
    socket.on('agentevent', function (data) {
		console.log('agentEvent')
        //shape library
        queue.push(data);
        clearInterval(qManager);
        qManager = setInterval(()=>{
            //console.log('now draw')
            let residue = [];//todo
            while(queue.length > 0){
                let datum = queue.shift();
               let residueE =  map.drawAgent(datum);
               if(residueE){
                   residue.push(residueE);//throw out
                   console.log('residue')
               }
            }
            queue = residue;
            if(!queue){
                clearInterval(qManager);
            }
        }, qTime);

        
    });
    
        socket.on('update', function (data) {
        console.log("Socket event!!!!!!!!!!!")
        console.log(data)
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
        console.log(data.filename)
        
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
        
        console.log(data)
        if (allowRe && data.filename == "NuclearPlants\.owl") { // todo: add type to change event: add and file update are different types of change
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
    //TODO: add this to backend
    let deviceMap = new Map();
    $.getJSON("JSON/prototypeDevices.json", function (data) {
        for (let device of Object.keys(data)) {
            console.log("loading device: " + device);
            deviceMap.set(device, data[device]);
        }
        
    });
    
    return deviceMap;
    
})();

function normalUrl(u){
    let uar = u.split('#');
    if(uar.length > 0){
        return uar[0]
    }
    return u;
}

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
        //TODO:hightlight bubble
        
       
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