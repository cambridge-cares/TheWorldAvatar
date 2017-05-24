/**
 * Created by Shaocong on 4/7/2017.
 */
// http://blog.thomsonreuters.com/index.php/mobile-patent-suits-graphic-of-the-day/


var FileLinkMap = function (options) {
    var width = options.width || 1500,
        height = options.height || 1200,
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
        "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
        "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C", "#000000"];

    var colorMap = {};
    var mapSize = 0;


    //TODO: side bar shows domain
    //TODO: onclick hightlight all conenctions to this


    function packNodesArr(links) {
        var nodes = {};
        var nodesArr = [];

        function getDomain(str) {
            var arr = str.split("/");
            if(arr.length >3) {
                return arr[0] + "/" + arr[1] + "/" + arr[2] + "/" + arr[3];
            } else{
                return arr[0] + "/" + arr[1] + "/" + arr[2];
            }
        }

        function getSimpleName(url) {
            var arr = url.split("/");

            //  return (arr[arr.length - 1] === "")? arr[arr.length-3]+'/'+arr[arr.length-2] : arr[arr.length-2]+"/"+arr[arr.length -1];
            return (arr[arr.length - 1] === "") ? arr[arr.length - 2] : arr[arr.length - 1];

            return url;
        }

        // Compute the distinct nodes from the links.
        links.forEach(function (link) {

            link.source = nodes[link.source] || (nodes[link.source] = {
                    url : link.source,

                    name: getSimpleName(link.source),
                    domain: getDomain(link.source),
                    count: 0
                });
            link.target = nodes[link.target] || (nodes[link.target] = {
                    url : link.target,
                    name: getSimpleName(link.target),
                    domain: getDomain(link.target),
                    count: 0
                });

            //get count of links on each node
            /**
             if (nodes[link.source] != null) {
                nodes[link.source].count = nodes[link.source].count + 1;
                console.log("++");
            }
             if (nodes[link.target] != null) {
                nodes[link.target].count = nodes[link.target].count + 1;
                console.log("++");

            }
             ***/
        });


        // console.log(JSON.stringify(nodes));
        //packs object :nodes into array
        for (var attr in nodes) {
            if (nodes.hasOwnProperty(attr)) {
                nodesArr.push(nodes[attr]);
                //console.log("count" + nodes[attr].count);

            }
        }
        return nodesArr;
    }


    function setBodyS(node) {
        // return -1000*(node.count||1);

		return -700;
        //return charge;
    }

    function setD() {
        return distance;
    }


    /**
     * Allocate color for nodes of each domain
     * @param d  datum
     * @returns one color defined in colormap
     */
    function allocateColor(d) {
        // is this exist in color map?

        colorMap[d.domain] = colorMap[d.domain] || (colorList[mapSize++]);

        if (mapSize >= mapSize.length) {
            mapSize = 0;
            console.log("WARNING: DOMAIN NUMBER EXISTS COLOR MAP NUMBER!");
        }

        return colorMap[d.domain];
    }




    var svg = d3.select("#draw-panel").append("svg")
        .attr("width", width)
        .attr("height", height);
    var g = svg.append("g");


    function clear() {
        g.selectAll("line").data([]).exit().remove();
        g.selectAll("circle").data([]).exit().remove();
        g.selectAll("text").data([]).exit().remove();
    }

    function update(links) {
        let nodesArr = packNodesArr(links);

        //set force simulation
        var simulation = d3.forceSimulation()
            .force("link", d3.forceLink(links))
            .force("charge", d3.forceManyBody().strength(setBodyS))
            .force("center", d3.forceCenter(width / 2, height / 2));

        var path = g.selectAll("line") //linking lines
            .data(links, function (d) {
                return d.source.domain + d.source.name + d.target.domain + d.target.name;
            });

        path.exit().remove();


        path.enter().append("line")
            .attr("class", function (d) {
                return "link " + "licensing";
            })
            .attr("marker-end", function (d) {
                return "url(#" + "licensing" + ")";
            })
        ;
        // path.exit().remove();
        path = g.selectAll("line");

        var circle = g.selectAll("a.cir") //node bubbles
            .data(nodesArr, function (d) {
                return d.domain + d.name;
            });

        circle.exit().remove();

        circle.enter().append("a")
            .attr('class', 'cir')
            //.append("a")
            .attr("xlink:href", function (d) {
                 return d.url;
            })

            .append("circle")
            .attr("r", nodeR)
            .attr("fill", allocateColor)
            .on("mouseover", handleMouseOver)         //highlight all links when mouse over
            .on("mouseout", handleMouseOut)
            .call(d3.drag()                         //enable user to drag
                .on("start", dragstarted)
                .on("drag", dragged)
                .on("end", dragended)
            );


        circle = g.selectAll("a.cir");
       var circleDraw = g.selectAll("a.cir").select("circle");

        var text = g.selectAll("text")  //node tags
            .data(nodesArr, function (d) {
                return d.domain + d.name;
            });
        text.exit().remove();


    text.enter().append("text")
            .attr("x", textSize)
            .attr("y", "0.31em")
            .text(function (d) {
                return d.name;
            });
        text = g.selectAll("text");

        simulation
            .nodes(nodesArr)
            .on("tick", ticked);

        simulation.force("link")
            .links(links);
        function ticked() {

            path
                .attr("x1", function (d) {
                    return d.source.x;
                })
                .attr("y1", function (d) {
                    return d.source.y;
                })
                .attr("x2", function (d) {
                    return d.target.x;
                })
                .attr("y2", function (d) {
                    return d.target.y;
                });

            circle
                .attr("x", function (d) {
                    return d.x;
                })
                .attr("y", function (d) {
                    return d.y;
                });

            circleDraw
                .attr("cx", function (d) {
                    return d.x;
                })
                .attr("cy", function (d) {
                    return d.y;
                });
            text
                .attr("x", function (d) {
                    return d.x;
                })
                .attr("y", function (d) {
                    return d.y;
                });

        }


        function dragstarted(d) {
            if (!d3.event.active) simulation.alphaTarget(0.3).restart();
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

        function handleMouseOver(dCircle) {
            //node name : d.name
            svg.selectAll("line")
                .style("stroke", highlightPath);

            function highlightPath(dLink) {
                // console.log("source: "+JSON.stringify(dLink.source)+"  target: "+ JSON.stringify(dLink.target) +" node name: "+dCircle.name);
                if ((dLink.source && dLink.source.name === dCircle.name) || ( dLink.target && dLink.target.name === dCircle.name)) {
                    //     console.log("change color");


                    return "#ffff00";
                }
                return "#008000";
            }
        }

        function handleMouseOut(dCircle) {
            //node name : d.name
            // console.log("change back");

            svg.selectAll("line").style("stroke", "#008000");

        }
    }


    var links = JSON.parse($("#data").val());//extract link data from web page
    update(links);
    return {update: update};

};

$(document).ready(function () {// when web dom ready
    let url = window.location.href;     // Returns full URL

    var map = FileLinkMap({});
    $("#checkShowImport").change(function () {
if($('#checkShowImport').prop('checked')) {

        //TODO:ajax, only change data, but with d3...how? should use angular instead?
        $.ajax({
            url: '/visualize/includeImport',
            type: 'GET',

            success: function (data) {
                console.log('ajax successful!\n');
                //console.log(JSON.stringify(data));
                map.update(data);

            },
            error: function (err) {
                console.log(err);


            }
        });

} else {
	       window.location.href = '/visualize';
}
    })


});

//module.exports = FileLinkMap;