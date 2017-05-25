/**
 * Created by Shaocong on 4/7/2017.
 */
// http://blog.thomsonreuters.com/index.php/mobile-patent-suits-graphic-of-the-day/


var colorList = ["#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
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
    "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C"];

var colorMap=  {};
var mapSize = 0;

$(document).ready(function () {

console.log($("#data").val());
    var links = JSON.parse($("#data").val());

    for(var link of links) {
        console.log("S: "+link.source);
        console.log("T: "+link.target);
    }
    console.log(links instanceof Array);



    //TODO: side bar shows domain
       //TODO: onclick hightlight all conenctions to this
    var nodes = {};

    function getDomain(str){
        var arr = str.split("/");
        return arr[0]+"/"+arr[1]+"/"+arr[2]+"/"+arr[3];

    }

    function getSimpleName(url){
        var arr = url.split("/");

        return (arr[arr.length - 1] === "")? arr[arr.length-3]+'/'+arr[arr.length-2] : arr[arr.length-2]+"/"+arr[arr.length -1];

        return url;
    }

        // Compute the distinct nodes from the links.
        links.forEach(function (link) {

            /**
            if(!nodes.hasOwnProperty(link.source)){
                nodes[link.source] = {name: link.source};
            }
            if(!nodes.hasOwnProperty(link.target)){
                nodes[link.target] = {name: link.target};
            }
             ***/
           link.source = nodes[link.source] || (nodes[link.source] = {name: getSimpleName(link.source),  domain: getDomain(link.source)});
            link.target = nodes[link.target] || (nodes[link.target] = {name: getSimpleName(link.target),  domain :getDomain(link.target)});
        });


        var width = 1500,
            height = 1200;


        var force = d3.layout.force()
            .nodes(d3.values(nodes))
            .links(links)
            .size([width, height])
            .linkDistance(300)
            .charge(-5000)
            .gravity(0.1)
            .on("tick", tick)
            .start();

        var svg = d3.select("body").append("svg")
            .attr("width", width)
            .attr("height", height);

        // Per-type markers, as they don't inherit styles.
        svg.append("defs").selectAll("marker")
            .data(["suit", "licensing", "resolved"])
            .enter().append("marker")
            .attr("id", function (d) {
                return d;
            })
            .attr("viewBox", "0 -5 10 10")
            .attr("refX", 15)
            .attr("refY", -1.5)
            .attr("markerWidth", 6)
            .attr("markerHeight", 6)
            .attr("orient", "auto")
            .append("path")
            .attr("d", "M0,-5L10,0L0,5");

        var path = svg.append("g").selectAll("path")
            .data(force.links())
            .enter().append("path")
            .attr("class", function (d) {
                return "link " + "licensing";
            })
            .attr("marker-end", function (d) {
                return "url(#" + "licensing" + ")";
            })
            ;

           var g = 0;
        var circle = svg.append("g").selectAll("circle")
            .data(force.nodes())
            .enter().append("circle")
            .attr("r", 15)
            .attr("fill", allocateColor)
            .on("mouseover", handleMouseOver)
            .on("mouseout", handleMouseOut)
            .call(force.drag);

        var text = svg.append("g").selectAll("text")
            .data(force.nodes())
            .enter().append("text")
            .attr("x", 5)
            .attr("y", "0.31em")
            .text(function (d) {
                return d.name;
            });

        // Use elliptical arc path segments to doubly-encode directionality.
        function tick() {
            path.attr("d", linkArc);
            circle.attr("transform", transform);
            text.attr("transform", transform);
        }

        function linkArc(d) {
            var dx = d.target.x - d.source.x,
                dy = d.target.y - d.source.y,
                dr = Math.sqrt(dx * dx + dy * dy);
            return "M" + d.source.x + "," + d.source.y + "A" + dr + "," + dr + " 0 0,1 " + d.target.x + "," + d.target.y;
        }

        function transform(d) {
            return "translate(" + d.x + "," + d.y + ")";
        }

        function allocateColor(d){
           // is this exist in color map?

            colorMap[d.domain] = colorMap[d.domain]||(colorList[mapSize++]);

            console.log("mapsize" + mapSize);
         if(mapSize >= mapSize.length){
             mapSize = 0;
             console.log("WARNING: DOMAIN NUMBER EXISTS COLOR MAP NUMBER!");
         }

         return  colorMap[d.domain];
        }

        function handleMouseOver(dCircle){
            //node name : d.name
            svg.selectAll("path")
                .style("stroke", highlightPath);



            function highlightPath(dLink)
            {
               // console.log("source: "+JSON.stringify(dLink.source)+"  target: "+ JSON.stringify(dLink.target) +" node name: "+dCircle.name);
                if( (dLink.source && dLink.source.name === dCircle.name) ||( dLink.target && dLink.target.name===dCircle.name)){
               //     console.log("change color");
                    return "#ffff00";
                }
                return "#008000";
            }
        }
    function handleMouseOut(dCircle){
        //node name : d.name
       // console.log("change back");

        svg.selectAll("path").style("stroke", "#008000");

    }


});