

(function() {
    d3.legend = function(g) {
        g.each(function() {
            var g= d3.select(this),
                items = {},
                svg = d3.select(g.property("nearestViewportElement")),
                legendPadding = g.attr("data-style-padding") || 5,
                lb = g.selectAll(".legend-box").data([true]),
                li = g.selectAll(".legend-items").data([true])
                title = g.selectAll(".title").data(["Level in structure"]);

            lb.exit().remove();
            li.exit().remove();
            title.exit().remove();
            lb.enter().append("rect").classed("legend-box",true);

            li.enter().append("g").classed("legend-items",true)
            title.enter().append("text").text(function (d) {
                return d;
            }).classed("title",true);

            lb = g.selectAll(".legend-box")
            li =  g.selectAll(".legend-items")
            title = g.selectAll("text.title");
            console.log(title);
            svg.selectAll("[data-legend]").each(function() {
                console.log("new legend!")
                var self = d3.select(this)
                items[self.attr("data-legend")] = {
                    pos : self.attr("data-legend-pos") || this.getBBox().y,
                    color : self.attr("data-legend-color") != undefined ? self.attr("data-legend-color") : self.style("fill") != 'none' ? self.style("fill") : self.style("stroke")
                }
            })

            console.log(svg.selectAll("[data-legend]"));
            items = d3.entries(items).sort(function(a,b) { return a.value.pos-b.value.pos})

            console.log(JSON.stringify(items));

            li.selectAll("text")
                .data(items,function(d) { return d.key})
             //   .call(function(d) { d.enter().append("text")})
              //  .call(function(d) { d.exit().remove()})
                .enter().append("text")
                .attr("y",function(d,i) { return i+"em"})
                .attr("x","1em")
                .text(function(d) {return d.key});

          var cir =   li.selectAll("circle")
                .data(items,function(d) { return d.key});
               // .call(function(d) { d.enter().append("circle")})
               // .call(function(d) { d.exit().remove()})
            cir.exit().remove();
                cir.enter().append("circle")
                .attr("cy",function(d,i) { return i-0.25+"em"})
                .attr("cx",0)
                .attr("r","0.4em")
                .style("fill",function(d) { console.log(d.value.color);return d.value.color})

            // Reposition and resize the box
            var lbbox = li._groups[0][0].getBBox();
            lb.attr("x",(lbbox.x-legendPadding))
                .attr("y",(lbbox.y-legendPadding))
                .attr("height",(lbbox.height+2*legendPadding))
                .attr("width",(lbbox.width+2*legendPadding))
            title.attr("x", lbbox.width+lbbox.x+2*legendPadding);

        })


        return g
    }
})()