function visualizeComposition(dataObject) {

    document.getElementById("myDiagramDiv").hidden = true;
    document.getElementById("myDiagramDiv2").hidden = false;

    if (window.goSamples) goSamples();  // init for these samples -- you don't need to call this
    var $ = go.GraphObject.make;  // for conciseness in defining templates

    myDiagram =
        $(go.Diagram, "myDiagramDiv2",
            {
                //initialScale: 0.2,
                initialAutoScale: go.Diagram.Uniform,
                initialContentAlignment: go.Spot.Center,
                validCycle: go.Diagram.CycleNotDirected,  // don't allow loops
                // For this sample, automatically show the state of the diagram's model on the page
                "ModelChanged": function (e) {
                    if (e.isTransactionFinished) showModel();
                },
                "undoManager.isEnabled": true
            });
			
			
	myDiagram.linkTemplate =
    $(go.Link,go.Link.Bezier,
      { curve: go.Link.Bezier },  // Bezier curve
      $(go.Shape),
      $(go.Shape, { toArrow: "Standard" }),
	   new go.Binding("fromEndSegmentLength", "curviness"),
         new go.Binding("toEndSegmentLength", "curviness")
    );

    // This template is a Panel that is used to represent each item in a Panel.itemArray.
    // The Panel is data bound to the item object.
    var fieldTemplate =
        $(go.Panel, "TableRow",  // this Panel is a row in the containing Table
            new go.Binding("portId", "name"),  // this Panel is a "port"
            {
                background: "transparent",  // so this port's background can be picked by the mouse
                fromSpot: go.Spot.Right,  // links only go from the right side to the left side
                toSpot: go.Spot.Left,
                // allow drawing links from or to this port:
                fromLinkable: true, toLinkable: true
            },
            $(go.TextBlock,
                {
                    column: 1,
                    margin: new go.Margin(0, 2),
                    stretch: go.GraphObject.Horizontal,
                    font: "bold 14px sans-serif",
                    wrap: go.TextBlock.None,
                    overflow: go.TextBlock.OverflowEllipsis,
                    // and disallow drawing links from or to this text:
                    fromLinkable: true, toLinkable: true
                },
                new go.Binding("text", "name")),
            $(go.TextBlock,
                {
                    column: 1,
                    margin: new go.Margin(0, 2),
                    stretch: go.GraphObject.Horizontal,
                    font: "bold 14px sans-serif",
                    wrap: go.TextBlock.None,
                    overflow: go.TextBlock.OverflowEllipsis,
                    // and disallow drawing links from or to this text:
                    fromLinkable: true, toLinkable: true
                },
                new go.Binding("text", "name"))
        );

    // Return initialization for a RowColumnDefinition, specifying a particular column
    // and adding a Binding of RowColumnDefinition.width to the IDX'th number in the data.widths Array
    function makeWidthBinding(idx) {
        // These two conversion functions are closed over the IDX variable.
        // This source-to-target conversion extracts a number from the Array at the given index.
        function getColumnWidth(arr) {
            if (Array.isArray(arr) && idx < arr.length) return arr[idx];
            return NaN;
        }

        // This target-to-source conversion sets a number in the Array at the given index.
        function setColumnWidth(w, data) {
            var arr = data.widths;
            if (!arr) arr = [];
            if (idx >= arr.length) {
                for (var i = arr.length; i <= idx; i++) arr[i] = NaN;  // default to NaN
            }
            arr[idx] = w;
            return arr;  // need to return the Array (as the value of data.widths)
        }

        return [
            {column: idx},
            new go.Binding("width", "widths", getColumnWidth).makeTwoWay(setColumnWidth)
        ]
    }
	
    // This template represents a whole "record".
    myDiagram.nodeTemplate =
        $(go.Node, "Auto",
            new go.Binding("location", "loc", go.Point.parse).makeTwoWay(go.Point.stringify),
            // this rectangular shape surrounds the content of the node
            $(go.Shape,
                {fill: "#EEEEEE"}),


            $(go.Panel, "Vertical",
            		
  
            		
                $(go.Panel, "Auto",
                    {stretch: go.GraphObject.Horizontal},  // as wide as the whole node
                    $(go.Shape,
                        {fill: "#1570A6", stroke: null}),
                    $(go.TextBlock,
                        {
                            alignment: go.Spot.Center,
                            margin: 3,
                            stroke: "white",
                            textAlign: "center",
                            font: "bold 12pt sans-serif"
                        },
                        new go.Binding("text", "key"))
                ),
                // the content consists of a header and a list of items
                $(go.Panel, "Horizontal",
                    {stretch: go.GraphObject.Horizontal, alignment: go.Spot.TopLeft},
                    // this is the header for the whole node

                    // this Panel holds a Panel for each item object in the itemArray;
                    // each item Panel is defined by the itemTemplate to be a TableRow in this Table
                    $(go.Panel, "Table",
                        {
                            name: "TABLE", stretch: go.GraphObject.Horizontal,
                            minSize: new go.Size(100, 30),
                            defaultAlignment: go.Spot.Center,
                            defaultStretch: go.GraphObject.Horizontal,
                            defaultColumnSeparatorStroke: "gray",
                            defaultRowSeparatorStroke: "gray",
                            itemTemplate: fieldTemplate
                        },
                        $(go.RowColumnDefinition, makeWidthBinding(1)),
                        new go.Binding("itemArray", "inputs")
                    ),
                    $(go.Panel, "Table",
                        {
                            name: "TABLE", stretch: go.GraphObject.Horizontal,
                            minSize: new go.Size(100, 30),
                            defaultAlignment: go.Spot.Center,
                            defaultStretch: go.GraphObject.Horizontal,
                            defaultColumnSeparatorStroke: "gray",
                            defaultRowSeparatorStroke: "gray",
                            itemTemplate: fieldTemplate
                        },
                        $(go.RowColumnDefinition, makeWidthBinding(1)),
                        new go.Binding("itemArray", "outputs")
                    )
                ),                
                $(go.Panel, "Auto",
                        {stretch: go.GraphObject.Horizontal},  // as wide as the whole node
                        $(go.Shape,
                            {fill: "#1570A6", stroke: null}),
                        $(go.TextBlock,
                            {
                                alignment: go.Spot.Center,
                                margin: 3,
                                stroke: "white",
                                textAlign: "center",
                                font: "bold 12pt sans-serif"
                            },
                            new go.Binding("text", "score"))
                    )
                
            )
        );  // end Node

/*
    myDiagram.linkTemplate =
        $(go.Link,
            {	curve: go.Link.Bezier
         //       relinkableFrom: true, relinkableTo: true, // let user reconnect links
          //      toShortLength: 4, fromShortLength: 2
            },
            $(go.Shape),
            $(go.Shape, {toArrow: "Standard"})
        );
*/

    myDiagram.model =
        $(go.GraphLinksModel, dataObject);

    showModel();  // show the diagram's initial model

    function showModel() {
        document.getElementById("mySavedModel").textContent = myDiagram.model.toJson();
    }


}
