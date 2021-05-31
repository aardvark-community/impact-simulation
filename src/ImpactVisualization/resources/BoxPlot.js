var width_b = 900; 
var height_b = 420;
var barWidth = 30;

var margin_b = {top: 20, right: 10, bottom: 40, left: 10};

var width_b = width_b - margin_b.left - margin_b.right;
var height_b = height_b - margin_b.top - margin_b.bottom;

var totalWidth = width_b + margin_b.left + margin_b.right;
var totalHeight = height_b + margin_b.top + margin_b.bottom;

let groupCounts = {}; //this will come from the program with all necessary data!!
let globalCounts = [];

let svg_b = undefined;
let svg_container = undefined;
let axisG = undefined;
let axisBottomG = undefined;
let g_b = undefined;
let boxPlotId = undefined;
//let attribute = undefined;

function initBoxPlot(id) {

    boxPlotId = id;

    // Setup the svg and group we will draw the box plot in
    svg_b = d3.select(".boxPlot").append("svg")
        .attr("id","boxPlotSvg")
        .attr("viewBox", "0 0 " + (totalWidth + 2 * margin_b.right) + " " + height_b)
	    .attr("width", totalWidth)
	    .attr("height", totalHeight)


    	// add a title
	svg_b.append("g")
        .attr("transform", "translate("  + ((totalWidth + margin_b.left + margin_b.right) / 2) + ",-20)")
        .append("text")
        .attr("id","mainText")
        //.attr("x", ((totalWidth + margin_b.left + margin_b.right) / 2))             
       // .attr("y", 0)
        .attr("text-anchor", "middle")  
        .style("font", "42px sans-serif")
        .text("Select probes with main controller!");

    svg_container = svg_b.append("g")
        .attr("transform", "translate(" + margin_b.left + "," + margin_b.top + ")");

    // Move the left axis over 25 pixels, and the top axis over 35 pixels
    axisG = svg_container.append("g").attr("transform", "translate(45,10)");
    axisBottomG = svg_container.append("g").attr("transform", "translate(50," + (height_b + margin_b.top) + ")");

    // Setup the group the box plot elements will render in
    g_b = svg_container.append("g")
	    .attr("transform", "translate(35,10)");
}

function setNewAttribute(attr) {
    svg_b.select("#mainText")
        .text(attr)
}

function refreshBoxPlot(data){

    resetContainers();
   
    groupCounts = data

    //svg_b.select("#mainText")
    //    .text(attribute)

    // Sort group counts so quantile methods work
    for(var key in groupCounts) {
	    var groupCount = groupCounts[key];
	    groupCounts[key] = groupCount.sort(sortNumber);
        globalCounts = globalCounts.concat(groupCount);
    }

    // Setup a color scale for filling each box
    var colorScale = d3.scaleOrdinal(d3.schemeCategory10)
	    .domain(Object.keys(groupCounts));

    var boxPlotData = [];
    for (var [key, groupCount] of Object.entries(groupCounts)) {

	    var record = {};
	    var localMin = d3.min(groupCount);
	    var localMax = d3.max(groupCount);

	    record["key"] = key;
	    record["counts"] = groupCount;
	    record["quartile"] = boxQuartiles(groupCount);
	    record["whiskers"] = [localMin, localMax];
	    record["color"] = colorScale(key);

	    boxPlotData.push(record);
    }

    // Compute an ordinal xScale for the keys in boxPlotData
    var xScale = d3.scalePoint()
	    .domain(Object.keys(groupCounts))
	    .rangeRound([0, width_b])
	    .padding([0.5]);

    // Compute a global y scale based on the global counts
    var globalMin = d3.min(globalCounts);
    var globalMax = d3.max(globalCounts);
    var yScale = d3.scaleLinear()
	    .domain([globalMin, globalMax])
	    .range([height_b, 0]);

      // Draw the box plot vertical lines
    var verticalLines = g_b.selectAll(".verticalLines")
	    .data(boxPlotData)
	    .enter()
	    .append("line")
	    .attr("x1", d => xScale(d.key) + barWidth/2)
	    .attr("y1", d => yScale(d.whiskers[0]))
	    .attr("x2", d => xScale(d.key) + barWidth/2)
	    .attr("y2", d => yScale(d.whiskers[1]))
	    .attr("stroke", "#000")
	    .attr("stroke-width", 1)
        .style("stroke-dasharray","5,5")
	    .attr("fill", "none");

    // Draw the boxes of the box plot, filled in white and on top of vertical lines
    var rects = g_b.selectAll("rect")
        .data(boxPlotData)
        .enter()
        .append("rect")
        .attr("width", barWidth)
        .attr("height", d =>  yScale(d.quartile[0]) - yScale(d.quartile[2]))
        .attr("x", d => xScale(d.key))
        .attr("y", d => yScale(d.quartile[2]))
        .attr("fill", d => d.color)
        .attr("stroke", "#000")
        .attr("stroke-width", 1);

    var format = d3.format(".2s")

    var rectText1 = g_b.selectAll(".rectText1")
        .data(boxPlotData)
        .enter()
        .append("text")
        .attr("dy", ".35em")
        .attr("dx", -3)
        .attr("x", d => xScale(d.key))
        .attr("y", d => yScale(d.quartile[0]))
        .attr("fill", "#000")
        .attr("text-anchor", "end")
        .style("font", "14px sans-serif")
        .text(d => format(d.quartile[0]));

    var rectText2 = g_b.selectAll(".rectText2")
        .data(boxPlotData)
        .enter()
        .append("text")
        .attr("dy", ".35em")
        .attr("dx", -3)
        .attr("x", d => xScale(d.key))
        .attr("y", d => yScale(d.quartile[2]))
        .attr("fill", "#000")
        .attr("text-anchor", "end")
        .style("font", "14px sans-serif")
        .text(d => format(d.quartile[2]));

     // Now render all the horizontal lines at once - the whiskers and the median
    var horizontalLineConfigs = [
        // Top whisker
        {
          x1: d => xScale(d.key),
          y1: d => yScale(d.whiskers[1]),
          x2: d => xScale(d.key) + barWidth,
          y2: d => yScale(d.whiskers[1]),
          text: d => format(d.whiskers[1])
        },
        // Median line
        {
          x1: d => xScale(d.key),
          y1: d => yScale(d.quartile[1]),
          x2: d => xScale(d.key) + barWidth,
          y2: d => yScale(d.quartile[1]),
          text: d => format(d.quartile[1])
        },
        // Bottom whisker
        {
          x1: d => xScale(d.key),
          y1: d => yScale(d.whiskers[0]),
          x2: d => xScale(d.key) + barWidth,
          y2: d => yScale(d.whiskers[0]),
          text: d => format(d.whiskers[0])
        }
    ];

    for(var i=0; i < horizontalLineConfigs.length; i++) {
        var lineConfig = horizontalLineConfigs[i];

        // Draw the whiskers at the min for this series
        var horizontalLine = g_b.selectAll(".whiskers")
          .data(boxPlotData)
          .enter()
          .append("line")
          .attr("x1", lineConfig.x1)
          .attr("y1", lineConfig.y1)
          .attr("x2", lineConfig.x2)
          .attr("y2", lineConfig.y2)
          .attr("stroke", "#000")
          .attr("stroke-width", 1)
          .attr("fill", "none");

         var lineTexts = g_b.selectAll(".whiskersText")
            .data(boxPlotData)
            .enter()
            .append("text")
            .attr("dy", ".3em")
            .attr("dx", 2)
            .attr("x", lineConfig.x2)
            .attr("y", lineConfig.y2)
            .attr("fill", "#000")
            .style("font", "14px sans-serif")
            .text(lineConfig.text);
    }

    // Setup a scale on the left
    var axisLeft = d3.axisLeft(yScale)
        .tickFormat(d3.format(".2s"));;
    axisG.append("g")
        .style("font", "16px sans-serif")
        .call(axisLeft);

    // Setup a series axis on the top
    var axisBottom = d3.axisBottom(xScale)
        .tickFormat(d => "Probe " + d);

    axisBottomG.append("g")
        .style("font", "16px sans-serif")
        .style("font-weight", "bold")
        .call(axisBottom);

    var xPositions = []
    var yPositions = []
    axisBottomG.selectAll('.tick')
        .each((d, i, n) => {
            var currNode = n[i].getScreenCTM();
            var xpos = currNode.e;
            var ypos = currNode.f;
            var w = window.innerWidth;
            var h = window.innerHeight;
            var normX = xpos / w;
            var normY = ypos / h;
            xPositions.push(normX);
            yPositions.push(normY);
            console.log("Width: " + w + ", Height: " + h);
            console.log("Position: x: " + xpos + ", y: " + ypos);
            console.log("Normalized Position: x: " + normX + ", y: " + normY);
        });

    if (xPositions.length > 0 && yPositions.length > 0) { 
        aardvark.processEvent(boxPlotId.id, "boxplot", xPositions, yPositions);
    }

}

function resetContainers(){
    groupCounts = {};
    globalCounts = [];
    g_b.selectAll("*").remove()
    axisG.selectAll("*").remove()
    axisBottomG.selectAll("*").remove()
}

function boxQuartiles(d) {
  	return [
    	d3.quantile(d, .25),
    	d3.quantile(d, .5),
    	d3.quantile(d, .75)
  	];
}

// Perform a numeric sort on an array
function sortNumber(a,b) {
	return a - b;
}

