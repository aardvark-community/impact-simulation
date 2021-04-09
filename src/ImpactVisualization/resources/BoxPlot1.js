var width_b = 900; 
var height_b = 400;
var barWidth = 30;

var margin_b = {top: 20, right: 10, bottom: 20, left: 10};

var width_b = width_b - margin_b.left - margin_b.right;
var height_b = height_b - margin_b.top - margin_b.bottom;

var totalWidth = width_b + margin_b.left + margin_b.right;
var totalHeight = height_b + margin_b.top + margin_b.bottom;

let groupCounts = {}; //this will come from the program with all necessary data!!
let globalCounts = [];

let svg_b = undefined;
let axisG = undefined;
let axisTopG = undefined;
let g_b = undefined;

function initBoxPlot1(id) {
    // Setup the svg and group we will draw the box plot in
    svg_b = d3.select(".boxPlot1").append("svg")
	    .attr("width", totalWidth)
	    .attr("height", totalHeight)
	    .append("g")
	    .attr("transform", "translate(" + margin_b.left + "," + margin_b.top + ")");

    // Move the left axis over 25 pixels, and the top axis over 35 pixels
    axisG = svg_b.append("g").attr("transform", "translate(45,0)");
    axisTopG = svg_b.append("g").attr("transform", "translate(55,0)");

    // Setup the group the box plot elements will render in
    g_b = svg_b.append("g")
	    .attr("transform", "translate(40,5)");
}

function refreshBoxPlot1(data){

    resetContainers();
   
    groupCounts = data

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
	    .range([0, height_b]);

      // Draw the box plot vertical lines
    var verticalLines = g_b.selectAll(".verticalLines")
	    .data(boxPlotData)
	    .enter()
	    .append("line")
	    .attr("x1", function(d) {return xScale(d.key) + barWidth/2;})
	    .attr("y1", function(d) {return yScale(d.whiskers[0]);})
	    .attr("x2", function(d) {return xScale(d.key) + barWidth/2;})
	    .attr("y2", function(d) {return yScale(d.whiskers[1]);})
	    .attr("stroke", "#000")
	    .attr("stroke-width", 1)
	    .attr("fill", "none");

    // Draw the boxes of the box plot, filled in white and on top of vertical lines
    var rects = g_b.selectAll("rect")
        .data(boxPlotData)
        .enter()
        .append("rect")
        .attr("width", barWidth)
        .attr("height", function(d) {return yScale(d.quartile[2]) - yScale(d.quartile[0]);})
        .attr("x", function(d) {return xScale(d.key);})
        .attr("y", function(d) {return yScale(d.quartile[0]);})
        .attr("fill", function(d) {return d.color;})
        .attr("stroke", "#000")
        .attr("stroke-width", 1);

     // Now render all the horizontal lines at once - the whiskers and the median
    var horizontalLineConfigs = [
        // Top whisker
        {
          x1: function(d) { return xScale(d.key) },
          y1: function(d) { return yScale(d.whiskers[0]) },
          x2: function(d) { return xScale(d.key) + barWidth },
          y2: function(d) { return yScale(d.whiskers[0]) }
        },
        // Median line
        {
          x1: function(d) { return xScale(d.key) },
          y1: function(d) { return yScale(d.quartile[1]) },
          x2: function(d) { return xScale(d.key) + barWidth },
          y2: function(d) { return yScale(d.quartile[1]) }
        },
        // Bottom whisker
        {
          x1: function(d) { return xScale(d.key) },
          y1: function(d) { return yScale(d.whiskers[1]) },
          x2: function(d) { return xScale(d.key) + barWidth },
          y2: function(d) { return yScale(d.whiskers[1]) }
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
    }

    // Setup a scale on the left
    var axisLeft = d3.axisLeft(yScale)
        .tickFormat(d3.format(".2s"));;
    axisG.append("g").call(axisLeft);

    // Setup a series axis on the top
    var axisTop = d3.axisTop(xScale)
        .tickFormat(function(d) {return "Probe " + d});
    axisTopG.append("g").call(axisTop);
}

function resetContainers(){
    groupCounts = {};
    globalCounts = [];
    g_b.selectAll("*").remove()
    axisG.selectAll("*").remove()
    axisTopG.selectAll("*").remove()
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

