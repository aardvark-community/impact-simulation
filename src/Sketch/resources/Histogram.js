var x = undefined;
var y = undefined;
var color = undefined;
var width = undefined;
var height = undefined;
var formatCount = undefined;
var svg = undefined;
var colorScale = undefined;
var tooltip = undefined; 
var showTooltip = undefined;
var moveTooltip = undefined;
var hideTooltip = undefined; 
var bar = undefined;
var clip = undefined;
var brush = undefined;
var min = undefined; 
var max = undefined;
var temp = undefined;


function initHisto(target) {
    color = "steelblue";

    // Generate a 1000 data points using normal distribution with mean=20, deviation=5
    var values = d3.range(0);
    //var values = d3.range(1000).map(d3.randomUniform(20, 5));

    // A formatter for counts.
    formatCount = d3.format(",.0f");

    var margin = { top: 15, right: 10, bottom: 20, left: 10 }
    width = 330 - margin.left - margin.right;
    height = 190 - margin.top - margin.bottom;


    max = Math.ceil(d3.max(values));
    min = Math.floor(d3.min(values));

    var range = max - min;
    var breite = (range / 10) < 0.5 ? 1 : Math.round(range / 10);
    if (range <= 1) {
        breite = 0.1;
    }
    var rest = range % breite;
    var toAdd = rest != 0 ? breite - rest : 0;
    var _max = max + toAdd;
    var bins = (_max - min) / breite;

   // console.log(rest, toAdd, bins, min, max, _max);

    x = d3.scaleLinear()
        .domain([min, max])
        .range([0, width]);

    // Generate a histogram using twenty uniformly-spaced bins.
    var data_ = d3.histogram()
        .value(d => d)
        .domain(x.domain())
        .thresholds(d3.range(x.domain()[0], x.domain()[1], (x.domain()[1] - x.domain()[0]) / bins));
        //.thresholds(x.ticks(10));
    var data = data_(values);
    console.log(data);

    var yMax = d3.max(data, function (d) { return d.length });
    var yMin = d3.min(data, function (d) { return d.length });
    colorScale = d3.scaleLinear()
        .domain([yMin, yMax])
        .range(["#00c6ff", "#2b44ff"])
        .interpolate(d3.interpolateHcl); //interpolateHsl interpolateHcl interpolateRgb

    y = d3.scaleLinear()
        .domain([0, yMax])
        .range([height, 0]);

    var xAxis = d3.axisBottom(x)
        .tickFormat(d3.format(".0s"));

    svg = d3.select(target).append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    tooltip = d3.select(target)
        .append("div")
        .style("opacity", 0)
        .attr("class", "tooltip")
        .style("background-color", "black")
        .style("color", "white")
        .style("border-radius", "5px")
        .style("padding", "10px")
        .style("position", "absolute")

    showTooltip = function (d) {
        tooltip
            .transition()
            .duration(100)
            .style("opacity", 0.75)
        tooltip
            .html("Range: " + d.x0 + " - " + d.x1)
            .style("left", (x(d.x0) - 10) + "px")
            .style("top", (y(d.length) - 5) + "px")
    }

    moveTooltip = function (d) {
        tooltip
            .style("left", (x(d.x0) - 10) + "px")
            .style("top", (y(d.length) - 5) + "px")
        console.log("x: " + (x(d.x0)));
        console.log("y: " + (y(d.length)));
    }

    hideTooltip = function (d) {
        tooltip
            .transition()
            .duration(100)
            .style("opacity", 0)
    }

    clip = svg.append("defs").append("svg:clipPath")
        .attr("id", "clip")
        .append("svg:rect")
        .attr("width", width)
        .attr("height", height)
        .attr("x", 0)
        .attr("y", 0);

    if (values.length > 0) {
        bar = svg.append("g")
            .attr("clip-path", "url(#clip)")
            .selectAll(".bar")
            .data(data)
            .enter().append("g")
            .attr("class", "bar")
            .attr("transform", function (d) { return "translate(" + x(d.x0) + "," + y(d.length) + ")"; })
            .on("mouseover", showTooltip)
            .on("mousemove", moveTooltip)
            .on("mouseleave", hideTooltip);

        bar.append("rect")
            .attr("x", 1)
            .attr("width", d => { var _new = x(d.x1) - x(d.x0) - 1; return _new < 0 ? 0 : _new; })
            .attr("height", function (d) { return height - y(d.length); })
            .attr("fill", function (d) { return colorScale(d.length) });

        bar.append("text")
            .attr("dy", ".75em")
            .attr("y", -12)
            .attr("x", d => (x(d.x1) - x(d.x0)) / 2)
            .attr("text-anchor", "middle")
            .text(function (d) { return formatCount(d.length); });
    }

    svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis)

    brush = d3.brushX()
        .extent([[0, 0], [width, height]])
        .on("end", updateHistogram)

    //temp.append("g")
    //    .attr("class", "brush")
    //    .call(brush);
        

    //svg.append("g")
    //    .attr("class", "brush")
    //    .call(brush);
}

var idleTimeout
function idled() { idleTimeout = null; }

function updateHistogram() {
    var extent = d3.event.selection

    if (!extent) {
        if (!idleTimeout) return idleTimeout = setTimeout(idled, 350); // This allows to wait a little bit
        x.domain([min, max])
    } else {
        x = d3.scaleLinear()
            .domain([x(Math.floor(extent[0])), x(Math.ceil(extent[1]))])
            .range([0, width]);

        temp.select(".brush").call(brush.move, null) // This remove the grey brush area as soon as the selection has been done
    }
}


/*
* Adding refresh method to reload new data
*/
function refresh(values) { // normalized values 

    // var values = d3.range(1000).map(d3.random.normal(20, 5));

    max = Math.ceil(d3.max(values));
    min = Math.floor(d3.min(values));

    var range = max - min;
    var breite = (range / 10) < 0.5 ? 1 : Math.round(range / 10);
    if (range <= 1) {
        breite = 0.1;
    }
    var rest = range % breite;
    var toAdd = rest != 0 ? breite - rest : 0;
    var _max = max + toAdd;
    var bins = (_max - min) / breite;

    //console.log(rest, toAdd, bins, min, max, _max);

    x = d3.scaleLinear()
        .domain([min, max]) 
        .range([0, width]);

    var data_ = d3.histogram()
        .value(d => d)
        .domain(x.domain())
        .thresholds(d3.range(x.domain()[0], x.domain()[1], (x.domain()[1] - x.domain()[0]) / bins));
        //.thresholds(x.ticks(10));
    var data = data_(values);
    console.log(data);

    // Reset y domain using new data
    var yMax = d3.max(data, function (d) { return d.length });
    var yMin = d3.min(data, function (d) { return d.length });
    y.domain([0, yMax]);

    colorScale = d3.scaleLinear()
        .domain([yMin, yMax])
        .range(["#00c6ff", "#2b44ff"])
        .interpolate(d3.interpolateHcl); //interpolateHsl interpolateHcl interpolateRgb

    var specifier = ".1";
    var value = 1;

    if (max >= 1000 || Math.abs(min) >= 1000) {
        specifier = ".0"
        value = 1e3;
    }
    if (max >= 1000000 || Math.abs(min) >= 1000000) {
        specifier = ".1"
        value = 1e6;
    }

    var xAxis = d3.axisBottom(x)
                .tickFormat(d3.format(".0s"));


    bar = svg.selectAll(".bar")
        .data(data)
        .on("mouseover", showTooltip)
        .on("mousemove", moveTooltip)
        .on("mouseleave", hideTooltip);

    // what happens when additional data arrives...
    //bar.enter().append("rect").append("text") // Add a new rect for each new elements

    var bars = bar.enter()
        .append("g")
        .attr("class", "bar")
        .merge(bar)
        .attr("transform", d => "translate(" + x(d.x0) + "," + y(d.length) + ")")
        .html("");

    bars.append("rect");
    bars.append("text");

    // describe how rectangle/bin look like
    bars.selectAll("rect")
        .transition()
        .duration(1000)
        .attr("x", 1)
        .attr("width", d => { var _new = x(d.x1) - x(d.x0) - 1; return _new < 0 ? 0 : _new; })
        .attr("height", d => height - y(d.length))
        .attr("fill", d => colorScale(d.length));


    // describe how text look like
    bars.selectAll("text")
        .transition()
        .duration(1000)
        .attr("dy", ".75em")
        .attr("y", -12)
        .attr("x", d => (x(d.x1) - x(d.x0)) / 2)
        .attr("text-anchor", "middle")
        .text(d => formatCount(d.length));

    // Changes / Animations
    bars.transition()
        .duration(1000)
        .attr("transform", d => "translate(" + x(d.x0) + "," + y(d.length) + ")");

    // Remove object with data
    bar.exit().remove();

    svg.selectAll("g.x.axis")
        .call(xAxis)

}

// Calling refresh repeatedly.
//setInterval(function () {
//    var values = d3.range(1000).map(d3.random.normal(20, 5));
//    refresh(values);
//}, 2000);