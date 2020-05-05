var x = undefined;
var y = undefined;
var color = undefined;
var width = undefined;
var height = undefined;
var formatCount = undefined;
var svg = undefined;
var colorScale = undefined;

function initHisto(target) {
    color = "steelblue";

    // Generate a 1000 data points using normal distribution with mean=20, deviation=5
    var values = d3.range(0);
    //var values = d3.range(1000).map(d3.randomUniform(20, 5));

    // A formatter for counts.
    formatCount = d3.format(",.0f");

    var margin = { top: 15, right: 10, bottom: 20, left: 10 }
    width = 280 - margin.left - margin.right;
    height = 150 - margin.top - margin.bottom;


    var max = Math.ceil(d3.max(values));
    var min = Math.floor(d3.min(values));

    var range = max - min;
    var breite = (range / 10) < 0.5 ? 1 : Math.round(range / 10);
    var rest = range % breite;
    var toAdd = rest != 0 ? breite - rest : 0;
    var _max = max + toAdd;
    var bins = (_max - min) / breite;

   // console.log(rest, toAdd, bins, min, max, _max);

    x = d3.scaleLinear()
        .domain([min, _max])
        .range([0, width]);

    // Generate a histogram using twenty uniformly-spaced bins.
    var data_ = d3.histogram()
        .value(d => d)
        .domain(x.domain())
        .thresholds(d3.range(x.domain()[0], x.domain()[1], (x.domain()[1] - x.domain()[0]) / bins));
        //.thresholds(x.ticks(bins));
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

    var xAxis = d3.axisBottom(x);

    svg = d3.select(target).append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    if (values.length > 0) {
        var bar = svg.selectAll(".bar")
            .data(data)
            .enter().append("g")
            .attr("class", "bar")
            .attr("transform", function (d) { return "translate(" + x(d.x0) + "," + y(d.length) + ")"; });

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
        .call(xAxis);
}

/*
* Adding refresh method to reload new data
*/
function refresh(values) { // normalized values 

    // var values = d3.range(1000).map(d3.random.normal(20, 5));

    var max = Math.ceil(d3.max(values));
    var min = Math.floor(d3.min(values));

    var range = max - min;
    var breite = (range / 10) < 0.5 ? 1 : Math.round(range / 10);
    var rest = range % breite;
    var toAdd = rest != 0 ? breite - rest : 0;
    var _max = max + toAdd;
    var bins = (_max - min) / breite;

    //console.log(rest, toAdd, bins, min, max, _max);

    x = d3.scaleLinear()
        .domain([min, _max]) 
        .range([0, width]);

    var data_ = d3.histogram()
        .value(d => d)
        .domain(x.domain())
        .thresholds(d3.range(x.domain()[0], x.domain()[1], (x.domain()[1] - x.domain()[0]) / bins));
        //.thresholds(x.ticks(bins - 1));
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

    var xAxis = d3.axisBottom(x);

    var bar = svg.selectAll(".bar")
        .data(data)

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
        .call(xAxis);

}

// Calling refresh repeatedly.
//setInterval(function () {
//    var values = d3.range(1000).map(d3.random.normal(20, 5));
//    refresh(values);
//}, 2000);