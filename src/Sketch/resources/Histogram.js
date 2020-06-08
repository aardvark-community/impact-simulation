let container = undefined;
let svg = undefined;
let g = undefined;
let x = undefined;
let y = undefined;
let color = undefined;
let width = undefined;
let height = undefined;
let formatCount = undefined;
let colorScale = undefined;
let tooltip = undefined;
let bar = undefined;
let clip = undefined;
let brush = undefined;
let brushOverlay = undefined;
let min = undefined;
let max = undefined;
let temp = undefined;
let bars_container = undefined;
let bars = undefined;
let xAxis = undefined;
let yAxis = undefined;
let lowerLimit = undefined;
let upperLimit = undefined;
let dataSet = undefined;

function initHisto(target) {
    color = "steelblue";

    // Generate a 1000 data points using normal distribution with mean=20, deviation=5
    let values = d3.range(0);
    //let values = d3.range(1000).map(d3.randomUniform(20, 5));

    // A formatter for counts.
    formatCount = d3.format(",.0f");

    let margin = { top: 20, right: 20, bottom: 25, left: 20 }
    width = 340 - margin.left - margin.right;
    height = 190 - margin.top - margin.bottom;


    max = Math.ceil(d3.max(values));
    min = Math.floor(d3.min(values));

    let range = max - min;
    let breite = (range / 10) < 0.5 ? 1 : Math.round(range / 10);
    if (range <= 1) {
        breite = 0.1;
    }
    let rest = range % breite;
    let toAdd = rest != 0 ? breite - rest : 0;
    let _max = max + toAdd;
    let bins = (_max - min) / breite;

    // console.log(rest, toAdd, bins, min, max, _max);

    x = d3.scaleLinear()
        .domain([min, max])
        .range([0, width]);

    // Generate a histogram using twenty uniformly-spaced bins.
    let data_ = d3.histogram()
        .value(d => d)
        .domain(x.domain())
        .thresholds(d3.range(x.domain()[0], x.domain()[1], (x.domain()[1] - x.domain()[0]) / bins));
    //.thresholds(x.ticks(10));
    let data = data_(values);
    console.log(data);

    let yMax = d3.max(data, function (d) { return d.length });
    let yMin = d3.min(data, function (d) { return d.length });
    colorScale = d3.scaleLinear()
        .domain([yMin, yMax])
        .range(["#00c6ff", "#2b44ff"])
        .interpolate(d3.interpolateHcl); //interpolateHsl interpolateHcl interpolateRgb

    y = d3.scaleLinear()
        .domain([0, yMax])
        .range([height, 0]);

    xAxis = d3.axisBottom(x)
        .tickFormat(d3.format(".0s"));

    container = d3.select(target).append("div")
        .style("background-color", "#ffffff")
        .style("border", "3px solid #ffffff");

    svg = container.append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom);

    g = svg
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    clip = g.append("defs").append("svg:clipPath")
        .attr("id", "clip")
        .append("svg:rect")
        .attr("width", width)
        .attr("height", height + margin.top)
        .attr("x", 0)
        .attr("y", -margin.top);

    // append bars
    bars_container = g.append("g")
        .attr("class", "bars")
        .attr("clip-path", "url(#clip)");


   

    /*if (values.length > 0) {
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
            .attr("width", d => { let _new = x(d.x1) - x(d.x0) - 1; return _new < 0 ? 0 : _new; })
            .attr("height", function (d) { return height - y(d.length); })
            .attr("fill", function (d) { return colorScale(d.length) });

        bar.append("text")
            .attr("dy", ".75em")
            .attr("y", -12)
            .attr("x", d => (x(d.x1) - x(d.x0)) / 2)
            .attr("text-anchor", "middle")
            .text(function (d) { return formatCount(d.length); });
    }*/

    g.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis);

    brush = d3.brushX()
        .extent([[0, 0], [width, height]])
        .on("end", brushed);

    brushOverlay = g.append("g")
        .attr("class", "brush")
        .call(brush)

    tooltip = svg.append("g")
        .style("opacity", 0)
        .attr("class", "tooltip")
        .style("background-color", "black")
        .style("color", "white")
        .style("border-radius", "5px")
        .style("padding", "10px")
        .style("position", "absolute")


    d3.select(".overlay")
        .on("mousemove", function () {
            var mouseCoords = d3.mouse(this);

            var mouseX = mouseCoords[0];
            var mouseY = mouseCoords[1];

            // Get x & y co-ordinates
            console.log(mouseCoords);
        })

}

function mouseevent(e) {
    var x = e.clientX;
    var y = e.clientY;
    console.log("mouseX: " + x + " mouseY: " + y);
}

let idleTimeout
function idled() { idleTimeout = null; }

function brushed(data) {

    var extent = d3.event.selection;
    //console.log("Range: " + extent);

    //aardvark.processEvent("Brushing", extent);
    if (!extent) {
        if (!idleTimeout) return idleTimeout = setTimeout(idled, 350); // This allows to wait a little bit
        if (lowerLimit && upperLimit) {
            lowerLimit = undefined;
            upperLimit = undefined;
            refresh(dataSet);
        }
    } else {
        svg.selectAll("g.x.axis")
            .call(x);

        console.log(data, extent, x.invert(extent[0]), x.invert(extent[1]));

        lowerLimit = x.invert(extent[0]);
        upperLimit = x.invert(extent[1]);

        refresh(dataSet);

        brushOverlay.call(brush.move, null);
    }
    /*
       if (!extent) {
        if (!idleTimeout) return idleTimeout = setTimeout(idled, 350); // This allows to wait a little bit
        x.domain([min, max])
    } else {
        x = d3.scaleLinear()
            .domain([x(Math.floor(extent[0])), x(Math.ceil(extent[1]))])
            .range([0, width]);

        temp.select(".brush").call(brush.move, null) // This remove the grey brush area as soon as the selection has been done
    }*/
}

function showTooltip(d) {
    console.log("show", d);
    tooltip
        .transition()
        .duration(100)
        .style("opacity", 0.75);
    tooltip
        .html("Range: " + d.x0 + " - " + d.x1)
        .style("left", (x(d.x0) - 10) + "px")
        .style("top", (y(d.length) - 5) + "px");
}

function moveTooltip(d) {
    console.log("move", d);
    tooltip
        .style("left", (x(d.x0) - 10) + "px")
        .style("top", (y(d.length) - 5) + "px")
    console.log("x: " + (x(d.x0)), "y: " + (y(d.length)));
}

function hideTooltip(d) {
    console.log("hide", d);
    tooltip
        .transition()
        .duration(100)
        .style("opacity", 0);
}

/*
* Adding refresh method to reload new data
*/
function refresh(values) { // normalized values 
    console.log("refresh", values, !values, !values || values.length == 0);
    if (values.length == 0) {
        lowerLimit = undefined;
        upperLimit = undefined;
    }
    //if (!values || values.length == 0) {
    //    // Hide histogram
    //    //svg.attr("display", "none");
    //    container.style("display", "none");
    //    return;
    //}
    //else {
    //    // Show histogram
    //    //svg.attr("display", "block");
    //    container.style("display", "block");
    //}

    dataSet = values;

    // let values = d3.range(1000).map(d3.random.normal(20, 5));
    console.log("Limits", upperLimit, lowerLimit);
    max = upperLimit ? upperLimit : Math.ceil(d3.max(values));
    min = lowerLimit ? lowerLimit : Math.floor(d3.min(values));

    let range = max - min;
    let breite = (range / 10) < 0.5 ? 1 : Math.round(range / 10);
    if (range <= 1) {
        breite = 0.1;
    }
    let rest = range % breite;
    let toAdd = rest != 0 ? breite - rest : 0;
    let _max = max + toAdd;
    let bins = (_max - min) / breite;

    //console.log(rest, toAdd, bins, min, max, _max);

    x = d3.scaleLinear()
        .domain([min, max])
        .range([0, width]).nice();

    let data_ = d3.histogram()
        .value(d => d)
        .domain(x.domain())
        .thresholds(d3.range(x.domain()[0], x.domain()[1], (x.domain()[1] - x.domain()[0]) / bins));
    //.thresholds(x.ticks(10));
    let data = data_(values);
    console.log(data);

    // Reset y domain using new data
    let yMax = d3.max(data, function (d) { return d.length });
    let yMin = d3.min(data, function (d) { return d.length });
    y.domain([0, yMax]);

    colorScale = d3.scaleLinear()
        .domain([yMin, yMax])
        .range(["#00c6ff", "#2b44ff"])
        .interpolate(d3.interpolateHcl); //interpolateHsl interpolateHcl interpolateRgb

    xAxis = d3.axisBottom(x)
        .ticks(8)
        .tickFormat(d3.format(".2s"));


    bar = bars_container
        .selectAll(".bar")
        .data(data);


    // what happens when additional data arrives...
    //bar.enter().append("rect").append("text") // Add a new rect for each new elements



    if (values.length != 0) {

        bars = bar.enter()
            .append("g")
            .attr("class", "bar")
            .merge(bar)
            .attr("transform", d => "translate(" + x(d.x0) + "," + y(d.length) + ")")
            .html("")
            .on("mouseover", showTooltip)
            .on("mousemove", moveTooltip)
            .on("mouseleave", hideTooltip);

        bars.append("rect");
        bars.append("text");

        // describe how rectangle/bin look like
        bars.selectAll("rect")
            .transition()
            .duration(1000)
            .attr("x", 1)
            .attr("width", d => { let _new = x(d.x1) - x(d.x0) - 1; return _new < 0 ? 0 : _new; })
            .attr("height", d => height - y(d.length))
            .attr("fill", d => colorScale(d.length))

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
    } else {
        bars = bar.enter()
            .merge(bar)
            .html("");
    }

    // Remove object with data
    bar.exit().remove();

    svg.selectAll("g.x.axis")
        .call(xAxis);
}
