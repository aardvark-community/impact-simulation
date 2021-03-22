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
let bars_container = undefined;
let bars = undefined;
let xAxis = undefined;
let yAxis = undefined;
let lowerLimit = undefined;
let upperLimit = undefined;
let dataSet = undefined;
let chart_data = undefined;
let targetID = undefined;
let shouldUseTransition = undefined;

let svg_margin = { top: 20, right: 20, bottom: 25, left: 20 };
let svg_width = 395;
let svg_height = 220;
let bar_width = 0;
let tooltip_visible = false;

function initHisto(id) {
    color = "steelblue";

    targetID = id;
    let values = d3.range(0);

    // A formatter for counts.
    formatCount = d3.format(".2s");

    width = svg_width - svg_margin.left - svg_margin.right;
    height = svg_height - svg_margin.top - svg_margin.bottom;

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

    x = d3.scaleLinear()
        .domain([min, max])
        .range([0, width]);

    // Generate a histogram using uniformly-spaced bins.
    let data_ = d3.histogram()
        .value(d => d)
        .domain(x.domain())
        .thresholds(d3.range(x.domain()[0], x.domain()[1], (x.domain()[1] - x.domain()[0]) / bins));
    chart_data = data_(values);

    let yMax = d3.max(chart_data, function (d) { return d.length });
    let yMin = d3.min(chart_data, function (d) { return d.length });
    colorScale = d3.scaleLinear()
        .domain([yMin, yMax])
        .range(["#00c6ff", "#2b44ff"])
        .interpolate(d3.interpolateHcl);

    y = d3.scaleLinear()
        .domain([0, yMax])
        .range([height, 0]);

    xAxis = d3.axisBottom(x)
        .tickFormat(d3.format(".0s"));

    container = d3.select(id).append("div")
        .style("background-color", "#ffffff")
        .style("border", "3px solid #ffffff");

    svg = container.append("svg")
        .attr("id","histogramSvg")
        .attr("viewBox", "0 0 " + svg_width + " " + svg_height)
        .attr("width", width + svg_margin.left + svg_margin.right)
        .attr("height", height + svg_margin.top + svg_margin.bottom);

    g = svg
        .append("g")
        .attr("transform", "translate(" + svg_margin.left + "," + svg_margin.top + ")");

    clip = g.append("defs").append("svg:clipPath")
        .attr("id", "clip")
        .append("svg:rect")
        .attr("width", width)
        .attr("height", height + svg_margin.top)
        .attr("x", 0)
        .attr("y", -svg_margin.top);

    // append bars
    bars_container = g.append("g")
        .attr("class", "bars")
        .attr("clip-path", "url(#clip)");

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

    tooltip = container.append("div")
        .style("opacity", 0)
        .attr("class", "tooltip")
        .style("background-color", "black")
        .style("color", "white")
        .style("border-radius", "5px")
        .style("padding", "10px")
        .style("position", "absolute")

    // mouse move event to check for tooltip
    d3.select(".overlay").on("mousemove", overlayMouseMove);
    d3.select(".overlay").on("mouseleave", hideTooltip);

    let idleTimeout;
    function idled() { idleTimeout = null; }

    function brushed(data) {

        var extent = d3.event.selection;
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

            lowerLimit = x.invert(extent[0]);
            upperLimit = x.invert(extent[1]);

            refresh(dataSet);

            brushOverlay.call(brush.move, null);
        }

    }
}

function overlayMouseMove(event) {
    var mouseCoords = d3.mouse(this);

    var mouseX = mouseCoords[0];
    var mouseY = mouseCoords[1];

    let hovered = chart_data.filter((d, i) => {

        let x1 = x(d.x0);
        let x2 = x(d.x1);
        let y1 = y(0);
        let y2 = y(d.length);
        return x1 <= mouseX && mouseX <= x2; // && y1 <= mouseY <= y2;
    });

    if (hovered.length == 1 && !tooltip_visible) {
        showTooltip(hovered[0], mouseCoords);
    } else if (hovered.length == 1 && tooltip_visible) {
        moveTooltip(hovered[0], mouseCoords);
    }
};

function showTooltip(d, mouseCoords) {
    tooltip
        .transition()
        .duration(100)
        .style("opacity", 0.75);
    tooltip
        .html("Range: " + formatCount(d.x0) + " - " + formatCount(d.x1))
        .style("left", (mouseCoords[0] + 20 + svg_margin.left) + "px")
        .style("top", (mouseCoords[1] - 30 + svg_margin.top) + "px");

    tooltip_visible = true;
}

function moveTooltip(d, mouseCoords) {
    tooltip
        .html("Range: " + formatCount(d.x0) + " - " + formatCount(d.x1))
        .style("left", (mouseCoords[0] + 20 + svg_margin.left) + "px")
        .style("top", (mouseCoords[1] - 30 + svg_margin.top) + "px")
}

function hideTooltip(d, mouseCoords) {
    tooltip
        .transition()
        .duration(100)
        .style("opacity", 0);

    tooltip_visible = false;
}

/*
* Adding refresh method to reload new data
*/
function refresh(values) { 
    if (values.length == 0) {
        lowerLimit = undefined;
        upperLimit = undefined;
    }

    dataSet = values;

    max = upperLimit ? upperLimit : Math.ceil(d3.max(values));
    min = lowerLimit ? lowerLimit : Math.floor(d3.min(values));

    //SEND BRUSHED DATA TO APP
    if (!isNaN(min) && !isNaN(max)) {
        aardvark.processEvent(targetID.id, "brushing", min, max);
    }

    let range = max - min;
    let breite = (range / 10) < 0.5 ? 1 : Math.round(range / 10);
    if (range <= 1) {
        breite = 0.1;
    }
    let rest = range % breite;
    let toAdd = rest != 0 ? breite - rest : 0;
    let _max = max + toAdd;
    let bins = (_max - min) / breite;

    x = d3.scaleLinear()
        .domain([min, max])
        .range([0, width]).nice();

    let data_ = d3.histogram()
        .value(d => d)
        .domain(x.domain())
        .thresholds(d3.range(x.domain()[0], x.domain()[1], (x.domain()[1] - x.domain()[0]) / bins));
    chart_data = data_(values);
    console.log(chart_data);

    // Reset y domain using new data
    let yMax = d3.max(chart_data, function (d) { return d.length });
    let yMin = d3.min(chart_data, function (d) { return d.length });
    y.domain([0, yMax]);

    colorScale = d3.scaleLinear()
        .domain([yMin, yMax])
        .range(["#00c6ff", "#2b44ff"])
        .interpolate(d3.interpolateHcl);

    xAxis = d3.axisBottom(x)
        .ticks(8)
        .tickFormat(d3.format(".2s"));


    bar = bars_container
        .selectAll(".bar")
        .data(chart_data);

    if (values.length != 0) {
        bars = bar.enter()
            .append("g")
            .attr("class", "bar")
            .merge(bar)
            .attr("transform", d => "translate(" + x(d.x0) + "," + y(d.length) + ")")
            .attr("id", (d, i) => "bar-" + i)
            .html("");

        bars.append("rect");
        bars.append("text");

        let format = d3.format(",.0f");

        let transitions = shouldUseTransition; 

        if (transitions) {
            bars.selectAll("rect")
                .transition()
                .duration(1000)
                .attr("x", 1)
                .attr("width", d => { bar_width = x(d.x1) - x(d.x0) - 1; return bar_width < 0 ? 0 : bar_width; })
                .attr("height", d => height - y(d.length))
                .attr("fill", d => colorScale(d.length))
        } else {
            // describe how rectangle/bin look like
            bars.selectAll("rect")
                .attr("x", 1)
                .attr("width", d => { bar_width = x(d.x1) - x(d.x0) - 1; return bar_width < 0 ? 0 : bar_width; })
                .attr("height", d => height - y(d.length))
                .attr("fill", d => colorScale(d.length))
        }

        if (transitions) {
            bars.selectAll("text")
                .transition()
                .duration(1000)
                .attr("dy", ".75em")
                .attr("y", -12)
                .attr("x", d => (x(d.x1) - x(d.x0)) / 2)
                .attr("text-anchor", "middle")
                .text(d => format(d.length));
        } else {

            // describe how text look like
            bars.selectAll("text")
                .attr("dy", ".75em")
                .attr("y", -12)
                .attr("x", d => (x(d.x1) - x(d.x0)) / 2)
                .attr("text-anchor", "middle")
                .text(d => format(d.length));
        }


        // Changes / Animations
        bars.transition()
            .duration(1000)
            .attr("transform", d => "translate(" + x(d.x0) + "," + y(d.length) + ")");

    } else {
        bars = bar.enter()
            .merge(bar)
            .html("");
    }

    // Remove object with chart_data
    bar.exit().remove();

    svg.selectAll("g.x.axis")
        .call(xAxis);
}
