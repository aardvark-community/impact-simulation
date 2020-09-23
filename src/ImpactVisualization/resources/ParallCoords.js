let margin_par = { top: 30, right: 15, bottom: 10, left: 30 },
    width_par = 395 - margin_par.left - margin_par.right,
    height_par = 220 - margin_par.top - margin_par.bottom;

let x_par = d3.scalePoint().range([0, width_par]).padding(.1),
    y_par = {},
    dragging = {};

let line = d3.line(),
    axis = d3.axisLeft(),
    background,
    foreground;

let svg_par = undefined;
let parallCoordsID = undefined;
let lastSelection = undefined;
let dimensions = undefined;
let brushed = false;

let selectionEnergy = null;
let selectionCubicRoot = null;
let selectionStrain = null;
let selectionAlphaJutzi = null;
let selectionPressure = null;

function initParallCoords(id) {

    parallCoordsID = id;

    svg_par = d3.select(id).append("div")
        .style("background-color", "#ffffff")
        .style("border", "3px solid #ffffff")
        .append("svg")
        .attr("width", width_par + margin_par.left + margin_par.right) 
        .attr("height", height_par + margin_par.top + margin_par.bottom)
        .append("g")
        .attr("transform", "translate(" + margin_par.left + "," + margin_par.top + ")");
}

function refreshPar(dataPath) {

    if (dataPath.length == 0) { return; }

    svg_par.html("");

    d3.csv(dataPath, function (error, data) {

        // Extract the list of dimensions and create a scale for each.
        x_par.domain(dimensions = d3.keys(data[0]).filter(function (d) {
            return d != "name" && (y_par[d] = d3.scaleLinear()
                .domain(d3.extent(data, function (p) { return +p[d]; }))
                .range([height_par, 0]));
        }));

        // Add grey background lines for context.
        background = svg_par.append("g")
            .attr("class", "background")
            .selectAll("path")
            .data(data)
            .enter().append("path")
            .attr("d", path);

        // Add blue foreground lines for focus.
        foreground = svg_par.append("g")
            .attr("class", "foreground")
            .selectAll("path")
            .data(data)
            .enter().append("path")
            .attr("d", path);

        // Add a group element for each dimension.
        var g = svg_par.selectAll(".dimension")
            .data(dimensions)
            .enter().append("g")
            .attr("class", "dimension")
            .attr("transform", function (d) { return "translate(" + x_par(d) + ")"; })
            .call(d3.drag()
                .subject(function (d) { return { x_par: x_par(d) }; })
                .on("start", function (d) {
                    dragging[d] = x_par(d);
                    background.attr("visibility", "hidden");
                })
                .on("drag", function (d) {
                    dragging[d] = Math.min(width_par, Math.max(0, d3.event.x));
                    foreground.attr("d", path);
                    dimensions.sort(function (a, b) { return position(a) - position(b); });
                    x_par.domain(dimensions);
                    g.attr("transform", function (d) { return "translate(" + position(d) + ")"; })
                })
                .on("end", function (d) {
                    delete dragging[d];
                    console.log("x_par: ", x_par(d));
                    transition(d3.select(this)).attr("transform", "translate(" + x_par(d) + ")");
                    transition(foreground).attr("d", path);
                    background
                        .attr("d", path)
                        .transition()
                        .delay(500)
                        .duration(0)
                        .attr("visibility", null);
                }));

        // Add an axis and title.
        g.append("g")
            .attr("class", "axis")
            .each(function (d) { d3.select(this).call(axis.scale(y_par[d])); })
            .append("text")
            .style("text-anchor", "middle")
            .attr("y", -9)
            .text(function (d) { return d; });

        // Add and store a brush for each axis.
        g.append("g")
            .attr("class", "brush")
            .each(function (d) {
                d3.select(this).call(y_par[d].brush = d3.brushY()
                    .extent([[-8, y_par[d].range()[1]], [8, y_par[d].range()[0]]])
                    .on("start", dim => brushstart(dim))
                    .on('brush', dim => brush(dim))
                    .on('end', dim => brushend(dim)));
            })
            .selectAll("rect")
            .attr("x", -8)
            .attr("width", 16);
    });

    function position(d) {
        var v = dragging[d];
        return v == null ? x_par(d) : v;
    }

    function transition(g) {
        return g.transition().duration(500);
    }

    // Returns the path for a given data point.
    function path(d) {
        return line(dimensions.map(function (p) { return [position(p), y_par[p](d[p])]; }));
    }

    function brushstart(d) {
        d3.event.sourceEvent.stopPropagation();
        brushed = false;
        const actives = [];
        
        // filter brushed extents
        svg_par.selectAll('.brush')
            .filter(function (d) {
                currentSelection = d3.brushSelection(this);

                return d3.brushSelection(this);
            })
            .each(function (d) {
                actives.push({
                    dimension: d,
                    extent: d3.brushSelection(this)
                });
            });

        // set un-brushed foreground line disappear
        foreground.style('display', function (d) {
            return actives.every(function (active) {
                const dim = active.dimension;
                return (active.extent[0] <= y_par[dim](d[dim]) && y_par[dim](d[dim]) <= active.extent[1]) || active.extent[0] == active.extent[1];
            }) ? null : 'none';
        });
    }

    function brush(dim) {
        const actives = [];
        brushed = true;

        // filter brushed extents
        svg_par.selectAll('.brush')
            .filter(function (d) {
                switch (d) {
                    case "energy":
                        selectionEnergy = d3.brushSelection(this);
                        if (dim == d) lastSelection = selectionEnergy
                        break;
                    case "cubicRoot":
                        selectionCubicRoot = d3.brushSelection(this);
                        if (dim == d) lastSelection = selectionCubicRoot
                        break;
                    case "strain":
                        selectionStrain = d3.brushSelection(this);
                        if (dim == d) lastSelection = selectionStrain
                        break;
                    case "alphaJutzi":
                        selectionAlphaJutzi = d3.brushSelection(this);
                        if (dim == d) lastSelection = selectionAlphaJutzi
                        break;
                    case "pressure":
                        selectionPressure = d3.brushSelection(this);
                        if (dim == d) lastSelection = selectionPressure
                        break;
                    default:
                        lastSelection = null;
                        break;
                }
                return d3.brushSelection(this);
            })
            .each(function (d) {
                actives.push({
                    dimension: d,
                    extent: d3.brushSelection(this)
                });
            });

        let min = lastSelection[0];
        let max = lastSelection[1];
        let lowerLimit = y_par[dim].invert(max);
        let upperLimit = y_par[dim].invert(min);

        aardvark.processEvent(parallCoordsID.id, "parallcoords", dim, lowerLimit, upperLimit, false);

        // set un-brushed foreground line disappear
        foreground.style('display', function (d) {
            return actives.every(function (active) {
                const dim = active.dimension;
                return active.extent[0] <= y_par[dim](d[dim]) && y_par[dim](d[dim]) <= active.extent[1];
            }) ? null : 'none';
        });
    }

    function brushend(d) {
        if (!brushed) {
            let extent = y_par[d].domain();

            aardvark.processEvent(parallCoordsID.id, "parallcoords", d, extent[0], extent[1], true);
        }
    }
}