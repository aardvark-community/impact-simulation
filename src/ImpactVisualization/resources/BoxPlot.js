//let width = undefined;
//let height = undefined;
//let svg = undefined; 
//let g = undefined;
let data_sorted = undefined; 
let q1 = undefined;
let median = undefined;
let q3 = undefined;
let interQuantileRange = undefined;
//let min = undefined;
//let max = undefined;
//let y = undefined;
//let container = undefined;

// set the dimensions and margins of the graph
svg_margin = {top: 10, right: 30, bottom: 30, left: 40};
svg_width = 400
svg_height = 400 

// a few features for the box
let box_center = 200
let box_width = 100

function initBoxPlot(id){
    width = svg_width - svg_margin.left - svg_margin.right;
    height = svg_height - svg_margin.top - svg_margin.bottom;

    // Show the Y scale
    y = d3.scaleLinear()
        .domain([0,24])
        .range([height, 0]);

    container = d3.select(id).append("div")
        .style("background-color", "#ffffff")
        .style("border", "3px solid #ffffff");

    // append the svg object to the body of the page
    svg = container.append("svg")
        .attr("width", width + svg_margin.left + svg_margin.right)
        .attr("height", height + svg_margin.top + svg_margin.bottom);

    g = svg 
        .append("g")
        .attr("transform", "translate(" + svg_margin.left + "," + svg_margin.top + ")");

    svg.call(d3.axisRight(y))

        // create dummy data

    //refreshBoxPlot(data)
}


function computeStatistics(data){
    // Compute summary statistics used for the box:
    data_sorted = data.sort(d3.ascending)
    q1 = d3.quantile(data_sorted, .25)
    median = d3.quantile(data_sorted, .5)
    q3 = d3.quantile(data_sorted, .75)
    interQuantileRange = q3 - q1
    min = q1 - 1.5 * interQuantileRange
    max = q1 + 1.5 * interQuantileRange
}


function refreshBoxPlot(data){

    //computeStatistics(data)

    data = [12,19,11,13,12,22,13,4,15,16,18,19,20,12,11,9]

    data_sorted = data.sort(d3.ascending)
    q1 = d3.quantile(data_sorted, .25)
    median = d3.quantile(data_sorted, .5)
    q3 = d3.quantile(data_sorted, .75)
    interQuantileRange = q3 - q1
    min = q1 - 1.5 * interQuantileRange
    max = q1 + 1.5 * interQuantileRange

    // Show the main vertical line
    svg
    .append("line")
        .attr("x1", box_center)
        .attr("x2", box_center)
        .attr("y1", y(min) )
        .attr("y2", y(max) )
        .attr("stroke", "black")

    // Show the box
    svg
    .append("rect")
        .attr("x", box_center - box_width/2)
        .attr("y", y(q3) )
        .attr("height", (y(q1)-y(q3)) )
        .attr("width", box_width )
        .attr("stroke", "black")
        .style("fill", "#69b3a2")

    // show median, min and max horizontal lines
    svg
    .selectAll("toto")
    .data([min, median, max])
    .enter()
    .append("line")
        .attr("x1", box_center - box_width/2)
        .attr("x2", box_center + box_width/2)
        .attr("y1", function(d){ return(y(d))} )
        .attr("y2", function(d){ return(y(d))} )
        .attr("stroke", "black")
}


