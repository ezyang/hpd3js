/*
var main = d3.select("#main");

var line = d3.svg.line()
      .x(function(d) { return d * 10 + 20 })
      .y(function(d,i) { return i * 10 + 20 });
main.append("svg:path")
  .data([1,5,7,2,3])
  .attr("d", line)
  .attr("class", "line")

circle.data([4,6,8]).enter().append("circle")
  .attr("cx", function(d) {return d*10+20})
  .attr("cy", function(d,i) {return i*10+20})
  .attr("r", 10)
  .attr("color", "black");
  */

// adapted off of http://bost.ocks.org/mike/path/

var n = 10,
    frandom = d3.random.normal(0, 25000000),
    random = function() {return Math.abs(frandom());};

function chart(domain, interpolation, tick) {

  var stack = d3.layout.stack()
      .values(function(d) { return d.data; })
      .x(function(d, i) { return i; }) // d[0]
      .y(function(d) { return d[1]; })
      .out(function(d, y0, y) { d.price0 = y0; })
      .order("reverse");

  //stack(symbols);
  var data = getData()[7].data;

  var margin = {top: 6, right: 0, bottom: 6, left: 40},
      width = 860 - margin.right,
      height = 300 - margin.top - margin.bottom;

  var x = d3.scale.linear()
      .domain(domain)
      .range([0, width]);

  var y = d3.scale.linear()
      .domain([0, 40324668])
      .range([height, 0]);

  var line = d3.svg.area()
      .interpolate(interpolation)
      .x(function(d, i) { return x(i); })
      .y0(y(0))
      .y1(function(d, i) { return y(d[1]); });

  // a little icky
  var svg = d3.select("body").append("p").append("svg")
      .attr("width", width + margin.left + margin.right)
      .attr("height", height + margin.top + margin.bottom)
      .style("margin-left", -margin.left + "px")
    .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  svg.append("defs").append("clipPath")
      .attr("id", "clip")
    .append("rect")
      .attr("width", width)
      .attr("height", height);

  // XXX fix me up
  svg.append("g")
      .attr("class", "y axis")
      .call(d3.svg.axis().scale(y).ticks(5).orient("left").tickFormat(d3.format("s")));

  var path = svg.append("g")
      .attr("clip-path", "url(#clip)")
    .append("path")
      .data([data]) // only reads first one
      .attr("class", "line")
      .attr("d", line);

  tick(path, line, data, x);
}
