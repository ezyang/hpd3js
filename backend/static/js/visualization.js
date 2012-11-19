d3.selection.prototype.jq = function() {return $(this.node())}

function heapgraph(container, backend, loc) {

/*
 known bugs:
  - percent -> bytes transitions preserve the "zero", which is a bit jarring
    because the scale changes
  - disappear captions when they're not around (scrubs)
  - interaction mechanism is not clear about how to interact with the scrubs
  - scrub should have two windows and zoom on the big one
      --> actually should just use the brush thing
  - in general, the backwards animations aren't as pretty
    (mostly disabled right now)
  - if you type too quickly, the animations don't complete
  - on zoom, if scrubs are narrowed, pie data is not correct
 */

// variable naming convention:
//    s = sequence (e.g. the entire "line" for a cost center)
//    d = data point (a particular (time, usage) point for a cost center)
//    a = annotation point
//    i,j,k = indexes

// ----------------------------------------------------------------------------
// some parameters
var interpol = "linear";
var showbands = 10; // how many bands to show (the last band will be "OTHER")

var margin = {top: 30, right: 200, bottom: 40, left: 50},
    width = 800 - margin.right - margin.left,
    height = 500 - margin.top - margin.bottom;

var duration = 1500,
    delay = 500;

var annotCircleR = 5;
var annotCircleUnfocusR = 2;

var origcolor = d3.scale.ordinal().range([
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", /*"#7f7f7f", */"#bcbd22", "#17becf"
]);
var undercolor = origcolor.copy();
// XXX hack
var color = function(x) {
  if (x == -1) return "#7f7f7f";
  else return undercolor(x);
}

// ----------------------------------------------------------------------------
// global widgets which are independent of data
//    and forward declarations for elements that will be defined later

var svg = container.append("svg")
    .attr("width", width + margin.right + margin.left)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

svg.append("text")
    .attr("transform", "translate("+(width+14)+","+(height+17)+")")
    .style("font-size", "12px")
    .text("seconds");

svg.append("text")
    .attr("transform", "translate(-20,0)rotate(90)")
    .style("font-size", "12px")
    .text("bytes");

var namegroup = svg.append("g");

var namebox = namegroup.append("rect")
  .attr("transform", "translate(0,-24)")
  .attr("class", "namebox")
  .style("opacity", 0)
  .attr("height", 20)
  .attr("width", width);
var namefield = namegroup.append("text")
  .attr("transform", "translate(2,-10)")
  .attr("class", "namefield");

var controls = container.append("div");
var types = controls.append("span");
types.selectAll(".graphtype")
  .data([["Stacked Graph", stackedArea],
         ["Normalized Stacked Graph", normalizedStackedArea],
         ["Overlayed Area Graph", overlappingArea]
         ])
  .enter()
  .append("span")
  .attr("class", "graphtype")
  .call(function(span) {
    span.append("input")
      .attr("type", "radio")
      .attr("name", "graphtype")
      .attr("value", function(_,i) {return i;})
      .attr("id", function(_,i) {return "graphtype" + i})
      .property("checked", function(d) {return d[1] == stackedArea ? true : false;})
      .on("change", function(d) { if (this.checked) d[1](duration); });
    span.append("label")
      .attr("for", function(_,i) {return "graphtype" + i})
      .text(function(d) {return d[0]});
  });
types.jq().buttonset();
controls.append("span").text(" ");
controls.append("button").attr("id", "backb").on("click", goback).text("Back").property("disabled", true).jq().button();
controls.append("div").call(function(span) {
  span.append("span").text("Lock scrubs together: ");
  span.append("input").attr("id", "scrublock").attr("type", "checkbox");
  span.append("span").text("Filter: ");
  span.append("input").attr("id", "filter").attr("type", "text").on("keyup", gofilter).property("disabled", false);
});

var form;     // form element used to submit annotations
var yaxisbox; // box which contains the y-axis (needs to change)
var pie;      // pie chart on bottom right

// ----------------------------------------------------------------------------
// The data types

// Haskell style type signatures for extensions for known-length lists
// [t1, t2 ...] and objects {f1 :: t1, f2 :: t2}; also we write in curried style.

// type Cid = Int
// type Tix = Int
// type Datapoint = {y :: Int}
// type Series =
//     { cid         :: Cid
//     , values      :: [Datapoint] -- index is Tix
//     , annotations :: [{tix :: Tix, text :: String}]
//     }

var symbols,   // :: [Series] -- index is Cid
    nametable, // :: [String] -- index is Cid
    timetable; // :: [Double] -- index is Tix

var slim,      // :: [Series] -- index is NOT Cid
    others;    // :: [Series] -- ditto
var otherband, old_otherband; // :: Series

var shistory = []; // :: [[Series, Series]] -- left is "active", right is "other"

var aggregate; // :: [{y :: Int}]

// ----------------------------------------------------------------------------
// Application modes

var first = true; // first render animates differently
var dragging = false;
var singleband = false;
var zooming_in = false;
var InteractEnum = {
  SELECT: 0, // we are selecting a data point to annotate
  ENTRY: 1  // we have an active form element for annotation
}
var GraphEnum = {
  STACKED:0,
  STREAM:1,
  OVERLAY:2,
  NORMALIZED:3,
  LINE:4
  }
var interactionMode = InteractEnum.SELECT;
var currGraph = GraphEnum.STACKED;
var nextGraph;

function redraw(dur) {
  if (currGraph == GraphEnum.STACKED) stackedArea(dur);
  else if (currGraph == GraphEnum.STREAM) streamgraph(dur);
  else if (currGraph == GraphEnum.OVERLAY) overlappingArea(dur);
  else if (currGraph == GraphEnum.NORMALIZED) normalizedStackedArea(dur);
}

// use these to calculate various parameters regardless of how we've scaled things
var x, y, line, xaxis, yaxis, area;


// ----------------------------------------------------------------------------
// Utility functions

// cid2name :: Cid -> String
function cid2name(cid) {
    if (cid == -1) {
        return "OTHER";
    } else {
        return nametable[cid];
    }
}

// tix2time :: Tix -> Double
function tix2time(tix) {
    return timetable[tix];
}

// sumColumns :: [{values: [{y: Int}]}] -> [{y: Int}]
function sumColumns(matrix) {
  return d3.transpose(matrix.map(function (s) {
     return s.values.map(function (d) { return d.y; });
  })).map(function (x) { return {y: d3.sum(x)}; })
}

// calcCoords :: Series -> Tix -> [Int,Int]
function calcCoords(s, tix) {
  return [x(tix2time(tix)), y((s.values[tix].y+s.values[tix].y0)/(currGraph == GraphEnum.NORMALIZED ? aggregate[tix].y : 1))]
}

// standardTranslate :: Series -> Datapoint -> String (SVG translate)
function standardTranslate(s, d) {
  var r = calcCoords(s, d.tix);
  return "translate("+r[0]+","+r[1]+")";
}

// use slim otherwise stale copies float around
// need to use efficient versions otherwise slow down

// mouseoverBand :: Cid -> IO ()
function mouseoverBand(cid) {
  if (dragging) return;
  if (!singleband) {
    namefield.text(cid2name(cid))
    namebox.style("fill", color(cid))
      .style("opacity", 0.6);
  }
  svg.selectAll(".legendrow .bgbox").filter(function(s) {return s.cid == cid}).style("opacity", 0.6);
  svg.selectAll(".legendrow .fgbox").filter(function(s) {return s.cid == cid}).style("opacity", 1.0);
  svg.selectAll(".pie").filter(function(s) {return s.data.cid == cid}).style("opacity", 1);
  svg.selectAll(".symbol").filter(function(s) {return s.cid == cid}).selectAll(".band").style("opacity", 1);
}

// mouseoutBand :: Cid -> IO ()
function mouseoutBand(cid) {
  if (dragging) return;
  if (!singleband) {
    namefield.text("")
    namebox.style("opacity", 0);
  }
  svg.selectAll(".legendrow .bgbox").filter(function(s) {return s.cid == cid}).style("opacity", 0);
  svg.selectAll(".legendrow .fgbox").filter(function(s) {return s.cid == cid}).style("opacity", 0.6);
  svg.selectAll(".pie").filter(function(s) {return s.data.cid == cid}).style("opacity", 0.6);
  svg.selectAll(".symbol").filter(function(s) {return s.cid == cid}).selectAll(".band").style("opacity", 0.6);
}

// xcoord2tix :: Int -> Tix
// reverse engineers an SVG relative pixel into a tix
function xcoord2tix(xpixel) {
  var xinv = x.invert(xpixel);
  var rightCand = d3.bisectRight(timetable, xinv);
  var leftCand = rightCand-1;
  return leftCand < 0 || tix2time(rightCand)-xinv < xinv-tix2time(leftCand) ? rightCand : leftCand;
}

function mkZoom(s) {
  return function() {
    if (shistory.length >= 1) {
      $("#filter").prop("disabled", true);
      $("#backb").button("enable");
    }
    if (s.cid >= 0) {
      shistory.push([slim,others,otherband,undercolor]);
      slim = [s];
      others = [];
      otherband = null;
      singleband = true;
      namefield.text(cid2name(s.cid))
      namebox.style("fill", color(s.cid))
        .style("opacity", 0.6);
    } else {
      zoomdown(false);
      zooming_in = true;
    }
    updateAnnotations(slim, duration);
    updateLegend(slim, duration);
    updatePie(slim, duration);
    redraw(duration);
    zooming_in = false;
  }
}

function mkAnnotInit(s) {
  return function() {
    var mouse = d3.mouse(svg.node());
    if (interactionMode == InteractEnum.SELECT) {
      updateMarker({st: MarkerEnum.FOCUSED, s: s, mouse: mouse});
      updateForm({cid: s.cid, tix: xcoord2tix(mouse[0]), s: s});
      interactionMode = InteractEnum.ENTRY;
    } else if (interactionMode == InteractEnum.ENTRY) {
      updateForm(false);
      updateMarker({st: MarkerEnum.UNFOCUSED, s: s, mouse: mouse});
      interactionMode = InteractEnum.SELECT;
    }
  };
}

// ----------------------------------------------------------------------------
// Update functions; the workhorses

function updateAnnotations(symbols, dur) {
  // JOIN
  var annotBand = svg.selectAll(".annotBand").data(symbols, function(s) {return s.cid});
  annotBand.enter().insert("g", ".marker")
    .attr("class", "annotBand");
  annotBand.each(function (s,i) {
    // JOIN
    var annot = d3.select(this).selectAll(".annot").data(s.annotations, function(a) {return a.tix;});
    // UPDATE
    annot.transition().duration(dur/2)
      .style("opacity", 1)
      .transition().duration(dur/2).delay(zooming_in ? dur/2 : 0)
      .attr("transform", function(d) { return standardTranslate(s, d); });
    // INSERT
    annot.enter().insert("g", ":first-child")
      .attr("class", "annot")
      .attr("transform", function(d) { return standardTranslate(s, d); }) // XXX bleh, shouldn't be necessary
      .style("opacity", 0)
      .call(function (g) {
        g.append("circle")
          .attr("r", annotCircleR)
          .style("fill", d3.hsl(color(s.cid)).darker())
          .on("click", mkAnnotInit(s))
        g.append("text")
          .attr("transform", function() { return "translate(9,5)" }); // slight offset
      }).
      transition().duration(dur/2).style("opacity", 1);
    // INSERT + UPDATE
    annot
      .selectAll("text").text(function (a) { return a.text; });
    // EXIT
    annot.exit().transition().duration(dur/2).style("opacity", 0).remove();
  });
  // EXIT
  annotBand.exit().remove();
}

var MarkerEnum = {
  HIDDEN: 0,
  UNFOCUSED: 1,
  FOCUSED: 2
}
function updateMarkerSelect(md) {
  if (interactionMode == InteractEnum.SELECT) updateMarker(md);
}
function updateMarker(md) {
  // JOIN (singleton)
  if (md.st != MarkerEnum.HIDDEN && (typeof md.x == "undefined" || typeof md.y == "undefined")) {
    // give it a little help
    var r = calcCoords(md.s, xcoord2tix(md.mouse[0]));
    md.x = r[0];
    md.y = r[1];
  }
  var marker = svg.selectAll(".marker").data(d3.range(1));
  marker.enter().append("circle")
    .attr("class", "marker")
    .style("stroke", "#000");
  // UPDATE
  marker
    .attr("cx", md.st == MarkerEnum.HIDDEN ? 0 : md.x)
    .attr("cy", md.st == MarkerEnum.HIDDEN ? 0 : md.y)
    .style("visibility", md.st == MarkerEnum.HIDDEN  ? "hidden" : "visible")
    .transition()
    .attr("r", md.st == MarkerEnum.FOCUSED ? 5 : 2)
    .style("stroke-width", md.st == MarkerEnum.FOCUSED ? 2 : 0)
    .style("fill",         md.st == MarkerEnum.FOCUSED ? d3.hsl(color(md.s.cid)).darker() : "#000");
}

function updateForm(fi) {
  // JOIN
  var annotForm = svg.selectAll(".annotForm").data(fi ? d3.range(1) : d3.range(0));
  var r = fi ? calcCoords(fi.s, fi.tix) : [0,0]; // hope these doesn't fuck me over too bad
  annotForm.enter().append("foreignObject")
    .attr("class", "annotForm")
    // XXX fiddly
    .attr("width", 228)
    .attr("height", 30)
    .attr("x", r[0])
    .attr("y", r[1])
    .style("opacity", 0)
    .call(function(fo) {
      var old;
      var input = fo.append("xhtml:input").attr("type", "text")
        .each(function(inp) { // not guaranteed to exist!
          d3.select(this).node().focus();
          old = fi.s.annotations.filter(function(a) {return a.tix == fi.tix});
          // use the source here
          if (old.length) { // XXX there should only be one
            d3.select(this).node().value = old[0].text;
          }
        })
        .on("keyup", function() {
          if (d3.event.keyCode == 13) { // ENTER
            var text = input.node().value;
            $.post(backend + "/annotate/" + loc, {
              f2: fi.cid,
              f3: fi.tix,
              f4: text
              }).complete(function() {
                var done = false;
                if (text.length) {
                  if (old.length) {
                    old[0].text = text;
                  } else {
                    fi.s.annotations.push({tix: fi.tix, text: text });
                  }
                } else {
                  if (old.length) {
                    i = fi.s.annotations.indexOf(old[0]);
                    fi.s.annotations.splice(i, 1);
                  }
                }
                updateAnnotations(slim);
                var r = calcCoords(fi.s, fi.tix);
                updateMarker({st: MarkerEnum.UNFOCUSED, x: r[0], y: r[1]});
                updateForm(false);
                interactionMode = InteractEnum.SELECT;
              });
            }
          })
        .on("blur", function() {
          setTimeout(function() {
            if (interactionMode != InteractEnum.SELECT) {
              updateMarker({st: MarkerEnum.HIDDEN});
              updateForm(false);
              interactionMode = InteractEnum.SELECT;
            }
          }, 200); // force it to run after
        });
      })
    .transition().duration(duration/4)
    .style("opacity", 1);
  // EXIT
  annotForm.exit().transition().duration(duration/8)
    .style("opacity", 0)
    .remove();
}

/*
function animateIncremental(duration) {
  var easing = function (t) {return 1 - (1-t)*(1-t)*(1-t);}
  var easeOver = showbands * 150;
  var delayCond = first || currGraph != GraphEnum.OVERLAY || nextGraph != GraphEnum.STACKED;
  var delayFun = function(d, i) { return delayCond ? 0 : easeOver * easing(i/showbands); };
  return [delayCond ? duration : duration/2, delayFun]
}
*/

function updateAreas(symbols, dur) {
  // JOIN
  var g = svg.selectAll(".symbolbox").data(d3.range(1));
  g.enter().insert("g", ":first-child").attr("class", "symbolbox")
  var symbol = g.selectAll(".symbol").data(symbols, function(s) {return s.cid});
  // EXIT
  symbol.exit().transition().duration(dur/2).style("opacity", 0).remove();
  // UPDATE
  symbol.transition().duration(dur/2).style("opacity", 1);
  symbol.each(function(s) {
    d3.select(this).selectAll(".band").transition().duration(dur/2).delay(zooming_in ? dur/2 : 0).attr("d", area(s.values));
  });
  // ENTER
  var enter = symbol.enter().insert("g")
    .attr("class", "symbol")
    .style("opacity", "0")
    .transition().duration(dur/2).style("opacity", 1);
  // ENTER + UPDATE
  symbol.each(function(s, i) {
    // JOIN (boring)
    var band = d3.select(this).selectAll(".band").data(d3.range(1));
    band.enter().append("path") // we want small areas to render on TOP
      .attr("class", "band")
      .style("fill", color(s.cid))
      .attr("d", area(s.values))
      .on("mouseover", function() {
        mouseoverBand(s.cid);
        if (s.cid != -1) {
          updateMarkerSelect({st: MarkerEnum.UNFOCUSED, s: s, mouse: d3.mouse(svg.node())})
        } else {
          updateMarkerSelect({st: MarkerEnum.HIDDEN})
        }
      })
      .on("mousemove", function() {
        if (s.cid != -1) {
          updateMarkerSelect({st: MarkerEnum.UNFOCUSED, s: s, mouse: d3.mouse(svg.node())})
        } else {
          updateMarkerSelect({st: MarkerEnum.HIDDEN})
        }
      })
      .on("mouseout", function() {
        mouseoutBand(s.cid);
        updateMarkerSelect({st: MarkerEnum.HIDDEN})
      })
      .on("click", s.cid != -1 ? mkAnnotInit(s) : mkZoom(s));
    band
      .style("opacity", 0.6);
  });
}

function updateLegend(symbols, dur) {
  // JOIN
  var legendrow = svg.selectAll(".legendrow").data(symbols, function(s) {return s.cid});
  // UPDATE
  legendrow
    .transition()
    .duration(dur/2)
    .style("opacity", 1)
    .attr("transform", function (s,i) { return "translate(" + (width + 20) + ", "+ (i * 22) +")" });
  // INSERT
  legendrow.enter().insert("g")
    .attr("class", "legendrow")
    .attr("transform", function (s,i) { return "translate(" + (width + 20) + ", "+ (i * 22) +")" })
    .style("opacity", 0)
    .each(function(s, i) {
      var cur = d3.select(this);
      cur.insert("rect")
        .attr("class", "bgbox")
        .attr("height", "22")
        .attr("width", margin.right-3)
        .attr("transform", "translate(-1,-1)")
      cur.insert("rect")
        .attr("class", "fgbox")
        .attr("width", "20")
        .attr("height", "20")
      cur.insert("text")
        .attr("transform", "translate(22,13)")
        .style("font-size", "12px")
        .text(cid2name(s.cid));
      cur.on("mouseover", function() {
          mouseoverBand(s.cid);
        })
        .on("mouseout", function() {
          mouseoutBand(s.cid);
        })
        .on("click", mkZoom(s))
        ;
    }).transition().duration(dur/2).style("opacity",1);
  // INSERT + UPDATE
  legendrow.selectAll(".bgbox")
    .style("fill", function(s) { return color(s.cid) })
    .style("opacity", 0);
  legendrow.selectAll(".fgbox")
    .style("fill", function (s) { return color(s.cid) })
    .style("opacity", 0.6);
  // EXIT
  legendrow.exit().transition().duration(dur/2).style("opacity", 0).remove();
}

var otherarc;
function updatePie(symbols, dur) {
  var pielayout = d3.layout.pie().value(function(s) {return s.sumUsage;}).sort(null);
  if (zooming_in) {
    pielayout.startAngle(otherarc.startAngle);
    pielayout.endAngle(otherarc.endAngle);
  }
  var inr  = margin.right/2-50;
  var outr = margin.right/2-10;
  var arc = d3.svg.arc().innerRadius(inr).outerRadius(outr);
  var random = Math.random();
  var zoomcompare = function(s) {return s.data.cid}
  var pie = svg.selectAll(".pie");
  // JOIN
  // XXX random hack
  pie = pie.data(pielayout(symbols), zoomcompare);
  // EXIT
  pie.exit().transition().duration(dur/2).attrTween("d", arcOut).style("opacity", 0).remove();
  // UPDATE
  pie.transition().duration(dur/2).delay(zooming_in ? 0 : dur/2).attrTween("d", arcTween);
  // INSERT
  var path = pie.enter().insert("path")
    .attr("class", "pie")
    .style("opacity", 0)
    .attr("fill", function(s) {return color(s.data.cid)})
    .attr("transform", "translate("+(width+margin.left+57)+","+(margin.top+margin.bottom+height-margin.right/2-50)+")")
    .each(function(s) {
        this._current = s;
        var cur = d3.select(this);
        cur.on("mouseover", function() {mouseoverBand(s.data.cid)})
           .on("mouseout", function() {mouseoutBand(s.data.cid)})
           .on("click", mkZoom(s.data));
        });
  if (!zooming_in) {
    path
      .transition().duration(dur/2)
      .attrTween("d", arcTween)
      .style("opacity", 0.6);
  } else {
    path.attr("d", arc).transition().duration(dur/2).style("opacity", 0.6);
  }

  if (zooming_in) {
    setTimeout(function() {
      pielayout.startAngle(0);
      pielayout.endAngle(2 * Math.PI);
      pie = pie.data(pielayout(symbols), zoomcompare);
      pie.transition().duration(dur/2).attrTween("d", arcTween);
      pie.each(function(s) {
        if (s.data.cid == -1) {
          otherarc = s;
        }
      });
    }, dur/2+20);
  }

  pie.each(function(s) {
    if (s.data.cid == -1) {
      otherarc = s;
    }
  });

  function arcTween(a) {
    var i = d3.interpolate(this._current, a);
    this._current = i(0);
    return function(t) {
      return arc(i(t));
    };
  }
  function arcOut(a) {
    var current = this._current;
    var i = d3.interpolate(inr, outr);
    return function(t) {
      return (d3.svg.arc().innerRadius(i(t)).outerRadius(i(t)-inr+outr))(current);
    };
  }
}

// ----------------------------------------------------------------------------
// Load the data!

function zoomdown(filtering) {
  shistory.push([slim, others, otherband, undercolor]);
  if (!filtering) {
    undercolor = origcolor.copy();
  }

  slim = others.slice(0);
  slim.sort(function(a, b) { return b.maxUsage - a.maxUsage; });

  // we can't render everything, so split it up
  var high = slim.splice(0, showbands - 1);
  old_otherband = otherband;
  if (slim.length) {
    otherband = {
      cid: -1,
      values: sumColumns(slim),
      annotations: []
    };
    otherband.maxUsage = d3.max(otherband.values, function(d) { return d.y; });
    otherband.sumUsage = d3.sum(otherband.values, function(d) { return d.y; });
    high.push(otherband);
  } else {
    otherband = null;
  }
  others = slim; // save 'em up
  slim = high;
  // other might be a big band, so re-sort!
  slim.sort(function(a, b) { return b.maxUsage - a.maxUsage; });
}

function goback() {
  if (shistory.length < 2) {
    var filter = d3.select("#filter").node();
    if (filter.value) {
      filter.value = "";
      gofilter();
    }
    $("#backb").button("disable");
    return;
  }
  var r = shistory.pop();
  if (shistory.length < 2) {
    $("#filter").prop("disabled", false);
    if (d3.select("#filter").node().value == "") {
      $("#backb").button("disable");
    }
  }
  slim = r[0];
  others = r[1];
  otherband = r[2];
  undercolor = r[3];
  singleband = false;
  namefield.text("")
  namebox.style("opacity", 0);
  updateLegend(slim, 0);
  updatePie(slim, 0);
  redraw(0);
}

function gofilter() {
  if (shistory.length >= 2) return; // only valid when back is disabled
  var filtertext = d3.select("#filter").node().value; // XXX case insensitive?
  if (filtertext != "") {
    $("#backb").button("enable");
  } else {
    $("#backb").button("disable");
  }
  others = symbols.filter(function(s) {return nametable[s.cid].search(filtertext) != -1});
  if (others.length == 0) {
    return;
  }
  zoomdown(true);
  shistory.pop();
  updateAnnotations(slim, 0);
  updateLegend(slim, 0);
  updatePie(slim, 0);
  redraw(0);
}

d3.json(backend + "/view/" + loc, function(heap) {
  symbols   = heap.data;
  timetable = heap.timetable;
  nametable = heap.nametable;

  if (!symbols.length) {
    svg.insert("text").attr("transform", "translate("+((width+margin.right+margin.left)/2-130)+","+((height+margin.top+margin.bottom)/2-20)+")").text("No data (profile is empty)");
    return;
  }

  // domains are data dependent, calculated later
  x = d3.scale.linear().range([0, width])
                       .domain([tix2time(0), tix2time(timetable.length-1)]); // XXX modify me if we do vertical scrub
  y = d3.scale.linear().range([height, 0]).domain([0,0]);

  // some generators
  line = d3.svg.line()
      .interpolate(interpol)
      .x(function(d,i) { return x(tix2time(i)); })
      .y(function(d) { return y(d.y); });

  area = d3.svg.area()
      .interpolate(interpol)
      .x(function(d,i) { return x(tix2time(i)); })
      .y(height)

  // some extra calculations
  symbols.forEach(function(s) {
    s.maxUsage = d3.max(s.values, function(d) { return d.y; });
    s.sumUsage = d3.sum(s.values, function(d) { return d.y; });
  });

  // symbols is the original data, slice is the new data; probably
  // this computation needs to be factored out...
  slim = [];
  others = symbols;
  zoomdown(false);

  // aggregate counts for pie chart
  aggregate = sumColumns(slim); // XXX eliminate me

// ----------------------------------------------------------------------------
// Build some stuff

  // Order is important!
  updateLegend(slim, 0);
  updatePie(slim);

  // some axes
  xaxis = d3.svg.axis().scale(x).orient("bottom");

  var bounds = [0, timetable.length-1];
  var moving;
  var vlines = svg.append("g").selectAll(".leyline")
    .data(bounds)
    .enter();
  function posleyline(m) {
    m.attr("x", function(d,i) {return i ? x(tix2time(d))+2 : 0})
     .attr("width", function(d,i) {return Math.max(0, i ? (width-x(tix2time(d))-2) : x(tix2time(d)))});
  }
  function postalisman(m) {
    m.attr("transform", function(d) {return "translate("+x(tix2time(d))+","+(height+30)+")";});
  }
  vlines.append("rect")
    .attr("class", "leyline")
    .attr("enable-background", "new")
    .style("fill", "#FFF")
    .style("opacity", 0.7)
    .attr("y", 0)
    .attr("height", height)
    .call(posleyline);
  vlines.append("path")
      .attr("class", "talisman")
      .style("opacity", 0.7)
      .call(postalisman)
      .attr("d", d3.svg.symbol().type("triangle-up").size(100))
      .on("dblclick", function() {
        var n = d3.select("#scrublock").node();
        n.checked = !n.checked;
      })
      .call(d3.behavior.drag()
       .on("dragstart", function(d,i) {dragging = true; moving = bounds[0] == bounds[1] ? -1 : i;})
       .on("dragend", function() {dragging = false})
       .on("drag", function() {
        var tix = xcoord2tix(d3.mouse(svg.node())[0]);
        var i = moving;
        if (d3.select("#scrublock").node().checked) {
          var width = bounds[1] - bounds[0];
          if (i == 0) {
            if (tix+width < timetable.length) {
              bounds = [tix, tix+width];
            } else {
              bounds = [timetable.length-1-width, timetable.length-1]
            }
          } else { // i == -1 OK here
            if (tix-width >= 0) {
              bounds = [tix-width, tix];
            } else {
              bounds = [0, width]
            }
          }
        } else {
          if (i == -1) {
            i = (tix > bounds[1]) ? 1 : i;
            i = (tix < bounds[0]) ? 0 : i;
            if (i == -1) return;
          }
          if (i == 0 && tix > bounds[1] || i == 1 && tix < bounds[0]) {
            return;
          } else {
            bounds[i] = tix;
          }
        }
        svg.selectAll(".talisman").data(bounds).call(postalisman);
        svg.selectAll(".leyline").data(bounds).call(posleyline);
        updatePie(slim.map(function (s) {return {cid: s.cid, sumUsage: d3.sum(s.values.slice(bounds[0], bounds[1]+1), function(d) {return d.y})}}), 0);
      }));

  svg.append("g")
    .attr("transform", "translate(0,"+height+")")
    .attr("class", "axis")
    .call(xaxis);

  yaxis = d3.svg.axis()
    .scale(y)
    .tickFormat(d3.format("s"))
    .orient("left");

  yaxisbox = svg.append("g")
    .attr("class", "axis")
    .call(yaxis);

  // alright, render time!
  setTimeout(function () { stackedArea(); first = false; }, 0);

});

function reset() {
  // reset interaction mode, not much else...
  interactionMode = InteractEnum.SELECT;
  updateMarker({st: MarkerEnum.HIDDEN});
  updateForm(false);
}

function stackedArea(duration) {
  reset();
  nextGraph = GraphEnum.STACKED;

  line
      .y(function(d) { return y(d.y0); });

  area
      .y0(function(d) { return y(d.y0); })
      .y1(function(d) { return y(d.y0 + d.y); });

  if (zooming_in && old_otherband) {
    // add an intermediate stage
    var prestack = d3.layout.stack()
        .values(function(s) { return s.values; })
        .x(function(d,i) { return tix2time(i); })
        .y(function(d) { return d.y; })
        .out(function(d, y0, y) { d.y0 = y0; })
        .offset(function() { return old_otherband.values.map(function (d) {return d.y0}); })
        .order("reverse");
    prestack(slim);
    updateAreas(slim, duration);
    updateAnnotations(slim, duration);
  }

  var stack = d3.layout.stack()
      .values(function(s) { return s.values; })
      .x(function(d,i) { return tix2time(i); })
      .y(function(d) { return d.y; })
      .out(function(d, y0, y) { d.y0 = y0; })
      .order("reverse");

  stack(slim);

  y.domain([0, d3.max(slim[0].values.map(function(d) { return d.y + d.y0; }))])
  yaxisbox.transition().duration(duration/2).delay(zooming_in ? duration/2 : 0)
    .call(yaxis);

  updateAreas(slim, duration);
  updateAnnotations(slim, duration);
  currGraph = GraphEnum.STACKED;
}

// XXX lots of duplication
function normalizedStackedArea(duration) {
  reset();
  nextGraph = GraphEnum.NORMALIZED;

  line
      .y(function(d,i) { return y(d.y0/aggregate[i].y); });

  area
      .y0(function(d,i) { return y(d.y0/aggregate[i].y); })
      .y1(function(d,i) { return y((d.y0 + d.y)/aggregate[i].y); });

  if (zooming_in && old_otherband) {
    // add an intermediate stage
    var prestack = d3.layout.stack()
        .values(function(s) { return s.values; })
        .x(function(d,i) { return tix2time(i); })
        .y(function(d) { return d.y; })
        .out(function(d, y0, y) { d.y0 = y0; })
        .offset(function() { return old_otherband.values.map(function (d) {return d.y0}); })
        .order("reverse");
    prestack(slim);
    updateAreas(slim, duration);
    updateAnnotations(slim, duration);
  }

  var stack = d3.layout.stack()
      .values(function(s) { return s.values; })
      .x(function(d,i) { return tix2time(i); })
      .y(function(d,i) { return d.y; })
      .out(function(d, y0, y) { d.y0 = y0; })
      .order("reverse");

  stack(slim);

  y
      .domain([0, d3.max(sumColumns(slim).map(function(d,i) {return d.y/aggregate[i].y}))]);
  yaxisbox.transition().duration(duration/2).delay(zooming_in ? duration/2 : 0)
      .call(d3.svg.axis().orient("left").tickFormat(d3.format("%")).scale(y));

  updateAreas(slim, duration);
  updateAnnotations(slim, duration);
  currGraph = GraphEnum.NORMALIZED;
}

function streamgraph(duration) {
  reset();
  nextGraph = GraphEnum.STREAM;
  var stack = d3.layout.stack()
      .values(function(d) { return d.values; })
      .x(function(d,i) { return tix2time(i); })
      .y(function(d) { return d.y; })
      .out(function(d, y0, y) { d.y0 = y0; })
      .order("reverse")
      .offset("wiggle");

  stack(slim);

  y.domain([0, d3.max(slim[0].values.map(function(d) { return d.y + d.y0; }))])
  yaxisbox.transition().duration(duration/2).delay(zooming_in ? duration/2 : 0)
    .call(yaxis);

  line
      .y(function(d) { return y(d.y0); });

  area
      .y0(function(d) { return y(d.y0); })
      .y1(function(d) { return y(d.y0 + d.y); });

  updateAreas(slim, duration);
  updateAnnotations(slim, duration);
  currGraph = GraphEnum.STREAM;
}

function overlappingArea(duration) {
  reset();
  nextGraph = GraphEnum.OVERLAY;
  var g = svg.selectAll(".symbol");

  if (zooming_in && old_otherband) {

    line
        .y(function(d) { return y(d.y0); });

    area
        .y0(function(d) { return y(d.y0); })
        .y1(function(d) { return y(d.y0 + d.y); });
    // Note: this is doing stacked rendering!
    var prestack = d3.layout.stack()
        .values(function(s) { return s.values; })
        .x(function(d,i) { return tix2time(i); })
        .y(function(d) { return d.y; })
        .out(function(d, y0, y) { d.y0 = y0; })
        .order("reverse");
    prestack(slim);
    updateAreas(slim, duration);
    updateAnnotations(slim, duration);
  }

  line
      .y(function(d) { return y(d.y); });

  area
      .y0(height)
      .y1(function(d) { return y(d.y); });

  y.domain([0, d3.max(slim[0].values.map(function(d) { return d.y; }))])
  yaxisbox.transition().duration(duration/2).delay(zooming_in ? duration/2 : 0)
    .call(yaxis);

  slim.forEach(function(s) {
      s.values.forEach(function(d) {d.y0 = 0;});
  });

  updateAreas(slim, duration);
  updateAnnotations(slim, duration);
  currGraph = GraphEnum.OVERLAY;
}

}
