// DOUGHNUT & DISPATCH

var dispatch = d3.dispatch("load", "candidatechange");

var groups =[
    "Islamic Issue",
    "Syrian Refugees",
    "Death Penalty",
    "Trans Pacific Partnership",
    "Gun Control",
    "Syria no fly zone",
    "Health Care",
    "Israel",
    "Climate Change",
    "Immigration",
    "Same sex Marriage",
    "NSA",
    "Government Spending",
    "Lobbying",
    "China"
    ];


var HC = [934, 946, 848, 1608, 9739, 10559, 8225, 2265, 1577, 989, 8888, 2767, 543, 126, 1056];
var BS = [605, 770, 728, 834, 9866, 8812, 7168, 1747, 1928, 902, 7850, 3354, 561, 76, 908];
var TC = [936, 1632, 578, 3444, 7395, 10724, 7010, 1692, 1108, 533, 4915, 1697, 710, 190, 1063];
var DT = [768, 1234, 692, 2027, 10587, 12496, 6713, 2235, 1223, 615, 5959, 1930, 711, 165, 1329];

d3.csv("new_issues.csv", type, function(error, candidates) {
  if (error) throw error;
  var candidateById = d3.map();
  candidates.forEach(function(d) { candidateById.set(d.id, d); });
  dispatch.load(candidateById);
  dispatch.candidatechange(candidateById.get("Clinton"));
});

// A drop-down menu for selecting a candidate; uses the "menu" namespace.
dispatch.on("load.menu", function(candidateById) {
  var select = d3.select("#chart2")
    .append("div")
    .append("select")
      .on("change", function() { dispatch.candidatechange(candidateById.get(this.value)); });

  select.selectAll("option")
      .data(candidateById.values())
    .enter().append("option")
      .attr("value", function(d) { return d.id; })
      .text(function(d) { return d.id; });

  dispatch.on("candidatechange.menu", function(candidate) {
    select.property("value", candidate.id);
  });
});

// A bar chart to show total population; uses the "bar" namespace.
dispatch.on("load.bar", function(candidateById) {
  var margin = {top: 30, right: 20, bottom: 30, left: 40},
      width = 80 - margin.left - margin.right,
      height = 360 - margin.top - margin.bottom;

  var y = d3.scale.linear()
      .domain([0, d3.max(candidateById.values(), function(d) { return d.total; })])
      .rangeRound([height, 0])
      .nice();

  var yAxis = d3.svg.axis()
      .scale(y)
      .orient("left")
      .tickFormat(d3.format(".2s"));

  var svg = d3.select("#chart2").append("svg")
      .attr("width", width + margin.left + margin.right)
      .attr("height", height + margin.top + margin.bottom)
    .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)
      .append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", 6)
      .attr("dy", ".71em")
      .style("text-anchor", "middle")
      .text("Count(x100)");

  var rect = svg.append("rect")
      .attr("x", 4)
      .attr("width", width - 4)
      .attr("y", height)
      .attr("height", 0)
      .style("fill", "#aaa");

  dispatch.on("candidatechange.bar", function(d) {
    rect.transition()
        .attr("y", y(d.total))
        .attr("height", y(0) - y(d.total));
  });
});

// A pie chart to show population by age group; uses the "pie" namespace.
dispatch.on("load.pie", function(candidateById) {
  var width = 300,
      height = 420,
      radius = Math.min(width, height) / 2;

  var color = d3.scale.category20b().domain(groups);

  var arc = d3.svg.arc()
      .outerRadius(radius - 10)
      .innerRadius(radius - 70);

  var pie = d3.layout.pie()
      .sort(null);

  var svg = d3.select("#chart2").append("svg")
      .attr("width", width)
      .attr("height", height)
      .append("g")
      .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

  var path = svg.selectAll("path")
      .data(groups)
      .enter().append("path")
      .style("fill", color)
      .each(function() { this._current = {startAngle: 0, endAngle: 0}; });

  dispatch.on("candidatechange.pie", function(d) {
        path.data(pie.value(function(g) { return d[g]; })(groups)).transition()
            .attrTween("d", function(d) {
                        var interpolate = d3.interpolate(this._current, d);
                        this._current = interpolate(0);
                                  return function(t) {
                                  return arc(interpolate(t));
            };
        });
  });
});

// // stacked bar chart - attempt
dispatch.on("load.chart", function(candidateById) {
  var margin = {top: 20, right: 20, bottom: 30, left: 40},
      width = 580 - margin.left - margin.right,
      height = 360 - margin.top - margin.bottom;
  var barPadding = 1;

  var color = d3.scale.category20b().domain(groups);
  
  var x = d3.scale.ordinal()
            .rangeRoundBands([0, width], .1)
            .domain(groups);

  var y = d3.scale.linear()
      .domain([0, d3.max(candidateById.values(), function(d) { return d.total; })])
      .rangeRound([height, 0])
      .nice();

  var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom");

  var yAxis = d3.svg.axis()
      .scale(y)
      .orient("left")
      .tickFormat(d3.format(".2s"));

  var svg = d3.select("#chart2").append("svg")
      .attr("width", width + margin.left + margin.right)
      .attr("height", height + margin.top + margin.bottom)
      .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis)
      .selectAll(".tick text")
      // .call(wrap, x.rangeBand())
      .style("text-anchor", "end")
      .attr("dx", "8em")
      .attr("dy", "-.55em")
      .attr("transform", function(d){ return "rotate(-45)" });

  svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)
      .append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", 6)
      .attr("dy", ".71em")
      .style("text-anchor", "end")
      .text("Count");

  var rect = svg.selectAll("rect")
   // .data(groups)
   .data(HC)
   .enter()
   .append("rect")
   .attr("class", "bar")

   .attr("height", function(d) { return console.log(y(d.y0)); y(d.y0) - y(d.y1)})
   .style("fill", color);
   // .each(function (d) { console.log(d); return d});



  dispatch.on("candidatechange.chart", function(d) {
    rect.transition()
        .duration(1000)
        .attr("y", y(d.total))
        .attr("x", function(d, i) { return i * (width / groups.length); }) 
        .attr("width", x.rangeBand())
        // .attr("height", y(0) - y(d['China']));
        // .attr("height", function(d){return y(0) - Math.round(Math.random() * 200);});
        .attr("height", function (d) { return (d / 100)});
  });
});


// Coerce tweets counts to numbers and compute total per candidate.
function type(d) {
  d.total = d3.sum(groups, function(k) { return d[k] =+d[k]; });
  return d;
}



