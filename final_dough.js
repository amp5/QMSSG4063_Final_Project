// 
// DOUGHNUT
// 

var dispatch = d3.dispatch("load", "candidatechange");


var groups =[
    "Economy",
    "Immigration",
    "Healthcare",
    "Military",
    "Gun_Control",
    "China",
    "Trade",
    "Race",
    "Climate_Change",
    "Religion"
    ];
// var groups = [
//   "Bernie Sanders",
//   "Hillary Clinton", 
//   "Ted Cruz", 
//   "Donald Trump"
// ];

d3.csv("data3.csv", type, function(error, candidates) {
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
  var margin = {top: 20, right: 20, bottom: 30, left: 40},
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
      .call(yAxis);

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
  var width = 200,
      height = 360,
      radius = Math.min(width, height) / 2;

  var color = d3.scale.ordinal()
      .domain(groups)
      .range(["#8e44ad","#2980b9",
              "#27ae60","#16a085",
              "#f39c12","#d35400",
              "#c0392b","#bdc3c7",
              "#7f8c8d","#2c3e50"]);

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

// Coerce population counts to numbers and compute total per candidate.
function type(d) {
  d.total = d3.sum(groups, function(k) { return d[k] = +d[k]; });
  return d;
}
