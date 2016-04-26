// 
// STACKED BAR CHART
// 

var parseDate = d3.time.format("%m/%d/%Y").parse
var formatDate = d3.time.format("%m/%d")
// var minDate = getDate(data[0]),
    // maxDate = getDate(data[data.length-1]);

var legend_w = 100;

var margin = {top: 20, right: 20, bottom: 30, left: 60},
    w = 660 - margin.left - margin.right+legend_w,
    h= 400 - margin.top - margin.bottom;

var x = d3.scale.ordinal()
    .rangeRoundBands([0, w], .1);
// var x = d3.time.scale().range([0, width]);

var yAbsolute = d3.scale.linear() // for absolute scale
    .rangeRound([h, 0]);

var yRelative = d3.scale.linear() // for absolute scale
      .rangeRound([h, 0]);

var color = d3.scale.ordinal()
    .range(["#8e44ad","#2980b9",
              "#27ae60","#16a085",
              "#f39c12","#d35400",
              "#c0392b","#bdc3c7",
              "#7f8c8d","#2c3e50"]);

var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom");
    // .tickFormat(d3.time.format("%d"));

// OR
  // var xAxis = d3.svg.axis().scale(x)
  // .orient("bottom").tickFormat(formatDate);

var yAxisRelative = d3.svg.axis()
    .scale(yRelative)
    .orient("left")
    .tickFormat(d3.format(".1%"));

var yAxisAbsolute = d3.svg.axis()
      .scale(yAbsolute)
      .orient("left")
      .tickFormat(d3.format(".2s"));


var svg = d3.select("#chart1").append("svg")
    .attr("width", w + margin.left + margin.right+legend_w)
    .attr("height", h + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");


var dataSource = "data.csv",
dataSource2 = "data2.csv",
dataSource3 = "data3.csv";





function updateChart(sourcefile) {


d3.csv("data.csv", function(error, data) {
  color.domain(d3.keys(data[0]).filter(function(key) { return key !== "Time"; }));
  
  
  data.forEach(function(d) {
  var mytime = formatDate(parseDate(d.Time));
    var y0 = 0;
  d.terms = color.domain().map(function(name) { return {mytime:mytime, name: name, y0: y0, y1: y0 += +d[name]}; });
  
    d.total = d.terms[d.terms.length - 1].y1;// the last row  
  d.pct = [];
  
  for (var i=0;i <d.terms.length;i ++ ){
    
    var y_coordinate = +d.terms[i].y1/d.total;
      var y_h1 = (d.terms[i].y1)/d.total; 
    var y_h0 = (d.terms[i].y0)/d.total; 
    var y_pct = y_h1 - y_h0;
    d.pct.push({
      y_coordinate: y_coordinate,
      y_h1: y_h1,
      y_h0: y_h0,
      name: d.terms[i].name,
      mytime: formatDate(parseDate(d.Time)),
      y_pct: y_pct
      
    }); 
  }
  });

  


  data.sort(function(a, b) { return b.total - a.total; });  
  

  x.domain(data.map(function(d) { return formatDate(parseDate(d.Time)) }));
  yAbsolute.domain([0, d3.max(data, function(d) { return d.total; })]);//Absolute View scale 
  yRelative.domain([0,1])// Relative View domain 
 
  var absoluteView = false // define a boolean variable, true is absolute view, false is relative view
                // Initial view is absolute 

  svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + h + ")")
      .call(xAxis);


    
     
//Define the rect of Relative     


  var timeRelative = svg.selectAll(".relative")
      .data(data)
    .enter().append("g")
      .attr("class", "relative")
      .attr("transform", function(d) {    
    return "translate(" + "0 "+ ",0)"; 
  
  });
    
    
    
  timeRelative.selectAll("rect")
  .data(function(d) {
    return d.pct;     
  })
  .enter().append("rect")
  //.attr("width", width/data.length);
  .attr("width", x.rangeBand())
  .attr("y", function(d) {
    return yRelative(d.y_coordinate); 
  })
  .attr("x",function(d) {return x(d.mytime)})
  .attr("height", function(d) { 
    return yRelative(d.y_h0) - yRelative(d.y_h1); //distance 
  })
  .attr("fill", function(d){return color(d.name)})
  .attr("stroke","pink")
  .attr("stroke-width",0.2)
  .attr("id",function(d) {return d.mytime})
  .attr("class","relative")
  .attr("id",function(d) {return d.mytime})
  .style("pointer-events","all");
     
      
  timeRelative.selectAll("rect")
    .on("mouseover", function(d){
      if(!absoluteView){
        var xPos = parseFloat(d3.select(this).attr("x"));
        var yPos = parseFloat(d3.select(this).attr("y"));
        var h = parseFloat(d3.select(this).attr("height"))
                
        d3.select(this).attr("stroke","blue").attr("stroke-width",0.8);             
        
        svg.append("text")
          .attr("x",xPos)
          .attr("y",yPos +h/2)
          .attr("class","tooltip")
          .text(Math.floor(d.y_pct.toFixed(2)*100) + "% proportion of " + d.mytime );    
              
      }
    })
    .on("mouseout",function(){
      svg.select(".tooltip").remove();
      d3.select(this).attr("stroke","pink").attr("stroke-width",0.2);
                            
    })
              
      
// End of define rect of relative    
      
      
      
// define rect for absolute 

      
  var timeAbsolute= svg.selectAll(".absolute")
            .data(data)
              .enter().append("g")
              .attr("class", "absolute")
             .attr("transform", function(d) { return "translate(" + "0" + ",0)"; });
    
    
   
  timeAbsolute.selectAll("rect")
          .data(function(d) { return d.terms})
          .enter().append("rect")
            //.attr("width", width/data.length);
          .attr("width", x.rangeBand())
          .attr("y", function(d) { 
            
            return yAbsolute(d.y1); 
        })
          .attr("x",function(d) {
            return x(d.mytime)
        })
          .attr("height", function(d) { 
            return yAbsolute(d.y0) - yAbsolute(d.y1); 
            })
          .attr("fill", function(d){
            return color(d.name)
            })
        .attr("id",function(d) {
            return d.mytime
        })
        .attr("class","absolute")
        .style("pointer-events","all")
        .attr("opacity",0); // initially it is invisible, i.e. start with Absolute View 

  //define two different scales, but one of them will always be hidden.       
  svg.append("g")
    .attr("class", "y axis absolute")
    .call(yAxisAbsolute)
    .append("text")
    .attr("transform", "rotate(-90)")
    .attr("y", 6)
    .attr("dy", ".71em")
    .style("text-anchor", "end")
    .text("Count");
    
  svg.append("g")
    .attr("class", "y axis relative")
    .call(yAxisRelative)
    .append("text")
    .attr("transform", "rotate(-90)")
    .attr("y", 6)
    .attr("dy", ".71em")
    .style("text-anchor", "end")
    .text("Rate");
         
  svg.select(".y.axis.absolute").style("opacity",0);    
        
        
        // end of define absolute
    
  
// adding legend
        var legend = svg.selectAll(".legend")
                .data(color.domain().slice().reverse())
                .enter().append("g")
                  .attr("class", "legend")
                  .attr("transform", function(d, i) { return "translate(0," + i * 20 + ")"; });

     legend.append("rect")
            .attr("x", w - 18+legend_w + 20)
          .attr("width", 18)
            .attr("height", 18)
            .attr("fill", color);

     legend.append("text")
          .attr("x", w - 24+legend_w + 23)
          .attr("y", 9)
            .attr("dy", ".35em")
            .style("text-anchor", "end")
          .text(function(d) { return d; });



      
      
  var clickButton = svg.selectAll(".clickButton")
              .data([30,30])
              .enter().append("g")
              .attr("class","clickButton")
              .attr("transform","translate(0," + 180 +")");   
      
    clickButton.append("text")
                 .attr("x", w +legend_w )
            .attr("y", 50)
              .attr("dy", ".35em")
                .style("text-anchor", "end")
              .text("Switch View")
          .style("text-decoration", "underline") 
          .style("font-size", "16px")
          .attr("fill","blue")
          .attr("id","clickChangeView") ;      
    
    // start with relative view
    Transition2Relative(); 

    // Switch view on click the clickButton 
    d3.selectAll("#"+ "clickChangeView")
    .on("click",function(){
      
      if(absoluteView){ // absolute, otherwise relative 
        Transition2Relative();      
      } else {
          Transition2Absolute();        
      }
      absoluteView = !absoluteView // change the current view status    
    });
    
    function Transition2Absolute(){    
    //Currently it is Relative  
    timeRelative.selectAll("rect").transition().duration(2000).style("opacity",0);   
    timeAbsolute.selectAll("rect").transition().duration(2000).style("opacity",1);//show absolute view rectangles    
    svg.select(".y.axis.relative").transition().duration(2000).style("opacity",0);      
    svg.select(".y.axis.absolute").transition().duration(2000).style("opacity",1);// show absolute view axis 
    }
    
    function Transition2Relative(){
    //Currently it is absolute
    timeAbsolute.selectAll("rect").transition().duration(2000).attr("fill",function(d) {return  color(d.name)})
      timeAbsolute.selectAll("rect").transition().duration(2000).style("opacity",0);//show absolute view rectangles      
      timeRelative.selectAll("rect").transition().duration(2000).style("opacity",1);     
      svg.select(".y.axis.relative").transition().duration(2000).style("opacity",1);    
      svg.select(".y.axis.absolute").transition().duration(2000).style("opacity",0);// show absolute view axis      
    }
});

}


updateChart(dataSource);

//here is where you change the data..
// d3.select(#econButton).on("click", function() {
// updateChart(dataSource2)
// })