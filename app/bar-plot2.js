// !preview r2d3 data= data.frame(label = c("Austin Bergstrom Intl", "Chicago Ohare Intl", "Dallas Fort Worth Intl", "Eagle Co Rgnl", "Fort Lauderdale Hollywood Intl", "General Edward Lawrence Logan Intl"), y = c(7257, 1455, 403,  103,  182,  2000), x = c("GPT", "GPT", "GPT","GPT","GPT","GPT"), y_label = c(1, 2, 3,  4,  5,  6))

var layer_left      = 0;
    layer_left_text = 0.01;
    layer_top       = 0.03;
    layer_height    = 0.93;
    layer_width     = 0.99;

var col_left_text   = width * layer_left_text;
var number_offset   = 300; //width * 0.4;

function svg_height() {return parseInt(svg.style('height'))}
function svg_width()  {return parseInt(svg.style('width'))}

function col_top()  {return svg_height() * layer_top; }
function col_left() {return svg_width()  * layer_left;}

function actual_max() {return d3.max(data, function (d) {return d.y; }); }
function col_width()  {return (svg_width() / actual_max()) * layer_width; }
function col_heigth() {return svg_height() / data.length * layer_height; }

var bars = svg.selectAll('rect').data(data);

bars.enter().append('rect')
    .attr('width', function(d) { return d.y * col_width(); })
    .attr('height',col_heigth() * 0.9)
    .attr('y', function(d, i) { return i * col_heigth() + col_top(); })
    .attr('x', col_left())
    .attr('fill', '#99ccff')
    .attr('tip', function(d) { return (d.y * col_width()) + col_left(); })
    .attr("d", function(d) { return d.x; })
    .on("click", function(){
      Shiny.setInputValue(
        "bar_clicked", 
        d3.select(this).attr("d"),
        {priority: "event"}
      );
    })    
    .on("mouseover", function(){
        d3.select(this)
          .attr('fill', '#ffb14e');
    })
    .on("mouseout", function(){
        d3.select(this)
          .attr('fill', '#99ccff');
    });

bars.exit().remove();

bars.transition()
  .duration(500)
    .attr('width', function(d) { return d.y * col_width(); })
    .attr('height',col_heigth() * 0.9)
    .attr('y', function(d, i) { return i * col_heigth() + col_top(); })
    .attr('x', col_left())
    .attr('tip', function(d) { return (d.y * col_width()) + col_left(); });

// Identity labels

var txt = svg.selectAll('text').data(data);

txt.enter().append('text')
      .attr('x', col_left_text)
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .text(function(d) {return d.label; })
      .style('font-size', '12px') 
      .style('font-family', 'sans-serif');  
      
txt.exit().remove();

txt.transition()
  .duration(1000)
      .attr('x', col_left_text)
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .attr("d", function(d) { return d.x; })
      .style('font-size', '12px') 
      .style('font-family', 'sans-serif')
      .text(function(d) {return d.label; });  

// Numeric labels

var totals = svg.selectAll().data(data);

totals.enter().append('text')
      .attr('x', number_offset)
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .style('font-size', '12px') 
      .style('font-family', 'sans-serif')
      .text(function(d) {return d.y_label; });  
      
totals.exit().remove();

totals.transition()
  .duration(1000)
      .attr('x', function(d) { return ((d.y * col_width()) + col_left()) * 1.01; })
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .attr("d", function(d) { return d.x; })
      .text(function(d) {return d.y_label; });
