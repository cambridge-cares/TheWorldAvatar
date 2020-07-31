/*!
    * Start Bootstrap - Freelancer v6.0.4 (https://startbootstrap.com/themes/freelancer)
    * Copyright 2013-2020 Start Bootstrap
    * Licensed under MIT (https://github.com/StartBootstrap/startbootstrap-freelancer/blob/master/LICENSE)
    */
	
	$(window).on('load', function(){ 
	
	    google.charts.load('current', {'packages':['table']});

	});
	
 

    (function($) {
    "use strict"; // Start of use strict

    // Smooth scrolling using jQuery easing
    $('a.js-scroll-trigger[href*="#"]:not([href="#"])').click(function() {
      if (location.pathname.replace(/^\//, '') == this.pathname.replace(/^\//, '') && location.hostname == this.hostname) {
        var target = $(this.hash);
        target = target.length ? target : $('[name=' + this.hash.slice(1) + ']');
        if (target.length) {
          $('html, body').animate({
            scrollTop: (target.offset().top - 71)
          }, 1000, "easeInOutExpo");
          return false;
        }
      }
    });

    // Scroll to top button appear
    $(document).scroll(function() {
      var scrollDistance = $(this).scrollTop();
      if (scrollDistance > 100) {
        $('.scroll-to-top').fadeIn();
      } else {
        $('.scroll-to-top').fadeOut();
      }
    });

    // Closes responsive menu when a scroll trigger link is clicked
    $('.js-scroll-trigger').click(function() {
      $('.navbar-collapse').collapse('hide');
    });

    // Activate scrollspy to add active class to navbar items on scroll
    $('body').scrollspy({
      target: '#mainNav',
      offset: 80
    });

    // Collapse Navbar
    var navbarCollapse = function() {
      if ($("#mainNav").offset().top > 100) {
        $("#mainNav").addClass("navbar-shrink");
      } else {
        $("#mainNav").removeClass("navbar-shrink");
      }
    };
    // Collapse now if page is not at top
    navbarCollapse();
    // Collapse the navbar when page is scrolled
    $(window).scroll(navbarCollapse);

  })(jQuery);


  // End of use strict
  function ask_question() {
    //$('#progress_bar').show();

    // =========================

    data = '{"head": {"vars": ["oLabel", "v", "value", "unitLabel"]},"results": {"bindings": [{"v": {"datatype": "http://www.w3.org/2001/XMLSchema#decimal", "type": "literal", "value": "79"}, "oLabel": {"xml:lang": "en", "type": "literal", "value": "ethanol"}}, {"v": {"datatype": "http://www.w3.org/2001/XMLSchema#decimal", "type": "literal", "value": "147"}, "oLabel": {"xml:lang": "en", "type": "literal", "value": "methanol"}}, {"v": {"datatype": "http://www.w3.org/2001/XMLSchema#decimal", "type": "literal", "value": "173"}, "oLabel": {"xml:lang": "en", "type": "literal", "value": "ethanol"}}]}}'

    table_rows = process_json_result(data)
    drawTable(table_rows)
    //$('#progress_bar').hide();

}

function process_json_result(result){

  // result = result.replace(/=\]/g, '=>').replace(/[}][\n ]+[{]/g, '},{')
  console.log('The request has returned a response ', result)

  result = JSON.parse(result)

  if ('results' in result){
          bindings = result.results.bindings;
  if (bindings.length == 0){
      return null
  }else
  {
      variables = result.head.vars
      table = []
      bindings.forEach(function(v){
          let row = []
          variables.forEach(function(k){
              if(v[k]){
              value = v[k].value;
              row.push(value)
              }
              else{
                 variables = removeItemAll(variables)
              }

          })
          table.push(row)
      })
      return [variables, table]
  }
  }else{
  // get a list of variables, which is the keys
  variables = Object.keys(result[0]);
  table = []
  console.log('variables', variables)
  result.forEach(function(v){
      row = Object.values(v)
      table.push(row)
  })
  return [variables, table]
  }
}


function removeItemAll(arr, value) {
  var i = 0;
  while (i < arr.length) {
      if(arr[i] === value) {
          arr.splice(i, 1);
      } else {
          ++i;
      }
  }
  return arr;
}

function drawTable(result_array) {
  console.log('result array', result_array)
  variables = result_array[0]
  rows = result_array[1]
  console.log('rows received', rows);
  first_col = rows[0];
  col_size = first_col.length;

  console.log('col size', col_size)
  var data = new google.visualization.DataTable();

/*
  if ((rows.length == 1) && (col_size == 1))
  {
      $('#single_div').val(rows[0][0])

  } */
  //else{
      for (col = 0; col < col_size; col++){
      data.addColumn('string', variables[col]);
      console.log('added col' + variables[col])

  }

      data.addRows(rows);
      table_element = document.getElementById('table_div')
      // table_element.style.color = 'black';
      var table = new google.visualization.Table(table_element);
      table.draw(data, {showRowNumber: true, width: '100%', height: '100%'});



//  }

}