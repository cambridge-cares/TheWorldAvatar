/*!
    * Start Bootstrap - Freelancer v6.0.4 (https://startbootstrap.com/themes/freelancer)
    * Copyright 2013-2020 Start Bootstrap
    * Licensed under MIT (https://github.com/StartBootstrap/startbootstrap-freelancer/blob/master/LICENSE)
    */

	$(window).on('load', function(){
	    google.charts.load('current', {'packages':['table']});
	    $('#google_result_box').hide()


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

    $('.sample-question').click(function(){
           // ask_question()
           let q =  $(this).text().replace('   ',' ');
           document.getElementById('input-field').value = q;
           $('html,body').scrollTop(0);

           ask_question()
    });



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




  function ask_question() {

    document.getElementById('search-icon').style.display = 'none';
    document.getElementById('search-spinner').style.display = 'block';

    // =========================
    msg = $('#input-field').val();
    //$('#input-field').val(null);
    // =========================
    msg = msg.replace(/[/+]/g, 'add_sign')

   // to test the code locally, the address need to be changed to  http://127.0.0.1:5000/

    address = 'http://127.0.0.1:5000/'
    cmcl_address = 'https://kg.cmclinnovations.com/'
    address = cmcl_address

    $('#google_result_box').hide()
    $.get(address + "query?question=" + msg, function( data ) {
      displayResults(data, 'jps')
    });

}



function process_json_result(result){

  // result = result.replace(/=\]/g, '=>').replace(/[}][\n ]+[{]/g, '},{')
  console.log('The request has returned a response ', result)
  result = JSON.parse(result)
  console.log('the result parsed', result, typeof(result) )


  if (result === 'Nothing'){
    console.log('Received nothing')


     address = 'http://127.0.0.1:5000/'
    cmcl_address = 'https://kg.cmclinnovations.com/'
    address = cmcl_address
    query_wolfram_alpha(address, msg);
    query_google(address, msg);
    return null
  }

  // result = JSON.parse(result)


  console.log('If it is nothing, you should not see this line')


  if (result){
      console.log('the result parsed', result, typeof(result) )
    if (typeof(result)!== 'object' && (result!== 'Nothing')){
      obj = '{"results": ' + result + '}'
      console.log(obj)
      r = JSON.parse(obj)
      console.log('the array',  r["results"])
        keys = []
        table = []
         console.log('this is a result from JPS', r)
         // get the variable names
         first_row = r["results"][0]
         head_object = {'result_id': 'index'}
         for (let head in first_row){
            head_object[head] = head
         }
         table.push(head_object)





        r["results"].forEach(function (item, index) {
         let row_object = {}


           for (let key in item) {
                 console.log(key, item[key]);
           counter = index + 1
           console.log(item, index);
           row_object['result_id'] = counter.toString()
           row_object[key] = item[key]
           }
            table.push(row_object)

        });
          console.log('------------- jps table ---------------')
          console.log(table)
    return table
  }


    if ('results' in result && !('Nothing' in result)){
    bindings = result.results.bindings;
    if (bindings.length == 0){
        // make a request to google or wolfram alpha
        return null
    }else

	{
        variables = result.head.vars
        table = []
		index_counter = 0
        bindings.forEach(function(v){
            let row = []
			let row_object = {}
			index_counter++
            row_object['result_id'] = index_counter.toString()

            if (v['oLabel']){
            			row_object['result_name'] = v['oLabel']['value']
            }

             if (v['name']){
            			row_object['result_name'] = v['name']['value']
            }


               if (v['v']){
			row_object['result_value'] = v['v']['value']
            }


			if (v['v2'])
		    {
		        row_object['result_value_2'] = v['v2']['value']
		    }

            if (v['unitLabel'])

		    {
			    row_object['result_unit'] = v['unitLabel']['value']
		    }


            table.push(row_object)
        })

		console.log('table', table)
        return table
    }
  }else{
  // get a list of variables, which is the keys
  variables = Object.keys(result[0]);
  console.log('variables', variables)
  index_counter = 0
  result.forEach(function(v){
      row = Object.values(v)
	  let row_obj = {}
	  row_obj['result_id'] = index_counter.toString()
	  row_obj['result_name'] = v
	  row_obj['result_value'] = row
      table.push(row_obj)
  })
  console.log('------------- table ---------------')
  console.log(table)
  return table
  }
  }

  else{
    // call wolfram_alpha or google
    address = 'http://127.0.0.1:5000/'
    cmcl_address = 'https://kg.cmclinnovations.com/'
    address = cmcl_address

    query_wolfram_alpha(address, msg);
    query_google(address, msg);

  }
}
// if the query to the JPS fails, the system queries both wolfram_alpha and google at the same time
function query_wolfram_alpha(address, msg){
    $.get(address + "query_wolfram?question=" + msg, function( data ) {
      displayResults(data, 'wolfram')
    });
}

function query_google(address, msg){
    // the result returned by google will be in the form of html divisions, the visualization will be different
        $.get(address + "query_google?question=" + msg, function( data ) {
            $('#google_result_box').show()


        visualize_google_result(data, 'google')
    });
}


function visualize_google_result(result){

    $("#google-results" ).html(result)

}

// TODO: query wolfram alpha and google no matter what
// TODO: Make the page Marie Curie
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
  for (col = 0; col < col_size; col++)
  {
    data.addColumn('string', variables[col]);
    console.log('added col' + variables[col])
  }

  data.addRows(rows);
  table_element = document.getElementById('table_div')
  // table_element.style.color = 'black';
  var table = new google.visualization.Table(table_element);

  var options =
  {
    showRowNumber: false,
    width: '100%',
    height: '100%',
    alternatingRowStyle: false,
    allowHtml: true,
    cssClassNames: {
      tableCell: 'cell',
      headerCell: 'headerCell'
    }
  };
  table.draw(data, options);



//  }

}

function displayResults(myData, source) {
     myData = process_json_result(myData)


  // EXTRACT VALUE FOR HTML HEADER.
  // ('Book ID', 'Book Name', 'Category' and 'Price')
  var col = [];
  for (var i = 0; i < myData.length; i++) {
      for (var key in myData[i]) {
          if (col.indexOf(key) === -1) {
              col.push(key);
          }
      }
  }

  var divContainer = document.getElementById("search-results");
  divContainer.innerHTML = "";

  var h = document.createElement("H1")                // Create a <h1> element
  h.setAttribute("id", "result");
  if (source == 'wolfram'){
    h.innerHTML = 'Results (from wolfram alpha)'
  }
  else{
    var t = document.createTextNode("Results");     // Create a text node
  h.appendChild(t);
  }

  divContainer.appendChild(h);

  // ADD JSON DATA TO THE TABLE AS ROWS.
  for (var i = 0; i < myData.length; i++) {

      var div_row = document.createElement("div");
      div_row.classList.add('div-row');

      for (var j = 0; j < col.length; j++) {
        // Create the list item:
        var div_inner = document.createElement('div');


        var data = myData[i][col[j]];





        if (data.includes('.svg') || data.includes('.png')){
//            var myImage = $('<img/>');
//            myImage.attr('src', data);

            div_inner.innerHTML = '<img src="' + data + '" style="width:250px">'


        }
        else{

                    if (data.includes('.g09') || data.includes('.xml')){
                    div_inner.innerHTML = '<a href="'+ data +'">'+ data +'</a>'
                // <a href="url">link text</a>
        }else{


                div_inner.appendChild(document.createTextNode(data));
}
        }

        // Set its contents:

        // Add it to the list:

        div_row.appendChild(div_inner);
      }
      divContainer.appendChild(div_row);
  }

  // FINALLY ADD THE NEWLY CREATED TABLE WITH JSON DATA TO A CONTAINER.

  document.getElementById('search-spinner').style.display = 'none';
  document.getElementById("search-results").style.display = "block";
  document.getElementById('search-icon').style.display = '';


};