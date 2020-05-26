var $messages = $('.messages-content'),
    d, h, m,
    i = 0;

$(window).load(function() {
    google.charts.load('current', {'packages':['table']});
    $messages.mCustomScrollbar();
 /*   setTimeout(function() {
        fakeMessage();
    }, 100); */
});

function updateScrollbar() {
    $messages.mCustomScrollbar("update").mCustomScrollbar('scrollTo', 'bottom', {
        scrollInertia: 10,
        timeout: 0
    });
}

function setDate() {
    d = new Date()
    if (m != d.getMinutes()) {
        m = d.getMinutes();
        $('<div class="timestamp">' + d.getHours() + ':' + m + '</div>').appendTo($('.message:last'));
    }
}

function insertMessage() {
    msg = $('.message-input').val();
    if ($.trim(msg) == '') {
        return false;
    }
    $('<div class="message message-personal">' + msg + '</div>').appendTo($('.mCSB_container')).addClass('new');
    setDate();

    // =========================
    msg = $('.message-input').val();
    $('.message-input').val(null);
    // =========================
    msg = msg.replace(/[/+]/g, 'add_sign')

    $.get("http://127.0.0.1:5000/test?question=" + msg, function( data ) {

        construct_response(data)

    });


    updateScrollbar();
    $('<div class="message loading new"><figure class="avatar"><img src="static/bat.png" /></figure><span></span></div>').appendTo($('.mCSB_container'));
    updateScrollbar();


}

$('.message-submit').click(function() {
    insertMessage();
});

$(window).on('keydown', function(e) {
    if (e.which == 13) {
        insertMessage();
        return false;
    }
})


function construct_response(data) {
    if ($('.message-input').val() != '') {
        msg = $('.message-input').val();
        return false;
    }
    table_rows = process_json_result(data)

    $('.message.loading').remove();
    setDate();
    updateScrollbar();
    i++;
    drawTable(table_rows)


}

function process_json_result(result){

    result = result.replace(/=\]/g, '=>').replace(/[}][\n ]+[{]/g, '},{')
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

    if ((rows.length == 1) && (col_size == 1))
    {
        $('<div class="message new"><figure class="avatar"><img src="static/bat.png" /></figure> <div id="table_div">'+ rows[0][0] +'</div> </div>').appendTo($('.mCSB_container')).addClass('new');

    }
    else{
        for (col = 0; col < col_size; col++){
        data.addColumn('string', variables[col]);
        console.log('added col' + variables[col])

    }
         data.addRows(rows);


        $('<div class="message new"><figure class="avatar"><img src="static/bat.png" /></figure> <div class="table_div"></div> </div>').appendTo($('.mCSB_container')).addClass('new')    .ready(function () {
        table_element  =document.getElementsByClassName("table_div")[document.getElementsByClassName("table_div").length -1];

        table_element.style.color = 'black';
                var table = new google.visualization.Table(table_element);




        table.draw(data, {showRowNumber: true, width: '100%', height: '100%'});
      });;


    }



  }