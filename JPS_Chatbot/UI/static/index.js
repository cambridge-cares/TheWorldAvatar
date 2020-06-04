$(window).load(function() {
    google.charts.load('current', {'packages':['table']});
       $('#progress_bar').hide();



});


$('.link_button').click(function(){

    $('#input-field').val($(this).html().trim())
    $('#input-field').html($(this).html().trim())
    $('#input-field').attr("placeholder", $(this).html().trim())
    ask_question()
});

function ask_question() {
    $('#progress_bar').show();

    // =========================
    msg = $('#input-field').val();
    $('#input-field').val(null);
    // =========================
    msg = msg.replace(/[/+]/g, 'add_sign')

    $.get("https://kg.cmclinnovations.com/test?question=" + msg, function( data ) {
        construct_response(data)
        $('#progress_bar').hide();
    });

}


function construct_response(data) {

    table_rows = process_json_result(data)
    drawTable(table_rows)


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

    if ((rows.length == 1) && (col_size == 1))
    {
        $('#single_div').val(rows[0][0])

    }
    else{
        for (col = 0; col < col_size; col++){
        data.addColumn('string', variables[col]);
        console.log('added col' + variables[col])

    }

        data.addRows(rows);
        table_element = document.getElementById('table_div')
        // table_element.style.color = 'black';
        var table = new google.visualization.Table(table_element);
        table.draw(data, {showRowNumber: true, width: '100%', height: '100%'});



    }



  }