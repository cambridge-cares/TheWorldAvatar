const hour = 1000 * 60 * 60;
const day = 1000 * 60 * 60 * 24;
const full_host = 'http://www.theworldavatar.com:85';

let previous_info = '';
draw('score','line');
draw('number','pie');

setInterval(function(){ get_info(); }, 3000);

function get_info(){



    $.ajax({url: full_host + "/read_info_box"})
        .done(function (data) {
            console.log('triggered get info',data);
            let data_array = JSON.parse(data)['info'];
            let last_info = data_array[data_array.length - 1];

            if(last_info !== previous_info) {
                for (let idx in data_array) {
                    let _info = data_array[idx];
                    document.getElementById('info_box').innerText = document.getElementById('info_box').innerText + "\n" + _info + " @" + new Date() ;
                }
                previous_info = last_info;
            }
        }

    );
}

function draw(mode, chart) {
    get_all_agents(mode, function (_agents_day_data) {

        console.log('-----------------------------');
        console.log(_agents_day_data);
        console.log('-----------------------------');

        let merged_data = merge_data(_agents_day_data);
        let rows = merged_data['rows']['rows'];
        let ticks = merged_data['rows']['ticks'];
        let labels = merged_data['labels'];
        // By default, load this graph...
        if (chart === 'line') {
            make_line_chart_grouped_by_day(rows, ticks, labels, 'Daily ' + mode);
        }
        else if (chart === 'pie') {
            let pie_chart_data = prepare_data_for_pie_chart(rows,labels);
            make_pie_chart(pie_chart_data, 'market share');
        }
    });

}





function get_all_agents(mode, callback) {
    $.ajax({url: full_host + "/get_all_agents"})
        .done(function (data) {
            let agents = JSON.parse(data);
            let agents_day_data = {};

            let counter = 0;

            for (let i = 0; i < agents.length; i++) {
                let agent_address = agents[i];
                get_all_info_of_a_agent(agent_address, function (info) {

                    console.log('--------- info ----------');
                    console.log(info);
                    console.log('-------------------------');

                    let agent_iri = JSON.parse(info)[1];
                    agents_day_data[agent_iri] = process_historical_record(info, mode);
                    counter++;
                    if (counter === agents.length) {
                        callback(agents_day_data);
                    }
                });
            }


        });
}



function get_all_info_of_a_agent(agent_id, callback) {

    $.ajax(
        {
            url: full_host + "/read_record_of_agent",
            data: {
                'agent_id': agent_id
            }
        })
        .done(function (data) {
            callback(data);
        });
}

function process_historical_record(data, mode) {
    data = JSON.parse(data);
    let over_all_score = data[0];
    let agent_iri = data[1];
    let historical_record = data[2];

    let data_grouped_in_hour = {};
    let data_grouped_in_day = {};

    // console.log('overall score: ', over_all_score);
    // console.log('agent iri', agent_iri);
    // console.log('historical record', historical_record);
    // put historical data and its timestamp together

    let data_array = historical_record[0];
    let time_array = historical_record[1];

    for (let i = 0; i < data_array.length; i++) {
        let _data = data_array[i];
        let _time = time_array[i];
        let time_in_hour = Math.floor(_time / hour) * hour;
        let time_in_day = Math.floor(_time / day) * day;

        if (data_grouped_in_day.hasOwnProperty(time_in_day)) {
            data_grouped_in_day[time_in_day].push(_data);
        } else {
            data_grouped_in_day[time_in_day] = [];
            data_grouped_in_day[time_in_day].push(_data);
        }

        if (data_grouped_in_hour.hasOwnProperty(time_in_hour)) {
            data_grouped_in_hour[time_in_hour].push(_data);
        } else {
            data_grouped_in_hour[time_in_hour] = [];
            data_grouped_in_hour[time_in_hour].push(_data);


        }
    }

    let processed_data_grouped_in_day;
    if (mode === 'score') {
        processed_data_grouped_in_day = calculate_over_all_score_in_groups(data_grouped_in_day)[0];

    } else if (mode === 'number') {
        processed_data_grouped_in_day = calculate_over_all_score_in_groups(data_grouped_in_day)[1];
    }


    let processed_data_grouped_in_hour = calculate_over_all_score_in_groups(data_grouped_in_hour)[0];
    let data_for_day = format_data_for_day(processed_data_grouped_in_day);
    console.log('agent_iri: ', data_for_day);
    return data_for_day;
}


function calculate_over_all_score_in_groups(data_group) {
    let _map = {};
    let _number_map = {};
    for (let key in data_group) {
        let single_array = data_group[key];
        _map[key] = calculate_overall(single_array);
        _number_map[key] = single_array.length;
    }

    return [_map, _number_map];

}

function calculate_overall(data_array) {

    let sum = 0;
    let length = data_array.length;
    for (let i = 0; i < data_array.length; i++) {
        sum = sum + parseInt(data_array[i]);
    }

    if (length === 0) {
        return 0;
    }
    else {
        return Math.round(sum / length);
    }

}


function format_data_for_day(processed_data_grouped_in_day) {

    let rows = [];
    let ticks = [];

    let ordered = {};

    Object.keys(processed_data_grouped_in_day).sort().forEach(function (key) {
        ordered[key] = processed_data_grouped_in_day[key];
    });

    for (let day in ordered) {
        let data_in_one_day = ordered[day];
        let d = new Date(parseInt(day));
        let _array = [data_in_one_day];
        rows.push(_array);
        ticks.push(d);
    }

    return {'rows': rows, 'ticks': ticks};

//    make_line_chart_grouped_by_day(rows,ticks);
}


function merge_data(data_map_for_day) {
    let keys = [];
    for (let key in data_map_for_day) {
        keys.push(key);
    }

    let first_service = data_map_for_day[keys[0]];

    for (let i = 1; i < keys.length; i++) {
        let other_service = data_map_for_day[keys[i]];
        let rows_from_first_service = first_service['rows'];
        for (let idx = 0; idx < rows_from_first_service.length; idx++) {

            console.log('row info : ', other_service['rows']);
            console.log('idx      : ', idx);

            let value  = 0;

            if(idx >= other_service['rows'].length){
                value = other_service['rows'][0]
            }
            else{
                value = other_service['rows'][idx];
            }


            rows_from_first_service[idx].push(value[0]);
        }
    }


    for (let j = 0; j < first_service['rows'].length; j++) {
        let tick = first_service['ticks'][j];
        let row = first_service['rows'][j];
        row.unshift(tick);
    }

    return {'rows': first_service, 'labels': keys};
}


function prepare_data_for_pie_chart(rows, labels){

    let processed_rows = [['Agent', 'Total Invocations']];
    for(let i = 0 ; i < labels.length; i++){
        let label = labels[i];
        let this_row_for_label = [label];

        let _sum = 0;
        for(let j = 0; j < rows.length; j++){
            let row = rows[j];
            let _number = row[i + 1];
            _sum = _sum + _number;
        }

        this_row_for_label.push(_sum);
        processed_rows.push(this_row_for_label);
    }

    return processed_rows;

}

function make_line_chart_grouped_by_day(rows, ticks, labels, title) {

    google.charts.load('current', {'packages': ['line']});
    google.charts.setOnLoadCallback(drawChart);

    function drawChart() {

        var button = document.getElementById('change-chart');
        var chartDiv = document.getElementById('chart_div');

        var data = new google.visualization.DataTable();
        data.addColumn('date', 'Day');
        for (let idx in labels) {
            console.log('labels', labels);
            data.addColumn('number', labels[idx]);
        }

        console.log('rows received ', rows);
        data.addRows(rows);

        var materialOptions = {
            chart: {
                title: title
            },
            width: 1800,
            height: 720

        };


        function drawMaterialChart() {
            var materialChart = new google.charts.Line(chartDiv);
            materialChart.draw(data, materialOptions);
            button.innerText = 'Change to Classic';
            button.onclick = drawClassicChart;
        }

        drawMaterialChart();

    }
}


function make_pie_chart(rows, title) {

    console.log('rows received', rows);

    google.charts.load("current", {packages:["corechart"]});
    google.charts.setOnLoadCallback(drawChart);
    function drawChart() {
        var data = google.visualization.arrayToDataTable(rows);

        var options = {
            title: title,
            is3D: true,
        };

        var chart = new google.visualization.PieChart(document.getElementById('piechart_3d'));
        chart.draw(data, options);
    }
}
