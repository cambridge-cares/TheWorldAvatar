$('document').ready(function () {
    $('#ask-button').click(function (e) {
        askQuestion(e);
    });

    $('#input-field').keypress(function (e) {
        if (e.which === 13) { //Enter key pressed
            askQuestion(e);
        }
    });
});

let is_processing = false;

function display_results(data) {
    let content = "<table class='table table-sm table-hover'><thead><tr>"

    let vars = data["head"]["vars"].slice();
    let is_var_used = new Array(vars.length).fill(false);

    if (data["results"]["bindings"].length > 0) {
        for (const key in data["results"]["bindings"][0]) {
            if (data["results"]["bindings"][0][key]["value"]) {
                is_var_used[vars.indexOf(key)] = true;
            }
        }
    }

    // remove vars with no values
    for (let i = is_var_used.length - 1; i >= 0; i--) {
        if (!is_var_used[i]) {
            vars.splice(i);
        }
    }

    vars.forEach(varname => {
        content += `<th>${varname}</th>`
    });
    content += "</tr></thead><tbody>"

    data["results"]["bindings"].forEach(valueset => {
        content += "<tr>"
        vars.forEach(varname => {
            content += `<td>${valueset[varname]["value"]}</td>`
        })
        content += "</tr>"
    })

    content += "</tbody></table>"
    $('#search-result').append(content);
}

function askQuestion() {
    if (is_processing) { // No concurrent questions
        return;
    }

    $('#search-result').empty()

    const question = $("#input-field").val();
    if (question === "") {
        return;
    }

    is_processing = true;
    console.log(is_processing)
    document.getElementById('ask-button').className = "mybutton spinner"

    $.ajax({
        url: "/",
        type: "POST",
        data: JSON.stringify({ question }),
        contentType: "application/json; charset=utf-8",
        dataType: "json",
        success: function (data) {
            display_results(data)
            is_processing = false;
            document.getElementById('ask-button').className = "mybutton"
        },
        error: function (xhr, ajaxOptions, thrownError) {
            console.log(xhr.status);
            console.log(thrownError);
            is_processing = false;
            document.getElementById('ask-button').className = "mybutton"
        }
    })
}
