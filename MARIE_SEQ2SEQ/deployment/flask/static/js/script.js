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

const sampleQuestions = document.getElementsByClassName("sample-question");
for (let elem of sampleQuestions) {
    elem.addEventListener("click", function() {
        document.getElementById('input-field').value = elem.textContent
        window.scrollTo(0, 0);
        askQuestion();
    })
}

let is_processing = false;

function display_sparql_query(sparql_query) {
    $('#sparql-query').append(sparql_query);
    document.getElementById('sparql-query-container').style.display = "block";
}

function display_results(data) {
    if (!data) {
        // TODO
        return
    }
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
    $('#results').append(content);
}

function askQuestion() {
    if (is_processing) { // No concurrent questions
        return;
    }

    const question = $("#input-field").val();
    if (question === "") {
        return;
    }

    document.getElementById('sparql-query-container').style.display = "none";

    $('#results').empty()
    $('#sparql-query').empty()

    is_processing = true;
    document.getElementById('ask-button').className = "mybutton spinner"

    $.ajax({
        url: "/",
        type: "POST",
        data: JSON.stringify({ question }),
        contentType: "application/json; charset=utf-8",
        dataType: "json",
        success: function (response) {
            display_sparql_query(response["sparql_query"])
            display_results(response["data"])
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
