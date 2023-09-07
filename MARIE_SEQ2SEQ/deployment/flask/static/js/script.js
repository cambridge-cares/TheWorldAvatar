const sampleQuestions = document.getElementsByClassName("sample-question");
for (let elem of sampleQuestions) {
    elem.addEventListener("click", function () {
        document.getElementById('input-field').value = elem.textContent
        window.scrollTo(0, 0);
        askQuestion();
    })
}

let is_processing = false;

function display_sparql_query(sparql_query) {
    document.getElementById("sparql-query").innerHTML = sparql_query;
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

    content += "<th>#</th>"
    vars.forEach(varname => {
        content += `<th>${varname}</th>`
    });
    content += "</tr></thead><tbody>"

    data["results"]["bindings"].forEach((valueset, idx) => {
        content += `<tr><td>${idx + 1}</td>`
        vars.forEach(varname => {
            content += `<td>${valueset[varname]["value"]}</td>`
        })
        content += "</tr>"
    })

    content += "</tbody></table>"
    document.getElementById("results").innerHTML = content;
}

function askQuestion() {
    if (is_processing) { // No concurrent questions
        return;
    }

    const question = document.getElementById("input-field").value;
    if (question === "") {
        return;
    }

    document.getElementById('sparql-query-container').style.display = "none";

    document.getElementById("results").innerHTML = ""
    document.getElementById("sparql-query").innerHTML = ""

    is_processing = true;
    document.getElementById('ask-button').className = "mybutton spinner"

    fetch("/", {
        method: "POST",
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        body: JSON.stringify({ question })
    }).then(res => {
        if (!res.ok) {
            throw Error(res.status)
        }
        return res.json()
    }).then(json => {
        display_sparql_query(json["sparql_query"])
        display_results(json["data"])
    }).catch(error => {
        console.log(error)
    }).finally(() => {
        is_processing = false;
        document.getElementById('ask-button').className = "mybutton"
    })
}
