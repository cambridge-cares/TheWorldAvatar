const sampleQuestions = document.getElementsByClassName("sample-question");
for (let elem of sampleQuestions) {
    elem.addEventListener("click", function () {
        const question = elem.textContent.match(/\S+/g).join(" ")
        document.getElementById('input-field').value = question
        window.scrollTo(0, 0);
        askQuestion();
    })
}

let is_processing = false;

function display_latency_info(trans_latency, kg_latency) {
    elem = document.getElementById("latency-info")
    elem.innerHTML = `<p style="margin: auto;">Translation latency: ${trans_latency.toFixed(2)}s.</p><p style="margin: auto;">SPARQL query execution latency: ${kg_latency.toFixed(2)}s.</p>`
    elem.style.display = "block";
}

function displaySparqlQuery(sparql_query) {
    document.getElementById("sparql-query").innerHTML = sparql_query;
    document.getElementById('sparql-query-container').style.display = "block";
}

function displayResults(data) {
    if (!data) {
        displayError("The predicted SPARQL query is malformed and cannot be executed against the OntoSpecies knowledge graph.")
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

        // remove vars with no values
        for (let i = is_var_used.length - 1; i >= 0; i--) {
            if (!is_var_used[i]) {
                vars.splice(i);
            }
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

class HttpError extends Error {
    constructor(statusCode) {
        super("HTTP Error")
        this.statusCode = statusCode
    }
}

function hideElems() {
    document.getElementById("latency-info").style.display = "none";
    document.getElementById('sparql-query-container').style.display = "none";
    document.getElementById("error-container").style.display = "none"

    document.getElementById("results").innerHTML = ""
    document.getElementById("sparql-query").innerHTML = ""
}

function askQuestion() {
    if (is_processing) { // No concurrent questions
        return;
    }

    const question = document.getElementById("input-field").value;
    if (question === "") {
        return;
    }

    hideElems();

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
            throw new HttpError(res.status)
        }
        return res.json()
    }).then(json => {
        display_latency_info(json["translation_latency"], json["kg_latency"])
        if (json["sparql_query"]) {
            displaySparqlQuery(json["sparql_query"])
            displayResults(json["data"])
        } else {
            displayError("The given question cannot be translated to a SPARQL query. Please try to reformulate your question.")
        }
    }).catch(error => {
        if (error instanceof HttpError) {
            if (error.statusCode == 500) {
                displayError("An internal server error is encountered. Please try again.");
            }
        }
    }).finally(() => {
        is_processing = false;
        document.getElementById('ask-button').className = "mybutton"
    })
}

function displayError(message) {
    elem = document.getElementById("error-container")
    elem.innerHTML = message
    elem.style.display = "block"
}
