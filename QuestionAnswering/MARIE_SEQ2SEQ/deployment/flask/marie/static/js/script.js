/* 
------------------------------
Custom classes
------------------------------
*/

class HttpError extends Error {
    constructor(statusCode) {
        super("HTTP Error")
        this.statusCode = statusCode
    }
}

/* 
------------------------------
Global variables
------------------------------
*/

let is_processing = false;


/* 
------------------------------
Functions that manipulate UI
------------------------------
*/

function hideElems() {
    document.getElementById("preprocessed-question").style.display = "none"
    document.getElementById("latency-info").style.display = "none";
    document.getElementById('sparql-query-container').style.display = "none";
    document.getElementById("error-container").style.display = "none"
    document.getElementById("results").style.display = "none"
}

function display_latency_info(trans_latency, kg_latency) {
    elem = document.getElementById("latency-info")
    elem.innerHTML = `<p style="margin: auto;">Translation latency: ${trans_latency.toFixed(2)}s.</p><p style="margin: auto;">SPARQL query execution latency: ${kg_latency.toFixed(2)}s.</p>`
    elem.style.display = "block";
}

function displayPreprocessedQuestion(question) {
    elem = document.getElementById("preprocessed-question")
    elem.innerHTML = `<p style="margin: auto;"><strong>The input query has been reformatted to the following</strong></p><p style="margin: auto; color: gray;">${question}</p>`
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
    let content = "<table id='results-table' class='table table-striped table-bordered' style='width: 100%;'><thead><tr>"

    let vars = data["head"]["vars"].slice();
    if (data["results"]["bindings"].length > 0) {
        vars = vars.filter(varname => varname in data["results"]["bindings"][0])
    }

    content += "<th>#</th>"
    vars.forEach(varname => {
        content += `<th>${varname}</th>`
    });
    content += "</tr></thead><tbody>"

    data["results"]["bindings"].forEach((valueset, idx) => {
        content += `<tr><td>${idx + 1}</td>`
        vars.forEach(varname => {
            if (varname in valueset) {
                content += `<td>${valueset[varname]["value"]}</td>`
            } else {
                content += "<td></td>"
            }
        })
        content += "</tr>"
    })

    content += "</tbody></table>"
    elem = document.getElementById("results")
    elem.innerHTML = content;


    elem.style.display = "block"

    // create paginated table
    new DataTable('#results-table', {
        scrollX: true
    });

}

function displayError(message) {
    elem = document.getElementById("error-container")
    elem.innerHTML = message
    elem.style.display = "block"
}


/* 
----------------------------------------
Functions that respond to onclick events
----------------------------------------
*/

function populateInputText(text) {
    document.getElementById('input-field').value = text
    window.scrollTo(0, 0);
}

function addToInputText(text) {
    document.getElementById('input-field').value += text
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
            if (json["question"] != json["preprocessed_question"]) {
                displayPreprocessedQuestion(json["preprocessed_question"])
            }
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
