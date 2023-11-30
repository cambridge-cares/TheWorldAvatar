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

let isProcessing = false
let isShowingIRI = false
let table = null

/* 
------------------------------
Functions that manipulate UI
------------------------------
*/

function hideElems() {
    document.getElementById("preprocessed-question").style.display = "none"
    document.getElementById("query-domain").style.display = "none"
    document.getElementById("latency-info").style.display = "none";
    document.getElementById('sparql-query-predicted-container').style.display = "none";
    document.getElementById('sparql-query-postprocessed-container').style.display = "none";
    document.getElementById("error-container").style.display = "none"
    document.getElementById("results").style.display = "none"
    document.getElementById("toggle-iri").style.display = "none"
}

function displayDomainPredicted(domain) {
    document.getElementById("query-domain").innerHTML = `<p>Predicted query domain: ${domain}</p>`;
    document.getElementById('query-domain').style.display = "block";
}

function displayLatencyInfo(trans_latency, kg_latency) {
    elem = document.getElementById("latency-info")
    elem.innerHTML = `<p style="margin: auto;">Translation latency: ${trans_latency.toFixed(2)}s.</p><p style="margin: auto;">SPARQL query execution latency: ${kg_latency.toFixed(2)}s.</p>`
    elem.style.display = "block";
}

function displayPreprocessedQuestion(question) {
    elem = document.getElementById("preprocessed-question")
    elem.innerHTML = `<p style="margin: auto;"><strong>The input query has been reformatted to the following</strong></p><p style="margin: auto; color: gray;">${question}</p>`
    elem.style.display = "block";
}

function displaySparqlQueryPredicted(sparql_query) {
    document.getElementById("sparql-query-predicted").innerHTML = sparql_query
    document.getElementById('sparql-query-predicted-container').style.display = "block";
}

function displaySparqlQueryPostProcessed(sparql_query) {
    document.getElementById("sparql-query-postprocessed").innerHTML = sparql_query;
    document.getElementById('sparql-query-postprocessed-container').style.display = "block";
}

function displayResults(data) {
    if (!data) {
        displayError("The generated SPARQL query is malformed and cannot be executed against the knowledge base.")
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
    document.getElementById("table-container").innerHTML = content;
    document.getElementById("toggle-iri").style.display = "block"
    document.getElementById("results").style.display = "block"

    table = new DataTable('#results-table', {
        retrieve: true,
        scrollX: true,
    });

    isShowingIRI = true
    toggleIRIColumns()
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
    if (isProcessing) { // No concurrent questions
        return;
    }

    const question = document.getElementById("input-field").value;
    if (question === "") {
        return;
    }

    hideElems();

    isProcessing = true;
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
        if (json["question"] != json["preprocessed_question"]) {
            displayPreprocessedQuestion(json["preprocessed_question"])
        }
        displayDomainPredicted(json["domain"])
        displayLatencyInfo(json["translation_latency"], json["kg_latency"])

        displaySparqlQueryPredicted(json["sparql"]["predicted"])
        if (json["sparql"]["postprocessed"]) {
            displaySparqlQueryPostProcessed(json["sparql"]["postprocessed"])
            displayResults(json["data"])
        } else {
            displayError("The model is unable to generate a well-formed query. Please try reformulating your question.")
        }
    }).catch(error => {
        if (error instanceof HttpError) {
            if (error.statusCode == 500) {
                displayError("An internal server error is encountered. Please try again.");
            }
        }
    }).finally(() => {
        isProcessing = false;
        document.getElementById('ask-button').className = "mybutton"
    })
}

function toggleIRIColumns() {
    if (table === null) {
        return
    }

    const rowNum = table.rows().count()
    if (rowNum == 0) {
        return
    }

    isShowingIRI = !isShowingIRI
    const rowData = table.row(0).data()
    const IRIcolIdx = rowData.reduce((arr, val, idx) => {
        if (val.startsWith("http://www.theworldavatar.com/kb/")) {
            arr.push(idx)
        }
        return arr
    }, [])
    IRIcolIdx.forEach(colIdx => {
        const col = table.column(colIdx)
        col.visible(isShowingIRI)
    })

    if (isShowingIRI) {
        document.getElementById("toggle-iri").innerHTML = "Hide IRIs"
    } else {
        document.getElementById("toggle-iri").innerHTML = "Show IRIs"
    }
}