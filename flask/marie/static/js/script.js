/* 
------------------------------
Constants
------------------------------
*/

TWA_ABOX_IRI_PREFIX = "http://www.theworldavatar.com/kb/"

/* 
------------------------------
Custom classes
------------------------------
*/

class HttpError extends Error {
    constructor(statusCode) {
        super("HTTP error")
        this.statusCode = statusCode
    }
}

/* 
------------------------------
Global states
------------------------------
*/

const globalState = {
    isProcessing: false
}

/* 
------------------------------
API calls
------------------------------
*/

async function fetchTranslation(question) {
    return fetch("/translate", {
        method: "POST",
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        body: JSON.stringify({ question })
    })
        .then(throwErrorIfNotOk)
        .then(res => res.json())
}


async function fetchKgResults(domain, sparql_query) {
    return fetch("/kg", {
        method: "POST",
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        body: JSON.stringify({ domain, sparql_query })
    })
        .then(throwErrorIfNotOk)
        .then(res => res.json())
}

/* 
------------------------------
UI Components
------------------------------
*/

const inferenceMetadataCard = {
    // Helpers
    formatLatency(latency) {
        if (typeof latency === "number") {
            return latency.toFixed(2)
        } else {
            return latency
        }
    },

    getTransMetadataCardUl() {
        const elem = document.getElementById("infer-metadata-card")
        elem.style.display = "block"

        const ulChildren = elem.getElementsByTagName("ul")
        if (ulChildren.length === 0) {
            const ul = document.createElement("ul")
            ul.className = "list-group list-group-flush"
            elem.appendChild(ul)
            return ul
        } else {
            return ulChildren[0]
        }
    },

    getLatencyLi() {
        const optional = document.getElementById("latency-info")
        if (!optional) {
            this.getTransMetadataCardUl().insertAdjacentHTML("beforeend", `
                <li class="list-group-item" id="latency-info"></li>`)
            return document.getElementById("latency-info")
        } else {
            return optional
        }
    },

    // UI
    displayDomainPredicted(domain) {
        const ul = this.getTransMetadataCardUl()
        ul.insertAdjacentHTML("beforeend", `<li class="list-group-item"><p style="margin: auto;">Predicted query domain: ${domain}</p></li>`)
    },

    displayLatency(id, desc, latency) {
        const li = this.getLatencyLi()
        li.insertAdjacentHTML("beforeend", `<p style="margin: auto;">${desc} latency: <span id="${id}">${this.formatLatency(latency)}</span>s.</p>`)
    },

    updateLatency(id, latency) {
        document.getElementById(id).innerHTML = this.formatLatency(latency)
    },

    displayPreprocessedQuestion(question) {
        const ul = this.getTransMetadataCardUl()
        ul.insertAdjacentHTML("beforeend", `
            <li class="list-group-item">
                <p style="margin: auto;">
                    <strong>The input query has been reformatted to the following</strong>
                </p>
                <p style="margin: auto; color: gray;">${question}</p>
            </li>`)
    }
}

const sparqlContainer = (function() {
    const elem = document.getElementById("sparql-container")
    const sparqlQueryPredictedDiv = document.getElementById("sparql-query-predicted")
    const sparqlQueryPredictedContainer = document.getElementById('sparql-query-predicted-container')
    const sparqlQueryPostprocessedDiv = document.getElementById('sparql-query-postprocessed')
    const sparqlQueryPostprocessedContainer = document.getElementById('sparql-query-postprocessed-container')

    function displaySparqlQueryPredicted(sparql_query) {
        sparqlQueryPredictedDiv.innerHTML = sparql_query
        sparqlQueryPredictedContainer.style.display = "block";
    }

    function displaySparqlQueryPostProcessed(sparql_query) {
        sparqlQueryPostprocessedDiv.innerHTML = sparql_query;
        sparqlQueryPostprocessedContainer.style.display = "block";
    }

    return {
        reset() {
            elem.style.display = "none"
        },
    
        async render(trans_results) {
            if (trans_results["question"] != trans_results["preprocessed_question"]) {
                inferenceMetadataCard.displayPreprocessedQuestion(trans_results["preprocessed_question"])
            }
            inferenceMetadataCard.displayDomainPredicted(trans_results["domain"])
            inferenceMetadataCard.displayLatency("trans-latency", "Translation", trans_results["latency"])
    
            displaySparqlQueryPredicted(trans_results["sparql"]["predicted"])
            if (trans_results["sparql"]["postprocessed"]) {
                displaySparqlQueryPostProcessed(trans_results["sparql"]["postprocessed"])
            } else {
                displayError("The model is unable to generate a well-formed query. Please try reformulating your question.")
            }

            elem.style.display = "block"
        },
    }
})()

const kgResponseContainer = (function() {
    const kgResultsDiv = document.getElementById("kg-response-container")
    const tableContainer = document.getElementById("table-container")
    const toggleIriButton = document.getElementById("toggle-iri")
    
    let table = null 
    let isShowingIRI = false

    function _toggleIRIColumns() {
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
            if (val.startsWith(TWA_ABOX_IRI_PREFIX)) {
                arr.push(idx)
            }
            return arr
        }, [])
        IRIcolIdx.forEach(colIdx => {
            const col = table.column(colIdx)
            col.visible(isShowingIRI)
        })

        if (isShowingIRI) {
            toggleIriButton.innerHTML = "Hide IRIs"
        } else {
            toggleIriButton.innerHTML = "Show IRIs"
        }
    }

    function displayKgResponse(data) {
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
        tableContainer.innerHTML = content;
        kgResultsDiv.style.display = "block"

        table = new DataTable('#results-table', {
            retrieve: true,
            scrollX: true,
        });

        isShowingIRI = true
        _toggleIRIColumns()
    }

    return {
        reset() {
            kgResultsDiv.style.display = "none"
            tableContainer.innerHTML = ""
        },
    
        async render(kg_results) {
            inferenceMetadataCard.displayLatency("kg-latency", "SPARQL query execution", kg_results["latency"])
            displayKgResponse(kg_results["data"])
        },
    
        toggleIRIColumns() {
            _toggleIRIColumns()
        },
    }
})()

const chatbotResponseCard = (function () {
    const elem = document.getElementById("chatbot-response-card")
    const chatbotResponsePara = document.getElementById("chatbot-response")
    const chatbotSpinnerSpan = document.getElementById("chatbot-spinner")
    const chatbotStopAnchor = document.getElementById("chatbot-stop")

    let abortController = new AbortController()
    let streamInterrupted = false

    async function streamChatbotResponseBodyReader(reader) {
        inferenceMetadataCard.displayLatency("chatbot-latency", desc = "Chatbot response", latency = "...")

        function pump({ done, value }) {
            if (done) {
                // Do something with last chunk of data then exit reader
                chatbotStopAnchor.style.display = "none"
                return;
            }
            // Otherwise do something here to process current chunk
            value = value.trim()
            if (value.startsWith("data: ")) {
                value = value.substring("data: ".length)
            }
            datum = JSON.parse(value)

            chatbotResponsePara.innerHTML += datum["content"]
            if (/\s/.test(chatbotResponsePara.innerHTML.charAt(0))) {
                chatbotResponsePara.innerHTML = chatbotResponsePara.innerHTML.trimStart()
            }
            inferenceMetadataCard.updateLatency("chatbot-latency", datum["latency"])

            if (streamInterrupted) {
                return reader.cancel()
            } else {
                return reader.read().then(pump)
            }
        }

        return reader.read().then(pump);
    }

    // API calls
    async function fetchChatbotResponseReader(question, data) {
        const bindings = data["results"]["bindings"].map(binding => Object.keys(binding).reduce((obj, k) => {
            if (!binding[k]["value"].startsWith(TWA_ABOX_IRI_PREFIX)) {
                obj[k] = binding[k]["value"]
            }
            return obj
        }, {}))

        return fetch("/chatbot", {
            method: "POST",
            headers: {
                "Accept": "application/json",
                "Content-Type": "application/json"
            },
            body: JSON.stringify({ question, data: JSON.stringify(bindings) }),
            signal: abortController.signal
        })
            .then(throwErrorIfNotOk)
            .then(res => res.body.pipeThrough(new TextDecoderStream()).getReader())
    }

    return {
        reset() {
            elem.style.display = "none"
            chatbotResponsePara.innerHTML = ""
            chatbotSpinnerSpan.style.display = "inline-block"

            abortController = new AbortController()
            streamInterrupted = false
        },

        async render(question, data) {
            elem.style.display = "block"
            return fetchChatbotResponseReader(question, data).then(streamChatbotResponseBodyReader)
        },

        // On-click callbaks
        interruptChatbotStream() {
            streamInterrupted = true
            abortController.abort()
            this.hideChatbotSpinner()
        },

        hideChatbotSpinner() {
            chatbotSpinnerSpan.style.display = "none"
        }
    }
})()

const inputField = {
    isProcessing: false,

    populateInputText(text) {
        document.getElementById('input-field').value = text
        window.scrollTo(0, 0);
    },

    addToInputText(text) {
        document.getElementById('input-field').value += text
    },
}

async function askQuestion() {
    if (globalState.isProcessing) { // No concurrent questions
        return;
    }

    const question = document.getElementById("input-field").value;
    if (question === "") {
        return;
    }

    hideElems();

    globalState.isProcessing = true;
    sparqlContainer.reset()
    chatbotResponseCard.reset()
    document.getElementById('ask-button').className = "mybutton spinner"

    try {
        const trans_results = await fetchTranslation(question)
        await sparqlContainer.render(trans_results)

        const kg_results = await fetchKgResults(trans_results["domain"], trans_results["sparql"]["postprocessed"])
        await kgResponseContainer.render(kg_results)

        await chatbotResponseCard.render(question, kg_results["data"])
    } catch (error) {
        console.log(error)
        if ((error instanceof HttpError) && (error.statusCode == 500)) {
            displayError("An internal server error is encountered. Please try again.");
        } else {
            displayError("An unexpected error is encountered. Please report it with the following error message<br/>" + error)
        }
    } finally {
        globalState.isProcessing = false;
        document.getElementById('ask-button').className = "mybutton"
        chatbotResponseCard.hideChatbotSpinner()
    }
}

/* 
------------------------------
Functions that manipulate UI
------------------------------
*/

function hideElems() {
    let elemIds = ["infer-metadata-card", "error-container"]
    for (const elemId of elemIds) {
        document.getElementById(elemId).style.display = "none"
    }

    elemIds = ["infer-metadata-card"]
    for (const elemId of elemIds) {
        document.getElementById(elemId).innerHTML = ""
    }
}

function displayError(message) {
    elem = document.getElementById("error-container")
    elem.innerHTML = message
    elem.style.display = "block"
}

/* 
----------------------------------------
API calls
----------------------------------------
*/

async function throwErrorIfNotOk(res) {
    if (!res.ok) {
        console.log(await res.text())
        throw new HttpError(res.status)
    }
    return res
}
