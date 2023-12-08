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
Global variables
------------------------------
*/

let isProcessing = false

const chatbotResponseCard = {
    abortController: new AbortController(),
    streamInterrupted: false,

    // Helpers
    getChatbotResponseElem() {
        const optional = document.getElementById("chatbot-response")
        if (!optional) {
            this.initHtml()
            return document.getElementById("chatbot-response")
        } else {
            return optional
        }
    },

    getChatbotSpinner() {
        const optional = document.getElementById("chatbot-spinner")
        if (!optional) {
            this.initHtml()
            return document.getElementById("chatbot-spinner")
        } else {
            return optional
        }
    },

    // States
    reset() {
        this.abortController = new AbortController()
        this.streamInterrupted = false
    },

    // UI
    initHtml() {
        const card = document.getElementById("chatbot-response-card")
        card.style.display = "block"
        card.innerHTML = `
        <div class="card-body">
            <h5 class="card-title">Marie's response</h5>
            <div>
                <p id="chatbot-response" style="display: inline-block; margin: 0;"></p>
                <span class="spinner-grow spinner-grow-sm text-primary" role="status" id="chatbot-spinner">
                    <span class="sr-only">Loading...</span>
                </span>
            <div>
            <a class="card-link" id="chatbot-stream-interrupt" onclick="chatbotResponseCard.interruptChatbotStream()" style="cursor: pointer;">Stop</a>
        </div>`
    },

    hideChatbotSpinner() {
        this.getChatbotSpinner().style.display = "none"
    },

    async streamChatbotResponseBodyReader(reader) {
        const elem = this.getChatbotResponseElem()
    
        inferenceMetadataCard.displayLatency("chatbot-latency", desc = "Chatbot response", latency = "...")
        // read() returns a promise that resolves when a value has been received
        return reader.read().then(function pump({ done, value }) {
            if (done) {
                // Do something with last chunk of data then exit reader
                document.getElementById("chatbot-stream-interrupt").style.display = "none"
                return;
            }
            // Otherwise do something here to process current chunk
            value = value.trim()
            if (value.startsWith("data: ")) {
                value = value.substring("data: ".length)
            }
            datum = JSON.parse(value)
    
            elem.innerHTML += datum["content"]
            if (/\s/.test(elem.innerHTML.charAt(0))) {
                elem.innerHTML = elem.innerHTML.trimStart()
            }
            inferenceMetadataCard.updateLatency("chatbot-latency", datum["latency"])
    
            // Read some more, and call this function again
            if (this.streamInterrupted) {
                return reader.cancel().then(() => {
                    document.getElementById("chatbot-stream-interrupt").style.display = "none"
                })
            } else {
                return reader.read().then(pump)
            }
        });
    },

    // On-click callbaks
    interruptChatbotStream() {
        this.streamInterrupted = true
        this.abortController.abort()
        document.getElementById("chatbot-stream-interrupt").style.display = "none"
    },

    // API calls
    async fetchChatbotResponseReader(question, data) {
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
            signal: this.abortController.signal
        })
            .then(throwErrorIfNotOk)
            .then(res => res.body.pipeThrough(new TextDecoderStream()).getReader())
    }
}

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

const kgResponseContainer = {
    table: null,
    isShowingIRI: false,

    // UI
    displayKgResponse(data) {
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
    
        this.table = new DataTable('#results-table', {
            retrieve: true,
            scrollX: true,
        });
    
        this.isShowingIRI = true
        this.toggleIRIColumns()
    },

    toggleIRIColumns() {
        if (this.table === null) {
            return
        }
    
        const rowNum = this.table.rows().count()
        if (rowNum == 0) {
            return
        }
    
        this.isShowingIRI = !this.isShowingIRI
        const rowData = this.table.row(0).data()
        const IRIcolIdx = rowData.reduce((arr, val, idx) => {
            if (val.startsWith(TWA_ABOX_IRI_PREFIX)) {
                arr.push(idx)
            }
            return arr
        }, [])
        IRIcolIdx.forEach(colIdx => {
            const col = this.table.column(colIdx)
            col.visible(this.isShowingIRI)
        })
    
        if (this.isShowingIRI) {
            document.getElementById("toggle-iri").innerHTML = "Hide IRIs"
        } else {
            document.getElementById("toggle-iri").innerHTML = "Show IRIs"
        }
    },
    
    displayKgResults(json) {
        inferenceMetadataCard.displayLatency("kg-latency", "SPARQL query execution", json["latency"])
        this.displayKgResponse(json["data"])
    },

    // API calls
    async fetchKgResults(domain, sparql_query) {
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
}

const translationContainer = {
    // UI
    displaySparqlQueryPredicted(sparql_query) {
        document.getElementById("sparql-query-predicted").innerHTML = sparql_query
        document.getElementById('sparql-query-predicted-container').style.display = "block";
    },
    
    displaySparqlQueryPostProcessed(sparql_query) {
        document.getElementById("sparql-query-postprocessed").innerHTML = sparql_query;
        document.getElementById('sparql-query-postprocessed-container').style.display = "block";
    },
    
    displayTranslationResults(json) {
        if (json["question"] != json["preprocessed_question"]) {
            inferenceMetadataCard.displayPreprocessedQuestion(json["preprocessed_question"])
        }
        inferenceMetadataCard.displayDomainPredicted(json["domain"])
        inferenceMetadataCard.displayLatency("trans-latency", "Translation", json["latency"])
    
        this.displaySparqlQueryPredicted(json["sparql"]["predicted"])
        if (json["sparql"]["postprocessed"]) {
            this.displaySparqlQueryPostProcessed(json["sparql"]["postprocessed"])
        } else {
            displayError("The model is unable to generate a well-formed query. Please try reformulating your question.")
        }
    },

    // API calls
    async fetchTranslation(question) {
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
}

/* 
------------------------------
Functions that manipulate UI
------------------------------
*/

function hideElems() {
    let elemIds = ["infer-metadata-card", "chatbot-response-card", 'sparql-query-predicted-container', 'sparql-query-postprocessed-container', "error-container", "results", "toggle-iri"]
    for (const elemId of elemIds) {
        document.getElementById(elemId).style.display = "none"
    }

    elemIds = ["infer-metadata-card", "chatbot-response-card"]
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

async function askQuestion() {
    if (isProcessing) { // No concurrent questions
        return;
    }

    const question = document.getElementById("input-field").value;
    if (question === "") {
        return;
    }

    hideElems();

    isProcessing = true;
    chatbotResponseCard.reset()
    document.getElementById('ask-button').className = "mybutton spinner"

    try {
        const trans_results = await translationContainer.fetchTranslation(question)
        translationContainer.displayTranslationResults(trans_results)

        const kg_results = await kgResponseContainer.fetchKgResults(trans_results["domain"], trans_results["sparql"]["postprocessed"])
        kgResponseContainer.displayKgResults(kg_results)

        chatbotResponseCard.initHtml()
        const reader = await chatbotResponseCard.fetchChatbotResponseReader(question, kg_results["data"])
        await chatbotResponseCard.streamChatbotResponseBodyReader(reader)
    } catch (error) {
        console.log(error)
        if ((error instanceof HttpError) && (error.statusCode == 500)) {
            displayError("An internal server error is encountered. Please try again.");
        } else {
            displayError("An unexpected error is encountered. Please report it with the following error message<br/>" + error)
        }
    } finally {
        isProcessing = false;
        document.getElementById('ask-button').className = "mybutton"
        chatbotResponseCard.hideChatbotSpinner()
    }
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
