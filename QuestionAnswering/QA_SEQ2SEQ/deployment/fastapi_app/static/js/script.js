/* 
------------------------------
Constants
------------------------------
*/

TWA_ABOX_IRI_PREFIXES = ["http://www.theworldavatar.com/kb/", "https://www.theworldavatar.com/kg/", "http://www.theworldavatar.com/kg/"]

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

const globalState = (function () {
    const states = {
        domain: null,
        isProcessing: false,
        chatbotLatency: null,
        err: null
    }
    const watchers = {}

    return {
        get(prop) {
            return states[prop]
        },
        set(prop, val) {
            const oldVal = states[prop]
            states[prop] = val

            if (prop in watchers) {
                watchers[prop](oldVal, val)
            }
        },
        registerWatcher(prop, watcher) {
            watchers[prop] = watcher
        }
    }
})()

/* 
------------------------------
API calls
------------------------------
*/

async function throwErrorIfNotOk(res) {
    if (!res.ok) {
        console.log(await res.text())
        throw new HttpError(res.status)
    }
    return res
}

async function fetchTranslation(question) {
    return fetch("./translate", {
        method: "POST",
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        body: JSON.stringify({ question, domain: globalState.get("domain") })
    })
        .then(throwErrorIfNotOk)
        .then(res => res.json())
}

async function fetchKgResults(domain, query) {
    return fetch("./sparql", {
        method: "POST",
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        body: JSON.stringify({ domain, query })
    })
        .then(throwErrorIfNotOk)
        .then(res => res.json())
}

/* 
------------------------------
UI Components
------------------------------
*/

const errorContainer = (function () {
    const elem = document.getElementById("error-container")

    return {
        reset() {
            elem.style.display = "none"
            globalState.set("err", null)
        },

        displayError(message) {
            elem.innerHTML = message
            elem.style.display = "block"
        }
    }
})();

globalState.registerWatcher("err", (_, newVal) => {
    if (newVal) {
        errorContainer.displayError(newVal)
    }
})

globalState.registerWatcher("domain", (_, newVal) => {
    const elem = document.getElementById("domain-select")
    if (elem) {
        elem.value = newVal;
    }
})

const inferenceMetadataCard = (function () {
    const elem = document.getElementById("infer-metadata-card")
    const inferMetadataList = document.getElementById("infer-metadata-list")

    function displayPreprocessedQuestion(question) {
        inferMetadataList.insertAdjacentHTML("beforeend", `
            <li class="list-group-item">
                <p style="margin: auto;">
                    <strong>The input query has been reformatted to the following</strong>
                </p>
                <p style="margin: auto; color: gray;">${question}</p>
            </li>`)
    }

    function displayDomainPredicted(domain) {
        inferMetadataList.insertAdjacentHTML("beforeend", `<li class="list-group-item"><p style="margin: auto;">Predicted query domain: ${domain}</p></li>`)
    }

    function formatLatency(latency) {
        if (typeof latency === "number") {
            return latency.toFixed(2)
        } else {
            return latency
        }
    }

    function getLatencyLi() {
        const optional = document.getElementById("latency-info")
        if (!optional) {
            inferMetadataList.insertAdjacentHTML("beforeend", `
                <li class="list-group-item" id="latency-info"></li>`)
            return document.getElementById("latency-info")
        } else {
            return optional
        }
    }

    function _displayLatency(id, desc, latency) {
        if (elem.style.display !== "block") {
            elem.style.display = "block"
        }
        getLatencyLi().insertAdjacentHTML("beforeend", `<p style="margin: auto;">${desc} latency: <span id="${id}">${formatLatency(latency)}</span>s.</p>`)
    }

    function _updateLatency(id, latency) {
        document.getElementById(id).innerHTML = formatLatency(latency)
    }


    return {
        reset() {
            elem.style.display = "none"
            inferMetadataList.innerHTML = ""
        },

        displayTranslationMetadata({ question, preprocessedQuestion, domain, latency }) {
            if (preprocessedQuestion !== question) {
                displayPreprocessedQuestion(preprocessedQuestion)
            }
            displayDomainPredicted(domain)
            _displayLatency("trans-latency", "Translation", latency)
        },

        displayKgExecMetadata({ latency }) {
            _displayLatency("kg-latency", "SPARQL query execution", latency)
        },

        displayLatency(id, desc, latency) {
            _displayLatency(id, desc, latency)
        },

        updateLatency(id, latency) {
            _updateLatency(id, latency)
        }
    }
})();

globalState.registerWatcher("chatbotLatency", (oldVal, newVal) => {
    if (newVal === null) {
        inferenceMetadataCard.displayLatency("chatbot-latency", desc = "Chatbot response", latency = "...")
    } else if (newVal !== oldVal) {
        inferenceMetadataCard.updateLatency("chatbot-latency", newVal)
    }
})

const sparqlContainer = (function () {
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

        render(trans_results) {
            displaySparqlQueryPredicted(trans_results["sparql"]["predicted"])
            if (trans_results["sparql"]["postprocessed"]) {
                displaySparqlQueryPostProcessed(trans_results["sparql"]["postprocessed"])
            } else {
                globalState.set("err", "The model is unable to generate a well-formed query. Please try reformulating your question.")
            }

            elem.style.display = "block"
        },
    }
})()

const kgResponseContainer = (function () {
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
            if (TWA_ABOX_IRI_PREFIXES.some(prefix => val.startsWith(prefix))) {
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

    return {
        reset() {
            kgResultsDiv.style.display = "none"
            tableContainer.innerHTML = ""
        },

        render(data) {
            if (!data) {
                globalState.set("err", "The generated SPARQL query is malformed and cannot be executed against the knowledge base.")
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
        globalState.set("chatbotLatency", null)

        function pump({ done, value }) {
            if (done) {
                // Do something with last chunk of data then exit reader
                chatbotStopAnchor.style.display = "none"
                return;
            }
            // Otherwise do something here to process current chunk
            // format:
            // data: {"content": "abc", "latency": 123}
            // data: {"content": "def", "latency": 456}
            value.split("\n").forEach(line => {
                line = line.trim();
                if (line.startsWith("data: ")) {
                    const msg = line.substring("data: ".length)
                    let datum = null
                    try {
                        datum = JSON.parse(msg)
                    } catch (err) {
                        console.log("Unexpected data received from streaming server:\n".concat(msg))
                    }

                    if (datum !== null) {
                        chatbotResponsePara.innerHTML += datum["content"]
                        if (/\s/.test(chatbotResponsePara.innerHTML.charAt(0))) {
                            chatbotResponsePara.innerHTML = chatbotResponsePara.innerHTML.trimStart()
                        }
                        globalState.set("chatbotLatency", datum["latency"])
                    }
                }
            })

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
            if (TWA_ABOX_IRI_PREFIXES.every(prefix => !binding[k]["value"].startsWith(prefix))) {
                obj[k] = binding[k]["value"]
            }
            return obj
        }, {}))

        return fetch("./chat", {
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
            chatbotStopAnchor.style.display = "inline"

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
            chatbotSpinnerSpan.style.display = "none"
        },

        hideChatbotSpinner() {
            chatbotSpinnerSpan.style.display = "none"
        }
    }
})()

const inputField = (function () {
    const elem = document.getElementById('input-field')
    const askButton = document.getElementById('ask-button')

    return {
        populateInputText(text) {
            elem.value = text
            window.scrollTo(0, 0);
        },

        addToInputText(text) {
            const startPos = elem.selectionStart
            const endPos = elem.selectionEnd
            elem.value = elem.value.substring(0, startPos) + text + elem.value.substring(endPos, elem.value.length)
            elem.selectionStart = startPos + text.length
            elem.selectionEnd = startPos + text.length
        },

        disableAsk() {
            askButton.className = "mybutton spinner"
        },

        enableAsk() {
            askButton.className = "mybutton"
        }
    }
})()

globalState.registerWatcher("isProcessing", (oldVal, newVal) => {
    if (newVal === oldVal) {
        return
    }

    if (newVal === true) {
        inputField.disableAsk()
    } else {
        inputField.enableAsk()
    }
})

async function askQuestion() {
    if (globalState.get("isProcessing")) { // No concurrent questions
        return;
    }

    document.getElementById("result-section").style.display = "none"

    const question = document.getElementById("input-field").value;
    if (question === "") {
        return;
    }

    globalState.set("isProcessing", true);

    errorContainer.reset()
    sparqlContainer.reset()
    kgResponseContainer.reset()
    chatbotResponseCard.reset()
    inferenceMetadataCard.reset()

    try {
        const trans_results = await fetchTranslation(question)
        sparqlContainer.render(trans_results)
        document.getElementById("result-section").style.display = "block"

        inferenceMetadataCard.displayTranslationMetadata({
            question,
            preprocessedQuestion: trans_results["preprocessed_question"],
            domain: trans_results["domain"],
            latency: trans_results["latency"]
        })

        const kg_results = await fetchKgResults(trans_results["domain"], trans_results["sparql"]["postprocessed"])
        kgResponseContainer.render(kg_results["data"])
        inferenceMetadataCard.displayKgExecMetadata({ latency: kg_results["latency"] })

        await chatbotResponseCard.render(question, kg_results["data"])
    } catch (error) {
        console.log(error)
        if ((error instanceof HttpError) && (error.statusCode == 500)) {
            globalState.set("err", "An internal server error is encountered. Please try again.")
        } else {
            globalState.set("err", "An unexpected error is encountered. Please report it with the following error message<br/>" + error)
        }
    } finally {
        globalState.set("isProcessing", false);
        chatbotResponseCard.hideChatbotSpinner()
    }
}

async function AdvSearch(domain) {
    if (globalState.get("isProcessing")) { // No concurrent questions
        return;
    }

    document.getElementById("result-section").style.display = "none"

    globalState.set("isProcessing", true);

    errorContainer.reset()
    sparqlContainer.reset()
    kgResponseContainer.reset()
    chatbotResponseCard.reset()
    inferenceMetadataCard.reset()

    try {
        
        const form = document.getElementById('search-form-' + domain); // Adjust selector as needed
        const formData = new FormData(form);
        const serializedData = JSON.stringify(Object.fromEntries(formData));
        document.getElementById("result-section").style.display = "block"
        const kg_results = await fetchSearchResults(serializedData, domain);
        kgResponseContainer.render(kg_results["data"])

        inferenceMetadataCard.displayKgExecMetadata({ latency: kg_results["latency"] })
    } catch (error) {
        console.log(error)
        if ((error instanceof HttpError) && (error.statusCode == 500)) {
            globalState.set("err", "An internal server error is encountered. Please try again.")
        } else {
            globalState.set("err", "An unexpected error is encountered. Please report it with the following error message<br/>" + error)
        }
    } finally {
        globalState.set("isProcessing", false);
    }
}

async function fetchSearchResults(serializedData, domain) {
    const url = new URL("./adv_search", window.location.href);
    url.searchParams.append("domain", domain); // Append domain as a query parameter

    return fetch(url, {
        method: "POST",
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        body: serializedData
    })
    .then(response => {
        if (!response.ok) {
            throw new Error('Network response was not ok');
        }
        return response.json();
    })
    .then(data => {
        console.log(data);
        return data;
    });
}
