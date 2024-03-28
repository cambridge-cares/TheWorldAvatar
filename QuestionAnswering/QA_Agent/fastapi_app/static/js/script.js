/* 
------------------------------
Constants
------------------------------
*/

TWA_ABOX_IRI_PREFIXES = ["http://www.theworldavatar.com/kb/", "https://www.theworldavatar.com/kg/"]

/* 
------------------------------
Custom classes
------------------------------
*/

class HttpError extends Error {
    constructor(statusCode, detail = null) {
        super("HTTP error")
        this.statusCode = statusCode
        this.detail = detail
    }

    toString() {
        return `HTTPError (${this.statusCode}): ${this.detail}`
    }
}

/* 
------------------------------
Global states
------------------------------
*/

const globalState = (function () {
    const states = {
        qa_domain: "singapore",
        isProcessing: false,
        chatbotLatency: null,
        err: null
    }
    const watchers = {}

    return {
        get(key) {
            return states[key]
        },
        set(key, val) {
            const oldVal = states[key]
            states[key] = val

            if (key in watchers) {
                watchers[key](oldVal, val)
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
        let detail = null
        try {
            let resJson = await res.json()
            detail = resJson["detail"]
        } catch (_) {}
        throw new HttpError(res.status, detail)
    }
    return res
}

async function fetchQa(question) {
    return fetch("./qa", {
        method: "POST",
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        body: JSON.stringify({ question, qa_domain: globalState.get("qa_domain") })
    })
        .then(throwErrorIfNotOk)
        .then(res => res.json())
}

/* 
------------------------------
UI Components
------------------------------
*/

function renderDataTable(vars, bindings, id, containerElem) {
    let content = `<table id=${id} class='table table-striped table-bordered' style='width: 100%;'><thead><tr>`

    content += "<th>#</th>"
    vars.forEach(varname => {
        content += `<th>${varname}</th>`
    });
    content += "</tr></thead><tbody>"

    bindings.forEach((valueset, idx) => {
        content += `<tr><td>${idx + 1}</td>`
        vars.forEach(varname => {
            let cell_content = valueset[varname]
            if (typeof valueset[varname] !== "string") {
                cell_content = JSON.stringify(cell_content)
            }
            content += `<td>${cell_content}</td>`
        })
        content += "</tr>"
    })

    content += "</tbody></table>"
    containerElem.innerHTML = content;

    return new DataTable('#' + id, {
        retrieve: true,
        scrollX: true,
    });
}

function renderBootstrapTable(vars, bindings, id, containerElem) {
    let content = `<table id=${id} class='table' style='width: 100%;'><thead><tr>`

    content += "<th>#</th>"
    vars.forEach(varname => {
        content += `<th>${varname}</th>`
    });
    content += "</tr></thead><tbody>"

    bindings.forEach((valueset, idx) => {
        content += `<tr><td>${idx + 1}</td>`
        vars.forEach(varname => {
            let cell_content = valueset[varname]
            if (typeof valueset[varname] !== "string") {
                cell_content = JSON.stringify(cell_content)
            }
            content += `<td>${cell_content}</td>`
        })
        content += "</tr>"
    })

    content += "</tbody></table>"
    containerElem.innerHTML = content;
}

function renderTimeseriesGraph(title, vars, bindings, id) {
    let traces = bindings.map(binding => {
        return {
            type: "scatter",
            mode: "lines",
            name: vars.filter(key => key !== "timeseries").map(key => binding[key]).join(", "),
            x: binding["timeseries"].map(obs => obs[0]),
            y: binding["timeseries"].map(obs => obs[1]),
        }
    })

    let layout = { title }

    Plotly.newPlot(id, traces, layout)
}

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
    errorContainer.displayError(newVal)
})

const qaMetadataContainer = (function () {
    const elem = document.getElementById("qa-metadata-container")
    const tableContainer = document.getElementById("qa-steps-container")

    function displayQaSteps(steps) {
        renderBootstrapTable(vars = ["action", "arguments", "results", "latency"], bindings = steps, id = "qa-steps-table", containerElem = tableContainer)
        elem.style.display = "block"
    }

    return {
        reset() {
            elem.style.display = "none"
            tableContainer.innerHTML = ""
        },

        render(metadata) {
            displayQaSteps(metadata["steps"])
            elem.style.display = "block"
        },
    }
})()

const qaDataContainer = (function () {
    const tableContainerOuter = document.getElementById("tabular-data-container")
    const tableContainerInner = document.getElementById("tabular-data")
    const toggleIriButton = document.getElementById("toggle-iri")
    const figureContainer = document.getElementById("figure-container")

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
            tableContainerOuter.style.display = "none"
            tableContainerInner.innerHTML = ""
            figureContainer.display = "none"
            figureContainer.innerHTML = ""
        },

        render(data) {
            if (data["vars"].includes("timeseries")) {
                renderTimeseriesGraph(data["title"], data["vars"], data["bindings"], figureContainer.id)
                figureContainer.style.display = "block"
            } else {
                table = renderDataTable(vars = data["vars"], bindings = data["bindings"], id = "qa-results-table", containerElem = tableContainerInner)
                tableContainerOuter.style.display = "block"
            }

            isShowingIRI = true
            _toggleIRIColumns()
        },

        toggleIRIColumns() {
            _toggleIRIColumns()
        },
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
        const vars = data["vars"]
        const bindings = data["bindings"].map(binding => vars.reduce((obj, k) => {
            if ((typeof binding[k] !== "string") || TWA_ABOX_IRI_PREFIXES.every(prefix => !binding[k].startsWith(prefix))) {
                obj[k] = binding[k]
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

    const resultSection = document.getElementById("result-section")
    resultSection.style.display = "none"

    const question = document.getElementById("input-field").value;
    if (question === "") {
        return;
    }

    globalState.set("isProcessing", true);

    errorContainer.reset()
    qaMetadataContainer.reset()
    qaDataContainer.reset()
    chatbotResponseCard.reset()

    try {
        const results = await fetchQa(question)
        qaMetadataContainer.render(results["metadata"])
        resultSection.style.display = "block"
        qaDataContainer.render(results["data"])
        chatbotResponseCard.render(question, results["data"])
    } catch (error) {
        console.log(error.toString())
        if (error instanceof HttpError) {
            if (error.statusCode >= 500 && error.statusCode <= 599) {
                globalState.set("err", "An internal server error is encountered. Please try again.")
            } else if (error.statusCode >= 400 && error.statusCode <= 499) {
                globalState.set("err", error.detail)
            } else {
                globalState.set("err", "An unexpected error is encountered. Please report it with the following error message<br/>" + error)
            }
        } else {
            globalState.set("err", "An unexpected error is encountered. Please report it with the following error message<br/>" + error)
        }
        resultSection.style.display = "block"
    } finally {
        globalState.set("isProcessing", false);
        chatbotResponseCard.hideChatbotSpinner()
    }
}
