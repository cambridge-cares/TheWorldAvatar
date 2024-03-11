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

async function fetchQa(question) {
    return fetch("./qa", {
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
    const elem = document.getElementById("qa-data-container")
    const tableContainer = document.getElementById("qa-results-container")
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
            elem.style.display = "none"
            tableContainer.innerHTML = ""
        },

        render(data) {
            table = renderDataTable(vars=data["vars"], bindings=data["bindings"], id="qa-results-table", containerElem=tableContainer)
            elem.style.display = "block"

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

    document.getElementById("result-section").style.display = "none"

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
        document.getElementById("result-section").style.display = "block"
        qaDataContainer.render(results["data"])
        chatbotResponseCard.render(question, results["data"])
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
