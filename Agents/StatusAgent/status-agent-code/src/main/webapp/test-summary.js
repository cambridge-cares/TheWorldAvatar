
/**
 * Triggers on document load.
 */
function initialise() {
    let urlParams = new URLSearchParams(window.location.search);
    document.title = "Summary: " + urlParams.get("NAME");

    let titleElement = document.getElementById("title");
    titleElement.innerHTML = "<h1>Results for '" + urlParams.get("NAME") + "'</h1>";

    // Mouse over effects


    var testStubs = document.getElementsByClassName("test-result-stub");
    Array.from(testStubs).forEach(testStub => {
        const onEvent = ((evt) => testStub.style.backgroundColor = "rgb(235, 235, 235)");
        const offEvent = ((evt) => testStub.style.backgroundColor = "rgb(245, 245, 245)");
        testStub.addEventListener("mouseover", onEvent, false);
        testStub.addEventListener("mouseout", offEvent, false)
    });

    var dashboardButtons = document.getElementsByClassName("dashboard-button");
    Array.from(dashboardButtons).forEach(button => {
        const onEvent = ((evt) => button.style.backgroundColor = "rgb(235, 235, 235)");
        const offEvent = ((evt) => button.style.backgroundColor = "rgb(245, 245, 245)");
        button.addEventListener("mouseover", onEvent, false);
        button.addEventListener("mouseout", offEvent, false)
    });

    var testButtons = document.getElementsByClassName("run-test-button");
    Array.from(testButtons).forEach(button => {
        const onEvent = ((evt) => button.style.backgroundColor = "rgb(235, 235, 235)");
        const offEvent = ((evt) => button.style.backgroundColor = "rgb(245, 245, 245)");
        button.addEventListener("mouseover", onEvent, false);
        button.addEventListener("mouseout", offEvent, false)
    });
}

function onStubClick(testStub) {
    let testName = testStub.dataset.test;
    let testType = testStub.dataset.type;
    let testTime = testStub.dataset.time;

    let winURL = window.location;
    let baseURL = winURL.protocol + "//" + winURL.host + "/" + winURL.pathname.split('/')[1];

    $.ajax({
        url: baseURL + "/dashboard",
        type: "GET",
        dataType: "html",
        data: {NAME: testName, TYPE: testType, TIME: testTime, LOG: "true"},
        success: function (data) {
            let logContainer = document.getElementsByClassName("log-container")[0];
            logContainer.innerHTML = data;
        },
        failure: function (data) {
            let logContainer = document.getElementsByClassName("log-container")[0];
            logContainer.innerHTML = "An error has occured when contacting the StatusAgent to retrieve this log. Please contact the administrator.";
        }
    })
}

function toDashboard() {
    let winURL = window.location;
    let baseURL = winURL.protocol + "//" + winURL.host + "/" + winURL.pathname.split('/')[1];

    let summaryURL = new URL(baseURL + "/dashboard");
    window.open(summaryURL.href, "_self").focus();
}

function runTest(div) {
    if (confirm("Execute this test now?")) {
        let winURL = window.location;
        let baseURL = winURL.protocol + "//" + winURL.host + "/" + winURL.pathname.split('/')[1];

        let testName = div.dataset.test;
        let testType = div.dataset.type;
        
        $.ajax({
            url: baseURL + "/submit",
            type: "GET",
            dataType: "html",
            data: {NAME: testName, TYPE: testType},
            success: function (data) {
                location.reload();
            },
            failure: function (data) {
                alert("Test could not be submitted, please contact the administrator.");
            }
        })
    }
}