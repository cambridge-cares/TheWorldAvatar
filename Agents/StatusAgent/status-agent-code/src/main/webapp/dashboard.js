
/**
 * Triggers on document load, adds mouse over effects on test stub elements.
 */
function initialise() {
    var testStubs = document.getElementsByClassName("test-result-stub");

    Array.from(testStubs).forEach(testStub => {
        const onEvent = ((evt) => testStub.style.backgroundColor = "rgb(235, 235, 235)");
        const offEvent = ((evt) => testStub.style.backgroundColor = "rgb(245, 245, 245)");

        testStub.addEventListener("mouseover", onEvent, false);
        testStub.addEventListener("mouseout", offEvent, false)
    });
}

/**
 * Triggers when a test stub element is clicked on.
 * 
 * @param {Element} testStub - clicked element.
 */
function onStubClick(testStub) {
    let testName = testStub.dataset.test;
    let testType = testStub.dataset.type;

    let winURL = window.location;
    let baseURL = winURL.protocol + "//" + winURL.host + "/" + winURL.pathname.split('/')[1];
    
    let summaryURL = new URL(baseURL + "/dashboard");
    summaryURL.searchParams.append("NAME", testName);
    summaryURL.searchParams.append("TYPE", testType);
    summaryURL.searchParams.append("LIMIT", 7);
    window.open(summaryURL.href, "_self").focus();
}