
function initialise() {
    var testStubs = document.getElementsByClassName("test-result-stub");
    Array.from(testStubs).forEach(testStub => {
            
        const onEvent = ((evt) => {
           testStub.style.backgroundColor = "rgb(235, 235, 235)";
        });
        const offEvent = ((evt) => { 
            testStub.style.backgroundColor = "rgb(245, 245, 245)";
        });
        
        testStub.addEventListener("mouseover", onEvent , false);
        testStub.addEventListener("mouseout", offEvent , false)   
    });        
}

function onStubClick(testName) {
    console.log("Clicked on: " + testName);
    window.open("./test-summary.jps?TEST_NAME=" + testName, '_blank').focus();
}