(function PPMap(){
    var ppMap = new PopupMap({useCluster:true});


    $(document).on('input', 'input', function () {//when user makes input
        console.log("input changed");
        cleanMsg();
        let el = $(this), value = el.val();
        if (value === "") {
            return;
        }

        let attrid = el.attr("id");

        if (!validateInput(value)) {
            self.displayMsg(errMsgBox, "Wrong datatype.", "warning");
        }
    });

    //TODO: submit button that sends out simulation
    let runBtn = $("btn-run");

    runBtn.click(function () {
        //get each value defined

        //ajax out


    })
    //TODO: validate this
    function validateInput() {
        return true;
    }

    /*Msg***/
    let errMsgPanel = $("");

    function msgTemplate (msg, type) {
        return "<p class='alert alert-" + type + "'>" + msg + "</p>";
    }
     function displayMsg(msg, type) {
        //TODO: swithc type
         cleanMsg();
         errMsgPanel.append(msgTemplate(msg, type));

    }

    //TODO: define err msg panel
     function cleanMsg() {
         errMsgPanel.html("");
    }
})();


