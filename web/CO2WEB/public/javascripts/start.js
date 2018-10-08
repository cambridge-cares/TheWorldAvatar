$(function(){
    
    $('#start').click(function(){
        console.log("start simulation")
        //$('#start').attr("disabled", true);
        
        var xmax = $('#xupper').val();
        var xmin = $('#xlower').val();
        var ymax = $('#yupper').val();
        var ymin = $('#ylower').val();
        console.log(xmin +" "+xmax + " " + ymin + " " + ymax)
        
        
        $('#reaction-select').on('change',function () {
            //show a map of reactions
            $.ajax('/JSON/chemsrm.json').done(function (reactionlist) {
                //todo: init building
                console.log(reactionlist)
                var parent  = $('#reaction-list');
                for(let reaction of reactionlist){
                    parent.appendChild('<li>'+reaction+'</li>')
                }
            }).fail(function () {
                console.log("error")
            })
        })
        
        
        $.ajax('http://www.theworldavatar.com/ADMSCoordinationAgent?coordinates='+encodeURIComponent(JSON.stringify({xmin:xmin,xmax:xmax, ymin:ymin, ymax:ymax}))).done(function (bdnlist) {
            //todo: init building
            console.log(bdnlist)
            
        }).fail(function () {
            console.log("error")
        })
        
        
        
        
        
        
    })
});