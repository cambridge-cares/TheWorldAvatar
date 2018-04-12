$(function(){
    $("#reaction-select").on('change',function () {
        //show a map of reactions
        console.log("select changed")
        $.ajax('/JSON/chemsrm.json').done(function (reactionlist) {
            //todo: init building
            console.log(reactionlist.length)
            var parent  = $('#reaction-list');
            console.log(parent)
            for(let reaction of reactionlist){
                parent.append('<li>'+reaction+'</li>')
            }
        }).fail(function () {
            console.log("error")
        })
    })
    $('#start').click(function(){
        console.log("start simulation")
        //$('#start').attr("disabled", true);
        
        var xmax = $('#xupper').val();
        var xmin = $('#xlower').val();
        var ymax = $('#yupper').val();
        var ymin = $('#ylower').val();
        console.log(xmin +" "+xmax + " " + ymin + " " + ymax)
        
        

        
        
        $.ajax('http://www.theworldavatar.com/ADMSCoordinationAgent?coordinates='+encodeURIComponent(JSON.stringify({xmin:xmin,xmax:xmax, ymin:ymin, ymax:ymax}))).done(function (bdnlist) {
            //todo: init building
            console.log(JSON.parse(bdnlist))
            initadms3dmap(JSON.parse(bdnlist))
            
        }).fail(function () {
            console.log("error")
        })
        
        
        
        
        
        
    })
});