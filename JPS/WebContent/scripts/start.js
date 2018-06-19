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
//        console.log("start simulation");
        //$('#start').attr("disabled", true);
        
        var xmax = parseInt($('#xupper').val());
        var xmin = parseInt($('#xlower').val());
        var ymax = parseInt($('#yupper').val());
        var ymin = parseInt($('#ylower').val());
//        console.log(xmin +" "+xmax + " " + ymin + " " + ymax)
    
//        approximate becasue texture only work on power2(has to be 1:1,1:2,1:4...)
        [xmin, xmax, ymin, ymax] = appro2ratio(xmin, xmax, ymin, ymax);
        
        
        $.ajax('http://www.theworldavatar.com/JPS/ADMSCoordinationAgent?coordinates='+encodeURIComponent(JSON.stringify({'xmin':xmin,'xmax':xmax, 'ymin':ymin, 'ymax':ymax}).replaceAll('"',"'"))).done(function (bdnlist) {
            //todo: init building
//            console.log(JSON.parse(bdnlist))
            initadms3dmap(JSON.parse(bdnlist))
            
        }).fail(function () {
            console.log("error")
        })
        
        
        
        
        
        
    })
});

//approximate to ratio 1:1 or 1:2
function appro2ratio(xmin, xmax, ymin, ymax){
    x = xmax - xmin;
    y = ymax - ymin;
    ratio = x/y;
    if(ratio >= 2){
        ymax = ymin + x/2;
    } else if( ratio <2 && ratio > 1){
        ymax = ymin + x;
    } else if (ratio <=1 && ratio > 1/2){
        xmax =xmin + y;
    }else {
        xmax = xmin + y /2;
    }
    
    return [xmin, xmax, ymin, ymax];
    
}

String.prototype.replaceAll = function(search, replacement) {
    var target = this;
    return target.replace(new RegExp(search, 'g'), replacement);
};