// var prefix = "http://localhost:8080";
var prefix = "http://www.jparksimulator.com";
var markers = []

const toggleDisplay = elemId => {
    let x = document.getElementById(elemId);
    if (x.style.display !== 'block') {
        x.style.display = 'block';
    } else {
        x.style.display = 'none';
    }
};

$("#readme-button").click(function() {
    toggleDisplay("readme-text");
});

document.addEventListener("click", function(evt) {
    var arrUrl = window.location.pathname.split('/');
    var readmeButtonElement = document.getElementById('readme-button'),
        readmeTextElement = document.getElementById('readme-text'),
        targetElement = evt.target;  // clicked element

    if (targetElement == readmeButtonElement || targetElement == readmeTextElement) {
        return; //readme-button or readme-text is clicked. do nothing.
    }

    if(readmeTextElement.style.display === 'block') {
        readmeTextElement.style.display = 'none';
    }
});//first call to initMap. Determine center of map by url

function initMap() {
    //array of pathName
    var arrUrl = window.location.pathname.split('/');
    var center;
    map = new google.maps.Map(document.getElementById('map'));
    center = new google.maps.LatLng(52.4137756,-1.5849575);
    map.setZoom(10);
    map.setCenter(center);

    
  }