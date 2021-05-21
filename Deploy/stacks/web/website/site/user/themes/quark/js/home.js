
// Append CSS sheet specific to the home page
var cssID = "homeCSS";

if (!document.getElementById(cssID)) {
    var head  = document.getElementsByTagName('head')[0];
    var link  = document.createElement('link');
    link.id   = cssID;
    link.rel  = 'stylesheet';
    link.type = 'text/css';
    link.href = '/user/themes/quark/css/home.css';
    link.media = 'all';
    head.appendChild(link);
}

/**
*	Scroll down past the hero image.
*/
function scrollDown() {
	console.log("Scrolling down...");
	var amount = $("#inner-body").offset().top - 60;
	$("html, body").animate(
		{scrollTop: amount},
		800
	);
}