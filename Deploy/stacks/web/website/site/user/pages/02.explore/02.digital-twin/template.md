---
title: Title
slug: slug
---

<!--
	This is an example template for Digital Twin pages containing a Map visualisation.
	
	A custom twig template can be added to add a custom JavaScript file when loading this
	page (which in turn can embed a map visualisation from another Docker container).
-->

<div class="intro-container three-quarter-width">
	<div class="intro-left">
		<a href="/user/images/example-image-large.jpg" target="_blank" >
			<img src="/user/images/example-image-small.jpg" class="header-image" alt="Image" />
		</a>
	</div>
	<div class="intro-center">
		<h2>Page Heading</h2>
		<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam ac imperdiet arcu. Pellentesque ac ex bibendum, vulputate magna vitae, malesuada ligula.</p>
		<p>Praesent in enim elit. Maecenas id nibh sit amet turpis mollis auctor.</p>
		<p>Curabitur vitae leo ultricies, tristique erat eu, bibendum mauris. Nunc cursus justo vitae mi fermentum elementum. Nam pulvinar lectus ac tellus facilisis, ac facilisis libero porta. Pellentesque lobortis in mi at accumsan. Aenean id leo vulputate, feugiat tortor in, bibendum nisl. Donec non felis eu lacus finibus egestas. Phasellus sed lacus placerat sapien consequat malesuada ut sed nisi. Nunc tempus efficitur libero, laoreet feugiat magna maximus convallis. </p>
	</div>
</div>

<br><br>

<div id="map-container" class="full-width" style="height: 800px;">
	<div id="map-inner" style="width: 75%; height: 100%; margin: 0 auto; position: relative;">
		<iframe id="map-frame" width="100%" height="100%" src="http://localhost:9999/" />
	</div>
</div>

<br>

[plugin:content-inject](/modular/partners)
