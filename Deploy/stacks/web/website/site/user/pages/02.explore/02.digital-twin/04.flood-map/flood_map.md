---
title: Flood Risk
slug: flood-map
---

<div class="intro-container three-quarter-width">
	<div class="intro-left">
		<a href="/user/images/digital-twin/flood-map-large.jpg" target="_blank" >
			<img src="/user/images/digital-twin/flood-map-large.jpg" class="header-image" alt="Flood Risk" />
		</a>
	</div>
	<div class="intro-center">
		<h2>Flood Risk</h2>
		<p>The UK Digital Twin can be queried to address cross-domain geospatial questions and analysis of risk scenarios. In the example below, the digital twin is used to identify assets that are at risk from flooding in the vicinity of King's Lynn. The flood region is based on the <a href="https://ckan.publishing.service.gov.uk/dataset/flood-map-for-planning-rivers-and-sea-flood-zone-3">Flood Map for Planning (Rivers and Sea) - Flood Zone 3</a>, which is the best estimate of land that in the absence of flood defences has more than a 1 in 100 (1%) of flooding each year from rivers (a fluvial flood) or more than a 1 in 200 (0.5%) or greater chance of flooding each year from the sea (a tidal flood).</p>
	</div>
</div>

<div id="map-container" class="full-width" style="height: 840px; margin-top: 50px;">
	<div id="map-header" style="width: 75%; height: 40px; margin: 0 auto; position: relative;">
		<table width="100%" height="100%" style="margin: auto;">
			<tr>
				<td width="50%" style="text-align: left;">
					<a href="/explore/digital-twin/land-use">&lt;&lt; Land Use</a>
				</td>
				<td width="50%" style="text-align: right;">
					<a href="/explore/digital-twin/power-system">Power System &gt;&gt;</a>
				</td>
			<tr>
		</table>
	</div>
	<div id="map-inner" style="width: 75%; height: 100%; margin: 0 auto; position: relative;">
		<iframe id="map-frame" width="100%" height="100%" src="http://localhost:9999/" />
	</div>
</div>

<br>

[plugin:content-inject](/modular/partners)