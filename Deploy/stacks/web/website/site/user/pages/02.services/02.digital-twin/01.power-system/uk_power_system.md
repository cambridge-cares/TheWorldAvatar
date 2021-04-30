---
title: Power System
slug: power-system
---

<table class="three-quarter-width" style="margin: auto;">
	<tr>
		<td width="350px" style="text-align: center;" markdown="1">![UK](image://digital-twin/power-system-square.jpg?forceResize=300,300)</td>
		<td style="font-size: 10pt;">
			<h2>Power System</h2>
			<p>This module of the UK Digital Twin provides a map visualisation of the Knowledge Graph containing data on UK power plants. The Knowledge Graph is also queried to provide meta-data on each individual power plant; select a node on the map to view it. Please note that there may be some loading time before nodes are shown on the map</p>
		</td>
	</tr>
</table>
<br><br>

<div id="map-container" class="full-width" style="height: 800px;">
	<div id="map-inner" style="width: 75%; height: 100%; margin: 0 auto; position: relative;">
		<iframe id="map-frame" width="100%" height="100%" src="http://localhost:3001/ontotwinuk" />
	</div>
</div>
<br>

[plugin:content-inject](/modular/partners)
