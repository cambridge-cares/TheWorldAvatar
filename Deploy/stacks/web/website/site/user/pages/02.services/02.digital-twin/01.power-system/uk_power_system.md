---
title: Power System
slug: power-system
---

<table class="three-quarter-width" style="margin: auto;">
	<tr>
		<td width="350px" style="text-align: center;" markdown="1">![UK](image://digital-twin/power-system-square.jpg?forceResize=300,300)</td>
		<td style="font-size: 10pt;">
			<h2>Power System</h2>
			<p>The UK Digital Twin supports queries across number of aspects of the UK energy related infrastructure, including the electrical power system, the gas transmission system and land use.​</p>
			<br>
			<p>The knowledge-graph based architecture of the Digital Twin allows to address cross-domain geospatial questions. The image to the left shows an example that estimates the emissions intensity of each power generator (over 30 MW) in the UK. Each dot on the figure corresponds to a generator, with the size of the dot corresponding to the capacity of the generator and the colour of the dot corresponding to the value of the estimated emissions intensity. In this example, the emissions intensity is calculated at the mass of carbon dioxide emitted per unit value of electricity generated and serves as a proxy for Indicator 9.4.1 of the UN Sustainable Development Goals (see right).</p>
			<br>
			<p>The map below shows a sample of the power system data in the UK Digital Twin.</p>
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
