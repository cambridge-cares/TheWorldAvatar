---
title: Marie
slug: marie
---

<div class="intro-container three-quarter-width">
	<div class="intro-left">
		<img src="/user/images/marie/search-engine-large.jpg" class="header-image" alt="Search Engine" />
	</div>
	<div class="intro-center">
		<h2>Marie</h2>
		<p>This website presents a proof-of-concept search engine system for accessing chemical data from the World Avatar Knowledge Graph and the Wikidata Knowledge Graph. The Knowledge Graphs offers inter-connected data from chemical kinetics to chemical and physical properties of species and many other domains. We trained a question type classification model and an entity extraction model to interpret chemistry-related questions of interest. The system has a novel design which applies a topic model to identify the question-to-ontology affiliation to improve its accuracy.</p>
		<p>To use the Marie search engine, type a question into the field below, or select one of the provided sample questions.</p>
	</div>
</div>

<div class="marie-input-container full-width">
	<div class="input-group">
		<input id="input-field" type="search" autocomplete="off" placeholder="Type your query...">
		<button id="ask-button" type="button" class="mybutton" onclick="askQuestion()">
			<img src="/user/images/search.svg"/>
		</button>
		<button id="shuffle-button" type="button" class="mybutton" onclick="shuffleQuestion()">
			<img src="/user/images/shuffle.svg"/>
		</button>
	</div>
</div>

<div id="results-row">
	<h3>Results</h3>
	<br/>
	<b><u>From the World Avatar</u></b>
	<div id="chatbot-results" class="results-container"><img src="/user/images/spinner.svg" style="vertical-align: middle;" width="22px">  Loading, please wait...
	</div>
	<br/>
</div>

### Example Questions

<button type="submit" class="accordion">General Computational Quantum Chemistry</button>
<div class="accordion-panel">
	To query computational quantum-related data about molecules.
	<ul style="margin-left: 20px;">
		<li><div class="sample-question">Show me the vibration frequency of H2O2</div></li>
		<li><div class="sample-question">Find the gaussian files of c8h14</div></li>
		<li><div class="sample-question">What is the symmetry number of C8H14</div></li>
		<li><div class="sample-question">What is the spin multiplicity of C8H14</div></li>
		<li><div class="sample-question">Electronic energy of C2H2O2</div></li>
		<li><div class="sample-question">Show the formal charge of C3H6</div></li>
		<li><div class="sample-question">What is the geometry type of C2H2O2</div></li>
	</ul>
</div>

<button type="submit" class="accordion">Kinetics &amp; Thermodynamic</button>
<div class="accordion-panel">
	To query kinetic and thermodynamic data about molecules.
	<ul>
		<li><div class="sample-question">What is the lennard jones well depth of C2H2O2</div></li>
		<li><div class="sample-question">Polarizability of C2H2O2</div></li>
		<li><div class="sample-question">What is the dipole moment of C2H2O2</div></li>
		<li><div class="sample-question">Rotational relaxation collision number of HOCH2O2H</div></li>
	</ul>
</div>

<button type="submit" class="accordion">Reactions &amp; Mechanisms</button>
<div class="accordion-panel">
	To query reactions and mechanisms and their properties.
	<ul>
		<li><div class="sample-question">What reaction produces H2 + OH</div></li>
		<li><div class="sample-question">Is the reaction H + H2O == H2 + OH reversible</div></li>
		<li><div class="sample-question">What reaction has CH4 as reactant</div></li>
		<li><div class="sample-question">What mechanism contains CH4 + OH</div></li>
		<li><div class="sample-question">Reaction rate of H + O2 -&gt; O + OH</div></li>
	</ul>
</div>

<button type="submit" class="accordion">Molecule Classes</button>
<div class="accordion-panel">
	To query properties of a certain class of molecules.
	<ul>
		<li><div class="sample-question">Mass of aromatic hydrocarbons</div></li>
		<li><div class="sample-question">Molecular model of aromatic hydrocarbons</div></li>
		<li><div class="sample-question">Chemical structure of aromatic hydrocarbons</div></li>
	</ul>
</div>

<button type="submit" class="accordion">Conditional Queries</button>
<div class="accordion-panel">
	To find molecules meeting certain conditions.
	<ul>
		<li><div class="sample-question">Chemical formula of alkanol with heat capacity less than 15</div></li>
		<li><div class="sample-question">Mass of aromatic hydrocarbons with mass less than 170</div></li>
		<li><div class="sample-question">Aromatic hydrocarbons with mass less than 170</div></li>
		<li><div class="sample-question">Chemical formula of alkanol with heat capacity less than 15</div></li>
	</ul>
</div>

<button type="submit" class="accordion">Queries by SMILES</button>
<div class="accordion-panel">
	To find molecules by their SMILES.
	<ul>
		<li><div class="sample-question">What is the molecular weight of c1ccccc1</div></li>
		<li><div class="sample-question">Show me the molecular model of CH2=CHCHO</div></li>
		<li><div class="sample-question">Show me the ionization energy of C1=CC=CC=C1</div></li>
		<li><div class="sample-question">What is the heat capacity of c1=cc=cc=c1</div></li>
	</ul>
</div>

<button type="submit" class="accordion">Molecule Properties</button>
<div class="accordion-panel">
	To query molecule properties.
	<ul>
		<li><div class="sample-question">Molecular weight of ch4</div></li>
		<li><div class="sample-question">Chemical structure of benzene</div></li>
		<!--<li><div class="sample-question">Standard enthalpy of formation of methane</div></li>-->
		<li><div class="sample-question">What is the conjugate base of ethanedionic acid</div></li>
	</ul>
</div>
<br>

[plugin:content-inject](/modular/partners)
