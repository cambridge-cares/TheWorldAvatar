<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xd="http://www.pnp-software.com/XSLTdoc"
	xmlns:targetNamespace="http://www.xml-cml.org/schema" xmlns:cc="http://www.xml-cml.org/dictionary/compchem/"
	xmlns:conventions="http://www.xml-cml.org/convention/" xmlns:nonSi="http://www.xml-cml.org/unit/nonSi/"
	xmlns:gc="http://purl.org/gc/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" xmlns:ontokin="https://como.cheng.cam.ac.uk/kb/ontokin/">

	<!-- 
	Author: Nenad B. Krdzavac, e-mail: <a href="nk510@cam.ac.uk">nk510(at)cam.ac.uk</a> 
	-->

	<!--
	Copyright: Department of Chemical Engineering and Biotechnology, University 
	of Cambrdige, United Kingdom, 2018 
	-->

	<!--
	This xslt transforms of CompChem XML files to rdf graph as instance 
	of Gainesville Core Ontology (GNVC) ver 0.7. At the moment it covers the 
	transformation of the following features: 1. formula name 2. atom counts 
	and atom element type. 3. frequency unit, frequency values. 4. rotational 
	symmetry value, rotational symmetry unit. 5. rotational constants value, 
	rotational constants units, rotational constants size. 6. geometry type, 
	spin multiplicity. 
	-->

	<!-- Applying all templates -->


	<xsl:template match="/">

		<rdf:RDF>

			<xsl:apply-templates select="module/*[local-name() = 'module']" />
			<xsl:apply-templates select="module/module/*[local-name() = 'module']" />
			<xsl:apply-templates
				select="module/module/module/*[local-name() = 'module']" />

		</rdf:RDF>

	</xsl:template>

	<!-- 
	This template matches node that has cc:jobList attribute value. It 
	creates root rdf node for molecular system by using current node name and 
	dirctRef attribute value of current node. 
	-->

	<xsl:template match="module/*[local-name() = 'module']">

		<xsl:variable name="vmodule">
			<xsl:value-of select="local-name()" />
		</xsl:variable>
		<xsl:variable name="vdictRef">
			<xsl:value-of select="@dictRef" />
		</xsl:variable>
		<xsl:variable name="vdictRef_no_namespace">
			<xsl:value-of select="substring-after($vdictRef,'cc:')" />
		</xsl:variable>

		<rdf:Description
			rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_molecular_methоdology">
			<rdf:type rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/G09" />
		</rdf:Description>

	</xsl:template>

	<!-- 
	This template matches node that has cc:jo attribute value. It creates 
	child rdf node of molecular computation rdf node by using current node name 
	and dirctRef attribute value of current node. 
	-->

	<xsl:template match="module/module/*[local-name() = 'module']">

		<xsl:variable name="vmodule">
			<xsl:value-of select="local-name()" />
		</xsl:variable>

		<xsl:variable name="vdictRef">
			<xsl:value-of select="@dictRef" />
		</xsl:variable>
		<xsl:variable name="vdictRef_no_namespace">
			<xsl:value-of select="substring-after($vdictRef,'cc:')" />
		</xsl:variable>

		<xsl:variable name="vdictRef_parent">
			<xsl:value-of select="../@dictRef" />
		</xsl:variable>

		<xsl:variable name="vdictRef_parent_no_namespace">
			<xsl:value-of select="substring-after($vdictRef_parent,'cc:')" />
		</xsl:variable>

		<rdf:Description
			rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_parent_no_namespace}_{$vmodule}_molecular_methоdology">
			<rdf:type rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/G09" />
							
		    <ontokin:hasInitialization rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_initilization_module"/>

		</rdf:Description>
		

	</xsl:template>

	<xsl:template match="module/module/module/*[local-name() = 'module']">

		<xsl:variable name="module_type">
			<xsl:apply-templates select="@dictRef" />
		</xsl:variable>

		<xsl:value-of select="$module_type" />

		<xsl:variable name="vmodule">
			<xsl:value-of select="local-name()" />
		</xsl:variable>

		<xsl:variable name="vdictRef">
			<xsl:value-of select="@dictRef" />
		</xsl:variable>

		<xsl:variable name="vdictRef_no_namespace">
			<xsl:value-of select="substring-after($vdictRef,'cc:')" />
		</xsl:variable>

		<xsl:variable name="vdictRef_parent">
			<xsl:value-of select="../@dictRef" />
		</xsl:variable>	

		<xsl:variable name="vdictRef_parent_no_namespace">
			<xsl:value-of select="substring-after($vdictRef_parent,'cc:')" />
		</xsl:variable>
		
		<xsl:variable name="vdictRef_parent_of_parent">
			<xsl:value-of select="../../@dictRef" />
		</xsl:variable>
		
		<xsl:variable name="vdictRef_parent_of_parent_no_namespace">
			<xsl:value-of select="substring-after($vdictRef_parent_of_parent,'cc:')" />
		</xsl:variable>
		
		
		

		<xsl:choose>

			<xsl:when test="$module_type='cc:initialization'">


				<rdf:Description
					rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_parent_no_namespace}_{$vmodule}_has_initilization_module">	
					<rdf:type rdf:resource="http://purl.org/gc/MethodologyFeature"/>
									
					<gc:hasMoleculeProperty
						rdfs:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property" />

				</rdf:Description>

				<rdf:Description
					rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property">
					<rdf:type rdf:resource="http://purl.org/gc/MoleculeProperty" />
				</rdf:Description>

				<!-- 
				Read value of 'count' and 'elementType' attributes for a molecule 
				as attribute values of tag 'atom' and creates rdf graph for each atom by 
				using Periodic Table ontology. 
				-->

				<xsl:for-each select="molecule/atomArray/*[local-name() = 'atom']">
					.

					<xsl:variable name="velementType">
						<xsl:value-of select="@elementType" />
					</xsl:variable>

					<rdf:Description
						rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property">
						<rdf:type rdf:resource="http://purl.org/gc/MoleculeProperty" />

						<gc:hasMolecule
							rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$velementType}_{generate-id()}" />

					</rdf:Description>


					<rdf:Description
						rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$velementType}_{generate-id()}">
						<rdf:type rdf:resource="http://purl.org/gc/Molecule" />

						<!-- URI for chemical element implemented in Periodic table ontology 
							are not available on the web. For example: http://http://daml.org/2003/01/periodictable/PeriodicTable#Ti 
							does not give any result if we run it in a browser. -->

						<gc:hasAtom
							rdf:resource="http://www.daml.org/2003/01/periodictable/PeriodicTable#{$velementType}" />
						<gc:hasNumberOfAtoms>
							<xsl:value-of select="substring-before(@count,'.0')" />
						</gc:hasNumberOfAtoms>

					</rdf:Description>
					
					<rdf:Description rdf:about="http://www.daml.org/2003/01/periodictable/PeriodicTable#{$velementType}">
					<rdf:type rdf:resource="http://www.daml.org/2003/01/periodictable/PeriodicTable#Element"/>
					
					</rdf:Description>

				</xsl:for-each>

				<xsl:for-each select="molecule/*[local-name() = 'formula']">

					<rdf:Description
						rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property">

						<gc:hasName>
							<xsl:value-of select="@concise" />
						</gc:hasName>

					</rdf:Description>

				</xsl:for-each>

			</xsl:when>

			<xsl:when test="$module_type='cc:finalization'">

            	
			
			<xsl:for-each select="propertyList/*[local-name() = 'property']">
			    
			    <xsl:if test="@dictRef='cc:vibrations'">
			    
			    <rdf:Description
			    rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_parent_of_parent_no_namespace}_{$vmodule}_molecular_methоdology">
			    
			    <rdf:type rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/G09" />
			
			    <gc:isCalculationOn 	rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_molecular_computation_{generate-id()}"/>
				
			    </rdf:Description>
			
			    				
				</xsl:if>
				
				<xsl:if test="@dictRef='cc:rotational_symmetry'">			    
			    <xsl:apply-templates select="@dictRef"/>			
				</xsl:if>
				
				
				<xsl:if test="@dictRef='cc:rotational_constants'">			    
			    <xsl:apply-templates select="@dictRef"/>			
				</xsl:if>				
				
				<xsl:if test="@dictRef='cc:geometry_type'">			    
			    <xsl:apply-templates select="@dictRef"/>			
				</xsl:if>
				
			
			 </xsl:for-each>

			</xsl:when>

		</xsl:choose>

	</xsl:template>

	<!-- Read value of 'dictRef' attribute -->
	<xsl:template match="@dictRef">
		<xsl:value-of select="." />
	</xsl:template>

</xsl:transform>



