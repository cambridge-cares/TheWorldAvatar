<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xd="http://www.pnp-software.com/XSLTdoc"
	xmlns:targetNamespace="http://www.xml-cml.org/schema" xmlns:cc="http://www.xml-cml.org/dictionary/compchem/"
	xmlns:conventions="http://www.xml-cml.org/convention/" xmlns:nonSi="http://www.xml-cml.org/unit/nonSi/"
	xmlns:gc="http://purl.org/gc/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#">

<xd:doc>
	<xd:author>
		Nenad B. Krdzavac
		e-mail: <xd:a docid="nk510@cam.ac.uk">nk510_at_cam.ac.uk</xd:a>
	</xd:author>
	
	<xd:copyright>University of Cambrdige, Department of Chemical
		Engineering and Biotechnology, United Kingdom, 2018</xd:copyright>
</xd:doc>

	<xd:doc>
<xd:p>
		This xslt transforms of CompChem XML files to rdf graph as
		instance of
		Gainesville Core Ontology (GNVC) ver 0.7. At the moment it
		covers the
		transformation of the following features:
		<xd:p>
		1. formula name 
		2. atom counts and atom element type. 
		3. frequency unit, frequency values. 
		4. rotational symmetry value, rotational symmetry unit. 
		5. rotational constants value, rotational constants units, rotational constants size. 
		6. geometry type, spin multiplicity.
		</xd:p>
</xd:p>
	</xd:doc>

    <xd:doc><xd:p>Applying all templates</xd:p></xd:doc>

	<xsl:template match="/">

		<rdf:RDF>
		
			<xsl:apply-templates select="module/*[local-name() = 'module']" />
			<xsl:apply-templates select="module/module/*[local-name() = 'module']" />
	        <xsl:apply-templates select="module/module/module/*[local-name() = 'module']"/> 

		</rdf:RDF>

	</xsl:template>

	<xd:doc>
	<xd:p>
	This template matches node that has cc:jobList attribute value.
	It creates root rdf node for molecular system by using current
	node name
	and dirctRef attribute value of current node, and the
	dirctRef attribute value of
	child node.
	</xd:p>
	</xd:doc>

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
			rdf:about="http://www.xml-cml.org/dictionary/compchem/{$vdictRef_no_namespace}_{$vmodule}">
			<rdf:type rdf:resource="http://purl.org/gc/MolecularSystem"/>				
		</rdf:Description>

	</xsl:template>

	<xd:doc>
		This template matches node that has cc:jo attribute value.
		It creates child rdf node of molecular computation rdf node by using
		current node name and dirctRef attribute value of current node, and the
		dirctRef attribute value of parent node. 
	</xd:doc>
	
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
			rdf:about="http://www.xml-cml.org/dictionary/compchem/{$vdictRef_parent_no_namespace}_{$vmodule}">
			<rdf:type rdf:resource="http://purl.org/gc/MolecularSystem"/>

			<gc:hasMolecularComputation
				rdf:resource="http://www.xml-cml.org/dictionary/compchem/{$vdictRef_no_namespace}_{$vmodule}" />
				
		</rdf:Description>
		
		<rdf:Description		
			rdf:about="http://www.xml-cml.org/dictionary/compchem/{$vdictRef_no_namespace}_{$vmodule}">
			<rdf:type rdf:resource="http://purl.org/gc/MolecularComputation" />
		</rdf:Description>

		
	</xsl:template>
	
<xsl:template match="module/module/module/*[local-name() = 'module']">

<xsl:variable name="module_type">
<xsl:apply-templates select="@dictRef"/>
</xsl:variable>

<xsl:value-of select="$module_type"/>

<xsl:variable name="vmodule">
			<xsl:value-of select="local-name()" />
		</xsl:variable>

<xsl:variable name="vdictRef">
			<xsl:value-of select="@dictRef"/>
</xsl:variable>
		
		<xsl:variable name="vdictRef_no_namespace">
			<xsl:value-of select="substring-after($vdictRef,'cc:')" />
		</xsl:variable>
						
<xsl:variable name="vdictRef_parent">
			<xsl:value-of select="../@dictRef"/>
		</xsl:variable>
		
		<xsl:variable name="vdictRef_parent_no_namespace">
			<xsl:value-of select="substring-after($vdictRef_parent,'cc:')" />
		</xsl:variable>
		
<xsl:choose>

<xsl:when test="$module_type='cc:finalization'">		
		<rdf:Description		
			rdf:about="http://www.xml-cml.org/dictionary/compchem/{$vdictRef_parent_no_namespace}_{$vmodule}">
			<rdf:type rdf:resource="http://purl.org/gc/MolecularComputation" />
			
			<gc:hasResult rdfs:resource="http://www.xml-cml.org/dictionary/compchem/{$vdictRef_no_namespace}_{$vmodule}"/>
			
		</rdf:Description>	
		
		<rdf:Description rdf:about="http://www.xml-cml.org/dictionary/compchem/{$vdictRef_no_namespace}_{$vmodule}">
		<rdf:type rdf:resource="http://purl.org/gc/CalculationResult"/>
		</rdf:Description>
		
</xsl:when>

<xsl:when test="$module_type='cc:initialization'">		

		<rdf:Description		
			rdf:about="http://www.xml-cml.org/dictionary/compchem/{$vdictRef_parent_no_namespace}_{$vmodule}">
			<rdf:type rdf:resource="http://purl.org/gc/MolecularComputation" />
			
			<gc:hasResult rdfs:resource="http://www.xml-cml.org/dictionary/compchem/{$vdictRef_no_namespace}_{$vmodule}"/>
			
		</rdf:Description>	
		
		<rdf:Description rdf:about="http://www.xml-cml.org/dictionary/compchem/{$vdictRef_no_namespace}_{$vmodule}">
		<rdf:type rdf:resource="http://purl.org/gc/CalculationResult"/>
		</rdf:Description>
		
</xsl:when>

</xsl:choose>

</xsl:template>

<xsl:template match="module/module/module/module/*[local-name() = 'module']">

</xsl:template>

<xsl:template match="@dictRef">		
<xsl:value-of select="." />
</xsl:template>
	
	
</xsl:transform>



