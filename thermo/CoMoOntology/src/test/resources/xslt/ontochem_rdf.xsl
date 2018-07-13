<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xd="http://www.pnp-software.com/XSLTdoc"
	xmlns="http://www.xml-cml.org/schema" xmlns:cc="http://www.xml-cml.org/dictionary/compchem/"
	xmlns:conventions="http://www.xml-cml.org/convention/" xmlns:nonSi="http://www.xml-cml.org/unit/nonSi/"
	xmlns:gc="http://purl.org/gc/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" xmlns:kb="http://ontochem.theworldavatar.com/kb/OntoChem.owl#"
	xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:math="java.lang.Math"
	exclude-result-prefixes="math">

	<!--Author: Nenad B. Krdzavac, e-mail: nk510(at)cam.ac.uk -->

	<!-- 
	    Copyright: Department of Chemical Engineering and Biotechnology, University 
	    of Cambrdige, United Kingdom, 2018 
	-->

	<!--This xslt transforms of CompChem XML files to rdf graph as instance 
		of CompChem ontology (An extension of Gainesville Core Ontology (GNVC) ver 0.7.). At the moment it covers the 
		transformation of the following features: 
		
		1.  formula name, 
		2.  atom counts, 
		3.  atom element type, 
		4.  frequency unit, 
		5.  frequency values, 
		6.  rotational symmetry value, 
		7.  rotational symmetry unit,
		8.  rotational constants value, 
		9.  rotational constants units, 
		10. rotational constants size. 
		11. geometry type, 
		12. spin multiplicity. 
		13. atomic mass.
		14. basis set.
		15. level of theory.
		16. program name.
		17. program version.
		18. run date.	
	  -->

	<!-- Applying all templates -->

	<xsl:template match="/">

		<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
			xmlns:unit="http://data.nasa.gov/qudt/owl/unit#" xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
			xmlns:kb="http://ontochem.theworldavatar.com/kb/OntoChem.owl#" xmlns:spin="http://spinrdf.org/spin#"
			xmlns:arg="http://spinrdf.org/arg#" xmlns:obo="http://purl.obolibrary.org/obo/"
			xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:gc="http://purl.org/gc/"
			xmlns:qudt="http://data.nasa.gov/qudt/owl/qudt#" xmlns:sp="http://spinrdf.org/sp#"
			xmlns:spl="http://spinrdf.org/spl#" xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
			xmlns:molhub="http://como.cheng.cam.ac.uk/molhub/compchem/"
			xmlns:table="http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#">

			<owl:Ontology rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/">
			<owl:versionInfo>Created by compchem XSLT transformation</owl:versionInfo>
			</owl:Ontology>

			<xsl:apply-templates select="module/module/*[local-name() = 'module']"/>
			<xsl:apply-templates
				select="module/module/module/*[local-name() = 'module']"/>
		</rdf:RDF>

	</xsl:template>

	<xsl:variable name="id">
		<xsl:value-of select="(math:random()*100000)  + 1"/>
	</xsl:variable>

	<!-- 
	This template matches nodes that has cc:jobList and cc:job attributes 
	value. It creates root and its child rdf nodes for molecular system by using 
	current node name and dirctRef attribute value of current node. 
	-->

	<xsl:template match="module/module/*[local-name() = 'module']">

		<xsl:variable name="vmodule">
			<xsl:value-of select="local-name()"/>
		</xsl:variable>

		<xsl:variable name="vdictRef">
			<xsl:value-of select="@dictRef" />
		</xsl:variable>
		<xsl:variable name="vdictRef_no_namespace">
			<xsl:value-of select="substring-after($vdictRef,'cc:')"/>
		</xsl:variable>

		<xsl:variable name="vdictRef_parent">
			<xsl:value-of select="../@dictRef" />
		</xsl:variable>

		<xsl:variable name="vdictRef_parent_no_namespace">
			<xsl:value-of select="substring-after($vdictRef_parent,'cc:')"/>
		</xsl:variable>

		<!-- 
		Creates root rdf node that is instance of 'G09' ontology class. It 
		also creates property relation between root node and node that has a tree 
		structure of information stored in initialisation module. 
		-->
        
        <!-- 
        
        Check whether we need to add this part of xslt code in case of appearing more than one job in parsing g09.
        
		<owl:NamedIndividual rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_no_namespace}_{$vdictRef_no_namespace}_{$vmodule}_calcuation_{$id}}">
		
		<gc:hasCalculation rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}"/>
		
		</owl:NamedIndividual>
		
		-->

		<owl:NamedIndividual rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">
			<rdf:type rdf:resource="http://ontochem.theworldavatar.com/kb/OntoChem.owl#G09"/>
			<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>

			<kb:hasInitialization
				rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_initilization_module_{$id}"/>
				
		    <kb:hasEnvironment rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_environment_module_{$id}"/>

		</owl:NamedIndividual>

	</xsl:template>

	<!-- This template transforms all information that are available in initialisation 
		and finalisation modules of CompChem xml file. -->

	<xsl:template match="module/module/module/*[local-name() = 'module']">

		<!--Definitions of all variable used during xslt transformation. These 
			variables are used in creating rdf nodes. -->
		<xsl:variable name="module_type">
			<xsl:apply-templates select="@dictRef" />
		</xsl:variable>

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
		    	
            <xsl:when test="$module_type='cc:environment'">
            		    
			<xsl:for-each select="parameterList/parameter/*[local-name() = 'scalar']">
			
			<xsl:if test="../@dictRef='cc:program'">
			<owl:NamedIndividual
					rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_no_namespace}_{$vmodule}_has_environment_module_{$id}">
			<rdf:type rdf:resource="http://purl.org/gc/SourcePackage"/>
			<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>
			<kb:hasProgram><xsl:value-of select="."/></kb:hasProgram>		
		    </owl:NamedIndividual>
			</xsl:if>
			
			<xsl:if test="../@dictRef='cc:programVersion'">
			<owl:NamedIndividual
					rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_no_namespace}_{$vmodule}_has_environment_module_{$id}">
			<rdf:type rdf:resource="http://purl.org/gc/SourcePackage"/>
			<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>
			<kb:hasProgramVersion><xsl:value-of select="."/></kb:hasProgramVersion>		
		    </owl:NamedIndividual>
			</xsl:if>
			
			<xsl:if test="../@dictRef='cc:runDate'">
			<owl:NamedIndividual
					rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_no_namespace}_{$vmodule}_has_environment_module_{$id}">
			<rdf:type rdf:resource="http://purl.org/gc/SourcePackage"/>
			<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>
			<kb:hasRunDate><xsl:value-of select="."/></kb:hasRunDate>		
		    </owl:NamedIndividual>
			</xsl:if>
			
			
			</xsl:for-each>
			
			
			</xsl:when>
			
 
			<!-- Transformation of information about molecule stored in initialization 
				module of CompChem xml file. -->

			<xsl:when test="$module_type='cc:initialization'">

				<!-- -->
				<owl:NamedIndividual
					rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_no_namespace}_{$vmodule}_has_initilization_module_{$id}">
					<rdf:type rdf:resource="http://ontochem.theworldavatar.com/kb/OntoChem.owl#InitializationModule"/>
					
					<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>

					<gc:hasMoleculeProperty
						rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property_{$id}"/>

				</owl:NamedIndividual>

				<rdf:Description
					rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property_{$id}">
					<rdf:type rdf:resource="http://purl.org/gc/MoleculeProperty" />
					<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
				</rdf:Description>
				
				
				<xsl:for-each select="parameterList/parameter/*[local-name() = 'scalar']">
				
				<xsl:if test="../@dictRef='cc:method'">
				<owl:NamedIndividual
					rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_no_namespace}_{$vmodule}_has_initilization_module_{$id}">
				<gc:hasParameter rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_level_of_theory_parameter_{$id}"/>	
				</owl:NamedIndividual>
			    <owl:NamedIndividual
					rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_level_of_theory_parameter_{$id}">
				<rdf:type rdf:resource="http://ontochem.theworldavatar.com/kb/OntoChem.owl#LevelOfTheory"/>
				<rdf:type rdf:resource="http://purl.org/gc/MethodologyFeature"/>
				<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>
				
				<kb:hasLevelOfTheoryValue><xsl:value-of select="."/></kb:hasLevelOfTheoryValue>		
			    
			    		
		        </owl:NamedIndividual>
			    </xsl:if>
				
				<xsl:if test="../@dictRef='cc:basis'">
				
			    <xsl:variable name="vbasis_set">
			    <xsl:value-of select="." />
		        </xsl:variable>
		
		        <owl:NamedIndividual
					rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_no_namespace}_{$vmodule}_has_initilization_module_{$id}">
				<gc:hasParameter rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_basis_set_parameter_{$id}"/>	
				</owl:NamedIndividual>
					    
			    <owl:NamedIndividual
				rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_basis_set_parameter_{$id}">
				<xsl:if test="$vbasis_set='6-311+G(d,p)'">	
				<rdf:type rdf:resource="http://purl.org/gc/6-311pGS"/>
				</xsl:if>
				<rdf:type rdf:resource="http://purl.org/gc/BasisSet"/>
				<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>
				<gc:hasBasisSet><xsl:value-of select="."/></gc:hasBasisSet>
		        </owl:NamedIndividual>
			    </xsl:if>
				
				
				</xsl:for-each>


				<!-- 
				Read value of 'count' and 'elementType' attributes for a molecule 
				as attribute values of tag 'atom' and creates rdf graph for each atom by 
				using Periodic Table ontology. 
				-->
				
				

				<xsl:for-each select="molecule/atomArray/*[local-name() = 'atom']">

					<xsl:variable name="velementType">
						<xsl:value-of select="@elementType" />
					</xsl:variable>

					<xsl:variable name="atom_count">
						<xsl:value-of select="@count" />
					</xsl:variable>


					<owl:NamedIndividual
						rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/MoleculeProperty"/>

						<gc:hasMolecule
							rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$velementType}{$atom_count}_{generate-id()}_{$id}"/>

					</owl:NamedIndividual>

                     
					<owl:NamedIndividual
						rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$velementType}{$atom_count}_{generate-id()}_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/Molecule" />
						<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />

						<!-- URI for chemical elements implemented in Periodic table ontology 
							are not available on the web. For example: http://http://daml.org/2003/01/periodictable/PeriodicTable#Ti 
							does not give any result if we run it in a browser. -->

						<gc:hasAtom
							rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/atom_{$velementType}{$atom_count}_{generate-id()}_{$id}" />

						<gc:hasNumberOfAtoms>
							<xsl:value-of select="substring-before(@count,'.0')" />
						</gc:hasNumberOfAtoms>

					</owl:NamedIndividual>

					<owl:NamedIndividual
						rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/atom_{$velementType}{$atom_count}_{generate-id()}_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/Atom" />
						<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />

						<gc:isElement
							rdf:resource="http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#{$velementType}" />

					</owl:NamedIndividual>

					<owl:NamedIndividual
						rdf:about="http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#{$velementType}">
						<rdf:type
							rdf:resource="http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#Element" />
						<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
					</owl:NamedIndividual>

				</xsl:for-each>

				<!-- Transformation of formula name available as 'concise' attribute 
					value in CompChem xml file. -->

				<xsl:for-each select="molecule/*[local-name() = 'formula']">

					<owl:NamedIndividual
						rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/MoleculeProperty" />
						<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />

						<gc:hasName>
							<xsl:value-of select="@concise" />
						</gc:hasName>

					</owl:NamedIndividual>

				</xsl:for-each>

			</xsl:when>

			<xsl:when test="$module_type='cc:finalization'">

				<!-- Iterates over each property (vibrations, rotational constants, etc) 
					and transforms xml into Abox assertions -->

				<xsl:for-each select="propertyList/*[local-name() = 'property']">

					<!-- Transformation of vibrations data -->

					<xsl:if test="@dictRef='cc:vibrations'">

						<xsl:variable name="vdictRef_value">
							<xsl:value-of select="@dictRef" />
						</xsl:variable>

						<xsl:variable name="vdictRef_value_no_namespace">
							<xsl:value-of select="substring-after($vdictRef_value,'cc:')" />
						</xsl:variable>

						<xsl:for-each select="*[local-name()='array']">

							<xsl:variable name="fdictRef_value">
								<xsl:value-of select="@dictRef" />
							</xsl:variable>

							<xsl:variable name="fdictRef_value_no_namespace">
								<xsl:value-of select="substring-after($fdictRef_value,'cc:')" />
							</xsl:variable>

							<xsl:variable name="frequencies_size">
								<xsl:value-of select="@size" />
							</xsl:variable>

							<xsl:variable name="funit">
								<xsl:value-of select="@units" />
							</xsl:variable>
							<xsl:variable name="funit_value_no_namespace">
								<xsl:value-of select="substring-after($funit,':')" />
							</xsl:variable>

							<!-- Transformation of frequencies data -->

							<xsl:if test="$fdictRef_value='cc:frequencies'">

								<owl:NamedIndividual
									rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_of_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">

									<rdf:type rdf:resource="http://ontochem.theworldavatar.com/kb/OntoChem.owl#G09" />
									<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />

									<gc:isCalculationOn
										rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_{$vdictRef_value_no_namespace}_{generate-id()}_{$id}" />

								</owl:NamedIndividual>

								<owl:NamedIndividual
									rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_{$vdictRef_value_no_namespace}_{generate-id()}_{$id}">

									<rdf:type rdf:resource="http://purl.org/gc/VibrationalAnalysis"/>
									<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>

									<gc:hasResult
										rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_{$vdictRef_value_no_namespace}_{$fdictRef_value_no_namespace}_{generate-id()}_{$id}"/>

								</owl:NamedIndividual>

								<owl:NamedIndividual
									rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_{$vdictRef_value_no_namespace}_{$fdictRef_value_no_namespace}_{generate-id()}_{$id}">

									<rdf:type rdf:resource="http://purl.org/gc/Frequency"/>
									<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>

									<kb:hasFrequenciesValue>
										<xsl:value-of select="."/>
									</kb:hasFrequenciesValue>

									<gc:hasUnit rdf:resource="http://purl.org/gc/{$funit_value_no_namespace}"/>

									<gc:hasVibrationCount>
										<xsl:value-of select="$frequencies_size"/>
									</gc:hasVibrationCount>

								</owl:NamedIndividual>

								<owl:NamedIndividual
									rdf:about="http://purl.org/gc/{$funit_value_no_namespace}">
									<rdf:type rdf:resource="http://data.nasa.gov/qudt/owl/qudt#FrequencyUnit"/>
									<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>
								</owl:NamedIndividual>

							</xsl:if>

						</xsl:for-each>

					</xsl:if>

					<!-- Transformation geometry type property -->

					<xsl:if test="@dictRef='cc:geometry_type'">

						<xsl:variable name="gdictRef_value">
							<xsl:value-of select="@dictRef" />
						</xsl:variable>

						<xsl:variable name="gdictRef_value_no_namespace">
							<xsl:value-of select="substring-after($gdictRef_value,'cc:')" />
						</xsl:variable>
						
						<xsl:variable name="geometry_type_value">
							<xsl:value-of select="substring-after($gdictRef_value,'cc:')" />
						</xsl:variable>
						

						<owl:NamedIndividual
							rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_of_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">

							<rdf:type rdf:resource="http://ontochem.theworldavatar.com/kb/OntoChem.owl#G09" />
							<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />

							<gc:isCalculationOn
								rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_{$gdictRef_value_no_namespace}_{generate-id()}_{$id}"/>
						</owl:NamedIndividual>

						<owl:NamedIndividual
							rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_{$gdictRef_value_no_namespace}_{generate-id()}_{$id}">
							<rdf:type rdf:resource="http://ontochem.theworldavatar.com/kb/OntoChem.owl#GeometryType" />
							<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>
							<kb:hasGeometryTypeValue><xsl:apply-templates select="*[local-name() = 'scalar']"/></kb:hasGeometryTypeValue>
						</owl:NamedIndividual>

					</xsl:if>

					<!-- Transformation rotational symmetry property -->

					<xsl:if test="@dictRef='cc:rotational_symmetry'">

						<xsl:variable name="rsdictRef_value">
							<xsl:value-of select="@dictRef" />
						</xsl:variable>

						<xsl:variable name="rsdictRef_value_no_namespace">
							<xsl:value-of select="substring-after($rsdictRef_value,'cc:')" />
						</xsl:variable>

						<!-- Iterates over scalar tag inside property tag. Transforms attributes 
							and -->
						<xsl:for-each select="*[local-name()='scalar']">

							<xsl:variable name="rsunit">
								<xsl:value-of select="@units" />
							</xsl:variable>

							<xsl:variable name="rsunit_value_no_namespace">
								<xsl:value-of select="substring-after($rsunit,':')" />
							</xsl:variable>


							<owl:NamedIndividual
								rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_of_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">

								<rdf:type rdf:resource="http://ontochem.theworldavatar.com/kb/OntoChem.owl#G09" />
								<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />

								<gc:isCalculationOn
									rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_{$rsdictRef_value_no_namespace}_{generate-id()}_{$id}" />

							</owl:NamedIndividual>

							<owl:NamedIndividual
								rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_{$rsdictRef_value_no_namespace}_{generate-id()}_{$id}">
								<rdf:type
									rdf:resource="http://ontochem.theworldavatar.com/kb/OntoChem.owl#RotationalSymmetry" />
								<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
								<kb:hasRotationalSymmetryNumber>
									<xsl:value-of select="." />
								</kb:hasRotationalSymmetryNumber>

								<!--if value of unit is 'none' then do not transform it into Abox 
									assertion -->
								<xsl:if test="$rsunit_value_no_namespace!='none'">
									<gc:hasUnit rdf:resource="http://purl.org/gc/{$rsunit_value_no_namespace}" />
								</xsl:if>

							</owl:NamedIndividual>

							<!-- If value of unit is 'none' then do not transform it into Abox 
								assertion. -->

							<xsl:if test="$rsunit_value_no_namespace!='none'">
								<owl:NamedIndividual
									rdf:about="http://purl.org/gc/{$rsunit_value_no_namespace}">
									<rdf:type rdf:resource="http://data.nasa.gov/qudt/owl/qudt#Unit" />
									<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
								</owl:NamedIndividual>
							</xsl:if>

						</xsl:for-each>

					</xsl:if>

					<!-- Transformation rotational constants property -->

					<xsl:if test="@dictRef='cc:rotational_constants'">

						<xsl:variable name="rcdictRef_value">
							<xsl:value-of select="@dictRef" />
						</xsl:variable>

						<xsl:variable name="rcdictRef_value_no_namespace">
							<xsl:value-of select="substring-after($rcdictRef_value,'cc:')" />
						</xsl:variable>

						<xsl:for-each select="*[local-name()='array']">

							<xsl:variable name="rconst_unit">
								<xsl:value-of select="@units" />
							</xsl:variable>

							<xsl:variable name="rconst_unit_value_no_namespace">
								<xsl:value-of select="substring-after($rconst_unit,':')" />
							</xsl:variable>

							<xsl:variable name="rotational_constants_size">
								<xsl:value-of select="@size" />
							</xsl:variable>

							<owl:NamedIndividual
								rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_of_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">

								<rdf:type rdf:resource="http://ontochem.theworldavatar.com/kb/OntoChem.owl#G09"/>
								<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>

								<gc:isCalculationOn
									rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_{$rcdictRef_value_no_namespace}_{generate-id()}_{$id}"/>

							</owl:NamedIndividual>

							<owl:NamedIndividual
								rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_{$rcdictRef_value_no_namespace}_{generate-id()}_{$id}">
								<rdf:type
									rdf:resource="http://ontochem.theworldavatar.com/kb/OntoChem.owl#RotationalConstants"/>
								<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing"/>

								<kb:hasRotationalConstantsValue>
									<xsl:value-of select="."/>
								</kb:hasRotationalConstantsValue>

								<kb:hasRotationalConstantsCount>
									<xsl:value-of select="$rotational_constants_size" />
								</kb:hasRotationalConstantsCount>

								<xsl:if test="$rconst_unit_value_no_namespace='GHZ'">
									<gc:hasUnit rdf:resource="http://data.nasa.gov/qudt/owl/unit#GigaHertz"/>
								</xsl:if>

							</owl:NamedIndividual>

						</xsl:for-each>

					</xsl:if>

				</xsl:for-each>

				<!-- Definition of variable for spinMultiplicity by applying template. -->

				<xsl:variable name="spin_multiplicity_variable">
					<xsl:apply-templates select="*[local-name() = 'molecule']" />
				</xsl:variable>

				<!-- Transformation of the geometry of the molecule -->

				<xsl:if test="*[local-name()='molecule']">

					<xsl:variable name="vid">
						<xsl:value-of select="@id" />
					</xsl:variable>

					<owl:NamedIndividual
						rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_parent_of_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">
						<rdf:type rdf:resource="http://ontochem.theworldavatar.com/kb/OntoChem.owl#G09" />
						<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />

						<gc:isCalculationOn
							rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_geometry_optimization_{generate-id()}_{$id}" />

					</owl:NamedIndividual>

					<owl:NamedIndividual
						rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_geometry_optimization_{generate-id()}_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/GeometryOptimization" />
						<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
						<gc:hasMolecule
							rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$id}" />
					</owl:NamedIndividual>

					<!-- 
					Creates rdf node that is instance of gc:Molecule class. The node 
					contains spin multiplicity value over data type property relation 'hasSpinMultiplicityValue'. 
					-->
					
					<xsl:choose>
                    <xsl:when test="string-length($spin_multiplicity_variable)>0">
					<owl:NamedIndividual
						rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/Molecule" />
						<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
						<kb:hasSpinMultiplicityValue>
							<xsl:value-of select="$spin_multiplicity_variable" /></kb:hasSpinMultiplicityValue>
					</owl:NamedIndividual>
					</xsl:when>
					</xsl:choose>


					<!-- Iterates over atom tags in order to transform atom's attribute 
						value such as z3, y3, x3, id, elementType. -->

					<xsl:for-each select="molecule/atomArray/*[local-name() = 'atom']">

						<xsl:variable name="vid">
							<xsl:value-of select="@id" />
						</xsl:variable>
						<xsl:variable name="velementType">
							<xsl:value-of select="@elementType" />
						</xsl:variable>
						<xsl:variable name="x3">
							<xsl:value-of select="@x3" />
						</xsl:variable>
						<xsl:variable name="y3">
							<xsl:value-of select="@y3" />
						</xsl:variable>
						<xsl:variable name="z3">
							<xsl:value-of select="@z3" />
						</xsl:variable>
						
						<xsl:variable name="atomicMass">
							<xsl:value-of select="@atomicMass" />
						</xsl:variable>
						
						<owl:NamedIndividual
							rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$id}">
							<gc:hasAtom
								rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_atom_{$velementType}{$vid}_{generate-id()}_{$id}" />

						</owl:NamedIndividual>

						<!--Created rdf node that is instance of gc:Atom class.-->

						<owl:NamedIndividual
							rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_atom_{$velementType}{$vid}_{generate-id()}_{$id}">
							<rdf:type rdf:resource="http://purl.org/gc/Atom" />
							<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
							<gc:isElement
								rdf:resource="http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#{$velementType}" />
							<gc:hasAtomCoordinateX
								rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_x3_{$velementType}{$vid}_{generate-id()}_{$id}" />
							<gc:hasAtomCoordinateY
								rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_y3_{$velementType}{$vid}_{generate-id()}_{$id}" />
							<gc:hasAtomCoordinateZ
								rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_z3_{$velementType}{$vid}_{generate-id()}_{$id}" />
							
							<gc:hasMass rdf:resource="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_mass_{$velementType}{$vid}_{generate-id()}_{$id}"/>
							
						</owl:NamedIndividual>
						
						
						<owl:NamedIndividual
							rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_mass_{$velementType}{$vid}_{generate-id()}_{$id}">
							<rdf:type rdf:resource="http://purl.org/gc/FloatValue" />
							<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
							<gc:hasValue>
								<xsl:value-of select="$atomicMass"/>
							</gc:hasValue>
							<gc:hasUnit rdf:resource="http://data.nasa.gov/qudt/owl/unit#Dalton"/>
						</owl:NamedIndividual>
						

						<owl:NamedIndividual
							rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_x3_{$velementType}{$vid}_{generate-id()}_{$id}">
							<rdf:type rdf:resource="http://purl.org/gc/FloatValue" />
							<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
							<gc:hasValue>
								<xsl:value-of select="$x3" />
							</gc:hasValue>
						</owl:NamedIndividual>

						<owl:NamedIndividual
							rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_y3_{$velementType}{$vid}_{generate-id()}_{$id}">
							<rdf:type rdf:resource="http://purl.org/gc/FloatValue" />
							<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
							<gc:hasValue>
								<xsl:value-of select="$y3" />
							</gc:hasValue>
						</owl:NamedIndividual>

						<owl:NamedIndividual
							rdf:about="http://como.cheng.cam.ac.uk/molhub/compchem/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_z3_{$velementType}{$vid}_{generate-id()}_{$id}">
							<rdf:type rdf:resource="http://purl.org/gc/FloatValue" />
							<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
							<gc:hasValue>
								<xsl:value-of select="$z3" />
							</gc:hasValue>
						</owl:NamedIndividual>

						<owl:NamedIndividual
							rdf:about="http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#{$velementType}">
							<rdf:type
								rdf:resource="http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#Element" />
							<rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Thing" />
						</owl:NamedIndividual>

					</xsl:for-each>

				</xsl:if>

			</xsl:when>

		</xsl:choose>

	</xsl:template>

	<!-- Read value of 'dictRef' attribute -->

	<xsl:template match="@dictRef">
		<xsl:value-of select="." />
	</xsl:template>

	<xsl:template match="*[local-name() = 'molecule']">
		<xsl:value-of select="@spinMultiplicity" />
	</xsl:template>

    <xsl:template match="*[local-name() = 'scalar']">
		<xsl:value-of select="." />
	</xsl:template>
	
</xsl:transform>