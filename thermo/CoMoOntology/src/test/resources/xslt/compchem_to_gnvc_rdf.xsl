<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="2.0" xmlns=""
	xml:base="https://como.cheng.cam.ac.uk/kb/ontokin/" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:targetNamespace="http://www.xml-cml.org/schema"
	xmlns:cc="http://www.xml-cml.org/dictionary/compchem/"
	xmlns:conventions="http://www.xml-cml.org/convention/" xmlns:nonSi="http://www.xml-cml.org/unit/nonSi/"
	xmlns:gc="http://purl.org/gc/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" xmlns:ontokin="https://como.cheng.cam.ac.uk/kb/ontokin/"
	xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:math="java.lang.Math"
	exclude-result-prefixes="math">

	<!-- Author: Nenad B. Krdzavac, e-mail: nk510(at)cam.ac.uk -->

	<!-- Copyright: Department of Chemical Engineering and Biotechnology, University 
		of Cambrdige, United Kingdom, 2018 -->

	<!-- This xslt transforms of CompChem XML files to rdf graph as instance 
		of Gainesville Core Ontology (GNVC) ver 0.7. At the moment it covers the 
		transformation of the following features: 1. formula name 2. atom counts 
		and atom element type. 3. frequency unit, frequency values. 4. rotational 
		symmetry value, rotational symmetry unit. 5. rotational constants value, 
		rotational constants units, rotational constants size. 6. geometry type, 
		spin multiplicity. -->

	<!-- Applying all templates -->


	<xsl:template match="/">


		<rdf:RDF>
			<xsl:apply-templates select="module/module/*[local-name() = 'module']" />
			<xsl:apply-templates
				select="module/module/module/*[local-name() = 'module']" />
		</rdf:RDF>

	</xsl:template>

	<xsl:variable name="id">
		<xsl:value-of select="(math:random()*100000)  + 1" />
	</xsl:variable>
	<!-- This template matches node that has cc:jobList and cc:job attributes 
		value. It creates root and its child rdf nodes for molecular system by using 
		current node name and dirctRef attribute value of current node. -->
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

		<!-- Creates root rdf node that is instance of 'G09' ontology class. It 
			also creates property relation between root node and node that has a tree 
			structure of information stored in initialization module. -->
		<owl:NamedIndividual
			rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">
			<rdf:type rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/G09" />

			<ontokin:hasInitialization
				rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_initilization_module_{$id}" />

		</owl:NamedIndividual>


	</xsl:template>

	<!-- This template transforms all information that are available in initialization 
		and finalization modules of CompChem xml file. -->
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

			<!-- Transformation of information about molecule stored in initialization 
				module of CompChem xml file. -->
			<xsl:when test="$module_type='cc:initialization'">

				<!-- -->
				<owl:NamedIndividual
					rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_parent_no_namespace}_{$vmodule}_has_initilization_module_{$id}">
					<rdf:type rdf:resource="http://purl.org/gc/MethodologyFeature" />

					<gc:hasMoleculeProperty
						rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property_{$id}" />

				</owl:NamedIndividual>

				<rdf:Description
					rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property_{$id}">
					<rdf:type rdf:resource="http://purl.org/gc/MoleculeProperty" />
				</rdf:Description>

				<!-- Read value of 'count' and 'elementType' attributes for a molecule 
					as attribute values of tag 'atom' and creates rdf graph for each atom by 
					using Periodic Table ontology. -->

				<xsl:for-each select="molecule/atomArray/*[local-name() = 'atom']">


					<xsl:variable name="velementType">
						<xsl:value-of select="@elementType" />
					</xsl:variable>

					<xsl:variable name="atom_count">
						<xsl:value-of select="@count" />
					</xsl:variable>


					<owl:NamedIndividual
						rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/MoleculeProperty" />

						<gc:hasMolecule
							rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$velementType}{$atom_count}_{generate-id()}_{$id}" />

					</owl:NamedIndividual>


					<owl:NamedIndividual
						rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$velementType}{$atom_count}_{generate-id()}_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/Molecule" />

						<!-- URI for chemical elements implemented in Periodic table ontology 
							are not available on the web. For example: http://http://daml.org/2003/01/periodictable/PeriodicTable#Ti 
							does not give any result if we run it in a browser. -->

						<gc:hasAtom
							rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/atom_{$velementType}{$atom_count}_{generate-id()}_{$id}" />

						<gc:hasNumberOfAtoms>
							<xsl:value-of select="substring-before(@count,'.0')" />
						</gc:hasNumberOfAtoms>

					</owl:NamedIndividual>

					<owl:NamedIndividual
						rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/atom_{$velementType}{$atom_count}_{generate-id()}_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/Atom" />

						<gc:isElement
							rdf:resource="http://www.daml.org/2003/01/periodictable/PeriodicTable#{$velementType}" />

					</owl:NamedIndividual>

					<owl:NamedIndividual
						rdf:about="http://www.daml.org/2003/01/periodictable/PeriodicTable#{$velementType}">
						<rdf:type
							rdf:resource="http://www.daml.org/2003/01/periodictable/PeriodicTable#Element" />
					</owl:NamedIndividual>

				</xsl:for-each>

				<!-- Transformation of formula name available as 'concise' attribute 
					value in CompChem xml file. -->
				<xsl:for-each select="molecule/*[local-name() = 'formula']">

					<owl:NamedIndividual
						rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_property_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/MoleculeProperty" />

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
									rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_parent_of_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">

									<rdf:type rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/G09" />

									<gc:isCalculationOn
										rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_{$vdictRef_value_no_namespace}_{generate-id()}_{$id}" />

								</owl:NamedIndividual>


								<owl:NamedIndividual
									rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_{$vdictRef_value_no_namespace}_{generate-id()}_{$id}">

									<rdf:type rdf:resource="http://purl.org/gc/VibrationalAnalysis" />

									<gc:hasResult
										rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_{$vdictRef_value_no_namespace}_{$fdictRef_value_no_namespace}_{generate-id()}_{$id}" />

								</owl:NamedIndividual>

								<owl:NamedIndividual
									rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_{$vdictRef_value_no_namespace}_{$fdictRef_value_no_namespace}_{generate-id()}_{$id}">

									<rdf:type rdf:resource="http://purl.org/gc/Frequency" />

									<ontokin:hasFrequenciesValue>
										<xsl:value-of select="." />
									</ontokin:hasFrequenciesValue>

									<gc:hasUnit rdf:resource="http://purl.org/gc/{$funit_value_no_namespace}" />

									<gc:hasVibrationCount>
										<xsl:value-of select="$frequencies_size" />
									</gc:hasVibrationCount>

								</owl:NamedIndividual>

								<owl:NamedIndividual
									rdf:about="http://purl.org/gc/{$funit_value_no_namespace}">
									<rdf:type rdf:resource="http://data.nasa.gov/qudt/owl/qudt#FrequencyUnit" />
								</owl:NamedIndividual>

							</xsl:if>

						</xsl:for-each>

					</xsl:if>

					<!-- transformation geometry type property -->

					<xsl:if test="@dictRef='cc:geometry_type'">


						<xsl:variable name="gdictRef_value">
							<xsl:value-of select="@dictRef" />
						</xsl:variable>

						<xsl:variable name="gdictRef_value_no_namespace">
							<xsl:value-of select="substring-after($gdictRef_value,'cc:')" />
						</xsl:variable>

						<owl:NamedIndividual
							rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_parent_of_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">

							<rdf:type rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/G09" />

							<gc:isCalculationOn
								rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_{$gdictRef_value_no_namespace}_{generate-id()}_{$id}" />

						</owl:NamedIndividual>

						<owl:NamedIndividual
							rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_{$gdictRef_value_no_namespace}_{generate-id()}_{$id}">
							<rdf:type rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/GeometryType" />
							<ontokin:hasGeometryTypeValue>
								<xsl:value-of select="." />
							</ontokin:hasGeometryTypeValue>
						</owl:NamedIndividual>


					</xsl:if>


					<!-- transformation rotational symmetry property -->

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
								rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_parent_of_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">

								<rdf:type rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/G09" />

								<gc:isCalculationOn
									rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_{$rsdictRef_value_no_namespace}_{generate-id()}_{$id}" />

							</owl:NamedIndividual>

							<owl:NamedIndividual
								rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_{$rsdictRef_value_no_namespace}_{generate-id()}_{$id}">
								<rdf:type
									rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/RotationalSymmetry" />
								<ontokin:hasRotationalSymmetryNumber>
									<xsl:value-of select="." />
								</ontokin:hasRotationalSymmetryNumber>

								<!--if value of unit is 'none' then do not transform it into Abox 
									assertion -->
								<xsl:if test="$rsunit_value_no_namespace!='none'">
									<gc:hasUnit rdf:resource="http://purl.org/gc/{$rsunit_value_no_namespace}" />
								</xsl:if>

							</owl:NamedIndividual>

							<!--if value of unit is 'none' then do not transform it into Abox 
								assertion -->
							<xsl:if test="$rsunit_value_no_namespace!='none'">
								<owl:NamedIndividual
									rdf:about="http://purl.org/gc/{$rsunit_value_no_namespace}">
									<rdf:type rdf:resource="http://data.nasa.gov/qudt/owl/qudt#Unit" />
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
								rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_parent_of_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">

								<rdf:type rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/G09" />

								<gc:isCalculationOn
									rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_{$rcdictRef_value_no_namespace}_{generate-id()}_{$id}" />

							</owl:NamedIndividual>

							<owl:NamedIndividual
								rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_{$rcdictRef_value_no_namespace}_{generate-id()}_{$id}">
								<rdf:type
									rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/RotationalConstants" />

								<ontokin:hasRotationalConstantsValue>
									<xsl:value-of select="." />
								</ontokin:hasRotationalConstantsValue>

								<ontokin:hasRotationalConstantsCount>
									<xsl:value-of select="$rotational_constants_size" />
								</ontokin:hasRotationalConstantsCount>

								<xsl:if test="$rconst_unit_value_no_namespace='GHZ'">
									<gc:hasUnit rdf:resource="http://data.nasa.gov/qudt/owl/unit#GigaHertz" />
								</xsl:if>

							</owl:NamedIndividual>

						</xsl:for-each>

					</xsl:if>

				</xsl:for-each>

                <!-- definition of variable for spinMultiplicity by applying template. -->
				<xsl:variable name="spin_multiplicity_variable">
					<xsl:apply-templates select="*[local-name() = 'molecule']" />
				</xsl:variable>

				<!-- Transformation of the geometry of the molecule -->
				<xsl:if test="*[local-name()='molecule']">

					<xsl:variable name="vid">
						<xsl:value-of select="@id" />
					</xsl:variable>

					<owl:NamedIndividual
						rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_parent_of_parent_no_namespace}_{$vmodule}_molecular_methоdology_{$id}">
						<rdf:type rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/G09" />

						<gc:isCalculationOn
							rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_geometry_optimization_{generate-id()}_{$id}" />

					</owl:NamedIndividual>

					<owl:NamedIndividual
						rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_geometry_optimization_{generate-id()}_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/GeometryOptimization" />
						<gc:hasMolecule
							rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$id}" />
					</owl:NamedIndividual>

					<owl:NamedIndividual
						rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$id}">
						<rdf:type rdf:resource="http://purl.org/gc/Molecule" />
						<ontokin:hasSpinMultiplicityValue>
							<xsl:value-of select="$spin_multiplicity_variable" />
						</ontokin:hasSpinMultiplicityValue>
					</owl:NamedIndividual>


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

						<owl:NamedIndividual
							rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_molecule_{$id}">
							<gc:hasAtom
								rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_atom_{$velementType}{$vid}_{generate-id()}_{$id}" />

						</owl:NamedIndividual>

						<owl:NamedIndividual
							rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_atom_{$velementType}{$vid}_{generate-id()}_{$id}">
							<rdf:type rdf:resource="http://purl.org/gc/Atom" />
							<gc:isElement
								rdf:resource="http://www.daml.org/2003/01/periodictable/PeriodicTable#{$velementType}" />
							<gc:hasAtomCoordinateX
								rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_x3_{$velementType}{$vid}_{generate-id()}_{$id}" />
							<gc:hasAtomCoordinateY
								rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_y3_{$velementType}{$vid}_{generate-id()}_{$id}" />
							<gc:hasAtomCoordinateZ
								rdf:resource="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_z3_{$velementType}{$vid}_{generate-id()}_{$id}" />
						</owl:NamedIndividual>

						<owl:NamedIndividual
							rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_x3_{$velementType}{$vid}_{generate-id()}_{$id}">
							<rdf:type rdf:resource="http://purl.org/gc/FloatValue" />
							<gc:hasValue>
								<xsl:value-of select="$x3" />
							</gc:hasValue>
						</owl:NamedIndividual>

						<owl:NamedIndividual
							rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_y3_{$velementType}{$vid}_{generate-id()}_{$id}">
							<rdf:type rdf:resource="http://purl.org/gc/FloatValue" />
							<gc:hasValue>
								<xsl:value-of select="$y3" />
							</gc:hasValue>
						</owl:NamedIndividual>

						<owl:NamedIndividual
							rdf:about="https://como.cheng.cam.ac.uk/kb/ontokin/{$vdictRef_no_namespace}_{$vmodule}_has_coordinate_z3_{$velementType}{$vid}_{generate-id()}_{$id}">
							<rdf:type rdf:resource="http://purl.org/gc/FloatValue" />
							<gc:hasValue>
								<xsl:value-of select="$z3" />
							</gc:hasValue>
						</owl:NamedIndividual>


						<owl:NamedIndividual
							rdf:about="http://www.daml.org/2003/01/periodictable/PeriodicTable#{$velementType}">
							<rdf:type
								rdf:resource="http://www.daml.org/2003/01/periodictable/PeriodicTable#Element" />
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

</xsl:transform>



