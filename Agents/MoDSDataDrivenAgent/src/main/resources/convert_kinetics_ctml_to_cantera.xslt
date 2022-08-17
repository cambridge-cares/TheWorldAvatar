<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">

	<!-- Strip leading space to reduce file size, speed up processing and improve robustness -->
	<xsl:strip-space elements="*" />

	<!-- Pretty print settings -->
	<xsl:output method="xml" encoding="utf-8" indent="yes" xslt:indent-amount="4" xmlns:xslt="http://xml.apache.org/xslt" />

	<!-- Copies all other nodes without change (identity transform) -->
	<xsl:template match="@*|node()">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()"/>
		</xsl:copy>
	</xsl:template>

	<!-- Select reaction nodes and test to check whether it is a PLOG reaction; 
  the fact that we have to test a child node and therefore cannot just select the PLOG reactions indicates a design flaw in how PLOG reactions are encoded -->
	<xsl:template match="/ctml/reactionData/reaction[@type='falloff']">
		<xsl:choose>
			<xsl:when test="rateCoeff/falloff[@type='PLOG']">
				<!-- This is a PLOG reaction-->
				<xsl:copy>
					<!-- Replace type attribute; copy everything else except the rateCoeff code (we will replace this) -->
					<xsl:attribute name="type">plog</xsl:attribute>
					<xsl:apply-templates select="@*[name()!='type']|node()[not(self::rateCoeff)]"/>
					<xsl:for-each select="rateCoeff">
						<xsl:copy>
							<!-- Replace rateCoeff node; copy everything else except the Arrhenius (we will replace these) and falloff nodes -->
							<xsl:apply-templates select="@*|node()[not(self::Arrhenius) and not(self::falloff)]"/>
							<xsl:for-each select="Arrhenius">
								<xsl:copy>
									<!-- Replace Arrhenius nodes; copy everything else except the name attribute -->
									<xsl:apply-templates select="@*[name()!='name']|node()"/>
								</xsl:copy>
							</xsl:for-each>
						</xsl:copy>
					</xsl:for-each>
				</xsl:copy>
			</xsl:when>
			<xsl:otherwise>
				<xsl:copy>
					<xsl:apply-templates select="@*|node()"/>
				</xsl:copy>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

</xsl:stylesheet>