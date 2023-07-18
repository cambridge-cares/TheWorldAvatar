<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
  xmlns:b="http://www.ncbi.nlm.nih.gov"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" />

  <xsl:template match="/">

    <!-- output the header row, the &#xA; is a newline character -->
    <xsl:text>compound_id,charge,compound_complexity,count_hydrogen_bond_acceptor,count_hydrogen_bond_donor,count_rotatable_bond,fingerprint,iupac_name,inchi_standard,inchi_key,log_p,exact_mass,molecular_formula,molecular_weight,tpsa,canonical_smiles,count_heavy_atoms,count_def_atom_stereo,count_undef_atom_setereo,count_def_bond_stereo,count_undef_bond_setereo,count_isotope_atom,count_covalent_unit&#xA;</xsl:text>

    <!-- run book template below-->
    <xsl:apply-templates select="/b:PC-Compounds/b:PC-Compound" />
  </xsl:template>

  <xsl:template match="/b:PC-Compounds/b:PC-Compound">
    <!--select the cid-->
    <xsl:text>CID</xsl:text>
     <xsl:value-of select="./b:PC-Compound_id/b:PC-CompoundType/b:PC-CompoundType_id/b:PC-CompoundType_id_cid"/>
    <!-- add comma -->
     <xsl:text>,</xsl:text>
     <!--select the charge value-->
     <xsl:value-of select="./b:PC-Compound_charge"/>
     <xsl:text>,</xsl:text>
     <!--select Compound Complexity-->
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'Compound Complexity']/b:PC-InfoData_value/b:PC-InfoData_value_fval"/>
     <xsl:text>,</xsl:text>
     <!--select Hydrogen Bond Acceptor Count-->
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'Count' and b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_name = 'Hydrogen Bond Acceptor']/b:PC-InfoData_value/b:PC-InfoData_value_ival"/>
     <xsl:text>,</xsl:text>
     <!--select Hydrogen Bond Donor Count-->
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'Count' and b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_name = 'Hydrogen Bond Donor']/b:PC-InfoData_value/b:PC-InfoData_value_ival"/>
     <xsl:text>,</xsl:text>
     <!--select Rotatable Bond Count-->
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'Count' and b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_name = 'Rotatable Bond']/b:PC-InfoData_value/b:PC-InfoData_value_ival"/>
     <xsl:text>,</xsl:text>
     <!--select Fingerprint-->
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'Fingerprint' and b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_name = 'SubStructure Keys']/b:PC-InfoData_value/b:PC-InfoData_value_binary"/>
     <xsl:text>,</xsl:text>
     <!--select preferred IUPAC name-->
         <!--add quotation for name value-->
      <xsl:call-template name="escape_quotes">
       <xsl:with-param name="text" select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'IUPAC Name' and b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_name = 'Preferred']/b:PC-InfoData_value/b:PC-InfoData_value_sval" />
      </xsl:call-template>
     <xsl:text>,</xsl:text>
     <!--select standard IUPAC InChI-->
          <!--add quotation for name value-->
     <xsl:text>"</xsl:text>
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'InChI' and b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_name = 'Standard']/b:PC-InfoData_value/b:PC-InfoData_value_sval"/>
     <xsl:text>"</xsl:text>
     <xsl:text>,</xsl:text>
     <!--select standard InChIKey name-->
          <!--add quotation for name value-->
      
     <xsl:text>"</xsl:text>
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'InChIKey' and b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_name = 'Standard']/b:PC-InfoData_value/b:PC-InfoData_value_sval"/>
     <xsl:text>"</xsl:text>
     <xsl:text>,</xsl:text>
     <!--select Log P value-->
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'Log P']/b:PC-InfoData_value/b:PC-InfoData_value_fval"/>
     <xsl:text>,</xsl:text>
     <!--select Exact Mass value-->
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'Mass' and b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_name = 'Exact']/b:PC-InfoData_value/b:PC-InfoData_value_sval"/>
     <xsl:text>,</xsl:text>
     <!--select molecular formula-->
          <!--add quotation for name value-->
     <xsl:text>"</xsl:text>
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'Molecular Formula']/b:PC-InfoData_value/b:PC-InfoData_value_sval"/>
     <xsl:text>"</xsl:text>
     <xsl:text>,</xsl:text>     
     <!--select molecular weight-->
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'Molecular Weight']/b:PC-InfoData_value/b:PC-InfoData_value_sval"/>
     <xsl:text>,</xsl:text>
     <!--select Topological Polar Surface Area-->
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'Topological' and b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_name = 'Polar Surface Area']/b:PC-InfoData_value/b:PC-InfoData_value_fval"/>
     <xsl:text>,</xsl:text>
     <!--select canonical SMILES-->
          <!--add quotation for name value-->
     <xsl:text>"</xsl:text>
     <xsl:value-of select="./b:PC-Compound_props/b:PC-InfoData[b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_label = 'SMILES' and b:PC-InfoData_urn/b:PC-Urn/b:PC-Urn_name = 'Canonical']/b:PC-InfoData_value/b:PC-InfoData_value_sval"/>
     <xsl:text>"</xsl:text>
     <xsl:text>,</xsl:text>
     <!--select Count heavy atoms-->
     <xsl:value-of select="./b:PC-Compound_count/b:PC-Count/b:PC-Count_heavy-atom"/>
     <xsl:text>,</xsl:text>
     <!--select Count Defined Atom Setereo-->
     <xsl:value-of select="./b:PC-Compound_count/b:PC-Count/b:PC-Count_atom-chiral-def"/>
     <xsl:text>,</xsl:text>
     <!--select Count Undefined Atom Setereo-->
     <xsl:value-of select="./b:PC-Compound_count/b:PC-Count/b:PC-Count_atom-chiral-undef"/>
     <xsl:text>,</xsl:text>
     <!--select Count Defined Bond Setereo-->
     <xsl:value-of select="./b:PC-Compound_count/b:PC-Count/b:PC-Count_bond-chiral-def"/>
     <xsl:text>,</xsl:text>
     <!--select Count Undefined Bond Setereo-->
     <xsl:value-of select="./b:PC-Compound_count/b:PC-Count/b:PC-Count_bond-chiral-undef"/>
     <xsl:text>,</xsl:text>     
     <!--select Count Isotope Atoms-->
     <xsl:value-of select="./b:PC-Compound_count/b:PC-Count/b:PC-Count_isotope-atom"/>
     <xsl:text>,</xsl:text> 
     <!--select Count Covalent Unit-->
     <xsl:value-of select="./b:PC-Compound_count/b:PC-Count/b:PC-Count_covalent-unit"/>   

     <!--add a new line-->
     <xsl:text>&#xA;</xsl:text>
  </xsl:template>

<xsl:template name="escape_quotes">
  <xsl:param name="text" />
  <xsl:text>"</xsl:text>
      <xsl:call-template name="string-replace-all">
        <xsl:with-param name="text" select="$text" />
        <xsl:with-param name="replace" select="'&#34;'" />
        <xsl:with-param name="by" select="'&#34;&#34;'" />
      </xsl:call-template>
  <xsl:text>"</xsl:text>
</xsl:template>

<xsl:template name="string-replace-all">
  <xsl:param name="text" />
  <xsl:param name="replace" />
  <xsl:param name="by" />
  <xsl:choose>
    <xsl:when test="contains($text, $replace)">
      <xsl:value-of select="substring-before($text,$replace)" />
      <xsl:value-of select="$by" />
      <xsl:call-template name="string-replace-all">
        <xsl:with-param name="text" select="substring-after($text,$replace)" />
        <xsl:with-param name="replace" select="$replace" />
        <xsl:with-param name="by" select="$by" />
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$text" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>







</xsl:stylesheet>