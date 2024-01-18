
import os
import requests
from bs4 import BeautifulSoup
import json
import zeolist
import time
import re

IZADATA = "izadata"

class IzaFramework:
    #__slots__ = [ "", "", "", "", "", "", "", "",  ]

    def __init__( self ):
        self.prepare()
        pass # IzaFramework.__init__()

    def prepare( self ):
        if not os.path.isdir( IZADATA ):
            os.makedirs( IZADATA )

        for theme in [ "coord", "volume", "frame", "tatom", "refmat", "mater" ]:
            path = os.path.join( IZADATA, theme )
            if not os.path.isdir( path ):
                os.makedirs( path )

        pass # IzaFramework.prepare()
        

    def getFrameworkData( self, name, iName = None ):

        name2 = zeolist.zeoCodeToCode3( name )
        if None != iName:
            #print( "Starting ", name, iName )
            pass
        else:
            print( "Starting ", name )
            pass

        # Function returns a dict() with all data
        # Format o fthe dictionary is not clear yet, will be adjusted over iterations.
        output = dict()

        # Get the Coord System html files (for t-atoms):
        html = self.getHtmlAsText( "coord", name2 )
        #print( html )

        # Extract T atoms (the main table):
        table = self.getTAtomTable( html, name )

        atoms = self.getTAtoms( table )
        #print( "atoms =", atoms, type(atoms) )

        output["Framework"] = name
        output["TAtoms"   ] = atoms["TAtoms"]

        # Get the Framework html page:
        html = self.getHtmlAsText( "frame", name2 )

        # Extract framework data (the main table):
        tables = self.getFrameworkTables( html, name )

        value = self.getCellParam  ( tables )
        if "CellParameters" in value:
            output["CellParameters"] = value["CellParameters"]
        else:
            print( "No data for CellParameter for framework", name )

        value = self.getFWDensity  ( tables )
        if "FrameworkDensity" in value:
            output["FrameworkDensity"] = value["FrameworkDensity"]
        else:
            print( "No data for FrameworkDensity for framework", name )

        value = self.getTopoDensity( tables )
        if "TopologicalDensity" in value:
            output["TopologicalDensity"] = value["TopologicalDensity"]
        else:
            print( "No data for TopologicalDensity for framework", name )


        value = self.getRingSize   ( tables )
        if "RingSizes" in value:
            output["RingSizes"] = value["RingSizes"]
        else:
            print( "No data for RingSizes for framework", name )

        value = self.getChannelDim ( tables )
        if "ChannelDimensions" in value:
            output["ChannelDimensions"] = value["ChannelDimensions"]
        else:
            print( "No data for ChannelDimensions for framework", name )


        value = self.getSphereDiam ( tables )
        if "SphereDiameter" in value:
            output["SphereDiameter"] = value["SphereDiameter"]
        else:
            print( "No data for SphereDiameter for framework", name )


        value = self.getAccessVolume( tables )
        if "AccessVolume" in value:
            output["AccessVolume"] = value["AccessVolume"]
        else:
            print( "No data for AccessVolume for framework", name )


        value = self.getSecondaryBU( tables )
        if "SecondaryBU" in value:
            output["SecondaryBU"] = value["SecondaryBU"]
        else:
            print( "No data for SecondaryBU for framework", name )


        value = self.getCompositeBU( tables )
        if "CompositeBU" in value:
            output["CompositeBU"] = value["CompositeBU"]
        else:
            print( "No data for CompositeBU for framework", name )


        value = self.getNaturalTile( tables )
        if "NaturalTiles" in value:
            output["NaturalTiles"] = value["NaturalTiles"]
        else:
            print( "No data for NaturalTiles for framework", name )



        # Get the Coord System html files (for t-atoms):
        html = self.getHtmlAsText( "volume", name2 )
        #print( html )

        # Extract T atoms (the main table):
        table = self.getVolumeTable( html, name )

        volume = self.getVolumeArea( table, name )
        #print( "atoms =", atoms, type(atoms) )

        #output["TAtoms"   ] = atoms["TAtoms"]


        html = self.getHtmlAsText( "refmat", name2 )
        html = self.getHtmlAsText(  "mater", name2 )
        html = self.getHtmlAsText(  "tatom", name2 )
        """
        """

        return output

        # Extract individual materials and references:
        
        return output
        pass # IzaFramework.getFrameworkData()

    def getVolumeArea( self, table, name ):
        output = dict()

        value = self.getVolProperty( table, "Occupiable Volume", name )
        if not value:
            print( "No data for occup. vol. for framework", name )
        else:
            output |= value

        value = self.getVolProperty( table, "Accessible Volume", name )
        if not value:
            print( "No data for access. vol. for framework", name )
        else:
            output |= value

        value = self.getVolProperty( table, "Occupiable Area", name )
        if not value:
            print( "No data for occup. area. for framework", name )
        else:
            output |= value

        value = self.getVolProperty( table, "Accessible Area", name )
        if not value:
            print( "No data for access. area. for framework", name )
        else:
            output |= value

        value = self.getVolProperty( table, "Specific Occupiable Area", name )
        if not value:
            print( "No data for spec. occup. area. for framework", name )
        else:
            output |= value

        value = self.getVolProperty( table, "Specific Accessible Area", name )
        if not value:
            print( "No data for spec. access. area. for framework", name )
        else:
            output |= value


        """
        value = self.getVolProperty( table, "
Maximum diameter of a sphere:            
    that can be included    5.42    Å       
        that can diffuse along
        """
        #print( output )
        return output
        pass # IzaFramework.getVolumeArea()


    def getVolProperty( self, table, title, name ):
        #print( "Starting getVolProperty() for row", title )
        output = dict()

        key = title.strip().replace( " ", "" )

        tr_els = table.find_all( 'tr' )
        # first row is the header, need to check it. TODO
        #if not
        #print( table )
        #print( "nTr = ", len( tr_els ) )
        #print( "tr = ", tr_els )
        #1/0
        #print( "key =", key )
        for ir,tr in enumerate(tr_els[1:]):
                # WARNING! There is an error (either in webpage, or in parser),
                #          but the first row cannot be extracted separately.
                #          So I just ignore it. Anyway it is a standard header.
                #          Maybe, it is due to colspan=9, but I did not check.

            #print( "ir = ", ir )
            #print( tr )

            td_els = tr.find_all( 'td' )
            """
            if 0 == ir:
                # WARNING! There is an error (either in webpage, or in parser),
                #          but the first row cannot be extracted separately.
                #          So I just ignore it. Anyway it is a standard header.
                #          Maybe, it is due to colspan=9, but I did not check.
                continue # I.e. Ignore this case.

                if len(td_els) != 1:
                    print( "Error! In Volume prop first row must be a single cell" )
                    #print( "      ", len( td_els ), ":", td_els )
                    #print( td_els )
                td = td_els[0]
                if not td.text.strip().lower().startswith( "Volumes and areas".lower() ):
                    print( "In volume prop first row must start with " + \
                           "'Volumes and areas' but got", td, "in", name )
                continue
            """

            if len( td_els ) < 4:
                print( "Error! Possibly too short row in", name )
                continue

            td = td_els[0]
            word = td.text.strip()
            key  = word.strip().replace( " ", "" )
            #print( "word =", word )
            #if word.lower().startswith( title.lower() ) or :
            if   word.lower().startswith( "Occupiable Volume".lower() ):
                #print( "Found", title, "at", ir )
                #print( "      ", output )
                output[key] = dict()

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "A^3"

                td = td_els[3]
                value = td.text.replace("(","").strip()
                output[key]["percent"] = value
                #output[key]["unit"] = "%"
                #print( "After:", output )

            elif word.lower().startswith( "Accessible Volume".lower() ):
                #print( "Found", title, "at", ir )
                #print( "      ", output )
                output[key] = dict()

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "A^3"

                td = td_els[3]
                value = td.text.replace("(","").strip()
                output[key]["percent"] = value
                #output[key]["unit"] = "%"
                #print( "After:", output )

            elif word.lower().startswith( "Occupiable Area".lower() ):
                #print( "Found", title, "at", ir )
                #print( "      ", output )
                output[key] = dict()

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "A^2"
                td = td_els[3]
                value = td.text.replace("(","").strip()
                output[key]["pergram"] = value
                output[key]["pergramunit"] = "m^2/g"
                #print( "After:", output )

            elif word.lower().startswith( "Accessible Area".lower() ):
                #print( "Found", title, "at", ir )
                #print( "      ", output )
                output[key] = dict()

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "A^2"
                td = td_els[3]
                value = td.text.replace("(","").strip()
                output[key]["pergram"] = value
                output[key]["pergramunit"] = "m^2/g"
                #print( "After:", output )

            elif word.lower().startswith( "Specific Occupiable Area".lower() ):
                #print( "Found", title, "at", ir )
                #print( "      ", output )
                output[key] = dict()

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "m^2/cm^3"
                #print( "After:", output )

            elif word.lower().startswith( "Specific Accessible Area".lower() ): 
                #print( "Found", title, "at", ir )
                #print( "      ", output )
                output[key] = dict()

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "m^2/cm^3"
                #print( "After:", output )
            else:
                pass

                 #word.lower().startswith( "".lower() ) or 
                 #word.lower().startswith( "".lower() ) or 
                 #word.lower().startswith( "".lower() ) or 

            #:




        #output[key]["value"] = "AAAAAAAAAAAA"
        #output[key]["unit" ] = "AAAAAAAAAAAA"

        #print( output )
        return output
        pass # IzaFramework.getVolProperty()

    def getTableTitle( self, table ):
        #print( table )
        #print( table.find_all( 'tr' ) )

        #tr_left = table.find_all( 'tr' )[0]
        tr_els = table.find_all( 'tr' )

        # Sometimes the first row is empty, so I check the first two rows (if they exist):
        nTrToCheck = min( len(tr_els), 2 )

        for i in range( nTrToCheck ):
            tr = tr_els[i]
            #print( "tr_left =", tr_left )
            td_els = tr.find_all( 'td' )
            #print( "td_elem =", td_elem )
            if len( td_els ) > 1:
                for td in td_els:
                    #print( td )
                    pass
                title = td_els[1].get_text( )
                title = title.replace( ":", "" )
                return title.strip()

        return ""
        pass # IzaFramework.()

    def getCellParam( self, tables ):
        table = tables[0]
        title = self.getTableTitle( table )
        #print( title )

        output = dict()
        if "cell parameters" == title.lower():
            output["CellParameters"] = dict()
            #output["CellParameters"]["a"] = 

        else:
            print( "Failed to find 'Cell Parameters' section" )
        #value = 
        return output
        pass # IzaFramework.()


    def getFWDensity( self, tables ):
        table = tables[1]
        title = self.getTableTitle( table )
        #print( table )
        #print( title, "expecting FW Density" )

        output = dict()
        if title.lower().startswith( "framework density" ):
            output["FrameworkDensity"] = dict()
            #output["FrameworkDensity"]["a"] = 
            #print( "   >>>> In fw density", table )
            tr_els = table.find_all( 'tr' )
            tr_el = tr_els[0]
            td_els = tr_el.find_all( 'td' )
            for td in td_els[1:]:
                #print( " >>> ", td )
                break
            td = td_els[2].contents
            output["FrameworkDensity"]["value"] = float(td[0].split()[0])
            output["FrameworkDensity"]["unit"]  =   str(td[0].split()[1]) + "A^3"


        else:
            print( "Failed to find 'Framework Density' section" )
 
        return output
        pass # IzaFramework.getFWDensity()

    def getTopoDensity( self, tables ):
        table = tables[1]
        tr_els = table.find_all( 'tr' )
        #print( ">>>>>>>>>", tr_els )
        tr_el = tr_els[1]  # << Topological

        title = tr_el.find_all( 'td' )[1].contents[0]
        #print( "In topo density", title )

        output = dict()
        if title.lower().startswith( "topological density" ):
            output["TopologicalDensity"] = dict()
            #output["TopologicalDensity"]["a"] = 
            #print( "   >>>> In fw density", table )
            tr_els = table.find_all( 'tr' )
            tr_el = tr_els[1]  # << Topological

            td_els = tr_el.find_all( 'td' )
            for td in td_els[0:]:
                #print( " >>> ", td )
                break

            #print( "as text:", td_els[2].text )
            if "TD" == td_els[2].text.split("=")[0].strip():
                td = td_els[2].contents
                #print( td[0], td[1], td[2] )
                #output["TopologicalDensity"]["type"]  = "TD"
                output["TopologicalDensity"]["valueTD"] = float(td[2].replace( "=","") )

            elif "TD10" == td_els[2].text.split("=")[0].strip():
                td = td_els[2].contents
                #print( "---", td, ",", td[2].split("="), ",", td[2].strip()  )
                if len(td[2].strip()) > 0:
                    output["TopologicalDensity"]["valueTD10"] = float(td[2].split("=")[1] )



            if len(td_els) > 3:

                #print( "as text:", td_els[3].text )

                if "TD" == td_els[3].text.split("=")[0].strip():
                    td = td_els[3].contents
                    #print( td, ",", td[0].split("="), ",", td[0].strip()  )
                    #print( td[0], td[1], td[2] )
                    #output["TopologicalDensity"]["type"]  = "TD"
                    output["TopologicalDensity"]["valueTD"] = float(td[0].split("=")[1] )

                elif "TD10" == td_els[3].text.split("=")[0].strip():
                    td = td_els[3].contents
                    #print( td, ",", td[0].split("="), ",", td[0].strip()  )
                    if len(td[0].strip()) > 0:
                        output["TopologicalDensity"]["valueTD10"] = float(td[0].split("=")[1] )

                pass

        # Verification:
        if "valueTD10" in output["TopologicalDensity"]:
            td10 = output["TopologicalDensity"]["valueTD10"] 
            if td10 < 100.:
                print( "Error: td10 may be too small:", td10, type(td10) )

        if "valueTD" in output["TopologicalDensity"]:
            td = output["TopologicalDensity"]["valueTD"] 
            if td > 10.:
                print( "Error: td may be too big:", td, type(td) )

        #print( output )
        return output

        pass # IzaFramework.getTopoDensity()

    def getRingSize   ( self, tables ):

        table = tables[1]
        #print( "Ring size:", table )
        tr_els = table.find_all( 'tr' )
        #print( ">>>>>>>>>", tr_els )
        tr_el = tr_els[2]  # << RingSize

        title = tr_el.find_all( 'td' )[1].contents[0]

        #title = self.getTableTitle( table )
        #print( "In ring size:", title )

        output = dict()
        if title.lower().startswith( "ring sizes" ):
            output["RingSizes"] = list()
            #output["TopologicalDensity"]["a"] = 
            #print( "   >>>> In fw density", table )
            tr_els = table.find_all( 'tr' )
            tr_el = tr_els[2]  # << Ring sizes

            td_els = tr_el.find_all( 'td' )
            for td in td_els[0:]:
                #print( " >>> ", td )
                break
            td = td_els[2].contents
            #print( td, type(td) )
            #print( td[0].split() )
            rings = td[0].split() 
            #output["TopologicalDensity"]["type"]  = "TD"
            for r in rings:
                output["RingSizes"].append( int( r ) )
                #output["RingSizes"]["a"] = int( abc[0] )
                #output["RingSizes"]["b"] = int( abc[1] )
                #output["RingSizes"]["c"] = int( abc[2] )

        #print( output )

        return output

        pass # IzaFramework.getRingSize   ()

    def getChannelDim ( self, tables ):

        table = tables[1]
        #print( "Ring size:", table )
        tr_els = table.find_all( 'tr' )
        #print( ">>>>>>>>>", tr_els )
        tr_el = tr_els[3]  # << ChannelDimension

        title = tr_el.find_all( 'td' )[1].contents[0]

        #title = self.getTableTitle( table )
        #print( "In channel dimension:", title )

        output = dict()
        if title.lower().startswith( "channel dimensionality" ):
            output["ChannelDimensions"] = dict()
            #output["TopologicalDensity"]["a"] = 
            #print( "   >>>> In fw density", table )
            tr_els = table.find_all( 'tr' )
            tr_el = tr_els[3]  # << ChannelDimension 

            td_els = tr_el.find_all( 'td' )
            for td in td_els[0:]:
                #print( " >>> ", td )
                break
            td = td_els[2].contents
            #print( td, type(td) )
            #print( td[0].split() )
            value = td[0]
            #print( value )
            output["ChannelDimensions"] = value

        #print( output )

        return output

        pass # IzaFramework.getChannelDim ()

    def getSphereDiam ( self, tables ):
        table = tables[2]
        title = self.getTableTitle( table )
        #print( "in sphere diam title:", title )

        output = dict()
        if title.lower().startswith( "maximum diameter of a sphere" ):
            output["SphereDiameter"] = dict()
            table = tables[3]
            tr_els = table.find_all( 'tr' )
            #print( tr_els )
            tr_el = tr_els[0]
            td_els = tr_el.find_all( 'td' )
            td = td_els[2].contents
            #print( td )
            words = td[0].split()
            output["SphereDiameter"]["unit"]    = "A"
            output["SphereDiameter"]["included"] = float( words[0] )

            table = tables[4]
            tr_els = table.find_all( 'tr' )
            #print( tr_els )
            tr_el = tr_els[0]
            td_els = tr_el.find_all( 'td' )

            td = td_els[2].contents
            #print( td )
            words = td[0].split()
            output["SphereDiameter"]["a"] = float( words[1] )

            td = td_els[3].contents
            #print( td )
            words = td[0].split()
            output["SphereDiameter"]["b"] = float( words[1] )

            td = td_els[4].contents
            #print( td )
            words = td[0].split()
            output["SphereDiameter"]["c"] = float( words[1] )

        #print( output )
        return output

        pass # IzaFramework.getSphereDiam ()

    def getAccessVolume( self, tables ):
        table = tables[5]
        #print( table )
        title = self.getTableTitle( table )
        #print( title )

        output = dict()
        if title.lower().strip().startswith("accessible volume"): 
            output["AccessVolume"] = dict()

            tr_els = table.find_all( 'tr' )
            #print( tr_els )
            tr_el = tr_els[0]
            td_els = tr_el.find_all( 'td' )
            #print( td_els )
            td = td_els[2].contents[0]
            #print( td )
            words = td.split()
            output["AccessVolume"]["value"] = float( words[0] )
            output["AccessVolume"]["unit"]  = "%"
            
        #print( output )
        return output

        pass # IzaFramework.getAccesVolume()
 
    def getSecondaryBU( self, tables ):

        table = tables[6]
        #print( table )
        title = self.getTableTitle( table )
        #print( "In Secondary BU title:", title )

        output = dict()
        if title.lower().strip().startswith("secondary building units"): 
            output["SecondaryBU"] = dict()

            tr_els = table.find_all( 'tr' )
            #print( tr_els )
            tr_el = tr_els[0]
            td_els = tr_el.find_all( 'td' )
            #print( td_els )
            td = td_els[2].contents[0]
            #print( td )
            output["SecondaryBU"] = []
            words = td.split("or")
            for w in words:
                w2 = w.replace("&nbsp", "").strip()
                output["SecondaryBU"].append( w2 )
            #output["SecondaryBU"]["unit"]  = "%"
            
        #print( output )

        return output

        pass # IzaFramework.getSecondaryBU()

    def getTableByTitle( self, tables, title, minOrder = 0 ):
        """
        Returnd the table and its order (integer number).
        If not found, return (None,None)

        minOrder is optional argument, if the minimum order is known.
        """

        for i in range( minOrder, len(tables) ):
            table = tables[i]
            value = self.getTableTitle( table )
            #print( "value = ", value, ", i =", i )

            if value.lower().strip().startswith( title.lower() ):
                #print( "Found value" )
                return table, i 
        

        return None, None 

    def getCompositeBU( self, tables, msg = "" ):

        # Header:
        table, iTitle = self.getTableByTitle( tables, "composite building units" )
        iCBUs = None
        # Chains:
        table, iChain = self.getTableByTitle( tables, "chains" )
        table, iTiles = self.getTableByTitle( tables, "natural tiling" )

        if iTitle:
            table = tables[iTitle]
            pass
        else:
            #print( "Failed to find the Composite BU title" )
            pass

        if iChain:
            if iChain - iTitle == 2:
                iCBUs = iTitle + 1
            #table = tables[iCBUs]
        else:
            #print( "Failed to find the Chains title" )
            pass

        if iTiles:
            #if (iTiles - iChain) == 1:
            #    pass # Do nothing
            if (iTiles - iTitle) == 2:
                if iChain:
                    pass # Do nothing
                else:
                    iCBUs = iTiles - 1


        #print( "Table order ids: Title:", iTitle, "CBU:", iCBUs, "Chain:", iChain, "Tiles:", iTiles )

        table = tables[iTitle]
        #print( table )
        title = self.getTableTitle( table )
        #print( "In Composite BU title:", title )
        #print( [i for i in range( 1, 4, 2 ) ] )

        output = dict()
        if title.lower().strip().startswith("composite building units"): 
            output["CompositeBU"] = dict()
            output["CompositeBU"][  "cages"] = []
            output["CompositeBU"]["t-cages"] = []
            output["CompositeBU"][ "chains"] = []


            if iCBUs:
                #print( "Found CBU table at position", iCBUs )
                table = tables[iCBUs]

                tr_els = table.find_all( 'tr' )

                #if len(tr_els) > 0:
                for ir in range( 1, len(tr_els), 2 ):
                    #print( tr_els )
                    tr_el = tr_els[ir]
                    td_els = tr_el.find_all( 'td' )
                    #print( td_els )
                    if len(td_els) == 0:
                        continue
                    for td in td_els[1:]:
                        #if td.text
                        #print( td )
                        #print( td.text )
                        if len(td.text.strip()) == 0:
                            continue
                        td = td.contents[0]
                        #print( " td = ", td )
                        word = td.find_all( "div" ) #.contents
                        word = td.contents[0].strip()
                        #print( word )
                        cName, tName = self.splitBrackets( word, msg = "where?" )
                        if cName:
                            output["CompositeBU"][  "cages"].append( cName )
                        if tName:
                            output["CompositeBU"]["t-cages"].append( tName )
                    
            if iChain:
                #print( "Found Chain table at position", iChain )
                table = tables[iChain]

                tr_els = table.find_all( 'tr' )

                if len(tr_els) > 1:
                    print( ">>>>>>>>>>>>>>>>>>>>>> More than one chain" )
                #for ir in range( 1, len(tr_els), 2 ):
                for ir, tr_el in enumerate(tr_els):
                    #print( tr_el )
                    #tr_el = tr_els[ir]
                    td_els = tr_el.find_all( 'td' )
                    #print( td_els )
                    if len(td_els) < 3:
                        continue
                    td = td_els[2]
                    #for td in td_els[2]:
                        #if td.text
                        #print( td )
                        #print( td.text )
                    if len(td.text.strip()) == 0:
                        continue
                    td = td.contents[0]
                    #print( " td = ", td )
                    word = td.text.strip()
                    #word = td.find_all( "div" ) #.contents
                    #word = td.contents[0].strip()
                    #print( word )
                    #cName, tName = self.splitBrackets( word, msg = "where?" )
                    output["CompositeBU"]["chains"].append( word )
 
        #print( output )

        return output

        pass # IzaFramework.getCompositeBU()

    def splitBrackets( self, word, msg = "" ):
        #print( word, type(word) )

        pattern = re.compile( r'\s*([\w-]+)\s*(?:\(\s*([\w-]+)\s*\))?\s*' )
        #pattern = re.compile(r'\s*(\w+)\s*\(\s*(\w+)\s*\)\s*')
        #pattern = re.compile( r'(\w+)\((\w+)\)' )
        match = pattern.match( word )
        if match:
            #matches = pattern.findall( word )
            #print( len(match.groups() ) )
            if 2 != len(match.groups() ):
                print( "Error in match find word", word, ", in ", msg )
            cName = match.group( 1 )
            tName = match.group( 2 )
            #print( match.group(0), " ==>", cName, "vs", tName )
        else:
            print( "Failed to find match for word '" + word + "', " + msg )
            cName = word
            tName = word

        return cName, tName
        pass # IzaFramework.splitBrackets()

    def getNaturalTile( self, tables ):
        #table = tables[0]
        #title = self.getTableTitle( table )
        #print( title )
        table, iTiles = self.getTableByTitle( tables, "natural tiling" )

        output = dict()
        output["NaturalTiles"] = []

        if table:
            tr_els = table.find_all( 'tr' )

            if len(tr_els) > 1:
                #print( ">>>>>>>>>>>>>>>>>>>>>> More than one chain" )
                #for ir in range( 1, len(tr_els), 2 ):
                for ir, tr_el in enumerate(tr_els):
                    #print( tr_el )
                    #tr_el = tr_els[ir]
                    td_els = tr_el.find_all( 'td' )
                    #print( td_els )
                    if len(td_els) < 3:
                        continue
                    #td = td_els[2]
                    for td in td_els[1:]:
                        #if td.text
                        #print( td )
                        #print( td.text )
                        #td = td.contents
                        #print( " td = ", td.text )
                        if len(td.text.strip()) == 0:
                            continue
                        word = td.text.strip()
                    #word = td.find_all( "div" ) #.contents
                    #word = td.contents[0].strip()
                        if not word.lower().startswith( "natural tiling" ):
                            output["NaturalTiles"].append( word )

        #print( output )
        return output

        pass # IzaFramework.getNaturalTile()

    #def ( self, tables ):
    #    pass # IzaFramework.()


    def getTAtoms( self, table ):
        atoms = dict()
        atoms["TAtoms"] = []

        #print( "warning: only 2 rows read" )
        rows = table.find_all( "tr" )[1:]
        for row in rows:
            atom = dict()
            #print ( row )
            cells = row.find_all( "td" )
            #print( "cells =", cells )
            #print( type( table ) )
            #print( type( cells ) )
            #print( type( cells[0] ) )

            for ic, cell in enumerate(cells):
                #print( type( cell ), ":", cell )
                value = self.getCellContent( cell )
                #print( value )
                if 0 == ic:
                    atom["TAtomName"] = dict()
                    atom["TAtomName"]["Name"] = value["p"]
                    atom["TAtomName"]["Id"]   = value["sub"]

                    atom["CoordinateSequence"] = []
                elif ic < 13:
                    #print( "coord seq =", value )
                    atom["CoordinateSequence"].append( int(value["p"]) )
                elif ic < 14:
                    #print( "cell = ", cell, type(cell) )
                    #print( "vert symb =", value )
                    #atom["VertexSymbol"] = value["p"]
                    vs = self.getVertexSymbol( cell )
                    atom["VertexSymbol"] = vs
                else:
                    print( " Error! too many columns in TAtoms table", ic )
                    
                #print( cell )
                #print( "atom = ", atom )
                #atom["key"] = value
                #atom["value"] = value

            atoms["TAtoms"].append( atom )
        return atoms

        pass # IzaFramework.getTAtoms()

    def getVertexSymbol( self, cell ):
        #print( "Vert Symb from cell =", cell, type(cell) )
        #value = self.getCellContent( cell )
        #print( value["p"] )
        #print( value["p"][0] )
        #print( hex(ord(value["p"][1])) )
        #print( u"\u00B7" )
        #print( value["p"].split( u"\u00B7" ) )

        #td = cell.find( "div").get_text( strip=True ) #.find( "div" )

        td_el = cell.find( "div" ) #.find( "div" )
        #print( "td = ", td_el.get_text )
        #print( "td = ", td_el.text )
        content = ''.join(map(str, td_el.contents))
        #print( content.split( u"\u00B7" ) )
        #print( "ssss:", content )
        dot = u"\u00B7" # This is a dot symbol, similar to TeX \cdoc symbol.
        vsArr = content.split( dot )

        output = []
        for vs in vsArr:
            #print( vs, type(vs) )
            tag = BeautifulSoup( "<div>"+vs+"</div>", "html.parser" )
            #print( tag, type(tag) )

            value = self.getCellContent( tag )
            #print( "vs value = ", value )
            vsValue = dict()
            vsValue["ring"] = value["p"].strip()
            if "sub" in value:
                vsValue["count"] = value["sub"].strip()
            #print( "   > ", vsValue )
            output.append( vsValue )

        #value = td.find( 'div' )
        #value = td.text
        #print( "value = ", value )
        #print( value.split( u"\u00B7" ) )

        return output
        pass # IzaFramework.getVertexSymbol()

    def getCellContent( self, cell ):
        #print( cell )
        #print( "------------" )
        #print( cell.prettify() )
        #cp = BeautifulSoup( cell, 'html.parser' )

        #print( cell )
        cp = cell.find( 'div' )
        div_text = ''.join([str(item) for item in cp.contents if item.name != 'sub'])
        #print( "div = ", cell.div.text )
        #print( "div = ", div_text )
        #print( "div = ", cell.p )
        #print( "sub = ", cell.find( 'sub' ) )

        sub_el = cell.find( 'sub' )
        #print( div_text, cp.find( 'sub' ).get_text() )

        output = dict()
        output["p"] = div_text 
        if sub_el:
            output["sub"] = sub_el.get_text() 

        return output
        """
        div_el = cell.find( 'div' )
        value = ""
        if div_el:
            value = div_el.get_text( strip=False )
            sub = sub_el.get_text( strip=True )
            print( value, sub )

        else:
            print( "No <div> elements found in cell", cell )
        #print( value )
        key = "a"
        #value = "aa"
        return key, value
        """
        pass # IzaFramework.getCellContent()


    def getFrameworkTables( self, html, name ):

        soup = BeautifulSoup( html, 'html.parser')
        #table = soup.find('table', {'id': 'table_id'})
        tables = soup.find_all('table')

        #print( len( tables ) )
        table = tables[0]
        #print( "table = ", type(table), table )

        output = []
        if table:
            tr_left = table.find_all( 'tr' )[0] # The left row of the html page
            td_el = tr_left.find( 'td' )
            #print( td_el )
            #output.append( td_el )
            tbl_el = td_el.find_all( 'table' )

            for tbl in tbl_el:
                output.append( tbl )
                #print( "   >>>>>> table =", tbl )
            #print("Prop: \n",table.prettify())
            pass
        else:
            print("Table with Framework is not found:", name)


        return output 
        return table 
        pass # IzaFramework.getFrameworkTable()

    def getTAtomTable( self, html, name ):
        #print( "Starting T atoms" )

        soup = BeautifulSoup( html, 'html.parser')
        #table = soup.find('table', {'id': 'table_id'})
        tables = soup.find_all('table')

        #print( len( tables ) )
        table = tables[1]
        #print( "tables = ", type(table) )
        if table:
            #print("Prop: \n",table.prettify())
            pass
        else:
            print("Table with T-atoms is not found:", name)

        return table
        pass # IzaFramework.getTAtomTable()

    def getVolumeTable( self, html, name ):
        soup = BeautifulSoup( html, 'html.parser')
        #table = soup.find('table', {'id': 'table_id'})
        tables = soup.find_all('table')

        #print( len( tables ) )
        table = tables[0]
        #print( "tables = ", type(table) )
        #print( "tables = ", table )

        return table 
        pass # IzaFramework.getVolumeTable()

    def nameToPath( self, theme, name ):
        filePath = os.path.join( IZADATA, theme, name + ".html" )
        return filePath
        pass # IzaFramework.nameToPath()

    def getHtmlAsText( self, theme, name ):
        
        filePath = self.nameToPath( theme, name )
        if os.path.isfile( filePath ):
            html = self.readHtmlFile( filePath )
        else:
            html = self.readUrl( theme, name )
            self.saveHtmlFile( theme, name, html )
        
        #print( html )
        return html
        pass # IzaFramework.getHtmlAsText()

    def readHtmlFile( self, filePath ):
        if os.path.isfile( filePath ):
            with open( filePath, encoding="utf-8" ) as fp:
                lines = fp.readlines()

        output = "".join(lines)
        #print( output )

        #print( type( output ) )
        return output 
        pass # IzaFramework.readHtmlFile()

    def readUrl( self, theme, name):
        print( "Info: Loading data from web: ", theme, name )
        time.sleep( 12. )

        if "coord" == theme:
            #url = "https://example.com/sample.html"
            url = "https://asia.iza-structure.org/IZA-SC/framework_cs.php?STC=" + name
        elif "frame" == theme:
            url = "https://asia.iza-structure.org/IZA-SC/framework.php?STC=" + name
        elif "volume" == theme:
            url = "https://asia.iza-structure.org/IZA-SC/framework_vol.php?STC=" + name 
        elif "refmat" == theme:
            url = "https://asia.iza-structure.org/IZA-SC/material_tm.php?STC=" + name
        elif "mater"  == theme:
            url = "https://asia.iza-structure.org/IZA-SC/material_rm.php?STC=" + name
        elif "tatom"  == theme:
            url = "https://asia.iza-structure.org/IZA-SC/framework_coord.php?STC=" + name
        else:
            print( "Invalid theme = '" + str(theme) + "'. ")

        response = requests.get(url)
        html = response.text
        #print( type( html ) )

        return html
        pass # IzaFramework.readUrl()

    def saveHtmlFile( self, theme, name, html ):
        filePath = self.nameToPath( theme, name )
        with open( filePath, "w", encoding="utf-8" ) as fp:
            fp.write( html )

        pass # IzaFramework.saveHtmlFile()

# Replace the URL with the actual URL of the HTML page containing the table

# Make a request to fetch the HTML content
# Parse the HTML content using BeautifulSoup

# Replace 'table_id' with the actual ID or class of the table you want to extract

# Print the table content

    pass # class IzaFramework


if __name__ == "__main__":

    allFrames = []

    #zList = zeolist.getZeoList( "main" )
    zList = zeolist.getZeoList( ["main", "new"] )
    print( "Number of zeolite frameworks in the list:", len(zList) )

    #zList = zList[:10]
    #zList = zList[150:155]
    #zList = [ "-EWT" ]
    #zList = [ "YFI" ]

    #for name in [ "ABW", "ACO", "MTW" ]:
    for iName, name in enumerate(zList):

        if len(zList) > 1 and "-EWT" == name:
            # Note: -EWT for some reason it has completely different structure. I will do it manually.
            #continue
            pass

        fw = IzaFramework()
        #fw = fw.getFrameworkData( "ABW" )
        #x = fw.getFrameworkData( "MTW" )
        x = fw.getFrameworkData(name, iName=iName)
        allFrames.append( x )

    fileOut = os.path.join("ontozeolite", "izadata", "iza-data.json")
    with open( fileOut, "w" ) as fp:
        json.dump( allFrames, fp, indent = 4 )

