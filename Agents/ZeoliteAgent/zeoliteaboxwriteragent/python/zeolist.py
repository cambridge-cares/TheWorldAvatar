import logging
#logging.basicConfig( level = logging.INFO )
logging.basicConfig( level = logging.WARNING )
import csv
import os
import uuid
import tools

rawString = " ABW     ACO     AEI     AEL     AEN     AET     AFG     AFI     AFN     "+\
    "AFO     AFR     AFS     AFT     AFV     AFX AFY     AHT     ANA     ANO     "+\
    "APC     APD     AST     ASV     ATN     ATO     ATS     ATT     ATV     AVE     "+\
    "AVL AWO     AWW     BCT     BEC     BIK     BOF     BOG     BOZ     BPH     "+\
    "BRE     BSV     CAN     CAS     CDO     CFI CGF     CGS     CHA     -CHI  "+\
    "-CLO    CON     CSV     CZP     DAC     DDR     DFO     DFT     DOH     DON     "+\
    "EAB EDI     EEI     EMT     EON     EPI     ERI     ESV     ETL     ETR     "+\
    "ETV     EUO     EWF     EWO     EWS     -EWT EZT     FAR     FAU     FER     "+\
    "FRA     GIS     GIU     GME     GON     GOO     HEU     -HOS    IFO     IFR    "+\
    "-IFT -IFU    IFW     IFY     IHW     IMF     IRN     IRR     -IRT    -IRY    ISV   "+\
    "ITE     ITG     ITH     ITR     ITT -ITV    ITW     IWR     IWS     IWV     IWW     "+\
    "JBW     JNT     JOZ     JRY     JSN     JSR     JST     JSW     JZO KFI     "+\
    "LAU     LEV     LIO     -LIT    LOS     LOV     LTA     LTF     LTJ     LTL     "+\
    "LTN     MAR     MAZ     MEI MEL     MEP     MER     MFI     MFS     MON     MOR    "+\
    "MOZ     MRT     MSE     MSO     MTF     MTN     MTT     MTW MVY  "+\
    "MWF     MWW     NAB     NAT     NES     NON     NPO     NPT     NSI     OBW     "+\
    "OFF     OKO     OSI     OSO OWE     -PAR    PAU     PCR     PHI     PON     POR     "+\
    "POS     PSI     PTF     PTO     PTT     PTY     PUN     PWN PWO     PWW     RHO     "+\
    "-RON    RRO     RSN     RTE     RTH     RUT     RWR     RWY     SAF     SAO     SAS    "+\
    "SAT SAV     SBE     SBN     SBS     SBT     SEW     SFE  "+\
    "SFF     SFG     SFH     SFN     SFO     SFS     SFW     SGT SIV     SOD     SOF     "+\
    "SOR     SOS     SOV     SSF     -SSO    SSY     STF     STI     STT     STW     -SVR  "+\
    "SVV SWY     -SYT    SZR     TER     THO     TOL     TON     TSC     TUN     UEI     "+\
    "UFI     UOS     UOV     UOZ     USI UTL     UWY     VET     VFI     VNI     VSV     "+\
    "WEI     -WEN    YFI     YUG     ZON "

newString = "JZT   EOS   -ION   RFE" 
inGrowString = "CIT-13  ITQ-39  ZSM-48  IPC-6   SSZ-57  SSZ-31  SSZ-70  IM-18  HPM-14"

#testString = " ABW     ACO     AEI     AEL   "
testString = " ABW     ACO      AET     AFG     AFI     CSV     ETV    PTY "
# DFO 

def getZeoList( arg="main" ):
    outString = ""
    if   isinstance( arg, str ):
        if "all" == arg:
            myArg = [ "main", "new", "inter" ]
        else:
            myArg = [arg]
    elif isinstance( arg, list ):
        myArg = arg
    else:
        print( "Unknown type of argument '" + arg + "' " )
        return

    # Testing the input values:
    for a in myArg:
        if not a in [ "main", "new", "inter", "all", "test" ]:
            print( "Warning: in getZeoList(" + str(arg) +") option '" + str(a) + "' is not supported" )
            pass

    if "main" in myArg:
       outString += rawString
    if "new" in myArg:
       outString += newString
    if "inter" in myArg:
       outString += inGrowString
    if ("test" in myArg ) and ( not "main" in myArg ):
       outString += testString

#    else:
#         print( "Unknown argument '" + arg  "'" )

    output = outString.split()
    print( "Total ", len(output), " frameworks" )
    return output
    pass # getZeoList()

def zeoCodeToCode3( code ):
  codeShort = code.strip()
  if   "-" == codeShort[0]:
    output  = codeShort[1:]
  elif "_" == codeShort[0]:
    output  = codeShort[1:]
  else:
    output  = codeShort[0:]

  if len( output ) != 3:
    logging.error( "Code is not 3 letters long: '" + output + "'" )

  return output
  pass # zeoCodeToCode3()

def zeoCodeToExcel( code ):
  # Replace "-" to "_" character. ("-" is not displayed in excel/csv)
  codeShort = code.strip()
  if "-" == codeShort[0]:
    output = "_" + codeShort[1:]
  else:
    output = codeShort
  
  return output
  pass # zeoCodeToExcel()

def zeoCodeToOrig( code ):
  # Replace back: "-" to "-" character. ("-" is not displayed in excel/csv)
  codeShort = code.strip()
  if "_" == codeShort[0]:
    output = "-" + codeShort[1:]
  else:
    output = codeShort
  
  return output

  pass # zeoCodeToOrig()


if __name__ == "__main__":
   getZeoList( )
   getZeoList( "main" )
   getZeoList( "new" )

   getZeoList( ["new", "main"] )
   getZeoList( 'inter' )
   getZeoList( 'test' )
   getZeoList( [ 'test', 'main' ] )
   #getZeoList( )

   print( tools.getZeoUUID( "AWW" ) )
   print( zeoCodeToExcel( "AWW" ) )
   print( zeoCodeToExcel( "-CLO" ) )
   print( zeoCodeToOrig( "_CLO" ) )
   print( zeoCodeToOrig( "BEC" ) )

   db = tools.loadUUID()

   print( tools.getZeoUUID1( db, "ClassA", "a1" ) )
   print( tools.getZeoUUID1( db, "ClassA", "a2" ) )
   print( tools.getZeoUUID1( db, "ClassA", "a3" ) )
   print( tools.getZeoUUID1( db, "ClassB", "b1" ) )

   print( db )
   tools.saveUUID( db )
