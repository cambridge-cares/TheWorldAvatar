"""
TODO:
- Check for inputs out of range, for string instead of number, etc.
- Add output as an image, and may be other additional data
- Advanced GUI: use grey color for suggested width and height (if not entered)
- Add (h,k,l) data also. This may require complete change of the logic..
  Now peak returns only 3-letter code, but need to return a set of peaks.
  Now PeakInfo is in fact a peakInterval.
- Add a query for colmpete data of given component name.
  This will be used later for user-friendly output.

DONE:
- Fixed. Bug Threshold even 101% still shows results.
- Fixed. Bug with entry 14, 25, 36. Should be an empty list
- Done. Add ignore fields without entry values.
- Done. Add other entries (a,b,c,alpha,beta,gamma).
  Query for these parameters in a range.


"""


import tkinter as tk
from SPARQLWrapper import SPARQLWrapper, JSON

#           http://localhost:8080/blazegraph/#query
address = "http://localhost:8080/blazegraph/namespace/zeo05w/sparql"
#  Works:  https://theworldavatar.io/chemistry/blazegraph-dev/ui/#query
#address = "https://theworldavatar.io/chemistry/blazegraph-dev/ui/namespace/ontozeolite/sparql"  # < need authorization
#address = "https://theworldavatar.io/chemistry/blazegraph/ui/namespace/ontozeolite/sparql"  # < updated once a week

DEBUG_ON = True
DEBUG_ON = False

class PeakRange:
    __slots__ = [ "min", "max", "pos", "wid", "int" ]
    def __init__( self ):
        self.pos = 0  # The approximate peak position to look for.
        self.wid = 0  # The +/- range of the peak position to look for.
        self.int = 0  # The minimum intensity of peak to look for.
        self.min = 0  # The left  side of the range. Usually equals to pos-wid
        self.max = 0  # The right side of the range. Usually equals to pos+wid
        pass # PeakRange.__INIT__()

    pass # class PeakRange


class PeakInfo:
    __slots__ = [ "h", "k", "l", "pos", "int" ]
    def __init__( self ):
        self.h   = None  # Miller indices of the peak: h,k,l
        self.k   = None
        self.l   = None

        self.pos = None  # Position in degrees (2theta angle)
        self.int = None  # Intensity in % of the highest peak
        pass # PeakInfo.__init__()

    pass # class PeakInfo

class ValueRange:
    __slots__ = [ "name", "min", "max" ]
    def __init__(self):
        self.min  = None
        self.max  = None
        self.name = ""

        pass # ValueRange.__init__()

    pass # class ValueRange


class GuiZeolite:

    def __init__( self ):

        self.root = tk.Tk()
        ir = 0
   
        # Specify the size of window.
        self.root.geometry( "1200x700" )
        self.root.title( "Zeolite knowledge graph agent" )
        #self.root.configure( font = ("Times New Roman", 20 ) )

        # This is the text in the EntryBox (not the TextBox used now)
        #self.txtStr = tk.StringVar()

        self.T2 = tk.Text( self.root, width=80, height=10 )
        self.T2.configure( font = ("Times New Roman",20 ) )

#    self.T2.bind( '<<Modified>>', self.textInput )

        #T = tk.Entry(root, textvariable=sv, validate="focusout", validatecommand=callback)
     
        # Create top level label:
        self.lblTitle = tk.Label(self.root, text = "Zeolite data")
        self.lblTitle.config( font =("Courier", 16) )
        #self.lblTitle.grid( row = ir, column = 0 )
        ir += 1

        self.addPeakFinderGrid()
        self.addUnitCellGrid()
        self.addControlGrid()

        self.frameUnitCell.grid( row = ir, column = 0 )
        ir += 1
        self.framePeaks.grid( row = ir, column = 0 )
        ir += 1
        self.frameControl.grid( row = ir, column = 0 )
        ir += 1

        self.setUnitCellDefault()
        self.setPeaksDefault()

        #root.bind("<KeyPress>", keyDown )
        #root.bind("<KeyRelease>", keyUp )
        self.root.bind("<Escape>",   self.escDown )
        self.root.focus_set()

        self.setFontSize( 30 )

        tk.mainloop()

        pass # GuiZeolite.__init__()

    def setFontSize( self, value ):
        entryList = [ self.lblTitle,
                      self.lblSecPeak,
                      self.lblPeak1,     self.lblPeak2,     self.lblPeak3,
                      self.lblPeakHead1, self.lblPeakHead2, self.lblPeakHead3,
                      self.entPeak1Pos,  self.entPeak1Wid,  self.entPeak1Int,
                      self.entPeak2Pos,  self.entPeak2Wid,  self.entPeak2Int,
                      self.entPeak3Pos,  self.entPeak3Wid,  self.entPeak3Int,

                      self.lblResult,    self.entResult,
                      self.btnFind,      self.btnExit,   self.btnCif,

                      self.lblUnitCell,
                      self.lblUnitCellHead1,  self.lblUnitCellHead2,
                      self.lblUnitCellHead3,  self.lblUnitCellHead4,
                      self.lblUC_a,        self.lblUC_b,       self.lblUC_c,
                      self.lblUC_alpha,    self.lblUC_beta,    self.lblUC_gamma,

                      self.entUC_aMin,     self.entUC_bMin,    self.entUC_cMin,
                      self.entUC_aMax,     self.entUC_bMax,    self.entUC_cMax,
                      self.entUC_alphaMin, self.entUC_betaMin, self.entUC_gammaMin,
                      self.entUC_alphaMax, self.entUC_betaMax, self.entUC_gammaMax
                    ]
        """
                      self.UC_aMin,      self.UC_aMin,     self.UC_aMin,
                      self.UC_aMax,      self.UC_aMax,     self.UC_aMax,
                      self.UC_alphaMin,  self.UC_betaMin,  self.UC_gammaMin,
                      self.UC_alphaMax,  self.UC_betaMax,  self.UC_gammaMax,
        """

        for e in entryList:
            #print( e )
            e["font"] = ( "Times New Roman", value )
            #e["font"] = ( value )

        pass # GuiZeolite.setFontSize()

    def addControlGrid( self ):
        self.frameControl = tk.Frame( self.root )

        self.lblResult = tk.Label( self.frameControl, text = "Framework code:" )
        self.lblResult.config( font = ("Courier", 12) )

        self.result = tk.StringVar()
        self.entResult = tk.Entry( self.frameControl, width = 40, textvariable=self.result )

        # Create an Exit button.
        self.btnFind = tk.Button( self.frameControl, text = " Find ", command = self.funcFind )
        self.btnExit = tk.Button( self.frameControl, text = " Exit ", command = self.root.destroy )
        self.btnCif  = tk.Button( self.frameControl, text =  " CIF ", command = lambda: self.makeCifFile("ABW","file.cif") )

        ir = 0
        self.btnCif.grid(  row = ir, column = 2 )
        self.btnFind.grid( row = ir, column = 3 )
        #self.btnExit.grid( row = ir, column = 4 )
        ir += 1

        self.lblResult.grid( row = ir, column = 0 )
        self.entResult.grid( row = ir, column = 1, columnspan=3 )
        ir += 1

        pass # GuiZeolite.

    def setPeaksDefault( self ):
        self.peak1pos.set(  "14"  )
        self.peak1wid.set(  0.5 )
        self.peak1int.set( 50 )
        self.peak2pos.set(  "28"  )
        self.peak2wid.set(  0.5 )
        self.peak2int.set( 50 )
        self.peak3pos.set(  "29.5"  )
        self.peak3wid.set(  0.5 )
        self.peak3int.set( 50 )
        pass # GuiZeolite.setPeaksDefault()

    def setUnitCellDefault( self ):
        self.ucAmin.set( 5  )
        self.ucAmax.set( 10 )
        self.ucBmin.set( 5  )
        self.ucBmax.set( 10 )
        self.ucCmin.set( 5  )
        self.ucCmax.set( 10 )
        """
        self.ucALmin.set( 0  )
        self.ucALmax.set( 120 )
        self.ucBEmin.set( 0  )
        self.ucBEmax.set( 120 )
        self.ucGAmin.set( 0  )
        self.ucGAmax.set( 120 )
        """

        pass # GuiZeolite.setUnitCellDefault()

    def getValueRange( self, name, valueMin = None, valueMax = None ):
        if not isinstance( valueMin, tk.StringVar ):
            print( "Value minimum is not a StringVar for valueMin" )
            return None
        if not isinstance( valueMin, tk.StringVar ):
            print( "Value minimum is not a StringVar for valueMin" )
            return None

        value = ValueRange()

        if None == valueMin.get() or "" == valueMin.get().strip():
            value.min = None
        else:
            value.min = float( valueMin.get() )

        if None == valueMax.get() or "" == valueMax.get().strip():
            value.max = None
        else:
            value.max = float( valueMax.get() )

        if None == value.min and None == value.max:
            return None

        value.name = name

        return value
        pass # GuiZeolite.getValueRange()

    def getPeakRange( self, peakPos, peakWid=None, peakInt=None ):
        if not isinstance( peakPos, tk.StringVar ):
            print( "Peak position is not a StringVar for peakPos" )
            return None
        if not isinstance( peakWid, tk.StringVar ) and peakWid != None:
            print( "Peak width is not a StringVar for peakWid" )
            return None
        if not isinstance( peakInt, tk.StringVar ) and peakInt != None:
            print( "Peak intensity is not a StringVar for peakInt" )
            return None

        peak = PeakRange()

        if None == peakPos.get() or "" == peakPos.get().strip():
            return None
        else:
            peak.pos = float( peakPos.get() )
        if None == peakWid.get():
            peak.wid = 0.5
        else:
            peak.wid = float( peakWid.get() )
            # TODO set the
            # peakInt.set(peak.wid)

        if None == peakInt.get():
            peak.int = 50.0
        else:
            peak.int = float( peakInt.get() )

        peak.min = peak.pos - peak.wid
        peak.max = peak.pos + peak.wid

        return peak
        pass # GuiZeolite.getPeakInfo()

    def funcFind( self ):
        print( "in funcFind(): ", float(self.peak1min.get()) )

        lists = []

        pRange = self.getPeakRange( self.peak1pos, self.peak1wid, self.peak1int )
        if pRange != None:
            list1 = self.queryPeakRange( address, pRange )
            if list1 != None:
                lists.append( list1 )
            if DEBUG_ON:
               print( "For peak 1 = ", sorted( list1 ) )
               #print( peak.min, peak.max, peak.int )
        else:
            list1 = []

        pRange = self.getPeakRange( self.peak2pos, self.peak2wid, self.peak2int )
        if pRange != None:
            list2 = self.queryPeakRange( address, pRange )
            if list2 != None:
                lists.append( list2 )
            if DEBUG_ON:
               print( "For peak 2 = ", sorted( list2 ) )
               #print( peak.min, peak.max, peak.int )
        else:
            list2 = []

        pRange = self.getPeakRange( self.peak3pos, self.peak3wid, self.peak3int )
        if pRange != None:
            list3 = self.queryPeakRange( address, pRange )
            if list3 != None:
                lists.append( list3 )
            if DEBUG_ON:
               print( "For peak 3 = ", sorted( list3 ) )
               #print( peak.min, peak.max, peak.int )
        else:
            list3 = []

        vRange = self.getValueRange( "a", self.ucAmin, self.ucAmax )
        if vRange != None:
            list1 = self.queryUnitCellRange( address, vRange )
            if list1 != None:
                lists.append( list1 )
            if DEBUG_ON:
                print( "For UC a:", sorted(list1) )

        vRange = self.getValueRange( "b", self.ucBmin, self.ucBmax )
        if vRange != None:
            list1 = self.queryUnitCellRange( address, vRange )
            if list1 != None:
                lists.append( list1 )
            if DEBUG_ON:
                print( "For UC b:", sorted(list1) )

        vRange = self.getValueRange( "c", self.ucCmin, self.ucCmax )
        if vRange != None:
            list1 = self.queryUnitCellRange( address, vRange )
            if list1 != None:
                lists.append( list1 )
            if DEBUG_ON:
                print( "For UC c:", sorted(list1) )

        vRange = self.getValueRange( "alpha", self.ucALmin, self.ucALmax )
        if vRange != None:
            list1 = self.queryUnitCellRange( address, vRange )
            if list1 != None:
                lists.append( list1 )
            if DEBUG_ON:
                print( "For UC alpha:", sorted(list1) )

        vRange = self.getValueRange( "beta", self.ucBEmin, self.ucBEmax )
        if vRange != None:
            list1 = self.queryUnitCellRange( address, vRange )
            if list1 != None:
                lists.append( list1 )
            if DEBUG_ON:
                print( "For UC beta:", sorted(list1) )

        vRange = self.getValueRange( "gamma", self.ucGAmin, self.ucGAmax )
        if vRange != None:
            list1 = self.queryUnitCellRange( address, vRange )
            if list1 != None:
                lists.append( list1 )
            if DEBUG_ON:
                print( "For UC gamma:", sorted(list1) )

        #listOut = self.intersectLists( [list1,list2,list3] )
        listOut = self.intersectLists( lists )
        listOut.sort()

        self.result.set( ", ".join(listOut) )
        pass # GuiZeolite.

    def keyDown( self, event):
        #print( "Pressed: '" + event.char + "'" )
        if event.char == "q":
            self.root.destroy()
        pass # GuiZeolite.

    def keyUp( self, event):
        if event.char == "q":
            self.root.destroy()
        pass # GuiZeolite.

    def escDown( self, event):
        self.root.destroy()
        pass # GuiZeolite.

    def intersectLists( self, lists ):
        output = []
        if not isinstance(lists,list):
            print( "Error: intersectLists input is not a list" )
            return output

        if len(lists) == 0:
            return output

        nonEmpty = []
        for i,x in enumerate(lists):
            if not isinstance(x,list):
                print("Error: Components of input list must be lists" )
                return output
            if len(x) == 0:
                pass
            else:
                nonEmpty.append(x)

        nonEmpty = lists

        if len(nonEmpty) == 0:
            return output
        elif len(nonEmpty) == 1:
            return nonEmpty[0]

        output = nonEmpty[0]
        for x in nonEmpty[1:]:
            output = list( set(output) & set( x ) )

        return output
        pass # GuiZeolite.intersectLists()

    def addPeakFinderGrid( self ):
        self.framePeaks = tk.Frame( self.root )

        self.lblSecPeak = tk.Label(self.framePeaks, text = "XRD peaks positions:", justify='left')
        self.lblSecPeak.config( font =("Courier", 16) )

        self.lblPeakHead1 = tk.Label( self.framePeaks, text = u"2\u03b8 " ) # \u03bc
        self.lblPeakHead1.config( font = ("Courier", 12) )
        self.lblPeakHead2 = tk.Label( self.framePeaks, text = u"+/- \u03b4\u03b8" )
        self.lblPeakHead2.config( font = ("Courier", 12) )
        self.lblPeakHead3 = tk.Label( self.framePeaks, text = "Theshold (%)" )
        self.lblPeakHead3.config( font = ("Courier", 12) )

        self.lblPeak1 = tk.Label( self.framePeaks, text = "Peak 1" )
        self.lblPeak1.config( font = ("Courier", 12) )
        self.lblPeak2 = tk.Label( self.framePeaks, text = "Peak 2" )
        self.lblPeak2.config( font = ("Courier", 12) )
        self.lblPeak3 = tk.Label( self.framePeaks, text = "Peak 3" )
        self.lblPeak3.config( font = ("Courier", 12) )

        self.peak1pos = tk.StringVar()
        self.entPeak1Pos = tk.Entry( self.framePeaks, width = 10, textvariable=self.peak1pos, justify='center' )
        self.peak1wid = tk.StringVar()
        self.entPeak1Wid = tk.Entry( self.framePeaks, width = 10, textvariable=self.peak1wid, justify='center' )
        self.peak1int = tk.StringVar()
        self.entPeak1Int = tk.Entry( self.framePeaks, width = 10, textvariable=self.peak1int, justify='center' )

        self.peak2pos = tk.StringVar()
        self.entPeak2Pos = tk.Entry( self.framePeaks, width = 10, textvariable=self.peak2pos, justify='center' )
        self.peak2wid = tk.StringVar()
        self.entPeak2Wid = tk.Entry( self.framePeaks, width = 10, textvariable=self.peak2wid, justify='center' )
        self.peak2int = tk.StringVar()
        self.entPeak2Int = tk.Entry( self.framePeaks, width = 10, textvariable=self.peak2int, justify='center' )

        self.peak3pos = tk.StringVar()
        self.entPeak3Pos = tk.Entry( self.framePeaks, width = 10, textvariable=self.peak3pos, justify='center' )
        self.peak3wid = tk.StringVar()
        self.entPeak3Wid = tk.Entry( self.framePeaks, width = 10, textvariable=self.peak3wid, justify='center' )
        self.peak3int = tk.StringVar()
        self.entPeak3Int = tk.Entry( self.framePeaks, width = 10, textvariable=self.peak3int, justify='center' )

        #==================================================
        # Positioning of the GUI elements within the frame
        ir = 0

        self.lblSecPeak.grid( row = ir, column = 0, columnspan=2 )
        ir += 1

        self.lblPeakHead1.grid( row = ir, column = 1 )
        self.lblPeakHead2.grid( row = ir, column = 2 )
        self.lblPeakHead3.grid( row = ir, column = 3 )
        ir += 1

        self.lblPeak1   .grid( row = ir, column = 0 )
        self.entPeak1Pos.grid( row = ir, column = 1 )
        self.entPeak1Wid.grid( row = ir, column = 2 )
        self.entPeak1Int.grid( row = ir, column = 3 )
        ir += 1

        self.lblPeak2   .grid( row = ir, column = 0 )
        self.entPeak2Pos.grid( row = ir, column = 1 )
        self.entPeak2Wid.grid( row = ir, column = 2 )
        self.entPeak2Int.grid( row = ir, column = 3 )
        ir += 1

        self.lblPeak3   .grid( row = ir, column = 0 )
        self.entPeak3Pos.grid( row = ir, column = 1 )
        self.entPeak3Wid.grid( row = ir, column = 2 )
        self.entPeak3Int.grid( row = ir, column = 3 )
        ir += 1

        return ir
        pass # GuiZeolite.addPeakFinderGrid()


    def addUnitCellGrid( self ):
        width = 6  # Width in characters of input fields (a.k.a. tk.Entry)
        dx = 10    # Distance in pixels between fields

        self.frameUnitCell = tk.Frame( self.root )

        self.lblUnitCell = tk.Label(self.frameUnitCell, text = "Unit Cell parameters:", justify='left')
        self.lblUnitCell.config( font =("Courier", 16) )

        #self.lblUnitCellHead1 = tk.Label( self.root, text = u"2\u03b8 " ) # \u03bc
        self.lblUnitCellHead1 = tk.Label( self.frameUnitCell, text = u"min, \u212b" )
        self.lblUnitCellHead1.config( font = ("Courier", 12) )
        self.lblUnitCellHead2 = tk.Label( self.frameUnitCell, text = u"max, \u212b" )
        self.lblUnitCellHead2.config( font = ("Courier", 12) )
        self.lblUnitCellHead3 = tk.Label( self.frameUnitCell, text = u"min, \u00b0" )
        self.lblUnitCellHead3.config( font = ("Courier", 12) )
        self.lblUnitCellHead4 = tk.Label( self.frameUnitCell, text = u"max, \u00b0" )
        self.lblUnitCellHead4.config( font = ("Courier", 12) )


        self.lblUC_a = tk.Label(self.frameUnitCell, text = "a")
        self.lblUC_a.config( font =("Courier", 16, "bold italic") )
        self.lblUC_b = tk.Label(self.frameUnitCell, text = "b")
        self.lblUC_b.config( font =("Courier", 16) )
        self.lblUC_c = tk.Label(self.frameUnitCell, text = "c")
        self.lblUC_c.config( font =("Courier", 16) )

        self.lblUC_alpha = tk.Label(self.frameUnitCell, text = u"\u03b1")
        self.lblUC_alpha.config( font =("Courier", 16) )
        self.lblUC_beta = tk.Label(self.frameUnitCell, text = u"\u03b2")
        self.lblUC_beta.config( font =("Courier", 16) )
        self.lblUC_gamma = tk.Label(self.frameUnitCell, text = u"\u03b3")
        self.lblUC_gamma.config( font =("Courier", 16) )

        self.ucAmin = tk.StringVar()
        self.entUC_aMin = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucAmin, justify='center' )
        self.ucAmax = tk.StringVar()
        self.entUC_aMax = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucAmax, justify='center' )

        self.ucBmin = tk.StringVar()
        self.entUC_bMin = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucBmin, justify='center' )
        self.ucBmax = tk.StringVar()
        self.entUC_bMax = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucBmax, justify='center' )

        self.ucCmin = tk.StringVar()
        self.entUC_cMin = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucCmin, justify='center' )
        self.ucCmax = tk.StringVar()
        self.entUC_cMax = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucCmax, justify='center' )

        self.ucALmin = tk.StringVar()
        self.entUC_alphaMin = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucALmin, justify='center' )
        self.ucALmax = tk.StringVar()
        self.entUC_alphaMax = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucALmax, justify='center' )

        self.ucBEmin = tk.StringVar()
        self.entUC_betaMin = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucBEmin, justify='center' )
        self.ucBEmax = tk.StringVar()
        self.entUC_betaMax = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucBEmax, justify='center' )

        self.ucGAmin = tk.StringVar()
        self.entUC_gammaMin = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucGAmin, justify='center' )
        self.ucGAmax = tk.StringVar()
        self.entUC_gammaMax = tk.Entry( self.frameUnitCell, width = width, textvariable=self.ucGAmax, justify='center' )

        #==================================================
        # Positioning of the GUI elements within the frame
        ir = 0

        self.lblUnitCell.grid( row = ir, column = 0 , columnspan=4 )
        ir += 1

        self.lblUnitCellHead1.grid( row = ir, column = 1 )
        self.lblUnitCellHead2.grid( row = ir, column = 2 )
        self.lblUnitCellHead3.grid( row = ir, column = 4 )
        self.lblUnitCellHead4.grid( row = ir, column = 5 )
        ir += 1

        self.lblUC_a.grid(    row = ir, column = 0, padx = dx )
        self.entUC_aMin.grid( row = ir, column = 1, padx = dx )
        self.entUC_aMax.grid( row = ir, column = 2, padx = dx*3  )
        #ir += 1
        self.lblUC_alpha.grid(    row = ir, column = 3, padx = dx )
        self.entUC_alphaMin.grid( row = ir, column = 4, padx = dx )
        self.entUC_alphaMax.grid( row = ir, column = 5, padx = dx )
        ir += 1

        self.lblUC_b.grid(    row = ir, column = 0 )
        self.entUC_bMin.grid( row = ir, column = 1 )
        self.entUC_bMax.grid( row = ir, column = 2 )
        #ir += 1
        self.lblUC_beta.grid(    row = ir, column = 3 )
        self.entUC_betaMin.grid( row = ir, column = 4 )
        self.entUC_betaMax.grid( row = ir, column = 5 )
        ir += 1

        self.lblUC_c.grid(    row = ir, column = 0 )
        self.entUC_cMin.grid( row = ir, column = 1 )
        self.entUC_cMax.grid( row = ir, column = 2 )
        #ir += 1
        self.lblUC_gamma.grid(    row = ir, column = 3 )
        self.entUC_gammaMin.grid( row = ir, column = 4 )
        self.entUC_gammaMax.grid( row = ir, column = 5 )
        ir += 1

        return ir
        pass # GuiZeolite.addUnitCellGrid()

    def queryPeakRange( self, namespace, peakRange ):
        if not isinstance( peakRange, PeakRange ):
            print( " Error in queryPeakRange(), expect element of class PeakRange" )
            return None

        peakMin = peakRange.min
        peakMax = peakRange.max
        peakHeight = peakRange.int

        endpoint = SPARQLWrapper( namespace )
        #endpoint = SPARQLWrapper( address )
        endpoint.setQuery( """
PREFIX zeo:	<http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr:	<http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>

#SELECT ?x ?y ?z
SELECT ?zeoname ?2t1 ?2t2 ?2t3 ?h_1 ?k_1 ?l_1 #?spectrum ?zeo
WHERE {
  ?zeo       zeo:hasZeoliteCode        ?zeoname .
  ?zeo       ocr:hasCrystalInformation ?cifcore .
  ?cifcore   ocr:hasXRDSpectrum        ?spectrum.
  ?spectrum  ocr:hasCharacteristicPeak ?peak1.
#  ?spectrum  ocr:hasCharacteristicPeak ?peak1 , ?peak2.
#  ?spectrum  ocr:hasCharacteristicPeak ?peak1 , ?peak2 , ?peak3 .
  ?peak1     ocr:hasTwoThetaPosition   ?2t1 ;
             ocr:hasRelativeIntensity  ?i1  .
#  ?peak2     ocr:hasTwoThetaPosition   ?2t2 ;              #==================================================
        # Positioning of the GUI elements within the frame

#             ocr:hasRelativeIntensity  ?i2  .

#  ?peak3     ocr:hasTwoThetaPosition   ?2t3 ;
#             ocr:hasRelativeIntensity  ?i3  .
""" +
  "FILTER( ?2t1 >= " + str( peakMin ) + " && ?2t1 <= " + str( peakMax ) +
  " && ?i1 > " + str( peakHeight ) + " ).\n "
+
"""
#  FILTER( ?2t1 < 14 + 1 && ?2t1 > 14 - 1 && ?i1 > 50 ).
#  FILTER( ?2t2 < 28 + 1 && ?2t2 > 28 - 1 && ?i2 > 50 ).
#  FILTER( ?2t3 < 29.5 + 1 && ?2t3 > 29.5 - 1 && ?i3 > 50 ).

  ?peak1   ocr:hasMillerIndices     ?hkl1 ;
           ocr:isSimulated          ?simulated.
  ?hkl1     ocr:hasVectorComponent   ?hkl1_h, ?hkl1_k, ?hkl1_l.
  ?hkl1_h   ocr:hasComponentLabel    "h" ;
           ocr:hasComponentValue    ?h_1  .
  ?hkl1_k   ocr:hasComponentLabel    "k" ;
           ocr:hasComponentValue    ?k_1  .
  ?hkl1_l   ocr:hasComponentLabel    "l" ;
           ocr:hasComponentValue    ?l_1  .

  OPTIONAL {
  }

  #FILTER ( ?zeoname = "ACO" )

  }
#LIMIT 5
      """ )

        endpoint.setReturnFormat(JSON)
        output = []

        results = endpoint.query().convert()

        #print( results.keys() )
        #print( results["results"]["bindings"] )
        for res in results["results"]["bindings"]:
            #print( "  a:", res["zeoname"]["value"], res["2t1"]["value"] )
            z = res["zeoname"]["value"]
            if z not in output:
                output.append( z )
        #print( "In detectPeaks():", output )
        return output
        pass # GuiZeolite.queryPeakRange()

    def queryUnitCellRange( self, namespace, valueRanges ):
        if isinstance( valueRanges, list ):
            values = valueRanges
        else:
            values = [valueRanges]
        #print( "value ranges =", valueRanges )

        filters = ""
        for val in values:
            if not isinstance( val, ValueRange ):
                print( " Expect ValueRange, got " + str(type(val)) + "." )
                return None
            if "" == val.name or None == val.name:
                print( " Unknown name of variable '" + str(val.name) + "' in queryUC" )
            filt = "FILTER ( xsd:decimal(xsd:string(?" + val.name.strip() + \
                    ")) >= " + str(val.min) + " " + \
                    "&& xsd:decimal(xsd:string(?" + val.name.strip() + \
                    ")) <= " + str(val.max) + " )\n"

            filters += filt

          #FILTER ( xsd:decimal(xsd:string(?a)) > 5 && xsd:decimal(xsd:string(?a)) < 20 ) .
        endpoint = SPARQLWrapper( namespace )
        #endpoint = SPARQLWrapper( address )
        endpoint.setQuery( """
PREFIX zeo:	<http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr:	<http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>
#PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?zeoname ?a ?b ?c ?alpha ?beta ?gamma ?volume ?lattice ?symmNum ?unit_length ?unit_angle #?unitlen ?d_vol #?zeo
WHERE {
  ?zeo       zeo:hasZeoliteCode         ?zeoname .
  ?zeo       ocr:hasCrystalInformation  ?cifdata .
  ?cifdata   ocr:hasUnitCell            ?unitcell .

  ?unitcell  ocr:hasUnitCellLengths   ?abc .
  ?abc       ocr:hasVectorComponent   ?abc_a, ?abc_b, ?abc_c .
  ?abc        om:hasUnit              ?unitlen .
  ?unitlen  rdfs:label                ?unit_length .
  ?abc_a     ocr:hasComponentLabel    "a";
             ocr:hasComponentValue    ?a .
  ?abc_b     ocr:hasComponentLabel    "b";
             ocr:hasComponentValue    ?b .
  ?abc_c     ocr:hasComponentLabel    "c";
             ocr:hasComponentValue    ?c .

  ?unitcell  ocr:hasUnitCellAngles     ?abg .
  ?abg       ocr:hasVectorComponent    ?abg_a, ?abg_b, ?abg_g .
  ?abg        om:hasUnit               ?unitangle .
  ?unitangle rdfs:label                ?unit_angle .
  ?abg_a     ocr:hasComponentLabel     "alpha";
             ocr:hasComponentValue     ?alpha .
  ?abg_b     ocr:hasComponentLabel     "beta";
             ocr:hasComponentValue     ?beta .
  ?abg_g     ocr:hasComponentLabel     "gamma";
             ocr:hasComponentValue     ?gamma .

  OPTIONAL {
  ?unitcell  ocr:hasUnitCellVolume      ?Volume .
  ?Volume    om:hasNumericalValue       ?volume .
	}

  OPTIONAL {
    ?abc     ocr:hasUncertaintyVector  ?dabc .
    ?dabc    ocr:hasVectorComponent    ?dabc_a, ?dabc_b, ?dabc_c.
    ?dabc_a  ocr:hasLabel              "a";
             ocr:hasComponentValue     ?d_a .
    ?dabc_b  ocr:hasLabel              "b";
             ocr:hasComponentValue     ?d_b .
    ?dabc_c  ocr:hasLabel              "c";
             ocr:hasComponentValue     ?d_c .
    }
  OPTIONAL {
    ?abg     ocr:hasUncertaintyVector  ?dabg .
    ?dabg    ocr:hasVectorComponent    ?dabg_a, ?dabg_b, ?dabg_g.
    ?dabg_a  ocr:hasLabel                "alpha";
             ocr:hasComponentValue     ?d_alpha .
    ?dabg_b  ocr:hasLabel                "beta";
             ocr:hasComponentValue     ?d_beta .
    ?dabg_g  ocr:hasLabel                "gamma";
             ocr:hasComponentValue     ?d_gamma .
    }
  OPTIONAL {
    ?Volume    ocr:hasUncertainty      ?DVol .
    ?DVol      om:hasNumericalValue    ?d_vol .
    }
  OPTIONAL {
    ?unitcell  ocr:hasLatticeSystem    ?lattice .
    ?unitcell  ocr:hasSymmetryNumber   ?symmNum .
    }
  """ +
  filters
  + """
  #FILTER ( ?volume  >  16000 && ?volume < 45000 ) .
  #FILTER ( ?volume  <  45000  ) .
  #FILTER ( xsd:decimal(?a) = 90 ) .
  #FILTER ( xsd:decimal(xsd:string(?alpha)) != 90 ) .

  #FILTER ( xsd:decimal(xsd:string(?a)) > 5 && xsd:decimal(xsd:string(?a)) < 20 ) .
  #FILTER ( xsd:decimal(xsd:string(?b)) > 5 && xsd:decimal(xsd:string(?b)) < 20 ) .
  #FILTER ( xsd:decimal(xsd:string(?c)) > 5 && xsd:decimal(xsd:string(?c)) < 20 ) .
  #FILTER ( xsd:decimal(xsd:string(?alpha)) > 60 && xsd:decimal(xsd:string(?alpha)) < 120 ) .
  #FILTER ( xsd:decimal(xsd:string(?beta )) > 60 && xsd:decimal(xsd:string(?beta )) < 120 ) .
  #FILTER ( xsd:decimal(xsd:string(?gamma)) > 60 && xsd:decimal(xsd:string(?gamma)) < 120 ) .
  #FILTER ( ?lattice = "orthorhombic" ) .
  #FILTER ( ?symmNum = 74 ) .
  }
#ORDER BY (-xsd:decimal(xsd:string(?volume)))
#LIMIT 10
      """ )

        endpoint.setReturnFormat(JSON)
        output = []

        results = endpoint.query().convert()

        #print( results.keys() )
        #print( results["results"]["bindings"] )
        for res in results["results"]["bindings"]:
            #print( "  a:", res["zeoname"]["value"], res["2t1"]["value"] )
            z = res["zeoname"]["value"]
            if z not in output:
                output.append( z )
        #print( "In detectPeaks():", output )
        return output
        pass # GuiZeolite.queryUnitCellRange()

    def makeCifFile( self, zeoName, filename ):
        #zeoName = "ABW"
        #filename = "file.cif"
        """
        For given zeoName return the cif file.
        - unit cell parameters
        - atomic strucure
        """

        output = []

        if not isinstance( zeoName, str ):
            print( "Error! zeoName must be a string, but got '" + zeoName + "'." )

        endpoint = SPARQLWrapper( address )
        endpoint.setQuery( """
PREFIX zeo:	<http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr:	<http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>
#PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?a ?b ?c ?alpha ?beta ?gamma ?volume ?lattice ?symmNum ?unit_length ?unit_angle #?unitlen ?d_vol #?zeo
WHERE {
  ?zeo       zeo:hasZeoliteCode         '""" 
  + zeoName 
  + """'.
  ?zeo       ocr:hasCrystalInformation  ?cifdata .
  ?cifdata   ocr:hasUnitCell            ?unitcell .

  ?unitcell  ocr:hasUnitCellLengths   ?abc .
  ?abc       ocr:hasVectorComponent   ?abc_a, ?abc_b, ?abc_c .
  ?abc        om:hasUnit              ?unitlen .
  ?unitlen  rdfs:label                ?unit_length .
  ?abc_a     ocr:hasComponentLabel    "a";
             ocr:hasComponentValue    ?a .
  ?abc_b     ocr:hasComponentLabel    "b";
             ocr:hasComponentValue    ?b .
  ?abc_c     ocr:hasComponentLabel    "c";
             ocr:hasComponentValue    ?c .

  ?unitcell  ocr:hasUnitCellAngles     ?abg .
  ?abg       ocr:hasVectorComponent    ?abg_a, ?abg_b, ?abg_g .
  ?abg        om:hasUnit               ?unitangle .
  ?unitangle rdfs:label                ?unit_angle .
  ?abg_a     ocr:hasComponentLabel     "alpha";
             ocr:hasComponentValue     ?alpha .
  ?abg_b     ocr:hasComponentLabel     "beta";
             ocr:hasComponentValue     ?beta .
  ?abg_g     ocr:hasComponentLabel     "gamma";
             ocr:hasComponentValue     ?gamma .

  OPTIONAL {
  ?unitcell  ocr:hasUnitCellVolume      ?Volume .
  ?Volume    om:hasNumericalValue       ?volume .
	}

  OPTIONAL {
    ?abc     ocr:hasUncertaintyVector  ?dabc .
    ?dabc    ocr:hasVectorComponent    ?dabc_a, ?dabc_b, ?dabc_c.
    ?dabc_a  ocr:hasLabel              "a";
             ocr:hasComponentValue     ?d_a .
    ?dabc_b  ocr:hasLabel              "b";
             ocr:hasComponentValue     ?d_b .
    ?dabc_c  ocr:hasLabel              "c";
             ocr:hasComponentValue     ?d_c .
    }
  OPTIONAL {
    ?Volume    ocr:hasUncertainty      ?DVol .
    ?DVol      om:hasNumericalValue    ?d_vol .
    }
  OPTIONAL {
    ?unitcell  ocr:hasLatticeSystem    ?lattice .
    ?unitcell  ocr:hasSymmetryNumber   ?symmNum .
    }
  """ +
  """
  #FILTER ( ?volume  >  16000 && ?volume < 45000 ) .
  #FILTER ( ?volume  <  45000  ) .
  #FILTER ( xsd:decimal(?a) = 90 ) .
  #FILTER ( xsd:decimal(xsd:string(?alpha)) != 90 ) .

  #FILTER ( xsd:decimal(xsd:string(?a)) > 5 && xsd:decimal(xsd:string(?a)) < 20 ) .
  #FILTER ( xsd:decimal(xsd:string(?b)) > 5 && xsd:decimal(xsd:string(?b)) < 20 ) .
  #FILTER ( xsd:decimal(xsd:string(?c)) > 5 && xsd:decimal(xsd:string(?c)) < 20 ) .
  #FILTER ( xsd:decimal(xsd:string(?alpha)) > 60 && xsd:decimal(xsd:string(?alpha)) < 120 ) .
  #FILTER ( xsd:decimal(xsd:string(?beta )) > 60 && xsd:decimal(xsd:string(?beta )) < 120 ) .
  #FILTER ( xsd:decimal(xsd:string(?gamma)) > 60 && xsd:decimal(xsd:string(?gamma)) < 120 ) .
  #FILTER ( ?lattice = "orthorhombic" ) .
  #FILTER ( ?symmNum = 74 ) .
  }
#ORDER BY (-xsd:decimal(xsd:string(?volume)))
#LIMIT 10
      """ )

        endpoint.setReturnFormat(JSON)
        output = []
        output.append( "#####################################" )
        output.append( "#" )
        output.append( "# CIF file for zeolite '" + zeoName + "' generated by ZeoliteAgent " )
        output.append( "#" )
        output.append( "#####################################" )

        results = endpoint.query().convert()

        #print( results["results"]["bindings"] )
        #for res in results["results"]["bindings"]:
        if True:
            res = results["results"]["bindings"][0]

            output.append( "" )
            output.append( "_cell_length_a     " + res["a"]["value"] )
            output.append( "_cell_length_b     " + res["b"]["value"] )
            output.append( "_cell_length_c     " + res["c"]["value"] )
            output.append( "_cell_angle_alpha  " + res["alpha"]["value"] )
            output.append( "_cell_angle_beta   " + res["beta" ]["value"] )
            output.append( "_cell_angle_gamma  " + res["gamma"]["value"] )
            output.append( "" )
           
        ### Adding the atom coordinates:

        endpoint = SPARQLWrapper( address )
        endpoint.setQuery( """
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>

#SELECT ?x ?y ?z
SELECT ?zeoname ?afx ?afy ?afz #?a1occup ?a1label #?aF_Unit
#SELECT ?zeoname
#SELECT ?zeoname ?rax ?ay ?az ?volume
#SELECT ?zeo ?a1
WHERE {
  ?zeo       zeo:hasZeoliteCode     '""" +
  zeoName +
  """'.
  ?zeo       ocr:hasCrystalInformation       ?cifcore .
  ?cifcore   ocr:hasAtomicStructure          ?atomic.
  ?atomic    ocr:hasAtomSite                 ?a1 .
  ?a1        ocr:hasFractionalPosition     ?aF_xyz.

  ?aF_xyz    ocr:hasVectorComponent     ?aF_x, ?aF_y, ?aF_z .
  ?aF_x      ocr:hasComponentValue      ?afx ;
  ocr:hasComponentLabel       "x" .
  ?aF_y      ocr:hasComponentValue      ?afy ;
  ocr:hasComponentLabel       "y" .
  ?aF_z      ocr:hasComponentValue      ?afz ;
  ocr:hasComponentLabel       "z" .

  #?aF_xyz     om:hasUnit               ?aF_Unit .
  #?aF_Unit  rdfs:label                 ?af_unit .
  #?aC_xyz     om:hasUnit               ?aC_Unit .
  #?aC_Unit  rdfs:label                 ?ac_unit .
  OPTIONAL {
    ?a1       ocr:hasAtomSiteLabel        ?a1label .
  }
  OPTIONAL {
    ?a1       ocr:hasOccupancy           ?a1occup.
  }
}

  #LIMIT 5
""" )

        endpoint.setReturnFormat(JSON)

        results = endpoint.query().convert()
        output.append( "_loop" )

        output.append( "_atom_site_fract_x" )
        output.append( "_atom_site_fract_y" )
        output.append( "_atom_site_fract_z" )
 
        for res in results["results"]["bindings"]:
            output.append( str(res["afx"]["value"]) + "\t" + \
                           str(res["afy"]["value"]) + "\t" + \
                           str(res["afz"]["value"]) )
 
        for line in output:
            print( line )

        with open( filename, "w", encoding="utf-8" ) as fp:
            for line in output:
                fp.write( line + "\n" )

        return output
        pass # GuiZeolite.makeCifFile()

    pass # class GuiZeolite

if __name__ == "__main__":

  GuiZeolite()
  #print( linkToLibLink( "https://pubs.rsc.org/en/content/articlepdf/2017/cc/c7cc05552h" ) )



