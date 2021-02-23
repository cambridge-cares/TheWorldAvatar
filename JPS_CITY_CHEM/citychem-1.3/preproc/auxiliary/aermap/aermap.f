      PROGRAM AERMAP
C***********************************************************************
C*    MAIN Module of the AMS/EPA Regulatory Model Terrain Preprocessor
C*                               AERMAP
C*                         Version Dated 11103
C*
C*                            April 13, 2011
C*
C*             *** SEE AERMAP MODEL CHANGE BULLETIN MCB#3 ***
C*
C*     ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS (SCRAM) WEBSITE
C*
C*                     http://www.epa.gov/scram001/
C*
C***********************************************************************
C
C       This revised version of AERMAP (dated 11103) includes the
C       following modifications relative to the previous version
C       (dated 09040); see MCB#3 and updated User's Guide Addendum:
C
C-----  Bug Fixes:
C
C       1.  Corrected formatting issue for message indicating that 
C           source location is not assigned to a terrain file, in
C           sub_demsrc. This error condition resulted in a Fortran
C           runtime error with version 09040.
C
C-----  Enhancements:
C
C       1.  Modified several subroutines to increase maximum length 
C           of source IDs from 8 to 12 characters, to be consistent
C           with enhancement introduced with version 11059 of AERMOD.
C
C-----  Miscellaneous:
C
C       1.  Modified subroutine ERRHDL to set an upper limit on line 
C           number included in error messages to avoid overflowing 
C           the field.
C
C
C-----  MODIFIED BY:    Roger W. Brode
C                       U.S. EPA, OAQPS/AQAD
C                       Air Quality Modeling Group
C
C                       April 13, 2011
C
C-----  MODIFIED FROM:          AERMAP
C                       (Version Dated 09040)
C
C***********************************************************************
C
C       This revised version of AERMAP (dated 09040) includes the
C       following modifications relative to the previous version
C       (dated 06341); see MCB#2:
C
C-----  Bug Fixes:
C
C       1.  Corrected errors with processing of DEM data for Alaska,
C           including handling of non-uniform delta-x and delta-y DEM 
C           node spacing.  These corrections also permit use of all 
C           types of Alaska DEM data, including 1-Degree, 15-Minute, 
C           and 7.5-Minute data.
C
C       2.  Corrected error that may occur with some cross-UTM zone 
C           applications when NAD (datum) conversions are performed.
C
C       3.  Modified several subroutines to address errors and potential
C           issues associated with receptors and sources located beyond 
C           the range of DEM profiles that are not accounted for by checks
C           included in previous versions. These problems have primarily 
C           been associated with non-standard (non-USGS) DEM files that
C           were converted from other data formats.  Receptors located 
C           in gaps between terrain files due to NAD conversions and 
C           receptors located within gaps inside terrain files are more 
C           clearly identified and documented.  Receptor (and source) 
C           elevations for gap locations are assigned a missing code 
C           of -9999.0, unless the new 'FILLGAPS' option on the DATAFILE
C           keyword for DEM files is specified. Modified the MAIN 
C           program unit to loop through all terrain files until a 
C           non-missing (non-gap) elevation is determined. Also improved 
C           the handling of "edge receptors" beyond the range of profiles 
C           within 7.5-minute DEM files to check for the north-south 
C           displacement in selecting the closest elevation nodes.  
C           These issues with edge and gap receptors should be eliminated 
C           with the enhancements to AERMAP to support the use of USGS 
C           National Elevation Dataset (NED) input data (see item #1 
C           below under Enhancements).  Note that the 'FILLGAPS' option
C           is not available for NED files since these gaps should
C           not occur with the use of NED data. 
C
C       4.  Modified subroutine OUCARD to correct errors with OU 
C           pathway options, which required that RECEPTOR keyword 
C           precede the SOURCLOC keyword in the input file.
C
C       5.  Modified subroutine SOLOCA to recognize all source types 
C           currently supported by AERMOD on the SO pathway, including 
C           POINTCAP, POINTHOR, and OPENPIT.
C
C       6.  Fixed bugs related to assignment of domain for cases with
C           7.5-minute DEM files based on UTM coordinates and domain
C           based on geographic coordinates using the DOMAINLL keyword.  
C           This bug could have resulted in AERMAP errors for domain 
C           outside the range of DEM data for applications where the 
C           domain extends close to the edge of the data.
C
C       7.  Modified subroutines DEMREC, DEMSRC, and CHKEXT to allow
C           for non-standard (non-USGS) terrain files with boundaries 
C           that follow UTM lines rather than latitude/longitude lines 
C           in assigning receptors, sources, and domain corners to 
C           terrain files.
C
C       8.  Modified subroutine INITER_DEM to correct the application 
C           of the adjustment for elevation of local datum (LOCEL) for
C           DEM files.
C
C       9.  Modified subroutines INITER_DEM, RECELV, and SRCELV 
C           to address potential problems with handling of nodes
C           assigned missing elevations in DEM and NED files.   
C
C
C-----  Enhancements:
C
C       1.  Added capability to process USGS National Elevation Dataset
C           (NED) input data in place of DEM data.  AERMAP currently
C           supports NED data files in the GeoTIFF format, requiring
C           only the *.tif file downloaded from the USGS Seamless 
C           Data Server (http://seamless.usgs.gov/).  AERMAP allows
C           for multiple NED files, and supports GeoTIFF files
C           in geographic or projected (UTM) coordinates, although NED
C           data from the USGS Seamless Data Server should be in 
C           geographic coordinates.  Note that GeoTIFF is NOT the 
C           default data format for NED data downloaded from the 
C           Seamless Data Server.  The user must modify the download 
C           request to specify the format as GeoTIFF.
C
C       2.  Incorporated capability for processing "mixed" DEM files
C           in a single AERMAP run, including all types of Alaska DEMs, 
C           as well as non-Alaskan 1-Degree and 7.5-Minute DEMs.
C           Receptor elevations are assigned based on the first 
C           DEM file encountered that contains the receptor, unless
C           the receptor is located within a data gap inside the file.
C           Users are required to identify more refined, higher 
C           resolution data first in the runstream input file
C           if a receptor is located within more than one DEM file.
C           This permits the use of the highest resolution data 
C           available to estimate elevations, even though the higher
C           resolution data may not be available for the entire modeling 
C           domain.  This enhancement allows the use of 1-degree DEM 
C           data to fill in gaps within the coverage of 7.5-minute DEM 
C           data, such as 7.5-minute quadrangles that are entirely over 
C           water.  AERMAP also allows "mixed" resolution NED files, 
C           with the same restrictions as for DEM files.  However, 
C           AERMAP does NOT support both DEM and NED files in the same 
C           AERMAP run.  
C
C       3.  The DOMAINXY or DOMAINLL keyword is no longer required 
C           on the CO pathway.  When the user-specified domain is 
C           omitted, AERMAP will use all of the elevation data 
C           included in the input files in the calculation of 
C           critical hill height scales.
C
C       4.  Array storage limits in AERMAP are now dynamically 
C           allocated at runtime, based on the data requirements
C           of the specific application.  This includes explicit
C           treatment of the number of DEM/NED files, sources, 
C           receptors, profiles per data file and the maximum 
C           number of nodes per profile.
C
C       5.  Added support for the INCLUDED keyword on the SO and
C           RE pathways to allow source and receptor data from
C           separate files to be included in the AERMAP input.
C
C       6.  Modified subroutine CALCHC to use a more accurate 
C           approach for calculating distances based on 
C           geographic coordinates, from the NGS INVERSE program.
C           Modified code for optimizing hill height calculation
C           by skipping DEM or NED files when possible to be more 
C           generic in order to support the use of mixed and 
C           non-standard DEM files. Also modified subroutine 
C           CALCHC to improve optimization for processing hill
C           height calculations, including changes to take 
C           advantage of more accurate distance calculation 
C           for geographic files to calculate distances between
C           profiles.
C
C       7.  Modified subroutine DATFIL to include an option on the
C           DATAFILE keyword for NED data to generate a GeoTIFF debug 
C           output file containing results for all TIFFTags and GeoKeys 
C           contained within the file.  The DATAFILE keyword for NED
C           data also includes an option for user-specified elevation 
C           units for NED data on the DATAFILE keyword.
C 
C           The optional keyword for the TIFF debug file is 
C           'TIFFDEBUG'.  AERMAP generates a hard-coded filename
C           for the TiffDebug file as 'TiffDebugFile_nnnnn.dbg',
C           where 'nnnnn' is the NED file number based on the
C           order listed in the AERMAP.INP file.  The optional
C           user-specified elevation units can be specified as 
C           'FEET', 'METERS', 'DECIFEET', 'DECIMETERS', 'DECAFEET',
C           or 'DECAMETERS' (note that secondary keywords are not
C           case-sensitive).  Most NED GeoTIFF files currently do 
C           not include the GeoKey to identify the elevation units.  
C           The default assumed by AERMAP in those cases is 'METERS', 
C           based on documentation provided by the USGS for NED 
C           data.  If elevations units are specified within the
C           GeoTIFF file and a conflict is found between the GeoKey 
C           in the NED file and the user-specified units, a  
C           fatal error message is generated to abort processing.
C           Also note that the optional 'CHECK' parameter available 
C           on the DATAFILE keyword for DEM files is not applicable 
C           to NED data.
C
C       8.  Increased maximum length for filenames to 200 (controlled 
C           by the ILEN_FLD parameter in mod_main1.f), and the maximum 
C           input string length to 512 (controlled by the ISTRG 
C           parameter in mod_main1.f).  Also modified subroutine DEFINE 
C           to allow double quotes (") as field delimiters in the 
C           AERMAP.INP file to support filenames with embedded spaces.
C           Note that the double quotes do not count toward the limit
C           for the maximum field length.
C
C       9.  Added an optional keyword for the CO pathway to allow 
C           the user to specify a pathname for the NADCON grid shift
C           files (such as CONUS.LAS and CONUS.LOS).  The optional 
C           keyword is NADGRIDS.  The pathname can include embedded 
C           spaces if double quotes (") are used as field delimiters.  
C           The default path for the NADCON grid shift files without
C           the NADGRIDS keyword is the local folder containing the
C           AERMAP.INP file.  Also moved the call to subroutine
C           NGRIDS to follow calls to subroutines DEMCHK and NEDCHK 
C           such that the NADCON grid shift files are only accessed 
C           if needed, based on the value of NADA specified by the 
C           user and the reference datums for all of the terrain files.
C
C
C-----  Miscellaneous:
C
C       1.  Modified several subroutines, including UTMGEO, NADCON, 
C           DEMREC, DEMSRC, RECCNV, SRCCNV, RECELV, and SRCELV, to 
C           use standard convention of negative values for West 
C           longitude.  This simplifies some aspects of the code, 
C           improves consistency with other tools, such as AERSURFACE, 
C           improves ability to support alternative elevation data
C           sources, such as NED, and improves portability of AERMAP 
C           for applications beyond the Northern/Western hemispheres.
C
C           As part of these modifications, the DOMAINLL and DOMAINXY
C           keywords both expect the domain to be defined by the 
C           coordinates for the lower-left and upper-right corners of
C           the domain.  The DOMAINLL keyword in previous versions
C           required the domain to be specified based on the lower-
C           right and upper-left corners.  Code has been included in 
C           AERMAP to support older AERMAP.INP files by adjusting the 
C           DOMAINLL inputs as necessary to conform with the new 
C           convention.  A warning message is generated in these cases,
C           and the adjusted DOMAINLL inputs are included in the 
C           AERMAP.OUT file, as well as the RECEPTOR and SOURCLOC
C           output files.  The direct access terrain file format
C           has also been modified to use I3 for the UTM zone field,
C           to accommodate negative UTM zones, which are used to
C           indicate Southern Hemisphere coordinates.
C
C       2.  Adjusted "map array" outputs in MAPPARAMS.OUT debug
C           file to allow for mixed DEM files and to improve
C           handling of non-standard DEM files, converted from 
C           other formats.
C
C       3.  Modified data structures for storing DEM file corner
C           coordinates (new subroutine CNRCNV) to improve the 
C           readability of code, eliminate redundancy, and to 
C           use consistent reference points for domain.
C
C       4.  Modified subroutine OPENFL in NADCON.F to assume
C           record length based on bytes rather than words
C           when reading the binary NAD grid files (*.las and 
C           *.los files).  This will improve the portability of 
C           AERMAP to other platforms and compilers, but may 
C           require inclusion of compiler options to specify the 
C           record length based on bytes for some compilers, such 
C           as the /assume:byterecl option for the Intel Fotran 
C           compiler used to generate the Windows-based executable 
C           for AERMAP.
C
C       5.  Modified subroutine SOCARD to ignore the SRCPARAM
C           keyword on the SO pathway, but issue a warning message. 
C           Previous versions of AERMAP did not allow for the
C           SRCPARAM keyword, causing a fatal error.  This may
C           facilitate the inclusion of portions of AERMOD input
C           files with source locations into the AERMAP.INP file 
C           more easily in some cases.
C
C       6.  Replaced ELEV logical variable, used to specify that
C           terrain elevations would be provided by the user, with
C           new EXTRACT logical variable to specify that terrain
C           elevations will be extracted from DEM data (the default
C           option).  EXTRACT=.true. in version 09040 code is 
C           equivalent with ELEV=.false. in previous versions.
C
C       7.  Modified subroutines SRCOUT and RECOUT to include
C           model rundate and runtime in the header of the 
C           source and receptor output files.  Also modified 
C           appropriate subroutines to include a header with 
C           version date, rundate and runtime in the debug 
C           output files.
C
C       8.  Modified data structures and formats to support use
C           of double precision for elevation variables.
C
C       9.  Modified subroutine HEADER in aermap.f to use ASCII
C           form feed character [ACHAR(12)] to eliminate need for
C           non-standard option of CARRIAGECONTROL = 'FORTRAN' in
C           OPEN statement for 'aermap.out' file, and modified 
C           subroutine FILOPN to eliminate the use of the 
C           CARRIAGECONTROL = 'Fortran' option when opening the
C           aermap.out file.  This improves the portability of the
C           AERMAP code.  Also modified subroutines SETUP and COCARD 
C           to remove '1X' from format statements for echoing runstream 
C           inputs to output file since Fortran carriage-control is no 
C           longer applied.  This eliminates the column shift in the 
C           input portion of the aermap.out file.
C
C      10.  Miscellaneous code clean-up, including;
C           
C           a. modifying subroutine DEMCHK to eliminate redundant 
C              information in the MAPDETAIL.OUT debug file, eliminate
C              reading of the Record B data to optimize runtime, and
C              simplify logic related to determining the file record type; 
C           b. modify the DOMDETAIL.OUT debug file to include 
C              information on the direct access files used to store
C              elevations for the portion of each terrain file that
C              falls within the user-specified domain; 
C           c. including error checking on the length of user-specified 
C              filenames to trap on filenames that the limit based on the
C              ILEN_FLD parameter; 
C           d. modifying criterion for "latitude out-of-range" warning 
C              from 60 to 66 degrees; 
C           e. cleaning up unused portions of NADCON code; 
C           f. additional initialization of variables where needed; 
C           g. error checking on consistency between UTM zone on ANCHORXY 
C              keyword and the UTM zone(s) based on the domain or 
C              range of data; and 
C           h. cleanup of unused error and warning messages and 
C              reordering of messages.
C
C
C-----  MODIFIED BY:    Roger W. Brode
C                       U.S. EPA, OAQPS/AQAD
C                       Air Quality Modeling Group
C
C                       Clint R. Tillerson
C                       MACTEC Federal Programs, Inc.
C
C                       February 9, 2009
C
C-----  MODIFIED FROM:          AERMAP
C                       (Version Dated 06341)
C
C***********************************************************************
C*
C*       This revised version of AERMAP (dated 06341) includes the 
C*       following modifications relative to version 04300; see MCB#1:
C*
C*       - Corrected several problems associated with NAD conversion
C*         process.  Previous version did not properly account for
C*         shift in UTM coordinates due to different ellipsoids used
C*         for the different datums, which is typically about 200m in
C*         the Northing coordinate.  Modified the incorporation of
C*         the NADCON program to use an iterative calculation for
C*         conversions from NAD82 to NAD27.  Other issues related to
C*         how NADA values were processed have been addressed.
C*
C*       - Corrected initialization problem that resulted in erroneous
C*         calculation for the first elevation (either source or
C*         receptor elevation, depending on inputs).
C*
C*       - Corrected array subscript out-of-bounds problem associated
C*         with reading blank NAD value from header record of old DEM
C*         files (typically 1-degree DEMs) that may have resulted
C*         in runtime errors in previous version.
C*
C*       - Corrected problem that may occur when reading data from DEM
C*         files that do not include carriage returns or line feeds
C*         (i.e., one continuous ASCII record).  Problem in previous
C*         version would result in runtime error for some DEM files.
C*
C*       - Corrected problem with the procedure to optimize hill
C*         height scale calculations by checking for maximum elevation
C*         from non-local DEM files and assigning that elevation to the
C*         closest point between the receptor and the DEM file. The
C*         previous version of AERMAP would skip some files that should
C*         not have been skipped.
C*
C*       - Incorporated additional optimization for calculation of
C*         critical hill height scales by skipping profiles within DEM
C*         files in CALCHC based on maximum elevation within the
C*         profile and based on closest distance between receptor and
C*         DEM file for adjacent DEMs and closest distance between
C*         receptor and profile for local DEMs.  The maximum elevation
C*         within a profile was added to the index file for the direct
C*         access files containing DEM nodes within the domain.
C*
C*       - Corrected problems (related to NAD conversion issues) for
C*         applications that cross UTM zone boundaries.
C*
C*       - Corrected runtime error that may occur in DISTLL for
C*         receptors (or sources) that are collocated with a DEM node.
C*
C*       - Modified procedure used to identify which DEM file a
C*         receptor or source is located within: initial check is made
C*         based on zero distance tolerance relative to edges of the
C*         DEM files; warnings are issued for receptors and sources
C*         not located within a DEM based on initial check; and a
C*         second check is made for receptors that may be located
C*         within a "gap" between adjacent DEM files with different
C*         datums due to the NAD conversion.  The distance tolerance
C*         used to assign "gap" receptors to a DEM file is based on
C*         the maximum datum shift for that application.  One half of
C*         the datum shift is used for the tolerance in order to
C*         assign the receptor to the closest DEM file.  Any receptor
C*         or source not located based on second check will generate
C*         a fatal error message.
C*
C*       - Modified the procedure to calculate receptor and source
C*         elevations to use four closest nodes within the DEM file
C*         containing the source or receptor.  Also modified procedure
C*         for assigning elevations to receptors located beyond the
C*         range of profiles along the edges of 7.5-minute DEM files
C*         to account for proximity to nodes in the Northing as well
C*         the Easting directions.  The datum "gap" receptors are also
C*         treated as "extended edge" receptors.  Also reverted to the
C*         original 2-D (bilinear) interpolation for calculating source
C*         and receptor elevations based on four (or two) closest nodes.
C*
C*       - Modified CALCHC to use distances between receptors and
C*         DEM nodes based on UTM coordinates for 7.5-minute DEMs;
C*         arc-distances based on Lat/Lon coordinates are still
C*         used for 1-degree DEMs.
C*
C*       - Moved call to DEMCHK to follow check for fatal errors
C*         during SETUP, and added error checking after return from
C*         DEMCHK to avoid proceeding with invalid data.
C*
C*       - Added optional "CHECK" option to CO DATAFILE card to
C*         activate full check of DEM file in subroutine DEMCHK;
C*         default operation is to check the first 20480 characters
C*         to determine file type (DOS, UNIX, no CR/LF, Binary).
C*
C*       - Added user options to control "debug" output files on
C*         new CO DEBUGOPT card.  Debug options include "HILL" for
C*         hill height scale calculations (performed in subroutine
C*         CALCHC), "RECEPTOR" for receptor related information, and
C*         "SOURCE" for source related information.  User can also
C*         specify "ALL" to request debug outputs for all three.
C*         Previous informational files regarding DEM files and domain
C*         setup (MAPDETAIL.OUT, MAPPARAMS.OUT, and DOMDETAIL.OUT)
C*         are automatically generated.  Content of debug files
C*         has also been modified to be more useful in documenting
C*         and resolving problems with the code or with the setup
C*         of the input data.
C*
C*       - Reinserted code for subroutine DATTYP to process the
C*         DATATYPE keyword (current options include DEM1 or DEM7),
C*         and incorporated error checking for inconsistencies between
C*         DATATYPE card and input DEM files.
C*
C*       - A number of miscellaneous changes to the code have been made
C*         to provide better documentation of the processing steps and
C*         better structure to the code.  Some extraneous code has also
C*         been deleted.  The number of miscellaneous changes is too
C*         numerous to list in detail.
C*
C*
C*       Modified by:      Roger W. Brode
C*                         U.S. EPA, OAQPS, AQMG
C*
C*       MODIFIED FROM:       (Version Dated 04300)
C*
C***********************************************************************
C*
C*                            (Version Dated 04300)
C*
C*       MODIFIED FROM:       (Version Dated 03107)
C*
C*       MODIFIED BY:   Peter Eckhoff, U.S. EPA
C*                      August, 2003
C*
C*
C*       Modified to:
C*
C*         - correct reported bugs
C*         - add a better method of reading DEM files with
C*             different record lengths 
C*         - add additional error checking statements
C*         - add domain independent hill height algorithm
C*         - initialize all variables
C*         - add, but comment out OPEN statements 
C*             thereby allowing a Command Line Interface to be 
C*             the default input method.
C*         - add several output files that will aid in understanding
C*           the processes taking place and to supply a visual 
C*           method for spotting discrepancies in the DEM files
C*         
C*
C*       MODIFIED FROM:       (Version Dated 02222)
C*
C*       MODIFIED BY:   Peter Eckhoff, U.S. EPA
C*                      November, 2002
C*
C*
C*       Modified to:
C*
C*         - correct where in the DEM header the meter to decimeter factor is 
C*             generated
C*         - correct for when a receptor/source is directly over a node and the 
C*             resulting elevation is zero (See Dick Perry code/ Bharvey from 9/4 email)
C*         - in some rare instances, the DCOS argument is greater than 1.0, added
C*             a check to make it 1.0
C*         - update SDST2DEM from version 0.014 to 0.018 (this is a separate program)
C*         - correct NADA selection
C*         - correct a process where receptors lying well within empty-node 
C*             areas of a DEM file (not just lying near an edge), the receptors 
C*             are assigned large negative numbers (Jason Maranche - email)
C*
C*
C*       MODIFIED FROM:       (Version Dated 02222)
C*
C*       MODIFIED BY:   Russell F. Lee
C*                      With financial support from Lakes Environmental
C*
C*       Modified to correct:
C* 
C*         - an error in defining unit numbers for LUOUT and NAPAR
C*         - in the variable IS intended to select the Clarke 1866 ellipsoid 
C*             parameters
C*         - an error that could cause array limits to be exceeded if more
C*             than one user-defined grid files are used
C*         - an error in a READ format and a FUNCTION statement
C*       Modified to allow international users to bypass subroutine NADCON21 
C*
C*
C*       MODIFIED FROM:       (Version Dated 99211)
C*
C*       MODIFIED BY:   Peter Eckhoff, U.S. EPA
C*                      July, 2002
C*
C*       Modified to add the following features: 
C*         - converting anchor, receptor, source and elevation coordinates
C*            from North American Datum (NAD) of 1927 and other datums
C*            to NAD 1983 using the Unites States Geological Survey (USGS)
C*            sanctioned program, NADCON version 2.1, 
C*         - updating coordinates to GRS80,
C*         - standardizing the code to Fortran 90,
C*         - eliminating compiler specific commands such as the
C*           Lahey-specific Command Line Interface subroutines,
C*         - changing all coordinate related variables so they are now double  
C*           precision which eliminates having to drop the first digit of  
C*           the northing values,
C*         - using file OPEN statements to read in the I/O file names,
C*         - adding the ability to process 200 DEM files instead of just 20.
C*       
C*       A preprocessor routine was added to the AERMAP package for  
C*       converting the USGS 
C*       Spatial Data Transfer Standard (SDTS) file format data 
C*       back to the old Digital Elevation Model (DEM) format.
C*       The preprocessor is based on the Sol Katz conversion program,
C*       SDTS2DEM, version 0.014 and is executed as an interactive batch
C*       file.  Additional information and links are available from the 
C*       web site: http://www.gisdatadepot.com.
C*
C*       NADCON IS THE FEDERAL STANDARD FOR NAD27 TO NAD83 DATUM 
C*       TRANSFORMATIONS.  The NADCON program, Version 2.10, was modified 
C*       into a set of subroutines and then edited into AERMAP.
C*       ADDITIONAL INFORMATION CONCERNING NADCON IS AVAILABLE   
C*       FROM THE NGS WEB SITE AT:
C*
C*       HTTP://WWW.NGS.NOAA.GOV:80/TOOLS/NADCON/NADCON.HTML  
C* 
C*       The Subroutine UTMGEO has been modified to accept the  
C*       output from NADCON and to convert the Lat/Lon values to
C*       UTM coordinates based on the GRS80 ellipsoid.  The semimajor and
C*       semiminor axis values for the International 1909 standard have 
C*       been replaced by the GRS80 axis values in UTMGEO.
C*
C*       AERMAP has been rewritten to Fortran 90 standards.
C*
C*
C*       MODIFIED FROM:       (Version Dated 98022)
C*
C*       MODIFIED BY:   Roger W. Brode, PES, Inc.
C*                      July 20, 1999
C*
C*       Modified for compatibility with the Lahey LF90 compiler.
C*
C*
C*       MODIFIED FROM:       (Version Dated 97119)
C*
C*       MODIFIED BY:   Roger W. Brode
C*                      January 22, 1998
C*
C*       Modified to correct an error that occurs with 1-degree DEM data
C*       when the number of sources exceeds the number receptors, and to
C*       prevent an error that may occur if a source or receptor is
C*       collocated with a node of the DEM file.
C*
C*
C*       MODIFIED FROM:       (Version Dated 96274)
C*
C*       MODIFIED BY:   Roger W. Brode
C*                      April 29, 1997
C*
C*       Modified to accept 7.5-minute DEM data in addition to 1-degree DEM
C*       data, and to add the SOurce pathway to allow for input of source
C*       locations.  The OUtput pathway has also been added to allow for
C*       specification of receptor and source output data files.
C*
C*
C*       MODIFIED FROM:       (Version Dated 96058)
C*
C*       MODIFIED BY:   Roger W. Brode
C*                      September 30, 1996
C*
C*       Modified to correct an error in SUB. INT2D of RCALC.FOR.
C*
C*
C*       MODIFIED FROM:       (Version Dated 95352)
C*
C*       MODIFIED BY:   Roger W. Brode
C*                      February 27, 1996
C*
C*       Modified to correct a problem with multiple DEM files.
C*
C*
C*       PURPOSE: Controls Overall Flow and Processing of AERMAP
C*
C*       PROGRAMMED BY: Jayant A. Hardikar
C*                      Roger W. Brode
C*                      James O. Paumier
C*                      
C*                      Pacific Environmental Services, Inc.
C*                      P.O. Box 12077
C*                      Research Triangle Park, North Carolina  27709
C*
C*       DATE:          December 18, 1995
C*
C*
C*       INPUTS:        
C*
C*       OUTPUTS:       
C*
C*
C***********************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE

      SAVE
      
      LOGICAL L_FileExist, L_FIRST
      CHARACTER*12 MODNAM
      INTEGER IREC, ISRC

      MODNAM = 'MAIN'
      
C*    Initialize FileExist logical variable
      L_FileExist = .FALSE.
      L_FIRST     = .TRUE.
  
C*    Open the Temporary File for Error Messages Generated from the Program
      OPEN(UNIT=IERUNT,FILE='ERRMSG.TMP',STATUS='REPLACE')

C*    Open AERMAP.INP and AERMAP.OUT                         ---   CALL FILOPN
      CALL FILOPN

C     Preprocess Setup Information to Determine Data Storage Needs
      CALL PRESET

C     Allocate SETUP Array Storage
      CALL ALLSETUP

C*    Variable Initializations                              ---   CALL VARINI
      CALL VARINI

C     Get Date and Time using Fortran 90 functions          ---   CALL DATIME
      RUNDAT = ' '
      RUNTIM = ' '
      CALL DATIME (RUNDAT, RUNTIM)

      WRITE(*,119)
 119  FORMAT('+','Processing Setup... ')

C*    Write Version Date Header to Output Message File
      WRITE(IOUNIT,9011) VERSN, RUNDAT, RUNTIM
9011  FORMAT('** AERMAP - VERSION ',A5,T72,A8,/
     &       '**',T72,A8/)

C*    Process The Model Setup Information                   ---   CALL SETUP
      CALL SETUP

      IF (.NOT. FATAL) THEN

C ---    Check for data type, 'DEM' or 'NED'
         IF (TYPDAT .EQ. 'DEM') THEN
C           Read each DEM file and analyze it.  Report any discrepancies with
C           respect to the USGS Standards for DEMs.          --- CALL DEMCHK

            CALL DEMCHK 

            write(IOUNIT,*) ' '
            write(IOUNIT,*) 'Finished Reading Input DEM Data'
            write(IOUNIT,*) ' '

C*          Check for DEM file error
            IF (DEMERR) THEN
               WRITE(IOUNIT,*)' '
               WRITE(IOUNIT,*)'Error Occurred in DEMCHK'
               WRITE(IOUNIT,*)' '
               WRITE(*,*) ''
               WRITE(*,*) 'Error Occurred in DEMCHK'
               WRITE(*,*) ''
C*             Skip over data processing               
               GO TO 99
            END IF
         
         ELSE IF (TYPDAT .EQ. 'NED') THEN
C           Read each NED file and analyze it.  Report any
C           discrepancies.                                --- CALL NEDCHK

            CALL NEDCHK

            write(IOUNIT,*) ' '
            write(IOUNIT,*) 'Finished Reading Input NED Data'
            write(IOUNIT,*) ' '

C*          Check for NED error
            IF (NEDERR) THEN
               WRITE(IOUNIT,*) ' '
               WRITE(IOUNIT,*) 'Error Occurred in NEDCHK'
               WRITE(IOUNIT,*) ' '
               WRITE(*,*) ''
               WRITE(*,*) 'Error Occurred in NEDCHK'
               WRITE(*,*) ''
C*             Skip over data processing               
               GO TO 99
            END IF
         
         END IF

C*       Read interpolation factors for converting NAD27 to NAD83 in NADCON
C*       in NADCON 2.1, if needed
      
         IF (.NOT. FATAL .AND. NADA .NE. 0 .AND. L_NeedNADCON) THEN
      
C*          Check for user-specified NADGRIDS pathname,
C*          and set length of pathname for call to NGRIDS
            IF (.NOT. NADPATH) THEN
               ILEN_PATH = 0
               NADGRID_PATH = ''
            ELSE
               ILEN_PATH = LEN_TRIM(NADGRID_PATH)
            END IF
            
            CALL NGRIDS (NODATA,.true.,ILEN_PATH,NADGRID_PATH)
      
            IF (NODATA) THEN
C*             NADCON grid files not found; issue fatal error
C*             and skip over rest of data processing
               CALL ERRHDL(PATH,MODNAM,'E','365','NADGRIDS')
               FATAL = .TRUE.
               GO TO 99
            END IF
      
         ELSE
C*          NADCON grid files are not needed, either because
C*          NADA = 0 or because all file datums are consistent
C*          with NADA
      
            NODATA = .FALSE.
      
         END IF

C*       Open MAPPARAMS.OUT debug file         
         OPEN (UNIT = MAPK, FILE = MAPPARAMS_FILE, STATUS = 'REPLACE')

C*       Write Version Date Header to MAPPARAMS.OUT File
         WRITE(MAPK,9011) VERSN, RUNDAT, RUNTIM

C ---    Convert corner coordinates from native units (arc-seconds for geographic 
C        or meters for UTM) in DMCNR to store all three formats (arc-seconds, degrees,
C ---    meters) in corner arrays                              --- CALL CNRCNV
         CALL CNRCNV

C*       Check for adjacency of DEM and NED files              --- CALL CHKADJ
         CALL CHKADJ
         
C*       Close MAPPARAMS.OUT debug file         
         CLOSE(MAPK)

C*       Synchronize the Coordinate Systems of the Domain, if specified, and the
C*       Terrain File Type.                                    ---  CALL DOMCNV
         IF (GOTDOMFLG) THEN
            CALL DOMCNV
         END IF

C*       Determine Receptor X,Y Coordinates in Lat/Long        ---  CALL RECCNV
         CALL RECCNV

         IF (NUMSRC .GT. 0) THEN
C*          Determine Source X,Y Coordinates in Lat/Long       ---  CALL SRCCNV
            CALL SRCCNV
         END IF

C*       If domain is specified, check to see that the extent of the 
C*       receptors (and sources) are within the extents of the
C*       user-specified domain and the domain itself is within the 
C*       supplied raw terrain files                            ---  CALL CHKEXT
         IF (GOTDOMFLG) THEN
            CALL CHKEXT
         END IF
         
C*       Find which DEM file each receptor falls within        --- CALL DEMREC
         CALL DEMREC
         
         IF (NUMSRC .GT. 0) THEN
C*          Find which DEM file each source falls within       --- CALL DEMSRC
            CALL DEMSRC
         END IF

      END IF

C*    Branch to skip processing due to data error (DEMERR or NEDERR)
99    CONTINUE

C*    Determine Number of Setup Messages by Message Type    ---   CALL TERRST
      CALL TERRST

      IF (.NOT.RUN .OR. FATAL .OR. IWRN .GT. 0) THEN
C*       Write Out Summary Of Setup Error/Message Stats     ---   CALL SUMTBL

C*       Call routine to print page header                  ---   CALL HEADER
         CALL HEADER
      
         WRITE (IOUNIT,9111)
 9111    FORMAT(//2X,'*** Message Summary For AERMAP Setup ***'/)
         CALL SUMTBL
      END IF

      IF (FATAL) THEN
         WRITE (IOUNIT,9112)
 9112    FORMAT(/4X,'**************************************',
     &          /4X,'*** SETUP Finishes UN-successfully ***',
     &          /4X,'**************************************'/)
      ELSE
         WRITE (IOUNIT,9113)
 9113    FORMAT(/1X,'***********************************',
     &          /1X,'*** SETUP Finishes Successfully ***',
     &          /1X,'***********************************'/)
      END IF

      IF (.NOT.FATAL .AND. RUN) THEN
C*       No Fatal Errors in Setup and RUN Option Selected

C*       Read Terrain Data from Raw Data Files and Write Out the 
C*       Data to Direct Access Binary Terrain Data Files and 
C*       Record Index Files 

         WRITE(*,919)
 919     FORMAT(/'+','Initializing Terrain Data...',
     &          /' This may take few a minutes...',/)
         
         IF (TYPDAT .EQ. 'DEM') THEN
C*          Initialize the terrain data for DEM files      ---   CALL INITER_DEM
            CALL INITER_DEM
            
         ELSE IF (TYPDAT .EQ. 'NED') THEN
C*          Initialize the terrain data for NED files      ---   CALL INITER_NED
            CALL INITER_NED
            
         END IF
         
         IF (RUNERR) GO TO 991

C ---    Check for option to estimate source elevations
         IF (NUMSRC.GT.0 .AND. IOSTAT(3).GT.0 .AND. EXTRACT) THEN
            WRITE(*,*)
            
C*          Loop over all sources to extract source elevations
            DO ISRC = 1, NUMSRC

               WRITE(*,929) ISRC, NUMSRC
 929           FORMAT('+','Now Processing Source   ',I7, ' of ',I7)

C*             If the user did not supply source elevations, then determine         
C*             source elevations by a 2-D interpolation between the 4 closest
C*             raw terrain data points.  Loop through DEM files and keep the
C*             first non-missing elevation.                  ---   CALL SRCELV
               IF (EXTRACT .AND. .NOT.RUNERR) THEN
C*                Set logical flag for first DEM file that contains source
                  L_FIRST = .TRUE.
C*                Set logical flag to identify sources in gaps within files
                  GAPSFOUND_IN = .FALSE.
C*                Loop through DEM files
                  DO IDEM = 1, NUMDEM
                  
                     IF (ISIN(ISRC,IDEM) .GT. 0) THEN
C*                      This DEM file contains the source;
C*                      call subroutine SRCELV to calculate elevation
                        CALL SRCELV(ISRC,IDEM)
                        
                        IF (AZS(ISRC) .GT. -9998.99D0) THEN
C*                         Non-missing elevation; if first DEM file for this
C*                         source, and not a gap source then EXIT from DEM loop
                           IF (L_FIRST .AND. ISIN(ISRC,IDEM).EQ.1) THEN
                              EXIT
                           ELSE IF(L_FIRST .AND. 
     &                                       ISIN(ISRC,IDEM).EQ.2) THEN
C*                            Non-missing elevation is from a NAD-shift gap
                              EXIT
                           ELSE IF (ISIN(ISRC,IDEM) .EQ. 3) THEN
C*                            Non-missing elevation is from an internal gap;
C*                            this may be replaced by a subsequent file.
                              L_FIRST = .FALSE.
                              IF (.NOT. GAPSFOUND_IN) THEN
C*                               This is the first internal gap for this source;
C*                               set flag and increment counter
                                 GAPSFOUND_IN = .TRUE.
                                 NSGAP_IN     = NSGAP_IN + 1
                              END IF
                              CYCLE
                           ELSE IF (.NOT.L_FIRST .AND. 
     &                                      (ISIN(ISRC,IDEM).EQ.1
     &                                  .OR. ISIN(ISRC,IDEM).EQ.2)) THEN
C*                            This was not the first DEM file for this
C*                            source; this means that the first file
C*                            returned a missing elevation or was a gap 
C*                            source.
C*                            Issue warning message that elevation came
C*                            from a subsequent DEM file; then EXIT loop
                              NSSUBS = NSSUBS + 1
                              CALL ERRHDL(PATH,MODNAM,'W','425',
     &                                                      SRCID(ISRC))
                              EXIT
                           END IF
                        ELSE
C*                         Elevation was missing (-9999.0); set flag for 
C*                         first DEM file to false, and cycle to next file
C*                         unless ISIN(ISRC,IDEM) = 2, which means source
C*                         was a "gap" source due to NAD shift, assigned 
C*                         to file on the 2nd pass.
                           L_FIRST = .FALSE.
                           IF (ISIN(ISRC,IDEM) .EQ. 1 .OR. 
     &                         ISIN(ISRC,IDEM) .EQ. 3) THEN
C*                            Missing elevation may be replaced by 
C*                            subsquent file     
                              IF (.NOT. GAPSFOUND_IN) THEN
C*                               This is the first internal gap for this source;
C*                               set flag and increment counter
                                 GAPSFOUND_IN = .TRUE.
                                 NSGAP_IN     = NSGAP_IN + 1
                              END IF
                              CYCLE
                           ELSE IF (ISIN(ISRC,IDEM) .EQ. 2) THEN
C*                            Missing elevation for NAD-shift gap source; 
C*                            no other file can replace this case so exit
                              EXIT
                           END IF
                        END IF
                        
                     END IF
                     
                  END DO        ! DEM file loop
                  
                  IF (IDEM .GE. 1 .AND. IDEM .LE. NUMDEM) THEN
C ---                DEM loop was exited
                     IF (ISIN(ISRC,IDEM) .EQ. 1) THEN
                        IF (AZS(ISRC) .LT. -9998.99D0) THEN
C*                         Source elevation is still missing, write warning,
C*                         set flag, and increment counters
                           MISSFOUND = .TRUE.
                           NSMISS    = NSMISS + 1
                           CALL ERRHDL(PATH,MODNAM,'W','430',
     &                                                      SRCID(ISRC))
                        END IF
                     ELSE IF (ISIN(ISRC,IDEM) .EQ. 2) THEN
                        IF (AZS(ISRC) .LT. -9998.99D0) THEN
C*                         Source elevation is still missing, write warning,
C*                         set flag, and increment counters
                           MISSFOUND = .TRUE.
                           NSMISS    = NSMISS + 1
                           CALL ERRHDL(PATH,MODNAM,'W','430',
     &                                                      SRCID(ISRC))
                        ELSE IF (LSrc_FILLED(ISRC,IDEM)) THEN
                           NSFILLED = NSFILLED + 1
                           CALL ERRHDL(PATH,MODNAM,'W','422',
     &                                                      SRCID(ISRC))
                        END IF
                     ELSE IF (ISIN(ISRC,IDEM) .EQ. 3) THEN
                        IF (AZS(ISRC) .LT. -9998.99D0) THEN
C*                         Source elevation is still missing, write warning,
C*                         set flag, and increment counters
                           MISSFOUND = .TRUE.
                           NSMISS    = NSMISS + 1
                           CALL ERRHDL(PATH,MODNAM,'W','430',
     &                                                      SRCID(ISRC))
                        ELSE IF (LSrc_FILLED(ISRC,IDEM)) THEN
                           NSFILLED = NSFILLED + 1
                           CALL ERRHDL(PATH,MODNAM,'W','423',
     &                                                      SRCID(ISRC))
                        END IF
                     END IF
                  ELSE
C*                   Completed DEM loop without exiting; this means that
C*                   the source was only located within an internal gap
                     IF (AZS(ISRC) .LT. -9998.99D0) THEN
C*                      Source elevation is still missing, write warning,
C*                      set flag, and increment counters
                        MISSFOUND = .TRUE.
                        NSMISS    = NSMISS + 1
                        CALL ERRHDL(PATH,MODNAM,'W','430',SRCID(ISRC))
                     ELSE 
                        NSFILLED = NSFILLED + 1
                        CALL ERRHDL(PATH,MODNAM,'W','423',SRCID(ISRC))
                     END IF
                  END IF
                  
               END IF

            END DO              ! Source loop
            
C*          Write Source Elevation Output File               ---   CALL SRCOUT
            CALL SRCOUT
            
         END IF

         WRITE(*,*)
C*       Loop Over All Receptors;
         DO IREC = 1, NUMREC
         
            WRITE(*,909) IREC, NUMREC
 909        FORMAT('+','Now Processing Receptor ',I7, ' of ',I7)
      
C*          If the user did not supply receptor elevations, then determine         
C*          receptor elevations by a 2-D interpolation between the 4 closest
C*          raw terrain data points.  Loop through DEM files and keep the
C*          first non-missing elevation.                     ---   CALL RECELV
            IF (EXTRACT .AND. .NOT.RUNERR) THEN
C*             Set logical flag for first DEM file that contains receptor
               L_FIRST = .TRUE.
C*             Set logical flag to identify receptors in gaps within files
               GAPSFOUND_IN = .FALSE.
C*             Loop through DEM files
               DO IDEM = 1, NUMDEM
               
                  IF (IRIN(IREC,IDEM) .GT. 0) THEN
C*                   This DEM file contains the receptor;
C*                   call subroutine RECELV to calculate elevation
                     CALL RECELV(IREC,IDEM)
                     
                     IF (AZELEV(IREC) .GT. -9998.99D0) THEN
C*                      Non-missing elevation; if first DEM file
C*                      for this receptor, then EXIT from DEM loop
                        IF (L_FIRST .AND. IRIN(IREC,IDEM) .EQ. 1) THEN
                           EXIT
                        ELSE IF(L_FIRST .AND. IRIN(IREC,IDEM).EQ.2) THEN
C*                         Non-missing elevation is from a NAD-shift gap
                           EXIT
                        ELSE IF (IRIN(IREC,IDEM) .EQ. 3) THEN
C*                         Non-missing elevation is from an internal gap;
C*                         this may be replaced by a subsequent file.
                           L_FIRST = .FALSE.
                           IF (.NOT. GAPSFOUND_IN) THEN
C*                            This is the first internal gap for this receptor;
C*                            set flag and increment counter
                              GAPSFOUND_IN = .TRUE.
                              NRGAP_IN     = NRGAP_IN + 1
                           END IF
                           CYCLE
                        ELSE IF (.NOT.L_FIRST .AND. 
     &                                      (IRIN(IREC,IDEM).EQ.1
     &                                  .OR. IRIN(IREC,IDEM).EQ.2)) THEN
C*                         This was not the first DEM file for this
C*                         receptor; this means that the first file
C*                         returned a missing elevation or was a gap 
C*                         receptor.
C*                         Issue warning message that elevation came
C*                         from a subsequent DEM file; then EXIT loop
                           NRSUBS = NRSUBS + 1
                           WRITE(DUMMY,'(I8)') IREC
                           CALL ERRHDL(PATH,MODNAM,'W','405',DUMMY)
                           EXIT
                        END IF
                     ELSE
C*                      Elevation was missing (-9999.0); set flag for 
C*                      first DEM file to false, and cycle to next file
C*                      unless IRIN(IREC,IDEM) = 2, which means receptor
C*                      was a "gap" receptor due to NAD shift, assigned 
C*                      to file on the 2nd pass.
                        L_FIRST = .FALSE.
                        IF (IRIN(IREC,IDEM) .EQ. 1 .OR. 
     &                      IRIN(IREC,IDEM) .EQ. 3) THEN
C*                         Missing elevation may be replaced by 
C*                         subsquent file     
                           IF (.NOT. GAPSFOUND_IN) THEN
C*                            This is the first internal gap for this receptor;
C*                            set flag and increment counter
                              GAPSFOUND_IN = .TRUE.
                              NRGAP_IN     = NRGAP_IN + 1
                           END IF
                           CYCLE
                        ELSE IF (IRIN(IREC,IDEM) .EQ. 2) THEN
C*                         Missing elevation for NAD-shift gap receptor; 
C*                         no other file can replace this case so exit
                           EXIT
                        END IF
                     END IF
                     
                  END IF
                  
               END DO        ! DEM file loop
               
               IF (IDEM .GE. 1 .AND. IDEM .LE. NUMDEM) THEN
C ---             DEM loop was exited
                  IF (IRIN(IREC,IDEM) .EQ. 1) THEN
                     IF (AZELEV(IREC) .LT. -9998.99D0) THEN
C*                      Receptor elevation is still missing, write warning,
C*                      set flag, and increment counters
                        MISSFOUND = .TRUE.
                        NRMISS    = NRMISS + 1
                        WRITE(DUMMY,'(I8)') IREC
                        CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
                     END IF
                  ELSE IF (IRIN(IREC,IDEM) .EQ. 2) THEN
                     IF (AZELEV(IREC) .LT. -9998.99D0) THEN
C*                      Receptor elevation is still missing, write warning,
C*                      set flag, and increment counters
                        MISSFOUND = .TRUE.
                        NRMISS    = NRMISS + 1
                        WRITE(DUMMY,'(I8)') IREC
                        CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
                     ELSE IF (LRec_FILLED(IREC,IDEM)) THEN
                        NRFILLED = NRFILLED + 1
                        WRITE(DUMMY,'(I8)') IREC
                        CALL ERRHDL(PATH,MODNAM,'W','402',DUMMY)
                     END IF
                  ELSE IF (IRIN(IREC,IDEM) .EQ. 3) THEN
                     IF (AZELEV(IREC) .LT. -9998.99D0) THEN
C*                      Receptor elevation is still missing, write warning,
C*                      set flag, and increment counters
                        MISSFOUND = .TRUE.
                        NRMISS    = NRMISS + 1
                        WRITE(DUMMY,'(I8)') IREC
                        CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
                     ELSE IF (LRec_FILLED(IREC,IDEM)) THEN
                        NRFILLED = NRFILLED + 1
                        WRITE(DUMMY,'(I8)') IREC
                        CALL ERRHDL(PATH,MODNAM,'W','403',DUMMY)
                     END IF
                  END IF
               ELSE
C*                Completed DEM loop without exiting; this means that
C*                the receptor was only located within an internal gap
                  IF (AZELEV(IREC) .LT. -9998.99D0) THEN
C*                   Receptor elevation is still missing, write warning,
C*                   set flag, and increment counters
                     MISSFOUND = .TRUE.
                     NRMISS    = NRMISS + 1
                     WRITE(DUMMY,'(I8)') IREC
                     CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
                  ELSE 
                     NRFILLED = NRFILLED + 1
                     WRITE(DUMMY,'(I8)') IREC
                     CALL ERRHDL(PATH,MODNAM,'W','403',DUMMY)
                  END IF
               END IF
               
            END IF

            IF (.NOT.RUNERR) THEN
C*             Determine height scales for each receptor      ---   CALL CALCHC
               CALL CALCHC(IREC)
            END IF

         END DO              ! Receptor loop
         
C*       Check for receptors located within gaps inside files;
C*       reset logical flag if needed
         IF (NRGAP_IN .GT. 0) THEN
            GAPSFOUND_IN = .TRUE.
         END IF

C*       Write the receptor pathway data including receptor
C*       elevations and height scales to the output file      ---   CALL WRITER
         CALL WRITER

991      CONTINUE

      END IF

C*    Call routine to print page header                       ---   CALL HEADER
      CALL HEADER
      
C*    Print Summary of the Input Data                         ---   CALL INPSUM
      CALL INPSUM

C*    Call routine to print page header
      CALL HEADER

      WRITE (IOUNIT,9114)
 9114 FORMAT(/1X,'*** Message Summary For AERMAP Execution ***'/)
C*    Determine Number of Errors/Messages by Message Type     ---   CALL TERRST
      CALL TERRST
C*    Write Summary of Message Stats for Model Execution      ---   CALL SUMTBL
      CALL SUMTBL

      IF (FATAL) THEN
         WRITE (IOUNIT,9115)
         WRITE (*,9115)
 9115    FORMAT(/4X,'***************************************',
     &          /4X,'*** AERMAP Finishes UN-successfully ***',
     &          /4X,'***************************************'/)

      ELSE
         WRITE (IOUNIT,9116)
 9116    FORMAT(/4X,'************************************',
     &          /4X,'*** AERMAP Finishes Successfully ***',
     &          /4X,'************************************'/)
      END IF

C     Delete the direct access files of terrain data.
      DO IDEM = 1,NUMDEM
         L_FileExist = .FALSE.
         INQUIRE(IDRUNT(IDEM),EXIST=L_FileExist)
         IF (L_FileExist) THEN
            OPEN (IDRUNT(IDEM),FILE=DIRFIL(IDEM),
     &            ACCESS='DIRECT',RECL=LREC_DIR)
            OPEN (IDXUNT(IDEM),FILE=IDXFIL(IDEM))

            CLOSE (IDRUNT(IDEM),STATUS='DELETE')
            CLOSE (IDXUNT(IDEM),STATUS='DELETE')
         END IF
      END DO

      CLOSE(IERUNT,STATUS='DELETE')

      STOP
      end 


C*----------------------------------------------------------------------
C*    SUBPROGRAM UNITS FOLLOW..

      SUBROUTINE VARINI
C***********************************************************************
C                 VARINI Module of AERMOD
C
C        PURPOSE: To Initialize Variables for Setup
C
C        PROGRAMMER: Roger Brode, James Paumier, Jayant Hardikar
C
C        DATE:    September 29, 1995
C
C        INPUTS:  None
C
C        OUTPUTS: Initialized Variables
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C --- Variable Initializations
      MODNAM = 'VARINI'

C --- Initialize double precision constants
      PI = 4.0D0*DATAN(1.0D0)
      DTORAD = PI/180.0D0
      RTODEG = 180.0D0/PI

C --- Initialize counters to zero
      IDEM   = 0
      IPNUM  = 0
      IPPNUM = 0

C --- Initialize counters for receptors and sources not assigned to any
C     DEM/NED files on the first pass (potential "gap" receptors/sources)
      NSGAP    = 0
      NRGAP    = 0
C --- Initialize counters for receptors and sources located within gaps
C     inside DEM/NED files
      NSGAP_IN = 0
      NRGAP_IN = 0
C --- Initialize counters for receptors and sources that are assigned to 
C     DEM/NED file on the 2nd pass; these represent "gaps" due to NAD
C     conversions
      NSGAP2   = 0
      NRGAP2   = 0
C --- Initialize counters for receptors and sources located within gaps
C     inside DEM/NED files
      NSGAP3   = 0
      NRGAP3   = 0
C --- Initialize counters for receptors and sources located within gaps
C     inside DEM/NED files, but with elevations from subsequent files
      NSSUBS   = 0
      NRSUBS   = 0
C --- Initialize counters for "gap" receptors and sources that are "filled"
C     based on closest nodes with FILLGAPS option
      NSFILLED = 0
      NRFILLED = 0
C --- Initialize logical variable identifying gap receptors and sources with
C     "filled" elevations [Lrec_FILLED(NREC,NDEM) and LSrc_FILLED(NSRC,NDEM)]
      LRec_FILLED = .FALSE.
      LSrc_FILLED = .FALSE.
C --- Initialize counters for receptors and sources with missing elevations
      NSMISS   = 0
      NRMISS   = 0

C --- Initialize the Logical Control Variables
      FATAL  = .FALSE.
      ISTART = .FALSE.
      IFINIS = .TRUE.
      ERRLST = .FALSE.
      RUN    = .FALSE.
      RECERR = .FALSE.
      ECHO   = .TRUE.
      RUNERR = .FALSE.

      HILLDBG = .FALSE.
      RECDBG  = .FALSE.
      SRCDBG  = .FALSE.

      DEMERR = .FALSE.
      NEDERR = .FALSE.
      
      GAPSFOUND    = .FALSE.
      GAPSFOUND_IN = .FALSE.
      MISSFOUND    = .FALSE.
      FILLGAPS     = .FALSE.

      NADPATH = .FALSE.
      
C*    File-dependent processing controls      
      L_DEMCHECK       = .FALSE.
      L_UserElevUnits  = .FALSE.
      L_TiffDebug      = .FALSE.
      L_NEDSkip        = .FALSE.
      
      DOMFIX = .FALSE.

C*    Initialize default debug output filenames:
C*    Automatically-generated files:
      MAPPARAMS_FILE = 'MAPPARAMS.OUT'
      MAPDET_FILE    = 'MAPDETAIL.OUT'
      DOMDET_FILE    = 'DOMDETAIL.OUT'

C*    User-specified options debug files:
      CALCHC_FILE  = 'CALCHCDET.OUT'
      RECDET_FILE  = 'RECDETAIL.OUT'
      RECNDEM_FILE = 'RECNDEM.OUT'
      RECELV_FILE  = 'RECELV.OUT'
      SRCDET_FILE  = 'SRCDETAIL.OUT'
      SRCNDEM_FILE = 'SRCNDEM.OUT'
      SRCELV_FILE  = 'SRCELV.OUT'

C*    Optional TIFF Debug file
      TiffDbgFil   = 'undefined'      

C     Initialize counter for number of receptors
      NUMREC = 0
C*    Initialize array for assigning receptors to DEM files,
C*    IRIN(NREC,NDEM), to 0
      IRIN = 0
C*    Initialize other receptor counters and flags
      ISTA = .FALSE.
      IEND = .FALSE.
      IRXR = 0
      IRYR = 0
      IRZE = 0
      IRZF = 0
      NEWID  = .TRUE.
      
C*    Initialize EXTRACT to .TRUE., default is for AERMAP to EXTRACT elevations 
      EXTRACT = .TRUE.
      FLGPOL = .FALSE.

C*    Initilize elevation units character strings
      REELEV = 'METERS'
      SOELEV = 'METERS'

C*    Initialize counter for number of sources
      NUMSRC = 0
C*    Initialize array for assigning sources to DEM files,
C*    ISIN(NSRC,NDEM), to 0
      ISIN   = 0

      INPFIL = ' ' 
      OUTFIL = ' ' 
      RECFIL = ' ' 
      SRCFIL = ' '
      
      TYPDAT = ' '

C     Initialize the Outputs
      TITLE1  = ' '
      TITLE2  = ' '
      DOMCARD = ' '
      DOMADJ  = ' '
      ANCHCRD = ' '
      HGTCARD = ' '
      
C     Initialize the Number of Error/Warning/Informational Messages, and
C     The Number of Fatal Errors.
      IERROR = 0
      NFATAL = 0
      NWARN  = 0
      

C***********************************************************************
C*    Initialize Receptor Arrays
C***********************************************************************

      AXR    = 0.0D0
      AYR    = 0.0D0
      AZELEV = 0.0D0
      AZFLAG = 0.0D0
      AZHILL = 0.0D0
      NDXARC = 0
      ARCID  = '        '

C***********************************************************************
C*    Initialize Source Arrays
C***********************************************************************

      AXS  = 0.0D0
      AYS  = 0.0D0
      AZS  = 0.0D0

C***********************************************************************
C*    Initialize Setup Status Arrays
C***********************************************************************

      ICSTAT = 0
      ISSTAT = 0
      IRSTAT = 0
      IOSTAT = 0

      RETURN
      end subroutine

      SUBROUTINE FILOPN
C***********************************************************************
C*                PCCODE Module of the AMS/EPA Regulatory Model - AERMOD
C
C*       PURPOSE: Controls Retrieving Input and Output File Names From
C*                an ASCII text file, FILENAMES.DAT. Opens those files.
C
C        PROGRAMMER: Peter Eckhoff
C
C        MODIFIED:   Remove non-standard option for 
C                    CARRIAGECONTROL='Fortran' to control
C                    page feed in aermod.out file.  ASCII form
C                    feed character is used in subroutine HEADER
C                    to insert page feed instead of using Fortan
C                    carriage control.
C                    R.W. Brode, USEPA/OAQPS/AQMG, February 9, 2009
C
C*       MODIFIED:   Lahey specific code was deleted.  Instead of reading
C                    the input and output filenames from the command line,
C                    the code opens a text file with the filenames listed
C                    in the order that they are read below.  The input 
C                    filename is entered first.  The filename paths need 
C                    to be added if the files are located in a different 
C                    subdirectory(ies) than the executable.
C*       DATE:       January 17, 2001
C
C*       PROGRAMMER: Roger Brode
C
C*       DATE:       September 29, 1995
C
C*       MODIFIED:   Length of command line for Lahey version changed
C*                   from 80 to 120 characters - 4/19/93
C
C*       INPUTS:  Input File Containing the Input/Output Filenames
C
C*       OUTPUTS: Input Runstream File Name
C*                Output Print File Name
C
C*       CALLED FROM:   MAIN
C***********************************************************************
C
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE
      
      CHARACTER*12 MODNAM

C*    Variable Initializations
      MODNAM = 'FILOPN'

C*    OPEN Input Runstream File, Unit INUNIT=5
      DUMMY = 'RUN-STRM'
      OPEN (UNIT=INUNIT,FILE='aermap.inp',
     &      ACTION='READ',ERR=99,STATUS='OLD')

C*    OPEN Print Output File, Unit IOUNIT=7
      DUMMY = 'OUTPUT'
      OPEN (UNIT=IOUNIT,FILE='aermap.out',
     &      ERR=99,STATUS='REPLACE')

C*    Write Out Update to the Screen
      WRITE(*,909)
 909  FORMAT(/'+','Now Processing SETUP Information')

      GO TO 1000

C*    WRITE Error Message:  Error Opening File
 99   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      IF (DUMMY .EQ. 'RUN-STRM') THEN
         WRITE(*,992)
 992     FORMAT('+','Error Opening Input File: AERMAP.INP   Aborting.')
         STOP
      END IF
      IF (DUMMY .EQ. 'OUTPUT') THEN
         WRITE(*,993)
 993     FORMAT('+','Error Opening Output File: AERMAP.OUT   Aborting.')
         STOP
      END IF

 1000 CONTINUE

      RETURN
      end subroutine

      SUBROUTINE SETUP
C***********************************************************************
C*                SETUP Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Controls Processing of Run SETUP Information
C*
C*       PROGRAMMER: Roger Brode, Jayant Hardikar
C*
C*       DATE:    September 29, 1995
C*
C*       MODIFIED:   Removed '1X' from format statements for echoing 
C*                   the runstream inputs to the output file.  This
C*                   was needed when Fortan carriage-control was 
C*                   invoked, which is no longer the case based on 
C*                   modifications to subroutine HEADER. 
C*                   R.W. Brode, USEPA/OAQPS/AQMG, February 9, 2009
C
C*       INPUTS:  Input Runstream File
C*
C*       OUTPUTS: Processing Option Switches
C*                Arrays of Receptor Locations
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER*12 MODNAM

      SAVE
      INTEGER :: I
      LOGICAL NOPATH, NOKEY
      CHARACTER RDFRM*20
      CHARACTER INPFLD*2, PATHWY(5)*2
      INTERFACE
         SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
            CHARACTER (LEN=2), INTENT(IN) :: INPFLD
            CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
            INTEGER, INTENT(IN) :: IPN
            LOGICAL, INTENT(OUT) :: NOPATH
         END SUBROUTINE EXPATH
      END INTERFACE

C*    Variable Initializations
      MODNAM = 'SETUP'
      EOF1 = .FALSE.
      ILINE = 0

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')
      
C*    Initialize DOMAIN flat to false - indicates if user specified domain
c     or if domain should be derived from data file boundaries.
      GOTDOMFLG = .FALSE.
      
C*    LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF1)

C*       Increment the Line Counter
         ILINE = ILINE + 1
         
C        READ Record to Buffers, as A'num' and 'num'A1 where 'num' = ISTRG.
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1
         READ (INUNIT,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

C*       Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C*       Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

C*       Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

         IF (ECHO .AND.
     &            (FIELD(1).EQ.'OU' .AND. FIELD(2).EQ.'FINISHED')) THEN
C*          Echo Last Input Card to Output File (Use Character Substring to
C*          Avoid Echoing ^Z Which May Appear at "End of File" for Some
C*          Editors).  Also, Allow for Shift in the Input Runstream File of
C*          Up to 3 Columns.
            IF (LOCB(1) .EQ. 1) THEN
               WRITE(IOUNIT,9200) RUNST1(1:11)
 9200          FORMAT(A11)
            ELSE IF (LOCB(1) .EQ. 2) THEN
               WRITE(IOUNIT,9210) RUNST1(1:12)
 9210          FORMAT(A12)
            ELSE IF (LOCB(1) .EQ. 3) THEN
               WRITE(IOUNIT,9220) RUNST1(1:13)
 9220          FORMAT(A13)
            ELSE IF (LOCB(1) .EQ. 4) THEN
               WRITE(IOUNIT,9230) RUNST1(1:14)
 9230          FORMAT(A14)
            END IF
         ELSE IF (ECHO) THEN
C*          Echo Full Input Card to Output File
            WRITE(IOUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))
         END IF

C*       If Blank Line, Then CYCLE to Next Card
         IF (BLINE) GO TO 11

C*       Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
            ECHO = .FALSE.
            GO TO 11
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         PATHWY(1) = 'CO'
         PATHWY(2) = 'SO'
         PATHWY(3) = 'RE'
         PATHWY(4) = 'OU'
         PATHWY(5) = '**'
         CALL EXPATH(FIELD(1),PATHWY,5,NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH) THEN
C           WRITE Error Message    ! Invalid Pathway ID
            CALL ERRHDL(PPATH,MODNAM,'E','100',PATH)
            PATH = PPATH
            GO TO 11
         ELSE IF (PATH .EQ. '**') THEN
            GO TO 11
         END IF

C*       Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

C*       When Keyword Is Wrong, Save Keyword and Skip To The Next Record
         IF (NOKEY) THEN
C           WRITE Error Message    ! Invalid Keyword
            CALL ERRHDL(PATH,MODNAM,'E','105',KEYWRD)
            PKEYWD = KEYWRD
            GO TO 11
         END IF

C*       Check for Proper Order of Setup Cards              ---   CALL SETORD
         CALL SETORD
         
C*       Process Input Card Based on Pathway
         IF (PATH .EQ. 'CO') THEN
C*          Process COntrol Pathway Cards                   ---   CALL COCARD
            CALL COCARD
C*          Save certain cards for output file headers
            IF (KEYWRD.EQ.'DOMAINLL' .OR. KEYWRD.EQ.'DOMAINXY' .AND.
     &                                               GOTDOMFLG) THEN
               WRITE(DOMCARD,'(a)') RUNST1(4:MIN(LEN_TRIM(RUNST1),
     &                                                    ILEN_FLD+3))
            ELSE IF (KEYWRD .EQ. 'ANCHORXY') THEN
               WRITE(ANCHCRD,'(a)') RUNST1(4:MIN(LEN_TRIM(RUNST1),
     &                                                    ILEN_FLD+3))
            ELSE IF (KEYWRD .EQ. 'TERRHGTS') THEN
               WRITE(HGTCARD,'(a)') RUNST1(4:MIN(LEN_TRIM(RUNST1),
     &                                                    ILEN_FLD+3))
            END IF

         ELSE IF (PATH .EQ. 'SO') THEN
C*          Process REceptor Pathway Cards                  ---   CALL SOCARD
            CALL SOCARD
            
         ELSE IF (PATH .EQ. 'RE') THEN
C*          Process REceptor Pathway Cards                  ---   CALL RECARD
            CALL RECARD
            
         ELSE IF (PATH .EQ. 'OU') THEN
C*          Process OUtput Pathway Cards                    ---   CALL OUCARD
            CALL OUCARD
         END IF

C*       Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

C*       Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
C*       to Statement 999 in Order to Avoid Reading a ^Z "End of File"
C*       Marker That May Be Present For Some Editors.
         IF (PATH .EQ. 'OU' .AND. KEYWRD .EQ. 'FINISHED') THEN
            GO TO 999
         END IF

         GO TO 11
 999     EOF1 = .TRUE.
 11      CONTINUE
      END DO

C*    Check That All Pathways Were Finished
      IF (ICSTAT(20).NE.1 .OR. IRSTAT(20).NE.1 .OR.
     &                         IOSTAT(20).NE.1) THEN
C*       Runstream File Incomplete, Save I?STAT to IFSTAT and Write Message
         IFSTAT = ICSTAT(20)*1000 + ISSTAT(20)*100 + IRSTAT(20)*10 +
     &            IOSTAT(20)
         WRITE(DUMMY,'(I4.4)') IFSTAT
         CALL ERRHDL(PATH,MODNAM,'E','125',DUMMY)
      END IF

      RETURN
      end subroutine

      SUBROUTINE LWRUPR
C***********************************************************************
C                 LWRUPR Module of ISC2 Model
C
C        PURPOSE: Transfer All Characters From Lower Case To
C                 Upper Case (Using INDEX Intrinsic Function)
C                 Note that the CHAR*ISTRG RUNST1 Variable Includes
C                 the Original Case for Echoing and for Later Use
C                 To Retrieve Filenames.
C
C        PROGRAMMER: Roger Brode, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image 
C                 Number of Characters in String, PARAMETER ISTRG
C
C        OUTPUTS: Input Runstream Card Image (Array) in Uppercase
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      INTEGER I
      CHARACTER*12 MODNAM

      CHARACTER UPCASE*26
      CHARACTER LWCASE*26

C     Variable Initializations
      DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/
      MODNAM = 'LWRUPR'

      DO I = 1, ISTRG
         IF (RUNST(I) .NE. ' ') THEN
            INDCHK = INDEX(LWCASE,RUNST(I))
            IF (INDCHK .NE. 0) THEN
               RUNST(I) = UPCASE(INDCHK:INDCHK)
            END IF
         END IF
      END DO

      RETURN
      end subroutine

      SUBROUTINE DEFINE
C***********************************************************************
C                 DEFINE Module of ISC2 Model
C
C        PURPOSE: Defines Location of Fields on Runstream Input Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C*       Revision History:
C*
C*       MODIFIED: February 9, 2009
C*                
C*                Modified to recognize double quotes (") as 
C*                field delimiters to allow for filenames with
C*                embedded spaces.
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Number of Fields on Card, IFC
C                 Beginning and Ending Columns of Fields, LOCB and LOCE
C
C        CALLED FROM:   SETUP
C***********************************************************************
C

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE
      
      LOGICAL INQUOTE      

      SAVE

      INTEGER I
      CHARACTER*12 MODNAM

C     Variable Initializations
      MODNAM = 'DEFINE'

C     Initialize the Blank Line and In-field Status Indicators
      BLINE = .TRUE.
      INFLD = .FALSE.
      INQUOTE = .FALSE.

      IF (ILINE .EQ. 1) THEN
C        Define the Starting Column for the Input File In Case File Is Shifted.
C        Allow for Shift of Up to 3 Columns
         LOCB(1) = 0
         IF (RUNST(1) .NE. ' ') THEN
            LOCB(1) = 1
         ELSE IF (RUNST(2) .NE. ' ') THEN
            LOCB(1) = 2
         ELSE IF (RUNST(3) .NE. ' ') THEN
            LOCB(1) = 3
         ELSE IF (RUNST(4) .NE. ' ') THEN
            LOCB(1) = 4
         ELSE
            LOCB(1) = 1
         END IF
         LOCE(1) = LOCB(1) + 1
         LOCB(2) = LOCB(1) + 3
         LOCE(2) = LOCB(1) + 10
      END IF

      IFC = 2

C     Loop Through the Pathway and Keyword Fields To Check for Blank Line
      DO I = LOCB(1), LOCE(2)+1
         IF (RUNST(I) .NE. ' ') BLINE = .FALSE.
      END DO

C     Loop through the Data Fields
      DO I = LOCB(1)+12, ISTRG

         IF (.NOT.INFLD .AND. RUNST(I).EQ.'"') THEN
C           Location is the Beginning of a Field using "'s
C           Set Mark of not Blank Line
            BLINE = .FALSE.
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Set Mark of in a Quote Field
            INQUOTE = .TRUE.
C           Increment the Field Counter
            IFC = IFC + 1
C           Record the Location of Beginning of the Field
            LOCB(IFC) = I + 1
         ELSE IF (.NOT.INFLD .AND. RUNST(I).NE.' ') THEN
C           Location is the Beginning of a Field
C           Set Mark of not Blank Line
            BLINE = .FALSE.
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Increment the Field Counter
            IFC = IFC + 1
C           Record the Location of Beginning of the Field
            LOCB(IFC) = I
         ELSE IF (INQUOTE .AND. RUNST(I).EQ.'"') THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
C           Set Mark of Not in a Quote Field
            INQUOTE = .FALSE.
C           Record the Location of Ending of the Field
            LOCE(IFC) = I - 1
         ELSE IF (.NOT.INQUOTE .AND. INFLD .AND. RUNST(I).EQ.' ') THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
C           Record the Location of Ending of the Field
            LOCE(IFC) = I - 1
         END IF

C        Check for End of Input String
C        (Length of ISTRG is Set as a PARAMETER in AERMAP.INC)
         IF (INFLD .AND. I.EQ.ISTRG) THEN
            LOCE(IFC) = ISTRG
         END IF

      END DO

      RETURN
      end subroutine

      SUBROUTINE GETFLD
C***********************************************************************
C                 GETFLD Module of ISC2 Model
C
C        PURPOSE: Gets Contents of Fields on Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Contents of Fields on Card
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE
      INTEGER :: I, J
      CHARACTER WRTFRM*20

C     Variable Initializations
      MODNAM = 'GETFLD'

C     Setup WRITE format for internal write to FIELD
C     based on the ILEN_FLD PARAMETER (set in MAIN1)
      WRITE(WRTFRM,9004) ILEN_FLD
 9004 FORMAT('(',I4.4,'(A1:))')

      DO I = 1, IFC
         IF (LOCE(I)-LOCB(I) .LE. ILEN_FLD-1) THEN
C           Field Satisfies Limit of ILEN_FLD Characters (set in MAIN1)
            WRITE(FIELD(I),WRTFRM) (RUNST(J),J=LOCB(I),LOCE(I))
         ELSE
C           Field Exceeds ILEN_FLD Character Limit
C           Truncate Field at ILEN_FLD Characters
            WRITE(FIELD(I),WRTFRM) (RUNST(J),J=LOCB(I),
     &                                         LOCB(I)+ILEN_FLD-1)
         END IF
      END DO

      RETURN
      end subroutine

      SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
C***********************************************************************
C                 EXPATH Module of ISC2 Model
C
C        PURPOSE: Extracts and Verifies Pathway ID from
C                 Runstream Input Card Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: The Extracted Pathway ID
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE
      INTEGER :: I
      CHARACTER (LEN=2), INTENT(IN) :: INPFLD
      CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
      INTEGER, INTENT(IN) :: IPN
      LOGICAL, INTENT(OUT) :: NOPATH

      CHARACTER*12 MODNAM

C     Variable Initializations
      NOPATH = .TRUE.
      MODNAM = 'EXPATH'

C     Begin The Processing
      IF (INPFLD .NE. '  ') THEN
C        Check the Read-in Pathway
         PATH = INPFLD
         DO I = 1, IPN
C           In Case of Match Set NOPATH to FALSE and Set Path Number, IPNUM
            IF (INPFLD .EQ. PATHWY(I)) THEN
               NOPATH = .FALSE.
               IPNUM = I
C              Exit to END
               GO TO 999
            END IF
         END DO
      ELSE
C        In Case of Blank Field Set Pathway to Previous Pathway
         NOPATH = .FALSE.
         PATH  = PPATH
         IPNUM = IPPNUM
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE EXKEY(INPFLD,NOKEY)
C***********************************************************************
C                 EXKEY Module of ISC2 Model
C
C        PURPOSE: Extracts and Verifies Keyword from
C                 Runstream Input Card Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: The Extracted Keyword
C
C        CALLED FROM:   SETUP
C***********************************************************************
C

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I
      CHARACTER INPFLD*8
      LOGICAL NOKEY

C     Variable Initializations
      NOKEY  = .TRUE.
      MODNAM = 'EXKEY'

C     Begin The Processing
      IF (INPFLD .NE. '        ') THEN
C        Check the Read-in Keyword
         KEYWRD = INPFLD
         DO I = 1, IKN
C           In Case of Match Set NOKEY to FALSE
            IF (INPFLD .EQ. KEYWD(I)) THEN
               NOKEY = .FALSE.
C              Exit to END
               GO TO 999
            END IF
         END DO
      ELSE
C        In Case of Blank Field, Keyword Is Set to Previous Keyword
         NOKEY  = .FALSE.
         KEYWRD = PKEYWD
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE SETORD
C***********************************************************************
C                 SETORD Module of ISC2 Model
C
C        PURPOSE: To Check Run Stream Setup Images for Proper
C                 Order
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Status Settings and Error Messages
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C     Variable Initializations
      MODNAM = 'SETORD'

      IF (KEYWRD .EQ. 'STARTING') THEN
         IF (ISTART .OR. .NOT.IFINIS) THEN
C           WRITE Error Message: Starting Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','115',PATH)
         ELSE IF (IPNUM .NE. IPPNUM+1) THEN
            IF (PATH.NE.'RE' .AND. PPATH.NE.'CO') THEN
C              WRITE Error Message: Pathway Out of Order
               CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
            END IF
         END IF
C        Set Starting Indicator
         ISTART = .TRUE.
C        Set Finished Indicator
         IFINIS = .FALSE.
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
         IF (IFINIS .OR. .NOT.ISTART) THEN
C           WRITE Error Message: Finished Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','115',PATH)
         ELSE IF (ISTART .AND. PATH.NE.PPATH) THEN
C           WRITE Warning Message: Pathway Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
         END IF
C        Reset Starting Indicator
         ISTART = .FALSE.
C        Set Finished Indicator
         IFINIS = .TRUE.
      ELSE IF (.NOT.ISTART .OR. IFINIS) THEN
C        WRITE Error Message: Starting or Finished Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','115',PATH)
      ELSE IF (ISTART .AND. PATH.NE.PPATH) THEN
C        WRITE Warning Message: Pathway Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
      END IF

C     Save Current Path and Path Number as Previous Path and Number
      PPATH = PATH
      IPPNUM = IPNUM

      RETURN
      end subroutine

      SUBROUTINE COCARD
C***********************************************************************
C*                COCARD Module of AERMAP
C*
C*       PURPOSE: To process COntrol Pathway card images
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       INPUTS:  Pathway (CO) and Keyword
C*
C*       OUTPUTS: Processing Option Switches
C*                Option Setup Status Switches
C*
C*       CALLED FROM:   SETUP
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE
      
      INTEGER :: I
      INTEGER :: utm_zone  ! utm_zone function to derive utm_zone
      
      DOUBLE PRECISION  :: tmpLon, XTMP     ! temp variable to reverse sign of longitude

      SAVE

      CHARACTER*12 MODNAM

C*    Variable Initializations
      MODNAM = 'COCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C*       Set Status Switch
         ISTART = .TRUE.
         ICSTAT(1) = ICSTAT(1) + 1
         IF (ICSTAT(1) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF

      ELSE IF (KEYWRD .EQ. 'TITLEONE') THEN
C*       Set Status Switch
         ICSTAT(2) = ICSTAT(2) + 1
         IF (ICSTAT(2) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C*          Process Titles                                  ---   CALL TITLES
            CALL TITLES
         END IF

      ELSE IF (KEYWRD .EQ. 'TITLETWO') THEN
C*       Set Status Switch
         ICSTAT(3) = ICSTAT(3) + 1
         IF (ICSTAT(3) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C*          Process Titles                                  ---   CALL TITLES
            CALL TITLES
         END IF

      ELSE IF (KEYWRD .EQ. 'DATATYPE') THEN
C*       Set Status Switch
         ICSTAT(4) = ICSTAT(4) + 1
         IF (ICSTAT(4) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C*          Process Raw Data Type Keyword                   ---   CALL DATTYP
            CALL DATTYP
         END IF

      ELSE IF (KEYWRD .EQ. 'DATAFILE') THEN
C*       Set Status Switch
         ICSTAT(5) = ICSTAT(5) + 1
C*       Process DEM/User Raw Terrain Data File             ---   CALL DATFIL
         CALL DATFIL

      ELSE IF (KEYWRD .EQ. 'DOMAINXY' .OR. 
     &         KEYWRD .EQ. 'DOMAINLL') THEN
C*       Set Status Switch
         ICSTAT(6) = ICSTAT(6) + 1
         IF (ICSTAT(6) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE         
C*          Process Domain Extent Coordinates               ---   CALL DOMAIN
            CALL DOMAIN
         END IF

      ELSE IF (KEYWRD .EQ. 'ANCHORXY') THEN
C*       Set Status Switch
         ICSTAT(7) = ICSTAT(7) + 1
         IF (ICSTAT(7) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE         
C*          Process Anchor Point Coordinates                ---   CALL ANCHOR
            CALL ANCHOR
         END IF

      ELSE IF (KEYWRD .EQ. 'TERRHGTS') THEN
C*       Set Status Switch
         ICSTAT(9) = ICSTAT(9) + 1
         IF (ICSTAT(9) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C*          Process Terrain Heights Option                  ---   CALL TERRHT
            CALL TERRHT
         END IF

      ELSE IF (KEYWRD .EQ. 'FLAGPOLE') THEN
C        Set Status Switch
         ICSTAT(11) = ICSTAT(11) + 1
         IF (ICSTAT(11) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Flagpole Receptor Height Option         ---   CALL FLAGDF
            CALL FLAGDF
         END IF

      ELSE IF (KEYWRD .EQ. 'RUNORNOT') THEN
C*       Set Status Switch
         ICSTAT(12) = ICSTAT(12) + 1
         IF (ICSTAT(12) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C*          Process Option to Run Model or Not              ---   CALL RUNNOT
            CALL RUNNOT
         END IF

      ELSE IF (KEYWRD .EQ. 'DEBUGOPT') THEN
C        Set Status Switch
         ICSTAT(13) = ICSTAT(13) + 1
         IF (ICSTAT(13) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Error File Option                       ---   CALL DEBOPT
            CALL DEBOPT
         END IF

      ELSE IF (KEYWRD .EQ. 'NADGRIDS') THEN
C        Set Status Switch
         ICSTAT(14) = ICSTAT(14) + 1
         IF (ICSTAT(14) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Error File Option                       ---   CALL NADGRIDS
            CALL NADGRIDS
         END IF

      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C*       Set Status Switch
         IFINIS = .TRUE.
C*       Set Status Switch
         ICSTAT(20) = ICSTAT(20) + 1
         IF (ICSTAT(20) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF

C*       Check for Missing Mandatory Keywords
         IF (ICSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (ICSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','TITLEONE')
         END IF         
         IF (ICSTAT(4) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','DATATYPE')
         END IF
         IF (ICSTAT(5) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','DATAFILE')
         END IF
CRT      remove DOMAINXX/DOMAINLL as mandatory keywords
CRT      when omitted, program will determine domain based on data files
         IF (ICSTAT(7) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','ANCHORXY')
         END IF
         IF (ICSTAT(12) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','RUNORNOT')
         END IF
         
C*       Check for invalid optional field on DATAFILE keyword
         IF (TYPDAT .EQ. 'DEM') THEN
            DO I = 1, NUMDEM
               IF (L_UserElevUnits(I)) 
     &             CALL ERRHDL(PATH,'DATFIL','E','203','DEMCHECK')
            END DO
         ELSE IF (TYPDAT .EQ. 'NED') THEN
            DO I = 1, NUMDEM
               IF (L_DEMCHECK(I)) 
     &             CALL ERRHDL(PATH,'DATFIL','E','203','USERUNIT')
            END DO
         END IF
            
CRT      Check anchorxy utm zone (ZATERR) when domain is specified
CRT      If ZONMIN = ZONMAX --> ZATERR allowed to differ by +/- 1
CRT      If ZONMIN /= ZONMAX --> ZATERR must equal ZONMIN or ZONMAX
CRT         ZONMIN AND ZONMAX may not differ by more than 1
         IF (ICSTAT(6) .NE. 0) THEN
            
            IF (ABS(ZONMIN) .EQ. ABS(ZONMAX) .AND. 
     &          ABS(ZONMIN) .EQ. ABS(ZATERR)) THEN
C----          No apparent conflict between Domain UTM zone and ANCHORXY zone
               GO TO 999
               
            ELSE IF ((ABS(ZONMIN) .EQ. ABS(ZONMAX)) .AND. 
     &               (ABS(ABS(ZONMIN)-ABS(ZATERR)) .LE. 1)) THEN
C----          No apparent conflict between Domain UTM zone and ANCHORXY zone
               GO TO 999
               
            ELSE IF ((ABS( ABS(ZONMAX)-ABS(ZONMIN) ) .EQ. 1) .AND.
     &               (ABS(ZATERR).EQ.ABS(ZONMIN) .OR. 
     &                ABS(ZATERR).EQ.ABS(ZONMAX))) THEN
C----          No apparent conflict between Domain UTM zone and ANCHORXY zone
               GO TO 999

            ELSE IF ((ABS(ZONMIN).EQ. 1 .AND.ABS(ZONMAX).EQ.60 .AND.
     &               (ABS(ZATERR).EQ. 1 .OR. ABS(ZATERR).EQ.60))
     &          .OR. (ABS(ZONMIN).EQ.60 .AND.ABS(ZONMAX).EQ. 1 .AND. 
     &               (ABS(ZATERR).EQ. 1 .OR. ABS(ZATERR).EQ.60))) THEN
C----          UTM zones cross the 180E/180W meridian and ANCHORXY zone is ok     
               GO TO 999
           
            ELSE IF (DOMTYP .EQ. 'LAT') THEN
C----          Check for wrong sign of longitude input for DOMAINLL, assume 
C              lower-right/upper-left corners were input, and apply same tests as above
               tmplon = -1.0D0*XDMIN
               ZONMAX = utm_zone(tmpLon)

               tmpLon = -1.0D0*XDMAX
               ZONMIN = utm_zone(tmpLon)
       
               IF (ABS(ZONMIN) .EQ. ABS(ZONMAX) .AND. 
     &             ABS(ZONMIN) .EQ. ABS(ZATERR)) THEN
C----             No apparent conflict between adjusted Domain UTM zone and ANCHORXY zone
C                 Save adjusted domain and write warning message
                  GO TO 888

               ELSE IF ((ABS(ZONMIN) .EQ. ABS(ZONMAX)) .AND. 
     &                 ((ABS(ABS(ZONMAX)-ABS(ZATERR)) .LE. 1) .AND.
     &                  (ABS(ABS(ZONMIN)-ABS(ZATERR)) .LE. 1))) THEN
C----             No apparent conflict between adjusted Domain UTM zone and ANCHORXY zone
C                 Save adjusted domain and write warning message
                  GO TO 888

               ELSE IF ((ABS( ABS(ZONMAX)-ABS(ZONMIN) ) .EQ. 1) .AND.
     &                  (ABS(ZATERR).EQ.ABS(ZONMIN) .OR. 
     &                   ABS(ZATERR).EQ.ABS(ZONMAX))) THEN
C----             No apparent conflict between adjusted Domain UTM zone and ANCHORXY zone
C                 Save adjusted domain and write warning message
                  GO TO 888
                  
               ELSE IF ((ABS(ZONMIN).EQ. 1 .AND.ABS(ZONMAX).EQ.60 .AND.
     &                  (ABS(ZATERR).EQ. 1 .OR. ABS(ZATERR).EQ.60))
     &             .OR. (ABS(ZONMIN).EQ.60 .AND.ABS(ZONMAX).EQ. 1 .AND. 
     &                  (ABS(ZATERR).EQ. 1 .OR. ABS(ZATERR).EQ.60)))THEN
C----             UTM zones cross the 180E/180W meridian and ANCHORXY zone is ok     
C                 Save adjusted domain and write warning message
                  GO TO 888

               ELSE                  
C----             Apparent conflict between adjusted DOMAIN UTM zone(s) and ANCHORXY zone
                  WRITE(DUMMY,'(I8)') ZATERR
                  CALL ERRHDL(PATH,MODNAM,'E','242',DUMMY)
                  GO TO 999
                  
               END IF
               
888            CONTINUE
               
C----          Assume sign was wrong - issue warning message and adjust sign
               DOMFIX = .TRUE.
               CALL ERRHDL(PATH,MODNAM,'W','244','Neg West')
               XTMP  = XDMIN
               XDMIN = -1.0D0*XDMAX
               XDMAX = -1.0D0*XTMP
               WRITE(IOUNIT,100) XDMIN, YDMIN, XDMAX, YDMAX
100            FORMAT('** Adjusted DOMAINLL inputs:',/
     &                '** DOMAINLL  ',2(F13.5,1X,F12.5,1X))
               WRITE(DOMADJ,200) XDMIN, YDMIN, XDMAX, YDMAX
200            FORMAT('DOMAINLL  ',2(F13.5,1X,F12.5,1X))
                 
            ELSE
C----          Apparent conflict between adjusted DOMAIN UTM zone(s) and ANCHORXY zone
               WRITE(DUMMY,'(I8)') ZATERR
               CALL ERRHDL(PATH,MODNAM,'E','242',DUMMY)
               GO TO 999

            END IF
           
         END IF         

      ELSE
C*       Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE TITLES
C***********************************************************************
C*                TITLES Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: Process Title Information From Runstream Input Image
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       INPUTS:  Input Runstream Image Parameters
C*
C*       OUTPUTS: Title Strings for Model Outputs
C*
C*       CALLED FROM:   COCARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C*    Variable Initializations
      MODNAM = 'TITLES'

      IF (KEYWRD .EQ. 'TITLEONE') THEN
         TITLE1 = RUNST1(LOCE(2)+2:MIN(LEN_TRIM(RUNST1),
     &                                         (LOCE(2)+2+ILEN_FLD-1)))
         IF (TITLE1 .EQ. ' ') THEN
C*          Write Error Message: Missing Parameter Title
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         END IF

      ELSE IF (KEYWRD .EQ. 'TITLETWO') THEN
         TITLE2 = RUNST1(LOCE(2)+2:MIN(LEN_TRIM(RUNST1),
     &                                         (LOCE(2)+2+ILEN_FLD-1)))
         IF (TITLE2 .EQ. ' ') THEN
C*          Write Warning Message
            CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
         END IF

      END IF

 999  RETURN
      end subroutine

      SUBROUTINE DATTYP
C***********************************************************************
C*                DATTYP Module of AERMAP
C*
C*       PURPOSE: Process the Raw Terrain Data Type Keyword
C*                From Runstream Input Image
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       INPUTS:  Input Runstream Image Parameters
C*
C*       OUTPUTS: Raw Terrain Data Type Switch
C*
C*       ERROR HANDLING:   Checks for Invalid Parameters;
C*                         Checks for No Parameters;
C*                         Checks for Too Many Parameters
C*
C*       CALLED FROM:   COCARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C*    Variable Initializations
      MODNAM = 'DATTYP'

C*    Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C*       Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 4) THEN
C*       Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C*    Check for type of elevation data; DEM or NED
C*    Limit check to first 3 characters for DEM data
C*    to allow for DEM1 or DEM7 from older input files.
      IF (FIELD(3)(1:3) .EQ. 'DEM') THEN
         TYPDAT = 'DEM'

      ELSE IF (FIELD(3) .EQ. 'NED') THEN
         TYPDAT = 'NED'

      ELSE
C*       WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',' TYPDAT ')
      END IF
      
      IF (IFC .EQ. 4) THEN
C*       Check for option to provide elevations based on 
C*       closest nodes for "gap" receptors and sources
         IF (FIELD(4) .EQ. 'FILLGAPS') THEN  
            FILLGAPS = .TRUE.
         ELSE
C*          WRITE Error Message     ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','FILLGAPS')
         END IF
      END IF

 999  RETURN
      end subroutine


      SUBROUTINE DATFIL
C***********************************************************************
C*                DATFIL Module of AERMET
C*
C*       PURPOSE: Process DEM/User Raw Terrain Data File
C*                From Runstream Input Image
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*       MODIFIED: December 7, 2006
C*                
C*                Added optional "CHECK" option to activate full
C*                check of DEM file in subroutine DEMCHK;
C*                default operation is to check the first 20480
C*                characters to determine file type (DOS, UNIX,
C*                no CR/LF, Binary).
C*
C*       INPUTS:  Input Runstream Image Parameters
C*
C*       OUTPUTS: Raw Digital Terrain Data Filename and Format
C*
C*       ERROR HANDLING:   Checks for No Parameters;
C*                         Checks for No Format (uses default);
C*                         Checks for Too Many Parameters
C*
C*       CALLED FROM:   COCARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I
      LOGICAL FOUND
      INTEGER IOERRN

C*    Variable Initializations
      MODNAM = 'DATFIL'      

C*    Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C*       Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF ((TYPDAT .EQ. 'DEM' .AND. IFC .GT. 4) .OR.
     &         (TYPDAT .EQ. 'NED' .AND. IFC .GT. 5)) THEN
C*       Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C*    Retrive File Name
      IF ((LOCE(3)-LOCB(3)) .LE. ILEN_FLD-1) THEN
C*       Retrieve Filename as Character Substring to Maintain Original Case
         IDEM = IDEM+1
         DEMFIL(IDEM) = RUNST1(LOCB(3):LOCE(3))
C*       Create filenames for direct access file and index file         
         DIRFIL(IDEM) = DEMFIL(IDEM)(1:LOCE(3)-LOCB(3)-2)//'dir'
         IDXFIL(IDEM) = DEMFIL(IDEM)(1:LOCE(3)-LOCB(3)-2)//'idx'
      ELSE
C*       WRITE Error Message:  Filename Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
         GO TO 999
      END IF

C*    Check for optional CHECK/TIFFDEBUG field
      IF (IFC .GE. 4) THEN
         IF (TYPDAT.EQ.'DEM' .AND. FIELD(4).EQ.'CHECK') THEN
C*          Set DEMCHECK logical flag         
            L_DEMCHECK(IDEM) = .TRUE.
         ELSE IF (TYPDAT .EQ. 'DEM') THEN
C*          Invalid optional field         
            CALL ERRHDL(PATH,MODNAM,'E','203','CHECK')
            GO TO 999
         ELSE IF (TYPDAT.EQ.'NED' .AND. FIELD(4).EQ.'TIFFDEBUG') THEN
C*          Set TiffDebug logical flag and open TiffDebug output file         
            L_TiffDebug(IDEM) = .TRUE.
            ITiffDbg_Unt(IDEM) = 5000 + IDEM
            WRITE(TiffDbgFil(IDEM),100) IDEM
 100        FORMAT('TiffDebugFile_',I5.5,'.dbg')
            IF (IFC .EQ. 5) THEN
C*             Process optional user-specified elevation units            
               IF (FIELD(5).EQ.'FEET') THEN
                  L_UserElevUnits(IDEM)   = .TRUE.
                  Chr_UserElevUnits(IDEM) = FIELD(5)            
                  UserElevUnits(IDEM)     = 1
                  UserDCI(IDEM)           = 1.0D0
               ELSE IF(FIELD(5).EQ.'DECI-FEET' .OR. 
     &                 FIELD(5).EQ.'DECIFEET') THEN
                  L_UserElevUnits(IDEM)   = .TRUE.
                  Chr_UserElevUnits(IDEM) = FIELD(5)            
                  UserElevUnits(IDEM)     = 1
                  UserDCI(IDEM)           = 0.1D0
               ELSE IF(FIELD(5).EQ.'DECA-FEET' .OR. 
     &                 FIELD(5).EQ.'DECAFEET') THEN
                  L_UserElevUnits(IDEM)   = .TRUE.
                  Chr_UserElevUnits(IDEM) = FIELD(5)            
                  UserElevUnits(IDEM)     = 1
                  UserDCI(IDEM)           = 10.0D0
               ELSE IF (FIELD(5).EQ.'METERS') THEN
                  L_UserElevUnits(IDEM)   = .TRUE.
                  Chr_UserElevUnits(IDEM) = FIELD(5)            
                  UserElevUnits(IDEM)     = 2
                  UserDCI(IDEM)           = 1.0D0
               ELSE IF(FIELD(5).EQ.'DECI-METERS' .OR. 
     &                 FIELD(5).EQ.'DECIMETERS') THEN
                  L_UserElevUnits(IDEM)   = .TRUE.
                  Chr_UserElevUnits(IDEM) = FIELD(5)            
                  UserElevUnits(IDEM)     = 2
                  UserDCI(IDEM)           = 0.1D0
               ELSE IF(FIELD(5).EQ.'DECA-METERS' .OR. 
     &                 FIELD(5).EQ.'DECAMETERS') THEN
                  L_UserElevUnits(IDEM)   = .TRUE.
                  Chr_UserElevUnits(IDEM) = FIELD(5)            
                  UserElevUnits(IDEM)     = 2
                  UserDCI(IDEM)           = 10.0D0
               ELSE
C*                Invalid optional field         
                  CALL ERRHDL(PATH,MODNAM,'E','203','ElevUnit')
               END IF
            END IF
         ELSE 
C*          Invalid optional field         
            CALL ERRHDL(PATH,MODNAM,'E','203',' TIFFDBG')
            GO TO 999
         END IF
      ELSE
C*       Set optional flags to false for this file      
         L_DEMCHECK(IDEM)       = .FALSE.
         L_TiffDebug(IDEM)      = .FALSE.
         L_UserElevUnits(IDEM)  = .FALSE.
      END IF
      
C*    Dynamically Allocate File Units (100's).
C*    Set so there is no limit to number of maps that can be processed.
      IDMUNT(IDEM) = 100 + (IDEM-1)*3 + 1
      IDRUNT(IDEM) = 100 + (IDEM-1)*3 + 2
      IDXUNT(IDEM) = 100 + (IDEM-1)*3 + 3

C*    Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO I = 1, IDEM-1

         IF (DEMFIL(IDEM) .EQ. DEMFIL(I) .AND.
     &       IDMUNT(IDEM) .EQ. IDMUNT(I)) THEN
            FOUND = .TRUE.
C           ERROR: File Already in Use
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','505',DUMMY)

         ELSE IF (DEMFIL(IDEM) .EQ. DEMFIL(I) .AND.
     &            IDMUNT(IDEM) .NE. IDMUNT(I)) THEN
C*          Write Error Message: Conflicting Inputs
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','550',DUMMY)
            GO TO 999

         ELSE IF (DEMFIL(IDEM) .NE. DEMFIL(I) .AND.
     &            IDMUNT(IDEM) .EQ. IDMUNT(I)) THEN
C*          Write Error Message: Conflicting Inputs
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','550',DUMMY)
            GO TO 999
         END IF

      END DO

      IF (.NOT. FOUND) THEN
C*       First Time File is Identified - OPEN File
         OPEN(IDMUNT(IDEM),ERR=99,FILE=DEMFIL(IDEM),
     &        ACTION='READ',IOSTAT=IOERRN,STATUS='OLD')
         NUMDEM = NUMDEM + 1
         CLOSE(IDMUNT(IDEM))
      END IF

      GO TO 999

C*    WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
   
      end subroutine

      SUBROUTINE DOMAIN
C***********************************************************************
C*                DOMAIN Module of AERMAP
C*
C*       PURPOSE: Process Domain Extent Values
C*                From Runstream Input Image
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       INPUTS:  Input Runstream Image Parameters
C*
C*       OUTPUTS: Coords of Domain Extents
C*
C*       ERROR HANDLING:   Checks for Invalid Parameters;
C*                         Checks for No Parameters;
C*                         Checks for Too Many Parameters
C*
C*       CALLED FROM:   COCARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER  ::  utm_zone ! utm zone function
      
      INTEGER IDUM
      REAL ARG
      
C*    Variable Initializations
      MODNAM = 'DOMAIN'
     
      IF (KEYWRD .EQ. 'DOMAINXY') THEN
C*       Parse the Domain Min/Max in UTM Coords      
         DOMTYP = 'UTM'

         IF (IFC .EQ. 8) THEN
            CALL STODBL(FIELD(3),ILEN_FLD,XDMIN,IDUM)
C*          Check The Numerical Field
            IF (IDUM.EQ.-1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            CALL STODBL(FIELD(4),ILEN_FLD,YDMIN,IDUM)
C*          Check The Numerical Field
            IF (IDUM.EQ.-1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            CALL STONUM(FIELD(5),ILEN_FLD,ARG,IDUM)
            ZONMIN = NINT(ARG)
C*          Check The Numerical Field
            IF (IDUM.EQ.-1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            CALL STODBL(FIELD(6),ILEN_FLD,XDMAX,IDUM)
C*          Check The Numerical Field
            IF (IDUM.EQ.-1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            CALL STODBL(FIELD(7),ILEN_FLD,YDMAX,IDUM)
C*          Check The Numerical Field
            IF (IDUM.EQ.-1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            CALL STONUM(FIELD(8),ILEN_FLD,ARG,IDUM)
            ZONMAX = NINT(ARG)
C*          Check The Numerical Field
            IF (IDUM.EQ.-1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
         ELSE IF (IFC .GT. 8) THEN
C*          WRITE Error Message           ! Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         ELSE
C*          WRITE Error Message           ! No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         END IF
         
C*       Set GOTDOMFLG logical to .TRUE.
         GOTDOMFLG = .TRUE.      ! flag to indicate user defined domain
         
C*       Check for UTM zone range
         IF (ABS(ABS(ZONMAX)-ABS(ZONMIN)) .GT. 1 .AND.
     &      .NOT.((ABS(ZONMIN).EQ. 1 .AND. ABS(ZONMAX).EQ.60) .OR.
     &            (ABS(ZONMIN).EQ.60 .AND. ABS(ZONMAX).EQ. 1))) THEN
C*          Domain spans more than 2 UTM zones     
            WRITE(DUMMY,'(I3,2X,I3)') ZONMIN, ZONMAX
            CALL ERRHDL(PATH,MODNAM,'E','232',DUMMY)   
         ELSE IF ((ABS(ZONMIN).EQ. 1 .AND. ABS(ZONMAX).EQ.60) .OR.
     &            (ABS(ZONMIN).EQ.60 .AND. ABS(ZONMAX).EQ. 1)) THEN
C*          Domain spans 180E/180W meridian; user-specified domain not supported by AERMAP
C*          Write warning message and set GOTDOMFLG = .FALSE.
            GOTDOMFLG = .FALSE.
            CALL ERRHDL(PATH,MODNAM,'W','246',KEYWRD)   
         END IF

      ELSE IF (KEYWRD .EQ. 'DOMAINLL') THEN
C*       Parse the Domain Min/Max in Lat/Long
         DOMTYP = 'LAT'

         IF (IFC .EQ. 6) THEN
            CALL STODBL(FIELD(3),ILEN_FLD,XDMIN,IDUM)
C*          Check The Numerical Field
            IF (IDUM.EQ.-1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            CALL STODBL(FIELD(4),ILEN_FLD,YDMIN,IDUM)
C*          Check The Numerical Field
            IF (IDUM.EQ.-1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            CALL STODBL(FIELD(5),ILEN_FLD,XDMAX,IDUM)
C*          Check The Numerical Field
            IF (IDUM.EQ.-1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            CALL STODBL(FIELD(6),ILEN_FLD,YDMAX,IDUM)
C*          Check The Numerical Field
            IF (IDUM.EQ.-1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
         ELSE IF (IFC .GT. 6) THEN
C*          WRITE Error Message           ! Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         ELSE
C*          WRITE Error Message           ! No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         END IF
         
C*       Calculate UTM zone range from Lat/Lon range
         ZONMIN = utm_zone(XDMIN)
         IF (YDMIN .LT. 0.0D0) THEN
            ZONMIN = -1*ZONMIN
         END IF

         ZONMAX = utm_zone(XDMAX)
         IF (YDMAX .LT. 0.0D0) THEN
            ZONMAX = -1*ZONMAX
         END IF

C*       Set GOTDOMFLG logical to .TRUE.
         GOTDOMFLG = .TRUE.      ! flag to indicate user defined domain
         
C*       Check for UTM zone range
         IF (ABS(ABS(ZONMAX)-ABS(ZONMIN)) .GT. 1 .AND.
     &      .NOT.((ABS(ZONMIN).EQ. 1 .AND. ABS(ZONMAX).EQ.60) .OR.
     &            (ABS(ZONMIN).EQ.60 .AND. ABS(ZONMAX).EQ. 1))) THEN
C*          Domain spans more than 2 UTM zones     
            WRITE(DUMMY,'(I3,2X,I3)') ZONMIN, ZONMAX
            CALL ERRHDL(PATH,MODNAM,'E','232',DUMMY)   
         ELSE IF ((ABS(ZONMIN).EQ. 1 .AND. ABS(ZONMAX).EQ.60) .OR.
     &            (ABS(ZONMIN).EQ.60 .AND. ABS(ZONMAX).EQ. 1)) THEN
C*          Domain spans 180E/180W meridian; user-specified domain not supported by AERMAP
C*          Write warning message and set GOTDOMFLG = .FALSE.
            GOTDOMFLG = .FALSE.
            CALL ERRHDL(PATH,MODNAM,'W','246',KEYWRD)   
         END IF

      END IF

 999  RETURN
      end subroutine


      SUBROUTINE STONUM(STRVAR,LENGTH,FNUM,IMUTI)
C***********************************************************************
C                 STONUM Module of ISC2 Model
C
C        PURPOSE: Gets Number From A String Variable
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input String Variable
C                 Length of Character String
C
C        OUTPUTS: Numbers
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************
C
C     Variable Declarations

      IMPLICIT NONE

      SAVE

      CHARACTER STRVAR*(*), CHK, NUMS*10, MODNAM*12
      INTEGER LENGTH, IMUTI, I
      REAL FNUM, CNUM, FDEC, FDC1, HEAD
      LOGICAL MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

C     Variable Initialization
      MODNAM = 'STONUM'
      NUMS = '0123456789'
      I = 1
      MEND  = .FALSE.
      IN    = .FALSE.
      NMARK = .FALSE.
      PMARK = .FALSE.
      DMARK = .FALSE.
      MMARK = .FALSE.
      EMARK = .FALSE.
      CNUM  = 0.0
      IMUTI = 1
      FDEC  = 1.0

C     Beginning the Processing
      DO WHILE (.NOT.MEND .AND. I.LE.LENGTH)
         CHK = STRVAR(I:I)
         IF (CHK .NE. ' ') THEN
            IN = .TRUE.
            IF (CHK.GE.'0' .AND. CHK.LE.'9') THEN
C              CHK is a Number, Assign a Value
               IF (.NOT. DMARK) THEN
                  CNUM = CNUM*10.+REAL(INDEX(NUMS,CHK)-1)
               ELSE
                  FDEC = FDEC/10.
                  FDC1 = FDEC*REAL(INDEX(NUMS,CHK)-1)
                  CNUM = CNUM+FDC1
               END IF
            ELSE
C              Handle The E-Type Real Number
               IF (.NOT.EMARK .AND. CHK.EQ.'E') THEN
                  EMARK = .TRUE.
                  IF (.NOT.NMARK) THEN
                     HEAD = CNUM
                  ELSE
                     HEAD = -CNUM
                  END IF
                  DMARK = .FALSE.
                  NMARK = .FALSE.
                  CNUM = 0.0
               ELSE IF (.NOT.PMARK .AND. CHK.EQ.'+') THEN
C                 Set Positive Indicator
                  PMARK = .TRUE.
               ELSE IF (.NOT.NMARK .AND. CHK.EQ.'-') THEN
C                 Set Negative Indicator
                  NMARK = .TRUE.
               ELSE IF (.NOT.DMARK .AND. CHK.EQ.'.') THEN
C                 Set Decimal Indicator
                  DMARK = .TRUE.
               ELSE IF (.NOT.MMARK .AND. CHK.EQ.'*' .AND.
     &                  .NOT.NMARK) THEN
C                 Set Repeat Number
                  MMARK = .TRUE.
                  IMUTI = NINT(CNUM)
                  CNUM = 0.0
               ELSE
C                 Error Occurs, Set Switch and Exit Out Of The Subroutine
                  GO TO 9999
               END IF
            END IF
         ELSE IF (IN .AND. CHK.EQ.' ') THEN
            MEND = .TRUE.
         END IF
         I = I + 1
      END DO

      FNUM = CNUM

C     In Case Of Negative Field, Value Set to Negative
      IF (NMARK) THEN
         FNUM = -FNUM
      END IF

C     In Case of E-Format, Check for Exponents Out of Range
      IF (EMARK .AND. ABS(FNUM) .LE. 30.) THEN
         FNUM = HEAD*10**(FNUM)
      ELSE IF (EMARK .AND. ABS(FNUM) .GT. 30.) THEN
         IF (FNUM .LT. 0.0) THEN
            FNUM = 0.0
         ELSE IF (FNUM .GT. 0.0) THEN
            FNUM = HEAD * 1.0E30
         END IF
         GO TO 9999
      END IF

      GO TO 1000

C     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
C     Error in Calling Routine)
 9999 IMUTI = -1

 1000 RETURN
      end subroutine

      SUBROUTINE STODBL(STRVAR,LEN,DNUM,IMUTI)
C***********************************************************************
C                 Subroutine STODBL
C
C        PURPOSE: Gets Double Precision of Real Number
C                 From A Stream Variable
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Change Exponent Limit for Out-of-range
C                    Inputs - 9/29/92
C
C        INPUTS:  Input String Variable
C                 Length of Character String
C
C        OUTPUTS: Double Precision Real Numbers
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************
C
C     Variable Declarations

      IMPLICIT NONE

      SAVE

      CHARACTER STRVAR*(*), CHK, MODNAM*12, NUMS*10
      INTEGER I, IMUTI, LEN
      DOUBLE PRECISION DNUM, CNUM, FDEC, FDC1, HEAD
      LOGICAL MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

C     Variable Initialization
      MODNAM = 'STODBL'
      NUMS = '0123456789'
      I = 1
      MEND  = .FALSE.
      IN    = .FALSE.
      NMARK = .FALSE.
      PMARK = .FALSE.
      DMARK = .FALSE.
      MMARK = .FALSE.
      EMARK = .FALSE.
      CNUM  = 0.0D0
      IMUTI = 1
      FDEC  = 1.0D0

C     Beginning the Processing
      DO WHILE (.NOT.MEND .AND. I.LE.LEN)
         CHK = STRVAR(I:I)
         IF (CHK .NE. ' ') THEN
            IN = .TRUE.
            IF (CHK.GE.'0' .AND. CHK.LE.'9') THEN
C              CHK is a Number, Assign a Value
               IF (.NOT. DMARK) THEN
                  CNUM = CNUM*10.0D0+DBLE(INDEX(NUMS,CHK)-1)
               ELSE
                  FDEC = FDEC/10.0D0
                  FDC1 = FDEC*DBLE(INDEX(NUMS,CHK)-1)
                  CNUM = CNUM+FDC1
               END IF
            ELSE
C              Handle The E-Type (or D-Type) Real Number
               IF (.NOT.EMARK .AND. CHK.EQ.'E' .OR.
     &             .NOT.EMARK .AND. CHK.EQ.'D') THEN
                  EMARK = .TRUE.
                  IF (.NOT.NMARK) THEN
                     HEAD = CNUM
                  ELSE
                     HEAD = -CNUM
                  END IF
                  DMARK = .FALSE.
                  NMARK = .FALSE.
                  CNUM = 0.0D0
               ELSE IF (.NOT.PMARK .AND. CHK.EQ.'+') THEN
C                 Set Positive Indicator
                  PMARK = .TRUE.
               ELSE IF (.NOT.NMARK .AND. CHK.EQ.'-') THEN
C                 Set Negative Indicator
                  NMARK = .TRUE.
               ELSE IF (.NOT.DMARK .AND. CHK.EQ.'.') THEN
C                 Set Decimal Indicator
                  DMARK = .TRUE.
               ELSE IF (.NOT.MMARK .AND. CHK.EQ.'*' .AND.
     &            .NOT.NMARK) THEN
C                 Set Repeat Indicator
                  MMARK = .TRUE.
                  IMUTI = IDNINT(CNUM)
                  CNUM = 0.0D0
               ELSE
C                 Error Occurs, Set Switch and Exit Out Of The Subroutine
                  GO TO 9999
               END IF
            END IF
         ELSE IF (IN .AND. CHK.EQ.' ') THEN
            MEND = .TRUE.
         END IF
         I = I + 1
      END DO

      DNUM = CNUM

C     In Case Of Negative Field, Value set to Negative
      IF (NMARK) THEN
         DNUM = -DNUM
      END IF

C     In Case of *E* Format, Check for Exponents Out of Range
      IF (EMARK .AND. DABS(DNUM) .LE. 30.0D0) THEN
         DNUM = HEAD*10.0D0**(DNUM)
      ELSE IF (EMARK .AND. DABS(DNUM) .GT. 30.0D0) THEN
         IF (DNUM .LT. 0.0D0) THEN
            DNUM = 0.0D0
         ELSE IF (DNUM .GT. 0.0D0) THEN
            DNUM = HEAD * 1.0D+30
         END IF
         GO TO 9999
      END IF

      GO TO 1000

C     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
C     Error in Calling Routine)
 9999 IMUTI = -1

 1000 RETURN
      end subroutine

      SUBROUTINE ANCHOR
C***********************************************************************
C*                ANCHOR Module of AERMAP
C*
C*       PURPOSE: Process Anchoring of a Raw Terrain Data Point to 
C*                User Coords From Runstream Input Image
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       INPUTS:  Input Runstream Image Parameters
C*
C*       OUTPUTS: Coords of Domain Extents
C*
C*       ERROR HANDLING:   Checks for Invalid Parameters;
C*                         Checks for No Parameters;
C*                         Checks for Too Many Parameters
C*
C*       CALLED FROM:   COCARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER IDUM
      REAL ARG

C*    Variable Initializations
      MODNAM = 'ANCHOR'

      IF (IFC .EQ. 8) THEN
         CALL STODBL(FIELD(3),ILEN_FLD,XAUSER,IDUM)
C*       Check The Numerical Field
         IF (IDUM.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         
         CALL STODBL(FIELD(4),ILEN_FLD,YAUSER,IDUM)
C*       Check The Numerical Field
         IF (IDUM.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         
         CALL STODBL(FIELD(5),ILEN_FLD,XATERR,IDUM)
C*       Check The Numerical Field
         IF (IDUM.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         
         CALL STODBL(FIELD(6),ILEN_FLD,YATERR,IDUM)
C*       Check The Numerical Field
         IF (IDUM.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         
         CALL STONUM(FIELD(7),ILEN_FLD,ARG,IDUM)
         ZATERR = NINT(ARG)
C*       Check The Numerical Field
         IF (IDUM.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         
C        Read the NAD of the ANCHOR point
         CALL STONUM(FIELD(8),ILEN_FLD,ARG,IDUM)
         NADA = NINT(ARG)
C*       Check The Numerical Field
         IF (IDUM.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
           end if

      ELSE IF (IFC .GT. 9) THEN
C*       WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C*       WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE TERRHT
C***********************************************************************
C*                TERRHT Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: Process Terrain Height Option From Runstream Input Image
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       Revision History:
C*
C*       MODIFIED: February 9, 2009
C*                
C*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*                Modified to use new EXTRACT logical variable to specify
C*                that terrain elevations will be EXTRACTed from DEM data,
C*                in place of ELEV logical variable used to specify that
C*                terrain elevations will be PROVIDED by the user.
C*
C*       INPUTS:  Input Runstream Image Parameters
C*
C*       OUTPUTS: Terrain Height Option Logical Switch
C*
C*       ERROR HANDLING:   Checks for Invalid Parameters;
C*                         Checks for No Parameters;
C*                         Checks for Too Many Parameters
C*
C*       CALLED FROM:   COCARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C*    Variable Initializations
      MODNAM = 'TERRHT'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'EXTRACT') THEN
C*          Elevations extracted from DEM data         
            EXTRACT = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'PROVIDED') THEN
C*          Elevations will be provided by user         
            EXTRACT = .FALSE.
         ELSE
C*          WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C*       WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C*       WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE FLAGDF
C***********************************************************************
C                 FLAGDF Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Default Flagpole Receptor Height Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    September 29, 1995
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Default Flagpole Receptor Heights
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      DOUBLE PRECISION :: ZFLG
      INTEGER I, IDUM

C     Variable Initializations
      MODNAM = 'FLAGDF'
      FLGPOL = .TRUE.

      IF (IFC .EQ. 3) THEN
         CALL STODBL(FIELD(3),ILEN_FLD,ZFLG,IDUM)
         IF (IDUM .EQ. -1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         IF (ZFLG .GE. 0.0D0 .AND. IDUM .EQ. 1) THEN
            DO I = 1, NREC
               AZFLAG(I) = ZFLG
            END DO
         ELSE IF (ZFLG .LT. 0.0D0) THEN
C            WRITE Error Message: Invalid Data. Positive Value Turns Negative
             CALL ERRHDL(PATH,MODNAM,'E','209','ZFLAG')
         ELSE IF (IDUM .NE. 1) THEN
C            WRITE Error Message: Field Number Not Meet Requirement
             CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
C            WRITE Error Message: Invalid Parameter
             CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'W','205','ZFLAG=0.')
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE RUNNOT
C***********************************************************************
C*                RUNNOT Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: Process Option To RUN Or NOT From Runstream Input Image
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       INPUTS:  Input Runstream Image Parameters
C*
C*       OUTPUTS: Model RUN Logical Switch
C*
C*       ERROR HANDLING:   Checks for Invalid Parameters;
C*                         Checks for No Parameters;
C*                         Checks for Too Many Parameters
C*
C*       CALLED FROM:   COCARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C*    Variable Initializations
      MODNAM = 'RUNNOT'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'RUN') THEN
            RUN = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'NOT') THEN
            RUN = .FALSE.
         ELSE
C*          WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C*       WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C*       WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE DEBOPT
C***********************************************************************
C                 DEBOPT Module of AERMAP
C
C        PURPOSE: Process Debug Output File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    December 7, 2006
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Debug File Logical Switches
C
C        ERROR HANDLING:   Checks for Too Few Parameters
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12, KOPT*8
      INTEGER I

      SAVE

C     Variable Initializations
      MODNAM = 'DEBOPT'

C     Check for Too Few or Too Many Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     First Check for Presence of Debug Switches
      DO I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'HILL') THEN
            HILLDBG = .TRUE.
         ELSE IF (KOPT .EQ. 'RECEPTOR') THEN
            RECDBG = .TRUE.
         ELSE IF (KOPT .EQ. 'SOURCE') THEN
            SRCDBG = .TRUE.
         ELSE IF (KOPT .EQ. 'ALL') THEN
            HILLDBG = .TRUE.
            RECDBG  = .TRUE.
            SRCDBG  = .TRUE.
         ELSE
C*          WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      END DO

 999  RETURN
      END

      SUBROUTINE NADGRIDS
C***********************************************************************
C                 NADGRIDS Module of AERMAP
C
C        PURPOSE: Process NADGRIDS option to specify
C                 path location for NADCON grid files,
C                 CONUS.LAS & CONUS.LOS, etc.
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    February 9, 2009
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Debug File Logical Switches
C
C        ERROR HANDLING:   Checks for Too Few Parameters
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12, KOPT*8
      INTEGER I

      SAVE

C     Variable Initializations
      MODNAM = 'NADGRIDS'

C     Check for Too Few or Too Many Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C*    Retrive NADGRIDS pathway
      IF ((LOCE(3)-LOCB(3)) .LE. ILEN_FLD-1) THEN
C*       Retrieve Pathway as Character Substring to Maintain Original Case
         NADGRID_PATH = RUNST1(LOCB(3):LOCE(3))
C*       Set logical flag for NADGRIDS path
         NADPATH = .TRUE.
      ELSE
C*       WRITE Error Message:  Pathway Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
         GO TO 999
      END IF

 999  RETURN
      END

      SUBROUTINE SOCARD
C***********************************************************************
C             SOCARD Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To process SOurce Pathway card images
C
C        PROGRAMMER:  Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Pathway (SO) and Keyword
C
C        OUTPUTS: Source Arrays
C                 Sourcer Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM
      INTEGER ISRC

C     Variable Initializations
      MODNAM = 'SOCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Initialize Counters and Set Status Switch
         ISRC = 0
         NUMSRC = 0
         ISSTAT(1) = ISSTAT(1) + 1
         IF (ISSTAT(1) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF
      ELSE IF (KEYWRD .EQ. 'LOCATION') THEN
C        Set Status Switch
         ISSTAT(2) = ISSTAT(2) + 1
C        Process Source Location                            ---   CALL SOLOCA
         CALL SOLOCA
      ELSE IF (KEYWRD .EQ. 'SRCPARAM') THEN
C        Write Warning Message: SRCPARAM Keyword ignored in AERMAP
         CALL ERRHDL(PATH,MODNAM,'W','112','ignored ')
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         ISSTAT(15) = ISSTAT(15) + 1
         IF (ISSTAT(15) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (NUMSRC .GT. 0) THEN
C           Write Error Message: ELEVUNIT must be first card after STARTING
            CALL ERRHDL(PATH,MODNAM,'E','152',PATH)
         ELSE
C           Process Elevation Units for Source Elevations   ---   CALL SOELUN
            CALL SOELUN
         END IF
      ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
C        Set Status Switch
         ISSTAT(18) = ISSTAT(18) + 1
C        Save ILINE as ISAVE
         ILSAVE = ILINE
C        Process the Included Receptor File                 ---   CALL INCLUD
         CALL INCLUD
C        Retrieve ILINE From ISAVE         
         ILINE = ILSAVE
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         ISSTAT(20) = ISSTAT(20) + 1
         IF (ISSTAT(20) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF

C        Check for Missing Mandatory Keywords
         IF (ISSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (ISSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','LOCATION')
         END IF

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE SOLOCA
C***********************************************************************
C                 SOLOCA Module of ISC2 Model
C
C        PURPOSE: Processes Source Location Card
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To increase maximum length of source IDs from
C                    8 to 12 characters.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011
C
C        MODIFIED:   To include full range of source types supported
C                    by AERMOD.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 03/03/08
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Type and Location
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      CHARACTER SOID*12
      LOGICAL FOUND
      INTEGER  INDEXS, IMIT, ISRC, ISET

C     Variable Initializations
      FOUND = .FALSE.
      MODNAM = 'SOLOCA'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 6) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Read In The Data Fields and Assign to Arrays
C     Check for Previous Occurrence of This SRCID
C*    First check for length of SRCID field (<=12)
      IF ((LOCE(3)-LOCB(3)) .LE. 11) THEN
C*       Retrieve Source ID Character Substring
         SOID = FIELD(3)
      ELSE
C*       WRITE Error Message:  Source ID Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','206',FIELD(3)(1:12))
         GO TO 999
      END IF

C*    Check whether SRCID has already been defined
      CALL SINDEX(SRCID,NSRC,SOID,INDEXS,FOUND)

      IF (.NOT. FOUND) THEN
         ISRC = ISRC + 1
         IF (ISRC .LE. NSRC) THEN
            SRCID(ISRC)  = FIELD(3)
            SRCTYP(ISRC) = FIELD(4)
            IF (SRCTYP(ISRC) .EQ. 'OPENPIT'  .OR.
     &          SRCTYP(ISRC) .EQ. 'OPEN_PIT' .OR.
     &          SRCTYP(ISRC) .EQ. 'OPEN-PIT') THEN
     
                   SRCTYP(ISRC) = 'OPENPIT'
            END IF

            IF (SRCTYP(ISRC).EQ.'POINT' .OR.
     &          SRCTYP(ISRC).EQ.'POINTCAP' .OR.
     &          SRCTYP(ISRC).EQ.'POINTHOR' .OR.
     &          SRCTYP(ISRC).EQ.'VOLUME' .OR.
     &          SRCTYP(ISRC).EQ.'AREA' .OR.
     &          SRCTYP(ISRC).EQ.'AREAPOLY' .OR.
     &          SRCTYP(ISRC).EQ.'AREACIRC' .OR.
     &          SRCTYP(ISRC).EQ.'OPENPIT') THEN

               CALL STODBL(FIELD(5), 40, AXS(ISRC), IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               CALL STODBL(FIELD(6), 40, AYS(ISRC), IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               IF (IFC .EQ. 7) THEN
C                 Retrieve Source Elevation From Inputs
                  CALL STODBL(FIELD(7), 40, AZS(ISRC), IMIT)
C                 Check The Numerical Field
                  IF (IMIT .NE. 1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  END IF
C                 Check for units conversion from feet to meters
                  IF (SOELEV .EQ. 'FEET') THEN
                     AZS(ISRC) = AZS(ISRC) * 0.3048D0
                  END IF
               ELSE
C                 No Source Elevation Field - Default to 0.0
                  AZS(ISRC) = 0.0D0
                  IF (.NOT.EXTRACT) THEN
C                    Write Warning Message for No Source Elevation with PROVIDED option
                     CALL ERRHDL(PATH,MODNAM,'W','205','ZS = 0.0')
                  END IF
               END IF
            ELSE
C              Error Message: Invalid Source Type
               CALL ERRHDL(PATH,MODNAM,'E','203',' SRCTYP ')
               GO TO 999
            END IF
            ISET = ISRC
            NUMSRC = NUMSRC + 1
         ELSE
C           WRITE Error Message    ! Number of Sources Exceeds NSRC Parameter
            WRITE(DUMMY,'(I8)') NSRC
            CALL ERRHDL(PATH,MODNAM,'E','217',DUMMY)
            GO TO 999
         END IF
      ELSE
C        WRITE Error Message    ! Source Location Has Already Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','377',SOID)
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE SINDEX(ARRIN,IDIM,ELEM,INDEXS,FOUND)
C***********************************************************************
C                 SINDEX Module of ISC2 Model
C
C        PURPOSE: Search The Index of An Input Array Element
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Character Element
C
C        OUTPUTS: Index Of This Element in An Array
C
C        CALLED FROM:  (This Is An Utility Programm)
C***********************************************************************
C
C     Variable Declarations

      IMPLICIT NONE

      CHARACTER (LEN=*) :: ARRIN(IDIM), ELEM
      CHARACTER MODNAM*12
      INTEGER IDIM, INDEXS, I
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'SINDEX'
      FOUND = .FALSE.
      I = 1
      INDEXS = 0

      DO WHILE (.NOT.FOUND .AND. I.LE.IDIM)
         IF (ELEM .EQ. ARRIN(I)) THEN
            FOUND = .TRUE.
            INDEXS = I
         END IF
         I = I + 1
      END DO

      RETURN
      end subroutine

      SUBROUTINE SOELUN
C***********************************************************************
C                 SOELUN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Elevation Units Option for Sources
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 22, 1994
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Elevation Units Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C     Variable Initializations
      MODNAM = 'SOELUN'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            SOELEV = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            SOELEV = 'FEET'
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',' SO_ELEV')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200','ElevUnit')
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE RECARD
C***********************************************************************
C*                RECARD Module of ISC2 Model
C
C*       PURPOSE: To process REceptor Pathway card images
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       INPUTS:  Pathway (RE) and Keyword
C
C*       OUTPUTS: Receptor Arrays
C*                Receptor Setup Status Switches
C
C*       CALLED FROM:   SETUP
C***********************************************************************
        
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM
      INTEGER IREC

C*    Variable Initializations
      MODNAM = 'RECARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C*       Initialize Counters and Set Status Switch
         IREC = 0
         INNET = 0
         NUMREC = 0
         NUMARC = 0
         IRXR = 0
         IRYR = 0
         IRZE = 0
         IRHZ = 0
         IRZF = 0
         PXSOID = ' '
         PESOID = ' '
         ISTA = .FALSE.
         IRSTAT(1) = IRSTAT(1) + 1
         IF (IRSTAT(1) .NE. 1) THEN
C*          Error Message: Repeat Starting In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
C*       Flush the Working Arrays (1:NREC)
         ZETMP1 = 0.0D0
         ZETMP2 = 0.0D0
         ZFTMP1 = 0.0D0
         ZFTMP2 = 0.0D0
      ELSE IF (KEYWRD .EQ. 'GRIDCART') THEN
C*       Set Status Switch
         IRSTAT(2) = IRSTAT(2) + 1
C*       Process Cartesian Grid Receptor Network            ---   CALL RECART
         CALL RECART
      ELSE IF (KEYWRD .EQ. 'GRIDPOLR') THEN
C*       Set Status Switch
         IRSTAT(3) = IRSTAT(3) + 1
C*       Process Polar Receptor Network                     ---   CALL REPOLR
         CALL REPOLR
      ELSE IF (KEYWRD .EQ. 'DISCCART') THEN
C*       Set Status Switch
         IRSTAT(4) = IRSTAT(4) + 1
C*       Process Discrete Cartesian Receptor Locations      ---   CALL DISCAR
         CALL DISCAR
      ELSE IF (KEYWRD .EQ. 'DISCPOLR') THEN
C*       Set Status Switch
         IRSTAT(5) = IRSTAT(5) + 1
C*       Process Discrete Polar Receptor Locations          ---   CALL DISPOL
         CALL DISPOL
      ELSE IF (KEYWRD .EQ. 'EVALCART') THEN
C        Set Status Switch
         IRSTAT(8) = IRSTAT(8) + 1
C        Process Discrete Cartesian Receptor Locations      ---   CALL EVCART
         CALL EVCART
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C*       Set Status Switch
         IRSTAT(9) = IRSTAT(9) + 1
         IF (IRSTAT(9) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (IRSTAT(2) .GT. 0 .OR. IRSTAT(3) .GT. 0 .OR.
     &            IRSTAT(4) .GT. 0 .OR. IRSTAT(5) .GT. 0 .OR.
     &                                  IRSTAT(8) .GT. 0) THEN
C*          Write Error Message: ELEVUNIT must be first card after STARTING
            CALL ERRHDL(PATH,MODNAM,'E','152',PATH)
         ELSE
C*          Process Elevation Units for Source Elevations   ---   CALL REELUN
            CALL REELUN
         END IF         
      ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
C        Set Status Switch
         IRSTAT(12) = IRSTAT(12) + 1
C        Save ILINE as ISAVE
         ILSAVE = ILINE
C        Process the Included Receptor File                 ---   CALL INCLUD
         CALL INCLUD
C        Retrieve ILINE From ISAVE         
         ILINE = ILSAVE
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C*       Set Status Switch
         IRSTAT(20) = IRSTAT(20) + 1
         IF (IRSTAT(20) .NE. 1) THEN
C*          Error Message: Repeat Finished In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF
C*       Write Out The Error Message: Mandatory Keyword Missing
         IF (IRSTAT(1) .EQ. 0)THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF

         IF (IRSTAT(2).EQ.0 .AND. IRSTAT(3).EQ.0 .AND.
     &       IRSTAT(4).EQ.0 .AND. IRSTAT(5).EQ.0 .AND.
     &                            IRSTAT(8).EQ.0) THEN
C*          WRITE Error Message:  No Receptor Keywords
            CALL ERRHDL(PATH,MODNAM,'E','185','NUMREC=0')
         END IF

         IF (ISTA) THEN
C*          WRITE Error Message:  Missing END Keyword for a Grid Network
            CALL ERRHDL(PATH,MODNAM,'E','175',PNETID)
         END IF

C*       Set Total Number of Receptors for This Run, NUMREC
         NUMREC = IRXR
         IF (NUMREC .EQ. 0) THEN
C*          WRITE Error Message:  No Receptors Defined
            CALL ERRHDL(PATH,MODNAM,'E','227','NUMREC=0')
         END IF

C*       Reinitialize ZELEV ZHILL and ZFLAG arrays if needed
C*       ZELEV and ZHILL Will Be Reset to Average Stack Base Elevation
         IF (.NOT. FLGPOL) THEN
            DO IREC = 1, NUMREC
               AZFLAG(IREC) = 0.0D0
            END DO
         END IF

      ELSE
C*       Write Error Message:  Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE INCLUD
C***********************************************************************
C*                INCLUD Module of ISCST3 Model
C*
C*       PURPOSE: To read an external receptor/source file using the
C*                INCLUDED keyword.
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 30, 1995
C*
C        MODIFIED:   To remove reference to obsolete TG pathway inherited
C                    from ISCST3 code.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 07/26/2007
C
C*                   
C*       INPUTS: 
C*
C*       OUTPUTS:
C*               
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
        
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER*12 MODNAM

      SAVE
      INTEGER :: I, ILREAL
      LOGICAL NOPATH, NOKEY      
      CHARACTER RDFRM*20
      CHARACTER INPFLD*2, PATHWY(5)*2
      INTERFACE
         SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
            CHARACTER (LEN=2), INTENT(IN) :: INPFLD
            CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
            INTEGER, INTENT(IN) :: IPN
            LOGICAL, INTENT(OUT) :: NOPATH
         END SUBROUTINE EXPATH
      END INTERFACE

C*    Variable Initializations
      MODNAM = 'INCLUD'
      EOF = .FALSE.
      ILINE = 1

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')
      

      IF (IFC .EQ. 3) THEN
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. ILEN_FLD-1) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            INCFIL = RUNST1(LOCB(3):LOCE(3))
            OPEN (INCUNT,FILE=INCFIL,ACTION='READ',STATUS='OLD',ERR=99)
         ELSE
C           WRITE Error Message:  INCFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
            GO TO 1002
         END IF

      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message         ! No Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      GO TO 1001

C     Write Out Error Message for File OPEN Error
99    CALL ERRHDL(PATH,MODNAM,'E','500','INCFILE ')
      GO TO 1002

1001  CONTINUE

      
C     LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF)

C        Increment the Line Counter.  It was Initially Set to 1, to Handle
C        the Code in Subroutine DEFINE
         ILINE = ILINE + 1
         ILREAL = ILREAL + 1

C        READ Record to Buffers, as A'num' and 'num'A1 where 'num' = ISTRG.
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1
         READ (INCUNT,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

C        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C        Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

         IF (ILREAL .EQ. 1) ILINE = ILINE -1

C        Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

C        If Blank Line, Then CYCLE to Next Card
         IF (BLINE) GO TO 11

C        Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
C           Skip record with NO ECHO in INCLUDED file, but leave ECHO "on"
            GO TO 11
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         PATHWY(1) = 'CO'
         PATHWY(2) = 'SO'
         PATHWY(3) = 'RE'
         PATHWY(4) = 'OU'
         PATHWY(5) = '**'
         CALL EXPATH(FIELD(1),PATHWY,5,NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH) THEN
C           WRITE Error Message    ! Invalid Pathway ID
            CALL ERRHDL(PPATH,MODNAM,'E','100',PATH)
            PATH = PPATH
            GO TO 11
         ELSE IF (PATH .EQ. '**') THEN
            GO TO 11
         END IF

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

         IF (NOKEY) THEN
C           WRITE Error Message    ! Invalid Keyword
            CALL ERRHDL(PATH,MODNAM,'E','105',KEYWRD)
            PKEYWD = KEYWRD
            GO TO 11
         END IF

C        Check for Proper Order of Setup Cards              ---   CALL SETORD
         IF (KEYWRD .NE. 'STARTING' .AND.
     &       KEYWRD .NE. 'FINISHED') CALL SETORD

C        First Check for Invalid Keywords (STARTING, FINISHED, INCLUDED)
         IF (KEYWRD .EQ. 'STARTING') THEN
C           Cannot Use the STARTING keyword in the INCLUDED file
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)

         ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
C           Cannot Recurse the INCLUDED Keyword in the INCLUDED file
C           Write Error Message: Repeat INCLUDED In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)

         ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C           Cannot Use the FINISHED Keyword in the INCLUDED File
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)

C        Process Input Card Based on Pathway
         ELSE IF (PATH .EQ. 'SO') THEN
C           Process SOurce Pathway Cards                    ---   CALL SOCARD
            CALL SOCARD
            
         ELSE IF (PATH .EQ. 'RE') THEN
C           Process REceptor Pathway Cards                  ---   CALL RECARD
            CALL RECARD
            
         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

         GO TO 11
 999     EOF = .TRUE.
 11      CONTINUE

      END DO
      EOF = .FALSE.

C     Close the INCLUDED File
      CLOSE (INCUNT)
      
1002  RETURN
      END

      SUBROUTINE RECART
C***********************************************************************
C*                RECART Module of ISC2 Model
C
C*       PURPOSE: Processes Cartesian Grid Receptor Network Inputs
C
C*       PROGRAMMER:  Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       MODIFIED:   To Fix Error Checking - Compare NETIDT With
C*                   Full Secondary Keywords - 9/29/92
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Cartesian Grid Receptor Network Inputs
C
C*       CALLED FROM:   RECARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM
      INTEGER I

C*    Variable Initializations
      MODNAM = 'RECART'

C*    READ in the Netid and Nettype
      IF (IFC .LT. 3) THEN
C*       Write Error Message: Missing Data Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      END IF
      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'XYINC' .OR. NETIDT.EQ.'XPNTS' .OR.
     &    NETIDT.EQ.'YPNTS' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'FLAG'  .OR. NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
      ELSE IF (NEWID .AND. NETIDT.NE.' ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
C*       The Keyword Counter
         INNET = INNET + 1
         IF (INNET .GT. NNET) THEN
C*          WRITE Error Message:  Too Many Networks
            WRITE(DUMMY,'(I8)') NNET
            CALL ERRHDL(PATH,MODNAM,'E','224',DUMMY)
            RECERR = .TRUE.
            GO TO 999
         END IF
         INCSET = 0
         IXYSET = 0
         IEVSET = 0
         IFGSET = 0
      ELSE
C*       Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',PNETID)
         RECERR = .TRUE.
         GO TO 999
      END IF

C*    Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
C*       Initialize Logical Control Variables
         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
C*       Set Counters of Calculation Field
         ICOUNT = 0
         JCOUNT = 0
         IZE = 0
         IZF = 0
         IDC1 = IRXR
C*       Check for Previous Grid Network With Same ID
         DO I = 1, INNET-1
            IF (FIELD(3) .EQ. NTID(I)) THEN
C*             WRITE Warning Message:  Duplicate Network ID
               CALL ERRHDL(PATH,MODNAM,'W','252',NTID(I))
            END IF
         END DO
      ELSE IF (KTYPE .EQ. 'XYINC') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C*       Error Message:Conflict Secondary Keyword
         IF (IXYSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C*       Set the Uniform Spacing Receptor Network           ---   CALL GENCAR
         CALL GENCAR
         INCSET = INCSET + 1
      ELSE IF (KTYPE.EQ.'XPNTS' .OR. KTYPE.EQ.'YPNTS') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C*       Error Message:Conflict Secondary Keyword
         IF (INCSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C*       Set the Non-uniform Spacing Receptor Network       ---   CALL XYPNTS
         CALL XYPNTS
         IXYSET = IXYSET + 1
      ELSE IF (KTYPE .EQ. 'ELEV') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C*       Read in and set the Terrain Elevation              ---   CALL TERHGT
         CALL TERHGT
         IEVSET = IEVSET + 1
      ELSE IF (KTYPE .EQ. 'FLAG') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C*       Read in and set the Flagpole Receptor              ---   CALL FLGHGT
         CALL FLGHGT
         IFGSET = IFGSET + 1
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
C*       Get The Final Results
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         ELSE IF (.NOT. RECERR) THEN
            CALL SETCAR
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.
C*       Check If The Secondary Parameter Has Been Specified
         IF (IXYSET.EQ.0 .AND. INCSET.EQ.0) THEN
C*          WRITE Error Message: Missing (X,Y) Point Setting
            CALL ERRHDL(PATH,MODNAM,'E','212',NETIDT)
         END IF

C*       Warning: Elevated Terrain Inputs Inconsistent With Options
         IF (.NOT.EXTRACT .AND. IEVSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','214',NETIDT)
            IRZE = IRXR
            IRHZ = IRZE
         END IF

C*       Warning: Flagpole Receptor Inputs Inconsistent With Options
         IF (FLGPOL .AND. IFGSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','216',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','215',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.EQ.0) THEN
            IRZF = IRXR
         END IF

C*       Check If The Number of Elev & Flag Is Match
         IF (.NOT.EXTRACT .AND. IEVSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZE) THEN
C*             Write Out The Error Message: No. Of ELEV not match
               CALL ERRHDL(PATH,MODNAM,'E','218','ELEV')
            END IF
         END IF
         IF (FLGPOL .AND. IFGSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZF) THEN
C*             Write Out The Error Message: No. Of FLAG not match
               CALL ERRHDL(PATH,MODNAM,'E','218','FLAG')
            END IF
         END IF

C*       Check If The Number of Elev & Flag Is Match
         IF (EXTRACT .AND. IEVSET.NE.0) THEN
C*          Write Out The Error Message: No. Of ELEV not match
            CALL ERRHDL(PATH,MODNAM,'W','214',NETIDT)
         END IF

      ELSE
C*       Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',NETIDT)
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      end subroutine

      SUBROUTINE GENCAR
C***********************************************************************
C*                GENCAR Module of ISC2 Model
C
C*       PURPOSE: Generates Cartesian Grid Receptor Network With
C*                Uniform Spacing
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Cartesian Grid Receptor Network With Uniform
C*                Spacing
C
C*       CALLED FROM:   RECART
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I, J, K
      DOUBLE PRECISION :: TEMPG(6)
      LOGICAL ERROR

C*    Variable Initializations
      MODNAM = 'GENCAR'
      ERROR = .FALSE.

C*    Check for Location of Secondary Keyword, XYINC
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'XYINC') THEN
            ISC = I + 1
         END IF
      END DO

C*    Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C*       Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+5) THEN
C*       Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+5) THEN
C*       Error Message: Too Few Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

C*    Input The Numerical Values
      DO K = 1,6
         CALL STODBL(FIELD(ISC + K-1),ILEN_FLD,TEMPG(K),MITL)
C*       Check The Numerical Field
         IF (MITL .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ERROR = .TRUE.
            RECERR = .TRUE.
         END IF
      END DO

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

C*    Assign Values to Appropriate Variables for Generated Network
      XINT   = TEMPG(1)
      ICOUNT = IDNINT(TEMPG(2))
      XDELTA = TEMPG(3)
      YINT   = TEMPG(4)
      JCOUNT = IDNINT(TEMPG(5))
      YDELTA = TEMPG(6)

C*    Assign Them to the Coordinate Arrays
      IF (ICOUNT .LE. IXM) THEN
         DO I = 1, ICOUNT
            XCOORD(I,INNET) = XINT + XDELTA*DBLE(I-1)
         END DO
      ELSE
C*       WRITE Error Message:  Too Many X-Coordinates for This Network
         WRITE(DUMMY,'(I8)') IXM
         CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
         RECERR = .TRUE.
      END IF
      IF (JCOUNT .LE. IYM) THEN
         DO J = 1, JCOUNT
            YCOORD(J,INNET) = YINT + YDELTA*DBLE(J-1)
         END DO
      ELSE
C*       WRITE Error Message:  Too Many Y-Coordinates for This Network
         WRITE(DUMMY,'(I8)') IYM
         CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
         RECERR = .TRUE.
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE XYPNTS
C***********************************************************************
C*                XYPNTS Module of ISC2 Model
C
C*       PURPOSE: Processes Cartesian Grid x,y Input Value
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       MODIFIED:   To Fix Error Checking - Change Limit for DO 15
C*                   To 'JSET -1' - 9/29/92
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Cartesian Grid x,y Input Value
C
C*       CALLED FROM:   RECART
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I, J, IMIT, ISET, JSET

C*    Variable Initializations
      MODNAM = 'XYPNTS'

      IF (KTYPE .EQ. 'XPNTS') THEN
C*       Check for Location of Secondary Keyword, XPNTS
         DO I = 1, IFC
            IF (FIELD(I) .EQ. 'XPNTS') THEN
               ISC = I + 1
            END IF
         END DO

C*       Determine Whether There Are Enough Parameter Fields
         IF (IFC .EQ. ISC-1) THEN
C*          Error Message: Missing Parameter
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            RECERR = .TRUE.
            GO TO 999
         END IF

         ISET = ICOUNT
         DO I = ISC, IFC
            CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C*          Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               RECERR = .TRUE.
            END IF
            ISET = ISET + 1
            IF (ISET .LE. IXM) THEN
               XCOORD(ISET,INNET) = DNUM
               DO J = 1, ISET-1
                  IF (DNUM .EQ. XCOORD(J,INNET)) THEN
C*                   WRITE Warning Message:  X-Coord Specified More Than Once
                     CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
                  END IF
               END DO
            ELSE
C*             WRITE Error Message:  Too Many X-Coordinates for This Network
               WRITE(DUMMY,'(I8)') IXM
               CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
               RECERR = .TRUE.
            END IF
         END DO
         ICOUNT = ISET

      ELSE IF (KTYPE .EQ. 'YPNTS') THEN
C*       Check for Location of Secondary Keyword, YPNTS
         DO I = 1, IFC
            IF (FIELD(I) .EQ. 'YPNTS') THEN
               ISC = I + 1
            END IF
         END DO

C*       Determine Whether There Are Enough Parameter Fields
         IF (IFC .EQ. ISC-1) THEN
C*          Error Message: Missing Parameter
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            RECERR = .TRUE.
            GO TO 999
         END IF

         JSET = JCOUNT

         DO I = ISC, IFC
            CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C*          Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               RECERR = .TRUE.
            END IF
            JSET = JSET + 1
            IF (JSET .LE. IYM) THEN
               YCOORD(JSET,INNET) = DNUM
               DO J = 1, JSET-1
                  IF (DNUM .EQ. YCOORD(J,INNET)) THEN
C*                   WRITE Warning Message:  Y-Coord Specified More Than Once
                     CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
                  END IF
               END DO
            ELSE
C*             WRITE Error Message:  Too Many Y-Coordinates for This Network
               WRITE(DUMMY,'(I8)') IYM
               CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
               RECERR = .TRUE.
            END IF
         END DO
         JCOUNT = JSET
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE SETCAR
C***********************************************************************
C*                SETCAR Module of ISC2 Model
C
C*       PURPOSE: Setup the Final Cartesian Grid Receptor Network Inputs
C
C*       PROGRAMMER:  Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C
C*       INPUTS:  The GRIDCART Sub-pathway Input Parameters
C
C*       OUTPUTS: Cartesian Grid Receptor Network Inputs
C
C*       CALLED FROM:   RECART
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I, J
      INTEGER ISET, JSET

C*    Variable Initializations
      MODNAM = 'SETCAR'

      IF (ICOUNT.NE.0 .AND. JCOUNT.NE.0) THEN
C*       Setup The Coordinate Of The Receptors
         NETSTA(INNET) = IRXR + 1
         ISET = IRXR
         JSET = IRYR
         DO J = 1, JCOUNT
            DO I = 1, ICOUNT
               ISET = ISET + 1
               JSET = JSET + 1
               IF (ISET .GT. NREC) THEN
C*                Error Msg: Maximum Number Of Receptor Exceeded
                  WRITE(DUMMY,'(I8)') NREC
                  CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
                  GO TO 999
               END IF
               IF (ICOUNT .GT. IXM) THEN
C*                WRITE Error Message:  Too Many X-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IXM
                  CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
                  GO TO 999
               END IF
               IF (JCOUNT .GT. IYM) THEN
C*                WRITE Error Message:  Too Many Y-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IYM
                  CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
                  GO TO 999
               END IF
               AXR(ISET) = XCOORD(I,INNET)
               AYR(JSET) = YCOORD(J,INNET)
            END DO
         END DO
         IRXR = ISET
         IRYR = JSET
         NETEND(INNET) = IRXR
         NUMXPT(INNET) = ICOUNT
         NUMYPT(INNET) = JCOUNT
         NTID(INNET)   = NETIDT
         NTTYP(INNET)  = 'GRIDCART'
      END IF

C*    Setup The AZELEV Array
      CALL SBYVAL(ZETMP1,ZETMP2,IZE)
      ISET = IRZE
      DO I = 1, IZE
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C*          Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZELEV(ISET) = ZETMP2(I)
      END DO
      IRZE = ISET
      IRHZ = IRZE

C*    Setup The AZFLAG Aarry
      CALL SBYVAL(ZFTMP1,ZFTMP2,IZF)
      ISET = IRZF
      DO I = 1, IZF
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C*          Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZFLAG(ISET) = ZFTMP2(I)
      END DO
      IRZF = ISET

      DO I = IDC1+1, IRXR
         NETID(I) = NETIDT
         RECTYP(I) = 'GC'
      END DO

 999  RETURN
      end subroutine

      SUBROUTINE REPOLR
C***********************************************************************
C*                REPOLR Module of ISC2 Model
C
C*       PURPOSE: Processes Polar Grid Receptor Network Inputs
C
C*       PROGRAMMER:  Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Polar Receptor Network Inputs
C
C*       CALLED FROM:   RECARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I
      INTEGER IORSET, IXRSET, IDRSET, IGRSET

C*    Variable Initializations
      MODNAM = 'REPOLR'

      IF (IFC .LT. 3) THEN
C*       Write Error Message: Missing Data Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      END IF

C*    READ in the Netid and Nettype
      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'ORIG' .OR. NETIDT.EQ.'DIST' .OR.
     &    NETIDT.EQ.'DDIR' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'FLAG' .OR. NETIDT.EQ.'GDIR' .OR.
     &    NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
      ELSE IF (NEWID .AND. NETIDT.NE.'    ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
C*       The Keyword Counter
         INNET = INNET + 1
         IF (INNET .GT. NNET) THEN
C*          WRITE Error Message:  Too Many Networks
            WRITE(DUMMY,'(I8)') NNET
            CALL ERRHDL(PATH,MODNAM,'E','224',DUMMY)
            RECERR = .TRUE.
            GO TO 999
         END IF
         IORSET = 0
         IXRSET = 0
         IDRSET = 0
         IGRSET = 0
         IEVSET = 0
         IFGSET = 0
      ELSE
C*       Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',PNETID)
         RECERR = .TRUE.
         GO TO 999
      END IF

C*    Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
         ICOUNT = 0
         JCOUNT = 0
         IZE = 0
         IZF = 0
         IDC1 = IRXR
C*       Check for Previous Grid Network With Same ID
         DO I = 1, INNET-1
            IF (FIELD(3) .EQ. NTID(I)) THEN
C*             WRITE Warning Message:  Duplicate Network ID
               CALL ERRHDL(PATH,MODNAM,'W','252',NTID(I))
            END IF
         END DO
      ELSE IF (KTYPE .EQ. 'ORIG') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
         IF (IORSET .NE. 0) THEN
C*          Error Message: Conflict Secondary Keyword
            CALL ERRHDL(PATH,MODNAM,'E','160',NETIDT)
         END IF
C*       Read In XINT, YINT                                 ---   CALL POLORG
         CALL POLORG
         IORSET = IORSET + 1
      ELSE IF (KTYPE .EQ. 'DIST') THEN
C*       Read in the Distance Set                           ---   CALL POLDST
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
         CALL POLDST
         IXRSET = IXRSET + 1
      ELSE IF (KTYPE .EQ. 'GDIR') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
         IF (IDRSET .NE. 0) THEN
C*          Error Message: Conflict Secondary Keyword
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C*       Set the Uniform Spacing Receptor Network           ---   CALL GENPOL
         CALL GENPOL
         IGRSET = IGRSET + 1
      ELSE IF (KTYPE .EQ. 'DDIR') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C*       Error Message: Conflict Secondary Keyword
         IF (IGRSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C*       Set the Non-uniform Spacing Receptor Network       ---   CALL RADRNG
         CALL RADRNG
         IDRSET = IDRSET + 1
      ELSE IF (KTYPE .EQ. 'ELEV') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C*       Read in and set the Terrain Elevation              ---   CALL TERHGT
         CALL TERHGT
         IEVSET = IEVSET + 1
      ELSE IF (KTYPE .EQ. 'FLAG') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C*       Read in and set the Flagpole Receptor              ---   CALL FLGHGT
         CALL FLGHGT
         IFGSET = IFGSET + 1
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
C*       Get the Final Result
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         ELSE IF (.NOT. RECERR) THEN
            CALL SETPOL
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.
C*       Check If The Secondary Parameter Has Been Specified
C*       Warning Message: Missing (Xin,Yin) Point Setting
         IF (IORSET .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','220',NETIDT)
            XINT = 0.0D0
            YINT = 0.0D0
         END IF
C*       Error Message: Missing Distance Point Setting
         IF (IXRSET .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','221',NETIDT)
         END IF
C*       Error Message: Missing Degree Or Rad Setting
         IF (IGRSET.EQ.0 .AND. IDRSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','222',NETIDT)
         END IF

C*       Warning: Elevated Terrain Inputs Inconsistent With Options
         IF (.NOT.EXTRACT .AND. IEVSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','214',NETIDT)
            IRZE = IRXR
            IRHZ = IRZE
         END IF

C*       Warning: Flagpole Receptor Inputs Inconsistent With Options
         IF (FLGPOL .AND. IFGSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','216',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','215',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.EQ.0) THEN
            IRZF = IRXR
         END IF

C*       Check If The Number of Elev & Flag Is Match
         IF (.NOT.EXTRACT .AND. IEVSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZE) THEN
C*             Write Out The Error Message: No. Of ELEV not match
               CALL ERRHDL(PATH,MODNAM,'E','218','ELEV')
            END IF
         END IF
         IF (FLGPOL .AND. IFGSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZF) THEN
C*             Write Out The Error Message: No. Of FLAG not match
               CALL ERRHDL(PATH,MODNAM,'E','218','FLAG')
            END IF
         END IF

C*       Check If The Number of Elev & Flag Is Match
         IF (EXTRACT .AND. IEVSET.NE.0) THEN
C*          Write Out The Error Message: No. Of ELEV not match
            CALL ERRHDL(PATH,MODNAM,'W','214',NETIDT)
         END IF

      ELSE
C*       Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',NETIDT)
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      end subroutine

      SUBROUTINE POLORG
C***********************************************************************
C*                POLORG Module of ISC2 Model
C
C*       PURPOSE: Input The Original of The Polar Network
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C        MODIFIED:   To increase maximum length of source IDs from
C                    8 to 12 characters.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Polar Network Origin  Coordinates
C
C*       CALLED FROM:   REPOLR
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      CHARACTER SOID*12
      LOGICAL FOUND
      INTEGER I
      INTEGER ISDX, IMUT

C*    Variable Initializations
      MODNAM = 'POLORG'
      FOUND = .FALSE.

C*    Check for the Location of the Secondary Keyword, ORIG
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'ORIG') THEN
            ISC = I + 1
         END IF
      END DO

C*    Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C*       Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+1) THEN
C*       Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

      IF (IFC .EQ. ISC) THEN
C*       Identify Origin Associated With a Source ID
C*       First check for length of SRCID field <=12
         IF ((LOCE(ISC)-LOCB(ISC)) .LE. 11) THEN
C*          Retrieve Source ID Character Substring
            SOID = FIELD(ISC)
         ELSE
C*          WRITE Error Message:  Source ID Field is Too Long
            CALL ERRHDL(PATH,MODNAM,'E','206',FIELD(ISC)(1:12))
            RECERR = .TRUE.
            GO TO 999
         END IF
C*       Check for valid SRCID         
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
         IF (.NOT. FOUND) THEN
C*          Error Message: Source ID Does Not Match Existing Sources
            CALL ERRHDL(PATH,MODNAM,'E','375',KEYWRD)
            RECERR = .TRUE.
         ELSE
            XINT = AXS(ISDX)
            YINT = AYS(ISDX)
         END IF

      ELSE
C*       Input Numerical Values, XINT and YINT
         CALL STODBL(FIELD(ISC),ILEN_FLD,XINT,IMUT)
C*       Check The Numerical Field
         IF (IMUT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF

         CALL STODBL(FIELD(ISC + 1),ILEN_FLD,YINT,IMUT)
C*       Check The Numerical Field
         IF (IMUT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE POLDST
C***********************************************************************
C*                POLDST Module of ISC2 Model
C
C*       PURPOSE: Gets Distances for the Polar Network
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Polar Network Distance Input Value
C
C*       CALLED FROM:   REPOLR
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I, J
      INTEGER IMIT, ISET

C*    Variable Initializations
      MODNAM = 'POLDST'

C*    Skip the Unrelated Fields
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'DIST') THEN
            ISC = I + 1
         END IF
      END DO

C*    Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C*       Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = ICOUNT

      DO I = ISC, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C*       Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .LE. IXM) THEN
C*          Store Distance to XCOORD Array and Check for Previous Occurrence
            XCOORD(ISET,INNET) = DNUM
            DO J = 1, (ISET - 1)
               IF (DNUM .EQ. XCOORD(J,INNET)) THEN
C*                WRITE Warning Message:  Distance Specified More Than Once
                  CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
               END IF
            END DO
         ELSE
C*          WRITE Error Message:  Too Many X-Coordinates for This Network
            WRITE(DUMMY,'(I8)') IXM
            CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
            RECERR = .TRUE.
         END IF
      END DO

      ICOUNT = ISET

 999  RETURN
      end subroutine

      SUBROUTINE GENPOL
C***********************************************************************
C*                GENPOL Module of ISC2 Model
C
C*       PURPOSE: Generates Polar Receptor Network With
C*                Uniform Spacing
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Polar Receptor Network With Uniform Direction Spacing
C
C*       CALLED FROM:   REPOLR
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I, J, K
      DOUBLE PRECISION :: TEMPG(3)
      DOUBLE PRECISION :: DIRINI, DIRINC
      LOGICAL ERROR

C*    Variable Initializations
      MODNAM = 'GENPOL'
      ERROR = .FALSE.

C*    Check for the Location of the Secondary Keyword, GDIR
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'GDIR') THEN
            ISC = I + 1
         END IF
      END DO

C*    Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C*       Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+2) THEN
C*       Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+2) THEN
C*       Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

C*    Input Numerical Values
      DO K = 1, 3
         CALL STODBL(FIELD(ISC + K-1),ILEN_FLD,TEMPG(K),MITL)
C*       Check The Numerical Field
         IF (MITL .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
            ERROR = .TRUE.
         END IF
      END DO

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

      JCOUNT = IDNINT(TEMPG(1))
      DIRINI = TEMPG(2)
      DIRINC = TEMPG(3)

C*    Assign Them to the Coordinate Arrays
      IF (JCOUNT .LE. IYM) THEN
         DO J = 1, JCOUNT
            YCOORD(J,INNET) = (DIRINI + DIRINC*DBLE(J-1))
            IF (YCOORD(J,INNET) .GT. 360.0D0) THEN
               YCOORD(J,INNET) = YCOORD(J,INNET) - 360.0D0
            ELSE IF (YCOORD(J,INNET) .LE. 0.0D0) THEN
               YCOORD(J,INNET) = YCOORD(J,INNET) + 360.0D0
            END IF
         END DO
      ELSE
C*       WRITE Error Message:  Too Many Y-Coordinates for This Network
         WRITE(DUMMY,'(I8)') IYM
         CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
         RECERR = .TRUE.
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE RADRNG
C***********************************************************************
C*                RADRNG Module of ISC2 Model
C
C*       PURPOSE: Processes Non-Uniform Polar Network Value
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Polar Network Directions in Non-Uniform Spacing
C
C*       CALLED FROM:   REPOLR
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I, J
      INTEGER IMIT, ISET

C*    Variable Initializations
      MODNAM = 'RADRNG'

C*    Skip the non-useful Fields
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'DDIR') THEN
            ISC = I + 1
         END IF
      END DO

C*    Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C*       Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = JCOUNT

      DO I = ISC, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C*       Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .LE. IYM) THEN
C*          Store Direction to YCOORD Array, Adjust to 0-360 Range if Needed,
C*          and Check for Previous Occurrence
            YCOORD(ISET,INNET) = DNUM
            IF (YCOORD(ISET,INNET) .GT. 360.0D0) THEN
               YCOORD(ISET,INNET) = YCOORD(ISET,INNET) - 360.0D0
            ELSE IF (YCOORD(ISET,INNET) .LE. 0.0D0) THEN
               YCOORD(ISET,INNET) = YCOORD(ISET,INNET) + 360.0D0
            END IF
            DO J = 1, ISET-1
               IF (DNUM .EQ. YCOORD(J,INNET)) THEN
C*                WRITE Warning Message:  Direction Specified More Than Once
                  CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
               END IF
            END DO
         ELSE
C*          WRITE Error Message:  Too Many Y-Coordinates for This Network
            WRITE(DUMMY,'(I8)') IYM
            CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
            RECERR = .TRUE.
         END IF
      END DO

      JCOUNT = ISET

 999  RETURN
      end subroutine

      SUBROUTINE SETPOL
C***********************************************************************
C*                SETPOL Module of ISC2 Model
C
C*       PURPOSE: Setup the Final Polar Receptor Network Inputs
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       INPUTS:  The GRIDPOLR Sub-pathway Input Parameters
C
C*       OUTPUTS: Polar Receptor Network Arrays
C
C*       CALLED FROM:   REPOLR
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I, J
      INTEGER ISET, JSET
      DOUBLE PRECISION :: YTEMP

C*    Variable Initializations
      MODNAM = 'SETPOL'

      IF (ICOUNT.NE.0 .AND. JCOUNT.NE.0) THEN
C*       Setup The Coordinate Of The Receptors
         NETSTA(INNET) = IRXR + 1
         ISET = IRXR
         JSET = IRYR
         DO J = 1, JCOUNT
            DO I = 1, ICOUNT
               ISET = ISET + 1
               JSET = JSET + 1
               IF (ISET .GT. NREC) THEN
C*                Error Msg: Maximum Number Of Receptor Exceeded
                  WRITE(DUMMY,'(I8)') NREC
                  CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
                  GO TO 999
               END IF
               IF (ICOUNT .GT. IXM) THEN
C*                WRITE Error Message:  Too Many X-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IXM
                  CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
                  GO TO 999
               END IF
               IF (JCOUNT .GT. IYM) THEN
C*                WRITE Error Message:  Too Many Y-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IYM
                  CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
                  GO TO 999
               END IF
               YTEMP = YCOORD(J,INNET) * DTORAD
               AXR(ISET) = XINT + XCOORD(I,INNET)*DSIN(YTEMP)
               AYR(JSET) = YINT + XCOORD(I,INNET)*DCOS(YTEMP)
            END DO
         END DO
         IRXR = ISET
         IRYR = JSET
         XORIG(INNET)  = XINT
         YORIG(INNET)  = YINT
         NETEND(INNET) = IRXR
         NUMXPT(INNET) = ICOUNT
         NUMYPT(INNET) = JCOUNT
         NTID(INNET)   = NETIDT
         NTTYP(INNET)  = 'GRIDPOLR'
      END IF

C*    Setup The AZELEV Array
      CALL SBYVAL(ZETMP1,ZETMP2,IZE)
      ISET = IRZE
      DO I = 1, IZE
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C*          Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZELEV(ISET) = ZETMP2(I)
      END DO
      IRZE = ISET
      IRHZ = IRZE

C*    Setup The AZFLAG Array
      CALL SBYVAL(ZFTMP1,ZFTMP2,IZF)
      ISET = IRZF
      DO I = 1, IZF
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C*          Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZFLAG(ISET) = ZFTMP2(I)
      END DO
      IRZF = ISET

      DO I = IDC1+1, IRXR
         NETID(I) = NETIDT
         RECTYP(I) = 'GP'
      END DO

 999  RETURN
      end subroutine

      SUBROUTINE TERHGT
C***********************************************************************
C*                TERHGT Module of ISC2 Model
C
C*       PURPOSE: Processes Elevated Terrain Inputs for Receptor Network
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Elevated Terrain Input for a Receptor Network
C
C*       CALLED FROM:   RECART
C*                      REPOLR
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I, J
      INTEGER IZE1, IMIT, ISET
      DOUBLE PRECISION ROW

C*    Variable Initializations
      MODNAM = 'TERHGT'
      IZE1 = IZE + 1

C*    Check for the Location of the Secondary Keyword, ELEV
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'ELEV') THEN
            ISC = I + 1
         END IF
      END DO

C*    Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C*       Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','223',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .EQ. ISC) THEN
C*       Error Message: Missing Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      CALL STODBL(FIELD(ISC),ILEN_FLD,DNUM,IMIT)
C*    Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         RECERR = .TRUE.
      END IF
      ROW = DNUM

      ISET = IZE

      DO I = ISC+1, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C*       Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
            ZETMP1(ISET) = ROW
            ZETMP2(ISET) = DNUM
         END DO
      END DO

      IZE = ISET

      IF (REELEV .EQ. 'FEET') THEN
C*       Convert ELEV to Metric System
         DO I = IZE1, IZE
            ZETMP2(I) = 0.3048D0*ZETMP2(I)
         END DO
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE FLGHGT
C***********************************************************************
C*                FLGHGT Module of ISC2 Model
C
C*       PURPOSE: Processes Flagpole Receptor Heights for Receptor Network
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Flagpole Receptor Heights for a Receptor Network
C
C*       CALLED FROM:   RECART
C*                      REPOLR
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I, J
      INTEGER IMIT, ISET
      REAL ROW

C*    Variable Initializations
      MODNAM = 'FLGHGT'

C*    Check for the Location of the Secondary Keyword, FLAG
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'FLAG') THEN
            ISC = I + 1
         END IF
      END DO

C*    Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C*       Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','223',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .EQ. ISC) THEN
C*       Error Message: Missing Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      CALL STODBL(FIELD(ISC),ILEN_FLD,DNUM,IMIT)
C*    Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         RECERR = .TRUE.
      END IF
      ROW = DNUM

      ISET = IZF

      DO I = ISC+1, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C*       Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
            ZFTMP1(ISET) = ROW
            ZFTMP2(ISET) = DNUM
         END DO
      END DO

      IZF = ISET

 999  RETURN
      end subroutine

      SUBROUTINE DISCAR
C***********************************************************************
C*                DISCAR Module of ISC2 Model
C
C*       PURPOSE: Processes Discrete Cartesian Receptor Location Inputs
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Discrete Cartesian Receptor Location Inputs
C
C*       CALLED FROM:   RECARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I1, I2, I3, I4, I5 , INUM

C*    Variable Initializations
      MODNAM = 'DISCAR'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF
      I5 = IRHZ

C*    Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF (IFC .LT. 4) THEN
C*       WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 6) THEN
C*       Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      ELSE IF (.NOT.EXTRACT .AND. FLGPOL .AND. IFC.LT.6) THEN
C*       WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF ((.NOT.EXTRACT .OR. FLGPOL) .AND. IFC.LT.5) THEN
C*       WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF (.NOT.EXTRACT .AND. .NOT.FLGPOL .AND. IFC .GT. 5) THEN
C*       WRITE Warning Message: Parameter Ignored, ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (FLGPOL .AND. EXTRACT .AND. IFC .GT. 5) THEN
C*       WRITE Warning Message: Parameter Ignored, ZELEV & ZHILL
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (EXTRACT .AND. .NOT.FLGPOL .AND. IFC .GT. 4) THEN
C*       WRITE Warning Message: Parameters Ignored, ZELEV ZHILL & ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      END IF

C*    Check Whether The Maximum Number of Receptors is Exceeded
      IF (I1.EQ.NREC .OR. I2.EQ.NREC .OR. I3.EQ.NREC .OR.
     &    I4.EQ.NREC .OR. I5.EQ.NREC) THEN
C*       Error Msg: Maximum Number Of Receptors Exceeded
         WRITE(DUMMY,'(I8)') NREC
         CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
         GO TO 999
      END IF

C*    READ XCOORD,YCOORD,ELEV,HILLZ,FLAG And Assign Them to Different 
C*    Arrays

      CALL STODBL(FIELD(3),ILEN_FLD,AXR(I1+1),INUM)
C*    Check The Numerical Field
      IF (INUM .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STODBL(FIELD(4),ILEN_FLD,AYR(I2+1),INUM)
C*    Check The Numerical Field
      IF (INUM .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      IF (.NOT.EXTRACT .AND. FLGPOL) THEN
         IF (IFC .GE. 5) THEN
            CALL STODBL(FIELD(5),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = DNUM
            END IF
         END IF
         IF (IFC .GE. 6) THEN
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZHILL(I5 + 1) = DNUM
            END IF
         END IF
         IF (IFC .EQ. 7) THEN
            CALL STODBL(FIELD(7),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         END IF
      ELSE IF (.NOT.EXTRACT .AND. .NOT.FLGPOL) THEN
         IF (IFC .GE. 5) THEN
            CALL STODBL(FIELD(5),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = DNUM
            END IF
         END IF
         IF (IFC .GE. 6) THEN
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZHILL(I5 + 1) = DNUM
            END IF
         END IF
      ELSE IF (FLGPOL .AND. EXTRACT) THEN
         IF (IFC .EQ. 5) THEN
            CALL STODBL(FIELD(5),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         ELSE IF (IFC .EQ. 7) THEN
            CALL STODBL(FIELD(7),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         END IF
      END IF

      IF (REELEV .EQ. 'FEET') THEN
C*       Convert ELEV AND ZHILL to Metric system
         AZELEV(I3 + 1) = 0.3048D0*AZELEV(I3 + 1)
      END IF

      IRXR = I1 + 1
      IRYR = I2 + 1
      IRZE = I3 + 1
      IRZF = I4 + 1
      IRHZ = I5 + 1
      NETID(IRXR) = ' '
      RECTYP(IRXR) = 'DC'

 999  RETURN
      end subroutine

      SUBROUTINE DISPOL
C***********************************************************************
C*                DISPOL Module of ISC2 Model
C
C*       PURPOSE: Processes Discrete Polar Receptor Location Inputs
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C        MODIFIED:   To increase maximum length of source IDs from
C                    8 to 12 characters.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Discrete Polar Receptor Location Inputs
C
C*       CALLED FROM:   RECARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      CHARACTER SOID*12
      INTEGER IMIT, INUM, ISDX, I1, I2, I3, I4, I5
      DOUBLE PRECISION :: RANGE, DIRECT, DARG
      LOGICAL FOUND

C*    Variable Initializations
      MODNAM = 'DISPOL'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF
      I5 = IRHZ

C*    Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF (IFC .LT. 5) THEN
C*       WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C*       Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      ELSE IF (.NOT.EXTRACT .AND. FLGPOL .AND. IFC.LT.7) THEN
C*       WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF ((.NOT.EXTRACT .OR. FLGPOL) .AND. IFC.LT.6) THEN
C*       WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF (.NOT.EXTRACT .AND. .NOT.FLGPOL .AND. IFC .GT. 6) THEN
C*       WRITE Warning Message: Parameter Ignored, ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (FLGPOL .AND. EXTRACT .AND. IFC .GT. 6) THEN
C*       WRITE Warning Message: Parameter Ignored, ZELEV
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (EXTRACT .AND. .NOT.FLGPOL .AND. IFC .GT. 5) THEN
C*       WRITE Warning Message: Parameters Ignored, ZELEV & ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      END IF

C*    Check Whether The Maximum Number of Receptors is Exceeded
      IF (I1.EQ.NREC .OR. I2.EQ.NREC .OR. I3.EQ.NREC .OR.
     &    I4.EQ.NREC .OR. I5.EQ.NREC) THEN
C*       Error Msg: Maximum Number Of Receptors Exceeded
         WRITE(DUMMY,'(I8)') NREC
         CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
         GO TO 999
      END IF
C*    READ SRCID,RANGE,DIRECT,ELEV,FLAG

C*    First check for length of SRCID field <=12
      IF ((LOCE(3)-LOCB(3)) .LE. 11) THEN
C*       Retrieve Source ID Character Substring
         SOID = FIELD(3)
      ELSE
C*       WRITE Error Message:  Source ID Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','206',FIELD(3)(1:12))
         RECERR = .TRUE.
         GO TO 999
      END IF

      CALL STODBL(FIELD(4),ILEN_FLD,RANGE,IMIT)
C*    Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STODBL(FIELD(5),ILEN_FLD,DIRECT,IMIT)
C*    Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE IF (DIRECT .GT. 360.0D0) THEN
         DIRECT = DIRECT - 360.0D0
      ELSE IF (DIRECT .LE. 0.0D0) THEN
         DIRECT = DIRECT + 360.0D0
      END IF

      IF (.NOT.EXTRACT .AND. FLGPOL) THEN
         IF (IFC .GE. 6) THEN
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = DNUM
            END IF
         END IF
         IF (IFC .EQ. 7) THEN
            CALL STODBL(FIELD(7),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         END IF
      ELSE IF (.NOT.EXTRACT .AND. .NOT.FLGPOL) THEN
         IF (IFC .GE. 6) THEN
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = DNUM
            END IF
         END IF
      ELSE IF (FLGPOL .AND. EXTRACT) THEN
         IF (IFC .EQ. 6) THEN
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         ELSE IF (IFC .EQ. 7) THEN
            CALL STODBL(FIELD(7),ILEN_FLD,DNUM,INUM)
C*          Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         END IF
      END IF

C*    Assign Them to Different Arrays,
C*    Retrieve The Origin From Source Coordinates

      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
      IF (.NOT. FOUND) THEN
C*       Error Message: Source ID Not Match
         CALL ERRHDL(PATH,MODNAM,'E','375',KEYWRD)
      ELSE
         DARG = DIRECT*DTORAD
         AXR(I1 + 1) = AXS(ISDX) + RANGE*DSIN(DARG)
         AYR(I2 + 1) = AYS(ISDX) + RANGE*DCOS(DARG)
         IF (REELEV .EQ. 'FEET') THEN
C*          Convert ELEV AND ZHILL to Metric system
            AZELEV(I3 + 1) = 0.3048D0*AZELEV(I3 + 1)
         END IF
         
         IRXR = I1 + 1
         IRYR = I2 + 1
         IRZE = I3 + 1
         IRZF = I4 + 1
         IRHZ = I5 + 1
         NETID(IRXR)  = ' '
         RECTYP(IRXR) = 'DP'
         IREF(IRXR)   = ISDX
      END IF


 999  RETURN
      end subroutine


      SUBROUTINE SBYVAL(ARRIN1,ARRIN2,INX)
C***********************************************************************
C*                SBYVAL Module of ISC2 Model
C
C*       PURPOSE: Sort Array By Its 'Index Value'
C
C*       PROGRAMMER: Jeff Wang, Roger Brode
C
C*       DATE:    March 2, 1992
C
C*       INPUTS:  ARRIN1: 'Index Array',  ARRIN2: 'Value Array'
C*                INX: Number of Values to Sort
C
C*       OUTPUTS: Sorted Array
C
C*       CALLED FROM: (This Is A Utility Program)
C***********************************************************************
C
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I
      INTEGER JC, INX, IMIN
      DOUBLE PRECISION :: MIN, TEMP1, TEMP2, ARRIN1(*), ARRIN2(*)

C*    Variable Initialization
      MODNAM = 'SBYVAL'
      JC = 1

      DO WHILE (JC .LE. INX)
C*       Find out The First Minimum In the Array
         MIN = ARRIN1(JC)
         IMIN = JC
         DO I = JC, INX
            IF (ARRIN1(I) .LT. MIN) THEN
               IMIN = I
               MIN = ARRIN1(I)
            END IF
         END DO
C*       Swap The Selected Array Elements
         TEMP1 = ARRIN1(JC)
         TEMP2 = ARRIN2(JC)
         ARRIN1(JC) = ARRIN1(IMIN)
         ARRIN2(JC) = ARRIN2(IMIN)
         ARRIN1(IMIN) = TEMP1
         ARRIN2(IMIN) = TEMP2
C*       Increment The Counter
         JC = JC + 1
      END DO

      RETURN
      END

      SUBROUTINE REELUN
C***********************************************************************
C*                REELUN Module of ISC2 Short Term Model - ISCST2
C
C*       PURPOSE: Process Elevation Units Option for Receptors
C*                From Runstream Input Image
C
C*       PROGRAMMER: Roger Brode
C
C*       DATE:    November 22, 1994
C
C*       INPUTS:  Input Runstream Image Parameters
C
C*       OUTPUTS: Receptor Elevation Units Switch
C
C*       ERROR HANDLING:   Checks for Invalid Parameters;
C*                         Checks for No Parameters;
C*                         Checks for Too Many Parameters
C
C*       CALLED FROM:   RECARD
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C*    Variable Initializations
      MODNAM = 'REELUN'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            REELEV = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            REELEV = 'FEET'
         ELSE
C*          WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','RE_ELEV')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C*       WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C*       WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200','ElevUnit')
      END IF

 999  RETURN
      end subroutine

      SUBROUTINE EVCART
C***********************************************************************
C                 EVCART Module of ISC2 Model
C
C        PURPOSE: Processes Discrete Cartesian Receptor Location Inputs
C                 for Use with the EVALFILE Option
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 29, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Discrete Cartesian Receptor Location Inputs
C                 With 'Arc' Grouping ID
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      LOGICAL FOUND
      INTEGER J
      INTEGER I1, I2, I3, I4, I5, ITAB, INUM

C     Variable Initializations
      MODNAM = 'EVCART'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF
      I5 = IRHZ

C     Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF (IFC .LT. 7) THEN
C        WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 8) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Check Whether The Maximum Number of Receptors is Exceeded
      IF (I1.EQ.NREC .OR. I2.EQ.NREC .OR. I3.EQ.NREC .OR.
     &    I4.EQ.NREC .OR. I5.EQ.NREC) THEN
C        Error Msg: Maximum Number Of Receptors Exceeded
         WRITE(DUMMY,'(I8)') NREC
         CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
         GO TO 999
      END IF

C     READ XCOORD,YCOORD,ELEV,HILLZ,FLAG And Assign Them to Different 
C     Arrays


      CALL STODBL(FIELD(3),ILEN_FLD,AXR(I1+1),INUM)
C     Check The Numerical Field
      IF (INUM .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STODBL(FIELD(4),ILEN_FLD,AYR(I2+1),INUM)
C     Check The Numerical Field
      IF (INUM .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF


      CALL STODBL(FIELD(5),ILEN_FLD,DNUM,INUM)
C*    Check The Numerical Field
      IF (INUM .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AZELEV(I3 + 1) = DNUM
      END IF

      CALL STODBL(FIELD(6),ILEN_FLD,DNUM,INUM)
C     Check The Numerical Field
      IF (INUM .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AZFLAG(I4 + 1) = DNUM
      END IF

C     Read ARCID Field, First Check for Previous Occurrence of This ARCID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMARC)
         IF (FIELD(7) .EQ. ARCID(J)) THEN
            FOUND = .TRUE.
            NDXARC(I1 + 1) = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
         NUMARC = NUMARC + 1
         IF (NUMARC .GT. NARC) THEN
C           Write Error Message:  Too Many ARCs
            WRITE(DUMMY,'(I8)') NARC
            CALL ERRHDL(PATH,MODNAM,'E','254',DUMMY)
            GO TO 999
         ELSE
            ARCID(NUMARC)  = FIELD(7)
            NDXARC(I1 + 1) = NUMARC
         END IF
      END IF

      IF (IFC .EQ. 8) THEN
         RECNAM(I1 + 1) = FIELD(8)
      ELSE
         RECNAM(I1 + 1) = '        '
      END IF

      IF (REELEV .EQ. 'FEET') THEN
C*       Convert ELEV AND ZHILL to Metric system
         AZELEV(I3 + 1) = 0.3048D0*AZELEV(I3 + 1)
      END IF


      IRXR = I1 + 1
      IRYR = I2 + 1
      IRZE = I3 + 1
      IRZF = I4 + 1
      IRHZ = I5 + 1
      NETID(IRXR) = ' '
      RECTYP(IRXR) = 'DC'
C     Reset ITAB Variable for TOXXFILE Option, 9/29/92
      ITAB = 0

 999  RETURN
      end subroutine

      SUBROUTINE OUCARD
C***********************************************************************
C                 OUCARD Module of AERMAP
C
C        PURPOSE: To process OUtput Pathway card images
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C*       Revision History:
C*
C*       MODIFIED: February 9, 2009
C*                
C*                Modified error handling for SOURCLOC keyword.
C*                Fatal error if SOURLOC keyword is specified,
C*                but no source have been defined.
C*                Warning message if sources have been defined,
C*                with TERRHGTS EXTRACT option, but no SOURCLOC
C*                keyword is specified.  This allows for source
C*                locations to be defined to provide reference
C*                coordinates for source-based polar grids, without
C*                requiring SOURCLOC keyword.
C
C        INPUTS:  Pathway (OU) and Keyword
C
C        OUTPUTS: Output Option Switches
C                 Output Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C     Variable Initializations
      MODNAM = 'OUCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         IOSTAT(1) = IOSTAT(1) + 1
      ELSE IF (KEYWRD .EQ. 'RECEPTOR') THEN
C        Set Status Switch
         IOSTAT(2) = IOSTAT(2) + 1
         IF (IOSTAT(2) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process High Value Output Option                ---   CALL OURECF
            CALL OURECF
         END IF
      ELSE IF (KEYWRD .EQ. 'SOURCLOC') THEN
C        Set Status Switch
         IOSTAT(3) = IOSTAT(3) + 1
         IF (IOSTAT(3) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (NUMSRC .EQ. 0) THEN
C*          WRITE Error Message: No sources specified for SOURCLOC output
            CALL ERRHDL(PATH,MODNAM,'E','190','        ')
         ELSE
C           Process Source Location file option              ---   CALL OUSRCF
            CALL OUSRCF
         END IF
      ELSE IF (KEYWRD .EQ. 'DEBUGHIL') THEN
C        Set Status Switch
         IOSTAT(4) = IOSTAT(4) + 1
         IF (IOSTAT(4) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (.NOT. HILLDBG) THEN
C*          WRITE Error Message: HILL DEBUG option not specified; Card ignored
            CALL ERRHDL(PATH,MODNAM,'E','150',KEYWRD)
         ELSE
C           Process High Value Output Option                ---   CALL OUHILDB
            CALL OUHILDB
         END IF
      ELSE IF (KEYWRD .EQ. 'DEBUGREC') THEN
C        Set Status Switch
         IOSTAT(5) = IOSTAT(5) + 1
         IF (IOSTAT(5) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (.NOT. RECDBG) THEN
C*          WRITE Error Message: RECEPTOR DEBUG option not specified; Card ignored
            CALL ERRHDL(PATH,MODNAM,'E','150',KEYWRD)
         ELSE
C           Process High Value Output Option                ---   CALL OURECDB
            CALL OURECDB
         END IF
      ELSE IF (KEYWRD .EQ. 'DEBUGSRC') THEN
C        Set Status Switch
         IOSTAT(6) = IOSTAT(6) + 1
         IF (IOSTAT(6) .NE. 1) THEN
C*          WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (.NOT. SRCDBG) THEN
C*          WRITE Error Message: SOURCE DEBUG option not specified; Card ignored
            CALL ERRHDL(PATH,MODNAM,'E','150',KEYWRD)
         ELSE
C           Process High Value Output Option                ---   CALL OUSRCDB
            CALL OUSRCDB
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IOSTAT(20) = IOSTAT(20) + 1

C*       Check for Missing Mandatory Keywords
         IF (IOSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (IOSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','RECEPTOR')
         END IF
C*       Issue warning for SOURCLOC keyword with EXTRACTED elevations         
         IF (IOSTAT(3).EQ.0 .AND. NUMSRC.GT.0 .AND. EXTRACT) THEN
            WRITE(DUMMY,'(I8)') NUMSRC
            CALL ERRHDL(PATH,MODNAM,'W','191',DUMMY)
         END IF

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

      RETURN
      end subroutine

      SUBROUTINE OURECF
C***********************************************************************
C                 OURECF Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Error Message File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Error Message File Logical Switch and ERRMSG Filename
C
C        ERROR HANDLING:   Checks for No Parameters (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C     Variable Initializations
      MODNAM = 'OURECF'
      
      IF (IFC .EQ. 3) THEN
         IF (LOCE(3)-LOCB(3) .LE. ILEN_FLD-1) THEN
            RECFIL = RUNST1(LOCB(3):LOCE(3))
            OPEN(UNIT=IRUNIT,FILE=RECFIL,ERR=99,STATUS='REPLACE')
         ELSE
C*          WRITE Error Message:  Filename Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
         END IF       
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message                      ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message              ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
      END IF

      GO TO 999
 99   CALL ERRHDL(PATH,MODNAM,'E','500','RECOUT  ')

 999  RETURN
      end subroutine

      SUBROUTINE OUSRCF
C***********************************************************************
C                 OUSRCF Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Error Message File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Error Message File Logical Switch and ERRMSG Filename
C
C        ERROR HANDLING:   Checks for No Parameters (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C     Variable Initializations
      MODNAM = 'OUSRCF'

      IF (IFC .EQ. 3) THEN
         IF (LOCE(3)-LOCB(3) .LE. ILEN_FLD-1) THEN
            SRCFIL = RUNST1(LOCB(3):LOCE(3))
            OPEN(UNIT=ISUNIT,FILE=SRCFIL,ERR=99,STATUS='REPLACE')
         ELSE
C*          WRITE Error Message:  Filename Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
         END IF       
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message                      ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message              ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
      END IF

      GO TO 999
 99   CALL ERRHDL(PATH,MODNAM,'E','500','SRCOUT  ')

 999  RETURN
      end subroutine

      SUBROUTINE OUHILDB
C***********************************************************************
C                 OUHILDB Module of AERMAP
C
C        PURPOSE: Process Filename for Hill Height Debug Option
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    December 7, 2006
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        ERROR HANDLING:   Checks for No Parameters (uses default names)
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   OUCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C     Variable Initializations
      MODNAM = 'OUHILDB'

      IF (IFC .EQ. 3) THEN
        
         IF (LOCE(3)-LOCB(3) .LE. ILEN_FLD-1) THEN
            CALCHC_FILE = RUNST1(LOCB(3):LOCE(3))
         ELSE
C*          WRITE Error Message:  Filename Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
         END IF       
         
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message             ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)

      ELSE

C        WRITE Warning Message           ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
      END IF

      RETURN

      end subroutine

      SUBROUTINE OURECDB
C***********************************************************************
C                 OURECDB Module of AERMAP
C
C        PURPOSE: Process Filenames for Receptor Debug Option
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    December 7, 2006
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        ERROR HANDLING:   Checks for No Parameters (uses default names)
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   OUCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C     Variable Initializations
      MODNAM = 'OURECDB'

      IF (IFC .EQ. 5) THEN

         IF (LOCE(3)-LOCB(3) .LE. ILEN_FLD-1) THEN
            RECDET_FILE = RUNST1(LOCB(3):LOCE(3))
         ELSE
C*          WRITE Error Message:  Filename Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
         END IF       

         IF (LOCE(4)-LOCB(4) .LE. ILEN_FLD-1) THEN
            RECNDEM_FILE = RUNST1(LOCB(4):LOCE(4))
         ELSE
C*          WRITE Error Message:  Filename Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
         END IF       

         IF (LOCE(5)-LOCB(5) .LE. ILEN_FLD-1) THEN
            RECELV_FILE = RUNST1(LOCB(5):LOCE(5))
         ELSE
C*          WRITE Error Message:  Filename Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
         END IF       

      ELSE IF (IFC .GT. 5) THEN
C        WRITE Error Message             ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)

      ELSE

C        WRITE Warning Message           ! Not enough Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
      END IF

      RETURN

      end subroutine

      SUBROUTINE OUSRCDB
C***********************************************************************
C                 OUSRCDB Module of AERMAP
C
C        PURPOSE: Process Filenames for Source Debug Option
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    December 7, 2006
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        ERROR HANDLING:   Checks for No Parameters (uses default names)
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   OUCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

C     Variable Initializations
      MODNAM = 'OUSRCDB'

      IF (IFC .EQ. 5) THEN

         IF (LOCE(3)-LOCB(3) .LE. ILEN_FLD-1) THEN
            SRCDET_FILE = RUNST1(LOCB(3):LOCE(3))
         ELSE
C*          WRITE Error Message:  Filename Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
         END IF       

         IF (LOCE(4)-LOCB(4) .LE. ILEN_FLD-1) THEN
            SRCNDEM_FILE = RUNST1(LOCB(4):LOCE(4))
         ELSE
C*          WRITE Error Message:  Filename Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
         END IF       

         IF (LOCE(5)-LOCB(5) .LE. ILEN_FLD-1) THEN
            SRCELV_FILE = RUNST1(LOCB(5):LOCE(5))
         ELSE
C*          WRITE Error Message:  Filename Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
         END IF       

      ELSE IF (IFC .GT. 5) THEN
C        WRITE Error Message             ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)

      ELSE

C        WRITE Warning Message           ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
      END IF

      RETURN

      end subroutine

C
C     End of call to Setup
C

      SUBROUTINE TERRST
C***********************************************************************
C*                TERRST Module of the AMS/EPA Regulatory Model
C*                Terrain Preprocessor- AERMAP
C
C*       PURPOSE: To Determine Total Error/Message Statistics
C
C*       PROGRAMMER:  Jeff Wang, Roger Brode
C
C*       DATE:      September 29, 1995
C
C        MODIFIED:  To increase secondary message string from 8 to 
C                   12 characters, allowing for 12-character source IDs
C                   R. W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011
C
C*       INPUTS:    Error Message Temporary File
C
C*       OUTPUTS:   Total Number of Messages by Message Type
C
C*       CALLED FROM:  This is A Utility Program
C***********************************************************************

C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER*12 MODNAM

      CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*12, INPFLD*3
      INTEGER IERRLN, IMIT

C*    Variable Initialization
      MODNAM = 'TERRST'
      IFTL = 0
      IWRN = 0
      INFO = 0
      ICLM = 0
      IMSG = 0
      EOF1 = .FALSE.

C*    Rewind the Temporary Error/Message File
      REWIND IERUNT

      DO WHILE (.NOT. EOF1)
         READ(IERUNT,1116,END=99,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                     MODNAM,ERRMG1,ERRMG2
C*       Sort Error Group And Find The Index
         INPFLD = ERRCD
         CALL STONUM(INPFLD,3,FNUM,IMIT)

         IF (ERRTP .EQ. 'E') THEN
            IFTL = IFTL + 1
         ELSE IF (ERRTP .EQ. 'W') THEN
            IWRN = IWRN + 1
         ELSE IF (ERRTP .EQ. 'I') THEN
            INFO = INFO + 1
         END IF

         GO TO 11
 99      EOF1 = .TRUE.
 11      CONTINUE
      END DO

 1116 FORMAT(A2,1X,A1,A3,I8,1X,A6,1X,A50,1X,A12)

C*    Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF1;
C*    This Is Needed in Order To Allow For Additional Message Writes
      BACKSPACE IERUNT

      GO TO 1000

C*    WRITE Error Message: Error Reading Temp Error Message File
 9999 CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

 1000 RETURN
      end subroutine

      SUBROUTINE SUMTBL
C***********************************************************************
C*                SUMTBL Module of the AMS/EPA Regulatory Model - AERMOD
C*                Terrain Preprocessor- AERMAP
C
C*       PURPOSE: To Print Out The Error Summary Table
C
C*       PROGRAMMER:  Jeff Wang, Roger Brode, Jayant Hardikar
C
C*       DATE:    September 29, 1995
C
C        MODIFIED:  To increase secondary message string from 8 to 
C                   12 characters, allowing for 12-character source IDs
C                   R. W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011
C
C*       INPUTS:  Error Message Temporary File
C
C*       OUTPUTS: Summary Of Errors
C
C*       CALLED FROM:  This is A Utility Program
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER J
      CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*12
      INTEGER IERRLN

C*    Variable Initialization
      MODNAM = 'SUMTBL'

C*    Write Out The Total Error Statistics
      WRITE (IOUNIT,*) '--------- Summary of Total Messages --------'
      WRITE (IOUNIT,*) ' '
      WRITE (IOUNIT,9014) IFTL
 9014 FORMAT(' A Total of ',I10,' Fatal Error Message(s)')
      WRITE (IOUNIT,9015) IWRN
 9015 FORMAT(' A Total of ',I10,' Warning Message(s)')
      WRITE (IOUNIT,9016) INFO
 9016 FORMAT(' A Total of ',I10,' Informational Message(s)')

      WRITE (IOUNIT,*) ' '

C*    Write Out All The Fatal Error Messages
      WRITE (IOUNIT,*) ' '
      WRITE (IOUNIT,*) '   ******** FATAL ERROR MESSAGES ******** '
      REWIND IERUNT
      EOF1 = .FALSE.
      J = 0
      DO WHILE (.NOT. EOF1)
         READ(IERUNT,1116,END=99,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                     MODNAM,ERRMG1,ERRMG2
         IF (ERRTP .EQ. 'E') THEN
            J = J + 1
            WRITE (IOUNIT,1117) PATH,ERRTP,ERRCD,IERRLN,
     &                          MODNAM(1:MIN(LEN_TRIM(MODNAM),6)),
     &                          ERRMG1,ERRMG2
         END IF
         GO TO 11
 99      EOF1 = .TRUE.
 11      CONTINUE
      END DO

C*    If No Fatal Error Messages, Then Write 'NONE'
      IF (J .EQ. 0) THEN
         WRITE (IOUNIT,*) '              ***  NONE  ***         '
         WRITE (IOUNIT,*) ' '
      END IF

C*    Write Out All The Warning Messages
      WRITE (IOUNIT,*) ' '
      WRITE (IOUNIT,*) '   ********   WARNING MESSAGES   ******** '
      REWIND IERUNT
      EOF1 = .FALSE.
      J = 0
      DO WHILE (.NOT. EOF1)
       READ(IERUNT,1116,END=999,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                    MODNAM(1:6),ERRMG1,ERRMG2
       IF (ERRTP .EQ. 'W') THEN
          J = J + 1
          WRITE (IOUNIT,1117) PATH,ERRTP,ERRCD,IERRLN,
     &                        MODNAM(1:MIN(LEN_TRIM(MODNAM),6)),
     &                        ERRMG1,ERRMG2
       END IF
       GO TO 111
 999     EOF1 = .TRUE.
 111     CONTINUE
      END DO

C*    If No Warning Messages, Then Write 'NONE'
      IF (J .EQ. 0) THEN
         WRITE (IOUNIT,*) '              ***  NONE  ***        '
         WRITE (IOUNIT,*) ' '
      END IF

 1116 FORMAT(A2,1X,A1,A3,I8,1X,A6,1X,A50,1X,A12)
 1117 FORMAT(1X,A2,1X,A1,A3,I8,1X,A6,':',A50,1X,A12) 

C*    Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF1;
C*    This Is Needed in Order To Allow For Additional Message Writes
      BACKSPACE IERUNT

      GO TO 1000

C*    WRITE Error Message: Error Reading Temp Error Message File
 9999 CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

 1000 RETURN
      end subroutine

      SUBROUTINE INPSUM
C***********************************************************************
C                 INPSUM Module of AERMAP
C
C        PURPOSE: Print Out The Input Data Summary, including statistics
C                 on receptors and/or sources located within gaps
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    December 7, 2006
C
C*       Revision History:
C*
C*       MODIFIED: February 9, 2009
C*                
C*                Included code to document the statistics on the 
C*                number of receptors and/or sources located within
C*                gaps, either between files due to NAD shift or 
C*                within files.  This also includes the number of
C*                receptors and/or sources with missing elevations
C*                of "filled-in" elevations based on the FILLGAPS 
C*                option.
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE

C     Variable Initializations
      MODNAM = 'INPSUM'

C     Summarize The Model Options
      WRITE(IOUNIT,9040)
 9040 FORMAT(//44X,'***     AERMAP SETUP OPTIONS SUMMARY       ***'/
     &       63(' -')/)

      WRITE(IOUNIT,9099)
 9099 FORMAT(1X,' ')

      IF (TYPDAT .EQ. 'DEM') THEN
         WRITE(IOUNIT,9041) NUMDEM
 9041    FORMAT(1X,'**This Run Includes: ',I7,'  DEM File(s)')
      ELSE IF (TYPDAT .EQ. 'NED') THEN
         WRITE(IOUNIT,9042) NUMDEM
 9042    FORMAT(1X,'**This Run Includes: ',I7,'  NED File(s)')
      END IF

      WRITE(IOUNIT,9099)
      WRITE(IOUNIT,9043) NUMREC, NUMSRC
 9043 FORMAT(1X,'**This Run Includes: ',I7,'  Receptor(s); and  '
     &       ,I7,'  Source(s)')
     
      IF (GAPSFOUND .OR. GAPSFOUND_IN) THEN
C*       Gap receptors and/or sources were found, but were assigned to DEM
C*       files on second pass, so no fatal errors issued
         WRITE(IOUNIT,9099)
         WRITE(IOUNIT,*) 'NOTE:'
         IF (GAPSFOUND .AND. .NOT.FATAL) THEN
            WRITE(IOUNIT,99043) NRGAP2, NSGAP2, TYPDAT, TYPDAT
99043       FORMAT(1X,'**This run includes: ',I7,'  Receptor(s); and  '
     &        ,I7,'  Source(s) located in GAPS BETWEEN ',A3,
     &            ' files due to NAD shifts,',
     &    /68X,'but ALL have been assigned to the closest ',A3,' file.')
         ELSE IF (GAPSFOUND .AND. FATAL) THEN
            WRITE(IOUNIT,99143) NRGAP2, NSGAP2, TYPDAT, TYPDAT
99143       FORMAT(1X,'**This run includes: ',I7,'  Receptor(s); and  '
     &     ,I7,'  Source(s) located in GAPS BETWEEN ',A3,
     &         ' files due to NAD shifts,',
     &    /68X,'but some may NOT have been assigned to another ',A3,
     &         ' file.')
         END IF
         IF (GAPSFOUND_IN) THEN
            WRITE(IOUNIT,99243) NRGAP_IN, NSGAP_IN, TYPDAT,
     &                          NRSUBS, NSSUBS
99243       FORMAT(1X,'**This run includes: ',I7,'  Receptor(s); and  '
     &        ,I7,'  Source(s) located in GAPS INSIDE ',A3,' file(s);',
     &              /'     with a total of: ',I7,'  Receptor(s); and  '
     &        ,I7,'  Source(s) assigned elevations based on subsequent',
     &            ' file(s).')
         END IF
         IF (MISSFOUND) THEN
            WRITE(IOUNIT,99343) NRMISS, NSMISS
99343       FORMAT(1X,'**This run includes: ',I7,'  Receptor(s); and  '
     &        ,I7,'  Source(s) that have been assigned missing ',
     &        'elevations (-9999.0)!')
         END IF
         IF (NRFILLED .GT. 0 .OR. NSFILLED .GT. 0) THEN
            WRITE(IOUNIT,99443) NRFILLED, NSFILLED
99443       FORMAT(1X,'**This run includes: ',I7,'  Receptor(s); and  '
     &  ,I7,'  Source(s) located in gaps and assigned ',
     &                                        'non-missing elevations ',
     &  /68X,'based on the closest nodes under the FILLGAPS option!')
         END IF
         IF (TYPDAT .EQ. 'DEM') THEN
            WRITE(IOUNIT,99543)
99543       FORMAT(1X,
     &      '**These gaps may be due to NAD conversions or use of ',
     &      'non-standard DEM files.',
     &    /'   Consider using standard DEM files with consistent NAD ',
     &      'values or NED data to avoid gaps between or within files.')
         ELSE IF (TYPDAT .EQ. 'NED') THEN
            WRITE(IOUNIT,99643)
99643       FORMAT(1X,
     &       '**These gaps may be due to NAD conversions or use of ',
     &      'non-standard NED files.',
     &     /'   Consider using standard NED files with consistent ',
     &      'NAD values to avoid gaps between or within files.')
         END IF
      END IF

      WRITE(IOUNIT,9099)
      IF (NADA .EQ. 0) THEN
         WRITE(IOUNIT,9044) NADA, XATERR-XAUSER,
     &                      YATERR-YAUSER,
     &                      XATERR, YATERR, ZATERR
 9044    FORMAT(1X,'**The Input Receptors and Sources Were ',
     &             'Assigned a NADA Value of ',I2,':  No NAD Shifts ',
     &             'Included in Calculations',//
     &            ' **The Input Receptors and Sources Are Offset:',
     &              F12.2,' meters East; ',F12.2,' meters North',
     &            /6X,'from the User-specified Anchor Point at:',
     &              F12.2,' meters East; ',F12.2,' meters North;  ',
     &             'Zone ',I3)
      ELSE
         WRITE(IOUNIT,9144) NADA, NADN(NADA), XATERR-XAUSER,
     &                      YATERR-YAUSER,
     &                      XATERR, YATERR, ZATERR
 9144    FORMAT(1X,'**The Input Receptors and Sources Were ',
     &             'Assigned a NADA Value of ',I2,':  ',A40,//
     &            ' **The Input Receptors and Sources Are Offset:',
     &              F12.2,' meters East; ',F12.2,' meters North',
     &            /6X,'from the User-specified Anchor Point at:',
     &              F12.2,' meters East; ',F12.2,' meters North;  ',
     &             'Zone ',I3)
      END IF

      WRITE(IOUNIT,9099)
      IF (.NOT.EXTRACT) THEN
         WRITE (IOUNIT,9245)
 9245    FORMAT(1X,'**Terrain heights were PROVIDED by user')
      ELSE IF (EXTRACT) THEN
         WRITE (IOUNIT,9246) TYPDAT
 9246    FORMAT(1X,'**Terrain heights were EXTRACTed from ',A3,' data')
      END IF

      WRITE(IOUNIT,9099)
      WRITE(IOUNIT,9247) TYPDAT, TYPDAT, TYPDAT, TYPDAT
 9247 FORMAT(1X,'**The Following Debug Output Files Have Been ',
     &       'Automatically Generated:'//
     &       '   DOMDETAIL.OUT - Details of User-specified Domain ',
     &       'and Relation to ',A3,' Files',/
     &       '   MAPDETAIL.OUT - Details Regarding Input ',A3,' Files',/
     &       '   MAPPARAMS.OUT - Summary of ',A3,' File Parameters and',
     &       ' ',A3,' File Adjacency')

      IF (HILLDBG .OR. RECDBG .OR. SRCDBG) THEN
         WRITE(IOUNIT,9099)
         WRITE(IOUNIT,9248)
 9248    FORMAT(1X,'**The Following User-specified Debug Output Files ',
     &          'Have Been Generated:'/)

         IF (HILLDBG) THEN
            WRITE(IOUNIT,9249) CALCHC_FILE(1:LEN_TRIM(CALCHC_FILE))
 9249       FORMAT(1X,'  Critical Hill Height Calculations:',
     &                2X,A)
         END IF

         IF (RECDBG) THEN
            WRITE(IOUNIT,9099)
            WRITE(IOUNIT,9250) RECDET_FILE(1:LEN_TRIM(RECDET_FILE))
 9250       FORMAT(1X,'  Receptor NAD Conversion Results:  ',
     &                2X,A)
            WRITE(IOUNIT,9251) TYPDAT, 
     &                         RECNDEM_FILE(1:LEN_TRIM(RECNDEM_FILE))
 9251       FORMAT(1X,'  Receptor vs ',A3,' File Locations:   ',
     &                2X,A)
            WRITE(IOUNIT,9252) RECELV_FILE(1:LEN_TRIM(RECELV_FILE))
 9252       FORMAT(1X,'  Receptor Elevation Calculations:  ',
     &                2X,A)
         END IF

         IF (SRCDBG .AND. NUMSRC .GT. 0) THEN
            WRITE(IOUNIT,9099)
            WRITE(IOUNIT,9260) SRCDET_FILE(1:LEN_TRIM(SRCDET_FILE))
 9260       FORMAT(1X,'  Source NAD Conversion Results:    ',
     &                2X,A)
            WRITE(IOUNIT,9261) TYPDAT,
     &                         SRCNDEM_FILE(1:LEN_TRIM(SRCNDEM_FILE))
 9261       FORMAT(1X,'  Source vs ',A3,' File Locations:     ',
     &                2X,A)
            WRITE(IOUNIT,9262) SRCELV_FILE(1:LEN_TRIM(SRCELV_FILE))
 9262       FORMAT(1X,'  Source Elevation Calculations:    ',
     &                2X,A)
         END IF
         WRITE(IOUNIT,9099)

      END IF

      RETURN
      END

      SUBROUTINE WRITEZ (IDM,JRECRD,ELV)
C***********************************************************************
C*       PURPOSE:  THIS SUBROUTINE WILL WRITE THE Z VALUES TO THE
C*                 SPECIFIED RECORD NUMBER OF THE DIRECT ACCESS FILE.
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       MODIFIED:   
C*                   
C*       INPUTS:  File Unit of Direct Access File, Record Number
C*
C*       OUTPUTS: Elevation at the Record Number
C*
C*       CALLED FROM:   MAIN
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER IDM, JRECRD

      DOUBLE PRECISION :: ELV

      MODNAM = 'WRITEZ'

      WRITE (IDRUNT(IDM),REC=JRECRD) ELV

      RETURN
      end subroutine


      DOUBLE PRECISION FUNCTION READZ(IDM,JRECRD)
C***********************************************************************
C*       PURPOSE:  THIS SUBROUTINE WILL READ THE Z VALUES FROM THE
C*                 SPECIFIED RECORD NUMBER OF THE DIRECT ACCESS FILE.
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       MODIFIED:   
C*                   
C*       INPUTS:  File Unit of Direct Access File, Record Number
C*
C*       OUTPUTS: Elevation at the Record Number
C*
C*       CALLED FROM:   MAIN
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER IDM, JRECRD

      DOUBLE PRECISION ELV

      MODNAM = 'READZ'

      READ (IDRUNT(IDM),REC=JRECRD) ELV
      READZ = ELV

      RETURN
      end function


      SUBROUTINE RECNUM
C***********************************************************************
C*    PURPOSE:  BASED ON A ROW & COLUMN NUMBER FROM A DEM PROFILE, THIS
C*              SUBROUTINE WILL CALCULATE A RECORD NUMBER FOR READING FROM
C*              OR WRITING TO A DIRECT ACCESS FILE
C*
C*    PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*    DATE:    September 29, 1995
C*
C*    MODIFIED:   
C*                
C*    INPUTS:  ROW NUMBER, COLUMN NUMBER OF DEM PROFILE
C*
C*    OUTPUTS: DIRECT ACCESS FILE RECORD NUMBER
C*
C*    CALLED FROM:   
C***********************************************************************

C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      INTEGER I

      DO I = 1, NX(1)-1
         JREC(1) = JREC(1) + NODES(I)
      END DO
      JREC(1) = JREC(1) + NY1(1)

      DO I = 1, NX(2)-1
         JREC(2) = JREC(2) + NODES(I)
      END DO
      JREC(2) = JREC(2) + NY1(2)

      DO I = 1, NX(2)-1
         JREC(3) = JREC(3) + NODES(I)
      END DO
      JREC(3) = JREC(3) + NY2(2)

      DO I = 1, NX(1)-1
         JREC(4) = JREC(4) + NODES(I)
      END DO
      JREC(4) = JREC(4) + NY2(1)
      
      RETURN
      end subroutine

      DOUBLE PRECISION FUNCTION GETDEM(ID,JR)
C***********************************************************************
C*               GETDEM Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Reads Elevation Data from a Direct Access File
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       MODIFIED:   
C*                   
C*       INPUTS:  
C*
C*       OUTPUTS: 
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
       
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER ID, JR
      DOUBLE PRECISION ELV

      MODNAM = 'GETDEM'

      READ (IDRUNT(ID),REC=JR) ELV
      GETDEM = ELV

      RETURN
      end function

      
      SUBROUTINE HEADER
C***********************************************************************
C*                HEADER Module of AERMAP
C
C*       PURPOSE: Control Page Feed and Header Information for
C*                Printed File Output
C
C*       PROGRAMMER: Roger Brode, James Paumier, Jayant Hardikar
C
C*       DATE:       September 29, 1995
C*
C*       MODIFIED:   Use ASCII form feed character [ACHAR(12)] for 
C*                   page feed in aermap.out file rather then
C*                   CARRIAGECONTROL='Fortran', which is not a
C*                   standard Fortran option.
C*                   Moved call to DATIME function for date/time
C*                   to MAIN program for use in other output files.
C*                   R.W. Brode, USEPA/OAQPS/AQMG, February 9, 2009
C
C*       INPUTS:  Page Number from COMMON
C
C*       OUTPUTS: Page Feed and Header
C
C*       CALLED FROM:  (This Is An Utility Program)
C***********************************************************************
C
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM
      CHARACTER*1  FFEED

C*    Variable Initializations
      MODNAM = 'HEADER'
      
C*    FFEED is ASCII form-feed character
      FFEED  = ACHAR(12)

C*    Write Header to Printed Output File
      WRITE (IOUNIT,9028) FFEED, VERSN, TITLE1(1:68), RUNDAT
      WRITE (IOUNIT,9029) TITLE2(1:68), RUNTIM
 9028 FORMAT(A1,' *** AERMAP - VERSION ',A5,' ***',4X,'*** ',A68,
     &       ' ***',8X,A8)
 9029 FORMAT(35X,'*** ',A68,' ***',8X,A8)


      RETURN
      end subroutine

      SUBROUTINE DATIME ( DCALL, TCALL )
C***********************************************************************
C                 DATIME Module
C
C        PURPOSE: Obtain the system date and time
C
C        PROGRAMMER: Jim Paumier, PES, Inc.
C
C        DATE:    April 15, 1994
C
C        MODIFIED:   Uses Fortran 90 DATE_AND_TIME routine.
C                    R.W. Brode, PES, 8/14/98
C
C        INPUTS:  none
C
C        OUTPUTS: Date and time in character format
C
C        CALLED FROM:  RUNTIME
C***********************************************************************
C
C     Variable Declarations
      IMPLICIT NONE

      CHARACTER DCALL*8, TCALL*8
      CHARACTER CDATE*8, CTIME*10, CZONE*5
      INTEGER  :: IDATETIME(8)
      INTEGER  :: IPTYR, IPTMON, IPTDAY, IPTHR, IPTMIN, IPTSEC

      DCALL = ' '
      TCALL = ' '

C     Call Fortran 90 date and time routine
      CALL DATE_AND_TIME (CDATE, CTIME, CZONE, IDATETIME)

C     Convert year to two digits and store array variables
      IPTYR  = IDATETIME(1) - 100 * INT(IDATETIME(1)/100)
      IPTMON = IDATETIME(2)
      IPTDAY = IDATETIME(3)
      IPTHR  = IDATETIME(5)
      IPTMIN = IDATETIME(6)
      IPTSEC = IDATETIME(7)

C     Write Date and Time to Character Variables, DCALL & TCALL
      WRITE(DCALL, '(2(I2.2,"/"),I2.2)' ) IPTMON, IPTDAY, IPTYR
      WRITE(TCALL, '(2(I2.2,":"),I2.2)' ) IPTHR, IPTMIN, IPTSEC

      RETURN
      end subroutine

      SUBROUTINE ERRHDL(PATHWY,MODNAM,INERTP,INERCD,INPMSG)
C***********************************************************************
C                 ERRHDL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: A General Error Handling Procedure
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    September 29, 1995
C
C        MODIFIED:  Sets upper limit on line number included in error
C                   message to avoid overflowing the field; also increased
C                   field length for last message field from 8 to 12 to
C                   accommodate 12 character source IDs.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011
C         
C        INPUTS:  Error Code, Occur Locations
C
C        OUTPUTS: Error Message, Error Statistics..etc.
C
C        CALLED FROM:  (This Is An Utility Programm)
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE

      INTEGER :: I, ILINE_PRT
      CHARACTER ERRMG1*50, PATHWY*2, INERTP*1, INERCD*3, ICODE*3,
     &          INPMSG*(*), MODNAM*(*), TMPMOD*6, TMPMSG*12
      LOGICAL FOUND

C     Variable Initializations
      IERROR = IERROR + 1
      FOUND = .FALSE.
      I = 1

C     Check for Occurrence of 'E' Error Type, and Set FATAL Switch
      IF (INERTP .EQ. 'E') THEN
         FATAL = .TRUE.
         NFATAL = NFATAL + 1
         IF (NFATAL .EQ. 999) THEN
C           Number Of Fatal Errors Has Reached Limit of 999
            ERRMG1 = 'Number of Fatal Errors Has Reached Limit of 999'
            TMPMOD = 'ERRHDL'
            ICODE  = '999'
            TMPMSG = ' '
            ILINE_PRT = MIN(ILINE,99999999)
            WRITE(IERUNT,1111) PATHWY,INERTP,ICODE,ILINE_PRT,TMPMOD,
     &                         ERRMG1,TMPMSG
            GO TO 999
         ELSE IF (NFATAL .GT. 999) THEN
C           Skip Any More Error WRITEs
            GO TO 999
         END IF
      END IF

C     Check for Occurrence of 'W' Type
      IF (INERTP .EQ. 'W') THEN
         NWARN = NWARN + 1
         IF (NWARN .EQ. 999) THEN
C           Number Of Warnings Has Reached Limit of 999
            ERRMG1 = 'Number of Warnings Has Reached Limit of 999'
            TMPMOD = 'ERRHDL'
            ICODE  = '888'
            TMPMSG = ' '
            ILINE_PRT = MIN(ILINE,99999999)
            WRITE(IERUNT,1111) PATHWY,INERTP,ICODE,ILINE_PRT,TMPMOD,
     &                         ERRMG1,TMPMSG
            GO TO 999
         ELSE IF (NWARN .GT. 999) THEN
C           Skip Any More Error WRITEs
            GO TO 999
         END IF
      END IF

C     Go To Match The Error Massage
      DO WHILE (.NOT.FOUND .AND. I.LE.IERRN)
         IF (INERCD .EQ. ERRCOD(I)) THEN
            ERRMG1 = ERRMSG(I)
            FOUND = .TRUE.
         END IF
         I = I + 1
      END DO

      IF (.NOT. FOUND) THEN
         WRITE(ERRMG1,1001)
 1001    FORMAT('SYSTEM ERROR: MESSAGE NOT FOUND FOR THIS NUMBER!')
      END IF

C --- Set upper limit on ILINE to avoid write error
      ILINE_PRT = MIN(ILINE,99999999)
C     Write Out The Error Message
      WRITE(IERUNT,1111) PATHWY,INERTP,INERCD,ILINE_PRT,
     &                   MODNAM(1:MIN(LEN_TRIM(MODNAM),6)),ERRMG1,
     &                   INPMSG(1:MIN(LEN_TRIM(INPMSG),12))
 1111 FORMAT(A2,1X,A1,A3,I8,1X,A6,':',A50,1X,A12)

 999  RETURN
      end subroutine


      SUBROUTINE SRCOUT
C***********************************************************************
C                 SRCOUT Module of AERMAP Model
C
C        PURPOSE: To process SOurce Pathway card images
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    April 15, 1997
C
C        INPUTS:  Pathway (SO) and Keyword
C
C        OUTPUTS: Source Location Cards
C
C        CALLED FROM:   MAIN
C***********************************************************************
        
C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM
      INTEGER I, ISRC, NUSER

C     Variable Initializations
      MODNAM = 'SRCOUT'

C*    Write Header Information to Output File
      WRITE (ISUNIT,9240,ERR=99) VERSN, RUNDAT, RUNTIM
      WRITE (ISUNIT,9248,ERR=99) TITLE1(1:LEN_TRIM(TITLE1))
      WRITE (ISUNIT,9248,ERR=99) TITLE2(1:LEN_TRIM(TITLE2))
      IF (TYPDAT .EQ. 'DEM') THEN
         WRITE (ISUNIT,9242,ERR=99) NUMDEM
      ELSE IF (TYPDAT .EQ. 'NED') THEN
         WRITE (ISUNIT,9244,ERR=99) NUMDEM
      END IF
      WRITE (ISUNIT,9247,ERR=99) NUMSRC
      IF (GOTDOMFLG) THEN
         WRITE (ISUNIT,9248,ERR=99) DOMCARD(1:LEN_TRIM(DOMCARD))
         IF (DOMFIX) THEN
            WRITE(ISUNIT,9250,ERR=99)
            WRITE(ISUNIT,9248,ERR=99) DOMADJ(1:LEN_TRIM(DOMADJ))
         END IF
      ELSE
         WRITE (ISUNIT,9249,ERR=99)
      END IF
      WRITE (ISUNIT,9248,ERR=99) ANCHCRD(1:LEN_TRIM(ANCHCRD))
      IF (HGTCARD(1:11) .NE. '           ') THEN
         WRITE (ISUNIT,9248,ERR=99) HGTCARD(1:LEN_TRIM(HGTCARD))
      ELSE
         WRITE (ISUNIT,9246,ERR=99)
      END IF
      IF (TYPDAT .EQ. 'NED') THEN
C*       Check for NED files with user-specified elevation units
         NUSER = 0
         DO I = 1, NUMDEM
            IF (L_UserElevUnits(I)) THEN
               NUSER = NUSER + 1
            END IF
         END DO
         IF (NUSER .GT. 0) THEN
            WRITE(ISUNIT,9255,ERR=99) NUSER
         END IF
      END IF
      IF (NSMISS .GT. 0) THEN
         WRITE (ISUNIT,9260,ERR=99) NSMISS
      END IF
      
9240  FORMAT('** AERMAP - VERSION ',A5,T72,A8,/
     &       '**',T72,A8)
9242  FORMAT('** A total of ',I6,'  DEM files were used')
9244  FORMAT('** A total of ',I6,'  NED files were used')
9247  FORMAT('** A total of ',I6,'  sources were processed')
9246  FORMAT('** Terrain heights were extracted by default')
9248  FORMAT('** ',A)
9249  FORMAT('** No user-specifed DOMAIN; all available data used')
9250  FORMAT('** DOMAINLL was adjusted for Negative West Longitude:')
9255  FORMAT('** User-specified elevation units for  ',I5,' NED files')
9260  FORMAT('** NOTE:  A total of ',I7,'  sources were located',
     &          ' within data gaps',
     &     /,'**        and assigned missing elevations (-9999.0)!')

C --- Check for gap sources and issue applicable warnings
      IF (NSGAP2 .GT. 0 .OR. NSGAP_IN .GT. 0 .OR.
     &    NSMISS .GT. 0 .OR. NSFILLED .GT. 0) THEN
         WRITE(ISUNIT,99040)
99040    FORMAT('**',/,'** NOTE:')
      END IF
      IF (NSGAP2 .GT. 0) THEN
         WRITE(ISUNIT,99043) NSGAP2
99043    FORMAT('**   This run includes: ',I7,'  Source(s) ',
     &   'located in GAPS BETWEEN files due to NAD shifts;',
     &  /'**',41X,'but ALL have been assigned to the closest file.')
      END IF
      IF (NSGAP_IN .GT. 0) THEN
         WRITE(ISUNIT,99243) NSGAP_IN, NSSUBS
99243    FORMAT('**   This run includes: ',I7,'  Source(s) ',
     &   'located in GAPS INSIDE file(s);',
     &  /'**     with a total of: ',I7,'  Source(s) assigned ',
     &   'elevations based on subsequent file(s).')
      END IF
      IF (NSMISS .GT. 0) THEN
         WRITE(ISUNIT,99343) NSMISS
99343    FORMAT('**   This run includes: ',I7,'  Source(s) ',
     &   'assigned missing elevations (-9999.0)!')
      END IF
      IF (NSFILLED .GT. 0) THEN
         WRITE(ISUNIT,99443) NSFILLED
99443    FORMAT('**   This run includes: ',I7,'  Source(s) ',
     &  'located in gaps and assigned non-missing elevations',
     & /'**',41X,'based on the closest nodes under the ',
     &                                      'FILLGAPS option!')
      END IF

      WRITE (ISUNIT,*,ERR=99) ' '

      IF (.NOT.RUNERR) THEN
C*       Write Elevation Units = Meters
         WRITE (ISUNIT,9252)
9252     FORMAT ('SO ELEVUNIT METERS')

         DO ISRC = 1, NUMSRC
            WRITE (ISUNIT,9254,ERR=99) SRCID(ISRC), SRCTYP(ISRC),
     &                          AXS(ISRC), AYS(ISRC), AZS(ISRC)
9254        FORMAT('SO LOCATION  ',A12,2X,A8,3(2X,F12.2))
         END DO
      END IF

      GO TO 999

 99   CALL ERRHDL(PATH,MODNAM,'E','520','SOURCLOC')

 999  CONTINUE

      RETURN
      end subroutine

      SUBROUTINE WRITER
C***********************************************************************
C*               WRITER Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Write Out Receptor Data to Output File
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       MODIFIED:   
C*                   
C*       INPUTS:  Receptor Data
C*
C*       OUTPUTS: Written to Output File
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
       
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER*12 MODNAM

      SAVE
      INTEGER :: I
      LOGICAL NOPATH, NOKEY      
      CHARACTER RDFRM*20
      CHARACTER INPFLD*2, PATHWY(5)*2
      INTERFACE
         SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
            CHARACTER (LEN=2), INTENT(IN) :: INPFLD
            CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
            INTEGER, INTENT(IN) :: IPN
            LOGICAL, INTENT(OUT) :: NOPATH
         END SUBROUTINE EXPATH
      END INTERFACE

C*    Variable Initializations
      MODNAM = 'WRITER'
      EOF1 = .FALSE.
      ILINE = 0
      
C*    Rewind Input File and Read Data
      REWIND (INUNIT)

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')

C*    LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF1)

C*       Increment the Line Counter
         ILINE = ILINE + 1

C        READ Record to Buffers, as A'num' and 'num'A1 where 'num' = ISTRG.
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1
         READ (INUNIT,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

C*       Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C*       Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

C*       Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

C*       If Blank Line, Then CYCLE to Next Card
         IF (BLINE) THEN
            GO TO 11
         END IF

C*       Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
            GO TO 11
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         PATHWY(1) = 'CO'
         PATHWY(2) = 'SO'
         PATHWY(3) = 'RE'
         PATHWY(4) = 'OU'
         PATHWY(5) = '**'
         CALL EXPATH(FIELD(1),PATHWY,5,NOPATH)

C*       For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH .OR. PATH .EQ. '**') THEN
            GO TO 11
         END IF

C*       Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

C*       When Keyword Is Wrong, Save Keyword and Skip To The Next Record
         IF (NOKEY) THEN
            PKEYWD = KEYWRD
            GO TO 11
         END IF

         PPATH  = PATH
         IPPNUM = IPNUM

C*       Process Receptor Input Card
         IF (PATH .EQ. 'RE') THEN
C*          Process REceptor Pathway Cards                  ---   CALL RECOUT
            CALL RECOUT
         END IF

C*       Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

C*       Check for 'RE FINISHED' Card.  Exit DO WHILE Loop By Branching
C*       to Statement 999 in Order to Avoid Reading a ^Z "End of File"
C*       Marker That May Be Present For Some Editors.
         IF (PATH .EQ. 'RE' .AND. KEYWRD .EQ. 'FINISHED') THEN
            GO TO 999
         END IF

         GO TO 11
 999     EOF1 = .TRUE.
 11      CONTINUE
      END DO

      RETURN
      end subroutine


      SUBROUTINE RECOUT
C***********************************************************************
C                 RECOUT Module of AERMAP Model
C
C        PURPOSE: To process REceptor Pathway card images
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C
C        DATE:    September 29, 1995
C
C        INPUTS:  Pathway (RE) and Keyword
C
C        OUTPUTS: Receptor Arrays
C
C        CALLED FROM:   WRITER
C***********************************************************************
        
C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM
      INTEGER I, IREC, NUSER

C     Variable Initializations
      MODNAM = 'RECOUT'
      
C*    Reinitialize IRSTAT array to 0      
      IRSTAT = 0

C     Skip output of data for RUNERR = .TRUE.
      IF (RUNERR .AND. KEYWRD .NE. 'STARTING') GO TO 999

      IF (KEYWRD .EQ. 'STARTING') THEN

C*       Write Header Information to Output File
         WRITE (IRUNIT,9240,ERR=99) VERSN, RUNDAT, RUNTIM
         WRITE (IRUNIT,9248,ERR=99) TITLE1(1:LEN_TRIM(TITLE1))
         WRITE (IRUNIT,9248,ERR=99) TITLE2(1:LEN_TRIM(TITLE2))
         IF (TYPDAT .EQ. 'DEM') THEN
            WRITE (IRUNIT,9242,ERR=99) NUMDEM
         ELSE IF (TYPDAT .EQ. 'NED') THEN
            WRITE (IRUNIT,9244,ERR=99) NUMDEM
         END IF
         WRITE (IRUNIT,9247,ERR=99) NUMREC
         IF (GOTDOMFLG) THEN
            WRITE (IRUNIT,9248,ERR=99) DOMCARD(1:LEN_TRIM(DOMCARD))
            IF (DOMFIX) THEN
               WRITE(IRUNIT,9250,ERR=99)
               WRITE(IRUNIT,9248,ERR=99) DOMADJ(1:LEN_TRIM(DOMADJ))
            END IF
         ELSE
            WRITE (IRUNIT,9249,ERR=99)
         END IF
         WRITE (IRUNIT,9248,ERR=99) ANCHCRD(1:LEN_TRIM(ANCHCRD))
         IF (HGTCARD(1:11) .NE. '           ') THEN
            WRITE (IRUNIT,9248,ERR=99) HGTCARD(1:LEN_TRIM(HGTCARD))
         ELSE
            WRITE (IRUNIT,9246,ERR=99)
         END IF
         IF (TYPDAT .EQ. 'NED') THEN
C*          Check for NED files with user-specified elevation units
            NUSER = 0
            DO I = 1, NUMDEM
               IF (L_UserElevUnits(I)) THEN
                  NUSER = NUSER + 1
               END IF
            END DO
            IF (NUSER .GT. 0) THEN
               WRITE(IRUNIT,9255,ERR=99) NUSER
            END IF
         END IF

9240     FORMAT('** AERMAP - VERSION ',A5,T72,A8,/
     &          '**',T72,A8)
9242     FORMAT('** A total of ',I7,'  DEM files were used')
9244     FORMAT('** A total of ',I7,'  NED files were used')
9247     FORMAT('** A total of ',I7,'  receptors were processed')
9246     FORMAT('** Terrain heights were extracted by default')
9248     FORMAT('** ',A)
9249     FORMAT('** No user-specifed DOMAIN; all available data used')
9250     FORMAT('** DOMAINLL was adjusted for Negative West Longitude:')
9255   FORMAT('** User-specified elevation units for  ',I5,' NED files')

C ---    Check for gap receptors and issue applicable warnings
         IF (NRGAP2 .GT. 0 .OR. NRGAP_IN .GT. 0 .OR.
     &       NRMISS .GT. 0 .OR. NRFILLED .GT. 0) THEN
            WRITE(IRUNIT,99040)
99040       FORMAT('**',/,'** NOTE:')
         END IF
         IF (NRGAP2 .GT. 0) THEN
            WRITE(IRUNIT,99043) NRGAP2
99043       FORMAT('**   This run includes: ',I7,'  Receptor(s) ',
     &      'located in GAPS BETWEEN files due to NAD shifts;',
     &     /'**',43X,'but ALL have been assigned to the closest file.')
         END IF
         IF (NRGAP_IN .GT. 0) THEN
            WRITE(IRUNIT,99243) NRGAP_IN, NRSUBS
99243       FORMAT('**   This run includes: ',I7,'  Receptor(s) ',
     &      'located in GAPS INSIDE file(s);',
     &     /'**     with a total of: ',I7,'  Receptor(s) assigned ',
     &      'elevations based on subsequent file(s).')
         END IF
         IF (NRMISS .GT. 0) THEN
            WRITE(IRUNIT,99343) NRMISS
99343       FORMAT('**   This run includes: ',I7,'  Receptor(s) ',
     &      'assigned missing elevations and hill heights (-9999.0)!')
         END IF
         IF (NRFILLED .GT. 0) THEN
            WRITE(IRUNIT,99443) NRFILLED
99443       FORMAT('**   This run includes: ',I7,'  Receptor(s) ',
     &     'located in gaps and assigned non-missing elevations',
     &    /'**',43X,'based on the closest nodes under the ',
     &                                         'FILLGAPS option!')
         END IF

         WRITE (IRUNIT,*,ERR=99) ' '

C        Skip output of data for RUNERR = .TRUE.
         IF (RUNERR) GO TO 999

C*       Write Elevation Units = Meters
         WRITE (IRUNIT,9252,ERR=99)
9252     FORMAT ('RE ELEVUNIT METERS')

C        Initialize Counters and Set Status Switch
         IREC = 0
         INNET = 0
         NUMREC = 0
         NUMARC = 0
         IRXR = 0
         IRYR = 0
         IRZE = 0
         IRHZ = 0
         IRZF = 0
         PXSOID = ' '
         PESOID = ' '
         ISTA = .FALSE.
         IRSTAT(1) = IRSTAT(1) + 1
         IF (IRSTAT(1) .NE. 1) THEN
C           Error Message: Repeat Starting In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
C        Flush the Working Arrays (1:NREC)
         ZETMP1 = 0.0D0
         ZETMP2 = 0.0D0
         ZFTMP1 = 0.0D0
         ZFTMP2 = 0.0D0
      ELSE IF (KEYWRD .EQ. 'GRIDCART') THEN
C        Set Status Switch
         IRSTAT(2) = IRSTAT(2) + 1
C        Process Cartesian Grid Receptor Network            ---   CALL WRITGC
         CALL WRITGC
      ELSE IF (KEYWRD .EQ. 'GRIDPOLR') THEN
C        Set Status Switch
         IRSTAT(3) = IRSTAT(3) + 1
C        Process Polar Receptor Network                     ---   CALL WRITGP
         CALL WRITGP
      ELSE IF (KEYWRD .EQ. 'DISCCART') THEN
C        Set Status Switch
         IRSTAT(4) = IRSTAT(4) + 1
C        Process Discrete Cartesian Receptor Locations      ---   CALL WRITDC
         CALL WRITDC
      ELSE IF (KEYWRD .EQ. 'DISCPOLR') THEN
C        Set Status Switch
         IRSTAT(5) = IRSTAT(5) + 1
C        Process Discrete Polar Receptor Locations          ---   CALL WRITDP
         CALL WRITDP
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         IRSTAT(8) = IRSTAT(8) + 1

C*       DO NOT ECHO USER-SPECIFIED ELEVUNIT

      ELSE IF (KEYWRD .EQ. 'EVALCART') THEN
C        Set Status Switch
         IRSTAT(11) = IRSTAT(11) + 1
C        Process Discrete Cartesian Receptor Locations      ---   CALL EVCART
         CALL WRITEV

      ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
C        Set Status Switch
         IRSTAT(12) = IRSTAT(12) + 1
C        Save ILINE as ISAVE
         ILSAVE = ILINE
C        Process the Included Receptor File                 ---   CALL INCLUD
         CALL WRITE_INCLUD
C        Retrieve ILINE From ISAVE         
         ILINE = ILSAVE
         
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IRSTAT(20) = IRSTAT(20) + 1

C        Set Total Number of Receptors for This Run, NUMREC
         NUMREC = IRXR

      ELSE
C        Write Error Message:  Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

      GO TO 999

 99   CALL ERRHDL(PATH,MODNAM,'E','520','RECEPTOR')

 999  RETURN
      end subroutine

      SUBROUTINE WRITE_INCLUD
C***********************************************************************
C*                WRITE_INCLUD Module of ISCST3 Model
C*
C*       PURPOSE: To write input records from an external receptor/source file using the
C*                INCLUDED keyword.
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 30, 1995
C*
C        MODIFIED:   To remove reference to obsolete TG pathway inherited
C                    from ISCST3 code.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 07/26/2007
C
C*                   
C*       INPUTS: 
C*
C*       OUTPUTS:
C*               
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
        
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER*12 MODNAM

      SAVE
      INTEGER :: I, ILREAL
      LOGICAL NOPATH, NOKEY      
      CHARACTER RDFRM*20
      CHARACTER INPFLD*2, PATHWY(5)*2
      INTERFACE
         SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
            CHARACTER (LEN=2), INTENT(IN) :: INPFLD
            CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
            INTEGER, INTENT(IN) :: IPN
            LOGICAL, INTENT(OUT) :: NOPATH
         END SUBROUTINE EXPATH
      END INTERFACE

C*    Variable Initializations
      MODNAM = 'WRITE_INCLUD'
      EOF = .FALSE.
      ILINE = 1

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')
      

      IF (IFC .EQ. 3) THEN
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. ILEN_FLD-1) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            INCFIL = RUNST1(LOCB(3):LOCE(3))
            OPEN (INCUNT,FILE=INCFIL,ACTION='READ',STATUS='OLD',ERR=99)
         ELSE
C           WRITE Error Message:  INCFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
            GO TO 1002
         END IF

      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message         ! No Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      GO TO 1001

C     Write Out Error Message for File OPEN Error
99    CALL ERRHDL(PATH,MODNAM,'E','500','INCFILE ')
      GO TO 1002

1001  CONTINUE

      
C     LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF)

C        Increment the Line Counter.  It was Initially Set to 1, to Handle
C        the Code in Subroutine DEFINE
         ILINE = ILINE + 1
         ILREAL = ILREAL + 1

C        READ Record to Buffers, as A'num' and 'num'A1 where 'num' = ISTRG.
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1
         READ (INCUNT,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

C        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C        Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

         IF (ILREAL .EQ. 1) ILINE = ILINE -1

C        Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

C        If Blank Line, Then CYCLE to Next Card
         IF (BLINE) GO TO 11

C        Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
C           Skip record with NO ECHO in INCLUDED file, but leave ECHO "on"
            GO TO 11
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         PATHWY(1) = 'CO'
         PATHWY(2) = 'SO'
         PATHWY(3) = 'RE'
         PATHWY(4) = 'OU'
         PATHWY(5) = '**'
         CALL EXPATH(FIELD(1),PATHWY,5,NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH) THEN
C           Skip error message         
            PATH = PPATH
            GO TO 11
         ELSE IF (PATH .EQ. '**') THEN
            GO TO 11
         END IF

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

         IF (NOKEY) THEN
            PKEYWD = KEYWRD
            GO TO 11
         END IF

         IF (KEYWRD .EQ. 'GRIDCART') THEN
C           Set Status Switch
            IRSTAT(2) = IRSTAT(2) + 1
C           Process Cartesian Grid Receptor Network            ---   CALL WRITGC
            CALL WRITGC
         ELSE IF (KEYWRD .EQ. 'GRIDPOLR') THEN
C           Set Status Switch
            IRSTAT(3) = IRSTAT(3) + 1
C           Process Polar Receptor Network                     ---   CALL WRITGP
            CALL WRITGP
         ELSE IF (KEYWRD .EQ. 'DISCCART') THEN
C           Set Status Switch
            IRSTAT(4) = IRSTAT(4) + 1
C           Process Discrete Cartesian Receptor Locations      ---   CALL WRITDC
            CALL WRITDC
         ELSE IF (KEYWRD .EQ. 'DISCPOLR') THEN
C           Set Status Switch
            IRSTAT(5) = IRSTAT(5) + 1
C           Process Discrete Polar Receptor Locations          ---   CALL WRITDP
            CALL WRITDP
         ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C           Set Status Switch
            IRSTAT(8) = IRSTAT(8) + 1

C*          DO NOT ECHO USER-SPECIFIED ELEVUNIT

         ELSE IF (KEYWRD .EQ. 'EVALCART') THEN
C           Set Status Switch
            IRSTAT(11) = IRSTAT(11) + 1
C           Process Discrete Cartesian Receptor Locations      ---   CALL EVCART
            CALL WRITEV

         ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C           Set Status Switch
            IRSTAT(20) = IRSTAT(20) + 1

C           Set Total Number of Receptors for This Run, NUMREC
            NUMREC = IRXR

         ELSE
C           Write Error Message:  Invalid Keyword for This Pathway
            CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

         GO TO 11
 999     EOF = .TRUE.
 11      CONTINUE

      END DO
      EOF = .FALSE.

C     Close the INCLUDED File
      CLOSE (INCUNT)
      
1002  RETURN
      END

      SUBROUTINE WRITGC
C***********************************************************************
C                 WRITGC Module of AERMAP
C
C        PURPOSE: Processes Cartesian Grid Receptor Network Output
C
C        PROGRAMMER:  Jayant Hardikar, Roger Brode
C
C        DATE:    September 29, 1995
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network Outputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM
      INTEGER I

      INTEGER IDST

      DOUBLE PRECISION :: TEMPP(6)

C     Variable Initializations
      MODNAM = 'WRITGC'

      IDST = 0
    
C     READ in the Netid and Nettype
      IF (IFC .LT. 3) THEN
C        Write Error Message: Missing Data Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      END IF
      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'XYINC' .OR. NETIDT.EQ.'XPNTS' .OR.
     &    NETIDT.EQ.'YPNTS' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'FLAG'  .OR. NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)         
         IDST = 4         
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
         IDST = 5
      ELSE IF (NEWID .AND. NETIDT.NE.' ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
         IDST = 5
C        The Keyword Counter
         INNET = INNET + 1
         IF (INNET .GT. NNET) THEN
C           WRITE Error Message:  Too Many Networks
            WRITE(DUMMY,'(I8)') NNET
            CALL ERRHDL(PATH,MODNAM,'E','224',DUMMY)
            RECERR = .TRUE.
            GO TO 999
         END IF
         INCSET = 0
         IXYSET = 0
         IEVSET = 0
         IFGSET = 0
      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',PNETID)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
      
C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))
      
C        Initialize Logical Control Variables
         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
C        Set Counters of Calculation Field
         ICOUNT = 0
         JCOUNT = 0
         IZE = 0
         IZF = 0
         IDC1 = IRXR
         IRXT = 0
         IRYT = 0

      ELSE IF (KTYPE .EQ. 'XYINC') THEN

C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))

C        Check for Location of Secondary Keyword, XYINC
         DO I = 1, IFC
            IF (FIELD(I) .EQ. 'XYINC') THEN
               ISC = I + 1
            END IF
         END DO

         CALL STODBL(FIELD(ISC + 2-1),ILEN_FLD,TEMPP(2),MITL)
C        Check The Numerical Field
         IF (MITL .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF

         CALL STODBL(FIELD(ISC + 5-1),ILEN_FLD,TEMPP(5),MITL)
C        Check The Numerical Field
         IF (MITL .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF

         IRXT = IDNINT(TEMPP(2))
         IRYT = IDNINT(TEMPP(5))
         
      
      ELSE IF (KTYPE.EQ.'XPNTS' .OR. KTYPE.EQ.'YPNTS') THEN

C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))

C*       Increment the Number of Receptors in this Network         
         IF (KTYPE .EQ. 'XPNTS') THEN
            IRXT = IRXT + IFC - IDST + 1
         END IF

         IF (KTYPE .EQ. 'YPNTS') THEN
            IRYT = IRYT + IFC - IDST + 1
         END IF
           

      ELSE IF (KTYPE .EQ. 'ELEV') THEN

C*       If Input Units Were in Feet, Convert to Meters and
C*       Write Card to Output File
C*       Don't echo runstream inputs for ELEV

      ELSE IF (KTYPE .EQ. 'FLAG') THEN

C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))

      ELSE IF (KTYPE .EQ. 'END') THEN
      
C*       Determine the Number of Receptors in this Network      
         IRLAS = IRXT*IRYT
      
C*       Write ELEV/HILL Data                            ---   CALL WRITRZ
         CALL WRITRZ
            
C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))
      
         IEND = .TRUE.
         ISTA = .FALSE.
         NEWID = .TRUE.

      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',NETIDT)
         RECERR = .TRUE.
         GO TO 999

      END IF
      PNETID = NETIDT

 999  RETURN
      end subroutine


      SUBROUTINE WRITGP
C***********************************************************************
C                 WRITGP Module of ISC2 Model
C
C        PURPOSE: Processes Polar Grid Receptor Network Inputs
C
C        PROGRAMMER:  Jayant Hardikar, Roger Brode
C
C        DATE:    September 29, 1995
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Receptor Network Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I, K
      INTEGER IORSET, IXRSET, IDRSET, IGRSET, IDST

      DOUBLE PRECISION :: TEMPP(6)

C     Variable Initializations
      MODNAM = 'WRITGP'

      IDST = 0

C     READ in the Netid and Nettype
      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'ORIG' .OR. NETIDT.EQ.'DIST' .OR.
     &    NETIDT.EQ.'DDIR' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'FLAG' .OR. NETIDT.EQ.'GDIR' .OR.
     &    NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)
         IDST = 4                  
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
         IDST = 5
      ELSE IF (NEWID .AND. NETIDT.NE.'    ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
         IDST = 5
C        The Keyword Counter
         INNET = INNET + 1
         IF (INNET .GT. NNET) THEN
C           WRITE Error Message:  Too Many Networks
            WRITE(DUMMY,'(I8)') NNET
            CALL ERRHDL(PATH,MODNAM,'E','224',DUMMY)
            RECERR = .TRUE.
            GO TO 999
         END IF
         IORSET = 0
         IXRSET = 0
         IDRSET = 0
         IGRSET = 0
         IEVSET = 0
         IFGSET = 0
      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',PNETID)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
      
C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))

         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
         ICOUNT = 0
         JCOUNT = 0
         IZE = 0
         IZF = 0
         IDC1 = IRXR
         IRXT = 0
         IRYT = 0

      ELSE IF (KTYPE .EQ. 'ORIG') THEN

C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))

      ELSE IF (KTYPE .EQ. 'DIST') THEN
         IRXT = IRXT + IFC - IDST + 1
C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))

      ELSE IF (KTYPE .EQ. 'GDIR') THEN

C        Check for the Location of the Secondary Keyword, GDIR
         DO I = 1, IFC
            IF (FIELD(I) .EQ. 'GDIR') THEN
               ISC = I + 1
            END IF
         END DO

C*       Input Numerical Values
         DO K = 1, 3
            CALL STODBL(FIELD(ISC + K-1),ILEN_FLD,TEMPP(K),MITL)
C*          Check The Numerical Field
            IF (MITL .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            END IF
         END DO
       
         JCOUNT = NINT(TEMPP(1))
         IRYT = JCOUNT

C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))
         
      ELSE IF (KTYPE .EQ. 'DDIR') THEN
         IRYT = IRYT + IFC - IDST + 1
         
C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))

      ELSE IF (KTYPE .EQ. 'ELEV') THEN

C*       Don't echo runstream inputs for ELEV

      ELSE IF (KTYPE .EQ. 'FLAG') THEN

C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))

      ELSE IF (KTYPE .EQ. 'END') THEN

C*       Determine the Number of Receptors in this Network      
         IRLAS = IRXT*IRYT
      
C*       Write ELEV/HILL Data                            ---   CALL WRITRZ
         CALL WRITRZ
            
C*       Write Card to Output File
         WRITE (IRUNIT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))
      
         IEND = .TRUE.
         ISTA = .FALSE.
         NEWID = .TRUE.

      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',NETIDT)
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT
      
 999  RETURN
      end subroutine


      SUBROUTINE WRITRZ
C***********************************************************************
C                 WRITRZ Module of AERMAP
C
C        PURPOSE: Write Out Elevated Terrain Inputs for Receptor Network
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C
C        DATE:    September 29, 1995
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Elevated Terrain Input for a Receptor Network
C
C        CALLED FROM:   RECART
C                       REPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM
      INTEGER I, J

      INTEGER IFIR, ILAS, NLIN, NITM, IBEG, IFIN, LEFTOV, ILN

C     Variable Initializations
      MODNAM = 'WRITRZ'

C     Set counters for first and last receptors of this network
      IFIR = IRXR + 1
      ILAS = IFIR + IRLAS - 1

C     Compute number of lines to output per radial/row and number leftover
C      NLIN = INT( (IRXT-1) / 6) + 1
C      LEFTOV = MOD (IRXT,6)
      NLIN = INT( (IRXT-1) / 5) + 1
      LEFTOV = MOD (IRXT,5)

C     Output Receptor Elevations
C     Loop through radials/rows and then through # of lines per radial/row
      DO J = 1, IRYT
         DO ILN = 1, NLIN
            IF (ILN .EQ. NLIN .AND. LEFTOV .NE. 0) THEN
               NITM = LEFTOV
            ELSE
C               NITM = 6
               NITM = 5
            END IF
            IF (ILN .EQ. 1 .AND. J .EQ. 1) THEN
               IBEG = IFIR
            ELSE
               IBEG = IFIN + 1
            END IF
            IFIN = IBEG + NITM -1

            WRITE (IRUNIT,3016) KEYWRD,NETIDT,J, (AZELEV(I),I=IBEG,IFIN)
         END DO
      END DO


C     Output Receptor Hill Height Scales
C     Loop through radials/rows and then through # of lines per radial/row
      DO J = 1, IRYT
         DO ILN = 1, NLIN
            IF (ILN .EQ. NLIN .AND. LEFTOV .NE. 0) THEN
               NITM = LEFTOV
            ELSE
C               NITM = 6
               NITM = 5
            END IF
            IF (ILN .EQ. 1 .AND. J .EQ. 1) THEN
               IBEG = IFIR
            ELSE
               IBEG = IFIN + 1
            END IF
            IFIN = IBEG + NITM -1

            WRITE (IRUNIT,3017) KEYWRD,NETIDT,J, (AZHILL(I),I=IBEG,IFIN)
         END DO
      END DO

C 3016 FORMAT(3X,A8,1X,A8,1X,'ELEV ',I4,6(1X,F8.1))
C 3017 FORMAT(3X,A8,1X,A8,1X,'HILL ',I4,6(1X,F8.1))
 3016 FORMAT(3X,A8,1X,A8,1X,'ELEV ',I4,5(1X,F8.1))
 3017 FORMAT(3X,A8,1X,A8,1X,'HILL ',I4,5(1X,F8.1))

C     Set Counters
      IRXR = IRXR + IRLAS
      IRYR = IRXR
      IRZE = IRXR
      IRZF = IRXR
      IRHZ = IRXR

 999  RETURN
      end subroutine


      SUBROUTINE WRITDC
C***********************************************************************
C                 WRITDC Module of ISC2 Model
C
C        PURPOSE: Processes Discrete Cartesian Receptor Location Inputs
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C
C        DATE:    September 29, 1995
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Discrete Cartesian Receptor Location Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I1, I2, I3, I4, I5

C     Variable Initializations
      MODNAM = 'WRITDC'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF
      I5 = IRHZ

C*    Write Card to Output File
      IF (FLGPOL) THEN
         WRITE(IRUNIT,3030) AXR(I1+1),AYR(I2+1),
     &        AZELEV(I3+1), AZHILL(I5+1),AZFLAG(I4+1)
      ELSE
         WRITE(IRUNIT,3031) AXR(I1+1),AYR(I2+1),
     &        AZELEV(I3+1), AZHILL(I5+1)     
      END IF      
      
3030  FORMAT(3X,'DISCCART ',2(1X,F12.2),3(1X,F10.2))
3031  FORMAT(3X,'DISCCART ',2(1X,F12.2),2(1X,F10.2))

      IRXR = I1 + 1
      IRYR = I2 + 1
      IRZE = I3 + 1
      IRZF = I4 + 1
      IRHZ = I5 + 1

 999  RETURN
      end subroutine

      SUBROUTINE WRITEV
C***********************************************************************
C                 WRITEV Module of ISC2 Model
C
C        PURPOSE: Processes EVALCART Cartesian Receptor Location Inputs
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C
C        DATE:    September 29, 1995
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Discrete Cartesian Receptor Location Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER I1, I2, I3, I4, I5

C     Variable Initializations
      MODNAM = 'WRITEV'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF
      I5 = IRHZ

C*    Write Card to Output File
         WRITE(IRUNIT,3030) AXR(I1+1),AYR(I2+1),
     &        AZELEV(I3+1), AZHILL(I5+1),AZFLAG(I4+1),
     &        ARCID(NDXARC(I1+1)), RECNAM(I1+1)
      
3030  FORMAT(3X,'EVALCART ',2(1X,F12.2),3(1X,F10.2),1X,A8,1X,A8)

      IRXR = I1 + 1
      IRYR = I2 + 1
      IRZE = I3 + 1
      IRZF = I4 + 1
      IRHZ = I5 + 1

 999  RETURN
      end subroutine


      SUBROUTINE WRITDP
C***********************************************************************
C                 WRITDP Module of AERMAP
C
C        PURPOSE: Processes Discrete Polar Receptor Location Inputs
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C
C        DATE:    September 29, 1995
C
C        MODIFIED:   To increase maximum length of source IDs from
C                    8 to 12 characters.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Discrete Polar Receptor Location Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      CHARACTER SOID*12
      INTEGER I1, I2, I3, I4, I5, IMIT
      DOUBLE PRECISION :: RANGE, DIRECT

C     Variable Initializations
      MODNAM = 'WRITDP'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF
      I5 = IRHZ

C*    First check for length of SRCID field <=12
      IF ((LOCE(3)-LOCB(3)) .LE. 11) THEN
C*       Retrieve Source ID Character Substring
         SOID = FIELD(3)
      ELSE
C*       WRITE Error Message:  Source ID Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','206',FIELD(3)(1:12))
         RECERR = .TRUE.
         GO TO 999
      END IF
      
      CALL STODBL(FIELD(4),ILEN_FLD,RANGE,IMIT)
C     Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STODBL(FIELD(5),ILEN_FLD,DIRECT,IMIT)
C     Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF
      
C*    Write Card to Out File
      IF (FLGPOL) THEN
         WRITE(IRUNIT,3040) SOID, RANGE,
     &        DIRECT, AZELEV(I3+1), AZHILL(I5+1),
     &        AZFLAG(I4+1)
      ELSE
         WRITE(IRUNIT,3041) SOID, RANGE,
     &        DIRECT, AZELEV(I3+1), AZHILL(I5+1)
      END IF      
      
3040  FORMAT(3X,'DISCPOLR ',A12,1X,2(1X,F12.2),3(1X,F10.2))
3041  FORMAT(3X,'DISCPOLR ',A12,1X,2(1X,F12.2),2(1X,F10.2))


      IRXR = I1 + 1
      IRYR = I2 + 1
      IRZE = I3 + 1
      IRZF = I4 + 1
      IRHZ = I5 + 1
 
 999  RETURN
      end subroutine

C***** Begin New Code for Allocatable Arrays


      SUBROUTINE PRESET
C***********************************************************************
C                 PRESET Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Preprocesses SETUP Information to Determine Data
C                 Storage Requirements
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        MODIFIED:   To check for NO ECHO in the input file.
C                    R.W. Brode, PES, Inc. - 12/2/98
C
C        INPUTS:  Input Runstream File
C
C        OUTPUTS: Array Sizes
C
C        CALLED FROM:   MAIN
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE
      INTEGER :: I
      LOGICAL NOPATH, NOKEY
      CHARACTER RDFRM*20
      CHARACTER INPFLD*2, PATHWY(5)*2
      INTERFACE
         SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
            CHARACTER (LEN=2), INTENT(IN) :: INPFLD
            CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
            INTEGER, INTENT(IN) :: IPN
            LOGICAL, INTENT(OUT) :: NOPATH
         END SUBROUTINE EXPATH
      END INTERFACE

C     Variable Initializations
      MODNAM = 'PRESET'
      EOF = .FALSE.
      ILINE  = 0

      NDEM   = 0
      NSRC   = 0
      IPNUM  = 0
      IPPNUM = 0
C     Counters for the Receptor Groups
      ISTA = .FALSE.
      IEND = .FALSE.
      NEWID = .TRUE.

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')

C     LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF)

C        Increment the Line Counter
         ILINE = ILINE + 1

C        READ Record to Buffers, as A'num' and 'num'A1 where 'num' = ISTRG.
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1
         READ (INUNIT,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

C        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C        Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

C        Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

C        If Blank Line, Then CYCLE to Next Card
         IF (BLINE) GO TO 11

C        Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
C           Skip record with NO ECHO during PRESET stage of processing
            GO TO 11
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         PATHWY(1) = 'CO'
         PATHWY(2) = 'SO'
         PATHWY(3) = 'RE'
         PATHWY(4) = 'OU'
         PATHWY(5) = '**'
         CALL EXPATH(FIELD(1),PATHWY,5,NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH) THEN
C           Skip Error Message for PRESET stage of processing
            PATH = PPATH
            GO TO 11
         ELSE IF (PATH .EQ. '**') THEN
            GO TO 11
         END IF

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

         IF (NOKEY) THEN
C           Invalid Keyword - Skip Error Message for PRESET stage
            PKEYWD = KEYWRD
            GO TO 11
         END IF

C        Save Current Path and Path Number as Previous Path and Number
         PPATH = PATH
         IPPNUM = IPNUM

         IF (PATH .EQ. 'CO' .AND. KEYWRD .EQ. 'DATAFILE') THEN
C           Increment number of DEM files         
            NDEM = NDEM + 1
         END IF
            
         IF (PATH .EQ. 'SO') THEN
C           Determine array sizes for source arrays         
            CALL SRCSIZ
         END IF

         IF (PATH .EQ. 'RE') THEN
C           Determine array sizes for receptor arrays         
            CALL RECSIZ
         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

C        Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
C        to Statement 999 in Order to Avoid Reading a ^Z "End of File"
C        Marker That May Be Present For Some Editors.
         IF (PATH .EQ. 'OU' .AND. KEYWRD .EQ. 'FINISHED') THEN
            GO TO 999
         END IF

         GO TO 11
 999     EOF = .TRUE.
 11      CONTINUE
      END DO

C     Rewind File and Reinitialize Line Number Counter for SETUP
      REWIND INUNIT
      ILINE = 0
      PNETID = '        '

C     Ensure that array limits are not < 1.
      NSRC = MAX( NSRC, 1)
      NREC = MAX( NREC, 1)
      NDEM = MAX( NDEM, 1)
      NARC = NREC
      NNET = MAX( NNET, 1)
      IXM  = MAX( IXM , 1)
      IYM  = MAX( IYM , 1)

      RETURN
      END


      SUBROUTINE PREINCLUD
C***********************************************************************
C*                PREINCLUD Module of the AMS/EPA Regulatory Model - AERMOD
C*
C*       PURPOSE: To read an external receptor/source file using the
C*                INCLUDED keyword.
C*
C*       PROGRAMMER: Roger Brode
C*
C*       DATE:    September 24, 1996
C*
C*       MODIFIED:   
C*                   
C*       INPUTS: 
C*
C*       OUTPUTS:
C*               
C*
C*       CALLED FROM:   PRESET, SRCSIZ, RECSIZ
C***********************************************************************
        
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER*12 MODNAM

      SAVE
      INTEGER :: I, ILREAL
      LOGICAL NOPATH, NOKEY      
      CHARACTER RDFRM*20
      CHARACTER INPFLD*2, PATHWY(5)*2
      INTERFACE
         SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
            CHARACTER (LEN=2), INTENT(IN) :: INPFLD
            CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
            INTEGER, INTENT(IN) :: IPN
            LOGICAL, INTENT(OUT) :: NOPATH
         END SUBROUTINE EXPATH
      END INTERFACE

C*    Variable Initializations
      MODNAM = 'PREINCLUD'
      EOF = .FALSE.
      ILINE = 1

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')
      

      IF (IFC .EQ. 3) THEN
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. ILEN_FLD-1) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            INCFIL = RUNST1(LOCB(3):LOCE(3))
            OPEN (INCUNT,FILE=INCFIL,ACTION='READ',STATUS='OLD',ERR=99)
         ELSE
C           WRITE Error Message:  INCFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
            GO TO 1002
         END IF

      ELSE IF (IFC .GT. 4) THEN
C        Too Many Parameters
         GO TO 1002
      ELSE
C        No Parameters Specified
         GO TO 1002
      END IF

      GO TO 1001

C     Write Out Error Message for File OPEN Error
99    CALL ERRHDL(PATH,MODNAM,'E','500','INCFILE ')
      GO TO 1002

1001  CONTINUE

C     LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF)

C        Increment the Line Counter.  It was Initially Set to 1, to Handle
C        the Code in Subroutine DEFINE
         ILINE = ILINE + 1
         ILREAL = ILREAL + 1

C        READ Record to Buffers, as A'num' and 'num'A1 where 'num' = ISTRG.
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1
         READ (INCUNT,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

C        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C        Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

         IF (ILREAL .EQ. 1) ILINE = ILINE - 1

C        Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

C        If Blank Line, Then CYCLE to Next Card
         IF (BLINE) GO TO 11

C        Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
C           Skip record with NO ECHO during PREINCLUD stage of processing
            GO TO 11
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         PATHWY(1) = 'CO'
         PATHWY(2) = 'SO'
         PATHWY(3) = 'RE'
         PATHWY(4) = 'OU'
         PATHWY(5) = '**'
         CALL EXPATH(FIELD(1),PATHWY,5,NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH) THEN
C           Skip Error Message for PREINCLUD stage of processing
            PATH = PPATH
            GO TO 11
         ELSE IF (PATH .EQ. '**') THEN
            GO TO 11
         END IF

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

         IF (NOKEY) THEN
C           Invalid Keyword - Skip Error Message for PREINCLUD stage
            PKEYWD = KEYWRD
            GO TO 11
         END IF

C        Save Current Path and Path Number as Previous Path and Number
         PPATH = PATH
         IPPNUM = IPNUM

C        Process Input Card Based on Pathway
         IF (PATH .EQ. 'SO') THEN
C           Process SOurce Pathway Cards                    ---   CALL SOINCL
            CALL PRESOINC
         ELSE IF (PATH .EQ. 'RE') THEN
C           Process REceptor Pathway Cards                  ---   CALL REINCL
            CALL PREREINC
         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

         GO TO 11
 999     EOF = .TRUE.
 11      CONTINUE

      END DO
      EOF = .FALSE.

C     Close the INCLUDED File
      CLOSE (INCUNT)
      
1002  RETURN
      END

      SUBROUTINE RECSIZ
C***********************************************************************
C                 RECSIZ Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To preprocess receptor inputs to determine
C                 storage requirements
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Pathway (RE) and Keyword
C
C        OUTPUTS: Receptor Arrays
C                 Receptor Setup Status Switches
C
C        CALLED FROM:   PRESET
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'RECSIZ'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Initialize Counters and Set Status Switch
         NREC = 0
         NNET = 0
         IXM  = 0
         IYM  = 0
         PXSOID = ' '
         ISTA = .FALSE.
      ELSE IF (KEYWRD .EQ. 'GRIDCART') THEN
C        Process Cartesian Grid Receptor Network            ---   CALL PRECART
         CALL PRECART
      ELSE IF (KEYWRD .EQ. 'GRIDPOLR') THEN
C        Process Polar Receptor Network                     ---   CALL PREPOLR
         CALL PREPOLR
      ELSE IF (KEYWRD .EQ. 'DISCCART') THEN
         NREC = NREC + 1
      ELSE IF (KEYWRD .EQ. 'EVALCART') THEN
         NREC = NREC + 1
      ELSE IF (KEYWRD .EQ. 'DISCPOLR') THEN
         NREC = NREC + 1
      ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
         CALL PREINCLUD
      END IF

 999  RETURN
      END

      SUBROUTINE SRCSIZ
C***********************************************************************
C                 SRCSIZ Module of AERMAP
C
C        PURPOSE: To preprocess source inputs to determine
C                 storage requirements
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Pathway (RE) and Keyword
C
C        OUTPUTS: Receptor Arrays
C                 Receptor Setup Status Switches
C
C        CALLED FROM:   PRESET
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'SRCSIZ'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Initialize Counters and Set Status Switch
         NSRC = 0
      ELSE IF (KEYWRD .EQ. 'LOCATION') THEN
         NSRC = NSRC + 1
      ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
         CALL PREINCLUD
      END IF

 999  RETURN
      END

      SUBROUTINE PRESOINC
C***********************************************************************
C                 PRESOINC Module of the AERMAP Terrain Processor
C
C        PURPOSE: To preprocess source inputs from an INCLUDE file to 
C                 determine number of sources for dynamically allocating
C                 array storage requirements
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    February 9, 2009
C
C        CALLED FROM:   PREINCLUD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE

C     Variable Initializations
      MODNAM = 'PRESOINC'

      IF (KEYWRD .EQ. 'LOCATION') THEN
         NSRC = NSRC + 1
      END IF

 999  RETURN
      END

      SUBROUTINE PREREINC
C***********************************************************************
C                 PRESOINC Module of the AERMAP Terrain Processor
C
C        PURPOSE: To preprocess source inputs from an INCLUDE file to 
C                 determine number of receptors for dynamically allocating
C                 array storage requirements
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    February 9, 2009
C
C        CALLED FROM:   PREINCLUD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE

C     Variable Initializations
      MODNAM = 'PREREINC'

      IF (KEYWRD .EQ. 'GRIDCART') THEN
C        Process Cartesian Grid Receptor Network            ---   CALL PRECART
         CALL PRECART
      ELSE IF (KEYWRD .EQ. 'GRIDPOLR') THEN
C        Process Polar Receptor Network                     ---   CALL PREPOLR
         CALL PREPOLR
      ELSE IF (KEYWRD .EQ. 'DISCCART') THEN
         NREC = NREC + 1
      ELSE IF (KEYWRD .EQ. 'EVALCART') THEN
         NREC = NREC + 1
      ELSE IF (KEYWRD .EQ. 'DISCPOLR') THEN
         NREC = NREC + 1
      END IF

 999  RETURN
      END

      SUBROUTINE PRECART
C***********************************************************************
C                 PRECART Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Cartesian Grid Receptor Network Inputs
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network Inputs
C
C        CALLED FROM:   RECSIZ, PREREINC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE

C     Variable Initializations
      MODNAM = 'PRECART'

C     READ in the Netid and Nettype
      IF (IFC .LT. 3) THEN
C        Missing Data Field
         GO TO 999
      END IF
      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'XYINC' .OR. NETIDT.EQ.'XPNTS' .OR.
     &    NETIDT.EQ.'YPNTS' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'HILL'  .OR.
     &    NETIDT.EQ.'FLAG'  .OR. NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
      ELSE IF (NEWID .AND. NETIDT.NE.' ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
C        The Keyword Counter
         NNET = NNET + 1
      ELSE
C        Invalid Secondary Keyword
         GO TO 999
      END IF

C     Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
C        Initialize Logical Control Variables
         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
C        Set Counters of Calculation Field
         ICOUNT = 0
         JCOUNT = 0
      ELSE IF (KTYPE .EQ. 'XYINC') THEN
C        Set the Uniform Spacing Receptor Network           ---   CALL PREGENCAR
         CALL PREGENCAR
      ELSE IF (KTYPE.EQ.'XPNTS' .OR. KTYPE.EQ.'YPNTS') THEN
C        Set the Non-uniform Spacing Receptor Network       ---   CALL PREXYPNTS
         CALL PREXYPNTS
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
         IF (.NOT. RECERR) THEN
            NREC = NREC + ICOUNT*JCOUNT
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.

      ELSE IF (KTYPE.NE.'ELEV' .AND. KTYPE.NE.'FLAG' .AND.
     &         KTYPE.NE.'HILL') THEN
C        Invalid Secondary Keyword
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      END

      SUBROUTINE PREGENCAR
C***********************************************************************
C                 PREGENCAR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Generates Cartesian Grid Receptor Network With
C                 Uniform Spacing
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network With Uniform
C                 Spacing
C
C        CALLED FROM:   PRECART
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE
      INTEGER :: I, K, IMIT
      DOUBLE PRECISION :: TEMPP(6)
      LOGICAL ERROR

C     Variable Initializations
      MODNAM = 'PREGENCAR'
      ERROR = .FALSE.

C     Check for Location of Secondary Keyword, XYINC
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'XYINC') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Missing Parameter
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+5) THEN
C        Too Many Parameters
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+5) THEN
C        Too Few Parameters
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Input The Numerical Values
      DO K = 1,6
         CALL STODBL(FIELD(ISC + K-1),ILEN_FLD,TEMPP(K),IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            ERROR = .TRUE.
            RECERR = .TRUE.
         END IF
      END DO

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

C     Assign Values to Appropriate Variables for Generated Network
      XINT   = TEMPP(1)
      ICOUNT = IDNINT(TEMPP(2))
      XDELTA = TEMPP(3)
      YINT   = TEMPP(4)
      JCOUNT = IDNINT(TEMPP(5))
      YDELTA = TEMPP(6)

C     Assign Them to the Coordinate Arrays
      IF (ICOUNT .GT. IXM) THEN
         IXM = ICOUNT
      END IF
      IF (JCOUNT .GT. IYM) THEN
         IYM = JCOUNT
      END IF

 999  RETURN
      END

      SUBROUTINE PREXYPNTS
C***********************************************************************
C                 PREXYPNTS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Cartesian Grid x,y Input Value
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid x,y Input Value
C
C        CALLED FROM:   PRECART
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE
      INTEGER :: I, ISET, JSET, IMIT

C     Variable Initializations
      MODNAM = 'PREXYPNTS'

      IF (KTYPE .EQ. 'XPNTS') THEN
C        Check for Location of Secondary Keyword, XPNTS
         DO I = 1, IFC
            IF (FIELD(I) .EQ. 'XPNTS') THEN
               ISC = I + 1
            END IF
         END DO

C        Determine Whether There Are Enough Parameter Fields
         IF (IFC .EQ. ISC-1) THEN
C           Missing Parameter
            RECERR = .TRUE.
            GO TO 999
         END IF

         ISET = ICOUNT
         DO I = ISC, IFC
            CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               RECERR = .TRUE.
            END IF
            ISET = ISET + 1
            IF (ISET .GT. IXM) THEN
               IXM = ISET
            END IF
         END DO
         ICOUNT = ISET

      ELSE IF (KTYPE .EQ. 'YPNTS') THEN
C        Check for Location of Secondary Keyword, YPNTS
         DO I = 1, IFC
            IF (FIELD(I) .EQ. 'YPNTS') THEN
               ISC = I + 1
            END IF
         END DO

C        Determine Whether There Are Enough Parameter Fields
         IF (IFC .EQ. ISC-1) THEN
C           Missing Parameter
            RECERR = .TRUE.
            GO TO 999
         END IF

         JSET = JCOUNT
         DO I = ISC, IFC
            CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               RECERR = .TRUE.
            END IF
            JSET = JSET + 1
            IF (JSET .GT. IYM) THEN
               IYM = JSET
            END IF
         END DO
         JCOUNT = JSET

      END IF

 999  RETURN
      END

      SUBROUTINE PREPOLR
C***********************************************************************
C                 PREPOLR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Polar Grid Receptor Network Inputs
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Receptor Network Inputs
C
C        CALLED FROM:   RECSIZ, PREREINC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE

C     Variable Initializations
      MODNAM = 'PREPOLR'

      IF (IFC .LT. 3) THEN
C        Missing Data Field
         GO TO 999
      END IF

C     READ in the Netid and Nettype
      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'ORIG' .OR. NETIDT.EQ.'DIST' .OR.
     &    NETIDT.EQ.'DDIR' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'HILL' .OR.
     &    NETIDT.EQ.'FLAG' .OR. NETIDT.EQ.'GDIR' .OR.
     &    NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
      ELSE IF (NEWID .AND. NETIDT.NE.'    ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
C        The Keyword Counter
         NNET = NNET + 1
      ELSE
C        Invalid Secondary Keyword
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
         ICOUNT = 0
         JCOUNT = 0
      ELSE IF (KTYPE .EQ. 'DIST') THEN
C        Read in the Distance Set                           ---   CALL PREPOLDST
         CALL PREPOLDST
      ELSE IF (KTYPE .EQ. 'GDIR') THEN
         CALL PREGENPOL
      ELSE IF (KTYPE .EQ. 'DDIR') THEN
         CALL PRERADRNG
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
C        Get the Final Result
         IF (.NOT. RECERR) THEN
            NREC = NREC + ICOUNT*JCOUNT
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.

      ELSE IF (KTYPE.NE.'ELEV' .AND. KTYPE.NE.'FLAG' .AND.
     &         KTYPE.NE.'HILL' .AND. KTYPE.NE.'ORIG') THEN
C        Invalid Secondary Keyword
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      END

      SUBROUTINE PREPOLDST
C***********************************************************************
C                 PREPOLDST Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Gets Distances for the Polar Network
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Distance Input Value
C
C        CALLED FROM:   PREPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE
      INTEGER :: I, ISET, IMIT

C     Variable Initializations
      MODNAM = 'PREPOLDST'

C     Skip the Unrelated Fields
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'DIST') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Missing Parameter
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = ICOUNT

      DO I = ISC, IFC
         CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .GT. IXM) THEN
            IXM = ISET
         END IF
      END DO

      ICOUNT = ISET

 999  RETURN
      END

      SUBROUTINE PREGENPOL
C***********************************************************************
C                 PREGENPOL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Generates Polar Receptor Network With
C                 Uniform Spacing
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Receptor Network With Uniform Direction Spacing
C
C        CALLED FROM:   PREPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE
      INTEGER :: I, K, IMIT
      DOUBLE PRECISION :: TEMPP(3), DIRINI, DIRINC
      LOGICAL ERROR

C     Variable Initializations
      MODNAM = 'PREGENPOL'
      ERROR = .FALSE.

C     Check for the Location of the Secondary Keyword, GDIR
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'GDIR') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Missing Parameter
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+2) THEN
C        Not Enough Parameters
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+2) THEN
C        Too Many Parameters
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Input Numerical Values
      DO K = 1, 3
         CALL STODBL(FIELD(ISC + K-1),ILEN_FLD,TEMPP(K),IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            RECERR = .TRUE.
            ERROR = .TRUE.
         END IF
      END DO

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

      JCOUNT = IDNINT(TEMPP(1))
      DIRINI = TEMPP(2)
      DIRINC = TEMPP(3)

C     Assign Them to the Coordinate Arrays
      IF (JCOUNT .GT. IYM) THEN
         IYM = JCOUNT
      END IF

 999  RETURN
      END

      SUBROUTINE PRERADRNG
C***********************************************************************
C                 PRERADRNG Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Non-Uniform Polar Network Value
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Directions in Non-Uniform Spacing
C
C        CALLED FROM:   PREPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      SAVE
      INTEGER :: I, ISET, IMIT

C     Variable Initializations
      MODNAM = 'PRERADRNG'

C     Skip the non-useful Fields
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'DDIR') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = JCOUNT

      DO I = ISC, IFC
         CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .GT. IYM) THEN
            IYM = ISET
         END IF
      END DO

      JCOUNT = ISET

 999  RETURN
      END

      SUBROUTINE ALLSETUP
C***********************************************************************
C                 ALLSETUP Module for AERMAP
C
C        PURPOSE: Allocate Array Storage for SETUP
C
C        PROGRAMMER: Roger W. Brode
C                    U.S. EPA, OAQPS/AQAD
C
C        DATE:    May 5, 2008
C
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C        CALLED FROM:  MAIN
C
C        ERROR HANDLING:   Checks for error allocating arrays
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'ALLSET'

      ALLOCATE (IDMUNT(NDEM), IDRUNT(NDEM), IDXUNT(NDEM))
      ALLOCATE (ITiffDbg_Unt(NDEM))
      ALLOCATE (DEMFIL(ndem), DIRFIL(ndem), IDXFIL(NDEM))
      ALLOCATE (TiffDbgFil(NDEM))

      ALLOCATE (AXR(NREC), AYR(NREC))
      ALLOCATE (AXS(NSRC), AYS(NSRC))
      ALLOCATE (RLAT(NREC), RLON(NREC))
      ALLOCATE (SLAT(NSRC), SLON(NSRC))
      ALLOCATE (XRECU(NREC), YRECU(NREC))
      ALLOCATE (XCOORD(ixm,nnet))
      ALLOCATE (YCOORD(iym,nnet))
      ALLOCATE (NETID(NREC), RECTYP(NREC), NTID(NNET))
      ALLOCATE (NTTYP(NNET), ARCID(NARC), RECNAM(NREC))
      
      ALLOCATE (LRec_FILLED(NREC,NDEM), LSrc_FILLED(NSRC,NDEM))
      
      ALLOCATE (XSRCU(NSRC), YSRCU(NSRC))
      ALLOCATE (XORIG(nnet), YORIG(nnet))
      ALLOCATE (XRDIFS(NREC), YRDIFS(NREC))
      ALLOCATE (XRDIFM(NREC), YRDIFM(NREC))
      ALLOCATE (XSDIFS(NSRC), YSDIFS(NSRC))
      ALLOCATE (XSDIFM(NSRC), YSDIFM(NSRC))
      ALLOCATE (DATUMSHFT(NREC), DATUMSHFTS(NSRC))

      ALLOCATE (HC(NREC), AZELEV(NREC), AZFLAG(NREC), AZS(NSRC))
      ALLOCATE (AZHILL(NREC))
      ALLOCATE (ZETMP1(NREC), ZETMP2(NREC))
      ALLOCATE (ZFTMP1(NREC), ZFTMP2(NREC))
               
      ALLOCATE (DXM(NDEM), DYM(NDEM), DCI(NDEM), UserDCI(NDEM))
               
      ALLOCATE (NUMPRF(NDEM), IZOND(NDEM))
               
      ALLOCATE (IZONR(NREC), IZONS(NSRC), IREF(NREC))
      ALLOCATE (IRIN(NREC,NDEM), ISIN(NSRC,NDEM))
      ALLOCATE (JDM(NREC), JDMS(NSRC))
      ALLOCATE (NDXARC(NREC))
      ALLOCATE (NUMXPT(NNET), NUMYPT(NNET))
      ALLOCATE (NETSTA(NNET), NETEND(NNET))

      ALLOCATE (SRCID(NSRC), SRCTYP(NSRC))
               
      ALLOCATE (FT(NDEM), ADJMAP(NDEM+1,9))
      ALLOCATE (DEMLVL(NDEM), ELEVPAT(NDEM), IPLAN(NDEM))
      ALLOCATE (IZO(NDEM), ACCUC(NDEM), LPINT(NDEM))
      ALLOCATE (CUNIT(NDEM), ELUNIT(NDEM), SIDZ(NDEM))
      ALLOCATE (UserElevUnits(NDEM))
      ALLOCATE (NROW(NDEM), NPROF(NDEM), LPRIM(NDEM))
      ALLOCATE (SPRIM(NDEM), SPINT(NDEM), DDATE(NDEM))
      ALLOCATE (DVALD(NDEM), SUSF(NDEM), VDAT(NDEM))
      ALLOCATE (DINSP(NDEM))
      ALLOCATE (EDITN(NDEM), PVOID(NDEM), NADD(NDEM))
      ALLOCATE (MAPNAME(NDEM+1))
      ALLOCATE (FLN(NDEM))
      ALLOCATE (Chr_UserElevUnits(NDEM))
      
      Allocate (tiffType(NDEM))                ! data orgination in GeoTIFF ('strip' || 'tile')
      ALLOCATE (byteSwap(NDEM))                ! endianness of NED GeoTiff file
      ALLOCATE (nCols(NDEM), nRows(NDEM))      ! number of rows and cols in NED GeoTIFF
      ALLOCATE (dataOS(NDEM))                  ! offset (bytes) to data strips or tiles in NED GeoTIFF
      ALLOCATE (rowsPerStrip(NDEM))            ! number of rows per strip of data in NED GeoTIFF
      Allocate (tileLen(NDEM))                 ! length (height) of tile in pixels
      Allocate (tileWid(NDEM))                 ! width of tile in pixels
      Allocate (bytesPerSample(NDEM))          ! number of bytes per data sample
               
      ALLOCATE (tiePtx(NDEM))    ! upper left X coordinate NED GeoTIFF
      ALLOCATE (tiePty(NDEM))    ! upper left Y coordinate NED GeoTIFF
      ALLOCATE (tiePtz(NDEM))    ! upper left Z coordinate NED GeoTIFF
               
      ALLOCATE (pxlScalex(NDEM))    ! model pixel scale X-direction
      ALLOCATE (pxlScaley(NDEM))    ! model pixel scale Y-direction
      ALLOCATE (pxlScalez(NDEM))    ! model pixel scale Z-direction
               
      ALLOCATE (SWE_MTRS(NDEM), SWN_MTRS(NDEM))
      ALLOCATE (NWE_MTRS(NDEM), NWN_MTRS(NDEM))
      ALLOCATE (NEE_MTRS(NDEM), NEN_MTRS(NDEM))
      ALLOCATE (SEE_MTRS(NDEM), SEN_MTRS(NDEM))
      ALLOCATE (SWLAT_ARCS(NDEM), SWLON_ARCS(NDEM))
      ALLOCATE (NWLAT_ARCS(NDEM), NWLON_ARCS(NDEM))
      ALLOCATE (NELAT_ARCS(NDEM), NELON_ARCS(NDEM))
      ALLOCATE (SELAT_ARCS(NDEM), SELON_ARCS(NDEM))
      ALLOCATE (SWLAT_DEGS(NDEM), SWLON_DEGS(NDEM))
      ALLOCATE (NWLAT_DEGS(NDEM), NWLON_DEGS(NDEM))
      ALLOCATE (NELAT_DEGS(NDEM), NELON_DEGS(NDEM))
      ALLOCATE (SELAT_DEGS(NDEM), SELON_DEGS(NDEM))
      
      ALLOCATE (SW_UTMZ(NDEM), SE_UTMZ(NDEM))
      ALLOCATE (NW_UTMZ(NDEM), NE_UTMZ(NDEM))
               
      ALLOCATE (ELEVMAX(NDEM))
      ALLOCATE (DLTN(NDEM), DLGE(NDEM))
      ALLOCATE (MPROJ(NDEM,15), DMCNR(NDEM,2,4))
      ALLOCATE (ELEVMN(NDEM), ELEVMX(NDEM))
      ALLOCATE (CNTRC(NDEM), SECTNL(NDEM), MCTR(NDEM)) 
      ALLOCATE (MAPN(NDEM), FREEF(NDEM), FILR1(NDEM), PROCODE(NDEM))
      ALLOCATE (FILR2(NDEM), INSPF(NDEM)) 

      ALLOCATE (L_DEMCHECK(NDEM), L_UserElevUnits(NDEM), 
     &          L_TiffDebug(NDEM), 
     &          L_NEDSkip(NDEM))
     
      RETURN
      END
