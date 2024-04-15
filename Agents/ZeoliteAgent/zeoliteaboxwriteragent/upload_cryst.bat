@echo off
rem Part of ontozeolite package.
rem Generation of entire ontozeolite knowledge graph.
rem Author: Rutkevych Pavlo
rem Date: 2024/04/01

rem Uploader to a blazegraph server. Requires vertual environment pyuploader_venv.
rem https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_uploader
rem
rem Accept up to TWO command line arguments:
rem if the first is "laura" then the server is remote (blazegraph-dev)
rem The other argument can specify the namespace.
rem 
rem It may take a long time to upload files to a remove server.
rem The start and end time stamps are saved to the upload.log file.

if "%1" == "laura" (
  if "%2"=="" (
    set NS=ontozeolite
  ) else (
    set NS=%2
  )
  rem set BASE="https://theworldavatar.io/chemistry/blazegraph-dev/ui/namespace/"
  set BASE="http://178.128.105.213:3838/blazegraph-dev/ui/namespace/"
  set AUTH=--auth-file=blazedev.auth
) else (
  if "%1"=="" (
    set NS=zeo06g
  ) else (
    set NS=%1
  )
  set BASE="http://localhost:8080/blazegraph/namespace/"
  set AUTH=--no-auth
)

set URL=%BASE%%NS%/sparql
echo Uploading to %URL% %AUTH%

echo Start upload: %DATE% %TIME% >> upload.log
echo Uploading to %URL% >> upload.log

rem goto EXIT
rem set PREFIX=start
set PREFIX=


set OWL_DIR=ontozeolite\zeolite\owl
%PREFIX% pyuploader ts_upload %OWL_DIR%"\ontozeolite_kg_01.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\ontozeolite_kg_02.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\ontozeolite_kg_03.owl" --url=%URL% %AUTH%

set OWL_DIR="ontozeolite\biblio\owl"
%PREFIX% pyuploader ts_upload %OWL_DIR%"\onto_bib.csv.owl" --url=%URL% %AUTH%

set OWL_DIR=ontozeolite\crystal\owl
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_0.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_1.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_2.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_3.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_4.csv.owl --url=%URL% %AUTH%

:BBBBB

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_5.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_6.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_7.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_8.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_9.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_10.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_11.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_12.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_13.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_14.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_15.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_16.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_17.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_18.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_19.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_20.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_21.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_22.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_23.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_24.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_25.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_26.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_27.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_28.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_29.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_30.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_31.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_32.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_33.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_34.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_35.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_36.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_37.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_38.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_39.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_40.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_41.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_42.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_43.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_44.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_45.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_46.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_47.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_48.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_49.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_50.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_51.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_52.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_53.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_54.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_55.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_56.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_57.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_58.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_59.csv.owl --url=%URL% %AUTH%


%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_60.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_61.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_62.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_63.csv.owl --url=%URL% %AUTH%
goto EXIT

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_64.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_65.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_66.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_67.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_68.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_69.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_70.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_71.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_72.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_73.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_74.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_75.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_76.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_77.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_78.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_79.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_80.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_81.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_82.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_83.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_84.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_85.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_86.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_87.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_88.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_89.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_90.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_91.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_92.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_93.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_94.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_95.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_96.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_97.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_98.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_99.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_100.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_101.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_102.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_103.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_104.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_105.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_106.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_107.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_108.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_109.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_110.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_111.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_112.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_113.csv.owl --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%\cif_twa_114.csv.owl --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_115.csv.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_116.csv.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_117.csv.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_118.csv.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_119.csv.owl" --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_120.csv.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_121.csv.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_122.csv.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_123.csv.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_124.csv.owl" --url=%URL% %AUTH%

%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_125.csv.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_126.csv.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_127.csv.owl" --url=%URL% %AUTH%

goto AAAAA
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_128.csv.owl" --url=%URL% %AUTH%
%PREFIX% pyuploader ts_upload %OWL_DIR%"\cif_twa_129.csv.owl" --url=%URL% %AUTH%

:AAAAA
goto EXIT

:EXIT
echo End upload: %DATE% %TIME% >> upload.log
