*<meta creator='isd_enthalpy_calculator'>
*<meta author='pb556'>
*<meta variables=4>
*<meta equations=8>
*<meta job_type='MIN'>
*<meta date_time='2013/10/23 11:59:12'>
NAME  C2H4
ROWS
 N  R0
 L  R1
 L  R2
 L  R3
 L  R4
 L  R5
 L  R6
 L  R7
 L  R8
 E  R9
 E  R10
 E  R11
 E  R12
 E  R13
 G  main
COLUMNS
    MARK0000  'MARKER'                 'INTORG'
    v[3]       R1        1.0000000000
    v[3]       R2        -1.000000000
    v[3]       R9        3.0000000000
    v[3]       R10       8.0000000000
    v[3]       R11       2.0000000000
    v[3]       R12       8.0000000000
    absV[3]    R0        10.000000000
    absV[3]    R1        -1.000000000
    absV[3]    R2        -1.000000000
    v[0]       R3        1.0000000000
    v[0]       R4        -1.000000000
    v[0]       R9        2.0000000000
    v[0]       R10       4.0000000000
    v[0]       R12       4.0000000000
    v[0]       R13       1.0000000000
    v[0]       main      1.0000000000
    v[1]       R5        1.0000000000
    v[1]       R6        -1.000000000
    v[1]       R9        3.0000000000
    v[1]       R10       6.0000000000
    v[1]       R11       1.0000000000
    v[1]       R12       6.0000000000
    v[1]       R13       1.0000000000
    absV[0]    R0        5.0000000000
    absV[0]    R3        -1.000000000
    absV[0]    R4        -1.000000000
    absV[1]    R0        8.0000000000
    absV[1]    R5        -1.000000000
    absV[1]    R6        -1.000000000
    v[2]       R7        1.0000000000
    v[2]       R8        -1.000000000
    v[2]       R9        2.0000000000
    v[2]       R10       6.0000000000
    v[2]       R11       1.0000000000
    v[2]       R12       6.0000000000
    absV[2]    R0        7.0000000000
    absV[2]    R7        -1.000000000
    absV[2]    R8        -1.000000000
    MARK0001  'MARKER'                 'INTEND'
RHS
    RHS       main      1.0000000000
BOUNDS
 FR BND       absV[3]
 FR BND       v[3]
 FR BND       absV[0]
 FR BND       v[0]
 FR BND       absV[1]
 FR BND       v[1]
 FR BND       absV[2]
 FR BND       v[2]
ENDATA