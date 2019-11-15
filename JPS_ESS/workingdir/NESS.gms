$TITLE eps-Constraint Method for Multiobjective Optimization (EPSCM,SEQ=319)
$inlinecom [ ]
$eolcom //
$STitle Example model definitions

Sets
   p       energy storage technologies /PHS,CAES,FES,LA,NaS,ZEBRA,NiCd,Li-ion,VRFB,ZnBr,PSB,SMES,Sc,H2,Powerwall,BlueBattery,Baobab/
   k       objective functions /  environment,cost, maturity /
   d       param /Pa_low, Pa_high, Da_low, Da_high/
$set min -1
$set max +1
Parameter dir(k) direction of the objective functions
   / cost %min%, environment %min%, maturity %max%/;

*Application specifications
*Power requirement
Parameter
*Pa_low Lower bound of power for different application
Pa_high Higher bound of power for different application
Pa_ave average power for different applications
Pa_halfrange half of the range of power for different applications *Duration requirement
*Da_low Lower bound of duration for different applications
*Da_high Higher bound of duration for different applications
Da_ave average duration for different applications
Da_halfrange half of the range of duration for different applications;


*Technology
Parameter
Pt_low(p) Lower bound of power for different ESS
Pt_high(p) Higher bound of power for different ESS
Dt_low(p) Lower bound of duration for different ESS
Dt_high(p) Higher bound of duration for different ESS
E(p) Environmental_Score obtained from the comparison of environmental impact among different ESS
C(p) Economical_Score obtained from the comparison of LCOE among different ESS-Levelized cost of energy storage system
M(p) Maturity obtained from the database for different ESS ;

$call csv2gdx Pa_high.csv id=Pa_high index=1 values=2..lastCol useHeader=y
$GDXIN Pa_high.gdx
$Load Pa_high
$GDXIN

$call csv2gdx Ptlow.csv id=Pt_low index=1 values=2..lastCol useHeader=y
$GDXIN Ptlow.gdx
$Load Pt_low
$GDXIN

$call csv2gdx Pthigh.csv id=Pt_high index=1 values=2..lastCol useHeader=y
$GDXIN Pthigh.gdx
$Load Pt_high
$GDXIN

$call csv2gdx Dtlow.csv id=Dt_low index=1 values=2..lastCol useHeader=y
$GDXIN Dtlow.gdx
$Load Dt_low
$GDXIN

$call csv2gdx Dthigh.csv id=Dt_high index=1 values=2..lastCol useHeader=y
$GDXIN Dthigh.gdx
$Load Dt_high
$GDXIN

$call csv2gdx EnvironmentalScore.csv id=E index=1 values=2..lastCol useHeader=y
$GDXIN EnvironmentalScore.gdx
$Load E
$GDXIN

$call csv2gdx EconomicalScore.csv id=C index=1 values=2..lastCol useHeader=y
$GDXIN EconomicalScore.gdx
$Load C
$GDXIN

$call csv2gdx Maturity.csv id=M index=1 values=2..lastCol useHeader=y
$GDXIN Maturity.gdx
$Load M
$GDXIN

Parameter

Pt_ave(p) average power for different ESS
Pt_halfrange(p) half of the range of power for different ESS
dP_ave(p) difference between the average of i and j
dP_halfrange(p) sum of half range of i and j
P_index(p) overlapping index of power between technology i and application j assuming standard deviation

Dt_ave(p) average duration for different ESS
Dt_halfrange(p) half of the range of duration for different ESS
dD_ave(p) difference between the average of i and j
dD_halfrange(p) sum of half range of i and j
D_index(p) overlapping index of duration between technology i and application j assuming standard deviation;

Pt_ave(p)=(Pt_high(p)+Pt_low(p))/2;
Pt_halfrange(p)=(Pt_high(p)-Pt_low(p))/2;
Pa_ave=(Pa_high('Pa_high')+Pa_high('Pa_low'))/2;
Pa_halfrange=(Pa_high('Pa_high') - Pa_high('Pa_low'))/2;
dP_ave(p)=abs(Pt_ave(p)-Pa_ave);
dP_halfrange(p)=Pt_halfrange(p)+Pa_halfrange;
P_index(p)=(dP_halfrange(p)-dP_ave(p));

Dt_ave(p)=(Dt_high(p)+Dt_low(p))/2;
Dt_halfrange(p)=(Dt_high(p)-Dt_low(p))/2;
Da_ave=(Pa_high('Da_high')+Pa_high('Da_low'))/2;
Da_halfrange=(Pa_high('Da_high')-Pa_high('Da_low'))/2;
dD_ave(p)=abs(Dt_ave(p)-Da_ave);
dD_halfrange(p)=Dt_halfrange(p)+Da_halfrange;
D_index(p)=(dD_halfrange(p)-dD_ave(p));

display Pa_high;
display Pa_ave;
display Pa_halfrange;

Variables
   z(k)      objective function variables
Binary Variables
   x(p)      if the ESS is selected z equals to 1 otherwise 0;

Equations
   objcost   objective for minimizing cost in K$
   objco2    objective for minimizing CO2 emissions in Kt
   objco3    Market Readiness
   cons1     D
   cons2     P
   cons select only one technology;

* Objective functions
objcost.. sum(p, x(p)*C(p)) =e= z('cost');
objco2..  sum(p, x(p)*E(p)) =e= z('environment');
objco3..  sum(p, x(p)*M(p)) =e= z('maturity');
cons1..  sum(p, x(p)*D_index(p))  =g= 0;
cons2..  sum(p, x(p)*P_index(p))  =g= 0;
cons..    sum(p, x(p))=e=1;
option decimals=5;
Model example / all /;

$STitle eps-constraint method

Set k1(k) the first element of k, km1(k) all but the first elements of k;
k1(k)$(ord(k)=1) = yes; km1(k)=yes; km1(k1) = no;
Set kk(k)     active objective function in constraint allobj
Parameter
   rhs(k)     right hand side of the constrained obj functions in eps-constraint
   maxobj(k)  maximum value from the payoff table
   minobj(k)  minimum value from the payoff table
Variables
   a_objval   auxiliary variable for the objective function
   obj        auxiliary variable during the construction of the payoff table
Positive Variables
   sl(k)      slack or surplus variables for the eps-constraints
Equations
   con_obj(k) constrained objective functions
   augm_obj   augmented objective function to avoid weakly efficient solutions
   allobj     all the objective functions in one expression;

con_obj(km1)..   z(km1) - dir(km1)*sl(km1) =e= rhs(km1);

* We optimize the first objective function and put the others as constraints
* the second term is for avoiding weakly efficient points
augm_obj..
  sum(k1,dir(k1)*z(k1))+1e-3*sum(km1,sl(km1)/(maxobj(km1)-minobj(km1))) =e= a_objval;

allobj..  sum(kk, dir(kk)*z(kk)) =e= obj;

Model mod_payoff    / example, allobj / ;
Model mod_epsmethod / example, con_obj, augm_obj / ;

option limrow=0, limcol=0;
option solprint=off, solvelink=%solvelink.CallModule%;

Parameter
  payoff(k,k)  payoff tables entries;
Alias(k,kp);

* Generate payoff table applying lexicographic optimization
loop(kp,
  kk(kp)=yes;
  repeat
    solve mod_payoff using mip maximizing obj;
    payoff(kp,kk) = z.l(kk);
    z.fx(kk) = z.l(kk); // freeze the value of the last objective optimized
    kk(k++1) = kk(k);   // cycle through the objective functions
  until kk(kp); kk(kp) = no;
* release the fixed values of the objective functions for the new iteration
  z.up(k) = inf; z.lo(k) =-inf;
);
if (mod_payoff.modelstat<>%modelstat.Optimal% and mod_payoff.modelstat<>%modelstat.FeasibleSolution%, abort 'no feasible solution for mod_payoff');

display payoff;
minobj(k)=smin(kp,payoff(kp,k));
maxobj(k)=smax(kp,payoff(kp,k));

$set fname p.%gams.scrext%
File fx solution points from eps-method / "%gams.scrdir%%fname%" /;

$if not set gridpoints $set gridpoints 10
Set g grid points /g0*g%gridpoints%/
    grid(k,g) grid
Parameter
    gridrhs(k,g) rhs of eps-constraint at grid point
    maxg(k) maximum point in grid for objective
    posg(k) grid position of objective
    firstOffMax, lastZero some counters
    numk(k) ordinal value of k starting with 1
    numg(g) ordinal value of g starting with 0;
lastZero=1; loop(km1, numk(km1)=lastZero; lastZero=lastZero+1); numg(g) = ord(g)-1;

grid(km1,g) = yes; // Here we could define different grid intervals for different objectives
maxg(km1) = smax(grid(km1,g), numg(g));
gridrhs(grid(km1,g))$(%min%=dir(km1)) = maxobj(km1) - numg(g)/maxg(km1)*(maxobj(km1)- minobj(km1));
gridrhs(grid(km1,g))$(%max%=dir(km1)) = minobj(km1) + numg(g)/maxg(km1)*(maxobj(km1)- minobj(km1));

* Walk the grid points and take shortcuts if the model becomes infeasible
posg(km1) = 0;
repeat
  rhs(km1) = sum(grid(km1,g)$(numg(g)=posg(km1)), gridrhs(km1,g));
  solve mod_epsmethod maximizing a_objval using mip;

$ontext
  if (mod_epsmethod.modelstat<>%modelstat.Optimal%,  // not optimal is in this case infeasible
    lastZero = 0; loop(km1$(posg(km1)>0 and lastZero=0), lastZero=numk(km1));
    posg(km1)$(numk(km1)<=lastZero) = maxg(km1); // skip all solves for more demanding values of rhs(km1)
  else
$offtext
    loop(k, put fx z.l(k):12:2);
put /;

* Proceed forward in the grid
  firstOffMax = 0;
  loop(km1$(posg(km1)<maxg(km1) and firstOffMax=0), posg(km1)=posg(km1)+1; firstOffMax=numk(km1));
  posg(km1)$(numk(km1)<firstOffMax) = 0;
until sum(km1$(posg(km1)=maxg(km1)),1)=card(km1) and firstOffMax=0;
putclose fx; // close the point file

* Get unique solutions from the point file using some Posix Tools (awk, (g)sort, uniq) that come with GAMS
$set awkscript awk.%gams.scrext%
file fa / "%gams.scrdir%%awkscript%" /; put fa 'BEGIN { printf("Table solutions(*,*)\n$ondelim\nsol';
loop(k, put ',' k.tl:0); putclose '\n"); }' / '{ print NR,$0 }' / 'END { print ";" }';
$if     %system.filesys% == UNIX execute 'cd "%gams.scrdir%" && sort %fname% | uniq | awk -f %awkscript% > g.%gams.scrext% && gams g.%gams.scrext% o=gx.%gams.scrext% lo=0 gdx=soleps';
$if NOT %system.filesys% == UNIX execute 'cd "%gams.wdir%" && gsort %fname% | uniq | awk -f %awkscript% > g.%gams.scrext% && gams g.%gams.scrext% o=gx.%gams.scrext% lo=0 gdx=soleps';
* execute 'mv -f "%gams.scrdir%soleps.gdx" .';

Set s Solutions /1*50/; Parameter solutions(s,k) Unique solutions;
execute_load 'soleps', solutions;
File output /solutions.csv/;
put output;
loop((s,k), put solutions(s,k));
display solutions;


$exit
* The display should produce a table with 18 unique solutions
