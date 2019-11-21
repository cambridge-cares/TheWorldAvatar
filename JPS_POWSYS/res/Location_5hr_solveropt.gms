  Sets
p loadpoint  /p1*p208/
e exiisting generator
ep existing generator parameters
s potential sites   /s1*s31/
lp The column labels for Loadpoints /xp,yp,Dp,rhop/
ls The column labels for sites /ys,xs,as,dcs/
i  number of units/1*12/
t  Type of SMRs/t1*t2/
rp Column labels for reactor types /Co,UAo,Fo,Q/
crp  row labels for the constant parameters /L,Ds,alpha,ro,FP,Hu/
out output file /units,type,site/;


Parameters
* The array contapnpng the data of all the landlots ps
cs(s,ls)
*The array contapnpng the data of all the loadpoints ps
cp(p,lp)
*The array contapnpng the data of all reactor parameters
cr(t,rp)
*The array contapnpng the data of all exisitng generator parameters
eg(e,ep)
*The array contapnpng the data of all constants used
cc(crp)
* The dpstance between the pth loadpopnt and sth landlot ps (pn m)
d(p,s)
* The dpstance between the pth loadpopnt and eth exisitng generator
de(p,e)
*Assuming cost per unit decreases linearly with increase in number of units
C(i,t)
*Min area(m2) required for i units of type t SMR is
UA(i,t)
*The capacity of i units of type t
F(i,t) ;



*Readpng the parameter data from excel files
$call csv2gdx parameters_req.csv id=cr index=1 values=2..lastCol useHeader=y
$GDXIN parameters_req.gdx
$Load cr
$GDXIN

*Readpng the constants data from excel files
$call csv2gdx constants_req.csv id=cc index=1 values=2..lastCol useHeader=y
$GDXIN constants_req.gdx
$Load cc
$GDXIN

*Readpng existing generator parameter files from excel files
$call csv2gdx parameters_req_existing.csv id=eg index=1 values=2..lastCol useHeader=y
$GDXIN parameters_req_existing.gdx
$Load e=Dim1
$Load ep=Dim2
$Load eg
$GDXIN

Loop((i,t),
UA(i,t)=ord(i)*cr(t,'UAo');
if (ord(t)=1,
C(i,t)=ord(i)*((-50/11)*ord(i)+304.55);
else
if(ord(i)<=6,
F(i,t)= ord(i)*cr(t,'Fo');
C(i,t)=ord(i)*cr(t,'Co');
else
C(i,t)=1000000;
F(i,t)=0;);
););



*Readpng the data from excel fples for cp and cs
*$call GDXXRW Landlots.xlsx trace=3 par=cs rng=Sheet1!a1:e28 rdim=1 cdim=1
$call csv2gdx inputlandlots.csv id=cs index=1 values=2..lastCol useHeader=y
$GDXIN inputlandlots.gdx
$Load cs
$GDXIN

$call csv2gdx inputloadpoints.csv id=cp index=1 values=2..lastCol useHeader=y
*$call GDXXRW Loadpoints.xlsx trace=3 par=cp rng=Sheet1!a1:e209 rdim=1 cdim=1
$gdxin inputloadpoints.gdx
$Load cp
$GDXIN



*Calculating the distance between the plant and load (pn m)
d(p,s) = 100000*(sqrt(sqr(cs(s,'xs')-cp(p,'yp'))+sqr(cs(s,'ys')-cp(p,'xp'))));

Display cp,eg;

*Calculating the distance between the exisitng generator and load
de(p,e) = 100000*(sqrt(sqr(eg(e,'X')-cp(p,'yp'))+sqr(eg(e,'Y')-cp(p,'xp'))));

Display de;

*Power delivered by the sth plant to the pth loadpopnt ps defpned by
Positive variable fr(p,s);
*Power delivered by the eth existing generator to the pth loadpopnt ps defpned by
Positive variable fre(p,e);
*Determpnes wether the landlot ps chosen
binary variable y(i,t,s);
*Total capital cost for the sth site
Positive variable TC(s);
*Annualised capital cost for the sth site
Positive variable AC(s);
*Capital cost of tth type SMR at site s
Positive variable Ct(t,s);
*Transmission loss function
Positive variable tr(p,s);
*Transmission loss function for exisitng generator
Positive variable tre(p,e);
*The cost per unit length of the pipeline
Positive variable mu(s);
*The piping cost
Positive variable PC(s);
*The neighbourhood radius for site s
Positive variable r(s);
*The neighbourhood risk function for site s
Positive variable RN(s);

variable z;
z.up=10000000000000000;
z.lo=10000000000;

Equations
demand(p)    Each loadpoints demand should be met
supply(s)    The supply should meet the demand
supply_e(e) Supply from exisitng generator
tsupply     Total supply
*surrogate    Surrogate constraint
ACapita(s)   Annualised capital cost for the sth site
Capita(t,s)    Capital cost of sth site
objective    The objective of  the problem
bnary(s,t)     To ensure the smooth functonpng of bpnary varpable
Area(s)      Area requirement
Transmission(p,s) Transmission losses
Transmission_e(p,e) Transmission losses for exisitng generators
Pcostl(s)       Cost per unit of the pipeline
TPcost(s)       Total pipeline cost
cradius(s)      Critical radius for site s
neighn(s)       Neighbourhood risk fn;


demand(p)..    sum(s,fr(p,s))+sum(e,fre(p,e))=g=cp(p,'Dp') ;
supply(s)..    sum(p,fr(p,s))=l= sum((i,t),F(i,t)*y(i,t,s));
supply_e(e)..  sum(p,fre(p,e))=l= eg(e,'Co');
tsupply..      sum((p,s),fr(p,s))+sum((p,e),fre(p,e))=l= sum((i,t,s),F(i,t)*y(i,t,s))+sum(e,eg(e,'Co'));
*surrogate..    sum((i,t,s),F(i,t)*y(i,t,s))=g= sum(p,cp(p,'Dp')) ;
Capita(t,s)..     sum(i,y(i,t,s)*C(i,t))=e=Ct(t,s);
ACapita(s)..    AC(s)=e=sum(t,Ct(t,s))*(cc('Ds')/(1-(1+cc('Ds'))**(-cc('L'))));
bnary(s,t)..      sum(i,y(i,t,s))=l=1;
Area(s)..         sum((i,t),y(i,t,s)*UA(i,t))=l= cs(s,'as');
Transmission(p,s).. sum((i,t), cc('alpha')* d(p,s)*fr(p,s)*y(i,t,s))=e=tr(p,s) ;
Transmission_e(p,e)..  de(p,e)*fre(p,e)=e=tre(p,e) ;
Pcostl(s)..        mu(s)=e=96*sum((i,t),(sqrt(cr(t,'Q')*ord(i)*y(i,t,s))));
TPcost(s)..         PC(s)=e=(cc('Ds')/(1-(1+cc('Ds'))**(-cc('L'))))*mu(s)*cs(s,'dcs') ;
cradius(s)..        r(s)=e=cc('ro')*sum((i,t),sqrt(F(i,t)*y(i,t,s)));
neighn(s)..          RN(s)=e= (cc('Ds')/(1-(1+cc('Ds'))*(-cc('L'))))*cc('FP')*cc('Hu')*sum(p,cp(p,'rhop')$(d(p,s)<5000));



objective..    z=e=sum((p,s),tr(p,s))+sum((p,e),tre(p,e))+ sum(s,AC(s))+sum(s,PC(s))+sum(s,RN(s)) ;


Model optim /all/;

option minlp=baron,resLIM=100000,optCR=.9,decimals=6;
optim.optfile=1;
Solve optim using minlp minimizing z;

File output /results.csv/;
*output.nr = 2  ;
output.nd = 1 ;
*output.nw = 0  ;
put output;
output.pc=5;
Display y.l,fr.l;
put output 'Site location','Reactor type','Number of units','Capacity of 1 unit(MW)','X coordinate','Y coordinate'/;
loop((i,t,s), put$(y.l(i,t,s)=1),s.tl,t.tl,i.tl,cr(t,'Fo'),cs(s,'xs'),cs(s,'ys')/);






