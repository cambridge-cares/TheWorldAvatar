Sets
p loadpoint  /p1*p179/
s potential sites   /s1*s31/
lp The column labels for Loadpoints /xp,yp,ap,Dp,rhop/
ls The column labels for sites /xs,ys,as,dcs/
i  number of units/i1*i12/
t  Type of SMRs/t1*t2/;
*Here x and y represent the coordpnates.
*a represent the area of landlot or loadpopnt
*D represnts the power demand at that load popnt
*rhop represent the populatpon denspty at

Parameters
* The array contapnpng the data of all the landlots ps
cs(s,ls)
*The array contapnpng the data of all the loadpoints ps
cp(p,lp)
* The dpstance between the pth loadpopnt and sth landlot ps (pn m)
d(p,s)
*Base cost of one unit of type t SMR
Co(t)  /t1 300,
         t2 1350/
*The cost of i units is (in millions of USD )
*Assuming cost per unit decreases linearly with increase in number of units
C(i,t)
*The min area required for one unit of modular power plant (sq.m)
UAo(t) /t1 21000,
       t2  61000 /
*Min area(m2) required for i units of type t SMR is
UA(i,t)
*The capacity of one unit of type t(pn MW)
Fo(t) /t1 50
      t2 225/
*The capacity of i units of type t
F(i,t)

*The volumetric flow req for one inti of each type of SMR (m3/h)
Q(t) /t1 2000
      t2 2200/;


Loop((i,t),
UA(i,t)=ord(i)*UAo(t);
F(i,t)= ord(i)*Fo(t);
if (ord(t)=1,
C(i,t)=ord(i)*((-50/11)*ord(i)+304.55);
else
if(ord(i)<=6,
C(i,t)=ord(i)*Co(t);
else
C(i,t)=1000000;);
););

*The project life span is given as (years)
scalar L /30/;
*Discount rate as fraction
scalar Ds /.02/;
*Distaince to loos conversion factor
scalar alpha /5/;
*Neighbourhood radius for 1MW plant
scalar ro /200/;
*Prob of reactor failure
scalar FP /.0002985/;
*Monetary value of human life
scalar Hu /7000000/;

*Readpng the data from excel fples for cp and cs
*$call GDXXRW Landlots.xlsx trace=3 par=cs rng=Sheet1!a1:e28 rdim=1 cdim=1
$GDXIN Landlots.gdx
$Load cs
$GDXIN

*$call GDXXRW Loadpoints.xlsx trace=3 par=cp rng=Sheet1!a1:f180 rdim=1 cdim=1
$GDXIN Loadpoints.gdx
$Load cp
$GDXIN


*Calculatpng the distance between the plant and load (pn m)
d(p,s) = 100000*(sqrt(sqr(cs(s,'ys')-cp(p,'yp'))+sqr(cs(s,'xs')-cp(p,'xp'))));



*Calculatpng the risk factor for areas pnspde the meltdown radpus.
*Loop((s,p),
*if (d(p,s)<5000,
*r(s)=(cp(p,'ap')*cp(p,'rhop'))/d(p,s);

*else
*r(s)=0;);
*r(s+1)=r(s)+r(s-1);
*);


*Power delivered by the sth plant to the pth loadpopnt ps defpned by
Positive variable fr(p,s);
*Determpnes wether the landlot ps chosen
binary variable y(i,t,s);
*Determpnes wether the landlot ps chosen
binary variable z(s);
*Total capital cost for the sth site
Positive variable TC(s);
*Annualised capital cost for the sth site
Positive variable AC(s);
*Capital cost of tth type SMR at site s
Positive variable Ct(t,s);
*Transmission loss function
Positive variable tr(p,s);
*The cost per unit length of the pipeline
Positive variable mu(s);
*The piping cost
Positive variable PC(s);
*The neighbourhood radius for site s
Positive variable r(s);
*The neighbourhood risk function for site s
Positive variable RN(s);

variable o;

Equations
demand(p)    Each loadpoints demand should be met
supply(s)    The supply should meet the demand
surrogate    Surrogate constraint
ACapita(s)   Annualised capital cost for the sth site
Capita(t,s)    Capital cost of sth site
objective    The objective of  the problem
bnary(s,t)     To ensure the smooth functonpng of bpnary varpable
bnary2(s)      For zs
bnary3(t,s)
Area(s)      Area requirement
Transmission(p,s) Transmission losses
Pcostl(s)       Cost per unit of the pipeline
TPcost(s)       Total pipeline cost
cradius(s)      Critical radius for site s
neighn(s)       Neighbourhood risk fn;


demand(p)..    sum(s,fr(p,s))=g=cp(p,'Dp') ;
supply(s)..    sum(p,fr(p,s))=l= sum((i,t),F(i,t)*y(i,t,s));
surrogate..    sum((i,t,s),F(i,t)*y(i,t,s))=g= sum(p,cp(p,'Dp')) ;
Capita(t,s)..     sum(i,y(i,t,s)*C(i,t))=e=Ct(t,s);
ACapita(s)..    AC(s)=e=sum(t,Ct(t,s))*(Ds/(1-(1+Ds)**(-L)));
bnary(s,t)..      sum(i,y(i,t,s))=l=1;
bnary2(s)..         z(s)=l= sum((i,t),y(i,t,s));
bnary3(t,s)..         z(s)=g=sum(i,y(i,t,s));
Area(s)..         sum((i,t),y(i,t,s)*UA(i,t))=l= cs(s,'as');
Transmission(p,s).. alpha* d(p,s)*fr(p,s)*z(s)=e=tr(p,s) ;
Pcostl(s)..        mu(s)=e=96*sum((i,t),(sqrt(Q(t)*ord(i)*y(i,t,s))));
TPcost(s)..         PC(s)=e=(Ds/(1-(1+Ds)**(-L)))*mu(s)*cs(s,'dcs') ;
cradius(s)..        r(s)=e=ro*sum((i,t),sqrt(F(i,t)*y(i,t,s)));
neighn(s)..          RN(s)=e= (Ds/(1-(1+Ds)*(-L)))*FP*Hu*sum(p,cp(p,'ap')*cp(p,'rhop')$(d(p,s)<5000));


objective..    o=e=sum((p,s),tr(p,s))+ sum(s,AC(s))+sum(s,PC(s))+sum(s,RN(s)) ;



Model optim /all/;
option minlp=baron,resLIM=10000,optCR=.05;
Solve optim using minlp minimizing o;
Display y.l,fr.l,AC.l,tr.l,PC.l,r.l,mu.l,RN.l,o.l,z.l,Ct.l;




