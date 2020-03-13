clc
clear
close all

%_____________________Parameter (Scalar)___________________________________

%transport
transport=readmatrix('transport.csv');
Unit_transport_capacity=transport(1);%ton/truck
Unit_transport_cost=transport(2);%$/km
pollutionTransportTax=transport(3);
dieselConsTruck=transport(4); %gCO2/km
%onsite
n_unit_onsite_max=10; % Onsite only AD avaialble, maximum 10 units
Unit_capacity_onsite=1; % Onsite only AD avaialble, maximum capacity 1kg/day
%costs

%Unit_installation_cost_onsite=117000;%$/unit capacity
Unit_installation_cost_onsite=readmatrix('Unit installation cost (onsite).csv');%$/unit capacity
%Unit_operation_cost_onsite=36;%$/unit capacity
Unit_operation_cost_onsite=readmatrix('Unit operation cost (onsite).csv');%$/unit capacity
%Pollution_treatment_tax_onsite=0;%$/ton
Pollution_treatment_tax_onsite=readmatrix('Pollution treatment tax (onsite).csv');%$/ton of CO2 equivalent
%Product_Conversion_onsite=0;% Electricity kWh/ton waste
Product_Conversion_onsite=readmatrix('Conversion rate (onsite).csv');% Electricity kWh/ton waste Amount of recovered energy/material/ton of processed waste
%Resource_conversion_onsite=0; % Electricity consumption for production
Resource_conversion_onsite=readmatrix('Resource conversion (onsite).csv');% kWh/ton, Electricity consumption for production

Unit_land_cost_onsite=500; %$/unit capacity
Unit_land_cost_offsite=300;%$/unit capacity
Unit_manpower_cost=0; % manpower cost for both onsite and offsite
Unit_resource_cost_electricity=0.1; %electricity

EOS=1; % Economy of scale


daysperyear=365;

%_____________________Parameter (Array)____________________________________

% locaton information
site=readmatrix('Site_xy.csv');% foodcourt
location=readmatrix('Location.csv');% offsite facilities
waste=readmatrix('Waste.csv')/1000;% waste in tons/day
n_unit_offsite_max=readmatrix('n_unit_max_offsite.csv');%max number of unit for each site
% cost information
Unit_capacity_offsite=readmatrix('Unit_capacity_offsite.csv');%unit capacity for each tech
Unit_installation_cost_offsite=readmatrix('Unit installation cost (offsite).csv');%$/unit capacity
Unit_operation_cost_offsite=readmatrix('Unit operation cost (offsite).csv');%$/unit capacity
Pollution_treatment_tax_offsite=readmatrix('Pollution treatment tax (offsite).csv');%$/ton of CO2 equivalent
% conversion information
Product_Conversion_offsite=readmatrix('Conversion rate (offsite).csv');% Electricity kWh/ton waste Amount of recovered energy/material/ton of processed waste
Resource_conversion_offsite=readmatrix('Resource conversion (offsite).csv');% kWh/ton, Electricity consumption for production

%_____________________Parameter Manipulation_______________________________
n_foodcourt=size(site,1);
[n_tech,n_offsite]=size(n_unit_offsite_max);

     
Distance=zeros(n_foodcourt,n_offsite);
for i=1:n_foodcourt
    for j=1:n_offsite
      Distance(i,j)= SphereDist(site(i,:),location(j,:));  
    end
end

Distance_onsite=zeros(n_foodcourt, n_foodcourt);
for i=1:n_foodcourt
    for j=1:n_foodcourt
       Distance_onsite(i,j)=SphereDist(site(i,:),site(j,:)); 
    end
end

%_________________________Variables________________________________________

cvx_begin

variable y_offsite(n_foodcourt,n_offsite,n_tech) binary; 
variable y_onsite(n_foodcourt,n_foodcourt) binary; %mass per day
variable n_unit_offsite(n_tech,n_offsite) integer;  %number of units
variable n_unit_onsite(n_foodcourt) integer; %number of units

%_________________________Equations________________________________________

a=repmat(Unit_capacity_offsite,1,n_offsite);
b=repmat(waste,1,n_offsite,n_tech);
c=repmat(waste,1,n_foodcourt);

m_onsite=y_onsite.*c;
m_offsite=y_offsite.*b;
m_offsite_jk=squeeze(sum(m_offsite,1));
m_offsite_tech=squeeze(sum(sum(m_offsite,2),1));
m_offsite_ij=squeeze(sum(m_offsite,3));
m_offsite_foodcourt=sum(sum(m_offsite,3),2);

actual_capacity_onsite=Unit_capacity_onsite*n_unit_onsite;%109*1?t
actual_capacity_offsite=a.*n_unit_offsite;                %6*3,t

%product
elec_offsite=daysperyear*sum(Product_Conversion_offsite.*m_offsite_tech);%1,t
elec_onsite=daysperyear*sum(Product_Conversion_onsite*sum(m_onsite,1));%1,t

% Revenue Parameters
revenue_offsite=Unit_resource_cost_electricity*elec_offsite;%1,t
revenue_onsite=Unit_resource_cost_electricity*elec_onsite;%1,t
revenue=revenue_offsite+revenue_onsite;%1,t

%cost
% CAPEXProduct_Conversion_offsite
Installation_cost_Onsite=sum(Unit_installation_cost_onsite*actual_capacity_onsite);%109->1,t
Installation_cost_Offsite=sum(Unit_installation_cost_offsite.'*actual_capacity_offsite);%(1*6) * (6*3)->1,t
Installation_cost=Installation_cost_Onsite+Installation_cost_Offsite;%1,t

% Operation
CTRSt_offsite=daysperyear*sum(sum(Unit_transport_cost*Distance.*(m_offsite_ij/Unit_transport_capacity)));% Transport (109*3)->(1*3)->1
CTRSt_onsite=daysperyear*sum(sum(Unit_transport_cost*Distance_onsite.*(m_onsite/Unit_transport_capacity)));% Transport (109*3)->(1*3)->1
CTRSt=CTRSt_offsite+CTRSt_onsite;

COMt_onsite=daysperyear*sum(Unit_operation_cost_onsite*actual_capacity_onsite);%109->1,t
COMt_offsite=daysperyear*sum(Unit_operation_cost_offsite.'*actual_capacity_offsite); % O&M (1*6) * (6*3)->1,t
COMt=COMt_onsite+COMt_offsite;

CLDt_onsite=sum(Unit_land_cost_onsite*actual_capacity_onsite);%109->1,t
CLDt_offsite=sum(sum(Unit_land_cost_offsite*actual_capacity_offsite)); % O&M (6*3)->(1*3)->1,t
CLDt=CLDt_onsite+CLDt_offsite;
 
CMPt_onsite=daysperyear*sum(Unit_manpower_cost*actual_capacity_onsite);%109->1,t
CMPt_offsite=daysperyear*sum(sum(Unit_manpower_cost*actual_capacity_offsite));% (6*3)->(1*3)->1,t
CMPt=CMPt_onsite+CMPt_offsite;%manpower cost

CPLtreatmentt_onsite=sum(sum(Pollution_treatment_tax_onsite*m_onsite));
CPLtreatmentt_offsite=sum(Pollution_treatment_tax_offsite.*m_offsite_tech);% (6*1)->1 pollution cost
CPLtreatmentt=CPLtreatmentt_onsite+CPLtreatmentt_offsite;%treatment pollution

CPLtransportt_onsite=daysperyear*sum(sum(pollutionTransportTax * dieselConsTruck*Distance_onsite.*(m_onsite/Unit_transport_capacity)));%(109*3)->(1*3)->1
CPLtransportt_offsite=daysperyear*sum(sum(pollutionTransportTax * dieselConsTruck*Distance.*(m_offsite_ij/Unit_transport_capacity)));%(109*3)->(1*3)->1
CPLttransprtt=CPLtransportt_onsite+CPLtransportt_offsite;% transport pollution
CPLt=CPLtreatmentt+CPLttransprtt;

CRCt_onsite=sum(Unit_resource_cost_electricity*Resource_conversion_onsite*sum(m_onsite,1)); %  109->1,t
CRCt_offsite=sum(Unit_resource_cost_electricity*Resource_conversion_offsite.*m_offsite_tech); % (6*1)->1
CRCt=CRCt_onsite+CRCt_offsite;

cost = CTRSt + COMt + CLDt + CMPt + CPLt + CRCt;

% Profit
i=0.01;%discount rate
NPV=-Installation_cost;
for t=1:15
NPV = NPV+(revenue-cost)/(1+i)^t; % $/day; CAPEX converted to daily CAPEX, 25 years, 365 days/y
end
%_________________________Optimization_____________________________________
   

        maximize(NPV)
        
        subject to
            n_unit_offsite>=0;
            n_unit_onsite>=0;
            n_unit_offsite <= n_unit_offsite_max;
            n_unit_onsite <= n_unit_onsite_max;
            y_onsite>=0;
            y_offsite>=0;
            y_onsite<=1;
            y_offsite<=1;
 
            sum(y_onsite,2)+squeeze(sum(sum(y_offsite,2),3))==1;
            m_offsite_jk.'<=actual_capacity_offsite;
            sum(m_onsite,1).'<=actual_capacity_onsite;

 cvx_solver mosek;
 
 cvx_end 

%_________________________Benchmark________________________________________ 
incinerationCapacity=1095000;
incinerationCapexCost=890000000;
operationCostBencmark=56540000;
landCostBenchmark=2700000;
manpowerCostBenchmark=0;
pollutionTreatmentTax=150;
Incineration=[103.620614,1.29591];
resourceConsumptionBencmark=70;
revFromWTFOffSite=0.1;
transRateList=290;

DistanceBenchmark=zeros(n_foodcourt,1);
for i=1:n_foodcourt
DistanceBenchmark(i)=SphereDist(site(i,:),Incineration); 
end

BenchmarkCapex = (daysperyear*sum(waste) / incinerationCapacity) * incinerationCapexCost ;      
      
    BenchmarkTransportation = daysperyear * sum(Unit_transport_cost * DistanceBenchmark .* ceil(waste / Unit_transport_capacity));
    BenchmarkTransportPollution = daysperyear * sum(pollutionTransportTax * dieselConsTruck * DistanceBenchmark.* ceil(waste/ Unit_transport_capacity)) ;

    BenchmarkOperMgnt =(daysperyear*sum(waste) / incinerationCapacity) * operationCostBencmark ;
    BenchmarkLand = daysperyear*(sum(waste) / incinerationCapacity) * landCostBenchmark ;
    BenchmarkManpower = (daysperyear*sum(waste) / incinerationCapacity) * manpowerCostBenchmark ;
    BenchmarkTreatmentPollution = daysperyear*sum(waste) * pollutionTreatmentTax ;
    BenchmarkPollution=BenchmarkTreatmentPollution+BenchmarkTransportPollution;
    BenchmarkResourceConsumption = daysperyear*sum(waste) * resourceConsumptionBencmark * Unit_resource_cost_electricity;
    BenchmarkRevenue = daysperyear*sum(waste) * revFromWTFOffSite * transRateList;

    Benchmarkcost=BenchmarkResourceConsumption+BenchmarkTreatmentPollution+BenchmarkManpower+BenchmarkLand+BenchmarkOperMgnt+BenchmarkTransportPollution+BenchmarkTransportation;

NPV_Benchmark=-BenchmarkCapex;
for t=1:15
NPV_Benchmark=NPV_Benchmark+(BenchmarkRevenue-Benchmarkcost)/(1+i)^t;
end

economicoutput=[revenue,BenchmarkRevenue;revenue,BenchmarkRevenue;Installation_cost,BenchmarkCapex;COMt,BenchmarkOperMgnt;CMPt,BenchmarkManpower;CLDt,BenchmarkLand;CPLt,BenchmarkPollution;CTRSt,BenchmarkTransportation;CRCt,BenchmarkResourceConsumption;cost,Benchmarkcost;NPV,NPV_Benchmark];

writematrix(n_unit_onsite,'number of units (onsite).csv');% no unit
writematrix(n_unit_offsite,'number of units (offsite).csv');% no unit
writematrix(m_onsite,'Treated waste (onsite).csv');% ton,row-from foodcourt i waste generation, column-to foodcourt j for treatment
writematrix(m_offsite,'Treated waste (offsite).csv');% no unit,row-from foodcourt i waste generation, column-to waste facility j for treatment
writematrix(y_onsite,'Waste flow relation (onsite).csv');% no unit,row-from foodcourt i waste generation, column-to foodcourt j for treatment
writematrix(y_offsite,'Waste flow relation (offsite).csv');% ton,row-from foodcourt i waste generation, column-to waste facility j for treatment
writematrix(economicoutput,'Economic output.csv');% $