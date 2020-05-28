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
%other parameters csv not found (and does not affect results for now)
Unit_land_cost_onsite=500; %$/unit capacity
Unit_land_cost_offsite=300;%$/unit capacity
Unit_manpower_cost=0; % manpower cost for both onsite and offsite

EOS=1; % Economy of scale


daysperyear=365;

%_____________________Parameter (Array)____________________________________

% location information
site=readmatrix('Site_xy.csv');% foodcourt
location=readmatrix('Location.csv');% offsite facilities
waste=readmatrix('Waste.csv')/1000;% waste in tons/day
%offsite
n_unit_offsite_max=readmatrix('n_unit_max_offsite.csv');%max number of unit for each site
% cost information
Unit_capacity_offsite=readmatrix('Unit_capacity_offsite.csv');%unit capacity for each tech
Unit_installation_cost_offsite=readmatrix('Unit installation cost (offsite).csv');%$/unit capacity
Unit_operation_cost_offsite=readmatrix('Unit operation cost (offsite).csv');%$/unit capacity
Pollution_treatment_tax_offsite=readmatrix('Pollution treatment tax (offsite).csv');%$/ton of CO2 equivalent
% conversion information
Product_Conversion_offsite=readmatrix('Conversion rate (offsite).csv');% Electricity kWh/ton waste Amount of recovered energy/material/ton of processed waste
Resource_conversion_offsite=readmatrix('Resource conversion (offsite).csv');% kWh/ton, Electricity consumption for production

%onsite
n_unit_onsite_max=10; % Onsite only AD available, maximum 10 units
Unit_capacity_onsite=readmatrix('Unit_capacity_onsite.csv');%unit capacity for each tech
%cost information
Unit_installation_cost_onsite=readmatrix('Unit installation cost (onsite).csv');%$/unit capacity
Unit_operation_cost_onsite=readmatrix('Unit operation cost (onsite).csv');%$/unit capacity
Pollution_treatment_tax_onsite=readmatrix('Pollution treatment tax (onsite).csv');%$/ton of CO2 equivalent
%conversion information
Product_Conversion_onsite=readmatrix('Conversion rate (onsite).csv');% Electricity kWh/ton waste Amount of recovered energy/material/ton of processed waste
Resource_conversion_onsite=readmatrix('Resource conversion (onsite).csv');% kWh/ton, Electricity consumption for production

Unit_resource_cost=0.1; %electricity
%_______________________________ Clustering _______________________________
[n_tech,n_offsite]=size(n_unit_offsite_max);
n_tech_onsite=size(Unit_capacity_onsite,1);
n_foodcourt=size(site,1); 

if n_foodcourt<=8
    Distance=zeros(n_foodcourt,n_offsite);
    for i=1:n_foodcourt
        for j=1:n_offsite
          Distance(i,j)= real(SphereDist(site(i,:),location(j,:)));  
        end
    end

    Distance_onsite=zeros(n_foodcourt, n_foodcourt);
    for i=1:n_foodcourt
        for j=1:n_foodcourt
           Distance_onsite(i,j)=real(SphereDist(site(i,:),site(j,:))); 
        end
    end

    %_________________________Variables________________________________________

    cvx_begin

    variable y_offsite(n_foodcourt,n_offsite,n_tech) binary; 
    variable y_onsite(n_foodcourt,n_foodcourt,n_tech_onsite) binary; %mass per day
    variable n_unit_offsite(n_tech,n_offsite) integer;  %number of units
    variable n_unit_onsite(n_tech_onsite,n_foodcourt) integer; %number of units
    %_________________________Equations________________________________________

    a=repmat(Unit_capacity_offsite,1,n_offsite);
    a1=repmat(Unit_capacity_onsite,1,n_foodcourt);
    b=repmat(waste,1,n_offsite,n_tech);
    b1=repmat(waste,1,n_foodcourt,n_tech_onsite);
    c=repmat(n_unit_onsite_max,1,n_foodcourt);

    m_offsite=y_offsite.*b;
    m_offsite_jk=squeeze(sum(m_offsite,1));
    m_offsite_tech=squeeze(sum(sum(m_offsite,2),1));
    m_offsite_ij=squeeze(sum(m_offsite,3));
    m_offsite_foodcourt=sum(sum(m_offsite,3),2);

    m_onsite=y_onsite.*b1;
    m_onsite_jk=squeeze(sum(m_onsite,1));
    m_onsite_tech=squeeze(sum(sum(m_onsite,2),1));
    m_onsite_ij=squeeze(sum(m_onsite,3));
    m_onsite_foodcourt=sum(sum(m_onsite,3),2);

    actual_capacity_offsite=a.*n_unit_offsite;                %6*3,t
    actual_capacity_onsite=a1.*n_unit_onsite;                %3*109,t

    %product
    elec_offsite=daysperyear*m_offsite_tech.'*Product_Conversion_offsite;%1,t
    elec_onsite=daysperyear*m_onsite_tech.'*Product_Conversion_onsite;%1,t
    % Revenue Parameters
    revenue_offsite=Unit_resource_cost*elec_offsite.';%1,t
    revenue_onsite=Unit_resource_cost*elec_onsite.';%1,t
    revenue=revenue_offsite+revenue_onsite;%1,t

    %cost
    % CAPEXProduct_Conversion_offsite
    Installation_cost_Onsite=sum(Unit_installation_cost_onsite.'*actual_capacity_onsite);%(1*6) * (6*3)->1,t
    Installation_cost_Offsite=sum(Unit_installation_cost_offsite.'*actual_capacity_offsite);%(1*6) * (6*3)->1,t
    Installation_cost=Installation_cost_Onsite+Installation_cost_Offsite;%1,t

    % Operation
    CTRSt_offsite=daysperyear*sum(sum(Unit_transport_cost*Distance.*(m_offsite_ij/Unit_transport_capacity)));% Transport (109*3)->(1*3)->1
    CTRSt_onsite=daysperyear*sum(sum(Unit_transport_cost*Distance_onsite.*(m_onsite_ij/Unit_transport_capacity)));% Transport (109*3)->(1*3)->1
    CTRSt=CTRSt_offsite+CTRSt_onsite;

    COMt_onsite=daysperyear*sum(Unit_operation_cost_onsite.'*actual_capacity_onsite);%109->1,t
    COMt_offsite=daysperyear*sum(Unit_operation_cost_offsite.'*actual_capacity_offsite); % O&M (1*6) * (6*3)->1,t
    COMt=COMt_onsite+COMt_offsite;

    CLDt_onsite=sum(sum(Unit_land_cost_onsite*actual_capacity_onsite));%109->1,t
    CLDt_offsite=sum(sum(Unit_land_cost_offsite*actual_capacity_offsite)); % O&M (6*3)->(1*3)->1,t
    CLDt=CLDt_onsite+CLDt_offsite;

    CMPt_onsite=daysperyear*sum(sum(Unit_manpower_cost*actual_capacity_onsite));%109->1,t
    CMPt_offsite=daysperyear*sum(sum(Unit_manpower_cost*actual_capacity_offsite));% (6*3)->(1*3)->1,t
    CMPt=CMPt_onsite+CMPt_offsite;%manpower cost

    CPLtreatmentt_onsite=sum(Pollution_treatment_tax_onsite.*m_onsite_tech);
    CPLtreatmentt_offsite=sum(Pollution_treatment_tax_offsite.*m_offsite_tech);% (6*1)->1 pollution cost
    CPLtreatmentt=CPLtreatmentt_onsite+CPLtreatmentt_offsite;%treatment pollution

    CPLtransportt_onsite=daysperyear*sum(sum(pollutionTransportTax * dieselConsTruck*Distance_onsite.*(m_onsite_ij/Unit_transport_capacity)));%(109*3)->(1*3)->1
    CPLtransportt_offsite=daysperyear*sum(sum(pollutionTransportTax * dieselConsTruck*Distance.*(m_offsite_ij/Unit_transport_capacity)));%(109*3)->(1*3)->1
    CPLttransprtt=CPLtransportt_onsite+CPLtransportt_offsite;% transport pollution
    CPLt=CPLtreatmentt+CPLttransprtt;

    %resource
    resource_offsite=m_offsite_tech.'*Resource_conversion_offsite;%1,t
    resource_onsite=m_onsite_tech.'*Resource_conversion_onsite;%1,t
    % Revenue Parameters
    CRCt_offsite=Unit_resource_cost*resource_offsite.';%1,t
    CRCt_onsite=Unit_resource_cost*resource_onsite.';%1,t
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
                
    %           n_unit_offsite <= 0;
    %           n_unit_onsite <= 0; 
    
                y_onsite>=0;
                y_offsite>=0;
                y_onsite<=1;
                y_offsite<=1;

                sum(y_onsite,2)+squeeze(sum(sum(y_offsite,2),3))==1;
                                
                if n_tech==1
                m_offsite_jk<=actual_capacity_offsite;
                else
                m_offsite_jk.'<=actual_capacity_offsite;
                end

                if n_tech_onsite==1
                m_onsite_jk<=actual_capacity_onsite;
                else
                m_onsite_jk.'<=actual_capacity_onsite;
                end

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
    %resourceConsumptionBencmark=[70,0];%[elec,water]
    resourceConsumptionBencmark=70;%only elec
    %revFromWTFOffSite=[0.1,0];%[elec,water]
    revFromWTFOffSite=0.1;%only elec
    %transRateList=[290,0];%[elec,water]
    transRateList=290;%only elec

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
        BenchmarkResourceConsumption = daysperyear*sum(waste) * resourceConsumptionBencmark* Unit_resource_cost.';
        BenchmarkRevenue = daysperyear*sum(waste) * revFromWTFOffSite* transRateList.';

        Benchmarkcost=BenchmarkResourceConsumption+BenchmarkTreatmentPollution+BenchmarkManpower+BenchmarkLand+BenchmarkOperMgnt+BenchmarkTransportPollution+BenchmarkTransportation;

    NPV_Benchmark=-BenchmarkCapex;
    for t=1:15
    NPV_Benchmark=NPV_Benchmark+(BenchmarkRevenue-Benchmarkcost)/(1+i)^t;
    end

    economicoutput=[revenue,BenchmarkRevenue;revenue,BenchmarkRevenue;Installation_cost,BenchmarkCapex;COMt,BenchmarkOperMgnt;CMPt,BenchmarkManpower;CLDt,BenchmarkLand;CPLt,BenchmarkPollution;CTRSt,BenchmarkTransportation;CRCt,BenchmarkResourceConsumption;cost,Benchmarkcost;NPV,NPV_Benchmark];

    y_onsite=round(y_onsite);
    y_offsite=round(y_offsite);
    m_onsite=round(m_onsite,4);
    m_offsite=round(m_offsite,4);
    n_unit_onsite=round(n_unit_onsite,4);
    n_unit_offsite=round(n_unit_offsite,4);

    Nonsite=nnz(y_onsite);
    Noffsite=nnz(y_offsite);
    disp(y_offsite);
    disp(y_onsite);
    disp(Nonsite);
    disp(Noffsite);
    disp(NPV/NPV_Benchmark);


    writematrix(transpose(n_unit_onsite),'number of units (onsite).csv');% no unit
    writematrix(n_unit_offsite,'number of units (offsite).csv');% no unit
    writematrix(m_onsite,'Treated waste (onsite).csv');% ton,row-from foodcourt i waste generation, column-to foodcourt j for treatment
    writematrix(m_offsite,'Treated waste (offsite).csv');% no unit,row-from foodcourt i waste generation, column-to waste facility j for treatment
    writematrix(y_onsite,'Waste flow relation (onsite).csv');% no unit,row-from foodcourt i waste generation, column-to foodcourt j for treatment
    writematrix(y_offsite,'Waste flow relation (offsite).csv');% ton,row-from foodcourt i waste generation, column-to waste facility j for treatment
    writematrix(economicoutput,'Economic output.csv');% $
else
    waste_original=readmatrix('Waste.csv')/1000;% waste in tons/day
    site_original=readmatrix('Site_xy.csv');% foodcourt

    %n_cluster=readmatrix('Number of clusters.csv');% foodcourt
    n_cluster=4;
    bigM=1000000;

    Distance_onsite=zeros(n_foodcourt, n_foodcourt);
    for i=1:n_foodcourt
        for j=1:n_foodcourt
           Distance_onsite(i,j)=real(SphereDist(site(i,:),site(j,:))); 
        end
    end

    Distance_offsite=zeros(n_foodcourt,n_offsite);
    for i=1:n_foodcourt
        for j=1:n_offsite
          Distance_offsite(i,j)= real(SphereDist(site(i,:),location(j,:)));  
        end
    end

    cvx_begin

    variable x_cluster_allocation1(n_foodcourt, n_foodcourt) binary
    variable yi1(n_foodcourt) binary %constraint

    z1=sum(sum(Distance_onsite.*x_cluster_allocation1));
    columnsum1=sum(x_cluster_allocation1,1);


    minimize(z1)

            subject to

            sum(x_cluster_allocation1,2)==1;
            for i=1:n_foodcourt
               columnsum1(i) <=0+bigM*yi1(i);
            end
            sum(yi1)==n_cluster;

     cvx_solver mosek;
     cvx_end 

    x_cluster_allocation1= round(x_cluster_allocation1);
    columnsum1=sum(x_cluster_allocation1,1);
    ind1=find(columnsum1);
    site_cluster1=site(ind1,:);%----------------
    n_cluster=size(site_cluster1,1);

    %______________________ Parameter manipulation ____________________________

    %% distance load
    %_-----------------onsite------------------------------
    d_onsite_1=sum(repmat(waste,1,n_foodcourt).*Distance_onsite.*x_cluster_allocation1,1);
    d_onsite_cluster=d_onsite_1(ind1)';

    %_-----------------offsite------------------------------

    d_offsite_1=zeros(n_foodcourt,n_offsite);
    for k=1:n_foodcourt
        for i=1:n_foodcourt
            if x_cluster_allocation1(i,k)==1
                d_offsite_1(k,:)=d_offsite_1(k,:)+ waste(i)*Distance_offsite(i,:);
            end
        end
    end
    d_offsite_cluster=d_offsite_1(ind1,:);

    %% waste
    waste_1=full(sum(repmat(waste,1,n_foodcourt).*x_cluster_allocation1,1));
    waste_cluster=waste_1(ind1)';

    %% minimum number of unit for onsite

    if n_tech_onsite==1
        unit_capacity_onsite_expand_cluster=repmat(Unit_capacity_onsite,1,n_cluster)';
        n_onsite_min_cluster=ceil(waste_cluster./unit_capacity_onsite_expand_cluster);

    else 
        waste_cluster_expandtech=repmat(waste_cluster,1,n_tech_onsite);
        unit_capacity_onsite_expand_cluster=repmat(Unit_capacity_onsite,1,n_cluster)';
        n_onsite_min_cluster=ceil(waste_cluster_expandtech./unit_capacity_onsite_expand_cluster);
        n_onsite_min_cluster=permute(repmat(n_onsite_min_cluster,1,1,n_cluster),[1 3 2]);
    end 

    %% 
    %_________________________Variables________________________________________

    cvx_begin

    variable y_offsite(n_cluster,n_offsite,n_tech) binary; 
    variable y_onsite(n_cluster,n_tech_onsite) binary; %mass per day
    variable n_unit_offsite(n_tech,n_offsite) integer;  %number of units
    variable n_unit_onsite(n_tech_onsite,n_cluster) integer; %number of units

    %_________________________Equations________________________________________

    a=repmat(Unit_capacity_offsite,1,n_offsite);
    a1=repmat(Unit_capacity_onsite,1,n_cluster);
    b=repmat(waste_cluster,1,n_offsite,n_tech);
    b1=repmat(waste_cluster,1,n_cluster,n_tech_onsite);
    c=repmat(n_unit_onsite_max,1,n_cluster);

    m_offsite=y_offsite.*b;
    m_offsite_jk=squeeze(sum(m_offsite,1));
    m_offsite_tech=squeeze(sum(sum(m_offsite,2),1));
    m_offsite_ij=squeeze(sum(m_offsite,3));
    m_offsite_foodcourt=sum(sum(m_offsite,3),2);

    m_onsite=repmat(waste_cluster,1,n_tech_onsite).*y_onsite;
    m_onsite_tech=sum(m_onsite,1);

    actual_capacity_offsite=a.*n_unit_offsite;                %6*3,t
    actual_capacity_onsite=a1.*n_unit_onsite;                %3*109,t


    %product
    elec_offsite=daysperyear*m_offsite_tech.'*Product_Conversion_offsite;%1,t
    elec_onsite=daysperyear*m_onsite_tech.'*Product_Conversion_onsite;%1,t
    % Revenue Parameters
    revenue_offsite=Unit_resource_cost*elec_offsite.';%1,t
    revenue_onsite=Unit_resource_cost*elec_onsite.';%1,t
    revenue=revenue_offsite+revenue_onsite;%1,t

    %cost
    % CAPEXProduct_Conversion_offsite
    Installation_cost_Onsite=sum(Unit_installation_cost_onsite.'*actual_capacity_onsite);%(1*6) * (6*3)->1,t
    Installation_cost_Offsite=sum(Unit_installation_cost_offsite.'*actual_capacity_offsite);%(1*6) * (6*3)->1,t
    Installation_cost=Installation_cost_Onsite+Installation_cost_Offsite;%1,t

    % Operation
    CTRSt_offsite=daysperyear*sum(sum(Unit_transport_cost*d_offsite_cluster.*squeeze(sum(y_offsite,3))/Unit_transport_capacity));% Transport (109*3)->(1*3)->1
    CTRSt_onsite=daysperyear*sum(sum(Unit_transport_cost*d_onsite_cluster.*sum(y_onsite,2)/Unit_transport_capacity));% Transport (109*3)->(1*3)->1
    CTRSt=CTRSt_offsite+CTRSt_onsite;

    COMt_onsite=daysperyear*sum(Unit_operation_cost_onsite.'*actual_capacity_onsite);%109->1,t
    COMt_offsite=daysperyear*sum(Unit_operation_cost_offsite.'*actual_capacity_offsite); % O&M (1*6) * (6*3)->1,t
    COMt=COMt_onsite+COMt_offsite;

    CLDt_onsite=sum(sum(Unit_land_cost_onsite*actual_capacity_onsite));%109->1,t
    CLDt_offsite=sum(sum(Unit_land_cost_offsite*actual_capacity_offsite)); % O&M (6*3)->(1*3)->1,t
    CLDt=CLDt_onsite+CLDt_offsite;

    CMPt_onsite=daysperyear*sum(sum(Unit_manpower_cost*actual_capacity_onsite));%109->1,t
    CMPt_offsite=daysperyear*sum(sum(Unit_manpower_cost*actual_capacity_offsite));% (6*3)->(1*3)->1,t
    CMPt=CMPt_onsite+CMPt_offsite;%manpower cost

    CPLtreatmentt_onsite=sum(Pollution_treatment_tax_onsite.*m_onsite_tech);
    CPLtreatmentt_offsite=sum(Pollution_treatment_tax_offsite.*m_offsite_tech);% (6*1)->1 pollution cost
    CPLtreatmentt=CPLtreatmentt_onsite+CPLtreatmentt_offsite;%treatment pollution

    CPLtransportt_onsite=daysperyear*sum(sum(pollutionTransportTax * dieselConsTruck*d_onsite_cluster.*sum(y_onsite,2)/Unit_transport_capacity));%(109*3)->(1*3)->1
    CPLtransportt_offsite=daysperyear*sum(sum(pollutionTransportTax * dieselConsTruck*d_offsite_cluster.*squeeze(sum(y_offsite,3))/Unit_transport_capacity));%(109*3)->(1*3)->1
    CPLttransprtt=CPLtransportt_onsite+CPLtransportt_offsite;% transport pollution
    CPLt=CPLtreatmentt+CPLttransprtt;

    %resource
    resource_offsite=m_offsite_tech.'*Resource_conversion_offsite;%1,t
    resource_onsite=m_onsite_tech.'*Resource_conversion_onsite;%1,t
    % Revenue Parameters
    CRCt_offsite=Unit_resource_cost*resource_offsite.';%1,t
    CRCt_onsite=Unit_resource_cost*resource_onsite.';%1,t
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
                n_unit_onsite <= c;

                y_onsite>=0;
                y_offsite>=0;
                y_onsite<=1;
                y_offsite<=1;


     %          n_unit_offsite <= 0;
     %          n_unit_onsite <= 0; 


                sum(y_onsite,2)+squeeze(sum(sum(y_offsite,2),3))==1;
                if n_tech==1
                m_offsite_jk<=actual_capacity_offsite;
                else
                m_offsite_jk.'<=actual_capacity_offsite;
                end

                if n_tech_onsite==1
                m_onsite'<=actual_capacity_onsite;
                n_unit_onsite'>=n_onsite_min_cluster.*y_onsite,1;
                else
                m_onsite'<=actual_capacity_onsite;
                n_unit_onsite'>=n_onsite_min_cluster_expand.*y_onsite,1;
                end

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
    %resourceConsumptionBencmark=[70,0];%[elec,water]
    resourceConsumptionBencmark=70;%only elec
    %revFromWTFOffSite=[0.1,0];%[elec,water]
    revFromWTFOffSite=0.1;%only elec
    %transRateList=[290,0];%[elec,water]
    transRateList=290;%only elec

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
        BenchmarkResourceConsumption = daysperyear*sum(waste) * resourceConsumptionBencmark* Unit_resource_cost.';
        BenchmarkRevenue = daysperyear*sum(waste) * revFromWTFOffSite* transRateList.';

        Benchmarkcost=BenchmarkResourceConsumption+BenchmarkTreatmentPollution+BenchmarkManpower+BenchmarkLand+BenchmarkOperMgnt+BenchmarkTransportPollution+BenchmarkTransportation;

    NPV_Benchmark=-BenchmarkCapex;
    for t=1:15
    NPV_Benchmark=NPV_Benchmark+(BenchmarkRevenue-Benchmarkcost)/(1+i)^t;
    end

    economicoutput=[revenue,BenchmarkRevenue;revenue,BenchmarkRevenue;Installation_cost,BenchmarkCapex;COMt,BenchmarkOperMgnt;CMPt,BenchmarkManpower;CLDt,BenchmarkLand;CPLt,BenchmarkPollution;CTRSt,BenchmarkTransportation;CRCt,BenchmarkResourceConsumption;cost,Benchmarkcost;NPV,NPV_Benchmark];

    y_onsite=round(y_onsite,4);
    y_offsite=round(y_offsite,4);
    m_onsite=round(m_onsite,4);
    m_offsite=round(m_offsite,4);
    n_unit_onsite=round(n_unit_onsite,4);
    n_unit_offsite=round(n_unit_offsite,4);

    Nonsite=nnz(sum(y_onsite,2));
    Noffsite=nnz(sum(sum(y_offsite,1),3));
    disp(y_offsite);
    disp(y_onsite);
    disp(Nonsite);
    disp(Noffsite);
    disp(NPV/NPV_Benchmark);

    ID_onsite_FCin=ind1'.*sum(y_onsite,2);
    ID_onsite_FCin=ID_onsite_FCin(find(ID_onsite_FCin));
    ID_offsite_FC=ind1'.*squeeze(sum(sum(y_offsite,2),3));
    ID_onsite_tech=find(sum(y_onsite,1));
    ID_offsite_FC=ID_offsite_FC(find(ID_offsite_FC));
    ID_offsite_facility=find(squeeze(sum(sum(y_offsite,1),3)));
    ID_offsite_tech=find(squeeze(sum(sum(y_offsite,1),2)));

    x_onsite_allocation=zeros(n_foodcourt,n_foodcourt);
    x_onsite_allocation(:,ID_onsite_FCin)=x_cluster_allocation1(:,ID_onsite_FCin);

    % x_offsite_allocation=zeros(n_foodcourt,n_offsite,n_tech);
    % x_offsite_allocation(ID_offsite_FC,ID_offsite_FC,ID_offsite_tech)=1;

    x_offsite_allocation=zeros(n_foodcourt,n_offsite,n_tech);
    for k=1:n_cluster
        for i=1:n_foodcourt
            if x_cluster_allocation1(i,ind1(k))==1
                x_offsite_allocation(i,:,:)=y_offsite(k,:,:);
            end
        end
    end

    %e=sum(x_offsite_allocation,[2 3])+sum(x_onsite_allocation,2);
    % 
    writematrix(transpose(n_unit_onsite),'number of units (onsite).csv');% no unit
    writematrix(n_unit_offsite,'number of units (offsite).csv');% no unit
    writematrix(m_onsite,'Treated waste (onsite).csv');% ton,row-from foodcourt i waste generation, column-to foodcourt j for treatment
    writematrix(m_offsite,'Treated waste (offsite).csv');% no unit,row-from foodcourt i waste generation, column-to waste facility j for treatment
    writematrix(x_onsite_allocation,'Waste flow relation (onsite).csv');% no unit,row-from foodcourt i waste generation, column-to foodcourt j for treatment
    writematrix(x_offsite_allocation,'Waste flow relation (offsite).csv');% ton,row-from foodcourt i waste generation, column-to waste facility j for treatment
    writematrix(economicoutput,'Economic output.csv');% $
    writematrix(x_cluster_allocation1,'x_cluster_allocation.csv');
end