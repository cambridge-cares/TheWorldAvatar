clc
clear
close all
cvx_begin
cvx_solver gurobi
cvx_save_prefs
%% 
%6.2 solving each year individually
%6.1 add the below conditional option for small size problem
% % if n_year==1
% %             NPV=-Installation_cost;
% %             for t=1:15
% %             NPV = NPV+(revenue-cost)/(1+ir)^t; % $/day; CAPEX converted to daily CAPEX, 25 years, 365 days/y
% %             end
% %     
% % else
% 
% %6.1 make the number of clusters changable
% % n_cluster=round(n_foodcourt/2);
% % 6.2 delete the n_cluster change;



%% _____________________Parameter (Scalar)___________________________________

bigM=1000000;

daysperyear=365;

ir=0.01;%discount rate
%transport
transport=readmatrix('transport.csv');
Unit_transport_capacity=transport(1);%ton/truck
Unit_transport_cost=transport(2);%$/km
pollutionTransportTax=transport(3);
dieselConsTruck=transport(4); %gCO2/km

%others=readmatrix('other parameters.csv');
%Unit_land_cost_onsite=others(1);
%Unit_land_cost_offsite=others(2);
%Unit_manpower_cost=others(3);
%EOS=others(4);

Unit_land_cost_onsite=500; %$/unit capacity
Unit_land_cost_offsite=300;%$/unit capacity
Unit_manpower_cost=0; % manpower cost for both onsite and offsite
EOS=1; % Economy of scale

%_____________________Parameter (Array)____________________________________

% locaton information
site=readmatrix('Site_xy.csv');% foodcourt
%site=site(1:9,:);% foodcourt
% % 
%  waste=waste(:,1);% waste in tons/day

location=readmatrix('Location.csv');% offsite facilities

% offsite input
n_unit_offsite_max=readmatrix('n_unit_max_offsite.csv');%max number of unit for each site
% cost information
Unit_capacity_offsite=readmatrix('Unit_capacity_offsite.csv');%unit capacity for each tech
Unit_installation_cost_offsite=readmatrix('Unit installation cost (offsite).csv');%$/unit capacity
Unit_operation_cost_offsite=readmatrix('Unit operation cost (offsite).csv');%$/unit capacity
Pollution_treatment_tax_offsite=readmatrix('Pollution treatment tax (offsite).csv');%$/ton of CO2 equivalent
% conversion information
Product_Conversion_offsite=readmatrix('Conversion rate (offsite).csv');% Electricity kWh/ton waste Amount of recovered energy/material/ton of processed waste
Resource_conversion_offsite=readmatrix('Resource conversion (offsite).csv');% kWh/ton, Electricity consumption for production

% onsite input
n_unit_onsite_max=10;%max number of unit for each site
% cost information
Unit_capacity_onsite=readmatrix('Unit_capacity_onsite.csv');%unit capacity for each tech
Unit_installation_cost_onsite=readmatrix('Unit installation cost (onsite).csv');%$/unit capacity
Unit_operation_cost_onsite=readmatrix('Unit operation cost (onsite).csv');%$/unit capacity
Pollution_treatment_tax_onsite=readmatrix('Pollution treatment tax (onsite).csv');%$/ton of CO2 equivalent
% conversion information
Product_Conversion_onsite=readmatrix('Conversion rate (onsite).csv');% Electricity kWh/ton waste Amount of recovered energy/material/ton of processed waste
Resource_conversion_onsite=readmatrix('Resource conversion (onsite).csv');% kWh/ton, Electricity consumption for production

Unit_resource_cost=0.1; %electricity and water


[n_tech,n_offsite]=size(n_unit_offsite_max);
n_tech_onsite=size(Unit_capacity_onsite,1);
n_foodcourt=size(site,1); 
n_cluster= readmatrix('n_cluster.txt');

revenue_year=zeros(15,1);
Installation_cost_year=zeros(15,1);
CTRSt_year=zeros(15,1);
COMt_year=zeros(15,1);
CLDt_year=zeros(15,1);
CMPt_year=zeros(15,1);
CPLtreatmentt_year=zeros(15,1);
CPLt_year=zeros(15,1);
CRCt_year=zeros(15,1);
cost_year=zeros(15,1);
NPV_year=zeros(15,1);

if n_foodcourt<=8
y_offsite_year=zeros(n_foodcourt,n_offsite,n_tech,15);
y_onsite_year=zeros(n_foodcourt,n_tech_onsite,15);
n_unit_offsite_year=zeros(n_tech,n_offsite,15);
n_unit_onsite_year=zeros(n_tech_onsite,n_foodcourt,15);
m_offsite_year=zeros(n_foodcourt,n_offsite,n_tech,15);
m_onsite_year=zeros(n_foodcourt,n_tech_onsite,15);
else
y_offsite_year=zeros(n_cluster,n_offsite,n_tech,15);
y_onsite_year=zeros(n_cluster,n_tech_onsite,15);
n_unit_offsite_year=zeros(n_tech,n_offsite,15);
n_unit_onsite_year=zeros(n_tech_onsite,n_cluster,15);
m_offsite_year=zeros(n_cluster,n_offsite,n_tech,15);
m_onsite_year=zeros(n_cluster,n_tech_onsite,15);
end

%% ------------------Distance-based clustering----------------------
    waste_original=readmatrix('Waste.csv')/1000;% waste in tons/day
    site_original=readmatrix('Site_xy.csv');% foodcourt

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
            % columnsum1<=4;

     cvx_solver Gurobi_2;
     cvx_end 
     
% --------------------Optimization options begin---------------------------
     
for t=1:15
% locaton information
waste=readmatrix('Waste.csv')/1000;% waste in tons/day
waste=waste(:,t);
n_year=size(waste,2);
%waste=waste(1:9,:);% waste in tons/day

if t~=1
Installation_cost_pre=Installation_cost_year(t-1);
NPV_pre=NPV_year(t-1);
n_unit_onsite_pre=n_unit_onsite_year(:,:,t-1);
n_unit_offsite_pre=n_unit_offsite_year(:,:,t-1);
end


%% ________________________Small scale problem _______________________________

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

    variable y_offsite(n_foodcourt,n_offsite,n_tech,n_year) binary; 
    variable y_onsite(n_foodcourt,n_foodcourt,n_tech_onsite,n_year) binary; %mass per day
    variable n_unit_offsite(n_tech,n_offsite,n_year) integer;  %number of units
    variable n_unit_onsite(n_tech_onsite,n_foodcourt,n_year) integer; %number of units
    %_________________________Equations________________________________________

    a=repmat(Unit_capacity_offsite,1,n_offsite,n_year);
    a1=repmat(Unit_capacity_onsite,1,n_foodcourt,n_year);
    b=permute(repmat(waste,1,1,n_offsite,n_tech),[1 3 4 2]);
    b1=permute(repmat(waste,1,1,n_foodcourt,n_tech_onsite),[1 3 4 2]);
    c=repmat(n_unit_onsite_max,1,n_foodcourt,n_year);

    m_offsite=y_offsite.*b;
    m_offsite_jk=squeeze(sum(m_offsite,1));
    m_offsite_tech=squeeze(sum(sum(m_offsite,2),1));
    m_offsite_ij=squeeze(sum(m_offsite,3));
    m_offsite_foodcourt=sum(sum(m_offsite,3),2);

    m_onsite=y_onsite.*b1;
    m_onsite_jk=squeeze(sum(m_onsite,1));
    
    if n_tech_onsite==1
            m_onsite_tech=squeeze(sum(sum(m_onsite,2),1))';
    else
            m_onsite_tech=squeeze(sum(sum(m_onsite,2),1));
    end
    
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

    d=repmat(Unit_installation_cost_onsite,1,n_foodcourt,n_year);
    d1=repmat(Unit_installation_cost_offsite,1,n_offsite,n_year);
    %cost
    % CAPEXProduct_Conversion_offsite
    Installation_cost_Onsite=sum(d.*actual_capacity_onsite);%(1*6) * (6*3)->1,t
    Installation_cost_Offsite=sum(sum(d1.*actual_capacity_offsite));%(1*6) * (6*3)->1,t
    Installation_cost=squeeze(Installation_cost_Onsite+Installation_cost_Offsite)';%1,t

    e=repmat(Distance,1,1,n_year);
    e1=repmat(Distance_onsite,1,1,n_year);
    
    % Operation
    CTRSt_offsite=daysperyear*sum(sum(Unit_transport_cost*e.*(m_offsite_ij/Unit_transport_capacity)));% Transport (109*3)->(1*3)->1
    CTRSt_onsite=daysperyear*sum(sum(Unit_transport_cost*e1.*(m_onsite_ij/Unit_transport_capacity)));% Transport (109*3)->(1*3)->1
    CTRSt=squeeze(CTRSt_offsite+CTRSt_onsite)';

    f=repmat(Unit_operation_cost_onsite,1,n_foodcourt,n_year);
    f1=repmat(Unit_operation_cost_offsite,1,n_offsite,n_year);
    COMt_onsite=daysperyear*sum(f.*actual_capacity_onsite);%109->1,t
    COMt_offsite=sum(daysperyear*sum(f1.*actual_capacity_offsite)); % O&M (1*6) * (6*3)->1,t
    COMt=squeeze(COMt_onsite+COMt_offsite)';

    CLDt_onsite=sum(sum(Unit_land_cost_onsite*actual_capacity_onsite));%109->1,t
    CLDt_offsite=sum(sum(Unit_land_cost_offsite*actual_capacity_offsite)); % O&M (6*3)->(1*3)->1,t
    CLDt=squeeze(CLDt_onsite+CLDt_offsite)';

    CMPt_onsite=daysperyear*sum(sum(Unit_manpower_cost*actual_capacity_onsite));%109->1,t
    CMPt_offsite=daysperyear*sum(sum(Unit_manpower_cost*actual_capacity_offsite));% (6*3)->(1*3)->1,t
    CMPt=squeeze(CMPt_onsite+CMPt_offsite)';%manpower cost
    
    g=repmat(Pollution_treatment_tax_onsite,1,n_year);
    g1=repmat(Pollution_treatment_tax_offsite,1,n_year);
    CPLtreatmentt_onsite=sum(g.*m_onsite_tech);
    CPLtreatmentt_offsite=sum(g1.*m_offsite_tech);% (6*1)->1 pollution cost
    CPLtreatmentt=CPLtreatmentt_onsite+CPLtreatmentt_offsite;%treatment pollution

    CPLtransportt_onsite=daysperyear*sum(sum(pollutionTransportTax * dieselConsTruck*e1.*(m_onsite_ij/Unit_transport_capacity)));%(109*3)->(1*3)->1
    CPLtransportt_offsite=daysperyear*sum(sum(pollutionTransportTax * dieselConsTruck*e.*(m_offsite_ij/Unit_transport_capacity)));%(109*3)->(1*3)->1
    CPLttransprtt=squeeze(CPLtransportt_onsite+CPLtransportt_offsite)';% transport pollution
    CPLt=CPLtreatmentt+CPLttransprtt;

    %resource
    resource_offsite=m_offsite_tech.'*Resource_conversion_offsite;%1,t
    resource_onsite=m_onsite_tech.'*Resource_conversion_onsite;%1,t
    % Revenue Parameters
    CRCt_offsite=Unit_resource_cost*resource_offsite.';%1,t
    CRCt_onsite=Unit_resource_cost*resource_onsite.';%1,t
    CRCt=CRCt_onsite+CRCt_offsite;

    cost = squeeze(CTRSt + COMt + CLDt + CMPt + CPLt + CRCt);

    if t==1
    % Profit
    ir=0.01;%discount rate
    NPV = -Installation_cost+(revenue-cost)/(i+ir)^1; % $/day; CAPEX converted to daily CAPEX, 25 years, 365 days/y
    else
    NPV=NPV_pre+(revenue-cost-(Installation_cost-Installation_cost_pre))/(1+ir)^t;
    end
    
    %_________________________Optimization_____________________________________


            maximize(NPV)

            subject to
                n_unit_offsite>=0;
                n_unit_onsite>=0;
                n_unit_offsite <= repmat(n_unit_offsite_max,1,1,n_year);
                n_unit_onsite <= repmat(n_unit_onsite_max,1,n_foodcourt,n_year);
                
                if t~=1
                n_unit_offsite>=n_unit_offsite_pre;
                n_unit_onsite>=n_unit_onsite_pre;
                end   
                
    %           n_unit_offsite <= 0;
    %           n_unit_onsite <= 0; 
    
                y_onsite>=0;
                y_offsite>=0;
                y_onsite<=1;
                y_offsite<=1;
                
                h1=squeeze(sum(sum(y_offsite,2),3));
                h=squeeze(sum(y_onsite,2));
                h+h1==1;
                
                m_offsite_jk<=actual_capacity_offsite;
                m_onsite_jk<=squeeze(actual_capacity_onsite);


     cvx_solver Gurobi_2;

     cvx_end 

    y_onsite=round(y_onsite);
    y_offsite=round(y_offsite);
    m_onsite=round(m_onsite,4);
    m_offsite=round(m_offsite,4);
    n_unit_onsite=round(n_unit_onsite);
    n_unit_offsite=round(n_unit_offsite);

    Nonsite=nnz(y_onsite);
    Noffsite=nnz(y_offsite);
    disp(y_offsite);
    disp(y_onsite);
    disp(Nonsite);
    disp(Noffsite);
% 
%% ----------------------Clustering problem----------------------------
else

    x_cluster_allocation1= round(x_cluster_allocation1);
    columnsum1=sum(x_cluster_allocation1,1);
    ind1=find(columnsum1);
    site_cluster1=site(ind1,:);%----------------
    n_cluster=size(site_cluster1,1);

    %______________________ Parameter manipulation ____________________________

    %% distance load
    %_-----------------onsite------------------------------
    d_actual=repmat(full(Distance_onsite.*x_cluster_allocation1),1,1,n_year);%get rid of the unclusterred routes
    waste_manipulation1=permute(repmat(waste,1,1,n_foodcourt),[1 3 2]);
    d_onsite_1=squeeze(sum(waste_manipulation1.*d_actual,1)); % amount of waste of cluster i in year t
   if n_year==1
       d_onsite_cluster=d_onsite_1(ind1);
   else
    d_onsite_cluster=d_onsite_1(ind1,:);
   end

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
    x_allocation_manipulation=repmat(full(x_cluster_allocation1),1,1,n_year);
    waste_1=squeeze(sum(waste_manipulation1.*x_allocation_manipulation,1));
    if n_year==1
        waste_cluster=waste_1(ind1);
    else
        waste_cluster=waste_1(ind1,:);
    end

    %% minimum number of unit for onsite

    if n_tech_onsite==1
        if n_year==1
        unit_capacity_onsite_expand_cluster=repmat(Unit_capacity_onsite,1,n_cluster);
        n_onsite_min_cluster=ceil(waste_cluster./unit_capacity_onsite_expand_cluster);
        else
        unit_capacity_onsite_expand_cluster=repmat(Unit_capacity_onsite,n_cluster,n_year);
        n_onsite_min_cluster=ceil(waste_cluster./unit_capacity_onsite_expand_cluster);
        end

    else 
        waste_cluster_expandtech=repmat(waste_cluster,1,n_tech_onsite);
        unit_capacity_onsite_expand_cluster=repmat(Unit_capacity_onsite,1,n_cluster)';
        n_onsite_min_cluster=ceil(waste_cluster_expandtech./unit_capacity_onsite_expand_cluster);
        n_onsite_min_cluster=permute(repmat(n_onsite_min_cluster,1,1,n_cluster),[1 3 2]);
    end 

    %% 
    %_________________________Variables________________________________________

    cvx_begin

    variable y_offsite(n_cluster,n_offsite,n_tech,n_year) binary; 
    variable y_onsite(n_cluster,n_tech_onsite,n_year) binary; %mass per day
    variable n_unit_offsite(n_tech,n_offsite,n_year) integer;  %number of units
    variable n_unit_onsite(n_tech_onsite,n_cluster,n_year) integer; %number of units

    %_________________________Equations________________________________________

    a=repmat(Unit_capacity_offsite,1,n_offsite,n_year);
    a1=repmat(Unit_capacity_onsite,1,n_cluster,n_year);
    if n_year==1
        b=repmat(waste_cluster',1,n_offsite,n_tech); 
        b1=repmat(waste_cluster',1,n_cluster,n_tech_onsite);
    else
    b=permute(repmat(waste_cluster,1,1,n_offsite,n_tech),[1 3 4 2]); 
    b1=permute(repmat(waste_cluster,1,1,n_cluster,n_tech_onsite),[1 3 4 2]); 
    end

    m_offsite=y_offsite.*b;
    m_offsite_jk=squeeze(sum(m_offsite,1));
    m_offsite_tech=squeeze(sum(sum(m_offsite,2),1));
    m_offsite_ij=squeeze(sum(m_offsite,3));
    m_offsite_foodcourt=sum(sum(m_offsite,3),2);

if n_year==1
    m_onsite=repmat(waste_cluster',1,n_tech_onsite).*y_onsite;
else
    m_onsite=permute(repmat(waste_cluster,1,1,n_tech_onsite),[1 3 2]).*y_onsite;
end
    m_onsite_tech=squeeze(sum(m_onsite,1))';

    actual_capacity_offsite=a.*n_unit_offsite;                %6*3,t
    actual_capacity_onsite=a1.*n_unit_onsite;                %3*109,t
    
    %product
    elec_offsite=daysperyear*m_offsite_tech.'*Product_Conversion_offsite;%1,t
    elec_onsite=daysperyear*m_onsite_tech.'*Product_Conversion_onsite;%1,t
    % Revenue Parameters
    revenue_offsite=Unit_resource_cost*elec_offsite.';%1,t
    revenue_onsite=Unit_resource_cost*elec_onsite.';%1,t
    revenue=revenue_offsite+revenue_onsite;%1,t

    d=repmat(Unit_installation_cost_onsite,1,n_cluster,n_year);
    d1=repmat(Unit_installation_cost_offsite,1,n_offsite,n_year); 
    %cost
    % CAPEXProduct_Conversion_offsite
    Installation_cost_Onsite=sum(d.*actual_capacity_onsite);%(1*6) * (6*3)->1,t
    Installation_cost_Offsite=sum(sum(d1.*actual_capacity_offsite));%(1*6) * (6*3)->1,t
    Installation_cost=Installation_cost_Onsite+Installation_cost_Offsite;%1,t
        
    e=repmat(d_offsite_cluster,1,1,n_year);

    % Operation
    CTRSt_offsite=daysperyear*sum(sum(Unit_transport_cost*e.*squeeze(sum(y_offsite,3))/Unit_transport_capacity));% Transport (109*3)->(1*3)->1
    if n_year==1
        CTRSt_onsite=daysperyear*sum(sum(Unit_transport_cost*d_onsite_cluster'.*squeeze(sum(y_onsite,2))/Unit_transport_capacity));% Transport (109*3)->(1*3)->1
    else
    CTRSt_onsite=daysperyear*sum(sum(Unit_transport_cost*d_onsite_cluster.*squeeze(sum(y_onsite,2))/Unit_transport_capacity));% Transport (109*3)->(1*3)->1
    end
    CTRSt=CTRSt_offsite+CTRSt_onsite;

    f=repmat(Unit_operation_cost_onsite,1,n_cluster,n_year);
    f1=repmat(Unit_operation_cost_offsite,1,n_offsite,n_year);  
    COMt_onsite=daysperyear*sum(f.*actual_capacity_onsite);%109->1,t
    COMt_offsite=sum(daysperyear*sum(f1.*actual_capacity_offsite)); % O&M (1*6) * (6*3)->1,t
    COMt=COMt_onsite+COMt_offsite;

    CLDt_onsite=sum(sum(Unit_land_cost_onsite*actual_capacity_onsite));%109->1,t
    CLDt_offsite=sum(sum(Unit_land_cost_offsite*actual_capacity_offsite)); % O&M (6*3)->(1*3)->1,t
    CLDt=CLDt_onsite+CLDt_offsite;

    CMPt_onsite=daysperyear*sum(sum(Unit_manpower_cost*actual_capacity_onsite));%109->1,t
    CMPt_offsite=daysperyear*sum(sum(Unit_manpower_cost*actual_capacity_offsite));% (6*3)->(1*3)->1,t
    CMPt=CMPt_onsite+CMPt_offsite;%manpower cost

    g=repmat(Pollution_treatment_tax_onsite,1,n_year);
    g1=repmat(Pollution_treatment_tax_offsite,1,n_year);

    CPLtreatmentt_onsite=sum(g.*m_onsite_tech);
    CPLtreatmentt_offsite=sum(g1.*m_offsite_tech);% (6*1)->1 pollution cost
    CPLtreatmentt=CPLtreatmentt_onsite+CPLtreatmentt_offsite;%treatment pollution

    if n_year==1
        CPLtransportt_onsite=daysperyear*sum(sum(pollutionTransportTax * dieselConsTruck*d_onsite_cluster'.*squeeze(sum(y_onsite,2))/Unit_transport_capacity));%(109*3)->(1*3)->1
    else
    CPLtransportt_onsite=daysperyear*sum(sum(pollutionTransportTax * dieselConsTruck*d_onsite_cluster.*squeeze(sum(y_onsite,2))/Unit_transport_capacity));%(109*3)->(1*3)->1
    end
    CPLtransportt_offsite=daysperyear*sum(sum(pollutionTransportTax * dieselConsTruck*e.*squeeze(sum(y_offsite,3))/Unit_transport_capacity));%(109*3)->(1*3)->1
    CPLttransprtt=squeeze(CPLtransportt_onsite+CPLtransportt_offsite)';% transport pollution
    CPLt=CPLtreatmentt+CPLttransprtt;

    %resource
    resource_offsite=m_offsite_tech.'*Resource_conversion_offsite;%1,t
    resource_onsite=m_onsite_tech.'*Resource_conversion_onsite;%1,t
    % Revenue Parameters
    CRCt_offsite=Unit_resource_cost*resource_offsite.';%1,t
    CRCt_onsite=Unit_resource_cost*resource_onsite.';%1,t
    CRCt=CRCt_onsite+CRCt_offsite;

    cost = squeeze(CTRSt + COMt + CLDt + CMPt) + (CPLt + CRCt)';
    
    if t==1
    % Profit
    ir=0.01;%discount rate
    NPV = -Installation_cost+(revenue-cost)/(i+ir)^1; % $/day; CAPEX converted to daily CAPEX, 25 years, 365 days/y
    else
    NPV=NPV_pre+(revenue-cost-(Installation_cost-Installation_cost_pre))/(1+ir)^t;
    end
    %_________________________Optimization_____________________________________

            maximize(NPV)

            subject to
                n_unit_offsite>=0;
                n_unit_onsite>=0;
                n_unit_offsite <= repmat(n_unit_offsite_max,1,1,n_year);
                n_unit_onsite <= repmat(n_unit_onsite_max,1,n_cluster,n_year);
                
                if t~=1
                n_unit_offsite>=n_unit_offsite_pre;
                n_unit_onsite>=n_unit_onsite_pre;
                end   
                
                y_onsite>=0;
                y_offsite>=0;
                y_onsite<=1;
                y_offsite<=1;


%                n_unit_offsite <= 0;
     %          n_unit_onsite <= 0; 

                squeeze(sum(y_onsite,2))+squeeze(sum(sum(y_offsite,2),3))==1;
if n_year==1

                if n_tech==1
                m_offsite_jk<=actual_capacity_offsite;
                else
                m_offsite_jk.'<=actual_capacity_offsite;
                end

                if n_tech_onsite==1
                m_onsite'<=actual_capacity_onsite;
                n_unit_onsite>=n_onsite_min_cluster.*y_onsite';
                else
                m_onsite'<=actual_capacity_onsite;
                n_unit_onsite>=n_onsite_min_cluster_expand.*y_onsite';
                end
else
                m_offsite_jk<=actual_capacity_offsite;
                squeeze(m_onsite)<=squeeze(actual_capacity_onsite);
                squeeze(n_unit_onsite)>=squeeze(n_onsite_min_cluster).*squeeze(y_onsite);
end
                
     cvx_solver Gurobi_2
    
     cvx_save_prefs

     cvx_end 

end

revenue_year(t)=revenue;
Installation_cost_year(t)=Installation_cost;
CTRSt_year(t)=CTRSt;
COMt_year(t)=COMt;
CLDt_year(t)=CLDt;
CMPt_year(t)=CMPt;
CPLtreatmentt_year(t)=CPLtreatmentt;
CPLt_year(t)=CPLt;
CRCt_year(t)=CRCt;
cost_year(t)=cost;
NPV_year(t)=NPV;
n_unit_offsite_year(:,:,t)=n_unit_offsite;
n_unit_onsite_year(:,:,t)=n_unit_onsite;
m_offsite_year(:,:,:,t)=m_offsite;
m_onsite_year(:,:,t)=m_onsite;
y_offsite_year(:,:,:,t)=y_offsite;
y_onsite_year(:,:,t)=y_onsite;

end


%% -------------------Output-----------------------------------------
    waste=readmatrix('Waste.csv')/1000;% waste in tons/day
%    waste=waste(1:9,:);
    
    n_year=size(waste,2);
    y_onsite_year=round(y_onsite_year);
    y_offsite_year=round(y_offsite_year);
    m_onsite_year=round(m_onsite_year,2);
    m_offsite_year=round(m_offsite_year,2);
    n_unit_onsite_year=round(n_unit_onsite_year);
    n_unit_offsite_year=round(n_unit_offsite_year);

    if n_foodcourt>=8

    x_offsite_allocation=zeros(n_foodcourt,n_offsite,n_tech,n_year);
    for k=1:n_cluster
        for i=1:n_foodcourt
            if x_cluster_allocation1(i,ind1(k))==1
                x_offsite_allocation(i,:,:,:)=y_offsite_year(k,:,:,:);
            end
        end
    end
    
    x_onsite_allocation=zeros(n_foodcourt,n_foodcourt,n_tech_onsite,n_year);
    for k=1:n_cluster
        for i=1:n_foodcourt
            if x_cluster_allocation1(i,ind1(k))==1
                x_onsite_allocation(i,ind1(k),:,:)=y_onsite_year(k,:,:,:);
            end
        end
    end
        
    n_onsite_allocation=zeros(n_tech_onsite,n_foodcourt,n_year);
    for k=1:n_cluster
                n_onsite_allocation(:,ind1(k),:)=n_unit_onsite_year(:,k,:);
    end
    
    y_onsite_cluster=y_onsite_year;
    y_offsite_cluster=y_offsite_year;
    y_onsite_year=x_onsite_allocation;
    y_offsite_year=x_offsite_allocation;    
    m_onsite_cluster=m_onsite_year;
    m_offsite_cluster=m_offsite_year;
    n_unit_onsite_cluster=n_unit_onsite_year;
    n_unit_onsite_year=n_onsite_allocation;
    
    if n_year==1
    m_onsite_year=repmat(waste,1,n_foodcourt,n_tech_onsite).*x_onsite_allocation;
    m_offsite_year=repmat(waste,1,n_offsite,n_tech).*x_offsite_allocation;
    else
    m_onsite_year=permute(repmat(waste,1,1,n_foodcourt,n_tech_onsite),[1 3 4 2]).*x_onsite_allocation;
    m_offsite_year=permute(repmat(waste,1,1,n_offsite,n_tech),[1 3 4 2]).*x_offsite_allocation;   
     end
    end
    

  
%% _________________________Benchmark________________________________________ 
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
        Benchmarkcost=BenchmarkResourceConsumption+BenchmarkPollution+BenchmarkManpower+BenchmarkLand+BenchmarkOperMgnt+BenchmarkTransportation;
   
    
    NPV_Benchmark=-BenchmarkCapex(1);
    BenchmarkCapex(n_year+1)=BenchmarkCapex(n_year);
    for t=1:n_year
    NPV_Benchmark=NPV_Benchmark+(BenchmarkRevenue(t)-Benchmarkcost(t)-(BenchmarkCapex(t+1)-BenchmarkCapex(t)))/(1+ir)^t;
    end
    BenchmarkRevenue_total=0;
    for t=1:n_year
    BenchmarkRevenue_total=BenchmarkRevenue_total+BenchmarkRevenue(t)/(1+ir)^t;
    end
    Benchmarkcost_total=0;
    for t=1:n_year
    Benchmarkcost_total=Benchmarkcost_total+Benchmarkcost(t)/(1+ir)^t;
    end
    BenchmarkCapex_total=BenchmarkCapex(1);
        for t=1:n_year
    BenchmarkCapex_total=BenchmarkCapex_total+(BenchmarkCapex(t+1)-BenchmarkCapex(t))/(1+ir)^t;
        end
    BenchmarkOperMgnt_total=0 ;     
        for t=1:n_year
    BenchmarkOperMgnt_total=BenchmarkOperMgnt_total+BenchmarkOperMgnt(t)/(1+ir)^t;
        end
    BenchmarkManpower_total=0 ;     
        for t=1:n_year
    BenchmarkManpower_total=BenchmarkManpower_total+BenchmarkManpower(t)/(1+ir)^t;
        end
    BenchmarkLand_total=0;    
        for t=1:n_year
    BenchmarkLand_total=BenchmarkLand_total+BenchmarkLand(t)/(1+ir)^t;
        end
    BenchmarkPollution_total=0;    
        for t=1:n_year
    BenchmarkPollution_total=BenchmarkPollution_total+BenchmarkPollution(t)/(1+ir)^t;
        end
    BenchmarkTransportation_total=0;    
        for t=1:n_year
    BenchmarkTransportation_total=BenchmarkTransportation_total+BenchmarkTransportation(t)/(1+ir)^t;
        end
    BenchmarkResourceConsumption_total=0;    
        for t=1:n_year
    BenchmarkResourceConsumption_total=BenchmarkResourceConsumption_total+BenchmarkResourceConsumption(t)/(1+ir)^t;
        end

        
   cost_total=0;
    for t=1:n_year
    cost_total=cost_total+cost_year(t)/(1+ir)^t;
    end
    revenue_total=0;
    for t=1:n_year
    revenue_total=revenue_total+revenue_year(t)/(1+ir)^t;
    end
    
    Installation_cost_total=Installation_cost_year(1);
        for t=2:n_year
    Installation_cost_total=Installation_cost_total+(Installation_cost_year(t)-Installation_cost_year(t-1))/(1+ir)^t;
        end
    
    COMt_total=0 ;     
        for t=1:n_year
    COMt_total=COMt_total+COMt_year(t)/(1+ir)^t;
        end
    CMPt_total=0 ;     
        for t=1:n_year
    CMPt_total=CMPt_total+CMPt_year(t)/(1+ir)^t;
        end
    CLDt_total=0;    
        for t=1:n_year
    CLDt_total=CLDt_total+CLDt_year(t)/(1+ir)^t;
        end
    CPLt_total=0;    
        for t=1:n_year
    CPLt_total=CPLt_total+CPLt_year(t)/(1+ir)^t;
        end
    CTRSt_total=0;    
        for t=1:n_year
    CTRSt_total=CTRSt_total+CTRSt_year(t)/(1+ir)^t;
        end
    CRCt_total=0;    
        for t=1:n_year
    CRCt_total=CRCt_total+CRCt_year(t)/(1+ir)^t;
        end        

economicoutput=[revenue_total,BenchmarkRevenue_total;Installation_cost_total,BenchmarkCapex_total;COMt_total,BenchmarkOperMgnt_total;CMPt_total,BenchmarkManpower_total;CLDt_total,BenchmarkLand_total;CPLt_total,BenchmarkPollution_total;CTRSt_total,BenchmarkTransportation_total;CRCt_total,BenchmarkResourceConsumption_total;cost_total,Benchmarkcost_total;NPV,NPV_Benchmark];

%% -------------------------Export-----------------------------------    
    
%     disp(y_offsite);
%     disp(y_onsite);
%     disp(Nonsite);
%     disp(Noffsite);
%     disp(NPV/NPV_Benchmark);

% 1==sum(x_offsite_allocation,[2 3])+sum(x_onsite_allocation,2);
    % 
    
    writematrix(n_unit_onsite_year(:,:,1),'year by year_number of units (onsite)_1.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,2),'year by year_number of units (onsite)_2.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,3),'year by year_number of units (onsite)_3.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,4),'year by year_number of units (onsite)_4.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,5),'year by year_number of units (onsite)_5.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,6),'year by year_number of units (onsite)_6.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,7),'year by year_number of units (onsite)_7.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,8),'year by year_number of units (onsite)_8.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,9),'year by year_number of units (onsite)_9.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,10),'year by year_number of units (onsite)_10.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,11),'year by year_number of units (onsite)_11.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,12),'year by year_number of units (onsite)_12.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,13),'year by year_number of units (onsite)_13.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,14),'year by year_number of units (onsite)_14.csv');% no unit
    writematrix(n_unit_onsite_year(:,:,15),'year by year_number of units (onsite)_15.csv');% no unit
       
    writematrix(n_unit_offsite_year(:,:,1),'year by year_number of units (offsite)_1.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,2),'year by year_number of units (offsite)_2.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,3),'year by year_number of units (offsite)_3.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,4),'year by year_number of units (offsite)_4.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,5),'year by year_number of units (offsite)_5.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,6),'year by year_number of units (offsite)_6.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,7),'year by year_number of units (offsite)_7.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,8),'year by year_number of units (offsite)_8.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,9),'year by year_number of units (offsite)_9.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,10),'year by year_number of units (offsite)_10.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,11),'year by year_number of units (offsite)_11.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,12),'year by year_number of units (offsite)_12.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,13),'year by year_number of units (offsite)_13.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,14),'year by year_number of units (offsite)_14.csv');% no unit
    writematrix(n_unit_offsite_year(:,:,15),'year by year_number of units (offsite)_15.csv');% no unit
    
    writematrix(m_onsite_year(:,:,:,1),'year by year_treated waste (onsite)_1.csv');% no unit
    writematrix(m_onsite_year(:,:,:,2),'year by year_treated waste (onsite)_2.csv');% no unit
    writematrix(m_onsite_year(:,:,:,3),'year by year_treated waste (onsite)_3.csv');% no unit
    writematrix(m_onsite_year(:,:,:,4),'year by year_treated waste (onsite)_4.csv');% no unit
    writematrix(m_onsite_year(:,:,:,5),'year by year_treated waste (onsite)_5.csv');% no unit
    writematrix(m_onsite_year(:,:,:,6),'year by year_treated waste (onsite)_6.csv');% no unit
    writematrix(m_onsite_year(:,:,:,7),'year by year_treated waste (onsite)_7.csv');% no unit
    writematrix(m_onsite_year(:,:,:,8),'year by year_treated waste (onsite)_8.csv');% no unit
    writematrix(m_onsite_year(:,:,:,9),'year by year_treated waste (onsite)_9.csv');% no unit
    writematrix(m_onsite_year(:,:,:,10),'year by year_treated waste (onsite)_10.csv');% no unit
    writematrix(m_onsite_year(:,:,:,11),'year by year_treated waste (onsite)_11.csv');% no unit
    writematrix(m_onsite_year(:,:,:,12),'year by year_treated waste (onsite)_12.csv');% no unit
    writematrix(m_onsite_year(:,:,:,13),'year by year_treated waste (onsite)_13.csv');% no unit
    writematrix(m_onsite_year(:,:,:,14),'year by year_treated waste (onsite)_14.csv');% no unit
    writematrix(m_onsite_year(:,:,:,15),'year by year_treated waste (onsite)_15.csv');% no unit
    
    writematrix(m_offsite_year(:,:,:,1),'year by year_treated waste (offsite)_1.csv');% no unit
    writematrix(m_offsite_year(:,:,:,2),'year by year_treated waste (offsite)_2.csv');% no unit
    writematrix(m_offsite_year(:,:,:,3),'year by year_treated waste (offsite)_3.csv');% no unit
    writematrix(m_offsite_year(:,:,:,4),'year by year_treated waste (offsite)_4.csv');% no unit
    writematrix(m_offsite_year(:,:,:,5),'year by year_treated waste (offsite)_5.csv');% no unit
    writematrix(m_offsite_year(:,:,:,6),'year by year_treated waste (offsite)_6.csv');% no unit
    writematrix(m_offsite_year(:,:,:,7),'year by year_treated waste (offsite)_7.csv');% no unit
    writematrix(m_offsite_year(:,:,:,8),'year by year_treated waste (offsite)_8.csv');% no unit
    writematrix(m_offsite_year(:,:,:,9),'year by year_treated waste (offsite)_9.csv');% no unit
    writematrix(m_offsite_year(:,:,:,10),'year by year_treated waste (offsite)_10.csv');% no unit
    writematrix(m_offsite_year(:,:,:,11),'year by year_treated waste (offsite)_11.csv');% no unit
    writematrix(m_offsite_year(:,:,:,12),'year by year_treated waste (offsite)_12.csv');% no unit
    writematrix(m_offsite_year(:,:,:,13),'year by year_treated waste (offsite)_13.csv');% no unit
    writematrix(m_offsite_year(:,:,:,14),'year by year_treated waste (offsite)_14.csv');% no unit
    writematrix(m_offsite_year(:,:,:,15),'year by year_treated waste (offsite)_15.csv');% no unit
           
    writematrix(y_onsite_year(:,:,:,1),'year by year_waste flow relation (onsite)_1.csv');% no unit
    writematrix(y_onsite_year(:,:,:,2),'year by year_waste flow relation (onsite)_2.csv');% no unit
    writematrix(y_onsite_year(:,:,:,3),'year by year_waste flow relation (onsite)_3.csv');% no unit
    writematrix(y_onsite_year(:,:,:,4),'year by year_waste flow relation (onsite)_4.csv');% no unit
    writematrix(y_onsite_year(:,:,:,5),'year by year_waste flow relation (onsite)_5.csv');% no unit
    writematrix(y_onsite_year(:,:,:,6),'year by year_waste flow relation (onsite)_6.csv');% no unit
    writematrix(y_onsite_year(:,:,:,7),'year by year_waste flow relation (onsite)_7.csv');% no unit
    writematrix(y_onsite_year(:,:,:,8),'year by year_waste flow relation (onsite)_8.csv');% no unit
    writematrix(y_onsite_year(:,:,:,9),'year by year_waste flow relation (onsite)_9.csv');% no unit
    writematrix(y_onsite_year(:,:,:,10),'year by year_waste flow relation (onsite)_10.csv');% no unit
    writematrix(y_onsite_year(:,:,:,11),'year by year_waste flow relation (onsite)_11.csv');% no unit
    writematrix(y_onsite_year(:,:,:,12),'year by year_waste flow relation (onsite)_12.csv');% no unit
    writematrix(y_onsite_year(:,:,:,13),'year by year_waste flow relation (onsite)_13.csv');% no unit
    writematrix(y_onsite_year(:,:,:,14),'year by year_waste flow relation (onsite)_14.csv');% no unit
    writematrix(y_onsite_year(:,:,:,15),'year by year_waste flow relation (onsite)_15.csv');% no unit
    
    writematrix(y_offsite_year(:,:,:,1),'year by year_waste flow relation (offsite)_1.csv');% no unit
    writematrix(y_offsite_year(:,:,:,2),'year by year_waste flow relation (offsite)_2.csv');% no unit
    writematrix(y_offsite_year(:,:,:,3),'year by year_waste flow relation (offsite)_3.csv');% no unit
    writematrix(y_offsite_year(:,:,:,4),'year by year_waste flow relation (offsite)_4.csv');% no unit
    writematrix(y_offsite_year(:,:,:,5),'year by year_waste flow relation (offsite)_5.csv');% no unit
    writematrix(y_offsite_year(:,:,:,6),'year by year_waste flow relation (offsite)_6.csv');% no unit
    writematrix(y_offsite_year(:,:,:,7),'year by year_waste flow relation (offsite)_7.csv');% no unit
    writematrix(y_offsite_year(:,:,:,8),'year by year_waste flow relation (offsite)_8.csv');% no unit
    writematrix(y_offsite_year(:,:,:,9),'year by year_waste flow relation (offsite)_9.csv');% no unit
    writematrix(y_offsite_year(:,:,:,10),'year by year_waste flow relation (offsite)_10.csv');% no unit
    writematrix(y_offsite_year(:,:,:,11),'year by year_waste flow relation (offsite)_11.csv');% no unit
    writematrix(y_offsite_year(:,:,:,12),'year by year_waste flow relation (offsite)_12.csv');% no unit
    writematrix(y_offsite_year(:,:,:,13),'year by year_waste flow relation (offsite)_13.csv');% no unit
    writematrix(y_offsite_year(:,:,:,14),'year by year_waste flow relation (offsite)_14.csv');% no unit
    writematrix(y_offsite_year(:,:,:,15),'year by year_waste flow relation (offsite)_15.csv');% no unit
    
    writematrix(economicoutput,'year by year_Economic output.csv');% $
      
writematrix(revenue_year,'year by year_revenue.xlsx');
writematrix(Installation_cost_year,'year by year_Installation_cost_year');
writematrix(CTRSt_year,'year by year_Transport');
writematrix(COMt_year,'year by year_Operating');
writematrix(CLDt_year,'year by year_land');
writematrix(CMPt_year,'year by year_manpower');
writematrix(CPLt_year,'year by year_pollution');
writematrix(CRCt_year,'year by year_resource');
writematrix(cost_year,'year by year_cost');
writematrix(NPV_year,'year by year_NPV');