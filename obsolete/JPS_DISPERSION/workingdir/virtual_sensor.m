function [c_gas, ic_name]=virtual_sensor(location, ic_name1, option, data_path, filename)
%**************************************************************************
% This is a post-processing code to estimate gas concentration at any
% selected location in the simulation domain.  Written by Kang @ Mar, 2020
% in CARES.
%
%Notes: 
% 1). In current version, 
% if option =1 (trilinear)
% if option =2 (interp3 function) - 'spline' method
% if option =3 (interp3 function) - 'cubic' method
% if option =4 (interp3 function) - 'makima' method
%
% 2). option =1 uses Trilinear interpolation (selected point is within a
% cubic inside simluation domain); 
% 3). option =2 -4 uses Matlab interp3() function.
% when option =2, "spline" option is used, which has the highest continuity.
% It considers 4 orignal points in x,y and z direction as coarse input
% data. The finer mesh where interpolation is taken has a resolution of
% 5m (dxx) by 5m (dyy) by 1m (dzz), which can be modified as per requirements.
% However, if dxx,dyy and dzz values are too small, the calculation may too
% heavy to make the Matlab program crash.ß
% When option =3 "cubic" option is used.
% When option =4 "makima" option is used. More details for this function
% can be found in Matlab interp3() funciton
% 4). the input window will let user to determine which gas species to be
% estimated. (ic_gas=1: O3, =2: NO, =3: NO2 .... ref to EPISODE gas index).
% 5), "c_gas(:)" contains the concentration for gas species
% defined in input window at selected location.  

 clc;
 %clear;
 close all;

%% ************************************************************************
%% Part 1: define the input variables
nrun_stop=0;  %% if selected point is within simluation domain. =0, yes, run interpolation. =1, no, stop interpolation.
ic_gas_run=1;  %% if gas name is available. if not, =0, and stop running code.
ic_coladd=3; %% how many columns from x to first species
date_coladd=4; %% how many columns for date information in concentration data files;

%var_input=inputdlg({'Enter space-separated location (x y z) @ meter:','Gas species (Such as NO, NO2, PM2.5)','Interpolation option (1: linear. 2: interp3 - spline, 3: interp3 - cubic, 4: interp3 - makima)'}, 'sensor', [1 100; 1 100; 1 100]);
%location=str2num(vinput); %% x,y,z (m) of selected point to get its local gas concentration
%ic_name1=name;  %% gas name from input box. 
%option=str2double(options);  %% method to interpolate data in seleted location.  =1, trilinear.  =2, matlab interp3 option (spline) =3, matlab interp3 option (cubic)
%ic_name1=convertCharsToStrings(ic_name0);

ic_name=strsplit(ic_name1); %% gas name. =2, NO, =3, NO2 check EPISODE for index # of gas species

if option==2
    interp_method='spline';
end
if option==3
    interp_method='cubic';
end   
if option==4
    interp_method='makima';
end

filename=strcat(data_path,filename);   %% data file name   
%% 1.1: read orignal data from data file
delimiterIn = ' ';
headerlinesIn = 1;% BC points is not included and not read from data file. 
disp(filename);
A = importdata(filename,delimiterIn,headerlinesIn);  %% read oringal data from output data file

    ic_gas=zeros(length(ic_name),1); %%index of selected gas species.
    c_gas=zeros(length(ic_name),1); %% concentration of selected gas species.
    
for nn=1:length(ic_name)
for mm=1:length(A.textdata)
    tr=strcmp(ic_name(nn), A.textdata(mm));
    if tr==1
        ic_gas(nn)=mm-ic_coladd-date_coladd; %% gas index. =2, NO, =3, NO2 check EPISODE for index # of gas species
    end
end
end


[num_row,num_col]=size(A.data);  %% get the size of orignal data file
   
%% 1.1.1: get the x,y,z range and dx,dy,dz from data file
    num_i=1;
    num_j=1;
    num_k=1;    
for i=1:num_row
    
if i==1
    x0(num_i,1)=A.data(i,5);
    y0(num_j,1)=A.data(i,6);
    z0(num_k,1)=A.data(i,7);
else  
    num_ii=0;    %% =0 if a new x value is read from data file; =1 if no new x value is read
    for ii=1:num_i  %% if any x0 value same with current x value from data file
        if x0(ii,1)==A.data(i,5)   
            num_ii=1;  
        end
    end
    if num_ii==0  %% if a new x is read, then it is record in x0
        num_i=num_i+1;
        x0(num_i,1)=A.data(i,5);%% get all x value for cells
    end
    
  
    num_jj=0;
    for jj=1:num_j
        if y0(jj,1)==A.data(i,6)
            num_jj=1;
        end
    end
    if num_jj==0
        num_j=num_j+1;
        y0(num_j,1)=A.data(i,6);
    end
    
    
    num_kk=0;
    for kk=1:num_k    
        if z0(kk,1)==A.data(i,7)
         num_kk=1;
        end
    end
    if num_kk==0       
        num_k=num_k+1;
        z0(num_k,1)=A.data(i,7);
    end    
end

end %% end of for i=1:num_row

x=sort(x0);  %%% x, y, z value for data point in orignal data file for simluation domain
y=sort(y0);
z=sort(z0);

dx=(max(x)-min(x))/(num_i-1);  %% dx, dy and dz for orignal data points (grid) from data file in simluation domain
dy=(max(y)-min(y))/(num_j-1);

dz=zeros(num_k,1);

for k=1:num_k
    if k==1
    dz(k)=z(k);    %% dz for 1st layer !!! when grid point is central point, this equation needs to be changed
    else
        dz(k)=z(k)-z(k-1);  %% dz for k layer !!! when grid point is central point, this equation needs to be changed
    end
    
end


%% 1.2. estimate the location (index) of selected point in orignal domain matrix
%ix=fix((location(1)-x(1))/dx)+1;  %% index of west side point for cell that selected point stays
%iy=fix((location(2)-y(1))/dy)+1;  %% index of south side point for cell that selected point stays

for i=1:num_i  
    if location(1)>=x(i) && location(1)<=x(num_i)+dx/2
        ix=i;      %% x index of selected point 
    end
    if location(1)<x(1) && location(1)>=x(1)-dx/2
        ix=0; %% for point in west side for 1st layer points      
    end
end
    if option == 1
        if location(1)<x(1) || location(1)>x(num_i)
            nrun_stop=1;
        end
    end
    if option > 1
    if location(1)<x(1)-dx/2 || location(1)>x(num_i)+dx/2
        nrun_stop=1;
    end
    end
    
for j=1:num_j  
    if location(2)>=y(j) && location(2)<=y(num_j)+dy/2
        iy=j;      %% y index of selected point 
    end
    if location(2)<y(1) && location(2)>=y(1)-dy/2
        iy=0; %% for point in south side for 1st layer points      
    end
end
    if option == 1
        if location(2)<y(1) || location(2)>y(num_j)
            nrun_stop=1;
        end
    end
    if option >1
    if location(2)<y(1)-dy/2 || location(2)>y(num_j)+dy/2
        nrun_stop=1;
    end   
    end

for k=1:num_k  
    if location(3)>=z(k) && location(3)<=z(num_k)+dz(num_k)/2
        iz=k;      %% index of lower side point for cell that selected point stays
    end
    if location(3)<z(1)
        iz=0; %% for point lower than 1st layer points      
    end
    if location(3)>z(num_k)+dz(num_k)/2
        nrun_stop=1;
    end    
end

if nrun_stop == 1  %% if out of domain, stop and print out message
    f=msgbox("outside of domain! STOP!");
    fileID = fopen('err.txt','w');
    fprintf(fileID,"outside of domain! STOP!"); 
	fclose(fileID);
end

for nn=1:length(ic_name)
if ic_gas(nn) ==0
     ic_gas_run=0;
     f=msgbox("No such gas species or check gas species name! STOP!");   
end
end

if option >4
     f=msgbox("interpolation option value does not exist! STOP!");   
end


if nrun_stop == 0 && ic_gas_run~=0 && option<=4 %% if inside simluation domain. then use option=1 or =2 to interpolate results in selected location

for nn=1:length(ic_name)
%% 1.3. transfer orignal data to matix for following calculation
B_data=zeros(num_col-date_coladd,num_i,num_j,num_k);   %% read x,y,z and species concentration c(ic=1:nc) to matix
for k=1:num_k
    for i=1:num_i
        for j=1:num_j
            for in=1:num_col-4
                ijk=j+(i-1)*num_j+(k-1)*num_i*num_j;
                B_data(in,i,j,k)=A.data(ijk,in+date_coladd);  
            end
        end
    end
end

%% ************************************************************************
%% Part 2: Interpolation of data to selected point
%% ************************************************************************
%% 2.1. Trilinear interpolation method (in a cubic)
if option ==1  
    %% trilinear interpolation for points inside the domain points (outside domain points, there is no interpolation now)
   
    if iz>=1 && iz< num_k 
        %% for location sits inside domain points (z(1) <= z <= z(NZ))
    x_d=(location(1)-x(ix))/(x(ix+1)-x(ix));
    y_d=(location(2)-y(iy))/(y(iy+1)-y(iy));
    z_d=(location(3)-z(iz))/(z(iz+1)-z(iz));
    
    c00(:,1)=B_data(:,ix,iy,iz)*(1-x_d)+B_data(:,ix+1,iy,iz)*x_d;
    c01(:,1)=B_data(:,ix,iy,iz+1)*(1-x_d)+B_data(:,ix+1,iy,iz+1)*x_d;
    c10(:,1)=B_data(:,ix,iy+1,iz)*(1-x_d)+B_data(:,ix+1,iy+1,iz)*x_d; 
    c11(:,1)=B_data(:,ix,iy+1,iz+1)*(1-x_d)+B_data(:,ix+1,iy+1,iz+1)*x_d;
    
    c0(:,1)=c00(:,1)*(1-y_d)+c10(:,1)*y_d;
    c1(:,1)=c01(:,1)*(1-y_d)+c11(:,1)*y_d;
    
    c(:,1)=c0(:,1)*(1-z_d)+c1(:,1)*z_d;
    end
    
    if iz==0  
 %% for location sits below 1st layer domain point height (z<z(1))
    
    x_d=(location(1)-x(ix))/(x(ix+1)-x(ix));
    y_d=(location(2)-y(iy))/(y(iy+1)-y(iy));
    z_d=(location(3)-z(iz+1))/(z(iz+2)-z(iz+1)); 
        
    c00(:,1)=B_data(:,ix,iy,iz+1)*(1-z_d)+B_data(:,ix,iy,iz+2)*z_d;
    c01(:,1)=B_data(:,ix,iy+1,iz+1)*(1-z_d)+B_data(:,ix,iy+1,iz+2)*z_d;
    c10(:,1)=B_data(:,ix+1,iy,iz+1)*(1-z_d)+B_data(:,ix+1,iy,iz+2)*z_d; 
    c11(:,1)=B_data(:,ix+1,iy+1,iz+1)*(1-z_d)+B_data(:,ix+1,iy+1,iz+2)*z_d;
    
    c0(:,1)=c00(:,1)*(1-x_d)+c10(:,1)*x_d;
    c1(:,1)=c01(:,1)*(1-x_d)+c11(:,1)*x_d;
    
    c(:,1)=c0(:,1)*(1-y_d)+c1(:,1)*y_d;   
    end
     
    if iz==num_k  
 %% for location sits above last layer domain point height (z>z(NZ)), however, since c(NC,NX+2,NY+2,NZ+1) includes c() above NZ layer, so if use c(:,:,:,NZ+1) for interpolation?? (z(NZ+1==?) 
    
    x_d=(location(1)-x(ix))/(x(ix+1)-x(ix));
    y_d=(location(2)-y(iy))/(y(iy+1)-y(iy));
    z_d=(location(3)-z(iz-1))/(z(iz)-z(iz-1)); 
        
    c00(:,1)=B_data(:,ix,iy,iz-1)*(1-z_d)+B_data(:,ix,iy,iz)*z_d;
    c01(:,1)=B_data(:,ix,iy+1,iz-1)*(1-z_d)+B_data(:,ix,iy+1,iz)*z_d;
    c10(:,1)=B_data(:,ix+1,iy,iz-1)*(1-z_d)+B_data(:,ix+1,iy,iz)*z_d; 
    c11(:,1)=B_data(:,ix+1,iy+1,iz-1)*(1-z_d)+B_data(:,ix+1,iy+1,iz)*z_d;
    
    c0(:,1)=c00(:,1)*(1-x_d)+c10(:,1)*x_d;
    c1(:,1)=c01(:,1)*(1-x_d)+c11(:,1)*x_d;
    
%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
%% <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    c(:,1)=c0(:,1)*(1-y_d)+c1(:,1)*y_d;  
%%<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    end   
    
%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%% <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c_gas(nn)=c(ic_gas(nn)+ic_coladd,1); %% output the concentration for gas species in selected location 
%%<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
end %% end of option=1



%% ************************************************************************
%% 2.2 option = 2 for interpolation (interp3 function for 1st level finer interpolation. Then trilinear/nearest interpolation for selected location based on finer grid data points)
%%================================================

if option>=2  %%% interp3 option (not just trilinear)
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% 2.2.1 parameters for interp3() function 
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
num_ref=4;  %% number of orignal data points in each direction for tricubic interpolation?????
num_startside=fix(num_ref/2); %% used to count the start point for interpolation. Affected by num_ref.
num_endside=fix(num_ref/2); %% used to count the end point for interpolation. Affected by num_ref.
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%% 2.2.2 finding out start and end points for interp3() interpolation

if ix>1 && ix<num_i  %% find out the original start and end point for interpolation
    ix_start=ix-num_startside+1;
end
if ix<=num_startside-1
    ix_start=1;
end
if ix>=num_i-num_endside+1
    ix_start=num_i-num_ref+1;
end
ix_end=ix_start+num_ref-1;

    
if iy>1 && iy<num_j
    iy_start=iy-num_startside+1;
end
if iy<=num_startside-1
    iy_start=1;
end
if iy>=num_j-1
    iy_start=num_j-num_ref+1;
end
iy_end=iy_start+num_ref-1;

    
if iz>1 && iz<num_k
    iz_start=iz-num_startside+1;
end
if iz<=num_startside-1
    iz_start=1;
end
if iz>=num_k-1
    iz_start=num_k-num_ref+1; 
end
iz_end=iz_start+num_ref-1;

%% 2.2.3. interp3() for level 1 interpolation when option=2 (output data:  cq @ (xq,yq,zq))
    for k=1:num_ref
        for j=1:num_ref
            for i=1:num_ref    
                V.data(j,i,k)=B_data(ic_gas(nn)+ic_coladd,ix_start+i-1,iy_start+j-1,iz_start+k-1);  %% data matix for gas species (ic_gas: ic index. =2, NO) that close to the selected loation with at least four point in x, y, z direction. 
                % matrix is at V.data(j,i,k) as xx1 will be at xx1(j,i,k) not at xx1(i,j,k).  
            end
        end
    end
    
%% 2.2.3.1. finer grid resolution (can be changed based on requirements)     
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
dxx=5; %% space for finer grid that used to interpolating coarse data.  unit:m.  Will be changed for different resolution requirement.
dyy=5;
dzz=1; 
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
%% 2.2.3.2 orignal grid from data file (x,y,z)
for i=1:num_ref %%% orignal point for interpolation.  need to be changed if more orignal data points (num_ref) are used in each direction for interpolation. 
    xx(i)=x(ix_start+i-1);
    yy(i)=y(iy_start+i-1);
    zz(i)=z(iz_start+i-1);
end

[xx1,yy1,zz1]=meshgrid(xx,yy,zz); %% generate mesh grid for coarse orignal points

%% 2.2.3.3 Finer grid (xq,yq,zq) 

if ix>=1 && ix<=num_i-1
    xq=x(ix_start):dxx:x(ix_end);  %% finer grid for interpolation based on orignal coarse data. need to be changed if more orignal data points (num_ref) are used in each direction for interpolation. 
end
if ix==0
    xq=x(ix_start)-dx/2:dxx:x(ix_end);
end
if ix>num_i-1
    xq=x(ix_start):dxx:x(ix_end)+dx/2;
end


if iy>=1 && iy<=num_j-1
    yq=y(iy_start):dyy:y(iy_end);  %% finer grid for interpolation based on orignal coarse data. need to be changed if more orignal data points (num_ref) are used in each direction for interpolation. 
end
if iy==0
    yq=y(iy_start)-dy/2:dyy:y(iy_end);
end
if iy>num_j-1
    yq=y(iy_start):dyy:y(iy_end)+dy/2;
end


if iz>=1 && iz<=num_k-1
    zq=z(iz_start):dzz:z(iz_end);
end
if iz==0
    zq=0:dzz:z(iz_end);
end
if iz>num_k-1
    zq=z(iz_start):dzz:z(iz_end)+dz(iz_end)/2;
end

[xq1,yq1,zq1]=meshgrid(xq,yq,zq); %% generate finer grid mesh.  using meshgrid, then xq1(length(yq),length(xq),lenght(zq)), yq1(length(yq),length(xq),lenght(zq)), zq1(length(yq),length(xq),lenght(zq))
%end


%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%% <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cq=interp3(xx1,yy1,zz1,V.data,xq1,yq1,zq1,interp_method);  %% cq: finer data point @ 10m resolution????  cq(j,i,k) not cq(i,j,k)
%%<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


%% 2.2.4. level 2 interpolation for option=2. (opt2=1, trilinear; opt2=2, nearest point)
%%2.2.4.1 method control variable for level 2 interpolation. (default, opt2=1)
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
opt2 =1; %% interpolation method for further finding the gas concentration at selected location based on the finer interpolated data points. opt2=1, using trilinear method
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%% 2.2.4.2 finding out location of selected point in finer grid

    ixx=1;
for i=1:length(xq)  
    if location(1)>=xq(i)
        ixx=i;      
    end
end

    iyy=1;
for j=1:length(yq)  
    if location(2)>=yq(j)
        iyy=j;      
    end
end

    izz=1;
for k=1:length(zq)  
    if location(3)>=zq(k)
        izz=k;      
    end
%     if location(3)<zq(1)
%         iz=0; %% for point lower than 1st layer points      
%     end
end


%% 2.2.4.3 level 2 interpolation
if opt2 ==1  %% trilinear interpolation for points inside the finer grids
    
    xx_d=(location(1)-xq(ixx))/(xq(ixx+1)-xq(ixx));
    yy_d=(location(2)-yq(iyy))/(yq(iyy+1)-yq(iyy));
    zz_d=(location(3)-zq(izz))/(zq(izz+1)-zq(izz));
    
    cc00=cq(iyy,ixx,izz)*(1-xx_d)+cq(iyy,ixx+1,izz)*xx_d;
    cc01=cq(iyy,ixx,izz+1)*(1-xx_d)+cq(iyy,ixx+1,izz+1)*xx_d;
    cc10=cq(iyy+1,ixx,izz)*(1-xx_d)+cq(iyy+1,ixx+1,izz)*xx_d; 
    cc11=cq(iyy+1,ixx,izz+1)*(1-xx_d)+cq(iyy+1,ixx+1,izz+1)*xx_d;
    
    cc0=cc00*(1-yy_d)+cc10*yy_d;
    cc1=cc01*(1-yy_d)+cc11*yy_d;
    
%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%% <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<   
    c_gas(nn)=cc0*(1-zz_d)+cc1*zz_d;  %% gas concentration at selected location.
%%<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

end  %% end of opt2 (trilinear interpolation for level 2)


if opt2 ==2 %% neareast point
    dis=zeros(8,2);  
    for i=1:2
        for j=1:2
            for k=1:2
                ijk=k+2*(j-1)+2*2*(i-1);
                dis(ijk,1)=sqrt((xq(ixx+i-1)-location(1))^2+(yq(iyy+j-1)-location(2))^2+(zq(izz+k-1)-location(3))^2); %% distance
                dis(ijk,2)=cq(iyy+j-1,ixx+i-1,izz+k-1); %% species concentration
            end
        end
    end

[dis_sort, dis_index]=sort(dis(:,1));  %% sort the data based on distance (near to far)
%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%% <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
c_gas(nn)=dis(dis_index(1)); %%%% gas concentration at selected location.
%%<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 

end % end of opt2=2. (nearest point for level 2).


end  %% end of option=2
c_gas(nn)=max(c_gas(nn),0.0);
end %% end of nn=1:length(ic_name).

CreateStruct.Interpreter = 'tex';
CreateStruct.WindowStyle = 'modal';
Message=cell(length(ic_name),1);
fileID = fopen('exp.csv','w');
for nn=1:length(ic_name)
    fprintf(fileID,'%s,%f \n',char(ic_name(nn)),c_gas(nn)); 
end%
fclose(fileID);
end %% end of nrun_stop =0

end %% end of function
