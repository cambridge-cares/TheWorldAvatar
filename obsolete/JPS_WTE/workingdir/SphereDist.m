function d = SphereDist(x,y,R)

if nargin < 3
    R = 6378.137;
end
x = D2R(x);
y = D2R(y);
DeltaS = acos(cos(x(2))*cos(y(2))*cos(x(1)-y(1))+sin(x(2))*sin(y(2)));
d = R*DeltaS;
end