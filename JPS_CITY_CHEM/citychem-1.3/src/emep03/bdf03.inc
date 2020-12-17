!-> O3
      P = DJ(3) * X(NO2,2)
      L = RC(5) * X(NO,2) + RC(160)
      X(O3,2) = (A1*X(O3,1)+A2*X(O3,0)+A3*Ldt*P)/(A4+A3*Ldt*L)

!-> NO
      P = RC(200) + DJ(3) * X(NO2,2)
      L = RC(5) * X(O3,2)
      X(NO,2) = (A1*X(NO,1)+A2*X(NO,0)+A3*Ldt*P)/(A4+A3*Ldt*L)

!-> NO2
      P = RC(201) + RC(5) * X(O3,2) * X(NO,2)
      L = RC(162) + DJ(3)
      X(NO2,2) = (A1*X(NO2,1)+A2*X(NO2,0)+A3*Ldt*P)/(A4+A3*Ldt*L)
