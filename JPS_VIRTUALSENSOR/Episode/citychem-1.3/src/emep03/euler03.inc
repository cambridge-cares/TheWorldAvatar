!-> O3
      P = DJ(3) * X(NO2,2)
      L = RC(5) * X(NO,2) + RC(160)
      X(O3,2)= (X(O3,1) + Ldt*P)/(1. + Ldt*L)

!-> NO
      P = RC(200) + DJ(3) * X(NO2,2)
      L = RC(5) * X(O3,2)
      X(NO,2)= (X(NO,1) + Ldt*P)/(1. + Ldt*L)

!-> NO2
      P = RC(201) + RC(5) * X(O3,2) * X(NO,2)
      L = RC(162) + DJ(3)
      X(NO2,2)= (X(NO2,1) + Ldt*P)/(1. + Ldt*L)
