!-> O3
      P = DBLE( DJ(3) ) * X(NO2,1)
      L = RC(5) * X(NO,1) + RC(160)
      F(O3)= P - L*X(O3,1)

!-> NO
      P = RC(200) + DJ(3) * X(NO2,1)
      L = RC(5) * X(O3,1)
      F(NO)= P - L*X(NO,1)

!-> NO2
      P = RC(201) + RC(5) * X(O3,1) * X(NO,1)
      L = RC(162) + DJ(3)
      F(NO2)= P - L*X(NO2,1)
