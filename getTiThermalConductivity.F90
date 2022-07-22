    !-----------------------------------------------------
    ! Written By: Anil Kunwar (Original 2015-03-13) (Modification 2021-11-16)
    ! material property user defined function for ELMER:
    ! Thermal conductivity of titanium fitted as a function of temperature
    ! (kth_ti)solid = A*T^2 + B*T + C, where A = -1.66755674e-06 W K/m and B = 4.80727332e-03 W/m and C = 1.47783620e+01 W/mK
    ! X.P. Zhang  et al. J. Mater. Sci. (2005), Vol. 40:4911-4916
    ! https://link.springer.com/content/pdf/10.1007/s10853-005-0418-0.pdf
    ! (kth_ti)liquid = D*T + E, where D = 0.017 W/m and E = 0.969 W/m K
    ! https://www.sciencedirect.com/science/article/pii/S0167732220373803
    !-----------------------------------------------------
    FUNCTION getThermalConductivity( model, n, temp ) RESULT(thcondt)
    ! modules needed
    USE DefUtils
    IMPLICIT None
    ! variables in function header
    TYPE(Model_t) :: model
    INTEGER :: n
    REAL(KIND=dp) :: temp, thcondt

    ! variables needed inside function
    REAL(KIND=dp) :: refThCond, alpha, beta, delta, lambda, refTemp
    Logical :: GotIt
    TYPE(ValueList_t), POINTER :: material

    ! get pointer on list for material
    material => GetMaterial()
    IF (.NOT. ASSOCIATED(material)) THEN
    CALL Fatal('getThermalConductivity', 'No material found')
    END IF

    ! read in reference conductivity at reference temperature
    refThCond = GetConstReal( material, 'Reference Thermal Conductivity C Solid Ti',GotIt)
    IF(.NOT. GotIt) THEN
    CALL Fatal('getThermalConductivity', 'Reference Thermal Conductivity Solid Ti not found')
    END IF

    ! read in Temperature Coefficient of Resistance
    alpha = GetConstReal( material, 'Cond Coeff B Solid Ti', GotIt)
    IF(.NOT. GotIt) THEN
    CALL Fatal('getThermalConductivity', 'slope of thermal conductivity-temperature curve solid not found')
    END IF
    
    ! read in Temperature Coefficient of Resistance
    beta = GetConstReal( material, 'Cond Coeff A Solid Ti', GotIt)
    IF(.NOT. GotIt) THEN
    CALL Fatal('getThermalConductivity', 'Coefficientt of T2 term solid Ti not found')
    END IF
    
    ! read in Temperature Coefficient of Resistance for Liquid Ti
    delta = GetConstReal( material, 'Cond Coeff D Liquid Ti', GotIt)
    IF(.NOT. GotIt) THEN
    CALL Fatal('getThermalConductivity', 'Coefficient of T term liquid Ti not found')
    END IF
    
    ! read in pseudo reference conductivity at reference temperature of liquid
    lambda = GetConstReal( material, 'Reference Thermal Conductivity E Liquid Ti',GotIt)
    IF(.NOT. GotIt) THEN
    CALL Fatal('getThermalConductivity', 'Reference Thermal Conductivity Liquid Ti not found')
    END IF

    ! read in reference temperature
    refTemp = GetConstReal( material, 'Melting Point Temperature Ti', GotIt)
    IF(.NOT. GotIt) THEN
    CALL Fatal('getThermalConductivity', 'Reference Temperature not found')
    END IF


    ! compute density conductivity
    IF (refTemp <= temp) THEN ! check for physical reasonable temperature
       CALL Warn('getThermalConductivity', 'The Ti material is in liquid state.')
            !CALL Warn('getThermalConductivity', 'Using density reference value')
    !thcondt = 1.11*(refThCond + alpha*(temp))
    thcondt = lambda + delta*(temp)
    ELSE
    thcondt = refThCond + alpha*(temp) + beta*(temp**2)
    END IF

    END FUNCTION getThermalConductivity

