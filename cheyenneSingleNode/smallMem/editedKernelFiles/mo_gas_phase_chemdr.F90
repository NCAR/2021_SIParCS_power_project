!KGEN-generated Fortran source file

!Generated at : 2016-03-01 11:27:40
!KGEN version : 0.6.2

module mo_gas_phase_chemdr

    USE shr_kind_mod, ONLY: r8 => shr_kind_r8
    USE chem_mods, ONLY: rxntot, gas_pcnst
    USE chem_mods, ONLY: extcnt
    USE ppgrid, ONLY: pver

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    USE kgen_extensions_mod, ONLY: read_runtime_options, variable_stats, perturb_flag
    IMPLICIT NONE
    SAVE

    PRIVATE
    PUBLIC gas_phase_chemdr


!
! CCMI
!




contains





    !-----------------------------------------------------------------------

   
!
! CCMI
!
!


    

!
!




! SAM       call add_default(tag_names(n),5,' ')

! SAM       call add_default(pht_names(n),5,' ')

! SAM       call add_default(rxn_names(n),5,' ')






!-----------------------------------------------------------------------
! get pbuf indicies
!-----------------------------------------------------------------------



    ! diagnostics for stratospheric heterogeneous reactions



!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
SUBROUTINE gas_phase_chemdr(kgen_unit, kgen_total_time, kgen_isverified, lchnk, ncol, delt,filename,rank)

    !-----------------------------------------------------------------------
    !     ... Chem_solver advances the volumetric mixing ratio
    !         forward one time step via a combination of explicit,
    !         ebi, hov, fully implicit, and/or rodas algorithms.
    !-----------------------------------------------------------------------

    USE mo_imp_sol, ONLY: imp_sol
!
! LINOZ
!
!
! for aqueous chemistry and aerosol growth
!


    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    USE mo_imp_sol, ONLY: kr_externs_out_mo_imp_sol
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
    USE kgen_utils_mod, ONLY: kgen_perturb_real

#ifdef _MPI
    use mpi
#endif


    IMPLICIT NONE

    LOGICAL, INTENT(OUT) :: kgen_isverified
    !-----------------------------------------------------------------------
    !        ... Dummy arguments
    !-----------------------------------------------------------------------
    INTEGER, INTENT(INOUT) :: lchnk
    INTEGER, INTENT(INOUT) :: ncol
    REAL(KIND=r8), INTENT(INOUT) :: delt
    CHARACTER(len=*) :: filename
    INTEGER, INTENT(IN) :: rank



    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------


    REAL(KIND=r8) :: extfrc(ncol,pver,max(1,extcnt))
    REAL(KIND=r8) :: vmr(ncol,pver,gas_pcnst)
    REAL(KIND=r8) :: reaction_rates_chnks(ncol*pver,max(1,rxntot))
    INTEGER :: chnkpnts
    REAL(KIND=r8) :: het_rates_chnks(ncol*pver,max(1,gas_pcnst))




  ! for aerosol formation....  

!
! CCMI
!
!
! jfl
!
!


    ! initialize to NaN to hopefully catch user defined rxts that go unset
    INTEGER, INTENT(IN) :: kgen_unit
    REAL(KIND=kgen_dp), INTENT(INOUT) :: kgen_total_time
    LOGICAL :: kgen_istrue
    REAL(KIND=8) :: kgen_array_sum
    
    REAL(KIND=r8) :: kgenref_vmr(ncol,pver,gas_pcnst)
    TYPE(check_t) :: check_status
    INTEGER*8 :: kgen_intvar, kgen_start_clock, kgen_stop_clock, kgen_rate_clock
    real(KIND=r8) :: start_clock,stop_clock
    INTEGER, PARAMETER :: kgen_maxiter = 100
    REAL(KIND=kgen_dp) :: kgen_elapsed_time
    real(kind=kgen_dp) :: tolerance

    ! mpi
    integer :: iError

    integer :: iloop

    !-----------------------------------------------------------------------      
    !        ... Get chunck latitudes and longitudes
    !-----------------------------------------------------------------------      


    !-----------------------------------------------------------------------      
    !        ... Calculate cosine of zenith angle
    !            then cast back to angle (radians)
    !-----------------------------------------------------------------------      


    !-----------------------------------------------------------------------      
    !        ... Xform geopotential height from m to km 
    !            and pressure from Pa to mb
    !-----------------------------------------------------------------------      


    !-----------------------------------------------------------------------      
    !        ... map incoming concentrations to working array
    !-----------------------------------------------------------------------      



    !-----------------------------------------------------------------------      
    !        ... Set atmosphere mean mass
    !-----------------------------------------------------------------------      

    !-----------------------------------------------------------------------      
    !        ... Xform from mmr to vmr
    !-----------------------------------------------------------------------      

!
! CCMI
!
! reset STE tracer to specific vmr of 200 ppbv
!

!
! reset AOA_NH, NH_5, NH_50, NH_50W surface mixing ratios between 30N and 50N
!


       !-----------------------------------------------------------------------      
       !        ... store water vapor in wrk variable
       !-----------------------------------------------------------------------      
       !-----------------------------------------------------------------------      
       !        ... Xform water vapor from mmr to vmr and set upper bndy values
       !-----------------------------------------------------------------------      



    !-----------------------------------------------------------------------      
    !        ... force ion/electron balance
    !-----------------------------------------------------------------------      

    !-----------------------------------------------------------------------      
    !        ... Set the "invariants"
    !-----------------------------------------------------------------------  

    !-----------------------------------------------------------------------      
    !        ... stratosphere aerosol surface area
    !-----------------------------------------------------------------------  

       ! Prognostic modal stratospheric sulfate: compute dry strato_sad


       !-----------------------------------------------------------------------      
       !        ... initialize condensed and gas phases; all hno3 to gas
       !-----------------------------------------------------------------------    




       !-----------------------------------------------------------------------      
       !        ... call SAD routine
       !-----------------------------------------------------------------------      

!      NOTE: output of total HNO3 is before vmr is set to gas-phase.




!
!
!
!

       !-----------------------------------------------------------------------      
       !        ... call aerosol reaction rates
       !-----------------------------------------------------------------------      



!      NOTE: For gas-phase solver only. 
!            ratecon_sfstrat needs total hcl.


    !-----------------------------------------------------------------------      
    !        ... Set the column densities at the upper boundary
    !-----------------------------------------------------------------------      

    !-----------------------------------------------------------------------      
    !       ...  Set rates for "tabular" and user specified reactions
    !-----------------------------------------------------------------------      
    

    
    !-----------------------------------------------------------------
    ! ... zero out sulfate above tropopause
    !-----------------------------------------------------------------




    !-----------------------------------------------------------------
    !-----------------------------------------------------------------


    










    !-----------------------------------------------------------------------
    !        ... Compute the photolysis rates at time = t(n+1)
    !-----------------------------------------------------------------------      
    !-----------------------------------------------------------------------      
    !-----------------------------------------------------------------------      

    !-----------------------------------------------------------------------      
    !-----------------------------------------------------------------------      





       !-----------------------------------------------------------------
       !-----------------------------------------------------------------


       !-----------------------------------------------------------------
       !-----------------------------------------------------------------



    !-----------------------------------------------------------------------      
    !-----------------------------------------------------------------------  

    !-----------------------------------------------------------------------
    !        ... Compute the extraneous frcing at time = t(n+1)
    !-----------------------------------------------------------------------      

    !-----------------------------------------------------------------------
    !        ... Compute the extraneous frcing at time = t(n+1)
    !-----------------------------------------------------------------------      



    !-----------------------------------------------------------------------
    !        ... Form the washout rates
    !-----------------------------------------------------------------------      

!
! CCMI
!
! set loss to below the tropopause only
!


!





    ! save h2so4 before gas phase chem (for later new particle nucleation)



    !=======================================================================
    !        ... Call the class solution algorithms
    !=======================================================================
    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------
!!$    call exp_sol( vmr, reaction_rates, het_rates, extfrc, delt, invariants(1,1,indexm), ncol, lchnk, ltrop_sol )

!!$    call exp_sol( vmr, reaction_rates_chnks, het_rates, extfrc, delt,  ncol, lchnk, chnkpnts )

    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------

    !
     !!! imp_sol( base_sol, reaction_rates, het_rates, extfrc, delt,  ncol, lchnk, chnkpnts )
    !$kgen callsite imp_sol
    
    !local input variables
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgen_array_sum
        READ (UNIT = kgen_unit) extfrc
        CALL kgen_array_sumcheck("extfrc", kgen_array_sum, REAL(SUM(extfrc), 8), .TRUE.)
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgen_array_sum
        READ (UNIT = kgen_unit) vmr
        CALL kgen_array_sumcheck("vmr", kgen_array_sum, REAL(SUM(vmr), 8), .TRUE.)
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgen_array_sum
        READ (UNIT = kgen_unit) reaction_rates_chnks
        CALL kgen_array_sumcheck("reaction_rates_chnks", kgen_array_sum, REAL(SUM(reaction_rates_chnks), 8), .TRUE.)
    END IF 
    READ (UNIT = kgen_unit) chnkpnts
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgen_array_sum
        READ (UNIT = kgen_unit) het_rates_chnks
        CALL kgen_array_sumcheck("het_rates_chnks", kgen_array_sum, REAL(SUM(het_rates_chnks), 8), .TRUE.)
    END IF 
    
    !extern output variables
    CALL kr_externs_out_mo_imp_sol(kgen_unit)
    
    !local output variables
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgen_array_sum
        READ (UNIT = kgen_unit) kgenref_vmr
        CALL kgen_array_sumcheck("kgenref_vmr", kgen_array_sum, REAL(SUM(kgenref_vmr), 8), .TRUE.)
    END IF 
    
    !Uncomment following call statement to turn on perturbation experiment.
    !Adjust perturbation value and/or kind parameter if required.
    !CALL kgen_perturb_real( your_variable, 1.0E-15_8 )

   if (perturb_flag .eqv. .true.) then
        call kgen_perturb_real(reaction_rates_chnks, epsilon(1.0_8))
   endif

   !if (stats_flag .eqv. .true.) then
     !write(*,*) "-- Variable Stats -- "
     !call variable_stats(extfrc, "extfrc")
     !call variable_stats(vmr, "vmr")
     !call variable_stats(reaction_rates_chnks, "reaction_rates_chnks")
     !call variable_stats(het_rates_chnks, "het_rates_chnks")
     !write(*,*) "-- standard run --"
   !endif
   
   do iloop=1,25 
    !call to kgen kernel
    CALL kgen_kernel



!
! jfl : CCMI : implement O3S here because mo_fstrat is not called
!


       ! mmr_new = average of mmr values before and after imp_sol

    ! save h2so4 change by gas phase chem (for later new particle nucleation)


!
! Aerosol processes ...
!




       !-----------------------------------------------------------------------      
       !         ... aerosol settling
       !             first settle hno3(2) using radius ice
       !             secnd settle hno3(3) using radius large nat
       !-----------------------------------------------------------------------      


       !-----------------------------------------------------------------------      
       !-----------------------------------------------------------------------      
!      NOTE: vmr for hcl and hno3 is gas-phase at this point.
!            hno3_cond(:,k,1) = STS; hno3_cond(:,k,2) = NAT
   
              



!
! LINOZ
!


    !-----------------------------------------------------------------------      
    !         ... Check for negative values and reset to zero
    !-----------------------------------------------------------------------      

    !-----------------------------------------------------------------------      
    !         ... Set upper boundary mmr values
    !-----------------------------------------------------------------------      

    !-----------------------------------------------------------------------      
    !         ... Set fixed lower boundary mmr values
    !-----------------------------------------------------------------------      

    !----------------------------------------------------------------------- 
    ! set NOy UBC     
    !-----------------------------------------------------------------------      



    !-----------------------------------------------------------------------      
    !         ... Xform from vmr to mmr
    !-----------------------------------------------------------------------      


    !-----------------------------------------------------------------------      
    !         ... Form the tendencies
    !----------------------------------------------------------------------- 











!
! jfl
!
! surface vmr
!


!
!
!


    
    !verify init
    tolerance=1.0e-13
    CALL kgen_init_check(check_status, rank, tolerance=tolerance, verboseLevel=1)
    
    !extern verify variables
    
    !local verify variables
    CALL kv_gas_phase_chemdr_real__r8_dim3("vmr", check_status, vmr, kgenref_vmr)
    if(check_status%rank==0) WRITE (*, *) ""
    IF (check_status%verboseLevel > 0) THEN
        if(check_status%rank==0) then 
        WRITE (*, *) "Number of verified variables: ", check_status%numTotal
        WRITE (*, *) "Number of identical variables: ", check_status%numIdentical
        WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol
        WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol
        WRITE (*, *) "Tolerance: ", check_status%tolerance
        endif
    END IF 
    if(check_status%rank==0) WRITE (*, *) ""
    IF (check_status%numOutTol > 0) THEN
        if(check_status%rank==0) WRITE (*, *) "Verification FAILED for (" // TRIM(filename) // ")"
        check_status%Passed = .FALSE.
        kgen_isverified = .FALSE.
    ELSE
        if(check_status%rank==0) WRITE (*, *) "Verification PASSED for (" // TRIM(filename) // ")"
        check_status%Passed = .TRUE.
        kgen_isverified = .TRUE.
    END IF 
    if(check_status%rank==0) WRITE (*, *) ""
    
    !Measuring elapsed time. Please increase the value of kgen_maxiter to get improve timing measurment resolution.
#ifdef _MPI
    call MPI_Barrier(MPI_COMM_WORLD, iError)
!    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
!    call sleep(rank+1)
#endif
    CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock)
!    call clock(start_clock)
    DO kgen_intvar = 1, kgen_maxiter
        CALL kgen_kernel
    END DO 
    CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock)
    kgen_elapsed_time = 1.0e6*(kgen_stop_clock - kgen_start_clock)/REAL(kgen_rate_clock*kgen_maxiter)
! Use a different timing of running on NEC Aurora
!    call clock(stop_clock)
!    kgen_elapsed_time = 1.0e6*(stop_clock - start_clock)/REAL(kgen_maxiter)
    !WRITE (*, *) "imp_sol : Time per call (usec): ", kgen_elapsed_time
    kgen_total_time = kgen_total_time + kgen_elapsed_time
    
    enddo
    CONTAINS
    
    !kgen kernel subroutine
    SUBROUTINE kgen_kernel()
    call imp_sol( vmr, reaction_rates_chnks, het_rates_chnks, extfrc, delt,  ncol, lchnk, chnkpnts )
    END SUBROUTINE kgen_kernel
    
    !verify state subroutine for kv_gas_phase_chemdr_real__r8_dim3
    RECURSIVE SUBROUTINE kv_gas_phase_chemdr_real__r8_dim3(varname, check_status, var, kgenref_var)
        CHARACTER(LEN=*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        REAL(KIND=r8), INTENT(IN), DIMENSION(:,:,:) :: var, kgenref_var
        INTEGER :: check_result
        LOGICAL :: is_print = .FALSE.
        
        INTEGER :: idx1, idx2, idx3
        INTEGER :: n
        real(KIND=r8) :: nrmsdiff, rmsdiff
        real(KIND=r8), ALLOCATABLE :: buf1(:,:,:), buf2(:,:,:)
        
        check_status%numTotal = check_status%numTotal + 1
        
        IF (ALL(var == kgenref_var)) THEN
            check_status%numIdentical = check_status%numIdentical + 1
            IF (check_status%verboseLevel > 1) THEN
                if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
            END IF 
            check_result = CHECK_IDENTICAL
        ELSE
            ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
            ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
            n = SIZE(var)
            WHERE ( ABS(kgenref_var) > check_status%minvalue )
                buf1 = ((var-kgenref_var)/kgenref_var)**2
                buf2 = (var-kgenref_var)**2
            ELSEWHERE
                buf1 = (var-kgenref_var)**2
                buf2 = buf1
            END WHERE 
            nrmsdiff = SQRT(SUM(buf1)/REAL(n))
            rmsdiff = SQRT(SUM(buf2)/REAL(n))
            IF (rmsdiff > check_status%tolerance) THEN
                check_status%numOutTol = check_status%numOutTol + 1
                IF (check_status%verboseLevel > 0) THEN
                    if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                END IF 
                check_result = CHECK_OUT_TOL
            ELSE
                check_status%numInTol = check_status%numInTol + 1
                IF (check_status%verboseLevel > 1) THEN
                    if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                END IF 
                check_result = CHECK_IN_TOL
            END IF 
        END IF 
        IF (check_result == CHECK_IDENTICAL) THEN
            IF (check_status%verboseLevel > 2) THEN
                if(check_status%rank==0) then 
                WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                WRITE (*, *) "RMS of difference is ", 0
                WRITE (*, *) "Normalized RMS of difference is ", 0
                WRITE (*, *) ""
                endif
            END IF 
        ELSE IF (check_result == CHECK_OUT_TOL) THEN
            IF (check_status%verboseLevel > 0) THEN
                if(check_status%rank==0) then 
                WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                WRITE (*, *) "RMS of difference is ", rmsdiff
                WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                WRITE (*, *) ""
                endif
            END IF 
        ELSE IF (check_result == CHECK_IN_TOL) THEN
            IF (check_status%verboseLevel > 1) THEN
                if(check_status%rank==0) then 
                WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                WRITE (*, *) "RMS of difference is ", rmsdiff
                WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                WRITE (*, *) ""
                endif
            END IF 
        END IF 
        
    END SUBROUTINE kv_gas_phase_chemdr_real__r8_dim3
    
END SUBROUTINE gas_phase_chemdr

end module mo_gas_phase_chemdr
