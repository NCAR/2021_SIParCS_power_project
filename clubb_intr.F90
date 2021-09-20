!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:33 
!KGEN version : 0.8.1 
  


module clubb_intr
  !----------------------------------------------------------------------------------------------------- !
  ! Module to interface CAM with Cloud Layers Unified by Bi-normals (CLUBB), developed                   !
  !    by the University of Wisconsin Milwaukee Group (UWM).                                             !
  !                                                                                                      !
  ! CLUBB replaces the exisiting turbulence, shallow convection, and macrophysics in CAM5                !  
  !                                                                                                      !  
  ! Lastly, a implicit diffusion solver is called, and tendencies retrieved by                           !
  ! differencing the diffused and initial states.                                                        !
  !                                                                                                      ! 
  ! Calling sequence:                                                                                    !
  !                                                                                                      !
  !---------------------------Code history-------------------------------------------------------------- !
  ! Authors:  P. Bogenschutz, C. Craig, A. Gettelman                                                     ! 
  !                                                                                                      ! 
  !----------------------------------------------------------------------------------------------------- !


    USE shr_kind_mod, ONLY: r8=>shr_kind_r8 
    USE ppgrid, ONLY: pver, pverp, pcols 
    USE physconst, ONLY: cpair, gravit, latvap, latice 
    USE constituents, ONLY: pcnst 
    USE ref_pres, ONLY: top_lev => trop_cloud_top_lev 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 
    IMPLICIT NONE 

    PRIVATE 
#ifdef _MPI 
  include "mpif.h" 
#endif 
    SAVE 
  ! ----------------- !
  ! Public interfaces !
  ! ----------------- !


    PUBLIC clubb_tend_cam, stats_end_timestep_clubb 
            ! This utilizes CLUBB specific variables in its interface
  ! Both of these utilize CLUBB specific variables in their interface

    PRIVATE stats_zero, stats_avg 

  logical, public :: do_cldcool
  ! ------------ !
  ! Private data !
  ! ------------ !

  integer :: iloop

  integer, parameter :: &
      grid_type    = 3, &               ! The 2 option specifies stretched thermodynamic levels
      hydromet_dim = 0                  ! The hydromet array in SAM-CLUBB is currently 0 elements
   


  real(r8), parameter :: &
      theta0   = 300._r8, &             ! Reference temperature                     [K]
      ts_nudge = 86400._r8, &           ! Time scale for u/v nudging (not used)     [s]
      p0_clubb = 100000._r8
      
  integer, parameter :: & 
    sclr_dim = 0                        ! Higher-order scalars, set to zero

  real(r8), parameter :: &
    wp3_const = 1._r8                   ! Constant to add to wp3 when moments are advected
    
  real(r8), parameter :: & 
    wpthlp_const = 10.0_r8              ! Constant to add to wpthlp when moments are advected
    
  real(r8), parameter :: & 
    wprtp_const = 0.01_r8               ! Constant to add to wprtp when moments are advected
    
  real(r8), parameter :: & 
    rtpthlp_const = 0.01_r8             ! Constant to add to rtpthlp when moments are advected
    
  real(r8), parameter :: unset_r8 = huge(1.0_r8)
    
  real(r8) :: clubb_rnevap_effic = unset_r8

!  Constant parameters

  logical, parameter, private :: &
    l_uv_nudge       = .false.,       &  ! Use u/v nudging (not used)
    l_implemented    = .true.,        &  ! Implemented in a host model (always true)
    l_host_applies_sfc_fluxes = .false.  ! Whether the host model applies the surface fluxes
    
  logical, parameter, private :: &
    apply_to_heat    = .false.           ! Apply WACCM energy fixer to heat or not (.true. = yes (duh))  
    
  logical            :: lq(pcnst)
  logical            :: do_rainturb
  logical            :: do_expldiff
  logical            :: clubb_do_adv
  logical            :: clubb_do_energyfix   = .true.


  integer            :: edsclr_dim       ! Number of scalars to transport in CLUBB
!  define physics buffer indicies here       
 
 
  integer, public :: & 
    ixthlp2 = 0, &
    ixwpthlp = 0, &
    ixwprtp = 0, &
    ixwp2 = 0, &
    ixwp3 = 0, &
    ixrtpthlp = 0, &
    ixrtp2 = 0, &
    ixup2 = 0, &
    ixvp2 = 0


  !  Output arrays for CLUBB statistics    

  real(r8), allocatable, dimension(:,:,:) :: out_zt, out_zm, out_radzt, out_radzm, out_sfc


  PUBLIC kr_externs_in_clubb_intr 
  PUBLIC kr_externs_out_clubb_intr 
    
  PUBLIC kv_externs_clubb_intr 
  REAL(KIND=r8), allocatable, dimension(:,:,:) :: kgenref_out_zt, kgenref_out_zm, kgenref_out_radzt, kgenref_out_radzm, &
  &kgenref_out_sfc 

  contains
  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !
  


  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !


  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !

 


  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !

  


  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !


  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !
    
    

  SUBROUTINE clubb_tend_cam(kgen_unit, kgen_measure, kgen_isverified, kgen_filepath, cam_in, hdtime, cld_macmic_num_steps, &
  &macmic_it) 
!-------------------------------------------------------------------------------
! Description: Provide tendencies of shallow convection, turbulence, and 
!              macrophysics from CLUBB to CAM
! Author: Cheryl Craig, March 2011
! Modifications: Pete Bogenschutz, March 2011 and onward
! Origin: Based heavily on UWM clubb_init.F90
! References:
!   None
!-------------------------------------------------------------------------------

!   

      USE physics_types, ONLY: physics_state, physics_ptend 


      USE camsrfexch, ONLY: cam_in_t 
      
      USE scammod, ONLY: single_column, scm_clubb_iop_name 
      USE clubb_api_module, ONLY: nparams, read_parameters_api, setup_parameters_api, setup_grid_heights_api, w_tol_sqd, rt_tol, &
      &thl_tol, l_stats, stats_tsamp, stats_tout, pdf_parameter, stats_begin_timestep_api, advance_clubb_core_api, &
      &calculate_thlp2_rad_api, update_xp2_mc_api, zt2zm_api, zm2zt_api 
   ! These are not exposed by the api module, but we want them anyway!


      USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
      USE kgen_utils_mod, ONLY: kgen_perturb_real 
      USE physics_types, ONLY: kr_physics_types_physics_state 
      USE physics_types, ONLY: kr_physics_types_physics_ptend 
      USE camsrfexch, ONLY: kr_camsrfexch_cam_in_t 
      USE clubb_api_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
      USE stats_variables, ONLY: kr_externs_out_stats_variables 
      USE parameters_tunable, ONLY: kr_externs_out_parameters_tunable 
      USE grid_class, ONLY: kr_externs_out_grid_class 
      USE error_code, ONLY: kr_externs_out_error_code 
      USE variables_diagnostic_module, ONLY: kr_externs_out_variables_diagnostic_module 
      USE saturation, ONLY: kr_externs_out_saturation 
      USE sponge_layer_damping, ONLY: kr_externs_out_sponge_layer_damping 
      USE physics_types, ONLY: kv_physics_types_physics_state 
      USE physics_types, ONLY: kv_physics_types_physics_ptend 
      USE camsrfexch, ONLY: kv_camsrfexch_cam_in_t 
      USE clubb_api_module, ONLY: kv_pdf_parameter_module_pdf_parameter 
      USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_init_verify, kgen_tolerance, kgen_minvalue, kgen_verboselevel, &
      &CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL 
      USE stats_variables, ONLY: kv_externs_stats_variables 
      USE parameters_tunable, ONLY: kv_externs_parameters_tunable 
      USE grid_class, ONLY: kv_externs_grid_class 
      USE variables_diagnostic_module, ONLY: kv_externs_variables_diagnostic_module 

      IMPLICIT NONE 
   ! --------------- !
   ! Input Auguments !
   ! --------------- !
   

      TYPE(cam_in_t), INTENT(INOUT) :: cam_in 
      REAL(KIND=r8), INTENT(INOUT) :: hdtime 
      INTEGER, INTENT(INOUT) :: cld_macmic_num_steps 
      INTEGER, INTENT(INOUT) :: macmic_it 
   ! ---------------------- !
   ! Input-Output Auguments !
   ! ---------------------- !
    
    
   ! ---------------------- !
   ! Output Auguments !
   ! ---------------------- !


   ! These two variables are needed for energy check    

   ! --------------- !
   ! Local Variables !
   ! --------------- !

        


      TYPE(physics_state) :: state1 
      TYPE(physics_ptend) :: ptend_loc 
   
      INTEGER :: i, k, t, ixind, nadv 
      INTEGER :: ixcldice, ixcldliq, ixq 
      INTEGER :: ncol 
      INTEGER :: err_code 
      INTEGER :: icnt, clubbtop 

   

      REAL(KIND=r8) :: dtime 
      REAL(KIND=r8) :: edsclr_in(pverp+1-top_lev,edsclr_dim) 
      REAL(KIND=r8) :: wp2_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: wp3_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: wpthlp_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: wprtp_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: rtpthlp_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: rtp2_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: rtp3_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: thlp2_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: thlp3_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: up2_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: vp2_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: upwp_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: vpwp_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: thlm_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: rtm_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: rvm_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: um_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: vm_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: rho_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: pre_in(pverp+1-top_lev) 
      REAL(KIND=r8) :: rtp2_mc_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: thlp2_mc_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: wprtp_mc_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: wpthlp_mc_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: rtpthlp_mc_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: rcm_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: rcm_out_zm(pverp+1-top_lev) 
      REAL(KIND=r8) :: wprcp_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: cloud_frac_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: rcm_in_layer_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: cloud_cover_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: thlprcp_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: rho_ds_zm(pverp+1-top_lev) 
      REAL(KIND=r8) :: rho_ds_zt(pverp+1-top_lev) 
      REAL(KIND=r8) :: invrs_rho_ds_zm(pverp+1-top_lev) 
      REAL(KIND=r8) :: invrs_rho_ds_zt(pverp+1-top_lev) 
      REAL(KIND=r8) :: thv_ds_zm(pverp+1-top_lev) 
      REAL(KIND=r8) :: thv_ds_zt(pverp+1-top_lev) 
      REAL(KIND=r8) :: rfrzm(pverp+1-top_lev) 
      REAL(KIND=r8) :: radf(pverp+1-top_lev) 
      REAL(KIND=r8) :: wprtp_forcing(pverp+1-top_lev) 
      REAL(KIND=r8) :: wpthlp_forcing(pverp+1-top_lev) 
      REAL(KIND=r8) :: rtp2_forcing(pverp+1-top_lev) 
      REAL(KIND=r8) :: thlp2_forcing(pverp+1-top_lev) 
      REAL(KIND=r8) :: rtpthlp_forcing(pverp+1-top_lev) 
      REAL(KIND=r8) :: ice_supersat_frac(pverp+1-top_lev) 
      REAL(KIND=r8) :: zt_g(pverp+1-top_lev) 
      REAL(KIND=r8) :: zi_g(pverp+1-top_lev) 
      REAL(KIND=r8) :: zt_out(pcols,pverp) 
      REAL(KIND=r8) :: zi_out(pcols,pverp) 
      REAL(KIND=r8) :: fcor 
      REAL(KIND=r8) :: sfc_elevation 
      REAL(KIND=r8) :: ubar 
      REAL(KIND=r8) :: ustar 
      REAL(KIND=r8) :: thlm_forcing(pverp+1-top_lev) 
      REAL(KIND=r8) :: rtm_forcing(pverp+1-top_lev) 
      REAL(KIND=r8) :: um_forcing(pverp+1-top_lev) 
      REAL(KIND=r8) :: vm_forcing(pverp+1-top_lev) 
      REAL(KIND=r8) :: wm_zm(pverp+1-top_lev) 
      REAL(KIND=r8) :: wm_zt(pverp+1-top_lev) 
      REAL(KIND=r8) :: p_in_pa(pverp+1-top_lev) 
      REAL(KIND=r8) :: rho_zt(pverp+1-top_lev) 
      REAL(KIND=r8) :: rho_zm(pverp+1-top_lev) 
      REAL(KIND=r8) :: exner(pverp+1-top_lev) 
      REAL(KIND=r8) :: wpthlp_sfc 
      REAL(KIND=r8) :: wprtp_sfc 
      REAL(KIND=r8) :: upwp_sfc 
      REAL(KIND=r8) :: vpwp_sfc 
      REAL(KIND=r8) :: sclrm_forcing(pverp+1-top_lev,sclr_dim) 
      REAL(KIND=r8) :: wpsclrp_sfc(sclr_dim) 
      REAL(KIND=r8) :: edsclrm_forcing(pverp+1-top_lev,edsclr_dim) 
      REAL(KIND=r8) :: wpedsclrp_sfc(edsclr_dim) 
      REAL(KIND=r8) :: sclrm(pverp+1-top_lev,sclr_dim) 
      REAL(KIND=r8) :: wpsclrp(pverp+1-top_lev,sclr_dim) 
      REAL(KIND=r8) :: sclrp2(pverp+1-top_lev,sclr_dim) 
      REAL(KIND=r8) :: sclrprtp(pverp+1-top_lev,sclr_dim) 
      REAL(KIND=r8) :: sclrpthlp(pverp+1-top_lev,sclr_dim) 
      REAL(KIND=r8) :: hydromet(pverp+1-top_lev,hydromet_dim) 
      REAL(KIND=r8) :: wphydrometp(pverp+1-top_lev,hydromet_dim) 
      REAL(KIND=r8) :: wp2hmp(pverp+1-top_lev,hydromet_dim) 
      REAL(KIND=r8) :: rtphmp_zt(pverp+1-top_lev,hydromet_dim) 
      REAL(KIND=r8) :: thlphmp_zt (pverp+1-top_lev,hydromet_dim) 
      REAL(KIND=r8) :: bflx22 
      REAL(KIND=r8) :: khzm_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: khzt_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: qclvar_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: qclvar(pcols,pverp) 
      REAL(KIND=r8) :: zo 
      REAL(KIND=r8) :: dz_g(pver) 
      REAL(KIND=r8) :: se_upper_a, se_upper_b, se_upper_diss 
      REAL(KIND=r8) :: tw_upper_a, tw_upper_b, tw_upper_diss 
      REAL(KIND=r8) :: grid_dx(pcols), grid_dy(pcols) 
      REAL(KIND=r8) :: host_dx, host_dy 
   ! Variables below are needed to compute energy integrals for conservation
   
      REAL(KIND=r8) :: ke_a(pcols), ke_b(pcols), te_a(pcols), te_b(pcols) 
      REAL(KIND=r8) :: wv_a(pcols), wv_b(pcols), wl_b(pcols), wl_a(pcols) 
      REAL(KIND=r8) :: se_dis, se_a(pcols), se_b(pcols), clubb_s(pver) 

      REAL(KIND=r8) :: exner_clubb(pcols,pverp) 
      REAL(KIND=r8) :: thv(pcols,pver) 
      REAL(KIND=r8) :: edsclr_out(pverp,edsclr_dim) 
      REAL(KIND=r8) :: rcm(pcols,pverp) 
      REAL(KIND=r8) :: cloud_frac(pcols,pverp) 
      REAL(KIND=r8) :: rcm_in_layer(pcols,pverp) 
      REAL(KIND=r8) :: wprcp(pcols,pverp) 
      REAL(KIND=r8) :: rvm(pcols,pverp) 
      REAL(KIND=r8) :: dum1 
      REAL(KIND=r8) :: qrl_clubb(pverp+1-top_lev) 
      REAL(KIND=r8) :: qrl_zm(pverp+1-top_lev) 
      REAL(KIND=r8) :: thlp2_rad_out(pverp+1-top_lev) 
      REAL(KIND=r8) :: rtm_test 

      INTEGER :: time_elapsed 
      REAL(KIND=r8), dimension(nparams) :: clubb_params 
      TYPE(pdf_parameter), dimension(pverp) :: pdf_params 
   ! --------------- !
   ! Pointers        !
   ! --------------- !


      REAL(KIND=r8), pointer, dimension(:,:) :: wp2 
      REAL(KIND=r8), pointer, dimension(:,:) :: wp3 
      REAL(KIND=r8), pointer, dimension(:,:) :: wpthlp 
      REAL(KIND=r8), pointer, dimension(:,:) :: wprtp 
      REAL(KIND=r8), pointer, dimension(:,:) :: rtpthlp 
      REAL(KIND=r8), pointer, dimension(:,:) :: rtp2 
      REAL(KIND=r8), pointer, dimension(:,:) :: thlp2 
      REAL(KIND=r8), pointer, dimension(:,:) :: up2 
      REAL(KIND=r8), pointer, dimension(:,:) :: vp2 

      REAL(KIND=r8), pointer, dimension(:,:) :: upwp 
      REAL(KIND=r8), pointer, dimension(:,:) :: vpwp 
      REAL(KIND=r8), pointer, dimension(:,:) :: thlm 
      REAL(KIND=r8), pointer, dimension(:,:) :: rtm 
      REAL(KIND=r8), pointer, dimension(:,:) :: um 
      REAL(KIND=r8), pointer, dimension(:,:) :: vm 
      REAL(KIND=r8), pointer, dimension(:,:) :: khzm 

      REAL(KIND=r8), pointer, dimension(:,:) :: prer_evap 
      REAL(KIND=r8), pointer, dimension(:,:) :: qrl 
      REAL(KIND=r8), pointer, dimension(:,:) :: radf_clubb 
  ! ZM microphysics


      INTEGER :: troplev(pcols) 
      LOGICAL :: apply_to_surface 


      INTEGER :: nlev 


      INTEGER, INTENT(IN) :: kgen_unit 
      REAL(KIND=kgen_dp), INTENT(OUT) :: kgen_measure 
      LOGICAL, INTENT(OUT) :: kgen_isverified 
      CHARACTER(LEN=*), INTENT(IN) :: kgen_filepath 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: kgen_intvar, kgen_ierr 
      INTEGER :: kgen_mpirank, kgen_openmptid, kgen_kernelinvoke 
      LOGICAL :: kgen_evalstage, kgen_warmupstage, kgen_mainstage 
      COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
      INTEGER, PARAMETER :: KGEN_MAXITER = 10
      INTEGER, PARAMETER :: CACHE_CONTROL_LENGTH=1024*1024 
      INTEGER, DIMENSION(CACHE_CONTROL_LENGTH) :: kgen_cache_control 
        
      TYPE(check_t) :: check_status 
      INTEGER*8 :: kgen_start_clock, kgen_stop_clock, kgen_rate_clock 
      REAL(KIND=kgen_dp) :: gkgen_measure 
      TYPE(physics_ptend) :: kgenref_ptend_loc 
      INTEGER :: kgenref_i 
      INTEGER :: kgenref_k 
      INTEGER :: kgenref_ixind 
      INTEGER :: kgenref_t 
      INTEGER :: kgenref_err_code 
      INTEGER :: kgenref_icnt 
      INTEGER :: kgenref_clubbtop 
      REAL(KIND=r8), dimension(pverp+1-top_lev,edsclr_dim) :: kgenref_edsclr_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_wp2_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_wp3_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_wpthlp_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_wprtp_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rtpthlp_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rtp2_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rtp3_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_thlp2_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_thlp3_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_up2_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_vp2_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_upwp_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_vpwp_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_thlm_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rtm_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rvm_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_um_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_vm_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rho_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_pre_in 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rtp2_mc_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_thlp2_mc_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_wprtp_mc_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_wpthlp_mc_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rtpthlp_mc_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rcm_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rcm_out_zm 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_wprcp_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_cloud_frac_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rcm_in_layer_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_cloud_cover_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_thlprcp_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rho_ds_zm 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rho_ds_zt 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_invrs_rho_ds_zm 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_invrs_rho_ds_zt 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_thv_ds_zm 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_thv_ds_zt 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rfrzm 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_radf 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_wprtp_forcing 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_wpthlp_forcing 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rtp2_forcing 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_thlp2_forcing 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rtpthlp_forcing 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_ice_supersat_frac 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_zt_g 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_zi_g 
      REAL(KIND=r8), dimension(pcols,pverp) :: kgenref_zt_out 
      REAL(KIND=r8), dimension(pcols,pverp) :: kgenref_zi_out 
      REAL(KIND=r8) :: kgenref_fcor 
      REAL(KIND=r8) :: kgenref_sfc_elevation 
      REAL(KIND=r8) :: kgenref_ubar 
      REAL(KIND=r8) :: kgenref_ustar 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_thlm_forcing 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rtm_forcing 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_um_forcing 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_vm_forcing 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_wm_zm 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_wm_zt 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_p_in_pa 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_rho_zm 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_exner 
      REAL(KIND=r8) :: kgenref_wpthlp_sfc 
      REAL(KIND=r8) :: kgenref_wprtp_sfc 
      REAL(KIND=r8) :: kgenref_upwp_sfc 
      REAL(KIND=r8) :: kgenref_vpwp_sfc 
      REAL(KIND=r8), dimension(edsclr_dim) :: kgenref_wpedsclrp_sfc 
      REAL(KIND=r8), dimension(pverp+1-top_lev,sclr_dim) :: kgenref_sclrm 
      REAL(KIND=r8), dimension(pverp+1-top_lev,sclr_dim) :: kgenref_wpsclrp 
      REAL(KIND=r8), dimension(pverp+1-top_lev,sclr_dim) :: kgenref_sclrp2 
      REAL(KIND=r8), dimension(pverp+1-top_lev,sclr_dim) :: kgenref_sclrprtp 
      REAL(KIND=r8), dimension(pverp+1-top_lev,sclr_dim) :: kgenref_sclrpthlp 
      REAL(KIND=r8), dimension(pverp+1-top_lev,hydromet_dim) :: kgenref_hydromet 
      REAL(KIND=r8), dimension(pverp+1-top_lev,hydromet_dim) :: kgenref_wphydrometp 
      REAL(KIND=r8), dimension(pverp+1-top_lev,hydromet_dim) :: kgenref_wp2hmp 
      REAL(KIND=r8), dimension(pverp+1-top_lev,hydromet_dim) :: kgenref_rtphmp_zt 
      REAL(KIND=r8), dimension(pverp+1-top_lev,hydromet_dim) :: kgenref_thlphmp_zt 
      REAL(KIND=r8) :: kgenref_bflx22 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_khzm_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_khzt_out 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_qclvar_out 
      REAL(KIND=r8), dimension(pcols,pverp) :: kgenref_qclvar 
      REAL(KIND=r8) :: kgenref_zo 
      REAL(KIND=r8), dimension(pver) :: kgenref_dz_g 
      REAL(KIND=r8) :: kgenref_se_upper_a 
      REAL(KIND=r8) :: kgenref_se_upper_b 
      REAL(KIND=r8) :: kgenref_se_upper_diss 
      REAL(KIND=r8) :: kgenref_tw_upper_a 
      REAL(KIND=r8) :: kgenref_tw_upper_b 
      REAL(KIND=r8) :: kgenref_tw_upper_diss 
      REAL(KIND=r8) :: kgenref_host_dx 
      REAL(KIND=r8) :: kgenref_host_dy 
      REAL(KIND=r8), dimension(pcols) :: kgenref_ke_a 
      REAL(KIND=r8), dimension(pcols) :: kgenref_ke_b 
      REAL(KIND=r8), dimension(pcols) :: kgenref_te_a 
      REAL(KIND=r8), dimension(pcols) :: kgenref_te_b 
      REAL(KIND=r8), dimension(pcols) :: kgenref_wv_a 
      REAL(KIND=r8), dimension(pcols) :: kgenref_wl_a 
      REAL(KIND=r8), dimension(pcols) :: kgenref_wv_b 
      REAL(KIND=r8), dimension(pcols) :: kgenref_wl_b 
      REAL(KIND=r8), dimension(pver) :: kgenref_clubb_s 
      REAL(KIND=r8), dimension(pcols) :: kgenref_se_a 
      REAL(KIND=r8), dimension(pcols) :: kgenref_se_b 
      REAL(KIND=r8) :: kgenref_se_dis 
      REAL(KIND=r8), dimension(pverp,edsclr_dim) :: kgenref_edsclr_out 
      REAL(KIND=r8), dimension(pcols,pverp) :: kgenref_rcm 
      REAL(KIND=r8), dimension(pcols,pverp) :: kgenref_cloud_frac 
      REAL(KIND=r8), dimension(pcols,pverp) :: kgenref_rcm_in_layer 
      REAL(KIND=r8), dimension(pcols,pverp) :: kgenref_wprcp 
      REAL(KIND=r8) :: kgenref_dum1 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_qrl_clubb 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_qrl_zm 
      REAL(KIND=r8), dimension(pverp+1-top_lev) :: kgenref_thlp2_rad_out 
      REAL(KIND=r8) :: kgenref_rtm_test 
      INTEGER :: kgenref_time_elapsed 
      REAL(KIND=r8), dimension(nparams) :: kgenref_clubb_params 
      TYPE(pdf_parameter), dimension(pverp) :: kgenref_pdf_params 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_wp2 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_wp3 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_wpthlp 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_wprtp 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_rtpthlp 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_rtp2 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_thlp2 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_up2 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_vp2 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_upwp 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_vpwp 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_thlm 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_rtm 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_um 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_vm 
      REAL(KIND=r8), pointer, dimension(:,:) :: kgenref_khzm 
      LOGICAL :: kgenref_apply_to_surface 
        
      !parent block preprocessing 
        
#ifdef _MPI 
      call mpi_comm_rank(mpi_comm_world, kgen_mpirank, kgen_ierr) 
#else 
      kgen_mpirank = 0 
#endif 
        
        
      !local input variables 
      CALL kr_physics_types_physics_state(state1, kgen_unit, "state1", .FALSE.) 
      CALL kr_physics_types_physics_ptend(ptend_loc, kgen_unit, "ptend_loc", .FALSE.) 
      READ (UNIT = kgen_unit) k 
      READ (UNIT = kgen_unit) i 
      READ (UNIT = kgen_unit) ixind 
      READ (UNIT = kgen_unit) nadv 
      READ (UNIT = kgen_unit) t 
      READ (UNIT = kgen_unit) ixcldice 
      READ (UNIT = kgen_unit) ixq 
      READ (UNIT = kgen_unit) ixcldliq 
      READ (UNIT = kgen_unit) ncol 
      READ (UNIT = kgen_unit) err_code 
      READ (UNIT = kgen_unit) icnt 
      READ (UNIT = kgen_unit) clubbtop 
      READ (UNIT = kgen_unit) dtime 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) edsclr_in 
          CALL kgen_array_sumcheck("edsclr_in", kgen_array_sum, DBLE(SUM(edsclr_in, mask=(edsclr_in .eq. edsclr_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wp2_in 
          CALL kgen_array_sumcheck("wp2_in", kgen_array_sum, DBLE(SUM(wp2_in, mask=(wp2_in .eq. wp2_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wp3_in 
          CALL kgen_array_sumcheck("wp3_in", kgen_array_sum, DBLE(SUM(wp3_in, mask=(wp3_in .eq. wp3_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wpthlp_in 
          CALL kgen_array_sumcheck("wpthlp_in", kgen_array_sum, DBLE(SUM(wpthlp_in, mask=(wpthlp_in .eq. wpthlp_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wprtp_in 
          CALL kgen_array_sumcheck("wprtp_in", kgen_array_sum, DBLE(SUM(wprtp_in, mask=(wprtp_in .eq. wprtp_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rtpthlp_in 
          CALL kgen_array_sumcheck("rtpthlp_in", kgen_array_sum, DBLE(SUM(rtpthlp_in, mask=(rtpthlp_in .eq. rtpthlp_in))), &
          &.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rtp2_in 
          CALL kgen_array_sumcheck("rtp2_in", kgen_array_sum, DBLE(SUM(rtp2_in, mask=(rtp2_in .eq. rtp2_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rtp3_in 
          CALL kgen_array_sumcheck("rtp3_in", kgen_array_sum, DBLE(SUM(rtp3_in, mask=(rtp3_in .eq. rtp3_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) thlp2_in 
          CALL kgen_array_sumcheck("thlp2_in", kgen_array_sum, DBLE(SUM(thlp2_in, mask=(thlp2_in .eq. thlp2_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) thlp3_in 
          CALL kgen_array_sumcheck("thlp3_in", kgen_array_sum, DBLE(SUM(thlp3_in, mask=(thlp3_in .eq. thlp3_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) up2_in 
          CALL kgen_array_sumcheck("up2_in", kgen_array_sum, DBLE(SUM(up2_in, mask=(up2_in .eq. up2_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) vp2_in 
          CALL kgen_array_sumcheck("vp2_in", kgen_array_sum, DBLE(SUM(vp2_in, mask=(vp2_in .eq. vp2_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) upwp_in 
          CALL kgen_array_sumcheck("upwp_in", kgen_array_sum, DBLE(SUM(upwp_in, mask=(upwp_in .eq. upwp_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) vpwp_in 
          CALL kgen_array_sumcheck("vpwp_in", kgen_array_sum, DBLE(SUM(vpwp_in, mask=(vpwp_in .eq. vpwp_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) thlm_in 
          CALL kgen_array_sumcheck("thlm_in", kgen_array_sum, DBLE(SUM(thlm_in, mask=(thlm_in .eq. thlm_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rtm_in 
          CALL kgen_array_sumcheck("rtm_in", kgen_array_sum, DBLE(SUM(rtm_in, mask=(rtm_in .eq. rtm_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rvm_in 
          CALL kgen_array_sumcheck("rvm_in", kgen_array_sum, DBLE(SUM(rvm_in, mask=(rvm_in .eq. rvm_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) um_in 
          CALL kgen_array_sumcheck("um_in", kgen_array_sum, DBLE(SUM(um_in, mask=(um_in .eq. um_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) vm_in 
          CALL kgen_array_sumcheck("vm_in", kgen_array_sum, DBLE(SUM(vm_in, mask=(vm_in .eq. vm_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rho_in 
          CALL kgen_array_sumcheck("rho_in", kgen_array_sum, DBLE(SUM(rho_in, mask=(rho_in .eq. rho_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) pre_in 
          CALL kgen_array_sumcheck("pre_in", kgen_array_sum, DBLE(SUM(pre_in, mask=(pre_in .eq. pre_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rtp2_mc_out 
          CALL kgen_array_sumcheck("rtp2_mc_out", kgen_array_sum, DBLE(SUM(rtp2_mc_out, mask=(rtp2_mc_out .eq. rtp2_mc_out))), &
          &.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) thlp2_mc_out 
          CALL kgen_array_sumcheck("thlp2_mc_out", kgen_array_sum, DBLE(SUM(thlp2_mc_out, mask=(thlp2_mc_out .eq. &
          &thlp2_mc_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wprtp_mc_out 
          CALL kgen_array_sumcheck("wprtp_mc_out", kgen_array_sum, DBLE(SUM(wprtp_mc_out, mask=(wprtp_mc_out .eq. &
          &wprtp_mc_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wpthlp_mc_out 
          CALL kgen_array_sumcheck("wpthlp_mc_out", kgen_array_sum, DBLE(SUM(wpthlp_mc_out, mask=(wpthlp_mc_out .eq. &
          &wpthlp_mc_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rtpthlp_mc_out 
          CALL kgen_array_sumcheck("rtpthlp_mc_out", kgen_array_sum, DBLE(SUM(rtpthlp_mc_out, mask=(rtpthlp_mc_out .eq. &
          &rtpthlp_mc_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rcm_out 
          CALL kgen_array_sumcheck("rcm_out", kgen_array_sum, DBLE(SUM(rcm_out, mask=(rcm_out .eq. rcm_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rcm_out_zm 
          CALL kgen_array_sumcheck("rcm_out_zm", kgen_array_sum, DBLE(SUM(rcm_out_zm, mask=(rcm_out_zm .eq. rcm_out_zm))), &
          &.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wprcp_out 
          CALL kgen_array_sumcheck("wprcp_out", kgen_array_sum, DBLE(SUM(wprcp_out, mask=(wprcp_out .eq. wprcp_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) cloud_frac_out 
          CALL kgen_array_sumcheck("cloud_frac_out", kgen_array_sum, DBLE(SUM(cloud_frac_out, mask=(cloud_frac_out .eq. &
          &cloud_frac_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rcm_in_layer_out 
          CALL kgen_array_sumcheck("rcm_in_layer_out", kgen_array_sum, DBLE(SUM(rcm_in_layer_out, mask=(rcm_in_layer_out .eq. &
          &rcm_in_layer_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) cloud_cover_out 
          CALL kgen_array_sumcheck("cloud_cover_out", kgen_array_sum, DBLE(SUM(cloud_cover_out, mask=(cloud_cover_out .eq. &
          &cloud_cover_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) thlprcp_out 
          CALL kgen_array_sumcheck("thlprcp_out", kgen_array_sum, DBLE(SUM(thlprcp_out, mask=(thlprcp_out .eq. thlprcp_out))), &
          &.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rho_ds_zm 
          CALL kgen_array_sumcheck("rho_ds_zm", kgen_array_sum, DBLE(SUM(rho_ds_zm, mask=(rho_ds_zm .eq. rho_ds_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rho_ds_zt 
          CALL kgen_array_sumcheck("rho_ds_zt", kgen_array_sum, DBLE(SUM(rho_ds_zt, mask=(rho_ds_zt .eq. rho_ds_zt))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) invrs_rho_ds_zm 
          CALL kgen_array_sumcheck("invrs_rho_ds_zm", kgen_array_sum, DBLE(SUM(invrs_rho_ds_zm, mask=(invrs_rho_ds_zm .eq. &
          &invrs_rho_ds_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) invrs_rho_ds_zt 
          CALL kgen_array_sumcheck("invrs_rho_ds_zt", kgen_array_sum, DBLE(SUM(invrs_rho_ds_zt, mask=(invrs_rho_ds_zt .eq. &
          &invrs_rho_ds_zt))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) thv_ds_zm 
          CALL kgen_array_sumcheck("thv_ds_zm", kgen_array_sum, DBLE(SUM(thv_ds_zm, mask=(thv_ds_zm .eq. thv_ds_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) thv_ds_zt 
          CALL kgen_array_sumcheck("thv_ds_zt", kgen_array_sum, DBLE(SUM(thv_ds_zt, mask=(thv_ds_zt .eq. thv_ds_zt))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rfrzm 
          CALL kgen_array_sumcheck("rfrzm", kgen_array_sum, DBLE(SUM(rfrzm, mask=(rfrzm .eq. rfrzm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) radf 

          !CALL kgen_array_sumcheck("radf", kgen_array_sum, DBLE(SUM(radf, mask=(radf .eq. radf))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wprtp_forcing 
          !CALL kgen_array_sumcheck("wprtp_forcing", kgen_array_sum, DBLE(SUM(wprtp_forcing, mask=(wprtp_forcing .eq. &
          !&wprtp_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wpthlp_forcing 
          CALL kgen_array_sumcheck("wpthlp_forcing", kgen_array_sum, DBLE(SUM(wpthlp_forcing, mask=(wpthlp_forcing .eq. &
          &wpthlp_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rtp2_forcing 
          CALL kgen_array_sumcheck("rtp2_forcing", kgen_array_sum, DBLE(SUM(rtp2_forcing, mask=(rtp2_forcing .eq. &
          &rtp2_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) thlp2_forcing 
          CALL kgen_array_sumcheck("thlp2_forcing", kgen_array_sum, DBLE(SUM(thlp2_forcing, mask=(thlp2_forcing .eq. &
          &thlp2_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rtpthlp_forcing 
          CALL kgen_array_sumcheck("rtpthlp_forcing", kgen_array_sum, DBLE(SUM(rtpthlp_forcing, mask=(rtpthlp_forcing .eq. &
          &rtpthlp_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) ice_supersat_frac 
          CALL kgen_array_sumcheck("ice_supersat_frac", kgen_array_sum, DBLE(SUM(ice_supersat_frac, mask=(ice_supersat_frac .eq. &
          &ice_supersat_frac))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) zt_g 
          CALL kgen_array_sumcheck("zt_g", kgen_array_sum, DBLE(SUM(zt_g, mask=(zt_g .eq. zt_g))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) zi_g 
          CALL kgen_array_sumcheck("zi_g", kgen_array_sum, DBLE(SUM(zi_g, mask=(zi_g .eq. zi_g))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) zt_out 
          CALL kgen_array_sumcheck("zt_out", kgen_array_sum, DBLE(SUM(zt_out, mask=(zt_out .eq. zt_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) zi_out 
          CALL kgen_array_sumcheck("zi_out", kgen_array_sum, DBLE(SUM(zi_out, mask=(zi_out .eq. zi_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) fcor 
      READ (UNIT = kgen_unit) sfc_elevation 
      READ (UNIT = kgen_unit) ubar 
      READ (UNIT = kgen_unit) ustar 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) thlm_forcing 
          CALL kgen_array_sumcheck("thlm_forcing", kgen_array_sum, DBLE(SUM(thlm_forcing, mask=(thlm_forcing .eq. &
          &thlm_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rtm_forcing 
          CALL kgen_array_sumcheck("rtm_forcing", kgen_array_sum, DBLE(SUM(rtm_forcing, mask=(rtm_forcing .eq. rtm_forcing))), &
          &.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) um_forcing 
          CALL kgen_array_sumcheck("um_forcing", kgen_array_sum, DBLE(SUM(um_forcing, mask=(um_forcing .eq. um_forcing))), &
          &.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) vm_forcing 
          CALL kgen_array_sumcheck("vm_forcing", kgen_array_sum, DBLE(SUM(vm_forcing, mask=(vm_forcing .eq. vm_forcing))), &
          &.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wm_zm 
          CALL kgen_array_sumcheck("wm_zm", kgen_array_sum, DBLE(SUM(wm_zm, mask=(wm_zm .eq. wm_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wm_zt 
          CALL kgen_array_sumcheck("wm_zt", kgen_array_sum, DBLE(SUM(wm_zt, mask=(wm_zt .eq. wm_zt))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) p_in_pa 
          CALL kgen_array_sumcheck("p_in_pa", kgen_array_sum, DBLE(SUM(p_in_pa, mask=(p_in_pa .eq. p_in_pa))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rho_zt 
          CALL kgen_array_sumcheck("rho_zt", kgen_array_sum, DBLE(SUM(rho_zt, mask=(rho_zt .eq. rho_zt))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rho_zm 
          CALL kgen_array_sumcheck("rho_zm", kgen_array_sum, DBLE(SUM(rho_zm, mask=(rho_zm .eq. rho_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) exner 
          CALL kgen_array_sumcheck("exner", kgen_array_sum, DBLE(SUM(exner, mask=(exner .eq. exner))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) wpthlp_sfc 
      READ (UNIT = kgen_unit) wprtp_sfc 
      READ (UNIT = kgen_unit) upwp_sfc 
      READ (UNIT = kgen_unit) vpwp_sfc 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) edsclrm_forcing 
          CALL kgen_array_sumcheck("edsclrm_forcing", kgen_array_sum, DBLE(SUM(edsclrm_forcing, mask=(edsclrm_forcing .eq. &
          &edsclrm_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wpedsclrp_sfc 
          !CALL kgen_array_sumcheck("wpedsclrp_sfc", kgen_array_sum, DBLE(SUM(wpedsclrp_sfc, mask=(wpedsclrp_sfc .eq. &
          !&wpedsclrp_sfc))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) bflx22 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) khzm_out 
          CALL kgen_array_sumcheck("khzm_out", kgen_array_sum, DBLE(SUM(khzm_out, mask=(khzm_out .eq. khzm_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) khzt_out 
          CALL kgen_array_sumcheck("khzt_out", kgen_array_sum, DBLE(SUM(khzt_out, mask=(khzt_out .eq. khzt_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) qclvar_out 
          !CALL kgen_array_sumcheck("qclvar_out", kgen_array_sum, DBLE(SUM(qclvar_out, mask=(qclvar_out .eq. qclvar_out))), &
          !&.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) qclvar 
          CALL kgen_array_sumcheck("qclvar", kgen_array_sum, DBLE(SUM(qclvar, mask=(qclvar .eq. qclvar))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) zo 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) dz_g 
          CALL kgen_array_sumcheck("dz_g", kgen_array_sum, DBLE(SUM(dz_g, mask=(dz_g .eq. dz_g))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) se_upper_a 
      READ (UNIT = kgen_unit) se_upper_b 
      READ (UNIT = kgen_unit) se_upper_diss 
      READ (UNIT = kgen_unit) tw_upper_a 
      READ (UNIT = kgen_unit) tw_upper_b 
      READ (UNIT = kgen_unit) tw_upper_diss 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) grid_dx 
          CALL kgen_array_sumcheck("grid_dx", kgen_array_sum, DBLE(SUM(grid_dx, mask=(grid_dx .eq. grid_dx))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) grid_dy 
          CALL kgen_array_sumcheck("grid_dy", kgen_array_sum, DBLE(SUM(grid_dy, mask=(grid_dy .eq. grid_dy))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) host_dx 
      READ (UNIT = kgen_unit) host_dy 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) ke_a 
          CALL kgen_array_sumcheck("ke_a", kgen_array_sum, DBLE(SUM(ke_a, mask=(ke_a .eq. ke_a))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) ke_b 
          CALL kgen_array_sumcheck("ke_b", kgen_array_sum, DBLE(SUM(ke_b, mask=(ke_b .eq. ke_b))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) te_b 
          CALL kgen_array_sumcheck("te_b", kgen_array_sum, DBLE(SUM(te_b, mask=(te_b .eq. te_b))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) te_a 
          CALL kgen_array_sumcheck("te_a", kgen_array_sum, DBLE(SUM(te_a, mask=(te_a .eq. te_a))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wv_a 
          CALL kgen_array_sumcheck("wv_a", kgen_array_sum, DBLE(SUM(wv_a, mask=(wv_a .eq. wv_a))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wl_a 
          CALL kgen_array_sumcheck("wl_a", kgen_array_sum, DBLE(SUM(wl_a, mask=(wl_a .eq. wl_a))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wv_b 
          CALL kgen_array_sumcheck("wv_b", kgen_array_sum, DBLE(SUM(wv_b, mask=(wv_b .eq. wv_b))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wl_b 
          CALL kgen_array_sumcheck("wl_b", kgen_array_sum, DBLE(SUM(wl_b, mask=(wl_b .eq. wl_b))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) clubb_s 
          CALL kgen_array_sumcheck("clubb_s", kgen_array_sum, DBLE(SUM(clubb_s, mask=(clubb_s .eq. clubb_s))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) se_a 
          CALL kgen_array_sumcheck("se_a", kgen_array_sum, DBLE(SUM(se_a, mask=(se_a .eq. se_a))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) se_b 
          CALL kgen_array_sumcheck("se_b", kgen_array_sum, DBLE(SUM(se_b, mask=(se_b .eq. se_b))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) se_dis 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) exner_clubb 
          CALL kgen_array_sumcheck("exner_clubb", kgen_array_sum, DBLE(SUM(exner_clubb, mask=(exner_clubb .eq. exner_clubb))), &
          &.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) thv 
          CALL kgen_array_sumcheck("thv", kgen_array_sum, DBLE(SUM(thv, mask=(thv .eq. thv))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) edsclr_out 
          CALL kgen_array_sumcheck("edsclr_out", kgen_array_sum, DBLE(SUM(edsclr_out, mask=(edsclr_out .eq. edsclr_out))), &
          &.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rcm 
          CALL kgen_array_sumcheck("rcm", kgen_array_sum, DBLE(SUM(rcm, mask=(rcm .eq. rcm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) cloud_frac 
          CALL kgen_array_sumcheck("cloud_frac", kgen_array_sum, DBLE(SUM(cloud_frac, mask=(cloud_frac .eq. cloud_frac))), &
          &.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rcm_in_layer 
          CALL kgen_array_sumcheck("rcm_in_layer", kgen_array_sum, DBLE(SUM(rcm_in_layer, mask=(rcm_in_layer .eq. &
          &rcm_in_layer))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) wprcp 
          CALL kgen_array_sumcheck("wprcp", kgen_array_sum, DBLE(SUM(wprcp, mask=(wprcp .eq. wprcp))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rvm 
          CALL kgen_array_sumcheck("rvm", kgen_array_sum, DBLE(SUM(rvm, mask=(rvm .eq. rvm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) dum1 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) qrl_clubb 
          !CALL kgen_array_sumcheck("qrl_clubb", kgen_array_sum, DBLE(SUM(qrl_clubb, mask=(qrl_clubb .eq. qrl_clubb))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) qrl_zm 
          !CALL kgen_array_sumcheck("qrl_zm", kgen_array_sum, DBLE(SUM(qrl_zm, mask=(qrl_zm .eq. qrl_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) thlp2_rad_out 
          !CALL kgen_array_sumcheck("thlp2_rad_out", kgen_array_sum, DBLE(SUM(thlp2_rad_out, mask=(thlp2_rad_out .eq. &
          !&thlp2_rad_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) rtm_test 
      READ (UNIT = kgen_unit) time_elapsed 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) clubb_params 
          CALL kgen_array_sumcheck("clubb_params", kgen_array_sum, DBLE(SUM(clubb_params, mask=(clubb_params .eq. &
          &clubb_params))), .TRUE.) 
      END IF   
      CALL kr_kgen_clubb_tend_cam_subp2(pdf_params, kgen_unit, "pdf_params", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(wp2, kgen_unit, "wp2", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(wp3, kgen_unit, "wp3", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(wpthlp, kgen_unit, "wpthlp", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(wprtp, kgen_unit, "wprtp", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(rtpthlp, kgen_unit, "rtpthlp", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(rtp2, kgen_unit, "rtp2", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(thlp2, kgen_unit, "thlp2", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(up2, kgen_unit, "up2", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(vp2, kgen_unit, "vp2", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(upwp, kgen_unit, "upwp", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(vpwp, kgen_unit, "vpwp", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(thlm, kgen_unit, "thlm", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(rtm, kgen_unit, "rtm", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(um, kgen_unit, "um", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(vm, kgen_unit, "vm", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(khzm, kgen_unit, "khzm", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(prer_evap, kgen_unit, "prer_evap", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(qrl, kgen_unit, "qrl", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(radf_clubb, kgen_unit, "radf_clubb", .FALSE.) 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) troplev 
          CALL kgen_array_sumcheck("troplev", kgen_array_sum, DBLE(SUM(troplev, mask=(troplev .eq. troplev))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) apply_to_surface 
      READ (UNIT = kgen_unit) nlev 
        
      !extern output variables 
      CALL kr_externs_out_clubb_intr(kgen_unit) 
      CALL kr_externs_out_stats_variables(kgen_unit) 
      CALL kr_externs_out_parameters_tunable(kgen_unit) 
      CALL kr_externs_out_grid_class(kgen_unit) 
      CALL kr_externs_out_error_code(kgen_unit) 
      CALL kr_externs_out_variables_diagnostic_module(kgen_unit) 
      CALL kr_externs_out_saturation(kgen_unit) 
      CALL kr_externs_out_sponge_layer_damping(kgen_unit) 
        
      !local output variables 
      CALL kr_physics_types_physics_ptend(kgenref_ptend_loc, kgen_unit, "kgenref_ptend_loc", .FALSE.) 
      READ (UNIT = kgen_unit) kgenref_i 
      READ (UNIT = kgen_unit) kgenref_k 
      READ (UNIT = kgen_unit) kgenref_ixind 
      READ (UNIT = kgen_unit) kgenref_t 
      READ (UNIT = kgen_unit) kgenref_err_code 
      READ (UNIT = kgen_unit) kgenref_icnt 
      READ (UNIT = kgen_unit) kgenref_clubbtop 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_edsclr_in 
          CALL kgen_array_sumcheck("kgenref_edsclr_in", kgen_array_sum, DBLE(SUM(kgenref_edsclr_in, mask=(kgenref_edsclr_in .eq. &
          &kgenref_edsclr_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wp2_in 
          CALL kgen_array_sumcheck("kgenref_wp2_in", kgen_array_sum, DBLE(SUM(kgenref_wp2_in, mask=(kgenref_wp2_in .eq. &
          &kgenref_wp2_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wp3_in 
          CALL kgen_array_sumcheck("kgenref_wp3_in", kgen_array_sum, DBLE(SUM(kgenref_wp3_in, mask=(kgenref_wp3_in .eq. &
          &kgenref_wp3_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wpthlp_in 
          CALL kgen_array_sumcheck("kgenref_wpthlp_in", kgen_array_sum, DBLE(SUM(kgenref_wpthlp_in, mask=(kgenref_wpthlp_in .eq. &
          &kgenref_wpthlp_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wprtp_in 
          CALL kgen_array_sumcheck("kgenref_wprtp_in", kgen_array_sum, DBLE(SUM(kgenref_wprtp_in, mask=(kgenref_wprtp_in .eq. &
          &kgenref_wprtp_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rtpthlp_in 
          CALL kgen_array_sumcheck("kgenref_rtpthlp_in", kgen_array_sum, DBLE(SUM(kgenref_rtpthlp_in, mask=(kgenref_rtpthlp_in &
          &.eq. kgenref_rtpthlp_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rtp2_in 
          CALL kgen_array_sumcheck("kgenref_rtp2_in", kgen_array_sum, DBLE(SUM(kgenref_rtp2_in, mask=(kgenref_rtp2_in .eq. &
          &kgenref_rtp2_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rtp3_in 
          CALL kgen_array_sumcheck("kgenref_rtp3_in", kgen_array_sum, DBLE(SUM(kgenref_rtp3_in, mask=(kgenref_rtp3_in .eq. &
          &kgenref_rtp3_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_thlp2_in 
          CALL kgen_array_sumcheck("kgenref_thlp2_in", kgen_array_sum, DBLE(SUM(kgenref_thlp2_in, mask=(kgenref_thlp2_in .eq. &
          &kgenref_thlp2_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_thlp3_in 
          CALL kgen_array_sumcheck("kgenref_thlp3_in", kgen_array_sum, DBLE(SUM(kgenref_thlp3_in, mask=(kgenref_thlp3_in .eq. &
          &kgenref_thlp3_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_up2_in 
          CALL kgen_array_sumcheck("kgenref_up2_in", kgen_array_sum, DBLE(SUM(kgenref_up2_in, mask=(kgenref_up2_in .eq. &
          &kgenref_up2_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_vp2_in 
          CALL kgen_array_sumcheck("kgenref_vp2_in", kgen_array_sum, DBLE(SUM(kgenref_vp2_in, mask=(kgenref_vp2_in .eq. &
          &kgenref_vp2_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_upwp_in 
          CALL kgen_array_sumcheck("kgenref_upwp_in", kgen_array_sum, DBLE(SUM(kgenref_upwp_in, mask=(kgenref_upwp_in .eq. &
          &kgenref_upwp_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_vpwp_in 
          CALL kgen_array_sumcheck("kgenref_vpwp_in", kgen_array_sum, DBLE(SUM(kgenref_vpwp_in, mask=(kgenref_vpwp_in .eq. &
          &kgenref_vpwp_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_thlm_in 
          CALL kgen_array_sumcheck("kgenref_thlm_in", kgen_array_sum, DBLE(SUM(kgenref_thlm_in, mask=(kgenref_thlm_in .eq. &
          &kgenref_thlm_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rtm_in 
          CALL kgen_array_sumcheck("kgenref_rtm_in", kgen_array_sum, DBLE(SUM(kgenref_rtm_in, mask=(kgenref_rtm_in .eq. &
          &kgenref_rtm_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rvm_in 
          CALL kgen_array_sumcheck("kgenref_rvm_in", kgen_array_sum, DBLE(SUM(kgenref_rvm_in, mask=(kgenref_rvm_in .eq. &
          &kgenref_rvm_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_um_in 
          CALL kgen_array_sumcheck("kgenref_um_in", kgen_array_sum, DBLE(SUM(kgenref_um_in, mask=(kgenref_um_in .eq. &
          &kgenref_um_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_vm_in 
          CALL kgen_array_sumcheck("kgenref_vm_in", kgen_array_sum, DBLE(SUM(kgenref_vm_in, mask=(kgenref_vm_in .eq. &
          &kgenref_vm_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rho_in 
          CALL kgen_array_sumcheck("kgenref_rho_in", kgen_array_sum, DBLE(SUM(kgenref_rho_in, mask=(kgenref_rho_in .eq. &
          &kgenref_rho_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_pre_in 
          CALL kgen_array_sumcheck("kgenref_pre_in", kgen_array_sum, DBLE(SUM(kgenref_pre_in, mask=(kgenref_pre_in .eq. &
          &kgenref_pre_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rtp2_mc_out 
          CALL kgen_array_sumcheck("kgenref_rtp2_mc_out", kgen_array_sum, DBLE(SUM(kgenref_rtp2_mc_out, mask=(kgenref_rtp2_mc_out &
          &.eq. kgenref_rtp2_mc_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_thlp2_mc_out 
          CALL kgen_array_sumcheck("kgenref_thlp2_mc_out", kgen_array_sum, DBLE(SUM(kgenref_thlp2_mc_out, &
          &mask=(kgenref_thlp2_mc_out .eq. kgenref_thlp2_mc_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wprtp_mc_out 
          CALL kgen_array_sumcheck("kgenref_wprtp_mc_out", kgen_array_sum, DBLE(SUM(kgenref_wprtp_mc_out, &
          &mask=(kgenref_wprtp_mc_out .eq. kgenref_wprtp_mc_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wpthlp_mc_out 
          CALL kgen_array_sumcheck("kgenref_wpthlp_mc_out", kgen_array_sum, DBLE(SUM(kgenref_wpthlp_mc_out, &
          &mask=(kgenref_wpthlp_mc_out .eq. kgenref_wpthlp_mc_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rtpthlp_mc_out 
          CALL kgen_array_sumcheck("kgenref_rtpthlp_mc_out", kgen_array_sum, DBLE(SUM(kgenref_rtpthlp_mc_out, &
          &mask=(kgenref_rtpthlp_mc_out .eq. kgenref_rtpthlp_mc_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rcm_out 
          CALL kgen_array_sumcheck("kgenref_rcm_out", kgen_array_sum, DBLE(SUM(kgenref_rcm_out, mask=(kgenref_rcm_out .eq. &
          &kgenref_rcm_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rcm_out_zm 
          CALL kgen_array_sumcheck("kgenref_rcm_out_zm", kgen_array_sum, DBLE(SUM(kgenref_rcm_out_zm, mask=(kgenref_rcm_out_zm &
          &.eq. kgenref_rcm_out_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wprcp_out 
          CALL kgen_array_sumcheck("kgenref_wprcp_out", kgen_array_sum, DBLE(SUM(kgenref_wprcp_out, mask=(kgenref_wprcp_out .eq. &
          &kgenref_wprcp_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_cloud_frac_out 
          CALL kgen_array_sumcheck("kgenref_cloud_frac_out", kgen_array_sum, DBLE(SUM(kgenref_cloud_frac_out, &
          &mask=(kgenref_cloud_frac_out .eq. kgenref_cloud_frac_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rcm_in_layer_out 
          CALL kgen_array_sumcheck("kgenref_rcm_in_layer_out", kgen_array_sum, DBLE(SUM(kgenref_rcm_in_layer_out, &
          &mask=(kgenref_rcm_in_layer_out .eq. kgenref_rcm_in_layer_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_cloud_cover_out 
          CALL kgen_array_sumcheck("kgenref_cloud_cover_out", kgen_array_sum, DBLE(SUM(kgenref_cloud_cover_out, &
          &mask=(kgenref_cloud_cover_out .eq. kgenref_cloud_cover_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_thlprcp_out 
          CALL kgen_array_sumcheck("kgenref_thlprcp_out", kgen_array_sum, DBLE(SUM(kgenref_thlprcp_out, mask=(kgenref_thlprcp_out &
          &.eq. kgenref_thlprcp_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rho_ds_zm 
          CALL kgen_array_sumcheck("kgenref_rho_ds_zm", kgen_array_sum, DBLE(SUM(kgenref_rho_ds_zm, mask=(kgenref_rho_ds_zm .eq. &
          &kgenref_rho_ds_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rho_ds_zt 
          CALL kgen_array_sumcheck("kgenref_rho_ds_zt", kgen_array_sum, DBLE(SUM(kgenref_rho_ds_zt, mask=(kgenref_rho_ds_zt .eq. &
          &kgenref_rho_ds_zt))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_invrs_rho_ds_zm 
          CALL kgen_array_sumcheck("kgenref_invrs_rho_ds_zm", kgen_array_sum, DBLE(SUM(kgenref_invrs_rho_ds_zm, &
          &mask=(kgenref_invrs_rho_ds_zm .eq. kgenref_invrs_rho_ds_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_invrs_rho_ds_zt 
          CALL kgen_array_sumcheck("kgenref_invrs_rho_ds_zt", kgen_array_sum, DBLE(SUM(kgenref_invrs_rho_ds_zt, &
          &mask=(kgenref_invrs_rho_ds_zt .eq. kgenref_invrs_rho_ds_zt))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_thv_ds_zm 
          CALL kgen_array_sumcheck("kgenref_thv_ds_zm", kgen_array_sum, DBLE(SUM(kgenref_thv_ds_zm, mask=(kgenref_thv_ds_zm .eq. &
          &kgenref_thv_ds_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_thv_ds_zt 
          CALL kgen_array_sumcheck("kgenref_thv_ds_zt", kgen_array_sum, DBLE(SUM(kgenref_thv_ds_zt, mask=(kgenref_thv_ds_zt .eq. &
          &kgenref_thv_ds_zt))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rfrzm 
          CALL kgen_array_sumcheck("kgenref_rfrzm", kgen_array_sum, DBLE(SUM(kgenref_rfrzm, mask=(kgenref_rfrzm .eq. &
          &kgenref_rfrzm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_radf 
          CALL kgen_array_sumcheck("kgenref_radf", kgen_array_sum, DBLE(SUM(kgenref_radf, mask=(kgenref_radf .eq. &
          &kgenref_radf))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wprtp_forcing 
          CALL kgen_array_sumcheck("kgenref_wprtp_forcing", kgen_array_sum, DBLE(SUM(kgenref_wprtp_forcing, &
          &mask=(kgenref_wprtp_forcing .eq. kgenref_wprtp_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wpthlp_forcing 
          CALL kgen_array_sumcheck("kgenref_wpthlp_forcing", kgen_array_sum, DBLE(SUM(kgenref_wpthlp_forcing, &
          &mask=(kgenref_wpthlp_forcing .eq. kgenref_wpthlp_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rtp2_forcing 
          CALL kgen_array_sumcheck("kgenref_rtp2_forcing", kgen_array_sum, DBLE(SUM(kgenref_rtp2_forcing, &
          &mask=(kgenref_rtp2_forcing .eq. kgenref_rtp2_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_thlp2_forcing 
          CALL kgen_array_sumcheck("kgenref_thlp2_forcing", kgen_array_sum, DBLE(SUM(kgenref_thlp2_forcing, &
          &mask=(kgenref_thlp2_forcing .eq. kgenref_thlp2_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rtpthlp_forcing 
          CALL kgen_array_sumcheck("kgenref_rtpthlp_forcing", kgen_array_sum, DBLE(SUM(kgenref_rtpthlp_forcing, &
          &mask=(kgenref_rtpthlp_forcing .eq. kgenref_rtpthlp_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_ice_supersat_frac 
          CALL kgen_array_sumcheck("kgenref_ice_supersat_frac", kgen_array_sum, DBLE(SUM(kgenref_ice_supersat_frac, &
          &mask=(kgenref_ice_supersat_frac .eq. kgenref_ice_supersat_frac))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_zt_g 
          CALL kgen_array_sumcheck("kgenref_zt_g", kgen_array_sum, DBLE(SUM(kgenref_zt_g, mask=(kgenref_zt_g .eq. &
          &kgenref_zt_g))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_zi_g 
          CALL kgen_array_sumcheck("kgenref_zi_g", kgen_array_sum, DBLE(SUM(kgenref_zi_g, mask=(kgenref_zi_g .eq. &
          &kgenref_zi_g))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_zt_out 
          CALL kgen_array_sumcheck("kgenref_zt_out", kgen_array_sum, DBLE(SUM(kgenref_zt_out, mask=(kgenref_zt_out .eq. &
          &kgenref_zt_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_zi_out 
          CALL kgen_array_sumcheck("kgenref_zi_out", kgen_array_sum, DBLE(SUM(kgenref_zi_out, mask=(kgenref_zi_out .eq. &
          &kgenref_zi_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgenref_fcor 
      READ (UNIT = kgen_unit) kgenref_sfc_elevation 
      READ (UNIT = kgen_unit) kgenref_ubar 
      READ (UNIT = kgen_unit) kgenref_ustar 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_thlm_forcing 
          CALL kgen_array_sumcheck("kgenref_thlm_forcing", kgen_array_sum, DBLE(SUM(kgenref_thlm_forcing, &
          &mask=(kgenref_thlm_forcing .eq. kgenref_thlm_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rtm_forcing 
          CALL kgen_array_sumcheck("kgenref_rtm_forcing", kgen_array_sum, DBLE(SUM(kgenref_rtm_forcing, mask=(kgenref_rtm_forcing &
          &.eq. kgenref_rtm_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_um_forcing 
          CALL kgen_array_sumcheck("kgenref_um_forcing", kgen_array_sum, DBLE(SUM(kgenref_um_forcing, mask=(kgenref_um_forcing &
          &.eq. kgenref_um_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_vm_forcing 
          CALL kgen_array_sumcheck("kgenref_vm_forcing", kgen_array_sum, DBLE(SUM(kgenref_vm_forcing, mask=(kgenref_vm_forcing &
          &.eq. kgenref_vm_forcing))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wm_zm 
          CALL kgen_array_sumcheck("kgenref_wm_zm", kgen_array_sum, DBLE(SUM(kgenref_wm_zm, mask=(kgenref_wm_zm .eq. &
          &kgenref_wm_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wm_zt 
          CALL kgen_array_sumcheck("kgenref_wm_zt", kgen_array_sum, DBLE(SUM(kgenref_wm_zt, mask=(kgenref_wm_zt .eq. &
          &kgenref_wm_zt))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_p_in_pa 
          CALL kgen_array_sumcheck("kgenref_p_in_pa", kgen_array_sum, DBLE(SUM(kgenref_p_in_pa, mask=(kgenref_p_in_pa .eq. &
          &kgenref_p_in_pa))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rho_zm 
          CALL kgen_array_sumcheck("kgenref_rho_zm", kgen_array_sum, DBLE(SUM(kgenref_rho_zm, mask=(kgenref_rho_zm .eq. &
          &kgenref_rho_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_exner 
          CALL kgen_array_sumcheck("kgenref_exner", kgen_array_sum, DBLE(SUM(kgenref_exner, mask=(kgenref_exner .eq. &
          &kgenref_exner))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgenref_wpthlp_sfc 
      READ (UNIT = kgen_unit) kgenref_wprtp_sfc 
      READ (UNIT = kgen_unit) kgenref_upwp_sfc 
      READ (UNIT = kgen_unit) kgenref_vpwp_sfc 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wpedsclrp_sfc 
          CALL kgen_array_sumcheck("kgenref_wpedsclrp_sfc", kgen_array_sum, DBLE(SUM(kgenref_wpedsclrp_sfc, &
          &mask=(kgenref_wpedsclrp_sfc .eq. kgenref_wpedsclrp_sfc))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgenref_bflx22 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_khzm_out 
          CALL kgen_array_sumcheck("kgenref_khzm_out", kgen_array_sum, DBLE(SUM(kgenref_khzm_out, mask=(kgenref_khzm_out .eq. &
          &kgenref_khzm_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_khzt_out 
          CALL kgen_array_sumcheck("kgenref_khzt_out", kgen_array_sum, DBLE(SUM(kgenref_khzt_out, mask=(kgenref_khzt_out .eq. &
          &kgenref_khzt_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_qclvar_out 
          CALL kgen_array_sumcheck("kgenref_qclvar_out", kgen_array_sum, DBLE(SUM(kgenref_qclvar_out, mask=(kgenref_qclvar_out &
          &.eq. kgenref_qclvar_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_qclvar 
          CALL kgen_array_sumcheck("kgenref_qclvar", kgen_array_sum, DBLE(SUM(kgenref_qclvar, mask=(kgenref_qclvar .eq. &
          &kgenref_qclvar))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgenref_zo 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_dz_g 
          CALL kgen_array_sumcheck("kgenref_dz_g", kgen_array_sum, DBLE(SUM(kgenref_dz_g, mask=(kgenref_dz_g .eq. &
          &kgenref_dz_g))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgenref_se_upper_a 
      READ (UNIT = kgen_unit) kgenref_se_upper_b 
      READ (UNIT = kgen_unit) kgenref_se_upper_diss 
      READ (UNIT = kgen_unit) kgenref_tw_upper_a 
      READ (UNIT = kgen_unit) kgenref_tw_upper_b 
      READ (UNIT = kgen_unit) kgenref_tw_upper_diss 
      READ (UNIT = kgen_unit) kgenref_host_dx 
      READ (UNIT = kgen_unit) kgenref_host_dy 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_ke_a 
          CALL kgen_array_sumcheck("kgenref_ke_a", kgen_array_sum, DBLE(SUM(kgenref_ke_a, mask=(kgenref_ke_a .eq. &
          &kgenref_ke_a))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_ke_b 
          CALL kgen_array_sumcheck("kgenref_ke_b", kgen_array_sum, DBLE(SUM(kgenref_ke_b, mask=(kgenref_ke_b .eq. &
          &kgenref_ke_b))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_te_a 
          CALL kgen_array_sumcheck("kgenref_te_a", kgen_array_sum, DBLE(SUM(kgenref_te_a, mask=(kgenref_te_a .eq. &
          &kgenref_te_a))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_te_b 
          CALL kgen_array_sumcheck("kgenref_te_b", kgen_array_sum, DBLE(SUM(kgenref_te_b, mask=(kgenref_te_b .eq. &
          &kgenref_te_b))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wv_a 
          CALL kgen_array_sumcheck("kgenref_wv_a", kgen_array_sum, DBLE(SUM(kgenref_wv_a, mask=(kgenref_wv_a .eq. &
          &kgenref_wv_a))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wl_a 
          CALL kgen_array_sumcheck("kgenref_wl_a", kgen_array_sum, DBLE(SUM(kgenref_wl_a, mask=(kgenref_wl_a .eq. &
          &kgenref_wl_a))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wv_b 
          CALL kgen_array_sumcheck("kgenref_wv_b", kgen_array_sum, DBLE(SUM(kgenref_wv_b, mask=(kgenref_wv_b .eq. &
          &kgenref_wv_b))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wl_b 
          CALL kgen_array_sumcheck("kgenref_wl_b", kgen_array_sum, DBLE(SUM(kgenref_wl_b, mask=(kgenref_wl_b .eq. &
          &kgenref_wl_b))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_clubb_s 
          CALL kgen_array_sumcheck("kgenref_clubb_s", kgen_array_sum, DBLE(SUM(kgenref_clubb_s, mask=(kgenref_clubb_s .eq. &
          &kgenref_clubb_s))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_se_a 
          CALL kgen_array_sumcheck("kgenref_se_a", kgen_array_sum, DBLE(SUM(kgenref_se_a, mask=(kgenref_se_a .eq. &
          &kgenref_se_a))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_se_b 
          CALL kgen_array_sumcheck("kgenref_se_b", kgen_array_sum, DBLE(SUM(kgenref_se_b, mask=(kgenref_se_b .eq. &
          &kgenref_se_b))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgenref_se_dis 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_edsclr_out 
          CALL kgen_array_sumcheck("kgenref_edsclr_out", kgen_array_sum, DBLE(SUM(kgenref_edsclr_out, mask=(kgenref_edsclr_out &
          &.eq. kgenref_edsclr_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rcm 
          CALL kgen_array_sumcheck("kgenref_rcm", kgen_array_sum, DBLE(SUM(kgenref_rcm, mask=(kgenref_rcm .eq. kgenref_rcm))), &
          &.TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_cloud_frac 
          CALL kgen_array_sumcheck("kgenref_cloud_frac", kgen_array_sum, DBLE(SUM(kgenref_cloud_frac, mask=(kgenref_cloud_frac &
          &.eq. kgenref_cloud_frac))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_rcm_in_layer 
          CALL kgen_array_sumcheck("kgenref_rcm_in_layer", kgen_array_sum, DBLE(SUM(kgenref_rcm_in_layer, &
          &mask=(kgenref_rcm_in_layer .eq. kgenref_rcm_in_layer))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_wprcp 
          CALL kgen_array_sumcheck("kgenref_wprcp", kgen_array_sum, DBLE(SUM(kgenref_wprcp, mask=(kgenref_wprcp .eq. &
          &kgenref_wprcp))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgenref_dum1 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_qrl_clubb 
          CALL kgen_array_sumcheck("kgenref_qrl_clubb", kgen_array_sum, DBLE(SUM(kgenref_qrl_clubb, mask=(kgenref_qrl_clubb .eq. &
          &kgenref_qrl_clubb))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_qrl_zm 
          !CALL kgen_array_sumcheck("kgenref_qrl_zm", kgen_array_sum, DBLE(SUM(kgenref_qrl_zm, mask=(kgenref_qrl_zm .eq. &
          !&kgenref_qrl_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_thlp2_rad_out 
          !CALL kgen_array_sumcheck("kgenref_thlp2_rad_out", kgen_array_sum, DBLE(SUM(kgenref_thlp2_rad_out, &
          !&mask=(kgenref_thlp2_rad_out .eq. kgenref_thlp2_rad_out))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgenref_rtm_test 
      READ (UNIT = kgen_unit) kgenref_time_elapsed 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgenref_clubb_params 
          CALL kgen_array_sumcheck("kgenref_clubb_params", kgen_array_sum, DBLE(SUM(kgenref_clubb_params, &
          &mask=(kgenref_clubb_params .eq. kgenref_clubb_params))), .TRUE.) 
      END IF   
      CALL kr_kgen_clubb_tend_cam_subp2(kgenref_pdf_params, kgen_unit, "kgenref_pdf_params", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_wp2, kgen_unit, "kgenref_wp2", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_wp3, kgen_unit, "kgenref_wp3", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_wpthlp, kgen_unit, "kgenref_wpthlp", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_wprtp, kgen_unit, "kgenref_wprtp", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_rtpthlp, kgen_unit, "kgenref_rtpthlp", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_rtp2, kgen_unit, "kgenref_rtp2", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_thlp2, kgen_unit, "kgenref_thlp2", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_up2, kgen_unit, "kgenref_up2", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_vp2, kgen_unit, "kgenref_vp2", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_upwp, kgen_unit, "kgenref_upwp", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_vpwp, kgen_unit, "kgenref_vpwp", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_thlm, kgen_unit, "kgenref_thlm", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_rtm, kgen_unit, "kgenref_rtm", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_um, kgen_unit, "kgenref_um", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_vm, kgen_unit, "kgenref_vm", .FALSE.) 
      CALL kr_clubb_tend_cam_real__r8_dim2_ptr(kgenref_khzm, kgen_unit, "kgenref_khzm", .FALSE.) 
      READ (UNIT = kgen_unit) kgenref_apply_to_surface 

   !-----------------------------------------------------------------------------------------------!
   !-----------------------------------------------------------------------------------------------!
   !-----------------------------------------------------------------------------------------------!
   !       MAIN COMPUTATION BEGINS HERE                                                            !
   !-----------------------------------------------------------------------------------------------!
   !-----------------------------------------------------------------------------------------------!
   !-----------------------------------------------------------------------------------------------!


 !  Get indicees for cloud and ice mass and cloud and ice number


 !  Copy the state to state1 array to use in this routine
   !  Initialize physics tendency arrays, copy the state to state1 array to use in this routine


   !  Determine number of columns and which chunk computation is to be performed on


   !  Determine time step of physics buffer

   
   !  Establish associations between pointers and physics buffer fields   


   


   ! Initialize the apply_const variable (note special logic is due to eularian backstepping)


   ! Define the grid box size.  CLUBB needs this information to determine what
   !  the maximum length scale should be.  This depends on the column for 
   !  variable mesh grids and lat-lon grids


   !  Determine CLUBB time step and make it sub-step friendly
   !  For now we want CLUBB time step to be 5 min since that is 
   !  what has been scientifically validated.  However, there are certain
   !  instances when a 5 min time step will not be possible (based on 
   !  host model time step or on macro-micro sub-stepping

   
   !  Now check to see if dtime is greater than the host model 
   !    (or sub stepped) time step.  If it is, then simply 
   !    set it equal to the host (or sub step) time step.  
   !    This section is mostly to deal with small host model
   !    time steps (or small sub-steps)
   
   

   !  Now check to see if CLUBB time step divides evenly into
   !    the host model time step.  If not, force it to divide evenly.
   !    We also want it to be 5 minutes or less.  This section is
   !    mainly for host model time steps that are not evenly divisible
   !    by 5 minutes  
   
   

   !  If resulting host model time step and CLUBB time step do not divide evenly
   !    into each other, have model throw a fit.  


   ! Since CLUBB has only been scientifically validated for a 5 minute timestep
   ! (the default value of clubb_timestep), we have decided to error out if the
   ! final value of dtime is less than clubb_timestep.  Thus to use a non-validated
   ! value for dtime the user will need to explicitly change the value of clubb_timestep
   ! in the namelist, or comment this check.
   

   !  determine number of timesteps CLUBB core should be advanced, 
   !  host time step divided by CLUBB time step  

   !  Initialize forcings for transported scalars to zero
  
   
   !  Compute exner function consistent with CLUBB's definition, which uses a constant
   !  surface pressure.  CAM's exner (in state does not).  Therefore, for consistent 
   !  treatment with CLUBB code, anytime exner is needed to treat CLUBB variables 
   !  (such as thlm), use "exner_clubb" other wise use the exner in state
   


   !  At each CLUBB call, initialize mean momentum  and thermo CLUBB state 
   !  from the CAM state 
   


   


  

   !  Compute virtual potential temperature, which is needed for CLUBB  


   !  Initialize physics tendencies


!$kgen begin_callsite clubb_tend_cam
   !  Loop over all columns in lchnk to advance CLUBB core


      IF (kgen_evalstage) THEN 
      END IF   
      IF (kgen_warmupstage) THEN 
      END IF   
      IF (kgen_mainstage) THEN 
      END IF   
        
      !Uncomment following call statement to turn on perturbation experiment. 
      !Adjust perturbation value and/or kind parameter if required. 
      CALL kgen_perturb_real( state1%q, 1.0E-15_r8 ) 
        
        
      !call to kgen kernel 

   do iloop=1,200
   do i=1,ncol   ! loop over columns

      !  Set time_elapsed to host model time step, this is for 
      !  CLUBB's budget stats
      time_elapsed = hdtime

      !  Determine Coriolis force at given latitude.  This is never used
      !  when CLUBB is implemented in a host model, therefore just set
      !  to zero.
      fcor = 0._r8 

      !  Define the CLUBB momentum grid (in height, units of m)
      do k=1,nlev+1
         zi_g(k) = state1%zi(i,pverp-k+1)-state1%zi(i,pver+1)
      enddo 

      !  Define the CLUBB thermodynamic grid (in units of m)
      do k=1,nlev
         zt_g(k+1) = state1%zm(i,pver-k+1)-state1%zi(i,pver+1)
      end do

      do k=1,pver
         dz_g(k) = state1%zi(i,k)-state1%zi(i,k+1)  ! compute thickness
      enddo
 
      !  Thermodynamic ghost point is below surface 
      zt_g(1) = -1._r8*zt_g(2)

      !  Set the elevation of the surface
      sfc_elevation = state1%zi(i,pver+1)
      
      !  Set the grid size
      host_dx = grid_dx(i)
      host_dy = grid_dy(i)

      !  Compute thermodynamic stuff needed for CLUBB on thermo levels.  
      !  Inputs for the momentum levels are set below setup_clubb core
      do k=1,nlev
         p_in_Pa(k+1)         = state1%pmid(i,pver-k+1)                              ! Pressure profile
         exner(k+1)           = 1._r8/exner_clubb(i,pver-k+1)
         rho_ds_zt(k+1)       = (1._r8/gravit)*(state1%pdel(i,pver-k+1)/dz_g(pver-k+1))
         invrs_rho_ds_zt(k+1) = 1._r8/(rho_ds_zt(k+1))                               ! Inverse ds rho at thermo
         rho_in(k+1)          = rho_ds_zt(k+1)                                       ! rho on thermo 
         thv_ds_zt(k+1)       = thv(i,pver-k+1)                                      ! thetav on thermo
         rfrzm(k+1)           = state1%q(i,pver-k+1,ixcldice)   
         radf(k+1)            = radf_clubb(i,pver-k+1)
         qrl_clubb(k+1)       = qrl(i,pver-k+1)/(cpair*state1%pdel(i,pver-k+1))
      enddo

      !  Below computes the same stuff for the ghost point.  May or may
      !  not be needed, just to be safe to avoid NaN's
      rho_ds_zt(1)       = rho_ds_zt(2)
      invrs_rho_ds_zt(1) = invrs_rho_ds_zt(2)
      rho_in(1)          = rho_ds_zt(2)
      thv_ds_zt(1)       = thv_ds_zt(2)
      rho_zt(:)          = rho_in(:)
      p_in_Pa(1)         = p_in_Pa(2)
      exner(1)           = exner(2)
      rfrzm(1)           = rfrzm(2)
      radf(1)            = radf(2)
      qrl_clubb(1)       = qrl_clubb(2)

      !  Compute mean w wind on thermo grid, convert from omega to w 
      wm_zt(1) = 0._r8
      do k=1,nlev
         wm_zt(k+1) = -1._r8*state1%omega(i,pver-k+1)/(rho_in(k+1)*gravit)
      enddo
    
      ! ------------------------------------------------- !
      ! Begin case specific code for SCAM cases.          !
      ! This section of code block NOT called in          !
      ! global simulations                                !
      ! ------------------------------------------------- !

      if (single_column) then

        !  Initialize zo if variable ustar is used

        if (cam_in%landfrac(i) .ge. 0.5_r8) then
           zo = 0.035_r8
        else
           zo = 0.0001_r8
        endif

        !  Compute surface wind (ubar)
        ubar = sqrt(um(i,pver)**2+vm(i,pver)**2)
        if (ubar .lt. 0.25_r8) ubar = 0.25_r8
    
        !  Below denotes case specifics for surface momentum
        !  and thermodynamic fluxes, depending on the case

        !  Define ustar (based on case, if not variable)     
        ustar = 0.25_r8   ! Initialize ustar in case no case
    
        if(trim(scm_clubb_iop_name) .eq. 'BOMEX_5day') then
           ustar = 0.28_r8
        endif
    
        if(trim(scm_clubb_iop_name) .eq. 'ATEX_48hr') then
           ustar = 0.30_r8
        endif
    
        if(trim(scm_clubb_iop_name) .eq. 'RICO_3day') then
           ustar = 0.28_r8
        endif

        if(trim(scm_clubb_iop_name) .eq. 'arm97' .or. trim(scm_clubb_iop_name) .eq. 'gate' .or. &
           trim(scm_clubb_iop_name) .eq. 'toga' .or. trim(scm_clubb_iop_name) .eq. 'mpace' .or. &
           trim(scm_clubb_iop_name) .eq. 'ARM_CC') then
       
             bflx22 = (gravit/theta0)*wpthlp_sfc
             ustar  = diag_ustar(zt_g(2),bflx22,ubar,zo)      
        endif
    
        !  Compute the surface momentum fluxes, if this is a SCAM simulation       
        upwp_sfc = -um(i,pver)*ustar**2/ubar
        vpwp_sfc = -vm(i,pver)*ustar**2/ubar
    
      endif   

      !  Define surface sources for transported variables for diffusion, will 
      !  be zero as these tendencies are done in vertical_diffusion
      do ixind=1,edsclr_dim
         wpedsclrp_sfc(ixind) = 0._r8
      enddo 

      !  Define forcings from CAM to CLUBB as zero for momentum and thermo,
      !  forcings already applied through CAM
      thlm_forcing = 0._r8
      rtm_forcing = 0._r8
      um_forcing = 0._r8
      vm_forcing = 0._r8
 
      wprtp_forcing   = 0._r8
      wpthlp_forcing  = 0._r8
      rtp2_forcing    = 0._r8
      thlp2_forcing   = 0._r8
      rtpthlp_forcing = 0._r8
 
      ice_supersat_frac = 0._r8

      !  Set stats output and increment equal to CLUBB and host dt
      stats_tsamp = dtime
      stats_tout  = hdtime
 
      !  Heights need to be set at each timestep.  Therefore, recall 
      !  setup_grid and setup_parameters for this.  
     
      !  Read in parameters for CLUBB.  Just read in default values 
      call read_parameters_api( -99, "", clubb_params )
 
      !  Set-up CLUBB core at each CLUBB call because heights can change
      call setup_grid_heights_api(l_implemented, grid_type, zi_g(2), &
           zi_g(1), zi_g, zt_g)

      call setup_parameters_api(zi_g(2), clubb_params, nlev+1, grid_type, &
        zi_g, zt_g, err_code)
 
      !  Compute some inputs from the thermodynamic grid
      !  to the momentum grid
      rho_ds_zm       = zt2zm_api(rho_ds_zt)
      rho_zm          = zt2zm_api(rho_zt)
      invrs_rho_ds_zm = zt2zm_api(invrs_rho_ds_zt)
      thv_ds_zm       = zt2zm_api(thv_ds_zt)
      wm_zm           = zt2zm_api(wm_zt)
      
      !  Surface fluxes provided by host model
      wpthlp_sfc = cam_in%shf(i)/(cpair*rho_ds_zm(1))       ! Sensible heat flux
      wprtp_sfc  = cam_in%cflx(i,1)/rho_ds_zm(1)            ! Moisture flux  (check rho)
      upwp_sfc   = cam_in%wsx(i)/rho_ds_zm(1)               ! Surface meridional momentum flux
      vpwp_sfc   = cam_in%wsy(i)/rho_ds_zm(1)               ! Surface zonal momentum flux  
      
      !  Need to flip arrays around for CLUBB core
      do k=1,nlev+1
         um_in(k)      = um(i,pverp-k+1)
         vm_in(k)      = vm(i,pverp-k+1)
         upwp_in(k)    = upwp(i,pverp-k+1)
         vpwp_in(k)    = vpwp(i,pverp-k+1)
         up2_in(k)     = up2(i,pverp-k+1)
         vp2_in(k)     = vp2(i,pverp-k+1)
         wp2_in(k)     = wp2(i,pverp-k+1)
         wp3_in(k)     = wp3(i,pverp-k+1)
         rtp2_in(k)    = rtp2(i,pverp-k+1)
         thlp2_in(k)   = thlp2(i,pverp-k+1)
         thlm_in(k)    = thlm(i,pverp-k+1)
         rtm_in(k)     = rtm(i,pverp-k+1)
         rvm_in(k)     = rvm(i,pverp-k+1)
         wprtp_in(k)   = wprtp(i,pverp-k+1)
         wpthlp_in(k)  = wpthlp(i,pverp-k+1)
         rtpthlp_in(k) = rtpthlp(i,pverp-k+1)
 
         if (k .ne. 1) then
            pre_in(k)    = prer_evap(i,pverp-k+1)
         endif

         !  Initialize these to prevent crashing behavior
         rcm_out(k)          = 0._r8
         wprcp_out(k)        = 0._r8
         cloud_frac_out(k)   = 0._r8
         rcm_in_layer_out(k) = 0._r8
         cloud_cover_out(k)  = 0._r8
         edsclr_in(k,:)      = 0._r8
         khzm_out(k)         = 0._r8
         khzt_out(k)         = 0._r8

         !  higher order scalar stuff, put to zero
         sclrm(k,:)          = 0._r8
         wpsclrp(k,:)        = 0._r8
         sclrp2(k,:)         = 0._r8
         sclrprtp(k,:)       = 0._r8
         sclrpthlp(k,:)      = 0._r8
         wpsclrp_sfc(:)      = 0._r8
         hydromet(k,:)       = 0._r8
         wphydrometp(k,:)    = 0._r8
         wp2hmp(k,:)         = 0._r8
         rtphmp_zt(k,:)      = 0._r8
         thlphmp_zt(k,:)     = 0._r8
 
      enddo
     
      pre_in(1) = pre_in(2)
     
      if (clubb_do_adv) then
        if (macmic_it .eq. 1) then
          wp2_in=zt2zm_api(wp2_in)    
          wpthlp_in=zt2zm_api(wpthlp_in)
          wprtp_in=zt2zm_api(wprtp_in)
          up2_in=zt2zm_api(up2_in)
          vp2_in=zt2zm_api(vp2_in)
          thlp2_in=zt2zm_api(thlp2_in)
          rtp2_in=zt2zm_api(rtp2_in)
          rtpthlp_in=zt2zm_api(rtpthlp_in)
 
          do k=1,nlev+1
            thlp2_in(k)=max(thl_tol**2,thlp2_in(k))
            rtp2_in(k)=max(rt_tol**2,rtp2_in(k))
            wp2_in(k)=max(w_tol_sqd,wp2_in(k))
            up2_in(k)=max(w_tol_sqd,up2_in(k))
            vp2_in(k)=max(w_tol_sqd,vp2_in(k))
          enddo
        endif
      endif

      ! rtp3_in and thlp3_in are not currently used in CLUBB's default code.
      rtp3_in(:)  = 0.0_r8
      thlp3_in(:) = 0.0_r8

      !  Do the same for tracers 
      icnt=0
      do ixind=1,pcnst
         if (lq(ixind))  then 
            icnt=icnt+1
            do k=1,nlev
               edsclr_in(k+1,icnt) = state1%q(i,pver-k+1,ixind)
            enddo
            edsclr_in(1,icnt) = edsclr_in(2,icnt)
         end if
      enddo
      
      if (do_expldiff) then 
        do k=1,nlev
          edsclr_in(k+1,icnt+1) = thlm(i,pver-k+1)
          edsclr_in(k+1,icnt+2) = rtm(i,pver-k+1)
        enddo
        
        edsclr_in(1,icnt+1) = edsclr_in(2,icnt+1)
        edsclr_in(1,icnt+2) = edsclr_in(2,icnt+2)  
      endif    

      do t=1,nadv    ! do needed number of "sub" timesteps for each CAM step
    
         !  Increment the statistics then being stats timestep
         if (l_stats) then
            time_elapsed = time_elapsed+dtime
            call stats_begin_timestep_api(time_elapsed, 1, 1)
         endif

         !  Advance CLUBB CORE one timestep in the future
         call advance_clubb_core_api &
            ( l_implemented, dtime, fcor, sfc_elevation, hydromet_dim, &
            thlm_forcing, rtm_forcing, um_forcing, vm_forcing, &
            sclrm_forcing, edsclrm_forcing, wprtp_forcing, &  
            wpthlp_forcing, rtp2_forcing, thlp2_forcing, &
            rtpthlp_forcing, wm_zm, wm_zt, &      
            wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc, &
            wpsclrp_sfc, wpedsclrp_sfc, &       
            p_in_Pa, rho_zm, rho_in, exner, &
            rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm, &
            invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt, hydromet, &
            rfrzm, radf, &
            wphydrometp, wp2hmp, rtphmp_zt, thlphmp_zt, &
            host_dx, host_dy, &
            um_in, vm_in, upwp_in, &
            vpwp_in, up2_in, vp2_in, &
            thlm_in, rtm_in, wprtp_in, wpthlp_in, &
            wp2_in, wp3_in, rtp2_in, rtp3_in, &
            thlp2_in, thlp3_in, rtpthlp_in, &
            sclrm, sclrp2, sclrprtp, sclrpthlp, &        
            wpsclrp, edsclr_in, err_code, &
            rcm_out, wprcp_out, cloud_frac_out, ice_supersat_frac, &
            rcm_in_layer_out, cloud_cover_out, &
            khzm_out, khzt_out, qclvar_out, thlprcp_out, &
            pdf_params)

         if (do_rainturb) then
            rvm_in = rtm_in - rcm_out 
            call update_xp2_mc_api(nlev+1, dtime, cloud_frac_out, &
            rcm_out, rvm_in, thlm_in, wm_zt, exner, pre_in, pdf_params, &
            rtp2_mc_out, thlp2_mc_out, &
            wprtp_mc_out, wpthlp_mc_out, &
            rtpthlp_mc_out)

            dum1 = (1._r8 - cam_in%landfrac(i))

            ! update turbulent moments based on rain evaporation  
            rtp2_in  = rtp2_in + clubb_rnevap_effic * dum1 * rtp2_mc_out * dtime
            thlp2_in = thlp2_in + clubb_rnevap_effic * dum1 * thlp2_mc_out * dtime  
            wprtp_in = wprtp_in + clubb_rnevap_effic * dum1 * wprtp_mc_out * dtime
            wpthlp_in = wpthlp_in + clubb_rnevap_effic * dum1 * wpthlp_mc_out * dtime
         endif     

         if (do_cldcool) then
         
            rcm_out_zm = zt2zm_api(rcm_out)
            qrl_zm = zt2zm_api(qrl_clubb)
            thlp2_rad_out(:) = 0._r8
            call calculate_thlp2_rad_api(nlev+1, rcm_out_zm, thlprcp_out, qrl_zm, thlp2_rad_out)
            thlp2_in = thlp2_in + thlp2_rad_out * dtime
            thlp2_in = max(thl_tol**2,thlp2_in)
          endif

          !  Check to see if stats should be output, here stats are read into
          !  output arrays to make them conformable to CAM output
          if (l_stats) call stats_end_timestep_clubb(i,out_zt,out_zm,&
                                                     out_radzt,out_radzm,out_sfc)

      enddo  ! end time loop
     
      if (clubb_do_adv) then
         if (macmic_it .eq. cld_macmic_num_steps) then 
            wp2_in=zm2zt_api(wp2_in)   
            wpthlp_in=zm2zt_api(wpthlp_in)
            wprtp_in=zm2zt_api(wprtp_in)
            up2_in=zm2zt_api(up2_in)
            vp2_in=zm2zt_api(vp2_in)
            thlp2_in=zm2zt_api(thlp2_in)
            rtp2_in=zm2zt_api(rtp2_in)
            rtpthlp_in=zm2zt_api(rtpthlp_in) 

            do k=1,nlev+1
               thlp2_in(k)=max(thl_tol**2,thlp2_in(k))
               rtp2_in(k)=max(rt_tol**2,rtp2_in(k))
               wp2_in(k)=max(w_tol_sqd,wp2_in(k))
               up2_in(k)=max(w_tol_sqd,up2_in(k))
               vp2_in(k)=max(w_tol_sqd,vp2_in(k))
            enddo
         endif
      endif

      !  Arrays need to be "flipped" to CAM grid 
      do k=1,nlev+1

          um(i,pverp-k+1)   = um_in(k)
          vm(i,pverp-k+1)   = vm_in(k)
          upwp(i,pverp-k+1) = upwp_in(k)
          vpwp(i,pverp-k+1) = vpwp_in(k)
          up2(i,pverp-k+1)  = up2_in(k)
          vp2(i,pverp-k+1)  = vp2_in(k)
          thlm(i,pverp-k+1) = thlm_in(k)
          rtm(i,pverp-k+1)  = rtm_in(k)
          wprtp(i,pverp-k+1)= wprtp_in(k)
          wpthlp(i,pverp-k+1)   = wpthlp_in(k)
          wp2(i,pverp-k+1)  = wp2_in(k)
          wp3(i,pverp-k+1)  = wp3_in(k)
          rtp2(i,pverp-k+1) = rtp2_in(k)
          thlp2(i,pverp-k+1)= thlp2_in(k)
          rtpthlp(i,pverp-k+1)  = rtpthlp_in(k)
          rcm(i,pverp-k+1)  = rcm_out(k)
          wprcp(i,pverp-k+1)= wprcp_out(k)
          cloud_frac(i,pverp-k+1)   = min(cloud_frac_out(k),1._r8)
          rcm_in_layer(i,pverp-k+1) = rcm_in_layer_out(k)
          zt_out(i,pverp-k+1)       = zt_g(k)
          zi_out(i,pverp-k+1)       = zi_g(k)
          khzm(i,pverp-k+1) = khzm_out(k)
          qclvar(i,pverp-k+1)       = min(1._r8,qclvar_out(k))

          do ixind=1,edsclr_dim
              edsclr_out(pverp-k+1,ixind) = edsclr_in(k,ixind)
          enddo

      enddo

      ! Values to use above top_lev, for variables that have not already been
      ! set up there. These are mostly fill values that should not actually be
      ! used in the run, but may end up in diagnostic output.
      upwp(i,:top_lev-1) = 0._r8
      vpwp(i,:top_lev-1) = 0._r8
      rcm(i,:top_lev-1) = 0._r8
      wprcp(i,:top_lev-1) = 0._r8
      cloud_frac(i,:top_lev-1) = 0._r8
      rcm_in_layer(i,:top_lev-1) = 0._r8
      zt_out(i,:top_lev-1) = 0._r8
      zi_out(i,:top_lev-1) = 0._r8
      khzm(i,:top_lev-1) = 0._r8
      qclvar(i,:top_lev-1) = 2._r8

      ! enforce zero tracer tendencies above the top_lev level -- no change
      icnt=0
      do ixind=1,pcnst
         if (lq(ixind)) then 
            icnt=icnt+1
            edsclr_out(:top_lev-1,icnt) = state1%q(i,:top_lev-1,ixind)
         end if
      enddo

      !  Fill up arrays needed for McICA.  Note we do not want the ghost point,
      !   thus why the second loop is needed.
     
      zi_out(i,1) = 0._r8

      ! Section below is concentrated on energy fixing for conservation.
      !   There are two steps to this process.  The first is to remove any tendencies
      !   CLUBB may have produced above where it is active due to roundoff. 
      !   The second is to provider a fixer because CLUBB and CAM's thermodynamic
      !   variables are different.  

      ! Initialize clubbtop with the chemistry topopause top, to prevent CLUBB from
      !  firing up in the stratosphere 
      clubbtop = troplev(i)
      do while ((rtp2(i,clubbtop) .le. 1.e-15_r8 .and. rcm(i,clubbtop) .eq. 0._r8) .and. clubbtop .lt. pver-1)
         clubbtop = clubbtop + 1
      enddo    
      
      ! Compute static energy using CLUBB's variables
      do k=1,pver
         clubb_s(k) = cpair*((thlm(i,k)+(latvap/cpair)*rcm(i,k))/exner_clubb(i,k))+ &
                      gravit*state1%zm(i,k)+state1%phis(i)      
      enddo      
      
      !  Compute integrals above layer where CLUBB is active
      se_upper_a = 0._r8   ! energy in layers above where CLUBB is active AFTER CLUBB is called
      se_upper_b = 0._r8   ! energy in layers above where CLUBB is active BEFORE CLUBB is called
      tw_upper_a = 0._r8   ! total water in layers above where CLUBB is active AFTER CLUBB is called
      tw_upper_b = 0._r8   ! total water in layers above where CLUBB is active BEFORE CLUBB is called
      do k=1,clubbtop
        se_upper_a = se_upper_a + (clubb_s(k)+0.5_r8*(um(i,k)**2+vm(i,k)**2)+(latvap+latice)* &
                     (rtm(i,k)-rcm(i,k))+(latice)*rcm(i,k))*state1%pdel(i,k)/gravit
        se_upper_b = se_upper_b + (state1%s(i,k)+0.5_r8*(state1%u(i,k)**2+state1%v(i,k)**2)+(latvap+latice)* &
                     state1%q(i,k,ixq)+(latice)*state1%q(i,k,ixcldliq))*state1%pdel(i,k)/gravit
        tw_upper_a = tw_upper_a + rtm(i,k)*state1%pdel(i,k)/gravit
        tw_upper_b = tw_upper_b + (state1%q(i,k,ixq)+state1%q(i,k,ixcldliq))*state1%pdel(i,k)/gravit
      enddo
      
      ! Compute the disbalance of total energy and water in upper levels,
      !   divide by the thickness in the lower atmosphere where we will 
      !   evenly distribute this disbalance
      se_upper_diss = (se_upper_a - se_upper_b)/(state1%pint(i,pverp)-state1%pint(i,clubbtop+1))
      tw_upper_diss = (tw_upper_a - tw_upper_b)/(state1%pint(i,pverp)-state1%pint(i,clubbtop+1))
      
      ! Perform a test to see if there will be any negative RTM errors
      !  in the column.  If so, apply the disbalance to the surface
      apply_to_surface = .false.
      if (tw_upper_diss .lt. 0._r8) then
        do k=clubbtop+1,pver
          rtm_test = (rtm(i,k) + tw_upper_diss*gravit) - rcm(i,k)
          if (rtm_test .lt. 0._r8) then
            apply_to_surface = .true.
          endif
        enddo
      endif
      
      if (apply_to_surface) then
        tw_upper_diss = (tw_upper_a - tw_upper_b)/(state1%pint(i,pverp)-state1%pint(i,pver))
        se_upper_diss = (se_upper_a - se_upper_b)/(state1%pint(i,pverp)-state1%pint(i,pver))
        rtm(i,pver) = rtm(i,pver) + tw_upper_diss*gravit
        if (apply_to_heat) clubb_s(pver) = clubb_s(pver) + se_upper_diss*gravit
      else
        ! Apply the disbalances above to layers where CLUBB is active
        do k=clubbtop+1,pver
          rtm(i,k) = rtm(i,k) + tw_upper_diss*gravit
        if (apply_to_heat) clubb_s(k) = clubb_s(k) + se_upper_diss*gravit
        enddo
      endif      
      
      ! Essentially "zero" out tendencies in the layers above where CLUBB is active
      do k=1,clubbtop
        if (apply_to_heat) clubb_s(k) = state1%s(i,k)
        rcm(i,k) = state1%q(i,k,ixcldliq)
        rtm(i,k) = state1%q(i,k,ixq) + rcm(i,k)
      enddo           

      ! Compute integrals for static energy, kinetic energy, water vapor, and liquid water
      ! after CLUBB is called.  
      se_a = 0._r8
      ke_a = 0._r8
      wv_a = 0._r8
      wl_a = 0._r8
      
      ! Do the same as above, but for before CLUBB was called.
      se_b = 0._r8
      ke_b = 0._r8
      wv_b = 0._r8
      wl_b = 0._r8            
      do k=1,pver
         se_a(i) = se_a(i) + clubb_s(k)*state1%pdel(i,k)/gravit
         ke_a(i) = ke_a(i) + 0.5_r8*(um(i,k)**2+vm(i,k)**2)*state1%pdel(i,k)/gravit
         wv_a(i) = wv_a(i) + (rtm(i,k)-rcm(i,k))*state1%pdel(i,k)/gravit
         wl_a(i) = wl_a(i) + (rcm(i,k))*state1%pdel(i,k)/gravit
 
         se_b(i) = se_b(i) + state1%s(i,k)*state1%pdel(i,k)/gravit
         ke_b(i) = ke_b(i) + 0.5_r8*(state1%u(i,k)**2+state1%v(i,k)**2)*state1%pdel(i,k)/gravit
         wv_b(i) = wv_b(i) + state1%q(i,k,ixq)*state1%pdel(i,k)/gravit
         wl_b(i) = wl_b(i) + state1%q(i,k,ixcldliq)*state1%pdel(i,k)/gravit 
      enddo
     
      ! Based on these integrals, compute the total energy before and after CLUBB call
      te_a(i) = se_a(i) + ke_a(i) + (latvap+latice)*wv_a(i)+latice*wl_a(i)
      te_b(i) = se_b(i) + ke_b(i) + (latvap+latice)*wv_b(i)+latice*wl_b(i)
      
      ! Take into account the surface fluxes of heat and moisture
      !  Use correct qflux from cam_in, not lhf/latvap as was done previously
      te_b(i) = te_b(i)+(cam_in%shf(i)+cam_in%cflx(i,1)*(latvap+latice))*hdtime      

      ! Compute the disbalance of total energy, over depth where CLUBB is active
      se_dis = (te_a(i) - te_b(i))/(state1%pint(i,pverp)-state1%pint(i,clubbtop+1))

      ! Fix the total energy coming out of CLUBB so it achieves enery conservation.
      ! Apply this fixer throughout the column evenly, but only at layers where 
      ! CLUBB is active.
      !
      ! NOTE: The energy fixer seems to cause the climate to change significantly
      ! when using specified dynamics, so allow this to be turned off via a namelist
      ! variable.
      if (clubb_do_energyfix) then
        do k=clubbtop+1,pver
           clubb_s(k) = clubb_s(k) - se_dis*gravit
        enddo
      endif           

      !  Now compute the tendencies of CLUBB to CAM, note that pverp is the ghost point
      !  for all variables and therefore is never called in this loop
      do k=1,pver
  
         ptend_loc%u(i,k)   = (um(i,k)-state1%u(i,k))/hdtime             ! east-west wind
         ptend_loc%v(i,k)   = (vm(i,k)-state1%v(i,k))/hdtime             ! north-south wind
         ptend_loc%q(i,k,ixq) = (rtm(i,k)-rcm(i,k)-state1%q(i,k,ixq))/hdtime ! water vapor
         ptend_loc%q(i,k,ixcldliq) = (rcm(i,k)-state1%q(i,k,ixcldliq))/hdtime   ! Tendency of liquid water
         ptend_loc%s(i,k)   = (clubb_s(k)-state1%s(i,k))/hdtime          ! Tendency of static energy

         if (clubb_do_adv) then
            if (macmic_it .eq. cld_macmic_num_steps) then

               ! Here add a constant to moments which can be either positive or 
               !  negative.  This is to prevent clipping when dynamics tries to
               !  make all constituents positive 
               wp3(i,k) = wp3(i,k) + wp3_const
               rtpthlp(i,k) = rtpthlp(i,k) + rtpthlp_const
               wpthlp(i,k) = wpthlp(i,k) + wpthlp_const
               wprtp(i,k) = wprtp(i,k) + wprtp_const

               ptend_loc%q(i,k,ixthlp2)=(thlp2(i,k)-state1%q(i,k,ixthlp2))/hdtime ! THLP Variance
               ptend_loc%q(i,k,ixrtp2)=(rtp2(i,k)-state1%q(i,k,ixrtp2))/hdtime ! RTP Variance
               ptend_loc%q(i,k,ixrtpthlp)=(rtpthlp(i,k)-state1%q(i,k,ixrtpthlp))/hdtime ! RTP THLP covariance
               ptend_loc%q(i,k,ixwpthlp)=(wpthlp(i,k)-state1%q(i,k,ixwpthlp))/hdtime ! WPTHLP 
               ptend_loc%q(i,k,ixwprtp)=(wprtp(i,k)-state1%q(i,k,ixwprtp))/hdtime ! WPRTP
               ptend_loc%q(i,k,ixwp2)=(wp2(i,k)-state1%q(i,k,ixwp2))/hdtime ! WP2
               ptend_loc%q(i,k,ixwp3)=(wp3(i,k)-state1%q(i,k,ixwp3))/hdtime ! WP3
               ptend_loc%q(i,k,ixup2)=(up2(i,k)-state1%q(i,k,ixup2))/hdtime ! UP2
               ptend_loc%q(i,k,ixvp2)=(vp2(i,k)-state1%q(i,k,ixvp2))/hdtime ! VP2
            else
               ptend_loc%q(i,k,ixthlp2)=0._r8
               ptend_loc%q(i,k,ixrtp2)=0._r8
               ptend_loc%q(i,k,ixrtpthlp)=0._r8
               ptend_loc%q(i,k,ixwpthlp)=0._r8
               ptend_loc%q(i,k,ixwprtp)=0._r8
               ptend_loc%q(i,k,ixwp2)=0._r8
               ptend_loc%q(i,k,ixwp3)=0._r8
               ptend_loc%q(i,k,ixup2)=0._r8
               ptend_loc%q(i,k,ixvp2)=0._r8 
            endif

         endif

         !  Apply tendencies to ice mixing ratio, liquid and ice number, and aerosol constituents.
         !  Loading up this array doesn't mean the tendencies are applied.  
         ! edsclr_out is compressed with just the constituents being used, ptend and state are not compressed

         icnt=0
         do ixind=1,pcnst
            if (lq(ixind)) then
               icnt=icnt+1
               if ((ixind /= ixq)       .and. (ixind /= ixcldliq) .and.&
                   (ixind /= ixthlp2)   .and. (ixind /= ixrtp2)   .and.&
                   (ixind /= ixrtpthlp) .and. (ixind /= ixwpthlp) .and.&
                   (ixind /= ixwprtp)   .and. (ixind /= ixwp2)    .and.&
                   (ixind /= ixwp3)     .and. (ixind /= ixup2)    .and. (ixind /= ixvp2) ) then
                       ptend_loc%q(i,k,ixind) = (edsclr_out(k,icnt)-state1%q(i,k,ixind))/hdtime ! transported constituents 
               end if
            end if
         enddo

      enddo
      

   enddo  ! end column loop
   enddo
   IF (kgen_mainstage) THEN 
         
       !verify init 
       CALL kgen_init_verify(tolerance=9.D-7, minvalue=1.D-14, verboseLevel=1) 
       CALL kgen_init_check(check_status, rank=kgen_mpirank) 
         
       !extern verify variables 
       CALL kv_externs_clubb_intr(check_status) 
       CALL kv_externs_stats_variables(check_status) 
       CALL kv_externs_parameters_tunable(check_status) 
       CALL kv_externs_grid_class(check_status) 
       CALL kv_externs_variables_diagnostic_module(check_status) 
         
       !local verify variables 
       CALL kv_physics_types_physics_ptend("ptend_loc", check_status, ptend_loc, kgenref_ptend_loc) 
       CALL kv_clubb_tend_cam_integer__("i", check_status, i, kgenref_i) 
       CALL kv_clubb_tend_cam_integer__("k", check_status, k, kgenref_k) 
       CALL kv_clubb_tend_cam_integer__("ixind", check_status, ixind, kgenref_ixind) 
       CALL kv_clubb_tend_cam_integer__("t", check_status, t, kgenref_t) 
       CALL kv_clubb_tend_cam_integer__("err_code", check_status, err_code, kgenref_err_code) 
       CALL kv_clubb_tend_cam_integer__("icnt", check_status, icnt, kgenref_icnt) 
       CALL kv_clubb_tend_cam_integer__("clubbtop", check_status, clubbtop, kgenref_clubbtop) 
       CALL kv_clubb_tend_cam_real__r8_dim2("edsclr_in", check_status, edsclr_in, kgenref_edsclr_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wp2_in", check_status, wp2_in, kgenref_wp2_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wp3_in", check_status, wp3_in, kgenref_wp3_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wpthlp_in", check_status, wpthlp_in, kgenref_wpthlp_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wprtp_in", check_status, wprtp_in, kgenref_wprtp_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rtpthlp_in", check_status, rtpthlp_in, kgenref_rtpthlp_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rtp2_in", check_status, rtp2_in, kgenref_rtp2_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rtp3_in", check_status, rtp3_in, kgenref_rtp3_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("thlp2_in", check_status, thlp2_in, kgenref_thlp2_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("thlp3_in", check_status, thlp3_in, kgenref_thlp3_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("up2_in", check_status, up2_in, kgenref_up2_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("vp2_in", check_status, vp2_in, kgenref_vp2_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("upwp_in", check_status, upwp_in, kgenref_upwp_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("vpwp_in", check_status, vpwp_in, kgenref_vpwp_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("thlm_in", check_status, thlm_in, kgenref_thlm_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rtm_in", check_status, rtm_in, kgenref_rtm_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rvm_in", check_status, rvm_in, kgenref_rvm_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("um_in", check_status, um_in, kgenref_um_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("vm_in", check_status, vm_in, kgenref_vm_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rho_in", check_status, rho_in, kgenref_rho_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("pre_in", check_status, pre_in, kgenref_pre_in) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rtp2_mc_out", check_status, rtp2_mc_out, kgenref_rtp2_mc_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("thlp2_mc_out", check_status, thlp2_mc_out, kgenref_thlp2_mc_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wprtp_mc_out", check_status, wprtp_mc_out, kgenref_wprtp_mc_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wpthlp_mc_out", check_status, wpthlp_mc_out, kgenref_wpthlp_mc_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rtpthlp_mc_out", check_status, rtpthlp_mc_out, kgenref_rtpthlp_mc_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rcm_out", check_status, rcm_out, kgenref_rcm_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rcm_out_zm", check_status, rcm_out_zm, kgenref_rcm_out_zm) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wprcp_out", check_status, wprcp_out, kgenref_wprcp_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("cloud_frac_out", check_status, cloud_frac_out, kgenref_cloud_frac_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rcm_in_layer_out", check_status, rcm_in_layer_out, kgenref_rcm_in_layer_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("cloud_cover_out", check_status, cloud_cover_out, kgenref_cloud_cover_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("thlprcp_out", check_status, thlprcp_out, kgenref_thlprcp_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rho_ds_zm", check_status, rho_ds_zm, kgenref_rho_ds_zm) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rho_ds_zt", check_status, rho_ds_zt, kgenref_rho_ds_zt) 
       CALL kv_clubb_tend_cam_real__r8_dim1("invrs_rho_ds_zm", check_status, invrs_rho_ds_zm, kgenref_invrs_rho_ds_zm) 
       CALL kv_clubb_tend_cam_real__r8_dim1("invrs_rho_ds_zt", check_status, invrs_rho_ds_zt, kgenref_invrs_rho_ds_zt) 
       CALL kv_clubb_tend_cam_real__r8_dim1("thv_ds_zm", check_status, thv_ds_zm, kgenref_thv_ds_zm) 
       CALL kv_clubb_tend_cam_real__r8_dim1("thv_ds_zt", check_status, thv_ds_zt, kgenref_thv_ds_zt) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rfrzm", check_status, rfrzm, kgenref_rfrzm) 
       CALL kv_clubb_tend_cam_real__r8_dim1("radf", check_status, radf, kgenref_radf) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wprtp_forcing", check_status, wprtp_forcing, kgenref_wprtp_forcing) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wpthlp_forcing", check_status, wpthlp_forcing, kgenref_wpthlp_forcing) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rtp2_forcing", check_status, rtp2_forcing, kgenref_rtp2_forcing) 
       CALL kv_clubb_tend_cam_real__r8_dim1("thlp2_forcing", check_status, thlp2_forcing, kgenref_thlp2_forcing) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rtpthlp_forcing", check_status, rtpthlp_forcing, kgenref_rtpthlp_forcing) 
       CALL kv_clubb_tend_cam_real__r8_dim1("ice_supersat_frac", check_status, ice_supersat_frac, kgenref_ice_supersat_frac) 
       CALL kv_clubb_tend_cam_real__r8_dim1("zt_g", check_status, zt_g, kgenref_zt_g) 
       CALL kv_clubb_tend_cam_real__r8_dim1("zi_g", check_status, zi_g, kgenref_zi_g) 
       CALL kv_clubb_tend_cam_real__r8_dim2("zt_out", check_status, zt_out, kgenref_zt_out) 
       CALL kv_clubb_tend_cam_real__r8_dim2("zi_out", check_status, zi_out, kgenref_zi_out) 
       CALL kv_clubb_tend_cam_real__r8("fcor", check_status, fcor, kgenref_fcor) 
       CALL kv_clubb_tend_cam_real__r8("sfc_elevation", check_status, sfc_elevation, kgenref_sfc_elevation) 
       CALL kv_clubb_tend_cam_real__r8("ubar", check_status, ubar, kgenref_ubar) 
       CALL kv_clubb_tend_cam_real__r8("ustar", check_status, ustar, kgenref_ustar) 
       CALL kv_clubb_tend_cam_real__r8_dim1("thlm_forcing", check_status, thlm_forcing, kgenref_thlm_forcing) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rtm_forcing", check_status, rtm_forcing, kgenref_rtm_forcing) 
       CALL kv_clubb_tend_cam_real__r8_dim1("um_forcing", check_status, um_forcing, kgenref_um_forcing) 
       CALL kv_clubb_tend_cam_real__r8_dim1("vm_forcing", check_status, vm_forcing, kgenref_vm_forcing) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wm_zm", check_status, wm_zm, kgenref_wm_zm) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wm_zt", check_status, wm_zt, kgenref_wm_zt) 
       CALL kv_clubb_tend_cam_real__r8_dim1("p_in_pa", check_status, p_in_pa, kgenref_p_in_pa) 
       CALL kv_clubb_tend_cam_real__r8_dim1("rho_zm", check_status, rho_zm, kgenref_rho_zm) 
       CALL kv_clubb_tend_cam_real__r8_dim1("exner", check_status, exner, kgenref_exner) 
       CALL kv_clubb_tend_cam_real__r8("wpthlp_sfc", check_status, wpthlp_sfc, kgenref_wpthlp_sfc) 
       CALL kv_clubb_tend_cam_real__r8("wprtp_sfc", check_status, wprtp_sfc, kgenref_wprtp_sfc) 
       CALL kv_clubb_tend_cam_real__r8("upwp_sfc", check_status, upwp_sfc, kgenref_upwp_sfc) 
       CALL kv_clubb_tend_cam_real__r8("vpwp_sfc", check_status, vpwp_sfc, kgenref_vpwp_sfc) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wpedsclrp_sfc", check_status, wpedsclrp_sfc, kgenref_wpedsclrp_sfc) 
       CALL kv_clubb_tend_cam_real__r8("bflx22", check_status, bflx22, kgenref_bflx22) 
       CALL kv_clubb_tend_cam_real__r8_dim1("khzm_out", check_status, khzm_out, kgenref_khzm_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("khzt_out", check_status, khzt_out, kgenref_khzt_out) 
       CALL kv_clubb_tend_cam_real__r8_dim1("qclvar_out", check_status, qclvar_out, kgenref_qclvar_out) 
       CALL kv_clubb_tend_cam_real__r8_dim2("qclvar", check_status, qclvar, kgenref_qclvar) 
       CALL kv_clubb_tend_cam_real__r8("zo", check_status, zo, kgenref_zo) 
       CALL kv_clubb_tend_cam_real__r8_dim1("dz_g", check_status, dz_g, kgenref_dz_g) 
       CALL kv_clubb_tend_cam_real__r8("se_upper_b", check_status, se_upper_b, kgenref_se_upper_b) 
       CALL kv_clubb_tend_cam_real__r8("se_upper_diss", check_status, se_upper_diss, kgenref_se_upper_diss) 
       CALL kv_clubb_tend_cam_real__r8("se_upper_a", check_status, se_upper_a, kgenref_se_upper_a) 
       CALL kv_clubb_tend_cam_real__r8("tw_upper_a", check_status, tw_upper_a, kgenref_tw_upper_a) 
       CALL kv_clubb_tend_cam_real__r8("tw_upper_b", check_status, tw_upper_b, kgenref_tw_upper_b) 
       CALL kv_clubb_tend_cam_real__r8("tw_upper_diss", check_status, tw_upper_diss, kgenref_tw_upper_diss) 
       CALL kv_clubb_tend_cam_real__r8("host_dx", check_status, host_dx, kgenref_host_dx) 
       CALL kv_clubb_tend_cam_real__r8("host_dy", check_status, host_dy, kgenref_host_dy) 
       CALL kv_clubb_tend_cam_real__r8_dim1("te_a", check_status, te_a, kgenref_te_a) 
       CALL kv_clubb_tend_cam_real__r8_dim1("te_b", check_status, te_b, kgenref_te_b) 
       CALL kv_clubb_tend_cam_real__r8_dim1("ke_a", check_status, ke_a, kgenref_ke_a) 
       CALL kv_clubb_tend_cam_real__r8_dim1("ke_b", check_status, ke_b, kgenref_ke_b) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wl_a", check_status, wl_a, kgenref_wl_a) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wl_b", check_status, wl_b, kgenref_wl_b) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wv_b", check_status, wv_b, kgenref_wv_b) 
       CALL kv_clubb_tend_cam_real__r8_dim1("wv_a", check_status, wv_a, kgenref_wv_a) 
       CALL kv_clubb_tend_cam_real__r8("se_dis", check_status, se_dis, kgenref_se_dis) 
       CALL kv_clubb_tend_cam_real__r8_dim1("clubb_s", check_status, clubb_s, kgenref_clubb_s) 
       CALL kv_clubb_tend_cam_real__r8_dim1("se_a", check_status, se_a, kgenref_se_a) 
       CALL kv_clubb_tend_cam_real__r8_dim1("se_b", check_status, se_b, kgenref_se_b) 
       CALL kv_clubb_tend_cam_real__r8_dim2("edsclr_out", check_status, edsclr_out, kgenref_edsclr_out) 
       CALL kv_clubb_tend_cam_real__r8_dim2("rcm", check_status, rcm, kgenref_rcm) 
       CALL kv_clubb_tend_cam_real__r8_dim2("cloud_frac", check_status, cloud_frac, kgenref_cloud_frac) 
       CALL kv_clubb_tend_cam_real__r8_dim2("rcm_in_layer", check_status, rcm_in_layer, kgenref_rcm_in_layer) 
       CALL kv_clubb_tend_cam_real__r8_dim2("wprcp", check_status, wprcp, kgenref_wprcp) 
       CALL kv_clubb_tend_cam_real__r8("dum1", check_status, dum1, kgenref_dum1) 
       CALL kv_clubb_tend_cam_real__r8_dim1("qrl_clubb", check_status, qrl_clubb, kgenref_qrl_clubb) 
       CALL kv_clubb_tend_cam_real__r8_dim1("qrl_zm", check_status, qrl_zm, kgenref_qrl_zm) 
       CALL kv_clubb_tend_cam_real__r8_dim1("thlp2_rad_out", check_status, thlp2_rad_out, kgenref_thlp2_rad_out) 
       CALL kv_clubb_tend_cam_real__r8("rtm_test", check_status, rtm_test, kgenref_rtm_test) 
       CALL kv_clubb_tend_cam_integer__("time_elapsed", check_status, time_elapsed, kgenref_time_elapsed) 
       CALL kv_clubb_tend_cam_real__r8_dim1("clubb_params", check_status, clubb_params, kgenref_clubb_params) 
       CALL kv_kgen_clubb_tend_cam_subp1("pdf_params", check_status, pdf_params, kgenref_pdf_params) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("wp2", check_status, wp2, kgenref_wp2) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("wp3", check_status, wp3, kgenref_wp3) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("wpthlp", check_status, wpthlp, kgenref_wpthlp) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("wprtp", check_status, wprtp, kgenref_wprtp) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("rtpthlp", check_status, rtpthlp, kgenref_rtpthlp) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("rtp2", check_status, rtp2, kgenref_rtp2) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("thlp2", check_status, thlp2, kgenref_thlp2) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("up2", check_status, up2, kgenref_up2) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("vp2", check_status, vp2, kgenref_vp2) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("upwp", check_status, upwp, kgenref_upwp) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("vpwp", check_status, vpwp, kgenref_vpwp) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("thlm", check_status, thlm, kgenref_thlm) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("rtm", check_status, rtm, kgenref_rtm) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("um", check_status, um, kgenref_um) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("vm", check_status, vm, kgenref_vm) 
       CALL kv_clubb_tend_cam_real__r8_dim2_ptr("khzm", check_status, khzm, kgenref_khzm) 
       CALL kv_clubb_tend_cam_logical__("apply_to_surface", check_status, apply_to_surface, kgenref_apply_to_surface) 
       IF (check_status%rank == 0) THEN 
           WRITE (*, *) "" 
       END IF   
       IF (kgen_verboseLevel > 0) THEN 
           IF (check_status%rank == 0) THEN 
               WRITE (*, *) "Number of output variables: ", check_status%numTotal 
               WRITE (*, *) "Number of identical variables: ", check_status%numIdentical 
               WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol 
               WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol 
               WRITE (*, *) "Tolerance: ", kgen_tolerance 
           END IF   
       END IF   
       IF (check_status%rank == 0) THEN 
           WRITE (*, *) "" 
       END IF   
       IF (check_status%numOutTol > 0) THEN 
           IF (check_status%rank == 0) THEN 
               WRITE (*, *) "Verification FAILED with" // TRIM(ADJUSTL(kgen_filepath)) 
           END IF   
           check_status%Passed = .FALSE. 
           kgen_isverified = .FALSE. 
       ELSE 
           IF (check_status%rank == 0) THEN 
               WRITE (*, *) "Verification PASSED with " // TRIM(ADJUSTL(kgen_filepath)) 
           END IF   
           check_status%Passed = .TRUE. 
           kgen_isverified = .TRUE. 
       END IF   
       IF (check_status%rank == 0) THEN 
           WRITE (*, *) "" 
       END IF   
         
#ifdef _MPI 
       call mpi_barrier(mpi_comm_world, kgen_ierr) 
#endif 
         
       kgen_cache_control = CACHE_CONTROL_LENGTH 
       DO kgen_intvar = CACHE_CONTROL_LENGTH-1, 1, -1 
           kgen_cache_control(kgen_intvar) = kgen_cache_control(kgen_intvar+1) - 1   
       END DO   
       CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock) 
       DO kgen_intvar = 1, KGEN_MAXITER 
   do i=1,ncol   ! loop over columns

      !  Set time_elapsed to host model time step, this is for 
      !  CLUBB's budget stats
      time_elapsed = hdtime

      !  Determine Coriolis force at given latitude.  This is never used
      !  when CLUBB is implemented in a host model, therefore just set
      !  to zero.
      fcor = 0._r8 

      !  Define the CLUBB momentum grid (in height, units of m)
      do k=1,nlev+1
         zi_g(k) = state1%zi(i,pverp-k+1)-state1%zi(i,pver+1)
      enddo 

      !  Define the CLUBB thermodynamic grid (in units of m)
      do k=1,nlev
         zt_g(k+1) = state1%zm(i,pver-k+1)-state1%zi(i,pver+1)
      end do

      do k=1,pver
         dz_g(k) = state1%zi(i,k)-state1%zi(i,k+1)  ! compute thickness
      enddo
 
      !  Thermodynamic ghost point is below surface 
      zt_g(1) = -1._r8*zt_g(2)

      !  Set the elevation of the surface
      sfc_elevation = state1%zi(i,pver+1)
      
      !  Set the grid size
      host_dx = grid_dx(i)
      host_dy = grid_dy(i)

      !  Compute thermodynamic stuff needed for CLUBB on thermo levels.  
      !  Inputs for the momentum levels are set below setup_clubb core
      do k=1,nlev
         p_in_Pa(k+1)         = state1%pmid(i,pver-k+1)                              ! Pressure profile
         exner(k+1)           = 1._r8/exner_clubb(i,pver-k+1)
         rho_ds_zt(k+1)       = (1._r8/gravit)*(state1%pdel(i,pver-k+1)/dz_g(pver-k+1))
         invrs_rho_ds_zt(k+1) = 1._r8/(rho_ds_zt(k+1))                               ! Inverse ds rho at thermo
         rho_in(k+1)          = rho_ds_zt(k+1)                                       ! rho on thermo 
         thv_ds_zt(k+1)       = thv(i,pver-k+1)                                      ! thetav on thermo
         rfrzm(k+1)           = state1%q(i,pver-k+1,ixcldice)   
         radf(k+1)            = radf_clubb(i,pver-k+1)
         qrl_clubb(k+1)       = qrl(i,pver-k+1)/(cpair*state1%pdel(i,pver-k+1))
      enddo

      !  Below computes the same stuff for the ghost point.  May or may
      !  not be needed, just to be safe to avoid NaN's
      rho_ds_zt(1)       = rho_ds_zt(2)
      invrs_rho_ds_zt(1) = invrs_rho_ds_zt(2)
      rho_in(1)          = rho_ds_zt(2)
      thv_ds_zt(1)       = thv_ds_zt(2)
      rho_zt(:)          = rho_in(:)
      p_in_Pa(1)         = p_in_Pa(2)
      exner(1)           = exner(2)
      rfrzm(1)           = rfrzm(2)
      radf(1)            = radf(2)
      qrl_clubb(1)       = qrl_clubb(2)

      !  Compute mean w wind on thermo grid, convert from omega to w 
      wm_zt(1) = 0._r8
      do k=1,nlev
         wm_zt(k+1) = -1._r8*state1%omega(i,pver-k+1)/(rho_in(k+1)*gravit)
      enddo
    
      ! ------------------------------------------------- !
      ! Begin case specific code for SCAM cases.          !
      ! This section of code block NOT called in          !
      ! global simulations                                !
      ! ------------------------------------------------- !

      if (single_column) then

        !  Initialize zo if variable ustar is used

        if (cam_in%landfrac(i) .ge. 0.5_r8) then
           zo = 0.035_r8
        else
           zo = 0.0001_r8
        endif

        !  Compute surface wind (ubar)
        ubar = sqrt(um(i,pver)**2+vm(i,pver)**2)
        if (ubar .lt. 0.25_r8) ubar = 0.25_r8
    
        !  Below denotes case specifics for surface momentum
        !  and thermodynamic fluxes, depending on the case

        !  Define ustar (based on case, if not variable)     
        ustar = 0.25_r8   ! Initialize ustar in case no case
    
        if(trim(scm_clubb_iop_name) .eq. 'BOMEX_5day') then
           ustar = 0.28_r8
        endif
    
        if(trim(scm_clubb_iop_name) .eq. 'ATEX_48hr') then
           ustar = 0.30_r8
        endif
    
        if(trim(scm_clubb_iop_name) .eq. 'RICO_3day') then
           ustar = 0.28_r8
        endif

        if(trim(scm_clubb_iop_name) .eq. 'arm97' .or. trim(scm_clubb_iop_name) .eq. 'gate' .or. &
           trim(scm_clubb_iop_name) .eq. 'toga' .or. trim(scm_clubb_iop_name) .eq. 'mpace' .or. &
           trim(scm_clubb_iop_name) .eq. 'ARM_CC') then
       
             bflx22 = (gravit/theta0)*wpthlp_sfc
             ustar  = diag_ustar(zt_g(2),bflx22,ubar,zo)      
        endif
    
        !  Compute the surface momentum fluxes, if this is a SCAM simulation       
        upwp_sfc = -um(i,pver)*ustar**2/ubar
        vpwp_sfc = -vm(i,pver)*ustar**2/ubar
    
      endif   

      !  Define surface sources for transported variables for diffusion, will 
      !  be zero as these tendencies are done in vertical_diffusion
      do ixind=1,edsclr_dim
         wpedsclrp_sfc(ixind) = 0._r8
      enddo 

      !  Define forcings from CAM to CLUBB as zero for momentum and thermo,
      !  forcings already applied through CAM
      thlm_forcing = 0._r8
      rtm_forcing = 0._r8
      um_forcing = 0._r8
      vm_forcing = 0._r8
 
      wprtp_forcing   = 0._r8
      wpthlp_forcing  = 0._r8
      rtp2_forcing    = 0._r8
      thlp2_forcing   = 0._r8
      rtpthlp_forcing = 0._r8
 
      ice_supersat_frac = 0._r8

      !  Set stats output and increment equal to CLUBB and host dt
      stats_tsamp = dtime
      stats_tout  = hdtime
 
      !  Heights need to be set at each timestep.  Therefore, recall 
      !  setup_grid and setup_parameters for this.  
     
      !  Read in parameters for CLUBB.  Just read in default values 
      call read_parameters_api( -99, "", clubb_params )
 
      !  Set-up CLUBB core at each CLUBB call because heights can change
      call setup_grid_heights_api(l_implemented, grid_type, zi_g(2), &
           zi_g(1), zi_g, zt_g)

      call setup_parameters_api(zi_g(2), clubb_params, nlev+1, grid_type, &
        zi_g, zt_g, err_code)
 
      !  Compute some inputs from the thermodynamic grid
      !  to the momentum grid
      rho_ds_zm       = zt2zm_api(rho_ds_zt)
      rho_zm          = zt2zm_api(rho_zt)
      invrs_rho_ds_zm = zt2zm_api(invrs_rho_ds_zt)
      thv_ds_zm       = zt2zm_api(thv_ds_zt)
      wm_zm           = zt2zm_api(wm_zt)
      
      !  Surface fluxes provided by host model
      wpthlp_sfc = cam_in%shf(i)/(cpair*rho_ds_zm(1))       ! Sensible heat flux
      wprtp_sfc  = cam_in%cflx(i,1)/rho_ds_zm(1)            ! Moisture flux  (check rho)
      upwp_sfc   = cam_in%wsx(i)/rho_ds_zm(1)               ! Surface meridional momentum flux
      vpwp_sfc   = cam_in%wsy(i)/rho_ds_zm(1)               ! Surface zonal momentum flux  
      
      !  Need to flip arrays around for CLUBB core
      do k=1,nlev+1
         um_in(k)      = um(i,pverp-k+1)
         vm_in(k)      = vm(i,pverp-k+1)
         upwp_in(k)    = upwp(i,pverp-k+1)
         vpwp_in(k)    = vpwp(i,pverp-k+1)
         up2_in(k)     = up2(i,pverp-k+1)
         vp2_in(k)     = vp2(i,pverp-k+1)
         wp2_in(k)     = wp2(i,pverp-k+1)
         wp3_in(k)     = wp3(i,pverp-k+1)
         rtp2_in(k)    = rtp2(i,pverp-k+1)
         thlp2_in(k)   = thlp2(i,pverp-k+1)
         thlm_in(k)    = thlm(i,pverp-k+1)
         rtm_in(k)     = rtm(i,pverp-k+1)
         rvm_in(k)     = rvm(i,pverp-k+1)
         wprtp_in(k)   = wprtp(i,pverp-k+1)
         wpthlp_in(k)  = wpthlp(i,pverp-k+1)
         rtpthlp_in(k) = rtpthlp(i,pverp-k+1)
 
         if (k .ne. 1) then
            pre_in(k)    = prer_evap(i,pverp-k+1)
         endif

         !  Initialize these to prevent crashing behavior
         rcm_out(k)          = 0._r8
         wprcp_out(k)        = 0._r8
         cloud_frac_out(k)   = 0._r8
         rcm_in_layer_out(k) = 0._r8
         cloud_cover_out(k)  = 0._r8
         edsclr_in(k,:)      = 0._r8
         khzm_out(k)         = 0._r8
         khzt_out(k)         = 0._r8

         !  higher order scalar stuff, put to zero
         sclrm(k,:)          = 0._r8
         wpsclrp(k,:)        = 0._r8
         sclrp2(k,:)         = 0._r8
         sclrprtp(k,:)       = 0._r8
         sclrpthlp(k,:)      = 0._r8
         wpsclrp_sfc(:)      = 0._r8
         hydromet(k,:)       = 0._r8
         wphydrometp(k,:)    = 0._r8
         wp2hmp(k,:)         = 0._r8
         rtphmp_zt(k,:)      = 0._r8
         thlphmp_zt(k,:)     = 0._r8
 
      enddo
     
      pre_in(1) = pre_in(2)
     
      if (clubb_do_adv) then
        if (macmic_it .eq. 1) then
          wp2_in=zt2zm_api(wp2_in)    
          wpthlp_in=zt2zm_api(wpthlp_in)
          wprtp_in=zt2zm_api(wprtp_in)
          up2_in=zt2zm_api(up2_in)
          vp2_in=zt2zm_api(vp2_in)
          thlp2_in=zt2zm_api(thlp2_in)
          rtp2_in=zt2zm_api(rtp2_in)
          rtpthlp_in=zt2zm_api(rtpthlp_in)
 
          do k=1,nlev+1
            thlp2_in(k)=max(thl_tol**2,thlp2_in(k))
            rtp2_in(k)=max(rt_tol**2,rtp2_in(k))
            wp2_in(k)=max(w_tol_sqd,wp2_in(k))
            up2_in(k)=max(w_tol_sqd,up2_in(k))
            vp2_in(k)=max(w_tol_sqd,vp2_in(k))
          enddo
        endif
      endif

      ! rtp3_in and thlp3_in are not currently used in CLUBB's default code.
      rtp3_in(:)  = 0.0_r8
      thlp3_in(:) = 0.0_r8

      !  Do the same for tracers 
      icnt=0
      do ixind=1,pcnst
         if (lq(ixind))  then 
            icnt=icnt+1
            do k=1,nlev
               edsclr_in(k+1,icnt) = state1%q(i,pver-k+1,ixind)
            enddo
            edsclr_in(1,icnt) = edsclr_in(2,icnt)
         end if
      enddo
      
      if (do_expldiff) then 
        do k=1,nlev
          edsclr_in(k+1,icnt+1) = thlm(i,pver-k+1)
          edsclr_in(k+1,icnt+2) = rtm(i,pver-k+1)
        enddo
        
        edsclr_in(1,icnt+1) = edsclr_in(2,icnt+1)
        edsclr_in(1,icnt+2) = edsclr_in(2,icnt+2)  
      endif    

      do t=1,nadv    ! do needed number of "sub" timesteps for each CAM step
    
         !  Increment the statistics then being stats timestep
         if (l_stats) then
            time_elapsed = time_elapsed+dtime
            call stats_begin_timestep_api(time_elapsed, 1, 1)
         endif

         !  Advance CLUBB CORE one timestep in the future
         call advance_clubb_core_api &
            ( l_implemented, dtime, fcor, sfc_elevation, hydromet_dim, &
            thlm_forcing, rtm_forcing, um_forcing, vm_forcing, &
            sclrm_forcing, edsclrm_forcing, wprtp_forcing, &  
            wpthlp_forcing, rtp2_forcing, thlp2_forcing, &
            rtpthlp_forcing, wm_zm, wm_zt, &      
            wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc, &
            wpsclrp_sfc, wpedsclrp_sfc, &       
            p_in_Pa, rho_zm, rho_in, exner, &
            rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm, &
            invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt, hydromet, &
            rfrzm, radf, &
            wphydrometp, wp2hmp, rtphmp_zt, thlphmp_zt, &
            host_dx, host_dy, &
            um_in, vm_in, upwp_in, &
            vpwp_in, up2_in, vp2_in, &
            thlm_in, rtm_in, wprtp_in, wpthlp_in, &
            wp2_in, wp3_in, rtp2_in, rtp3_in, &
            thlp2_in, thlp3_in, rtpthlp_in, &
            sclrm, sclrp2, sclrprtp, sclrpthlp, &        
            wpsclrp, edsclr_in, err_code, &
            rcm_out, wprcp_out, cloud_frac_out, ice_supersat_frac, &
            rcm_in_layer_out, cloud_cover_out, &
            khzm_out, khzt_out, qclvar_out, thlprcp_out, &
            pdf_params)

         if (do_rainturb) then
            rvm_in = rtm_in - rcm_out 
            call update_xp2_mc_api(nlev+1, dtime, cloud_frac_out, &
            rcm_out, rvm_in, thlm_in, wm_zt, exner, pre_in, pdf_params, &
            rtp2_mc_out, thlp2_mc_out, &
            wprtp_mc_out, wpthlp_mc_out, &
            rtpthlp_mc_out)

            dum1 = (1._r8 - cam_in%landfrac(i))

            ! update turbulent moments based on rain evaporation  
            rtp2_in  = rtp2_in + clubb_rnevap_effic * dum1 * rtp2_mc_out * dtime
            thlp2_in = thlp2_in + clubb_rnevap_effic * dum1 * thlp2_mc_out * dtime  
            wprtp_in = wprtp_in + clubb_rnevap_effic * dum1 * wprtp_mc_out * dtime
            wpthlp_in = wpthlp_in + clubb_rnevap_effic * dum1 * wpthlp_mc_out * dtime
         endif     

         if (do_cldcool) then
         
            rcm_out_zm = zt2zm_api(rcm_out)
            qrl_zm = zt2zm_api(qrl_clubb)
            thlp2_rad_out(:) = 0._r8
            call calculate_thlp2_rad_api(nlev+1, rcm_out_zm, thlprcp_out, qrl_zm, thlp2_rad_out)
            thlp2_in = thlp2_in + thlp2_rad_out * dtime
            thlp2_in = max(thl_tol**2,thlp2_in)
          endif

          !  Check to see if stats should be output, here stats are read into
          !  output arrays to make them conformable to CAM output
          if (l_stats) call stats_end_timestep_clubb(i,out_zt,out_zm,&
                                                     out_radzt,out_radzm,out_sfc)

      enddo  ! end time loop
     
      if (clubb_do_adv) then
         if (macmic_it .eq. cld_macmic_num_steps) then 
            wp2_in=zm2zt_api(wp2_in)   
            wpthlp_in=zm2zt_api(wpthlp_in)
            wprtp_in=zm2zt_api(wprtp_in)
            up2_in=zm2zt_api(up2_in)
            vp2_in=zm2zt_api(vp2_in)
            thlp2_in=zm2zt_api(thlp2_in)
            rtp2_in=zm2zt_api(rtp2_in)
            rtpthlp_in=zm2zt_api(rtpthlp_in) 

            do k=1,nlev+1
               thlp2_in(k)=max(thl_tol**2,thlp2_in(k))
               rtp2_in(k)=max(rt_tol**2,rtp2_in(k))
               wp2_in(k)=max(w_tol_sqd,wp2_in(k))
               up2_in(k)=max(w_tol_sqd,up2_in(k))
               vp2_in(k)=max(w_tol_sqd,vp2_in(k))
            enddo
         endif
      endif

      !  Arrays need to be "flipped" to CAM grid 
      do k=1,nlev+1

          um(i,pverp-k+1)   = um_in(k)
          vm(i,pverp-k+1)   = vm_in(k)
          upwp(i,pverp-k+1) = upwp_in(k)
          vpwp(i,pverp-k+1) = vpwp_in(k)
          up2(i,pverp-k+1)  = up2_in(k)
          vp2(i,pverp-k+1)  = vp2_in(k)
          thlm(i,pverp-k+1) = thlm_in(k)
          rtm(i,pverp-k+1)  = rtm_in(k)
          wprtp(i,pverp-k+1)= wprtp_in(k)
          wpthlp(i,pverp-k+1)   = wpthlp_in(k)
          wp2(i,pverp-k+1)  = wp2_in(k)
          wp3(i,pverp-k+1)  = wp3_in(k)
          rtp2(i,pverp-k+1) = rtp2_in(k)
          thlp2(i,pverp-k+1)= thlp2_in(k)
          rtpthlp(i,pverp-k+1)  = rtpthlp_in(k)
          rcm(i,pverp-k+1)  = rcm_out(k)
          wprcp(i,pverp-k+1)= wprcp_out(k)
          cloud_frac(i,pverp-k+1)   = min(cloud_frac_out(k),1._r8)
          rcm_in_layer(i,pverp-k+1) = rcm_in_layer_out(k)
          zt_out(i,pverp-k+1)       = zt_g(k)
          zi_out(i,pverp-k+1)       = zi_g(k)
          khzm(i,pverp-k+1) = khzm_out(k)
          qclvar(i,pverp-k+1)       = min(1._r8,qclvar_out(k))

          do ixind=1,edsclr_dim
              edsclr_out(pverp-k+1,ixind) = edsclr_in(k,ixind)
          enddo

      enddo

      ! Values to use above top_lev, for variables that have not already been
      ! set up there. These are mostly fill values that should not actually be
      ! used in the run, but may end up in diagnostic output.
      upwp(i,:top_lev-1) = 0._r8
      vpwp(i,:top_lev-1) = 0._r8
      rcm(i,:top_lev-1) = 0._r8
      wprcp(i,:top_lev-1) = 0._r8
      cloud_frac(i,:top_lev-1) = 0._r8
      rcm_in_layer(i,:top_lev-1) = 0._r8
      zt_out(i,:top_lev-1) = 0._r8
      zi_out(i,:top_lev-1) = 0._r8
      khzm(i,:top_lev-1) = 0._r8
      qclvar(i,:top_lev-1) = 2._r8

      ! enforce zero tracer tendencies above the top_lev level -- no change
      icnt=0
      do ixind=1,pcnst
         if (lq(ixind)) then 
            icnt=icnt+1
            edsclr_out(:top_lev-1,icnt) = state1%q(i,:top_lev-1,ixind)
         end if
      enddo

      !  Fill up arrays needed for McICA.  Note we do not want the ghost point,
      !   thus why the second loop is needed.
     
      zi_out(i,1) = 0._r8

      ! Section below is concentrated on energy fixing for conservation.
      !   There are two steps to this process.  The first is to remove any tendencies
      !   CLUBB may have produced above where it is active due to roundoff. 
      !   The second is to provider a fixer because CLUBB and CAM's thermodynamic
      !   variables are different.  

      ! Initialize clubbtop with the chemistry topopause top, to prevent CLUBB from
      !  firing up in the stratosphere 
      clubbtop = troplev(i)
      do while ((rtp2(i,clubbtop) .le. 1.e-15_r8 .and. rcm(i,clubbtop) .eq. 0._r8) .and. clubbtop .lt. pver-1)
         clubbtop = clubbtop + 1
      enddo    
      
      ! Compute static energy using CLUBB's variables
      do k=1,pver
         clubb_s(k) = cpair*((thlm(i,k)+(latvap/cpair)*rcm(i,k))/exner_clubb(i,k))+ &
                      gravit*state1%zm(i,k)+state1%phis(i)      
      enddo      
      
      !  Compute integrals above layer where CLUBB is active
      se_upper_a = 0._r8   ! energy in layers above where CLUBB is active AFTER CLUBB is called
      se_upper_b = 0._r8   ! energy in layers above where CLUBB is active BEFORE CLUBB is called
      tw_upper_a = 0._r8   ! total water in layers above where CLUBB is active AFTER CLUBB is called
      tw_upper_b = 0._r8   ! total water in layers above where CLUBB is active BEFORE CLUBB is called
      do k=1,clubbtop
        se_upper_a = se_upper_a + (clubb_s(k)+0.5_r8*(um(i,k)**2+vm(i,k)**2)+(latvap+latice)* &
                     (rtm(i,k)-rcm(i,k))+(latice)*rcm(i,k))*state1%pdel(i,k)/gravit
        se_upper_b = se_upper_b + (state1%s(i,k)+0.5_r8*(state1%u(i,k)**2+state1%v(i,k)**2)+(latvap+latice)* &
                     state1%q(i,k,ixq)+(latice)*state1%q(i,k,ixcldliq))*state1%pdel(i,k)/gravit
        tw_upper_a = tw_upper_a + rtm(i,k)*state1%pdel(i,k)/gravit
        tw_upper_b = tw_upper_b + (state1%q(i,k,ixq)+state1%q(i,k,ixcldliq))*state1%pdel(i,k)/gravit
      enddo
      
      ! Compute the disbalance of total energy and water in upper levels,
      !   divide by the thickness in the lower atmosphere where we will 
      !   evenly distribute this disbalance
      se_upper_diss = (se_upper_a - se_upper_b)/(state1%pint(i,pverp)-state1%pint(i,clubbtop+1))
      tw_upper_diss = (tw_upper_a - tw_upper_b)/(state1%pint(i,pverp)-state1%pint(i,clubbtop+1))
      
      ! Perform a test to see if there will be any negative RTM errors
      !  in the column.  If so, apply the disbalance to the surface
      apply_to_surface = .false.
      if (tw_upper_diss .lt. 0._r8) then
        do k=clubbtop+1,pver
          rtm_test = (rtm(i,k) + tw_upper_diss*gravit) - rcm(i,k)
          if (rtm_test .lt. 0._r8) then
            apply_to_surface = .true.
          endif
        enddo
      endif
      
      if (apply_to_surface) then
        tw_upper_diss = (tw_upper_a - tw_upper_b)/(state1%pint(i,pverp)-state1%pint(i,pver))
        se_upper_diss = (se_upper_a - se_upper_b)/(state1%pint(i,pverp)-state1%pint(i,pver))
        rtm(i,pver) = rtm(i,pver) + tw_upper_diss*gravit
        if (apply_to_heat) clubb_s(pver) = clubb_s(pver) + se_upper_diss*gravit
      else
        ! Apply the disbalances above to layers where CLUBB is active
        do k=clubbtop+1,pver
          rtm(i,k) = rtm(i,k) + tw_upper_diss*gravit
        if (apply_to_heat) clubb_s(k) = clubb_s(k) + se_upper_diss*gravit
        enddo
      endif      
      
      ! Essentially "zero" out tendencies in the layers above where CLUBB is active
      do k=1,clubbtop
        if (apply_to_heat) clubb_s(k) = state1%s(i,k)
        rcm(i,k) = state1%q(i,k,ixcldliq)
        rtm(i,k) = state1%q(i,k,ixq) + rcm(i,k)
      enddo           

      ! Compute integrals for static energy, kinetic energy, water vapor, and liquid water
      ! after CLUBB is called.  
      se_a = 0._r8
      ke_a = 0._r8
      wv_a = 0._r8
      wl_a = 0._r8
      
      ! Do the same as above, but for before CLUBB was called.
      se_b = 0._r8
      ke_b = 0._r8
      wv_b = 0._r8
      wl_b = 0._r8            
      do k=1,pver
         se_a(i) = se_a(i) + clubb_s(k)*state1%pdel(i,k)/gravit
         ke_a(i) = ke_a(i) + 0.5_r8*(um(i,k)**2+vm(i,k)**2)*state1%pdel(i,k)/gravit
         wv_a(i) = wv_a(i) + (rtm(i,k)-rcm(i,k))*state1%pdel(i,k)/gravit
         wl_a(i) = wl_a(i) + (rcm(i,k))*state1%pdel(i,k)/gravit
 
         se_b(i) = se_b(i) + state1%s(i,k)*state1%pdel(i,k)/gravit
         ke_b(i) = ke_b(i) + 0.5_r8*(state1%u(i,k)**2+state1%v(i,k)**2)*state1%pdel(i,k)/gravit
         wv_b(i) = wv_b(i) + state1%q(i,k,ixq)*state1%pdel(i,k)/gravit
         wl_b(i) = wl_b(i) + state1%q(i,k,ixcldliq)*state1%pdel(i,k)/gravit 
      enddo
     
      ! Based on these integrals, compute the total energy before and after CLUBB call
      te_a(i) = se_a(i) + ke_a(i) + (latvap+latice)*wv_a(i)+latice*wl_a(i)
      te_b(i) = se_b(i) + ke_b(i) + (latvap+latice)*wv_b(i)+latice*wl_b(i)
      
      ! Take into account the surface fluxes of heat and moisture
      !  Use correct qflux from cam_in, not lhf/latvap as was done previously
      te_b(i) = te_b(i)+(cam_in%shf(i)+cam_in%cflx(i,1)*(latvap+latice))*hdtime      

      ! Compute the disbalance of total energy, over depth where CLUBB is active
      se_dis = (te_a(i) - te_b(i))/(state1%pint(i,pverp)-state1%pint(i,clubbtop+1))

      ! Fix the total energy coming out of CLUBB so it achieves enery conservation.
      ! Apply this fixer throughout the column evenly, but only at layers where 
      ! CLUBB is active.
      !
      ! NOTE: The energy fixer seems to cause the climate to change significantly
      ! when using specified dynamics, so allow this to be turned off via a namelist
      ! variable.
      if (clubb_do_energyfix) then
        do k=clubbtop+1,pver
           clubb_s(k) = clubb_s(k) - se_dis*gravit
        enddo
      endif           

      !  Now compute the tendencies of CLUBB to CAM, note that pverp is the ghost point
      !  for all variables and therefore is never called in this loop
      do k=1,pver
  
         ptend_loc%u(i,k)   = (um(i,k)-state1%u(i,k))/hdtime             ! east-west wind
         ptend_loc%v(i,k)   = (vm(i,k)-state1%v(i,k))/hdtime             ! north-south wind
         ptend_loc%q(i,k,ixq) = (rtm(i,k)-rcm(i,k)-state1%q(i,k,ixq))/hdtime ! water vapor
         ptend_loc%q(i,k,ixcldliq) = (rcm(i,k)-state1%q(i,k,ixcldliq))/hdtime   ! Tendency of liquid water
         ptend_loc%s(i,k)   = (clubb_s(k)-state1%s(i,k))/hdtime          ! Tendency of static energy

         if (clubb_do_adv) then
            if (macmic_it .eq. cld_macmic_num_steps) then

               ! Here add a constant to moments which can be either positive or 
               !  negative.  This is to prevent clipping when dynamics tries to
               !  make all constituents positive 
               wp3(i,k) = wp3(i,k) + wp3_const
               rtpthlp(i,k) = rtpthlp(i,k) + rtpthlp_const
               wpthlp(i,k) = wpthlp(i,k) + wpthlp_const
               wprtp(i,k) = wprtp(i,k) + wprtp_const

               ptend_loc%q(i,k,ixthlp2)=(thlp2(i,k)-state1%q(i,k,ixthlp2))/hdtime ! THLP Variance
               ptend_loc%q(i,k,ixrtp2)=(rtp2(i,k)-state1%q(i,k,ixrtp2))/hdtime ! RTP Variance
               ptend_loc%q(i,k,ixrtpthlp)=(rtpthlp(i,k)-state1%q(i,k,ixrtpthlp))/hdtime ! RTP THLP covariance
               ptend_loc%q(i,k,ixwpthlp)=(wpthlp(i,k)-state1%q(i,k,ixwpthlp))/hdtime ! WPTHLP 
               ptend_loc%q(i,k,ixwprtp)=(wprtp(i,k)-state1%q(i,k,ixwprtp))/hdtime ! WPRTP
               ptend_loc%q(i,k,ixwp2)=(wp2(i,k)-state1%q(i,k,ixwp2))/hdtime ! WP2
               ptend_loc%q(i,k,ixwp3)=(wp3(i,k)-state1%q(i,k,ixwp3))/hdtime ! WP3
               ptend_loc%q(i,k,ixup2)=(up2(i,k)-state1%q(i,k,ixup2))/hdtime ! UP2
               ptend_loc%q(i,k,ixvp2)=(vp2(i,k)-state1%q(i,k,ixvp2))/hdtime ! VP2
            else
               ptend_loc%q(i,k,ixthlp2)=0._r8
               ptend_loc%q(i,k,ixrtp2)=0._r8
               ptend_loc%q(i,k,ixrtpthlp)=0._r8
               ptend_loc%q(i,k,ixwpthlp)=0._r8
               ptend_loc%q(i,k,ixwprtp)=0._r8
               ptend_loc%q(i,k,ixwp2)=0._r8
               ptend_loc%q(i,k,ixwp3)=0._r8
               ptend_loc%q(i,k,ixup2)=0._r8
               ptend_loc%q(i,k,ixvp2)=0._r8 
            endif

         endif

         !  Apply tendencies to ice mixing ratio, liquid and ice number, and aerosol constituents.
         !  Loading up this array doesn't mean the tendencies are applied.  
         ! edsclr_out is compressed with just the constituents being used, ptend and state are not compressed

         icnt=0
         do ixind=1,pcnst
            if (lq(ixind)) then
               icnt=icnt+1
               if ((ixind /= ixq)       .and. (ixind /= ixcldliq) .and.&
                   (ixind /= ixthlp2)   .and. (ixind /= ixrtp2)   .and.&
                   (ixind /= ixrtpthlp) .and. (ixind /= ixwpthlp) .and.&
                   (ixind /= ixwprtp)   .and. (ixind /= ixwp2)    .and.&
                   (ixind /= ixwp3)     .and. (ixind /= ixup2)    .and. (ixind /= ixvp2) ) then
                       ptend_loc%q(i,k,ixind) = (edsclr_out(k,icnt)-state1%q(i,k,ixind))/hdtime ! transported constituents 
               end if
            end if
         enddo

      enddo
      

   enddo  ! end column loop
       END DO   
       CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock) 
       IF (kgen_cache_control(1) == 0) THEN 
           kgen_stop_clock = kgen_stop_clock   + kgen_cache_control(1) 
       END IF   
       kgen_measure = 1.0D6*(kgen_stop_clock - kgen_start_clock)/DBLE(kgen_rate_clock*KGEN_MAXITER) 
#ifdef _MPI 
       CALL mpi_allreduce(kgen_measure, gkgen_measure, 1, mpi_real8, mpi_max, mpi_comm_world, kgen_ierr) 
       kgen_measure = gkgen_measure 
#endif 
       IF (check_status%rank==0) THEN 
           WRITE (*, *) "clubb_tend_cam : Time per call (usec): ", kgen_measure 
       END IF   
   END IF   
   IF (kgen_warmupstage) THEN 
   END IF   
   IF (kgen_evalstage) THEN 
   END IF   
!$kgen end_callsite


   ! Add constant to ghost point so that output is not corrupted 


   ! ------------------------------------------------- !
   ! End column computation of CLUBB, begin to apply   !
   ! and compute output, etc                           !
   ! ------------------------------------------------- !
   !  Output CLUBB tendencies 


   !  Update physics tendencies     

    ! Due to the order of operation of CLUBB, which closes on liquid first, 
    ! then advances it's predictive equations second, this can lead to 
    ! RHliq > 1 directly before microphysics is called.  Therefore, we use 
    ! ice_macro_tend to enforce RHliq <= 1 everywhere before microphysics is called. 
 
 

   ! ------------------------------------------------------------ !
   ! ------------------------------------------------------------ ! 
   ! ------------------------------------------------------------ !
   ! The rest of the code deals with diagnosing variables         !
   ! for microphysics/radiation computation and macrophysics      !
   ! ------------------------------------------------------------ !
   ! ------------------------------------------------------------ !
   ! ------------------------------------------------------------ !
   ! --------------------------------------------------------------------------------- !  
   !  COMPUTE THE ICE CLOUD DETRAINMENT                                                !
   !  Detrainment of convective condensate into the environment or stratiform cloud    !
   ! --------------------------------------------------------------------------------- ! 
   !  Initialize the shallow convective detrainment rate, will always be zero
 

   


   


   

   
  
   ! ------------------------------------------------- !
   ! Diagnose relative cloud water variance            !
   ! ------------------------------------------------- !


   


   ! ------------------------------------------------- !
   ! Optional Accretion enhancement factor             !
   ! ------------------------------------------------- !   
   

   ! ------------------------------------------------- !
   ! Diagnose some output variables                    !
   ! ------------------------------------------------- !
   !  density
   


   

   ! --------------------------------------------------------------------------------- ! 
   !  Diagnose some quantities that are computed in macrop_tend here.                  !
   !  These are inputs required for the microphysics calculation.                      !
   !                                                                                   !
   !  FIRST PART COMPUTES THE STRATIFORM CLOUD FRACTION FROM CLUBB CLOUD FRACTION      !
   ! --------------------------------------------------------------------------------- ! 
   !  initialize variables 
   
 
 

   ! --------------------------------------------------------------------------------- !  
   !  THIS PART COMPUTES CONVECTIVE AND DEEP CONVECTIVE CLOUD FRACTION                 !
   ! --------------------------------------------------------------------------------- ! 
 
 
 

   

   ! --------------------------------------------------------------------------------- !  
   !  COMPUTE THE ICE CLOUD FRACTION PORTION                                           !
   !  use the aist_vector function to compute the ice cloud fraction                   !
   ! --------------------------------------------------------------------------------- !  
   


   ! --------------------------------------------------------------------------------- !  
   !  THIS PART COMPUTES THE LIQUID STRATUS FRACTION                                   !
   !                                                                                   !
   !  For now leave the computation of ice stratus fraction from macrop_driver intact  !
   !  because CLUBB does nothing with ice.  Here I simply overwrite the liquid stratus ! 
   !  fraction that was coded in macrop_driver                                         !
   ! --------------------------------------------------------------------------------- !  
   !  Recompute net stratus fraction using maximum over-lapping assumption, as done
   !  in macrophysics code, using alst computed above and aist read in from physics buffer
  
 
   

   !  Probably need to add deepcu cloud fraction to the cloud fraction array, else would just 
   !  be outputting the shallow convective cloud fraction 
   


   ! --------------------------------------------------------------------------------- !  
   !  DIAGNOSE THE PBL DEPTH                                                           !
   !  this is needed for aerosol code                                                  !
   ! --------------------------------------------------------------------------------- !   
   
    

   ! diagnose surface friction and obukhov length (inputs to diagnose PBL depth)
 
   ! use correct qflux from coupler
   
   
   !  Compute PBL depth according to Holtslag-Boville Scheme

   !  Output the PBL depth

   ! Assign the first pver levels of cloud_frac back to cld
 
   ! --------------------------------------------------------------------------------- !   
   !  END CLOUD FRACTION DIAGNOSIS, begin to store variables back into buffer          !
   ! --------------------------------------------------------------------------------- !  
   !  Output calls of variables goes here

 


   !  Output CLUBB history here


   
     
   CONTAINS 
     
   !read state subroutine for kr_kgen_clubb_tend_cam_subp2 
   SUBROUTINE kr_kgen_clubb_tend_cam_subp2(var, kgen_unit, printname, printvar) 
       TYPE(pdf_parameter), INTENT(INOUT), DIMENSION(:) :: var 
       INTEGER, INTENT(IN) :: kgen_unit 
       CHARACTER(LEN=*), INTENT(IN) :: printname 
       LOGICAL, INTENT(IN), OPTIONAL :: printvar 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
       INTEGER :: idx1 
       INTEGER, DIMENSION(2,1) :: kgen_bound 
         
       READ (UNIT = kgen_unit) kgen_istrue 
       IF (kgen_istrue) THEN 
           READ (UNIT = kgen_unit) kgen_bound(1, 1) 
           READ (UNIT = kgen_unit) kgen_bound(2, 1) 
           DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
               IF (PRESENT( printvar ) .AND. printvar) THEN 
                   CALL kr_pdf_parameter_module_pdf_parameter(var(idx1), kgen_unit, printname // "(idx1)", .TRUE.) 
               ELSE 
                   CALL kr_pdf_parameter_module_pdf_parameter(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
               END IF   
           END DO   
       END IF   
   END SUBROUTINE kr_kgen_clubb_tend_cam_subp2 
     
   !read state subroutine for kr_clubb_tend_cam_real__r8_dim2_ptr 
   SUBROUTINE kr_clubb_tend_cam_real__r8_dim2_ptr(var, kgen_unit, printname, printvar) 
       REAL(KIND=r8), INTENT(INOUT), POINTER, DIMENSION(:,:) :: var 
       INTEGER, INTENT(IN) :: kgen_unit 
       CHARACTER(LEN=*), INTENT(IN) :: printname 
       LOGICAL, INTENT(IN), OPTIONAL :: printvar 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
       INTEGER :: idx1, idx2 
       INTEGER, DIMENSION(2,2) :: kgen_bound 
         
       READ (UNIT = kgen_unit) kgen_istrue 
       IF (kgen_istrue) THEN 
           IF (ASSOCIATED( var )) THEN 
               NULLIFY (var) 
           END IF   
           READ (UNIT = kgen_unit) kgen_array_sum 
           READ (UNIT = kgen_unit) kgen_bound(1, 1) 
           READ (UNIT = kgen_unit) kgen_bound(2, 1) 
           READ (UNIT = kgen_unit) kgen_bound(1, 2) 
           READ (UNIT = kgen_unit) kgen_bound(2, 2) 
           ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
           READ (UNIT = kgen_unit) var 
           CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
           IF (PRESENT( printvar ) .AND. printvar) THEN 
               WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
           END IF   
       END IF   
   END SUBROUTINE kr_clubb_tend_cam_real__r8_dim2_ptr 
     
   !verify state subroutine for kv_clubb_tend_cam_integer__ 
   RECURSIVE SUBROUTINE kv_clubb_tend_cam_integer__(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       INTEGER, INTENT(IN) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       integer :: diff 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       IF (var == kgenref_var) THEN 
           check_status%numIdentical = check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff = ABS(var - kgenref_var) 
           IF (diff <= kgen_tolerance) THEN 
               check_status%numInTol = check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               check_status%numOutTol = check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 0) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_OUT_TOL 
           END IF   
       END IF   
       IF (check_result == CHECK_IDENTICAL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", 0 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_OUT_TOL) THEN 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", diff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", diff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
   END SUBROUTINE kv_clubb_tend_cam_integer__ 
     
   !verify state subroutine for kv_clubb_tend_cam_real__r8_dim2 
   RECURSIVE SUBROUTINE kv_clubb_tend_cam_real__r8_dim2(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       REAL(KIND=r8), INTENT(IN), DIMENSION(:,:) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       INTEGER :: idx1, idx2 
       INTEGER :: n 
       real(KIND=r8) :: nrmsdiff, rmsdiff 
       real(KIND=r8), ALLOCATABLE :: buf1(:,:), buf2(:,:) 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       IF (ALL(var == kgenref_var)) THEN 
           check_status%numIdentical = check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
           ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
           n = SIZE(var) 
           WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
               buf1 = ((var-kgenref_var)/kgenref_var)**2 
               buf2 = (var-kgenref_var)**2 
           ELSEWHERE 
               buf1 = (var-kgenref_var)**2 
               buf2 = buf1 
           END WHERE   
           nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
           rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
           IF (rmsdiff > kgen_tolerance) THEN 
               check_status%numOutTol = check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 0) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_OUT_TOL 
           ELSE 
               check_status%numInTol = check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           END IF   
       END IF   
       IF (check_result == CHECK_IDENTICAL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                   WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                   WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                   WRITE (*, *) "RMS of difference is ", 0 
                   WRITE (*, *) "Normalized RMS of difference is ", 0 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_OUT_TOL) THEN 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                   WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                   WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                   WRITE (*, *) "RMS of difference is ", rmsdiff 
                   WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                   WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                   WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                   WRITE (*, *) "RMS of difference is ", rmsdiff 
                   WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
   END SUBROUTINE kv_clubb_tend_cam_real__r8_dim2 
     
   !verify state subroutine for kv_clubb_tend_cam_real__r8_dim1 
   RECURSIVE SUBROUTINE kv_clubb_tend_cam_real__r8_dim1(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       REAL(KIND=r8), INTENT(IN), DIMENSION(:) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       INTEGER :: idx1 
       INTEGER :: n 
       real(KIND=r8) :: nrmsdiff, rmsdiff 
       real(KIND=r8), ALLOCATABLE :: buf1(:), buf2(:) 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       IF (ALL(var == kgenref_var)) THEN 
           check_status%numIdentical = check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           ALLOCATE (buf1(SIZE(var,dim=1))) 
           ALLOCATE (buf2(SIZE(var,dim=1))) 
           n = SIZE(var) 
           WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
               buf1 = ((var-kgenref_var)/kgenref_var)**2 
               buf2 = (var-kgenref_var)**2 
           ELSEWHERE 
               buf1 = (var-kgenref_var)**2 
               buf2 = buf1 
           END WHERE   
           nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
           rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
           IF (rmsdiff > kgen_tolerance) THEN 
               check_status%numOutTol = check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 0) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_OUT_TOL 
           ELSE 
               check_status%numInTol = check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           END IF   
       END IF   
       IF (check_result == CHECK_IDENTICAL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                   WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                   WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                   WRITE (*, *) "RMS of difference is ", 0 
                   WRITE (*, *) "Normalized RMS of difference is ", 0 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_OUT_TOL) THEN 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                   WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                   WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                   WRITE (*, *) "RMS of difference is ", rmsdiff 
                   WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                   WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                   WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                   WRITE (*, *) "RMS of difference is ", rmsdiff 
                   WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
   END SUBROUTINE kv_clubb_tend_cam_real__r8_dim1 
     
   !verify state subroutine for kv_clubb_tend_cam_real__r8 
   RECURSIVE SUBROUTINE kv_clubb_tend_cam_real__r8(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       REAL(KIND=r8), INTENT(IN) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       real(KIND=r8) :: diff 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       IF (var == kgenref_var) THEN 
           check_status%numIdentical = check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff = ABS(var - kgenref_var) 
           IF (diff <= kgen_tolerance) THEN 
               check_status%numInTol = check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               check_status%numOutTol = check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 0) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_OUT_TOL 
           END IF   
       END IF   
       IF (check_result == CHECK_IDENTICAL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", 0 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_OUT_TOL) THEN 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", diff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", diff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
   END SUBROUTINE kv_clubb_tend_cam_real__r8 
     
   !verify state subroutine for kv_kgen_clubb_tend_cam_subp1 
   RECURSIVE SUBROUTINE kv_kgen_clubb_tend_cam_subp1(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       TYPE(check_t) :: comp_check_status 
       TYPE(pdf_parameter), INTENT(IN), DIMENSION(:) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       INTEGER :: idx1 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
       DO   idx1=LBOUND(var,1), UBOUND(var,1) 
           CALL kv_pdf_parameter_module_pdf_parameter(trim(adjustl(varname)), comp_check_status, var(idx1), kgenref_var(idx1)) 
       END DO   
       IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
           check_status%numIdentical = check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE IF (comp_check_status%numOutTol > 0) THEN 
           check_status%numOutTol = check_status%numOutTol + 1 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
               END IF   
           END IF   
           check_result = CHECK_OUT_TOL 
       ELSE IF (comp_check_status%numInTol > 0) THEN 
           check_status%numInTol = check_status%numInTol + 1 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
               END IF   
           END IF   
           check_result = CHECK_IN_TOL 
       END IF   
       IF (check_result == CHECK_IDENTICAL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
                   WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                   WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                   WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_OUT_TOL) THEN 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
                   WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                   WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                   WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
                   WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                   WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                   WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
   END SUBROUTINE kv_kgen_clubb_tend_cam_subp1 
     
   !verify state subroutine for kv_clubb_tend_cam_real__r8_dim2_ptr 
   RECURSIVE SUBROUTINE kv_clubb_tend_cam_real__r8_dim2_ptr(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       REAL(KIND=r8), pointer, INTENT(IN), DIMENSION(:,:) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       INTEGER :: idx1, idx2 
       INTEGER :: n 
       real(KIND=r8) :: nrmsdiff, rmsdiff 
       real(KIND=r8), ALLOCATABLE :: buf1(:,:), buf2(:,:) 
         
       IF (ASSOCIATED(var)) THEN 
           check_status%numTotal = check_status%numTotal + 1 
             
           IF (ALL(var == kgenref_var)) THEN 
               check_status%numIdentical = check_status%numIdentical + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                   END IF   
               END IF   
               check_result = CHECK_IDENTICAL 
           ELSE 
               ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
               ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
               n = SIZE(var) 
               WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
                   buf1 = ((var-kgenref_var)/kgenref_var)**2 
                   buf2 = (var-kgenref_var)**2 
               ELSEWHERE 
                   buf1 = (var-kgenref_var)**2 
                   buf2 = buf1 
               END WHERE   
               nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
               rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
               IF (rmsdiff > kgen_tolerance) THEN 
                   check_status%numOutTol = check_status%numOutTol + 1 
                   IF (kgen_verboseLevel > 0) THEN 
                       IF (check_status%rank == 0) THEN 
                           WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                       END IF   
                   END IF   
                   check_result = CHECK_OUT_TOL 
               ELSE 
                   check_status%numInTol = check_status%numInTol + 1 
                   IF (kgen_verboseLevel > 1) THEN 
                       IF (check_status%rank == 0) THEN 
                           WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                       END IF   
                   END IF   
                   check_result = CHECK_IN_TOL 
               END IF   
           END IF   
           IF (check_result == CHECK_IDENTICAL) THEN 
               IF (kgen_verboseLevel > 2) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                       WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                       WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                       WRITE (*, *) "RMS of difference is ", 0 
                       WRITE (*, *) "Normalized RMS of difference is ", 0 
                       WRITE (*, *) "" 
                   END IF   
               END IF   
           ELSE IF (check_result == CHECK_OUT_TOL) THEN 
               IF (kgen_verboseLevel > 0) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                       WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                       WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                       WRITE (*, *) "RMS of difference is ", rmsdiff 
                       WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                       WRITE (*, *) "" 
                   END IF   
               END IF   
           ELSE IF (check_result == CHECK_IN_TOL) THEN 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                       WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                       WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                       WRITE (*, *) "RMS of difference is ", rmsdiff 
                       WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                       WRITE (*, *) "" 
                   END IF   
               END IF   
           END IF   
             
       END IF   
   END SUBROUTINE kv_clubb_tend_cam_real__r8_dim2_ptr 
     
   !verify state subroutine for kv_clubb_tend_cam_logical__ 
   RECURSIVE SUBROUTINE kv_clubb_tend_cam_logical__(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       LOGICAL, INTENT(IN) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       logical :: diff 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       IF (var .EQV. kgenref_var) THEN 
           check_status%numIdentical = check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           check_status%numOutTol = check_status%numOutTol + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_OUT_TOL 
       END IF   
       IF (check_result == CHECK_IDENTICAL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "NOT IMPLEMENTED" 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_OUT_TOL) THEN 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "NOT IMPLEMENTED" 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "NOT IMPLEMENTED" 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
   END SUBROUTINE kv_clubb_tend_cam_logical__ 
     
  END SUBROUTINE clubb_tend_cam 
  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !
! ----------------------------------------------------------------------
! DISCLAIMER : this code appears to be correct but has not been
!              very thouroughly tested. If you do notice any
!              anomalous behaviour then please contact Andy and/or
!              Bjorn
! Function diag_ustar:  returns value of ustar using the below
! similarity functions and a specified buoyancy flux (bflx) given in
! kinematic units
! phi_m (zeta > 0) =  (1 + am * zeta)
! phi_m (zeta < 0) =  (1 - bm * zeta)^(-1/4)
! where zeta = z/lmo and lmo = (theta_rev/g*vonk) * (ustar^2/tstar)
! Ref: Businger, 1973, Turbulent Transfer in the Atmospheric Surface
! Layer, in Workshop on Micormeteorology, pages 67-100.
! Code writen March, 1999 by Bjorn Stevens
    

!
!
!
!
!
!
!

real(r8) function diag_ustar( z, bflx, wnd, z0 ) 

    USE shr_const_mod, ONLY: shr_const_karman, shr_const_pi, shr_const_g 

implicit none

real(r8), parameter      :: am   =  4.8_r8   !   "          "         "
real(r8), parameter      :: bm   = 19.3_r8  !   "          "         "

real(r8), parameter      :: grav = shr_const_g
real(r8), parameter      :: vonk = shr_const_karman
real(r8), parameter      :: pi   = shr_const_pi

real(r8), intent (in)    :: z             ! height where u locates
real(r8), intent (in)    :: bflx          ! surface buoyancy flux (m^2/s^3)
real(r8), intent (in)    :: wnd           ! wind speed at z
real(r8), intent (in)    :: z0            ! momentum roughness height


integer :: iterate
real(r8)    :: lnz, klnz, c1, x, psi1, zeta, lmo, ustar

lnz   = log( z / z0 )
klnz  = vonk/lnz
c1    = pi / 2.0_r8 - 3.0_r8*log( 2.0_r8 )

ustar =  wnd*klnz
if (abs(bflx) > 1.e-6_r8) then
   do iterate=1,4

      if (ustar > 1.e-6_r8) then
         lmo   = -ustar**3 / ( vonk * bflx )
         zeta  = z/lmo
         if (zeta > 0._r8) then
            ustar =  vonk*wnd  /(lnz + am*zeta)
         else
            x     = sqrt( sqrt( 1.0_r8 - bm*zeta ) )
            psi1  = 2._r8*log( 1.0_r8+x ) + log( 1.0_r8+x*x ) - 2._r8*atan( x ) + c1
            ustar = wnd*vonk/(lnz - psi1)
         end if

      endif

   end do
end if


diag_ustar = ustar

return


end function diag_ustar
  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !


  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !
    !-----------------------------------------------------------------------
  


  
  subroutine stats_end_timestep_clubb(thecol,out_zt,out_zm,out_radzt,out_radzm,out_sfc)
    !     Description: Called when the stats timestep has ended. This subroutine
    !     is responsible for calling statistics to be written to the output
    !     format.
    !-----------------------------------------------------------------------


      USE shr_infnan_mod, ONLY: is_nan => shr_infnan_isnan 

      USE constants_clubb, ONLY: fstderr 

      USE stats_variables, ONLY: stats_zt, stats_zm, stats_rad_zt, stats_rad_zm, stats_sfc, l_stats_last, stats_tsamp, &
      &stats_tout, l_output_rad_files 

      USE error_code, ONLY: clubb_at_least_debug_level 

      USE cam_abortutils, ONLY: endrun 

    implicit none


    integer :: thecol
    
    real(r8), intent(inout) :: out_zt(:,:,:)     ! (pcols,pverp,zt%nn)
    real(r8), intent(inout) :: out_zm(:,:,:)     ! (pcols,pverp,zt%nn)
    real(r8), intent(inout) :: out_radzt(:,:,:)  ! (pcols,pverp,rad_zt%nn)
    real(r8), intent(inout) :: out_radzm(:,:,:)  ! (pcols,pverp,rad_zm%nn)
    real(r8), intent(inout) :: out_sfc(:,:,:)    ! (pcols,1,sfc%nn)
    ! Local Variables


    integer :: i, k
    logical :: l_error
    !  Check if it is time to write to file


    if ( .not. l_stats_last ) return
    !  Initialize

    l_error = .false.
    !  Look for errors by checking the number of sampling points
    !  for each variable in the zt statistics at each vertical level.

    do i = 1, stats_zt%num_output_fields
      do k = 1, stats_zt%kk

        if ( stats_zt%accum_num_samples(1,1,k,i) /= 0 .and.  &
             stats_zt%accum_num_samples(1,1,k,i) /= floor(stats_tout/stats_tsamp) ) then

          l_error = .true.  ! This will stop the run

          if ( clubb_at_least_debug_level( 1 ) ) then
            write(fstderr,*) 'Possible sampling error for variable ',  &
                             trim(stats_zt%file%var(i)%name), ' in zt ',  &
                             'at k = ', k,  &
                             '; stats_zt%accum_num_samples(',k,',',i,') = ', stats_zt%accum_num_samples(1,1,k,i)
          endif

        endif

      enddo
    enddo
    !  Look for errors by checking the number of sampling points
    !  for each variable in the zm statistics at each vertical level.

    do i = 1, stats_zm%num_output_fields
      do k = 1, stats_zm%kk

        if ( stats_zm%accum_num_samples(1,1,k,i) /= 0 .and.  &
             stats_zm%accum_num_samples(1,1,k,i) /= floor(stats_tout/stats_tsamp) ) then

          l_error = .true.  ! This will stop the run

          if ( clubb_at_least_debug_level( 1 ) ) then
            write(fstderr,*) 'Possible sampling error for variable ',  &
                             trim(stats_zm%file%var(i)%name), ' in zm ',  &
                             'at k = ', k,  &
                             '; stats_zm%accum_num_samples(',k,',',i,') = ', stats_zm%accum_num_samples(1,1,k,i)
          endif

        endif

      enddo
    enddo

    if (l_output_rad_files) then
      !  Look for errors by checking the number of sampling points
      !  for each variable in the rad_zt statistics at each vertical level.
      do i = 1, stats_rad_zt%num_output_fields
        do k = 1, stats_rad_zt%kk

          if ( stats_rad_zt%accum_num_samples(1,1,k,i) /= 0 .and.  &
               stats_rad_zt%accum_num_samples(1,1,k,i) /= floor(stats_tout/stats_tsamp) ) then

            l_error = .true.  ! This will stop the run

            if ( clubb_at_least_debug_level( 1 ) ) then
              write(fstderr,*) 'Possible sampling error for variable ',  &
                               trim(stats_rad_zt%file%var(i)%name), ' in rad_zt ',  &
                               'at k = ', k,  &
                               '; stats_rad_zt%accum_num_samples(',k,',',i,') = ', stats_rad_zt%accum_num_samples(1,1,k,i)
            endif

          endif

        enddo
      enddo
      !  Look for errors by checking the number of sampling points
      !  for each variable in the rad_zm statistics at each vertical level.
    
      do i = 1, stats_rad_zm%num_output_fields
        do k = 1, stats_rad_zm%kk

          if ( stats_rad_zm%accum_num_samples(1,1,k,i) /= 0 .and.  &
               stats_rad_zm%accum_num_samples(1,1,k,i) /= floor(stats_tout/stats_tsamp) ) then

            l_error = .true.  ! This will stop the run

            if ( clubb_at_least_debug_level( 1 ) ) then
              write(fstderr,*) 'Possible sampling error for variable ',  &
                               trim(stats_rad_zm%file%var(i)%name), ' in rad_zm ',  &
                               'at k = ', k,  &
                               '; stats_rad_zm%accum_num_samples(',k,',',i,') = ', stats_rad_zm%accum_num_samples(1,1,k,i)
            endif

          endif

        enddo
      enddo
    end if ! l_output_rad_files 
    !  Look for errors by checking the number of sampling points
    !  for each variable in the sfc statistics at each vertical level.

    do i = 1, stats_sfc%num_output_fields
      do k = 1, stats_sfc%kk

        if ( stats_sfc%accum_num_samples(1,1,k,i) /= 0 .and.  &
             stats_sfc%accum_num_samples(1,1,k,i) /= floor(stats_tout/stats_tsamp) ) then

          l_error = .true.  ! This will stop the run

          if ( clubb_at_least_debug_level( 1 ) ) then
            write(fstderr,*) 'Possible sampling error for variable ',  &
                             trim(stats_sfc%file%var(i)%name), ' in sfc ',  &
                             'at k = ', k,  &
                             '; stats_sfc%accum_num_samples(',k,',',i,') = ', stats_sfc%accum_num_samples(1,1,k,i)
          endif

        endif

      enddo
    enddo
    !  Stop the run if errors are found.

    if ( l_error ) then
       write(fstderr,*) 'Possible statistical sampling error'
       write(fstderr,*) 'For details, set debug_level to a value of at ',  &
                        'least 1 in the appropriate model.in file.'
       call endrun ('stats_end_timestep:  error(s) found')
    endif
    !  Compute averages

    call stats_avg( stats_zt%kk, stats_zt%num_output_fields, stats_zt%accum_field_values, stats_zt%accum_num_samples )
    call stats_avg( stats_zm%kk, stats_zm%num_output_fields, stats_zm%accum_field_values, stats_zm%accum_num_samples )
    if (l_output_rad_files) then
      call stats_avg( stats_rad_zt%kk, stats_rad_zt%num_output_fields, stats_rad_zt%accum_field_values, &
                      stats_rad_zt%accum_num_samples )
      call stats_avg( stats_rad_zm%kk, stats_rad_zm%num_output_fields, stats_rad_zm%accum_field_values, &
                      stats_rad_zm%accum_num_samples )
    end if
    call stats_avg( stats_sfc%kk, stats_sfc%num_output_fields, stats_sfc%accum_field_values, stats_sfc%accum_num_samples )
   !  Here we are not outputting the data, rather reading the stats into 
   !  arrays which are conformable to CAM output.  Also, the data is "flipped"
   !  in the vertical level to be the same as CAM output.    

    do i = 1, stats_zt%num_output_fields
      do k = 1, stats_zt%kk 
         out_zt(thecol,pverp-k+1,i) = stats_zt%accum_field_values(1,1,k,i)
         if(is_nan(out_zt(thecol,k,i))) out_zt(thecol,k,i) = 0.0_r8
      enddo   
    enddo

    do i = 1, stats_zm%num_output_fields
      do k = 1, stats_zt%kk 
         out_zm(thecol,pverp-k+1,i) = stats_zm%accum_field_values(1,1,k,i)
         if(is_nan(out_zm(thecol,k,i))) out_zm(thecol,k,i) = 0.0_r8
      enddo   
    enddo

    if (l_output_rad_files) then 
      do i = 1, stats_rad_zt%num_output_fields
        do k = 1, stats_rad_zt%kk 
          out_radzt(thecol,pverp-k+1,i) = stats_rad_zt%accum_field_values(1,1,k,i)
          if(is_nan(out_radzt(thecol,k,i))) out_radzt(thecol,k,i) = 0.0_r8
        enddo   
      enddo
    
      do i = 1, stats_rad_zm%num_output_fields
        do k = 1, stats_rad_zm%kk 
          out_radzm(thecol,pverp-k+1,i) = stats_rad_zm%accum_field_values(1,1,k,i)
          if(is_nan(out_radzm(thecol,k,i))) out_radzm(thecol,k,i) = 0.0_r8
        enddo   
      enddo
      ! Fill in values above the CLUBB top.

      out_zt(thecol,:top_lev-1,:) = 0.0_r8
      out_zm(thecol,:top_lev-1,:) = 0.0_r8
      out_radzt(thecol,:top_lev-1,:) = 0.0_r8
      out_radzm(thecol,:top_lev-1,:) = 0.0_r8

    endif
    
    do i = 1, stats_sfc%num_output_fields
      out_sfc(thecol,1,i) = stats_sfc%accum_field_values(1,1,1,i)   
      if(is_nan(out_sfc(thecol,1,i))) out_sfc(thecol,1,i) = 0.0_r8
    enddo
    !  Reset sample fields

    call stats_zero( stats_zt%kk, stats_zt%num_output_fields, stats_zt%accum_field_values, &
                     stats_zt%accum_num_samples, stats_zt%l_in_update )
    call stats_zero( stats_zm%kk, stats_zm%num_output_fields, stats_zm%accum_field_values, &
                     stats_zm%accum_num_samples, stats_zm%l_in_update )
    if (l_output_rad_files) then
      call stats_zero( stats_rad_zt%kk, stats_rad_zt%num_output_fields, stats_rad_zt%accum_field_values, &
                       stats_rad_zt%accum_num_samples, stats_rad_zt%l_in_update )
      call stats_zero( stats_rad_zm%kk, stats_rad_zm%num_output_fields, stats_rad_zm%accum_field_values, &
                       stats_rad_zm%accum_num_samples, stats_rad_zm%l_in_update )
    end if
    call stats_zero( stats_sfc%kk, stats_sfc%num_output_fields, stats_sfc%accum_field_values, &
                     stats_sfc%accum_num_samples, stats_sfc%l_in_update )

    return


  end subroutine stats_end_timestep_clubb
  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !
    !-----------------------------------------------------------------------
  
  

  
  subroutine stats_zero( kk, nn, x, n, l_in_update )
    !     Description:
    !     Initialize stats to zero
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: stat_rknd, stat_nknd 


    implicit none
    !  Input

    integer, intent(in) :: kk, nn
    !  Output

    real(kind=stat_rknd),    dimension(1,1,kk,nn), intent(out) :: x
    integer(kind=stat_nknd), dimension(1,1,kk,nn), intent(out) :: n
    logical,                 dimension(1,1,kk,nn), intent(out) :: l_in_update
    !  Zero out arrays


    if ( nn > 0 ) then
       x(:,:,:,:) = 0.0_r8
       n(:,:,:,:) = 0
       l_in_update(:,:,:,:) = .false.
    end if

    return

  end subroutine stats_zero
  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !
    !-----------------------------------------------------------------------
  


  
  subroutine stats_avg( kk, nn, x, n )
    !     Description:
    !     Compute the average of stats fields
    !-----------------------------------------------------------------------

      USE clubb_precision, ONLY: stat_rknd, stat_nknd 

    implicit none
    !  Input

    integer, intent(in) :: nn, kk
    integer(kind=stat_nknd), dimension(1,1,kk,nn), intent(in) :: n
    !  Output

    real(kind=stat_rknd), dimension(1,1,kk,nn), intent(inout)  :: x
    !  Internal


    integer k,m
    !  Compute averages


    do m=1,nn
       do k=1,kk

          if ( n(1,1,k,m) > 0 ) then
             x(1,1,k,m) = x(1,1,k,m) / real( n(1,1,k,m) )
          end if

       end do
    end do

    return

  end subroutine stats_avg


  
  !read state subroutine for kr_externs_in_clubb_intr 
  SUBROUTINE kr_externs_in_clubb_intr(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) do_cldcool 
      READ (UNIT = kgen_unit) clubb_rnevap_effic 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) lq 
      END IF   
      READ (UNIT = kgen_unit) do_rainturb 
      READ (UNIT = kgen_unit) do_expldiff 
      READ (UNIT = kgen_unit) clubb_do_adv 
      READ (UNIT = kgen_unit) clubb_do_energyfix 
      READ (UNIT = kgen_unit) edsclr_dim 
      READ (UNIT = kgen_unit) ixrtpthlp 
      READ (UNIT = kgen_unit) ixwprtp 
      READ (UNIT = kgen_unit) ixwp3 
      READ (UNIT = kgen_unit) ixwp2 
      READ (UNIT = kgen_unit) ixvp2 
      READ (UNIT = kgen_unit) ixwpthlp 
      READ (UNIT = kgen_unit) ixrtp2 
      READ (UNIT = kgen_unit) ixup2 
      READ (UNIT = kgen_unit) ixthlp2 
      CALL kr_clubb_intr_real__r8_dim3(out_zm, kgen_unit, "out_zm", .FALSE.) 
      CALL kr_clubb_intr_real__r8_dim3(out_zt, kgen_unit, "out_zt", .FALSE.) 
      CALL kr_clubb_intr_real__r8_dim3(out_sfc, kgen_unit, "out_sfc", .FALSE.) 
      CALL kr_clubb_intr_real__r8_dim3(out_radzm, kgen_unit, "out_radzm", .FALSE.) 
      CALL kr_clubb_intr_real__r8_dim3(out_radzt, kgen_unit, "out_radzt", .FALSE.) 
  END SUBROUTINE kr_externs_in_clubb_intr 
    
  !read state subroutine for kr_externs_out_clubb_intr 
  SUBROUTINE kr_externs_out_clubb_intr(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      CALL kr_clubb_intr_real__r8_dim3(kgenref_out_zm, kgen_unit, "kgenref_out_zm", .FALSE.) 
      CALL kr_clubb_intr_real__r8_dim3(kgenref_out_zt, kgen_unit, "kgenref_out_zt", .FALSE.) 
      CALL kr_clubb_intr_real__r8_dim3(kgenref_out_sfc, kgen_unit, "kgenref_out_sfc", .FALSE.) 
      CALL kr_clubb_intr_real__r8_dim3(kgenref_out_radzm, kgen_unit, "kgenref_out_radzm", .FALSE.) 
      CALL kr_clubb_intr_real__r8_dim3(kgenref_out_radzt, kgen_unit, "kgenref_out_radzt", .FALSE.) 
  END SUBROUTINE kr_externs_out_clubb_intr 
    
  !verify state subroutine for kv_externs_clubb_intr 
  SUBROUTINE kv_externs_clubb_intr(check_status) 
      TYPE(check_t), INTENT(INOUT) :: check_status 
        
      CALL kv_clubb_intr_real__r8_dim3("out_zm", check_status, out_zm, kgenref_out_zm) 
      CALL kv_clubb_intr_real__r8_dim3("out_zt", check_status, out_zt, kgenref_out_zt) 
      CALL kv_clubb_intr_real__r8_dim3("out_sfc", check_status, out_sfc, kgenref_out_sfc) 
      CALL kv_clubb_intr_real__r8_dim3("out_radzm", check_status, out_radzm, kgenref_out_radzm) 
      CALL kv_clubb_intr_real__r8_dim3("out_radzt", check_status, out_radzt, kgenref_out_radzt) 
  END SUBROUTINE kv_externs_clubb_intr 
    
  !read state subroutine for kr_clubb_intr_real__r8_dim3 
  SUBROUTINE kr_clubb_intr_real__r8_dim3(var, kgen_unit, printname, printvar) 
      REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2, idx3 
      INTEGER, DIMENSION(2,3) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          READ (UNIT = kgen_unit) kgen_bound(1, 3) 
          READ (UNIT = kgen_unit) kgen_bound(2, 3) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), kgen_bound(1,3):kgen_bound(2,3))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
  END SUBROUTINE kr_clubb_intr_real__r8_dim3 
    
  !verify state subroutine for kv_clubb_intr_real__r8_dim3 
  RECURSIVE SUBROUTINE kv_clubb_intr_real__r8_dim3(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      REAL(KIND=r8), allocatable, INTENT(IN), DIMENSION(:,:,:) :: var, kgenref_var 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      INTEGER :: idx1, idx2, idx3 
      INTEGER :: n 
      real(KIND=r8) :: nrmsdiff, rmsdiff 
      real(KIND=r8), ALLOCATABLE :: buf1(:,:,:), buf2(:,:,:) 
        
      IF (ALLOCATED(var)) THEN 
          check_status%numTotal = check_status%numTotal + 1 
            
          IF (ALL(var == kgenref_var)) THEN 
              check_status%numIdentical = check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3))) 
              ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3))) 
              n = SIZE(var) 
              WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
                  buf1 = ((var-kgenref_var)/kgenref_var)**2 
                  buf2 = (var-kgenref_var)**2 
              ELSEWHERE 
                  buf1 = (var-kgenref_var)**2 
                  buf2 = buf1 
              END WHERE   
              nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
              rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
              IF (rmsdiff > kgen_tolerance) THEN 
                  check_status%numOutTol = check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 0) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  check_status%numInTol = check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", 0 
                      WRITE (*, *) "Normalized RMS of difference is ", 0 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 0) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
  END SUBROUTINE kv_clubb_intr_real__r8_dim3 
    
end module clubb_intr
