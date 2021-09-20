!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-07 15:55:26 
!KGEN version : 0.7.3 
  


module micro_mg_cam
!---------------------------------------------------------------------------------
!  CAM Interfaces for MG microphysics
!---------------------------------------------------------------------------------
! How to add new packed MG inputs to micro_mg_cam_tend:
! If you have an input with first dimension [psetcols, pver], the procedure
! for adding inputs is as follows:
! 1) In addition to any variables you need to declare for the "unpacked"
!    (CAM format) version, you must declare an array for the "packed" 
!    (MG format) version.
! 2) Add a call similar to the following line (look before the
!    micro_mg_tend calls to see similar lines):
!      packed_array = packer%pack(original_array)
!    The packed array can then be passed into any of the MG schemes.
! This same procedure will also work for 1D arrays of size psetcols, 3-D
! arrays with psetcols and pver as the first dimensions, and for arrays of
! dimension [psetcols, pverp]. You only have to modify the allocation of
! the packed array before the "pack" call.
!---------------------------------------------------------------------------------
! How to add new packed MG outputs to micro_mg_cam_tend:
! 1) As with inputs, in addition to the unpacked outputs you must declare
!    an array for packed data. The unpacked and packed arrays must *also* 
!    be targets or pointers (but cannot be both).
! 2) Add the field to post-processing as in the following line (again,
!    there are many examples before the micro_mg_tend calls):
!      call post_proc%add_field(p(final_array),p(packed_array))
!    *** IMPORTANT ** If the fields are only being passed to a certain version of
!    MG, you must only add them if that version is being called (see
!    the "if (micro_mg_version >1)" sections below
!    This registers the field for post-MG averaging, and to scatter to the
!    final, unpacked version of the array.
!    By default, any columns/levels that are not operated on by MG will be
!    set to 0 on output; this value can be adjusted using the "fillvalue"
!    optional argument to post_proc%add_field.
!    Also by default, outputs from multiple substeps will be averaged after
!    MG's substepping is complete. Passing the optional argument
!    "accum_method=accum_null" will change this behavior so that the last
!    substep is always output.
! This procedure works on 1-D and 2-D outputs. Note that the final,
! unpacked arrays are not set until the call to
! "post_proc%process_and_unpack", which sets every single field that was
! added with post_proc%add_field.
!---------------------------------------------------------------------------------

!
!
!
!
!
!
!
!
!
!
!
!
!
!
!  
!
!
!
!
!

    USE shr_kind_mod, ONLY: rkind_comp, rkind_io


    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 
    SAVE 

#ifdef _MPI
    include 'mpif.h'
#endif

integer :: num_steps ! Number of MG substeps

! Namelist variables for option to specify constant cloud droplet/ice number

! parameters for specified ice and droplet number concentration
! note: these are local in-cloud values, not grid-mean


! Physics buffer indices for fields registered by this module

! Fields for UNICON

! Fields needed as inputs to COSP

! Fields needed by Park macrophysics

! Used to replace aspects of MG microphysics
! (e.g. by CARMA)

! Index fields for precipitation efficiency.

! Physics buffer indices for fields registered by other modules

! Pbuf fields needed for subcol_SILHS


! pbuf fields for heterogeneous freezing


!===============================================================================
PUBLIC kr_externs_in_micro_mg_cam 
PUBLIC kr_externs_out_micro_mg_cam 
PUBLIC micro_mg_cam_tend_pack 


contains
!===============================================================================


!================================================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


SUBROUTINE micro_mg_cam_tend_pack(kgen_unit, kgen_measure, kgen_isverified, dtime, nlev, mgncol, filename) 


    USE micro_mg2_0, ONLY: micro_mg_tend2_0 => micro_mg_tend 

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE kgen_utils_mod, ONLY: kgen_perturb_real 
    USE micro_mg2_0, ONLY: kr_externs_out_micro_mg2_0 
    USE micro_mg_utils, ONLY: kr_externs_out_micro_mg_utils 
    USE wv_sat_methods, ONLY: kr_externs_out_wv_sat_methods 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 

    REAL(KIND=rkind_comp), INTENT(INOUT) :: dtime 

    INTEGER, INTENT(INOUT) :: nlev 
    INTEGER, INTENT(INOUT) :: mgncol 
    CHARACTER(len=*) :: filename
   ! Local variables

   ! Object that packs columns with clouds/precip.

   ! Packed versions of inputs.

    REAL(KIND=rkind_comp) :: packed_t(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_q(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_qc(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_nc(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_qi(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_ni(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_qr(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_nr(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_qs(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_ns(mgncol,nlev) 

    REAL(KIND=rkind_comp) :: packed_relvar(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_accre_enhan(mgncol,nlev) 

    REAL(KIND=rkind_comp) :: packed_p(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_pdel(mgncol,nlev) 

    REAL(KIND=rkind_comp) :: packed_cldn(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_liqcldf(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_icecldf(mgncol,nlev) 
    REAL(KIND=rkind_comp), allocatable :: packed_qsatfac(:,:) 

    REAL(KIND=rkind_comp) :: packed_naai(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_npccn(mgncol,nlev) 

    REAL(KIND=rkind_comp), allocatable :: packed_rndst(:,:,:) 
    REAL(KIND=rkind_comp), allocatable :: packed_nacon(:,:,:) 
   ! Optional outputs.

    REAL(KIND=rkind_comp) :: packed_tnd_qsnow(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_tnd_nsnow(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_re_ice(mgncol,nlev) 

    REAL(KIND=rkind_comp) :: packed_frzimm(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_frzcnt(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: packed_frzdep(mgncol,nlev) 
   ! Output field post-processing.

   ! Packed versions of outputs.

    REAL(KIND=rkind_comp), target :: packed_rate1ord_cw2pr_st(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_tlat(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qvlat(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qctend(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qitend(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_nctend(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_nitend(mgncol,nlev) 

    REAL(KIND=rkind_comp), target :: packed_qrtend(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qstend(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_nrtend(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_nstend(mgncol,nlev) 

    REAL(KIND=rkind_comp), target :: packed_prect(mgncol) 
    REAL(KIND=rkind_comp), target :: packed_preci(mgncol) 
    REAL(KIND=rkind_comp), target :: packed_nevapr(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_am_evp_st(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_evapsnow(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_prain(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_prodsnow(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_cmeout(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qsout(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_cflx(mgncol,nlev+1) 
    REAL(KIND=rkind_comp), target :: packed_iflx(mgncol,nlev+1) 
    REAL(KIND=rkind_comp), target :: packed_rflx(mgncol,nlev+1) 
    REAL(KIND=rkind_comp), target :: packed_sflx(mgncol,nlev+1) 
    REAL(KIND=rkind_comp), target :: packed_qrout(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qcsevap(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qisevap(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qvres(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_cmei(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_vtrmc(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_vtrmi(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qcsedten(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qisedten(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qrsedten(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qssedten(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_umr(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_ums(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_pra(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_prc(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_mnuccc(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_mnucct(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_msacwi(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_psacws(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_bergs(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_berg(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_melt(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_homo(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qcres(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_prci(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_prai(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qires(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_mnuccr(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_pracs(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_meltsdt(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_frzrdt(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_mnuccd(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_nrout(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_nsout(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_refl(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_arefl(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_areflz(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_frefl(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_csrfl(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_acsrfl(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_fcsrfl(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_rercld(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_ncai(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_ncal(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qrout2(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qsout2(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_nrout2(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_nsout2(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_freqs(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_freqr(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_nfice(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_prer_evap(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_qcrat(mgncol,nlev) 

    REAL(KIND=rkind_comp), target :: packed_rel(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_rei(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_sadice(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_sadsnow(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_lambdac(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_mu(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_des(mgncol,nlev) 
    REAL(KIND=rkind_comp), target :: packed_dei(mgncol,nlev) 
   ! Dummy arrays for cases where we throw away the MG version and
   ! recalculate sizes on the CAM grid to avoid time/subcolumn averaging
   ! issues.

    REAL(KIND=rkind_comp) :: rel_fn_dum(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: dsout2_dum(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: drout_dum(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: reff_rain_dum(mgncol,nlev) 
    REAL(KIND=rkind_comp) :: reff_snow_dum(mgncol,nlev) 
   ! Heterogeneous-only version of mnuccdo.

   ! physics buffer fields for COSP simulator

   ! physics buffer fields used with CARMA


                                               ! strat. cloud water to precip (1/s)    ! rce 2010/05/01


  ! variables for heterogeneous freezing


   ! A local copy of state is used for diagnostic calculations


   ! Averaging arrays for effective radius and number....

!  Averaging arrays for supercooled liquid


   ! Cloud fraction used for precipitation.

   ! Average cloud top radius & number


   ! Variables for precip efficiency calculation


   ! Variables for liquid water path and column condensation


   ! variables for autoconversion and accretion vertical averages


   


    CHARACTER(LEN=128) :: errstring 
   ! For rrtmg optics. specified distribution.


   !-------------------------------------------------------------------------------
    INTEGER, INTENT(IN) :: kgen_unit 
    REAL(KIND=kgen_dp), INTENT(OUT) :: kgen_measure 
    LOGICAL, INTENT(OUT) :: kgen_isverified 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    INTEGER :: kgen_mpirank, kgen_openmptid, kgen_kernelinvoke 
    LOGICAL :: kgen_evalstage, kgen_warmupstage, kgen_mainstage 
    COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
      
    TYPE(check_t) :: check_status 
    INTEGER*8 :: kgen_intvar, kgen_start_clock, kgen_stop_clock, kgen_rate_clock 
    INTEGER, PARAMETER :: maxiter = 10000
    REAL(KIND=kgen_dp) :: gkgen_measure
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_rate1ord_cw2pr_st 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_tlat 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qvlat 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qctend 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qitend 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_nctend 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_nitend 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qrtend 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qstend 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_nrtend 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_nstend 
    REAL(KIND=rkind_io), dimension(mgncol) :: kgenref_packed_prect 
    REAL(KIND=rkind_io), dimension(mgncol) :: kgenref_packed_preci 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_nevapr 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_am_evp_st 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_evapsnow 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_prain 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_prodsnow 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_cmeout 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qsout 
    REAL(KIND=rkind_io), dimension(mgncol,nlev+1) :: kgenref_packed_cflx 
    REAL(KIND=rkind_io), dimension(mgncol,nlev+1) :: kgenref_packed_iflx 
    REAL(KIND=rkind_io), dimension(mgncol,nlev+1) :: kgenref_packed_rflx 
    REAL(KIND=rkind_io), dimension(mgncol,nlev+1) :: kgenref_packed_sflx 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qrout 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qcsevap 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qisevap 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qvres 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_cmei 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_vtrmc 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_vtrmi 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qcsedten 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qisedten 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qrsedten 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qssedten 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_umr 

    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_ums 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_pra 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_prc 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_mnuccc 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_mnucct 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_msacwi 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_psacws 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_bergs 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_berg 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_melt 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_homo 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qcres 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_prci 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_prai 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qires 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_mnuccr 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_pracs 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_meltsdt 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_frzrdt 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_mnuccd 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_nrout 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_nsout 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_refl 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_arefl 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_areflz 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_frefl 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_csrfl 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_acsrfl 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_fcsrfl 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_rercld 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_ncai 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_ncal 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qrout2 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qsout2 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_nrout2 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_nsout2 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_freqs 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_freqr 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_nfice 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_prer_evap 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_qcrat 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_rel 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_rei 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_sadice 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_sadsnow 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_lambdac 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_mu 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_des 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_packed_dei 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_rel_fn_dum 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_dsout2_dum 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_drout_dum 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_reff_rain_dum 
    REAL(KIND=rkind_io), dimension(mgncol,nlev) :: kgenref_reff_snow_dum 
    CHARACTER(LEN=128) :: kgenref_errstring 
    integer :: myrank, mpisize, info
    real(kind=rkind_io), dimension(mgncol,nlev)   :: tmpA
    real(kind=rkind_io), dimension(mgncol) :: tmpB
    real(kind=rkind_io), dimension(mgncol,nlev+1) :: tmpC
 
#ifdef _MPI
!    call mpi_init(info)
    call mpi_comm_rank(mpi_comm_world, myrank, info)
    call mpi_comm_size(mpi_comm_world, mpisize, info)
#endif
      
    !local input variables 
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_t = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_t", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_q  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_q", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qc = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qc", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_nc = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_nc", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA ;packed_qi = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qi", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_ni   = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_ni", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qr  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qr", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_nr  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_nr", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qs  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qs", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_ns  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_ns", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_relvar  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_relvar", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_accre_enhan  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_accre_enhan", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_p  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_p", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_pdel  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_pdel", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_cldn  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_cldn", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_liqcldf  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_liqcldf", kgen_array_sum, DBLE(SUM(tmpA, mask=.true.)), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_icecldf  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_icecldf", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 
    END IF   
    CALL kr_micro_mg_cam_tend_pack_real__rkind_comp_dim2(packed_qsatfac, kgen_unit, "packed_qsatfac", .FALSE.) 
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_naai  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_naai", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_npccn  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_npccn", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    CALL kr_micro_mg_cam_tend_pack_real__rkind_comp_dim3(packed_rndst, kgen_unit, "packed_rndst", .FALSE.) 
    CALL kr_micro_mg_cam_tend_pack_real__rkind_comp_dim3(packed_nacon, kgen_unit, "packed_nacon", .FALSE.) 
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_tnd_qsnow  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_tnd_qsnow", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_tnd_nsnow  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_tnd_nsnow", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_re_ice  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_re_ice", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_frzimm  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_frzimm", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_frzcnt = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_frzcnt", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_frzdep = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_frzdep", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 
        packed_frzdep = real(tmpA,kind=rkind_comp)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_rate1ord_cw2pr_st = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_rate1ord_cw2pr_st", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 

    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_tlat  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_tlat", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qvlat = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qvlat", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qctend  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qctend", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qitend  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qitend", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_nctend = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_nctend", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_nitend = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_nitend", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qrtend = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qrtend", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qstend = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qstend", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_nrtend = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_nrtend", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_nstend = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_nstend", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpB; packed_prect = real(tmpB,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_prect", kgen_array_sum, DBLE(SUM(tmpB, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpB; packed_preci = real(tmpB,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_preci", kgen_array_sum, DBLE(SUM(tmpB, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_nevapr = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_nevapr", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_am_evp_st = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_am_evp_st", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_evapsnow = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_evapsnow", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_prain = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_prain", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_prodsnow = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_prodsnow", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_cmeout = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_cmeout", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qsout = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qsout", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpC; packed_cflx = real(tmpC,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_cflx", kgen_array_sum, DBLE(SUM(tmpC, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpC; packed_iflx = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_iflx", kgen_array_sum, DBLE(SUM(tmpC, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpC; packed_rflx = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_rflx", kgen_array_sum, DBLE(SUM(tmpC, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpC; packed_sflx = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_sflx", kgen_array_sum, DBLE(SUM(tmpC, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qrout = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qrout", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qcsevap = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qcsevap", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qisevap = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qisevap", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qvres = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qvres", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_cmei = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_cmei", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_vtrmc = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_vtrmc", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_vtrmi = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_vtrmi", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qcsedten = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qcsedten", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qisedten = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qisedten", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qrsedten = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qrsedten", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qssedten = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qssedten", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_umr = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_umr", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_ums = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_ums", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_pra = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_pra", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_prc = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_prc", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_mnuccc = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_mnuccc", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_mnucct = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_mnucct", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_msacwi = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_msacwi", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_psacws = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_psacws", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_bergs = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_bergs", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_berg = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_berg", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_melt = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_melt", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_homo = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_homo", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qcres = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_qcres", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_prci = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_prci", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_prai = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_prai", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qires = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qires", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_mnuccr = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_mnuccr", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_pracs = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_pracs", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_meltsdt = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_meltsdt", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_frzrdt = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_frzrdt", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_mnuccd = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_mnuccd", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_nrout = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_nrout", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_nsout = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_nsout", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_refl = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_refl", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_arefl = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_arefl", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_areflz = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_areflz", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_frefl = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_frefl", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_csrfl = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_csrfl", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_acsrfl = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_acsrfl", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_fcsrfl = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_fcsrfl", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA;packed_rercld = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_rercld", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_ncai = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_ncai", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_ncal = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_ncal", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qrout2 = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_qrout2", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qsout2 = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_qsout2", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_nrout2 = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_nrout2", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_nsout2 = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_nsout2", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_freqs = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_freqs", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_freqr = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_freqr", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_nfice = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_nfice", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_prer_evap  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_prer_evap", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_qcrat = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_qcrat", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_rel = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_rel", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_rei = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_rei", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_sadice = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_sadice", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_sadsnow  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_sadsnow", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_lambdac  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_lambdac", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_mu = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("packed_mu", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_des = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_des", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; packed_dei = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("packed_dei", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; rel_fn_dum = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("rel_fn_dum", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; dsout2_dum  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("dsout2_dum", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; drout_dum = real(tmpA,kind=rkind_comp) 
        CALL kgen_array_sumcheck("drout_dum", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; reff_rain_dum  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("reff_rain_dum", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmpA; reff_snow_dum  = real(tmpA,kind=rkind_comp)
        CALL kgen_array_sumcheck("reff_snow_dum", kgen_array_sum, DBLE(SUM(tmpA, mask=.true. )), .TRUE.)
    END IF   
    READ (UNIT = kgen_unit) errstring 
      
    !extern output variables 
    CALL kr_externs_out_micro_mg_cam(kgen_unit) 
    CALL kr_externs_out_micro_mg2_0(kgen_unit) 
    CALL kr_externs_out_micro_mg_utils(kgen_unit) 
    CALL kr_externs_out_wv_sat_methods(kgen_unit) 
      
    !local output variables 
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_rate1ord_cw2pr_st 
        CALL kgen_array_sumcheck("kgenref_packed_rate1ord_cw2pr_st", kgen_array_sum, DBLE(SUM(kgenref_packed_rate1ord_cw2pr_st, &
        &mask=(kgenref_packed_rate1ord_cw2pr_st .eq. kgenref_packed_rate1ord_cw2pr_st))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_tlat 
        CALL kgen_array_sumcheck("kgenref_packed_tlat", kgen_array_sum, DBLE(SUM(kgenref_packed_tlat, mask=(kgenref_packed_tlat &
        &.eq. kgenref_packed_tlat))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qvlat 
        CALL kgen_array_sumcheck("kgenref_packed_qvlat", kgen_array_sum, DBLE(SUM(kgenref_packed_qvlat, &
        &mask=(kgenref_packed_qvlat .eq. kgenref_packed_qvlat))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qctend 
        CALL kgen_array_sumcheck("kgenref_packed_qctend", kgen_array_sum, DBLE(SUM(kgenref_packed_qctend, &
        &mask=(kgenref_packed_qctend .eq. kgenref_packed_qctend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qitend 
        CALL kgen_array_sumcheck("kgenref_packed_qitend", kgen_array_sum, DBLE(SUM(kgenref_packed_qitend, &
        &mask=(kgenref_packed_qitend .eq. kgenref_packed_qitend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nctend 
        CALL kgen_array_sumcheck("kgenref_packed_nctend", kgen_array_sum, DBLE(SUM(kgenref_packed_nctend, &
        &mask=(kgenref_packed_nctend .eq. kgenref_packed_nctend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nitend 
        CALL kgen_array_sumcheck("kgenref_packed_nitend", kgen_array_sum, DBLE(SUM(kgenref_packed_nitend, &
        &mask=(kgenref_packed_nitend .eq. kgenref_packed_nitend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qrtend 
        CALL kgen_array_sumcheck("kgenref_packed_qrtend", kgen_array_sum, DBLE(SUM(kgenref_packed_qrtend, &
        &mask=(kgenref_packed_qrtend .eq. kgenref_packed_qrtend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qstend 
        CALL kgen_array_sumcheck("kgenref_packed_qstend", kgen_array_sum, DBLE(SUM(kgenref_packed_qstend, &
        &mask=(kgenref_packed_qstend .eq. kgenref_packed_qstend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nrtend 
        CALL kgen_array_sumcheck("kgenref_packed_nrtend", kgen_array_sum, DBLE(SUM(kgenref_packed_nrtend, &
        &mask=(kgenref_packed_nrtend .eq. kgenref_packed_nrtend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nstend 
        CALL kgen_array_sumcheck("kgenref_packed_nstend", kgen_array_sum, DBLE(SUM(kgenref_packed_nstend, &
        &mask=(kgenref_packed_nstend .eq. kgenref_packed_nstend))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prect 
        CALL kgen_array_sumcheck("kgenref_packed_prect", kgen_array_sum, DBLE(SUM(kgenref_packed_prect, &
        &mask=(kgenref_packed_prect .eq. kgenref_packed_prect))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_preci 
        CALL kgen_array_sumcheck("kgenref_packed_preci", kgen_array_sum, DBLE(SUM(kgenref_packed_preci, &
        &mask=(kgenref_packed_preci .eq. kgenref_packed_preci))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nevapr 
        CALL kgen_array_sumcheck("kgenref_packed_nevapr", kgen_array_sum, DBLE(SUM(kgenref_packed_nevapr, &
        &mask=(kgenref_packed_nevapr .eq. kgenref_packed_nevapr))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_am_evp_st 
        CALL kgen_array_sumcheck("kgenref_packed_am_evp_st", kgen_array_sum, DBLE(SUM(kgenref_packed_am_evp_st, &
        &mask=(kgenref_packed_am_evp_st .eq. kgenref_packed_am_evp_st))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_evapsnow 
        CALL kgen_array_sumcheck("kgenref_packed_evapsnow", kgen_array_sum, DBLE(SUM(kgenref_packed_evapsnow, &
        &mask=(kgenref_packed_evapsnow .eq. kgenref_packed_evapsnow))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prain 
        CALL kgen_array_sumcheck("kgenref_packed_prain", kgen_array_sum, DBLE(SUM(kgenref_packed_prain, &
        &mask=(kgenref_packed_prain .eq. kgenref_packed_prain))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prodsnow 
        CALL kgen_array_sumcheck("kgenref_packed_prodsnow", kgen_array_sum, DBLE(SUM(kgenref_packed_prodsnow, &
        &mask=(kgenref_packed_prodsnow .eq. kgenref_packed_prodsnow))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_cmeout 
        CALL kgen_array_sumcheck("kgenref_packed_cmeout", kgen_array_sum, DBLE(SUM(kgenref_packed_cmeout, &
        &mask=(kgenref_packed_cmeout .eq. kgenref_packed_cmeout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qsout 
        CALL kgen_array_sumcheck("kgenref_packed_qsout", kgen_array_sum, DBLE(SUM(kgenref_packed_qsout, &
        &mask=(kgenref_packed_qsout .eq. kgenref_packed_qsout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_cflx 
        CALL kgen_array_sumcheck("kgenref_packed_cflx", kgen_array_sum, DBLE(SUM(kgenref_packed_cflx, mask=(kgenref_packed_cflx &
        &.eq. kgenref_packed_cflx))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_iflx 
        CALL kgen_array_sumcheck("kgenref_packed_iflx", kgen_array_sum, DBLE(SUM(kgenref_packed_iflx, mask=(kgenref_packed_iflx &
        &.eq. kgenref_packed_iflx))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_rflx 
        CALL kgen_array_sumcheck("kgenref_packed_rflx", kgen_array_sum, DBLE(SUM(kgenref_packed_rflx, mask=(kgenref_packed_rflx &
        &.eq. kgenref_packed_rflx))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_sflx 
        CALL kgen_array_sumcheck("kgenref_packed_sflx", kgen_array_sum, DBLE(SUM(kgenref_packed_sflx, mask=(kgenref_packed_sflx &
        &.eq. kgenref_packed_sflx))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qrout 
        CALL kgen_array_sumcheck("kgenref_packed_qrout", kgen_array_sum, DBLE(SUM(kgenref_packed_qrout, &
        &mask=(kgenref_packed_qrout .eq. kgenref_packed_qrout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qcsevap 
        CALL kgen_array_sumcheck("kgenref_packed_qcsevap", kgen_array_sum, DBLE(SUM(kgenref_packed_qcsevap, &
        &mask=(kgenref_packed_qcsevap .eq. kgenref_packed_qcsevap))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qisevap 
        CALL kgen_array_sumcheck("kgenref_packed_qisevap", kgen_array_sum, DBLE(SUM(kgenref_packed_qisevap, &
        &mask=(kgenref_packed_qisevap .eq. kgenref_packed_qisevap))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qvres 
        CALL kgen_array_sumcheck("kgenref_packed_qvres", kgen_array_sum, DBLE(SUM(kgenref_packed_qvres, &
        &mask=(kgenref_packed_qvres .eq. kgenref_packed_qvres))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_cmei 
        CALL kgen_array_sumcheck("kgenref_packed_cmei", kgen_array_sum, DBLE(SUM(kgenref_packed_cmei, mask=(kgenref_packed_cmei &
        &.eq. kgenref_packed_cmei))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_vtrmc 
        CALL kgen_array_sumcheck("kgenref_packed_vtrmc", kgen_array_sum, DBLE(SUM(kgenref_packed_vtrmc, &
        &mask=(kgenref_packed_vtrmc .eq. kgenref_packed_vtrmc))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_vtrmi 
        CALL kgen_array_sumcheck("kgenref_packed_vtrmi", kgen_array_sum, DBLE(SUM(kgenref_packed_vtrmi, &
        &mask=(kgenref_packed_vtrmi .eq. kgenref_packed_vtrmi))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qcsedten 
        CALL kgen_array_sumcheck("kgenref_packed_qcsedten", kgen_array_sum, DBLE(SUM(kgenref_packed_qcsedten, &
        &mask=(kgenref_packed_qcsedten .eq. kgenref_packed_qcsedten))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qisedten 
        CALL kgen_array_sumcheck("kgenref_packed_qisedten", kgen_array_sum, DBLE(SUM(kgenref_packed_qisedten, &
        &mask=(kgenref_packed_qisedten .eq. kgenref_packed_qisedten))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qrsedten 
        CALL kgen_array_sumcheck("kgenref_packed_qrsedten", kgen_array_sum, DBLE(SUM(kgenref_packed_qrsedten, &
        &mask=(kgenref_packed_qrsedten .eq. kgenref_packed_qrsedten))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qssedten 
        CALL kgen_array_sumcheck("kgenref_packed_qssedten", kgen_array_sum, DBLE(SUM(kgenref_packed_qssedten, &
        &mask=(kgenref_packed_qssedten .eq. kgenref_packed_qssedten))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_umr 
        CALL kgen_array_sumcheck("kgenref_packed_umr", kgen_array_sum, DBLE(SUM(kgenref_packed_umr, mask=(kgenref_packed_umr .eq. &
        &kgenref_packed_umr))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_ums 
        CALL kgen_array_sumcheck("kgenref_packed_ums", kgen_array_sum, DBLE(SUM(kgenref_packed_ums, mask=(kgenref_packed_ums .eq. &
        &kgenref_packed_ums))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_pra 
        CALL kgen_array_sumcheck("kgenref_packed_pra", kgen_array_sum, DBLE(SUM(kgenref_packed_pra, mask=(kgenref_packed_pra .eq. &
        &kgenref_packed_pra))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prc 
        CALL kgen_array_sumcheck("kgenref_packed_prc", kgen_array_sum, DBLE(SUM(kgenref_packed_prc, mask=(kgenref_packed_prc .eq. &
        &kgenref_packed_prc))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mnuccc 
        CALL kgen_array_sumcheck("kgenref_packed_mnuccc", kgen_array_sum, DBLE(SUM(kgenref_packed_mnuccc, &
        &mask=(kgenref_packed_mnuccc .eq. kgenref_packed_mnuccc))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mnucct 
        CALL kgen_array_sumcheck("kgenref_packed_mnucct", kgen_array_sum, DBLE(SUM(kgenref_packed_mnucct, &
        &mask=(kgenref_packed_mnucct .eq. kgenref_packed_mnucct))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_msacwi 
        CALL kgen_array_sumcheck("kgenref_packed_msacwi", kgen_array_sum, DBLE(SUM(kgenref_packed_msacwi, &
        &mask=(kgenref_packed_msacwi .eq. kgenref_packed_msacwi))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_psacws 
        CALL kgen_array_sumcheck("kgenref_packed_psacws", kgen_array_sum, DBLE(SUM(kgenref_packed_psacws, &
        &mask=(kgenref_packed_psacws .eq. kgenref_packed_psacws))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_bergs 
        CALL kgen_array_sumcheck("kgenref_packed_bergs", kgen_array_sum, DBLE(SUM(kgenref_packed_bergs, &
        &mask=(kgenref_packed_bergs .eq. kgenref_packed_bergs))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_berg 
        CALL kgen_array_sumcheck("kgenref_packed_berg", kgen_array_sum, DBLE(SUM(kgenref_packed_berg, mask=(kgenref_packed_berg &
        &.eq. kgenref_packed_berg))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_melt 
        CALL kgen_array_sumcheck("kgenref_packed_melt", kgen_array_sum, DBLE(SUM(kgenref_packed_melt, mask=(kgenref_packed_melt &
        &.eq. kgenref_packed_melt))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_homo 
        CALL kgen_array_sumcheck("kgenref_packed_homo", kgen_array_sum, DBLE(SUM(kgenref_packed_homo, mask=(kgenref_packed_homo &
        &.eq. kgenref_packed_homo))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qcres 
        CALL kgen_array_sumcheck("kgenref_packed_qcres", kgen_array_sum, DBLE(SUM(kgenref_packed_qcres, &
        &mask=(kgenref_packed_qcres .eq. kgenref_packed_qcres))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prci 
        CALL kgen_array_sumcheck("kgenref_packed_prci", kgen_array_sum, DBLE(SUM(kgenref_packed_prci, mask=(kgenref_packed_prci &
        &.eq. kgenref_packed_prci))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prai 
        CALL kgen_array_sumcheck("kgenref_packed_prai", kgen_array_sum, DBLE(SUM(kgenref_packed_prai, mask=(kgenref_packed_prai &
        &.eq. kgenref_packed_prai))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qires 
        CALL kgen_array_sumcheck("kgenref_packed_qires", kgen_array_sum, DBLE(SUM(kgenref_packed_qires, &
        &mask=(kgenref_packed_qires .eq. kgenref_packed_qires))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mnuccr 
        CALL kgen_array_sumcheck("kgenref_packed_mnuccr", kgen_array_sum, DBLE(SUM(kgenref_packed_mnuccr, &
        &mask=(kgenref_packed_mnuccr .eq. kgenref_packed_mnuccr))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_pracs 
        CALL kgen_array_sumcheck("kgenref_packed_pracs", kgen_array_sum, DBLE(SUM(kgenref_packed_pracs, &
        &mask=(kgenref_packed_pracs .eq. kgenref_packed_pracs))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_meltsdt 
        CALL kgen_array_sumcheck("kgenref_packed_meltsdt", kgen_array_sum, DBLE(SUM(kgenref_packed_meltsdt, &
        &mask=(kgenref_packed_meltsdt .eq. kgenref_packed_meltsdt))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_frzrdt 
        CALL kgen_array_sumcheck("kgenref_packed_frzrdt", kgen_array_sum, DBLE(SUM(kgenref_packed_frzrdt, &
        &mask=(kgenref_packed_frzrdt .eq. kgenref_packed_frzrdt))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mnuccd 
        CALL kgen_array_sumcheck("kgenref_packed_mnuccd", kgen_array_sum, DBLE(SUM(kgenref_packed_mnuccd, &
        &mask=(kgenref_packed_mnuccd .eq. kgenref_packed_mnuccd))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nrout 
        CALL kgen_array_sumcheck("kgenref_packed_nrout", kgen_array_sum, DBLE(SUM(kgenref_packed_nrout, &
        &mask=(kgenref_packed_nrout .eq. kgenref_packed_nrout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nsout 
        CALL kgen_array_sumcheck("kgenref_packed_nsout", kgen_array_sum, DBLE(SUM(kgenref_packed_nsout, &
        &mask=(kgenref_packed_nsout .eq. kgenref_packed_nsout))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_refl 
        CALL kgen_array_sumcheck("kgenref_packed_refl", kgen_array_sum, DBLE(SUM(kgenref_packed_refl, mask=(kgenref_packed_refl &
        &.eq. kgenref_packed_refl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_arefl 
        CALL kgen_array_sumcheck("kgenref_packed_arefl", kgen_array_sum, DBLE(SUM(kgenref_packed_arefl, &
        &mask=(kgenref_packed_arefl .eq. kgenref_packed_arefl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_areflz 
        CALL kgen_array_sumcheck("kgenref_packed_areflz", kgen_array_sum, DBLE(SUM(kgenref_packed_areflz, &
        &mask=(kgenref_packed_areflz .eq. kgenref_packed_areflz))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_frefl 
        CALL kgen_array_sumcheck("kgenref_packed_frefl", kgen_array_sum, DBLE(SUM(kgenref_packed_frefl, &
        &mask=(kgenref_packed_frefl .eq. kgenref_packed_frefl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_csrfl 
        CALL kgen_array_sumcheck("kgenref_packed_csrfl", kgen_array_sum, DBLE(SUM(kgenref_packed_csrfl, &
        &mask=(kgenref_packed_csrfl .eq. kgenref_packed_csrfl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_acsrfl 
        CALL kgen_array_sumcheck("kgenref_packed_acsrfl", kgen_array_sum, DBLE(SUM(kgenref_packed_acsrfl, &
        &mask=(kgenref_packed_acsrfl .eq. kgenref_packed_acsrfl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_fcsrfl 
        CALL kgen_array_sumcheck("kgenref_packed_fcsrfl", kgen_array_sum, DBLE(SUM(kgenref_packed_fcsrfl, &
        &mask=(kgenref_packed_fcsrfl .eq. kgenref_packed_fcsrfl))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_rercld 
        CALL kgen_array_sumcheck("kgenref_packed_rercld", kgen_array_sum, DBLE(SUM(kgenref_packed_rercld, &
        &mask=(kgenref_packed_rercld .eq. kgenref_packed_rercld))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_ncai 
        CALL kgen_array_sumcheck("kgenref_packed_ncai", kgen_array_sum, DBLE(SUM(kgenref_packed_ncai, mask=(kgenref_packed_ncai &
        &.eq. kgenref_packed_ncai))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_ncal 
        CALL kgen_array_sumcheck("kgenref_packed_ncal", kgen_array_sum, DBLE(SUM(kgenref_packed_ncal, mask=(kgenref_packed_ncal &
        &.eq. kgenref_packed_ncal))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qrout2 
        CALL kgen_array_sumcheck("kgenref_packed_qrout2", kgen_array_sum, DBLE(SUM(kgenref_packed_qrout2, &
        &mask=(kgenref_packed_qrout2 .eq. kgenref_packed_qrout2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qsout2 
        CALL kgen_array_sumcheck("kgenref_packed_qsout2", kgen_array_sum, DBLE(SUM(kgenref_packed_qsout2, &
        &mask=(kgenref_packed_qsout2 .eq. kgenref_packed_qsout2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nrout2 
        CALL kgen_array_sumcheck("kgenref_packed_nrout2", kgen_array_sum, DBLE(SUM(kgenref_packed_nrout2, &
        &mask=(kgenref_packed_nrout2 .eq. kgenref_packed_nrout2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nsout2 
        CALL kgen_array_sumcheck("kgenref_packed_nsout2", kgen_array_sum, DBLE(SUM(kgenref_packed_nsout2, &
        &mask=(kgenref_packed_nsout2 .eq. kgenref_packed_nsout2))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_freqs 
        CALL kgen_array_sumcheck("kgenref_packed_freqs", kgen_array_sum, DBLE(SUM(kgenref_packed_freqs, &
        &mask=(kgenref_packed_freqs .eq. kgenref_packed_freqs))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_freqr 
        CALL kgen_array_sumcheck("kgenref_packed_freqr", kgen_array_sum, DBLE(SUM(kgenref_packed_freqr, &
        &mask=(kgenref_packed_freqr .eq. kgenref_packed_freqr))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_nfice 
        CALL kgen_array_sumcheck("kgenref_packed_nfice", kgen_array_sum, DBLE(SUM(kgenref_packed_nfice, &
        &mask=(kgenref_packed_nfice .eq. kgenref_packed_nfice))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_prer_evap 
        CALL kgen_array_sumcheck("kgenref_packed_prer_evap", kgen_array_sum, DBLE(SUM(kgenref_packed_prer_evap, &
        &mask=(kgenref_packed_prer_evap .eq. kgenref_packed_prer_evap))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_qcrat 
        CALL kgen_array_sumcheck("kgenref_packed_qcrat", kgen_array_sum, DBLE(SUM(kgenref_packed_qcrat, &
        &mask=(kgenref_packed_qcrat .eq. kgenref_packed_qcrat))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_rel 
        CALL kgen_array_sumcheck("kgenref_packed_rel", kgen_array_sum, DBLE(SUM(kgenref_packed_rel, mask=(kgenref_packed_rel .eq. &
        &kgenref_packed_rel))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_rei 
        CALL kgen_array_sumcheck("kgenref_packed_rei", kgen_array_sum, DBLE(SUM(kgenref_packed_rei, mask=(kgenref_packed_rei .eq. &
        &kgenref_packed_rei))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_sadice 
        CALL kgen_array_sumcheck("kgenref_packed_sadice", kgen_array_sum, DBLE(SUM(kgenref_packed_sadice, &
        &mask=(kgenref_packed_sadice .eq. kgenref_packed_sadice))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_sadsnow 
        CALL kgen_array_sumcheck("kgenref_packed_sadsnow", kgen_array_sum, DBLE(SUM(kgenref_packed_sadsnow, &
        &mask=(kgenref_packed_sadsnow .eq. kgenref_packed_sadsnow))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_lambdac 
        CALL kgen_array_sumcheck("kgenref_packed_lambdac", kgen_array_sum, DBLE(SUM(kgenref_packed_lambdac, &
        &mask=(kgenref_packed_lambdac .eq. kgenref_packed_lambdac))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_mu 
        CALL kgen_array_sumcheck("kgenref_packed_mu", kgen_array_sum, DBLE(SUM(kgenref_packed_mu, mask=(kgenref_packed_mu .eq. &
        &kgenref_packed_mu))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_des 
        CALL kgen_array_sumcheck("kgenref_packed_des", kgen_array_sum, DBLE(SUM(kgenref_packed_des, mask=(kgenref_packed_des .eq. &
        &kgenref_packed_des))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_packed_dei 
        CALL kgen_array_sumcheck("kgenref_packed_dei", kgen_array_sum, DBLE(SUM(kgenref_packed_dei, mask=(kgenref_packed_dei .eq. &
        &kgenref_packed_dei))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_rel_fn_dum 
        CALL kgen_array_sumcheck("kgenref_rel_fn_dum", kgen_array_sum, DBLE(SUM(kgenref_rel_fn_dum, mask=(kgenref_rel_fn_dum .eq. &
        &kgenref_rel_fn_dum))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_dsout2_dum 
        CALL kgen_array_sumcheck("kgenref_dsout2_dum", kgen_array_sum, DBLE(SUM(kgenref_dsout2_dum, mask=(kgenref_dsout2_dum .eq. &
        &kgenref_dsout2_dum))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_drout_dum 
        CALL kgen_array_sumcheck("kgenref_drout_dum", kgen_array_sum, DBLE(SUM(kgenref_drout_dum, mask=(kgenref_drout_dum .eq. &
        &kgenref_drout_dum))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_reff_rain_dum 
        CALL kgen_array_sumcheck("kgenref_reff_rain_dum", kgen_array_sum, DBLE(SUM(kgenref_reff_rain_dum, &
        &mask=(kgenref_reff_rain_dum .eq. kgenref_reff_rain_dum))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_reff_snow_dum 
        CALL kgen_array_sumcheck("kgenref_reff_snow_dum", kgen_array_sum, DBLE(SUM(kgenref_reff_snow_dum, &
        &mask=(kgenref_reff_snow_dum .eq. kgenref_reff_snow_dum))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgenref_errstring 


   ! Set the col_type flag to grid or subcolumn dependent on the value of use_subcol_microp

   !-----------------------
   ! These physics buffer fields are read only and not set in this parameterization
   ! If these fields do not have subcolumn data, copy the grid to the subcolumn if subcolumns is turned on
   ! If subcolumns is not turned on, then these fields will be grid data


   !-----------------------
   ! These physics buffer fields are calculated and set in this parameterization
   ! If subcolumns is turned on, then these fields will be calculated on a subcolumn grid, otherwise they will be a normal grid


   !-----------------------
   ! If subcolumns is turned on, all calculated fields which are on subcolumns
   ! need to be retrieved on the grid as well for storing averaged values


   !-----------------------
   ! These are only on the grid regardless of whether subcolumns are turned on or not


   !-------------------------------------------------------------------------------------
   ! Microphysics assumes 'liquid stratus frac = ice stratus frac
   !                      = max( liquid stratus frac, ice stratus frac )'.
   
   ! Output initial in-cloud LWP (before microphysics)


   ! Initialize local state from input.

   ! Because of the of limited vertical resolution, there can be a signifcant
   ! warm bias at the cold point tropopause, which can create a wet bias in the
   ! stratosphere. For the microphysics only, update the cold point temperature, with
   ! an estimate of the coldest point between the model layers.


   ! Initialize ptend for output.


   ! the name 'cldwat' triggers special tests on cldliq
   ! and cldice in physics_update


   ! The following are all variables related to sizes, where it does not
   ! necessarily make sense to average over time steps. Instead, we keep
   ! the value from the last substep, which is what "accum_null" does.

   ! Pack input variables that are not updated during substeps.


    IF (kgen_evalstage) THEN 
    END IF   
    IF (kgen_warmupstage) THEN 
    END IF   
    IF (kgen_mainstage) THEN 
    END IF   
      
    !Uncomment following call statement to turn on perturbation experiment. 
    !Adjust perturbation value and/or kind parameter if required. 
    !CALL kgen_perturb_real( your_variable, 1.0E-15_8 ) 
      
    !call to kgen kernel 
            call micro_mg_tend2_0( &
                 mgncol,         nlev,           dtime/num_steps,&
                 packed_t,               packed_q,               &
                 packed_qc,              packed_qi,              &
                 packed_nc,              packed_ni,              &
                 packed_qr,              packed_qs,              &
                 packed_nr,              packed_ns,              &
                 packed_relvar,          packed_accre_enhan,     &
                 packed_p,               packed_pdel,            &
                 packed_cldn, packed_liqcldf, packed_icecldf, packed_qsatfac, &
                 packed_rate1ord_cw2pr_st,                       &
                 packed_naai,            packed_npccn,           &
                 packed_rndst,           packed_nacon,           &
                 packed_tlat,            packed_qvlat,           &
                 packed_qctend,          packed_qitend,          &
                 packed_nctend,          packed_nitend,          &
                 packed_qrtend,          packed_qstend,          &
                 packed_nrtend,          packed_nstend,          &
                 packed_rel,     rel_fn_dum,     packed_rei,     &
                 packed_sadice,          packed_sadsnow,         &
                 packed_prect,           packed_preci,           &
                 packed_nevapr,          packed_evapsnow,        &
                 packed_am_evp_st,                               &
                 packed_prain,           packed_prodsnow,        &
                 packed_cmeout,          packed_dei,             &
                 packed_mu,              packed_lambdac,         &
                 packed_qsout,           packed_des,             &
                 packed_cflx,    packed_iflx,                    &
                 packed_rflx,    packed_sflx,    packed_qrout,   &
                 reff_rain_dum,          reff_snow_dum,          &
                 packed_qcsevap, packed_qisevap, packed_qvres,   &
                 packed_cmei,    packed_vtrmc,   packed_vtrmi,   &
                 packed_umr,             packed_ums,             &
                 packed_qcsedten,        packed_qisedten,        &
                 packed_qrsedten,        packed_qssedten,        &
                 packed_pra,             packed_prc,             &
                 packed_mnuccc,  packed_mnucct,  packed_msacwi,  &
                 packed_psacws,  packed_bergs,   packed_berg,    &
                 packed_melt,            packed_homo,            &
                 packed_qcres,   packed_prci,    packed_prai,    &
                 packed_qires,   packed_mnuccr,  packed_pracs,   &
                 packed_meltsdt, packed_frzrdt,  packed_mnuccd,  &
                 packed_nrout,           packed_nsout,           &
                 packed_refl,    packed_arefl,   packed_areflz,  &
                 packed_frefl,   packed_csrfl,   packed_acsrfl,  &
                 packed_fcsrfl,          packed_rercld,          &
                 packed_ncai,            packed_ncal,            &
                 packed_qrout2,          packed_qsout2,          &
                 packed_nrout2,          packed_nsout2,          &
                 drout_dum,              dsout2_dum,             &
                 packed_freqs,           packed_freqr,           &
                 packed_nfice,           packed_qcrat,           &
                 errstring, &
                 packed_tnd_qsnow,packed_tnd_nsnow,packed_re_ice,&
                 packed_prer_evap,                                     &
                 packed_frzimm,  packed_frzcnt,  packed_frzdep   )
            IF (kgen_mainstage) THEN 
                  
                !verify init 
#ifdef USE_R4
                CALL kgen_init_check(check_status, rank=myrank, tolerance=9.D-5, verboseLevel=1) 
#else
                CALL kgen_init_check(check_status, rank=myrank, tolerance=9.0D-7, verboseLevel=1) 
#endif
                  
                !extern verify variables 
                  
                !local verify variables 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_rate1ord_cw2pr_st", check_status, packed_rate1ord_cw2pr_st, &
                &kgenref_packed_rate1ord_cw2pr_st) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_tlat", check_status, packed_tlat, kgenref_packed_tlat) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qvlat", check_status, packed_qvlat, kgenref_packed_qvlat) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qctend", check_status, packed_qctend, kgenref_packed_qctend) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qitend", check_status, packed_qitend, kgenref_packed_qitend) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_nctend", check_status, packed_nctend, kgenref_packed_nctend) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_nitend", check_status, packed_nitend, kgenref_packed_nitend) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qrtend", check_status, packed_qrtend, kgenref_packed_qrtend) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qstend", check_status, packed_qstend, kgenref_packed_qstend) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_nrtend", check_status, packed_nrtend, kgenref_packed_nrtend) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_nstend", check_status, packed_nstend, kgenref_packed_nstend) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim1("packed_prect", check_status, packed_prect, kgenref_packed_prect) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim1("packed_preci", check_status, packed_preci, kgenref_packed_preci) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_nevapr", check_status, packed_nevapr, kgenref_packed_nevapr) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_am_evp_st", check_status, packed_am_evp_st, &
                &kgenref_packed_am_evp_st) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_evapsnow", check_status, packed_evapsnow, &
                &kgenref_packed_evapsnow) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_prain", check_status, packed_prain, kgenref_packed_prain) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_prodsnow", check_status, packed_prodsnow, &
                &kgenref_packed_prodsnow) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_cmeout", check_status, packed_cmeout, kgenref_packed_cmeout) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qsout", check_status, packed_qsout, kgenref_packed_qsout) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_cflx", check_status, packed_cflx, kgenref_packed_cflx) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_iflx", check_status, packed_iflx, kgenref_packed_iflx) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_rflx", check_status, packed_rflx, kgenref_packed_rflx) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_sflx", check_status, packed_sflx, kgenref_packed_sflx) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qrout", check_status, packed_qrout, kgenref_packed_qrout) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qcsevap", check_status, packed_qcsevap, &
                &kgenref_packed_qcsevap) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qisevap", check_status, packed_qisevap, &
                &kgenref_packed_qisevap) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qvres", check_status, packed_qvres, kgenref_packed_qvres) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_cmei", check_status, packed_cmei, kgenref_packed_cmei) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_vtrmc", check_status, packed_vtrmc, kgenref_packed_vtrmc) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_vtrmi", check_status, packed_vtrmi, kgenref_packed_vtrmi) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qcsedten", check_status, packed_qcsedten, &
                &kgenref_packed_qcsedten) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qisedten", check_status, packed_qisedten, &
                &kgenref_packed_qisedten) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qrsedten", check_status, packed_qrsedten, &
                &kgenref_packed_qrsedten) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qssedten", check_status, packed_qssedten, &
                &kgenref_packed_qssedten) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_umr", check_status, packed_umr, kgenref_packed_umr) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_ums", check_status, packed_ums, kgenref_packed_ums) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_pra", check_status, packed_pra, kgenref_packed_pra) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_prc", check_status, packed_prc, kgenref_packed_prc) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_mnuccc", check_status, packed_mnuccc, kgenref_packed_mnuccc) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_mnucct", check_status, packed_mnucct, kgenref_packed_mnucct) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_msacwi", check_status, packed_msacwi, kgenref_packed_msacwi) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_psacws", check_status, packed_psacws, kgenref_packed_psacws) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_bergs", check_status, packed_bergs, kgenref_packed_bergs) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_berg", check_status, packed_berg, kgenref_packed_berg) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_melt", check_status, packed_melt, kgenref_packed_melt) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_homo", check_status, packed_homo, kgenref_packed_homo) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qcres", check_status, packed_qcres, kgenref_packed_qcres) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_prci", check_status, packed_prci, kgenref_packed_prci) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_prai", check_status, packed_prai, kgenref_packed_prai) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qires", check_status, packed_qires, kgenref_packed_qires) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_mnuccr", check_status, packed_mnuccr, kgenref_packed_mnuccr) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_pracs", check_status, packed_pracs, kgenref_packed_pracs) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_meltsdt", check_status, packed_meltsdt, &
                &kgenref_packed_meltsdt) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_frzrdt", check_status, packed_frzrdt, kgenref_packed_frzrdt) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_mnuccd", check_status, packed_mnuccd, kgenref_packed_mnuccd) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_nrout", check_status, packed_nrout, kgenref_packed_nrout) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_nsout", check_status, packed_nsout, kgenref_packed_nsout) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_refl", check_status, packed_refl, kgenref_packed_refl) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_arefl", check_status, packed_arefl, kgenref_packed_arefl) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_areflz", check_status, packed_areflz, kgenref_packed_areflz) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_frefl", check_status, packed_frefl, kgenref_packed_frefl) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_csrfl", check_status, packed_csrfl, kgenref_packed_csrfl) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_acsrfl", check_status, packed_acsrfl, kgenref_packed_acsrfl) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_fcsrfl", check_status, packed_fcsrfl, kgenref_packed_fcsrfl) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_rercld", check_status, packed_rercld, kgenref_packed_rercld) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_ncai", check_status, packed_ncai, kgenref_packed_ncai) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_ncal", check_status, packed_ncal, kgenref_packed_ncal) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qrout2", check_status, packed_qrout2, kgenref_packed_qrout2) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qsout2", check_status, packed_qsout2, kgenref_packed_qsout2) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_nrout2", check_status, packed_nrout2, kgenref_packed_nrout2) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_nsout2", check_status, packed_nsout2, kgenref_packed_nsout2) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_freqs", check_status, packed_freqs, kgenref_packed_freqs) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_freqr", check_status, packed_freqr, kgenref_packed_freqr) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_nfice", check_status, packed_nfice, kgenref_packed_nfice) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_prer_evap", check_status, packed_prer_evap, &
                &kgenref_packed_prer_evap) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_qcrat", check_status, packed_qcrat, kgenref_packed_qcrat) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_rel", check_status, packed_rel, kgenref_packed_rel) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_rei", check_status, packed_rei, kgenref_packed_rei) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_sadice", check_status, packed_sadice, kgenref_packed_sadice) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_sadsnow", check_status, packed_sadsnow, &
                &kgenref_packed_sadsnow) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_lambdac", check_status, packed_lambdac, &
                &kgenref_packed_lambdac) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_mu", check_status, packed_mu, kgenref_packed_mu) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_des", check_status, packed_des, kgenref_packed_des) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("packed_dei", check_status, packed_dei, kgenref_packed_dei) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("rel_fn_dum", check_status, rel_fn_dum, kgenref_rel_fn_dum) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("dsout2_dum", check_status, dsout2_dum, kgenref_dsout2_dum) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("drout_dum", check_status, drout_dum, kgenref_drout_dum) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("reff_rain_dum", check_status, reff_rain_dum, kgenref_reff_rain_dum) 
                CALL kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2("reff_snow_dum", check_status, reff_snow_dum, kgenref_reff_snow_dum) 
                CALL kv_micro_mg_cam_tend_pack_character_128_("errstring", check_status, errstring, kgenref_errstring) 
                if(check_status%rank==0) WRITE (*, *) "" 
                IF (check_status%verboseLevel > 0) THEN 
                    if(check_status%rank==0) then
                    WRITE (*, *) "Number of output variables: ", check_status%numTotal 
                    WRITE (*, *) "Number of identical variables: ", check_status%numIdentical 
                    WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol 
                    WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol 
                    WRITE (*, *) "Tolerance: ", kgen_tolerance 
                    endif
                END IF   
                if(check_status%rank==0) WRITE (*, *) "" 
                IF (check_status%numOutTol > 0) THEN 
                    if(check_status%rank==0) WRITE (*, *) "Verification FAILED for (" // TRIM(filename) //")"
                    check_status%Passed = .FALSE. 
                    kgen_isverified = .FALSE. 
                ELSE 
                    if(check_status%rank==0) WRITE (*, *) " Verification PASSED for (" // TRIM(filename) //")" 
                    check_status%Passed = .TRUE. 
                    kgen_isverified = .TRUE. 
                END IF   
                if(check_status%rank==0) WRITE (*, *) "" 
#ifdef _MPI
                call MPI_Barrier(MPI_COMM_WORLD,info)
#endif
                CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock) 
            !call ftrace_region_begin('micro_mg_tend2_0')
            do kgen_intvar=1,maxiter
            call micro_mg_tend2_0( &
                 mgncol,         nlev,           dtime/num_steps,&
                 packed_t,               packed_q,               &
                 packed_qc,              packed_qi,              &
                 packed_nc,              packed_ni,              &
                 packed_qr,              packed_qs,              &
                 packed_nr,              packed_ns,              &
                 packed_relvar,          packed_accre_enhan,     &
                 packed_p,               packed_pdel,            &
                 packed_cldn, packed_liqcldf, packed_icecldf, packed_qsatfac, &
                 packed_rate1ord_cw2pr_st,                       &
                 packed_naai,            packed_npccn,           &
                 packed_rndst,           packed_nacon,           &
                 packed_tlat,            packed_qvlat,           &
                 packed_qctend,          packed_qitend,          &
                 packed_nctend,          packed_nitend,          &
                 packed_qrtend,          packed_qstend,          &
                 packed_nrtend,          packed_nstend,          &
                 packed_rel,     rel_fn_dum,     packed_rei,     &
                 packed_sadice,          packed_sadsnow,         &
                 packed_prect,           packed_preci,           &
                 packed_nevapr,          packed_evapsnow,        &
                 packed_am_evp_st,                               &
                 packed_prain,           packed_prodsnow,        &
                 packed_cmeout,          packed_dei,             &
                 packed_mu,              packed_lambdac,         &
                 packed_qsout,           packed_des,             &
                 packed_cflx,    packed_iflx,                    &
                 packed_rflx,    packed_sflx,    packed_qrout,   &
                 reff_rain_dum,          reff_snow_dum,          &
                 packed_qcsevap, packed_qisevap, packed_qvres,   &
                 packed_cmei,    packed_vtrmc,   packed_vtrmi,   &
                 packed_umr,             packed_ums,             &
                 packed_qcsedten,        packed_qisedten,        &
                 packed_qrsedten,        packed_qssedten,        &
                 packed_pra,             packed_prc,             &
                 packed_mnuccc,  packed_mnucct,  packed_msacwi,  &
                 packed_psacws,  packed_bergs,   packed_berg,    &
                 packed_melt,            packed_homo,            &
                 packed_qcres,   packed_prci,    packed_prai,    &
                 packed_qires,   packed_mnuccr,  packed_pracs,   &
                 packed_meltsdt, packed_frzrdt,  packed_mnuccd,  &
                 packed_nrout,           packed_nsout,           &
                 packed_refl,    packed_arefl,   packed_areflz,  &
                 packed_frefl,   packed_csrfl,   packed_acsrfl,  &
                 packed_fcsrfl,          packed_rercld,          &
                 packed_ncai,            packed_ncal,            &
                 packed_qrout2,          packed_qsout2,          &
                 packed_nrout2,          packed_nsout2,          &
                 drout_dum,              dsout2_dum,             &
                 packed_freqs,           packed_freqr,           &
                 packed_nfice,           packed_qcrat,           &
                 errstring, &
                 packed_tnd_qsnow,packed_tnd_nsnow,packed_re_ice,&
                 packed_prer_evap,                                     &
                 packed_frzimm,  packed_frzcnt,  packed_frzdep   )
            enddo
            !call ftrace_region_end('micro_mg_tend2_0')

            CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock) 
            kgen_measure = 1.0D6*(kgen_stop_clock - kgen_start_clock)/DBLE(kgen_rate_clock*maxiter) 
#ifdef _MPI
            call MPI_AllReduce(kgen_measure,gkgen_measure,1,MPI_REAL8,MPI_MAX,MPI_COMM_WORLD,info)
            if(check_status%rank==0) WRITE (*, *) "micro_mg_tend2_0 : Time per call (usec): ", gkgen_measure
            kgen_measure=gkgen_measure
#else
            if(check_status%rank==0) WRITE (*, *) "micro_mg_tend2_0 : Time per call (usec): ", kgen_measure
#endif
            END IF   
            IF (kgen_warmupstage) THEN 
            END IF   
            IF (kgen_evalstage) THEN 
            END IF   

   ! Divide ptend by substeps.

   ! Use summed outputs to produce averages


   ! Check to make sure that the microphysics code is respecting the flags that control
   ! whether MG should be prognosing cloud ice and cloud liquid or not.


   !add condensate fluxes for MG2 (ice and snow already added for MG1)


   !! calculate effective radius of convective liquid and ice using dcon and deicon (not used by code, not useful for COSP)
   !! hard-coded as average of hard-coded values used for deep/shallow convective detrainment (near line 1502/1505)

   ! Reassign rate1 if modal aerosols


   ! Sedimentation velocity for liquid stratus cloud droplet

   ! Microphysical tendencies for use in the macrophysics at the next time step

   ! Net micro_mg_cam condensation rate

   ! For precip, accumulate only total precip in prec_pcw and snow_pcw variables.
   ! Other precip output variables are set to 0
   ! Do not subscript by ncol here, because in physpkg we divide the whole
   ! array and need to avoid an FPE due to uninitialized data.


   ! ------------------------------------------------------------ !
   ! Compute in cloud ice and liquid mixing ratios                !
   ! Note that 'iclwp, iciwp' are used for radiation computation. !
   ! ------------------------------------------------------------ !


   ! Calculate cloud fraction for prognostic precip sizes.


   ! ------------------------------------------------------ !
   ! ------------------------------------------------------ !
   ! All code from here to the end is on grid columns only  !
   ! ------------------------------------------------------ !
   ! ------------------------------------------------------ !
   ! Average the fields which are needed later in this paramterization to be on the grid


   ! If on subcolumns, average the rest of the pbuf fields which were modified on subcolumns but are not used further in
   ! this parameterization  (no need to assign in the non-subcolumn case -- the else step)


   ! ------------------------------------- !
   ! Size distribution calculation         !
   ! ------------------------------------- !
   ! Calculate rho (on subcolumns if turned on) for size distribution
   ! parameter calculations and average it if needed
   ! State instead of state_loc to preserve answers for MG1 (and in any
   ! case, it is unlikely to make much difference).


   !

   ! Effective radius for cloud liquid, fixed number.


   ! Effective radius for cloud liquid, and size parameters
   ! mu_grid and lambdac_grid.

   ! Calculate ncic on the grid


   ! Rain/Snow effective diameter.


   ! Effective radius and diameter for cloud ice.


   ! Limiters for low cloud fraction.


   ! ------------------------------------- !
   ! Precipitation efficiency Calculation  !
   ! ------------------------------------- !
   !-----------------------------------------------------------------------
   ! Liquid water path
   ! Compute liquid water paths, and column condensation


   ! note: 1e-6 kgho2/kgair/s * 1000. pa / (9.81 m/s2) / 1000 kgh2o/m3 = 1e-7 m/s
   ! this is 1ppmv of h2o in 10hpa
   ! alternatively: 0.1 mm/day * 1.e-4 m/mm * 1/86400 day/s = 1.e-9
   !-----------------------------------------------------------------------
   ! precipitation efficiency calculation  (accumulate cme and precip)


   ! zero out precip efficiency and total averaged precip

   ! accumulate precip and condensation


   !-----------------------------------------------------------------------
   ! vertical average of non-zero accretion, autoconversion and ratio.
   ! vars: vprco_grid(i),vprao_grid(i),racau_grid(i),cnt_grid


   ! --------------------- !
   ! History Output Fields !
   ! --------------------- !
   ! Column droplet concentration


   ! Averaging for new output fields


   ! Cloud top effective radius and number.


   ! Evaporation of stratiform precipitation fields for UNICON


   ! Assign the values to the pbuf pointers if they exist in pbuf


   ! --------------------------------------------- !
   ! General outfield calls for microphysics       !
   ! --------------------------------------------- !
   ! Output a handle of variables which are calculated on the fly


   ! Output fields which have not been averaged already, averaging if use_subcol_microp is true


   ! Example subcolumn outfld call


   ! Output fields which are already on the grid


   ! ptend_loc is deallocated in physics_update above

              
            CONTAINS 
              

            !read state subroutine for kr_micro_mg_cam_tend_pack_real__rkind_comp_dim2 
            SUBROUTINE kr_micro_mg_cam_tend_pack_real__rkind_comp_dim2(var, kgen_unit, printname, printvar) 
                REAL(KIND=rkind_comp), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
                INTEGER, INTENT(IN) :: kgen_unit 
                CHARACTER(LEN=*), INTENT(IN) :: printname 
                LOGICAL, INTENT(IN), OPTIONAL :: printvar 
                LOGICAL :: kgen_istrue 
                REAL(KIND=8) :: kgen_array_sum 
                INTEGER :: idx1, idx2 
                INTEGER, DIMENSION(2,2) :: kgen_bound 
                REAL(KIND=rkind_io), allocatable,  DIMENSION(:,:) :: var_io
     
                  
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
                    ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
                    ALLOCATE (var_io(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
                    READ (UNIT = kgen_unit) var_io;var = real(var_io,kind=rkind_comp) 
                    CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var_io, mask=.TRUE.)), .TRUE.) 
                    IF (PRESENT( printvar ) .AND. printvar) THEN 
                        WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=.TRUE.)) 
                    END IF   
                    deallocate(var_io)
                END IF   
         
            END SUBROUTINE kr_micro_mg_cam_tend_pack_real__rkind_comp_dim2 
              
            !read state subroutine for kr_micro_mg_cam_tend_pack_real__rkind_comp_dim3 
            SUBROUTINE kr_micro_mg_cam_tend_pack_real__rkind_comp_dim3(var, kgen_unit, printname, printvar) 
                REAL(KIND=rkind_comp), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:) :: var 
                INTEGER, INTENT(IN) :: kgen_unit 
                CHARACTER(LEN=*), INTENT(IN) :: printname 
                LOGICAL, INTENT(IN), OPTIONAL :: printvar 
                LOGICAL :: kgen_istrue 
                REAL(KIND=8) :: kgen_array_sum 
                INTEGER :: idx1, idx2, idx3 
                INTEGER, DIMENSION(2,3) :: kgen_bound 
                REAL(KIND=rkind_io), ALLOCATABLE, DIMENSION(:,:,:) :: var_io
                  
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
                    ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), &
                    &kgen_bound(1,3):kgen_bound(2,3))) 
                    ALLOCATE (var_io(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), &
                    &kgen_bound(1,3):kgen_bound(2,3))) 
                    READ (UNIT = kgen_unit) var_io; var = REAL(var_io,kind=rkind_comp) 
                    CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var_io, mask=.TRUE.)), .TRUE.) 
                    IF (PRESENT( printvar ) .AND. printvar) THEN 
                        WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var_io, mask=.TRUE.)) 
                    END IF   
                    DEALLOCATE(var_io)
                END IF   
            END SUBROUTINE kr_micro_mg_cam_tend_pack_real__rkind_comp_dim3 
              
            !verify state subroutine for kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2 
            RECURSIVE SUBROUTINE kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2(varname, check_status, var, kgenref_var) 
                CHARACTER(LEN=*), INTENT(IN) :: varname 
                TYPE(check_t), INTENT(INOUT) :: check_status 
                REAL(KIND=rkind_comp), INTENT(IN), DIMENSION(:,:) :: var
                REAL(KIND=rkind_io),   INTENT(IN), DIMENSION(:,:) ::  kgenref_var 
                INTEGER :: check_result 
                LOGICAL :: is_print = .FALSE. 
                  
                INTEGER :: idx1, idx2 
                INTEGER :: n 
                real(KIND=rkind_comp) :: nrmsdiff, rmsdiff 
                real(KIND=rkind_comp), ALLOCATABLE :: buf1(:,:), buf2(:,:) 
                  
                check_status%numTotal = check_status%numTotal + 1 
                  
                IF (ALL(var == kgenref_var)) THEN 
                    check_status%numIdentical = check_status%numIdentical + 1 
                    IF (check_status%verboseLevel > 1) THEN 
                        if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                    END IF   
                    check_result = CHECK_IDENTICAL 
                ELSE 
                    ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
                    ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
                    n = SIZE(var) 
                    !n = COUNT(var /= kgenref_var) 
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
                        IF (check_status%verboseLevel > 1) THEN 
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
                  
            END SUBROUTINE kv_micro_mg_cam_tend_pack_real__rkind_comp_dim2 
              
            !verify state subroutine for kv_micro_mg_cam_tend_pack_real__rkind_comp_dim1 
            RECURSIVE SUBROUTINE kv_micro_mg_cam_tend_pack_real__rkind_comp_dim1(varname, check_status, var, kgenref_var) 
                CHARACTER(LEN=*), INTENT(IN) :: varname 
                TYPE(check_t), INTENT(INOUT) :: check_status 
                REAL(KIND=rkind_comp), INTENT(IN), DIMENSION(:) :: var 
                REAL(KIND=rkind_io), INTENT(IN), DIMENSION(:) :: kgenref_var 
                INTEGER :: check_result 
                LOGICAL :: is_print = .FALSE. 
                  
                INTEGER :: idx1 
                INTEGER :: n 
                real(KIND=rkind_comp) :: nrmsdiff, rmsdiff 
                real(KIND=rkind_comp), ALLOCATABLE :: buf1(:), buf2(:) 
                  
                check_status%numTotal = check_status%numTotal + 1 
                  
                IF (ALL(var == kgenref_var)) THEN 
                    check_status%numIdentical = check_status%numIdentical + 1 
                    IF (check_status%verboseLevel > 1) THEN 
                        if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                    END IF   
                    check_result = CHECK_IDENTICAL 
                ELSE 
                    ALLOCATE (buf1(SIZE(var,dim=1))) 
                    ALLOCATE (buf2(SIZE(var,dim=1))) 
                    !n = COUNT(var /= kgenref_var) 
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
                  
            END SUBROUTINE kv_micro_mg_cam_tend_pack_real__rkind_comp_dim1 
              
            !verify state subroutine for kv_micro_mg_cam_tend_pack_character_128_ 
            RECURSIVE SUBROUTINE kv_micro_mg_cam_tend_pack_character_128_(varname, check_status, var, kgenref_var) 
                CHARACTER(LEN=*), INTENT(IN) :: varname 
                TYPE(check_t), INTENT(INOUT) :: check_status 
                CHARACTER(LEN=128), INTENT(IN) :: var, kgenref_var 
                INTEGER :: check_result 
                LOGICAL :: is_print = .FALSE. 
                  
                character(LEN=128) :: diff 
                  
                check_status%numTotal = check_status%numTotal + 1 
                  
                IF (var == kgenref_var) THEN 
                    check_status%numIdentical = check_status%numIdentical + 1 
                    IF (check_status%verboseLevel > 1) THEN 
                        if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                    END IF   
                    check_result = CHECK_IDENTICAL 
                ELSE 
                    check_status%numOutTol = check_status%numOutTol + 1 
                    IF (check_status%verboseLevel > 1) THEN 
                        if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL." 
                    END IF   
                    check_result = CHECK_OUT_TOL 
                END IF   
                IF (check_result == CHECK_IDENTICAL) THEN 
                    IF (check_status%verboseLevel > 2) THEN 
                        if(check_status%rank==0) then 
                        WRITE (*, *) "NOT IMPLEMENTED" 
                        WRITE (*, *) "" 
                        endif
                    END IF   
                ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                    IF (check_status%verboseLevel > 0) THEN 
                        if(check_status%rank==0) then 
                        WRITE (*, *) "NOT IMPLEMENTED" 
                        WRITE (*, *) "" 
                        endif
                    END IF   
                ELSE IF (check_result == CHECK_IN_TOL) THEN 
                    IF (check_status%verboseLevel > 1) THEN 
                        if(check_status%rank==0) then 
                        WRITE (*, *) "NOT IMPLEMENTED" 
                        WRITE (*, *) "" 
                        endif
                    END IF   
                END IF   
                  
            END SUBROUTINE kv_micro_mg_cam_tend_pack_character_128_ 
              
END SUBROUTINE micro_mg_cam_tend_pack 


!read state subroutine for kr_externs_in_micro_mg_cam 
SUBROUTINE kr_externs_in_micro_mg_cam(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) num_steps 
!    print *,'kr_externs_in_micro_mg_cam: num_steps: ',num_steps
END SUBROUTINE kr_externs_in_micro_mg_cam 
  
!read state subroutine for kr_externs_out_micro_mg_cam 
SUBROUTINE kr_externs_out_micro_mg_cam(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
END SUBROUTINE kr_externs_out_micro_mg_cam 
  
end module micro_mg_cam
