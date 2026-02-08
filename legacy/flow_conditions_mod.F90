!====================================================================================================
! Convert stagnation to freestream conditions
! date: 02/18/2015
!====================================================================================================
module flow_conditions_mod
  !
  ! load modules
  use params
  use util_mod
  use viscosity_mod
  use dimension_conversion_mod
  use gas_properties_mod
  use standard_atmosphere_mod
  !
  implicit none
  !
  ! global variables
  character*128      :: type_name(10)
  integer, parameter :: calc_pres = 1
  integer, parameter :: calc_dens = 2
  integer, parameter :: calc_temp = 3
  integer, parameter :: action_wr_screen = 1
  integer, parameter :: action_wr_file = 2
  integer, parameter :: action_rd_file = 3
  !
  ! contains statement -> include subroutines after contains statement
contains
  !
  !====================================================================================================
  ! wrapper routine to initialize flow conditions calculation
  !====================================================================================================
  subroutine flow_conditions_ini(debug)
    !
    ! input variables
    logical, intent(inout) :: debug
    ! local variables
    type(t_state) :: stat,stag
    !
    call flow_conditions(stat,stag,debug)
    !
  end subroutine flow_conditions_ini
  !====================================================================================================
  ! compute full set of flow conditions
  !====================================================================================================
  subroutine flow_conditions(stat,stag,debug)
    !
    implicit none
    !
    ! input/output variables
    logical, intent(inout)              :: debug
    type(t_state), intent(out)          :: stat
    type(t_state), intent(out)          :: stag
    ! local variables
    character*128                       :: dim
    integer                             :: ierr
    real(kind=dp)                              :: re1
    integer                             :: key
    real(kind=dp)                              :: visc_lo, visc_hi, visc_nu
    real(kind=dp)                              :: temp_stag_lo, temp_stag_hi, temp_stag_nu
    real(kind=dp)                              :: temp_stat_lo, temp_stat_hi, temp_stat_nu
    real(kind=dp)                              :: re1_lo, re1_hi, re1_nu
    real(kind=dp)                              :: mach_lo, mach_hi, mach_nu
    real(kind=dp)                              :: pres_stat_lo, pres_stat_hi, pres_stat_nu
    real(kind=dp)                              :: error
    real(kind=dp)                              :: tol
    integer                             :: itermax
    integer                             :: iter
    character*512                       :: message
    integer                             :: visc_option
    real(kind=dp)                              :: lref
    real(kind=dp)                              :: tref
    integer                             :: pres_type
    integer                             :: temp_type
    integer                             :: dens_type
    real(kind=dp)                              :: dummy
    integer                             :: funit
    integer                             :: ifile
    integer                             :: n_temp
    integer                             :: n_pres
    real(kind=dp), dimension(:,:), allocatable :: stat_pres, stag_pres
    real(kind=dp), dimension(:,:), allocatable :: stat_temp, stag_temp
    real(kind=dp), dimension(:,:), allocatable :: mu
    real(kind=dp), dimension(:,:), allocatable :: unit_re
    real(kind=dp)                              :: ps_stat,pe_stat,dp_stat
    real(kind=dp)                              :: ps_stag,pe_stag,dp_stag
    real(kind=dp)                              :: ts_stat,te_stat,dt_stat
    real(kind=dp)                              :: ts_stag,te_stag,dt_stag
    integer                             :: i,j
    integer                             :: val
    real(kind=dp)                              :: h
    real(kind=dp)                              :: feet2meters, miles2meters
    character*128                       :: fname
    real(kind=dp)                              :: h_lo, h_hi
    type(t_state) :: stat_lo, stat_hi
    real(kind=dp) :: err, err_lo, err_hi
    integer :: n_alt
    real(kind=dp) :: alt_s, alt_e, d_alt
    real(kind=dp), dimension(:), allocatable :: alt
    integer :: n_re1
    real(kind=dp) :: re1_s, re1_e, d_re1
    !
    ! set conversion factors
    feet2meters = 0.3048
    miles2meters = 1609.34
    !
    ! set reference length scale
    lref = 1.d0
    !
    ! initialize states
    stat%uvel = -99
    stat%vvel = -99
    stat%wvel = -99
    stat%temp = -99
    stat%pres = -99
    stat%dens = -99
    !
    stag%uvel = -99
    stag%vvel = -99
    stag%wvel = -99
    stag%temp = -99
    stag%pres = -99
    stag%dens = -99    
    !
    ! get user input
    !
    ! gas properties
    call get_gas_properties
    !
    ! set option on how to compute the viscosity (either Standard or with low temperature correciton)
    call select_visc_option(visc_option)
    !
    write(*,'(a)')'select option:'
    write(*,'(a)')'( 1) Compute unit Reynolds with pressure and temperature input'
    write(*,'(a)')'( 2) Compute pressure value with unit Reynolds number and temperature input'
    write(*,'(a)')'( 3) Compute temperature value with unit Reynolds number and pressure input'
    write(*,'(a)')'( 4) Compute unit reynolds number for a pressrue range and a temprature range'
    write(*,'(a)')'( 5) Compute Mach number from Reynolds number, temperature and pressure value'
    write(*,'(a)')'( 6) Compute full set of conditions for given Mach number and standard atmosphere altitude'
    write(*,'(a)')'( 7) Compute full set of conditions for pressure, temperature and u-velocity input'
    write(*,'(a)')'( 8) Compute full set of conditions for a standard atmosphere with Mach number and unit Reynolds number input'
    write(*,'(a)')'( 9) Compute trajectory data (mach number and altitude range input)'
    write(*,'(a)')'(10) Compute trajectory data (mach number and re1 range input)'
    read(*,*)key
    !
    if(key.eq.1) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute unit reynolds with p and T input
       !----------------------------------------------------------------------------------------------------
       !
       ! mach number
       call get_mach_number(stat%mach)
       !
       write(*,*)'pressure type: '
       write(*,'(A,I1,A)')'(',stagnation,') stagnation pressure'
       write(*,'(A,I1,A)')'(',freestream,') freestream pressure'
       read(*,*)pres_type
       !
       call get_pres_val(dummy,pres_type)
       !
       if(pres_type.eq.stagnation) then
          !
          stag%pres = dummy
          call stag_stat_conversion(stat%mach,gamma,stag%pres,stat%pres,pres_id,stag2stat,.false.)
          !
       elseif(pres_type.eq.freestream) then
          !
          stat%pres = dummy
          call stag_stat_conversion(stat%mach,gamma,stat%pres,stag%pres,pres_id,stat2stag,.false.)
          !
       end if
       !
       write(*,*)'temperature type: '
       write(*,'(A,I1,A)')'(',stagnation,') stagnation temperature'
       write(*,'(A,I1,A)')'(',freestream,') freestream temperature'
       read(*,*)temp_type
       !
       call get_temp_val(dummy,temp_type)
       !
       if(temp_type.eq.stagnation) then
          !
          stag%temp = dummy
          call stag_stat_conversion(stat%mach,gamma,stag%temp,stat%temp,temp_id,stag2stat,.false.)
          !
       elseif(temp_type.eq.freestream) then
          !
          stat%temp = dummy
          call stag_stat_conversion(stat%mach,gamma,stat%temp,stag%temp,temp_id,stat2stag,.false.)
          !
       end if
       !
    elseif(key.eq.2) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute pressure value with re1 and T input
       !----------------------------------------------------------------------------------------------------
       !
       ! mach number
       call get_mach_number(stat%mach)
       !
       write(*,*)'Unit Reynolds number'
       read(*,*)re1
       !
       write(*,*)'Temperature type: '
       write(*,'(A,I1,A)')'(',stagnation,') stagnation temperature'
       write(*,'(A,I1,A)')'(',freestream,') freestream temperature'
       read(*,*)temp_type
       !
       call get_temp_val(dummy,temp_type)
       !
       if(temp_type.eq.stagnation) then
          !
          stag%temp = dummy
          call stag_stat_conversion(stat%mach,gamma,stag%temp,stat%temp,temp_id,stag2stat,.false.)
          !
       elseif(temp_type.eq.freestream) then
          !
          stat%temp = dummy
          call stag_stat_conversion(stat%mach,gamma,stat%temp,stag%temp,temp_id,stat2stag,.false.)
          !
       end if
       !
    elseif(key.eq.3) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute temperature value with re1 and p input
       !----------------------------------------------------------------------------------------------------
       !
       ! mach number
       call get_mach_number(stat%mach)
       !
       write(*,*)'Unit Reynolds number'
       read(*,*)re1
       !
       write(*,*)'pressure type: '
       write(*,'(A,I1,A)')'(',stagnation,') stagnation pressure'
       write(*,'(A,I1,A)')'(',freestream,') freestream pressure'
       read(*,*)pres_type
       !
       call get_pres_val(dummy,pres_type)
       !
       if(pres_type.eq.stagnation) then
          !
          stag%pres = dummy
          call stag_stat_conversion(stat%mach,gamma,stag%pres,stat%pres,pres_id,stag2stat,.false.)
          !
       elseif(pres_type.eq.freestream) then
          !
          stat%pres = dummy
          call stag_stat_conversion(stat%mach,gamma,stat%pres,stag%pres,pres_id,stat2stag,.false.)
          !
       end if
       !
    elseif(key.eq.4) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute unit reynolds number for a range of pressure and temperature values
       !----------------------------------------------------------------------------------------------------
       !
       ! mach number
       call get_mach_number(stat%mach)
       !
       write(*,'(a)')'Number of pressure values'
       read(*,*)n_pres
       !
       write(*,'(a)')'pressure type: '
       write(*,'(a,i1,a)')'(',stagnation,') stagnation pressure'
       write(*,'(a,i1,a)')'(',freestream,') freestream pressure'
       read(*,*)pres_type
       !
       write(*,'(a)')'start value for pressure: '
       call get_pres_val(dummy,pres_type)
       !
       if(pres_type.eq.stagnation) then
          !
          ps_stag = dummy
          call stag_stat_conversion(stat%mach,gamma,ps_stag,ps_stat,pres_id,stag2stat,.false.)
          !
       elseif(pres_type.eq.freestream) then
          !
          ps_stat = dummy
          call stag_stat_conversion(stat%mach,gamma,ps_stat,ps_stag,pres_id,stat2stag,.false.)
          !
       end if
       !
       write(*,'(a)')'end value for pressure: '
       call get_pres_val(dummy,pres_type)       
       !
       if(pres_type.eq.stagnation) then
          !
          pe_stag = dummy
          call stag_stat_conversion(stat%mach,gamma,pe_stag,pe_stat,pres_id,stag2stat,.false.)
          !
       elseif(pres_type.eq.freestream) then
          !
          pe_stat = dummy
          call stag_stat_conversion(stat%mach,gamma,pe_stat,pe_stag,pres_id,stat2stag,.false.)
          !
       end if
       !
       write(*,'(a)')'Number of temperature values'
       read(*,*)n_temp
       !
       write(*,*)'temperature type: '
       write(*,'(A,I1,A)')'(',stagnation,') stagnation temperature'
       write(*,'(A,I1,A)')'(',freestream,') freestream temperature'
       read(*,*)temp_type
       !
       write(*,'(a)')'start value for temperature: '
       call get_temp_val(dummy,temp_type)
       !
       if(temp_type.eq.stagnation) then
          !
          ts_stag = dummy
          call stag_stat_conversion(stat%mach,gamma,ts_stag,ts_stat,temp_id,stag2stat,.false.)
          !
       elseif(temp_type.eq.freestream) then
          !
          ts_stat = dummy
          call stag_stat_conversion(stat%mach,gamma,ts_stat,ts_stag,temp_id,stat2stag,.false.)
          !
       end if
       !
       write(*,'(a)')'end value for temperature: '
       call get_temp_val(dummy,temp_type)
       !
       if(temp_type.eq.stagnation) then
          !
          te_stag = dummy
          call stag_stat_conversion(stat%mach,gamma,te_stag,te_stat,temp_id,stag2stat,.false.)
          !
       elseif(temp_type.eq.freestream) then
          !
          te_stat = dummy
          call stag_stat_conversion(stat%mach,gamma,te_stat,te_stag,temp_id,stat2stag,.false.)
          !
       end if
       !
       ! compute step sizes 
       dp_stat = (pe_stat-ps_stat)/(n_pres-1)
       dp_stag = (pe_stag-ps_stag)/(n_pres-1)
       !
       dt_stat = (te_stat-ts_stat)/(n_temp-1)
       dt_stag = (te_stag-ts_stag)/(n_temp-1)
       !
       ! allocate arrays
       allocate(stag_pres(n_pres,n_temp))
       allocate(stat_pres(n_pres,n_temp))
       allocate(stag_temp(n_pres,n_temp))
       allocate(stat_temp(n_pres,n_temp))
       allocate(mu(n_pres,n_temp))
       allocate(unit_re(n_pres,n_temp))
       !
       funit = 200
       !
       open(unit=funit,file='re1_contours.dat',status='unknown')
       !
       write(funit,'(a)')'TITLE = "re1_contours"'
       write(funit,'(a)')'VARIABLES = "stag_pres_pa" "stag_pres_psi" "stat_pres_pa" "stag_temp_k" "stat_temp_k" "visc" "re1"'
       write(funit,'(a,i6,a,i6)')'ZONE T = "re1_contours", I = ',n_pres,', J = ',n_temp
       !
       ! loop over temperature values
       do j = 1,n_temp
          !
          ! loop over pressure values
          do i = 1,n_pres
             !
             stag_pres(i,j) = ps_stag + (i-1)*dp_stag
             stat_pres(i,j) = ps_stat + (i-1)*dp_stat
             stag_temp(i,j) = ts_stag + (j-1)*dt_stag
             stat_temp(i,j) = ts_stat + (j-1)*dt_stat
             !
             ! compute viscosity
             call viscosity(stat_temp(i,j),mu(i,j),visc_option)
             !
             unit_re(i,j) = stat_pres(i,j)*stat%mach*sqrt(gamma)/(mu(i,j)*sqrt(Rgas*stat_temp(i,j)))
             !
             call PAS2PSI_conversion(stag_pres(i,j),dummy)
             !
             write(funit,'(7e30.16)')stag_pres(i,j),dummy,stat_pres(i,j),stag_temp(i,j),stat_temp(i,j),mu(i,j),unit_re(i,j)
             !
             ! end i-loop
          end do
          !
          ! end j-loop
       end do
       !
       close(funit)
       !
    elseif(key.eq.5) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute Mach number with Reynolds number, temperature and pressure input
       !----------------------------------------------------------------------------------------------------
       !
       pres_type = stagnation
       call get_pres_val(stag%pres,pres_type)
       !
       temp_type = stagnation
       call get_temp_val(stag%temp,temp_type)
       !
       write(*,*)'unit Reynolds number (re1): '
       read(*,*)re1
       !
    elseif(key.eq.6) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute full set of conditions for standard atmosphere and mach number input
       !----------------------------------------------------------------------------------------------------
       !
       call get_mach_number(stat%mach)
       !
       write(*,'(a)')'Altitude value, dimension (meters,m, feet, ft, miles): '
       write(*,'(a)')'Note: dimension specifier is not case sensitive'
       read(*,*)val,dim
       !
       call low_cap(dim,cap2low)
       !
       if(trim(dim).eq.'m'.or.trim(dim).eq.'meters') then
          !
          ! no conversion needed
          h = val
          !
       elseif(trim(dim).eq.'ft'.or.trim(dim).eq.'feet') then
          !
          ! convert from feet to meters
          h = val*feet2meters
          !
       elseif(trim(dim).eq.'miles') then
          !
          ! convert from feet to meters
          h = val*miles2meters
          !
       end if
       !
       ! calculate conditions for standard earth atmosphere
       call standard_atmosphere_state(h,stat,debug)
       !
       ! calculate viscosity
       call viscosity(stat%temp,stat%visc,visc_option)
       !
       ! compute unit Reynolds number
       re1 = stat%pres*stat%mach*sqrt(gamma)/(stat%visc*sqrt(Rgas*stat%temp))
       !
       call stag_stat_conversion(stat%mach,gamma,stat%pres,stag%pres,pres_id,stat2stag,.false.)
       call stag_stat_conversion(stat%mach,gamma,stat%temp,stag%temp,temp_id,stat2stag,.false.)
       !
    elseif(key.eq.7) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute full set of flow conditions for pressure, temperature and density input
       !----------------------------------------------------------------------------------------------------
       write(*,*)'pressure type: '
       write(*,'(a,i1,a)')'(',stagnation,') stagnation pressure'
       write(*,'(a,i1,a)')'(',freestream,') freestream pressure'
       read(*,*)pres_type
       !
       call get_pres_val(dummy,pres_type)
       !
       if(pres_type.eq.stagnation) then
          !
          stag%pres = dummy
          !
       elseif(pres_type.eq.freestream) then
          !
          stat%pres = dummy
          !
       end if
       !
       write(*,*)'temperature type: '
       write(*,'(a,i1,a)')'(',stagnation,') stagnation temperature'
       write(*,'(a,i1,a)')'(',freestream,') freestream temperature'
       read(*,*)temp_type
       !
       call get_temp_val(dummy,temp_type)
       !
       if(temp_type.eq.stagnation) then
          !
          stag%temp = dummy
          !
       elseif(temp_type.eq.freestream) then
          !
          stat%temp = dummy
          !
       end if
       !
       write(*,*)'freestream velocity (in m/s):'
       read(*,*)stat%uvel
       !
       ! calculate speed of sound
       !
       stat%sos = sqrt(gamma*Rgas*stat%temp)
       !
       ! calculate mach number
       stat%mach = stat%uvel/stat%sos
       !
       if(pres_type.eq.stagnation) then
          !
          call stag_stat_conversion(stat%mach,gamma,stag%pres,stat%pres,pres_id,stag2stat,.false.)
          !
       elseif(pres_type.eq.freestream) then
          !
          call stag_stat_conversion(stat%mach,gamma,stat%pres,stag%pres,pres_id,stat2stag,.false.)
          !
       end if
       !
       if(temp_type.eq.stagnation) then
          !
          call stag_stat_conversion(stat%mach,gamma,stag%temp,stat%temp,temp_id,stag2stat,.false.)
          !
       elseif(temp_type.eq.freestream) then
          !
          call stag_stat_conversion(stat%mach,gamma,stat%temp,stag%temp,temp_id,stat2stag,.false.)
          !
       end if
       !
       call viscosity(stat%temp,stat%visc,visc_option)
       !
       ! calculate density from equation of state
       stat%dens = stat%pres/(Rgas*stat%temp)       
       !
       ! compute unit Reynolds number
       re1 = stat%pres*stat%mach*sqrt(gamma)/(stat%visc*sqrt(Rgas*stat%temp))
       !
    elseif(key.eq.8) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute full set of conditions for standard atmosphere with mach number and re1 input
       !----------------------------------------------------------------------------------------------------
       !
       call get_mach_number(stat%mach)
       !
       write(*,'(a)')'Unit Reynolds number (Re1) in 1/m: '
       read(*,*)re1
       !
       ! iterated to find proper height to obtain specified unit reynolds number
       !
       ! initialize iteration
       iter = 1
       !
       tol = 1e-12
       !
       itermax = 100
       !
       err = 1e10
       !
       ! bracket altitude values
       h_lo = 0
       h_hi = 30000
       !
       stat_lo%mach = stat%mach
       stat_hi%mach = stat%mach
       !
       do while (abs(err).gt.tol)
          !
          h = 0.5*(h_lo+h_hi)
          !
          ! alculate atmoshpheric pressure and temperature for lower guess
          call standard_atmosphere_state(h_lo,stat_lo,.false.)
          !
          ! calculate atmospheric pressure and temperature for higher guess
          call standard_atmosphere_state(h_hi,stat_hi,.false.)
          !
          ! calculate atmospheric pressure and temperature for new guess
          call standard_atmosphere_state(h,stat,.false.)
          !
          ! calculate unit reynolds number for all 3 guesses
          !
          ! viscosity
          call viscosity(stat_lo%temp,stat_lo%visc,visc_option)
          call viscosity(stat_hi%temp,stat_hi%visc,visc_option)
          call viscosity(stat%temp,stat%visc,visc_option)
          !
          re1_lo = stat_lo%pres*stat_lo%mach*sqrt(gamma)/(stat_lo%visc*sqrt(Rgas*stat_lo%temp))
          re1_hi = stat_hi%pres*stat_hi%mach*sqrt(gamma)/(stat_hi%visc*sqrt(Rgas*stat_hi%temp))
          re1_nu = stat%pres*stat%mach*sqrt(gamma)/(stat%visc*sqrt(Rgas*stat%temp))
          !
          err = re1_nu-re1
          err_lo = re1_lo-re1
          err_hi = re1_hi-re1
          !
          if(debug) then
             !
             if(iter.eq.1) then
                !
                write(*,'(a)')'          iter |           h_lo |         err_lo |           h_hi |         err_hi |              h |            err '
                !
             end if
             !
             write(*,'(i14,a,e14.6,a,e14.6,a,e14.6,a,e14.6,a,e14.6,a,e14.6)')iter,' | ',h_lo,' | ',err_lo,' | ',h_hi,' | ',err_hi,' | ',h,' | ',err
             !
          end if
          !
          if(err*err_lo.gt.0) then
             !
             h_lo = h
             !
          else
             !
             h_hi = h
             !
          end if
          !
          iter = iter+1
          !
          if(iter.eq.itermax) then
             !
             write(*,'(a)')'No convergence achieved'
             exit
             !
          end if
          !
       end do
       !
       write(*,'(a,f20.10,a)')'altitude h = ',h,' m'
       !
       call stag_stat_conversion(stat%mach,gamma,stat%pres,stag%pres,pres_id,stat2stag,.false.)
       call stag_stat_conversion(stat%mach,gamma,stat%temp,stag%temp,temp_id,stat2stag,.false.)
       !
    elseif(key.eq.9) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute trajectory data (mach and altitude input)
       !----------------------------------------------------------------------------------------------------
       !
       call get_mach_number(stat%mach)
       !
       write(*,'(a)')'altitude dimension (m, meters, ft, miles); note: dimension specifier is not case sensitive'
       read(*,*)dim
       !
       call low_cap(dim,cap2low)
       !
       write(*,'(a)')'altitude range (alt_s, alt_e, n_alt):'
       read(*,*)alt_s,alt_e,n_alt
       !
       allocate(alt(n_alt))
       !
       d_alt = (alt_e-alt_s)/(dble(n_alt)-1.d0)
       !
       do i = 1,n_alt
          !
          alt(i) = alt_s+(i-1)*d_alt
          !
       end do
       !
       if(trim(dim).eq.'m'.or.trim(dim).eq.'meters') then
          !
          ! no conversion needed
          !
       elseif(trim(dim).eq.'ft'.or.trim(dim).eq.'feet') then
          !
          ! convert from feet to meters
          do i = 1,n_alt
             !
             alt(i) = alt(i)*feet2meters
             !
          end do
          !
       elseif(trim(dim).eq.'miles') then
          !
          ! convert from feet to meters
          do i = 1,n_alt
             !
             alt(i) = alt(i)*miles2meters
             !
          end do
          !
       end if
       !
       call util_find_free_funit(funit,.false.)
       !
       fname = 'trajectory_data.dat'
       !
       open(unit=funit,file=trim(fname),status='unknown')
       !
       write(funit,'(a)')'TITLE = "trajectory_data"'
       write(funit,'(a)')'VARIABLES = "alt" "mach" "re1"'
       write(funit,'(a)')'ZONE T = "trajectory_data"'
       !
       ! calculate conditions for standard earth atmosphere for each altitude
       do i = 1,n_alt
          !
          call standard_atmosphere_state(alt(i),stat,debug)
          !
          ! calculate viscosity
          call viscosity(stat%temp,stat%visc,visc_option)
          !
          ! compute unit Reynolds number
          re1 = stat%pres*stat%mach*sqrt(gamma)/(stat%visc*sqrt(Rgas*stat%temp))
          !
          write(funit,'(3e32.16)')alt(i),stat%mach,re1
          !
       end do
       !
       flush(funit)
       close(funit)
       !
       ! done here
       return
       !
    elseif(key.eq.10) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute trajectory data (mach and re1 input)
       !----------------------------------------------------------------------------------------------------
       !
       call get_mach_number(stat%mach)
       !
       write(*,'(a)')'re1 range (re1_s, re1_e, n_re1) - in 1/m:'
       read(*,*)re1_s,re1_e,n_re1
       !
       d_re1 = (re1_e-re1_s)/(dble(n_re1)-1.d0)
       !
       call util_find_free_funit(funit,.false.)
       !
       fname = 'trajectory_data.dat'
       !
       open(unit=funit,file=trim(fname),status='unknown')
       !
       write(funit,'(a)')'TITLE = "trajectory_data"'
       write(funit,'(a)')'VARIABLES = "alt" "mach" "re1"'
       write(funit,'(a)')'ZONE T = "trajectory_data"'
       !
       ! set tolerance for achieving convergence
       tol = 1e-7
       !
       ! set maximum number of allowed iteration
       itermax = 100
       !
       ! calculate conditions for standard earth atmosphere for each altitude
       do i = 1,n_re1
          !
          ! calculte unit reynolds number
          re1 = re1_s + (i-1)*d_re1
          !
          ! iterate to find proper height to obtain specified unit reynolds number for given mach number
          !
          ! initialize iteration counter
          iter = 1
          !
          err = 1e10
          !
          ! bracket altitude values
          h_lo = 0
          h_hi = 50000
          !
          ! set mach number for lo and hi estimates
          stat_lo%mach = stat%mach
          stat_hi%mach = stat%mach
          !
          ! iterate
          do while (abs(err).gt.tol)
             !
             ! calculte new altitude
             h = 0.5*(h_lo+h_hi)
             !
             ! alculate atmoshpheric pressure and temperature for lower guess
             call standard_atmosphere_state(h_lo,stat_lo,.false.)
             !
             ! calculate atmospheric pressure and temperature for higher guess
             call standard_atmosphere_state(h_hi,stat_hi,.false.)
             !
             ! calculate atmospheric pressure and temperature for new guess
             call standard_atmosphere_state(h,stat,.false.)
             !
             ! calculate unit reynolds number for all 3 guesses
             !
             ! viscosity
             call viscosity(stat_lo%temp,stat_lo%visc,visc_option)
             call viscosity(stat_hi%temp,stat_hi%visc,visc_option)
             call viscosity(stat%temp,stat%visc,visc_option)
             !
             ! calculte reynolds number for all three guesses
             re1_lo = stat_lo%pres*stat_lo%mach*sqrt(gamma)/(stat_lo%visc*sqrt(Rgas*stat_lo%temp))
             re1_hi = stat_hi%pres*stat_hi%mach*sqrt(gamma)/(stat_hi%visc*sqrt(Rgas*stat_hi%temp))
             re1_nu = stat%pres*stat%mach*sqrt(gamma)/(stat%visc*sqrt(Rgas*stat%temp))
             !
             ! calculte error for all 3 guesses
             err = re1_nu-re1
             err_lo = re1_lo-re1
             err_hi = re1_hi-re1
             !
             if(debug) then
                !
                if(iter.eq.1) then
                   !
                   write(*,'(a)')'          iter |           h_lo |         err_lo |           h_hi |         err_hi |              h |            err '
                   !
                end if
                !
                write(*,'(i14,a,e20.10,a,e20.10,a,e20.10,a,e20.10,a,e20.10,a,e20.10)')iter,' | ',h_lo,' | ',err_lo,' | ',h_hi,' | ',err_hi,' | ',h,' | ',err
                !
             end if
             !
             ! adjust lo and hi solution
             if(err*err_lo.gt.0) then
                !
                h_lo = h
                !
             else
                !
                h_hi = h
                !
             end if
             !
             ! increase iteration counter
             iter = iter+1
             !
             if(iter.eq.itermax) then
                !
                write(*,*)
                write(*,'(a)')trim(hrule_err)
                write(*,'(a)')'error: no convergence achieved for altitude to corresponding re1 and mach values => abort'
                write(*,'(a)')trim(hrule_err)
                write(*,*)
                !
                stop
                !
             end if
             !
             ! end do while loop
          end do
          !
          ! print result on screen
          write(*,'(a,f20.10,a)')'altitude h = ',h,' m'
          !
          write(funit,'(3e32.16)')h,stat%mach,re1
          !
       end do
       !
       flush(funit)
       close(funit)
       !
       ! done here
       return
       !
    else
       !
       write(*,'(A,I3,A)')'Option ',key,' not supported'
       stop
       !
    end if
    !
    if(key.eq.1) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute unit reynolds with p and T input
       !----------------------------------------------------------------------------------------------------
       !
       ! compute viscosity
       call viscosity(stat%temp,stat%visc,visc_option)
       !
       ! compute unit Reynolds number
       re1 = stat%pres*stat%mach*sqrt(gamma)/(stat%visc*sqrt(Rgas*stat%temp))
       !
    elseif(key.eq.2) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute pressure value with re1 and T input
       !----------------------------------------------------------------------------------------------------
       !
       ! compute viscosity
       call viscosity(stat%temp,stat%visc,visc_option)
       !
       ! compute stagnation pressure
       stag%pres = (re1*Rgas*stat%temp*stat%visc)/(stat%mach*sqrt(gamma*Rgas*stat%temp))*((1.d0+0.5d0*(gamma-1.d0)*stat%mach*stat%mach)**(gamma/(gamma-1.d0)))
       !
       write(*,*)stag%pres
       !
       call stag_stat_conversion(stat%mach,gamma,stag%pres,stat%pres,pres_id,stag2stat,.false.)
       !
    elseif(key.eq.3) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute temperature value with re1 and p input
       !----------------------------------------------------------------------------------------------------
       !
       ! set lower and upper bound for temperature (in K) to perfrom bisection (bounds might have to be adjusted depending on the case)
       temp_stag_lo = 250.d0
       temp_stag_hi = 5000.d0
       !
       ! set tolerance for bisection (might have to be changed by user later)
       tol = 1e-12
       ! set maximum number of allowed iterations (might have to be adjusted by user)
       itermax = 100
       !
       ! initalize iteration count
       iter = 1
       !
       ! compute temperature iteratively
       !
       write(*,'(A5,A,A16,A,A16,A,A16,A,A16,A,A16,A,A16)')'i','|','temp_stag_lo','|','temp_stag_hi','|','temp_stag_nu','|','re1_nu','|','re1','|','error'
       !
       ! initialize error
       error = 1e10
       !
       do while (error.gt.tol)
          !
          ! compute new temperature estimate
          temp_stag_nu = (temp_stag_hi+temp_stag_lo)*0.5d0
          !
          ! convert stagnation to static temperatures
          call stag_stat_conversion(stat%mach,gamma,temp_stag_lo,temp_stat_lo,temp_id,stag2stat,.false.)
          call stag_stat_conversion(stat%mach,gamma,temp_stag_hi,temp_stat_hi,temp_id,stag2stat,.false.)
          call stag_stat_conversion(stat%mach,gamma,temp_stag_nu,temp_stat_nu,temp_id,stag2stat,.false.)
          !
          ! compute viscosities
          call viscosity(temp_stat_lo,visc_lo,visc_option)
          call viscosity(temp_stat_hi,visc_hi,visc_option)
          call viscosity(temp_stat_nu,visc_nu,visc_option)          
          !
          re1_lo  = stat%pres*stat%mach*sqrt(gamma)/(visc_lo*sqrt(Rgas*temp_stat_lo))
          re1_hi  = stat%pres*stat%mach*sqrt(gamma)/(visc_hi*sqrt(Rgas*temp_stat_hi))
          re1_nu = stat%pres*stat%mach*sqrt(gamma)/(visc_nu*sqrt(Rgas*temp_stat_nu))
          !
          error=abs(re1_nu-re1)/re1
          !
          ! write output for user
          write(*,'(I5,A,F16.8,A,F16.8,A,F16.8,A,E16.8,A,E16.8,A,E16.8)')iter,'|',temp_stag_lo,'|',temp_stag_hi,'|',temp_stag_nu,'|',re1_nu,'|',re1,'|',error
          !
          if((re1_nu-re1)*(re1_lo-re1).gt.0) then
             !
             temp_stag_lo = temp_stag_nu
             !
          else
             !
             temp_stag_hi = temp_stag_nu
             !
          end if
          !
          if(iter.ge.itermax) then
             !
             write(*,'(a)')'Maximum Number of iterations (',itermax,') reached without converging'
             write(*,'(a)')'Abort iteration'
             exit
             !
          end if
          !
          ! increase iteration count
          iter = iter + 1
          !
       end do
       !
       stat%temp = temp_stat_nu
       stag%temp = temp_stag_nu
       stat%visc = visc_nu
       !
    elseif(key.eq.5) then
       !
       !----------------------------------------------------------------------------------------------------
       ! compute pressure value with re1 and T input
       !----------------------------------------------------------------------------------------------------
       !
       ! compute viscosity
       call viscosity(stat%temp,stat%visc,visc_option)
       !
       ! compute mach number (need to iterate)
       !
       ! set lower and upper bound for mach number
       mach_lo = 0.1
       mach_hi = 100
       !
       ! set tolerance for bisection (might have to be changed by user later)
       tol = 1e-12
       !
       ! set maximum number of allowed iterations (might have to be adjusted by user)
       itermax = 100
       !
       ! initalize iteration count
       iter = 1
       !
       ! compute temperature iteratively
       !
       ! initialize error
       error = 1e10
       !
       if(debug) then
          !
          write(*,'(a4,a10,5a24)')'    ','iteration','mach number','static temperature','viscosity','static pressure','unit Reynolds number'
          !
       end if
       !
       do while (error.gt.tol)
          !
          ! compute new mach number estimate
          mach_nu = (mach_hi+mach_lo)*0.5d0
          !
          ! convert stagnation to static temperatures
          call stag_stat_conversion(mach_lo,gamma,stag%temp,temp_stat_lo,temp_id,stag2stat,.false.)
          call stag_stat_conversion(mach_hi,gamma,stag%temp,temp_stat_hi,temp_id,stag2stat,.false.)
          call stag_stat_conversion(mach_nu,gamma,stag%temp,temp_stat_nu,temp_id,stag2stat,.false.)
          !
          ! compute viscosities
          call viscosity(temp_stat_lo,visc_lo,visc_option)
          call viscosity(temp_stat_hi,visc_hi,visc_option)
          call viscosity(temp_stat_nu,visc_nu,visc_option)          
          !
          ! convert stagnation to static pressures
          call stag_stat_conversion(mach_lo,gamma,stag%pres,pres_stat_lo,pres_id,stag2stat,.false.)
          call stag_stat_conversion(mach_hi,gamma,stag%pres,pres_stat_hi,pres_id,stag2stat,.false.)
          call stag_stat_conversion(mach_nu,gamma,stag%pres,pres_stat_nu,pres_id,stag2stat,.false.)
          !
          re1_lo = pres_stat_lo*mach_lo*sqrt(gamma)/(visc_lo*sqrt(Rgas*temp_stat_lo))
          re1_hi = pres_stat_hi*mach_hi*sqrt(gamma)/(visc_hi*sqrt(Rgas*temp_stat_hi))
          re1_nu = pres_stat_nu*mach_nu*sqrt(gamma)/(visc_nu*sqrt(Rgas*temp_stat_nu))
          !
          if(debug) then
             !
             write(*,'(a4,i10,f24.12,f24.12,e24.12,e24.12,e24.12)')'lo: ',iter,mach_lo,temp_stat_lo,visc_lo,pres_stat_lo,re1_lo
             write(*,'(a4,i10,f24.12,f24.12,e24.12,e24.12,e24.12)')'hi: ',iter,mach_hi,temp_stat_hi,visc_hi,pres_stat_hi,re1_hi
             write(*,'(a4,i10,f24.12,f24.12,e24.12,e24.12,e24.12)')'nu: ',iter,mach_nu,temp_stat_nu,visc_nu,pres_stat_nu,re1_nu
             !
          end if
          !
          error=abs(re1_nu-re1)/re1
          !
          if((re1_nu-re1)*(re1_lo-re1).gt.0) then
             !
             mach_lo = mach_nu
             !
          else
             !
             mach_hi = mach_nu
             !
          end if
          !
          if(iter.ge.itermax) then
             !
             write(*,'(a,i4,a)')'Maximum Number of iterations (',itermax,') reached without converging'
             write(*,'(a)')'Abort iteration'
             exit
             !
          end if
          !
          ! increase iteration count
          iter = iter + 1
          !
          if(debug) then
             write(*,*)' '
          end if
          !
       end do
       !
       stat%mach = mach_nu
       !
       ! calculate static temperature and pressure values
       call stag_stat_conversion(stat%mach,gamma,stag%temp,stat%temp,temp_id,stag2stat,.false.)
       call stag_stat_conversion(stat%mach,gamma,stag%pres,stat%pres,pres_id,stag2stat,.false.)
       call viscosity(stat%temp,stat%visc,visc_option)
       !
    end if
    !
    ! compute freestream speed of sound
    stat%sos = sqrt(gamma*Rgas*stat%temp)
    !
    ! compute freestream velocity
    stat%uvel = stat%mach * stat%sos
    !
    ! compute reference time scale
    tref = lref/stat%uvel
    !
    ! compute freestream density from equation of state (p = rho*R*T)
    stat%dens = stat%pres/(Rgas*stat%temp)
    !
    ! compute stagnation enthalpy
    stag%ht = cp*stag%temp
    !
    ! calculate stagnation density
    stag%dens = stag%pres/(Rgas*stag%temp)
    !
    if(key.ne.4) then
       !
       fname = 'flow_conditions.dat'
       !
       ! write to file
       call flow_conditions_rdwr(action_wr_file,fname,stat,stag,re1,lref,tref,visc_option,debug)
       !
       ! write to screen
       call flow_conditions_rdwr(action_wr_screen,fname,stat,stag,re1,lref,tref,visc_option,debug)
       !
    end if
    !
  end subroutine flow_conditions
  !====================================================================================================
  ! read or write flow conditions to file or screen
  !====================================================================================================
  subroutine flow_conditions_rdwr(action,fname,stat,stag,re1,lref,tref,visc_option,debug)
    !
    implicit none
    !
    ! input/output variables
    integer, intent(in)    :: action
    character(*), intent(in) :: fname
    type(t_state), intent(inout) :: stat
    type(t_state), intent(inout) :: stag
    real(kind=dp), intent(inout)  :: re1
    real(kind=dp), intent(inout)  :: lref
    real(kind=dp), intent(inout)  :: tref
    integer, intent(inout) :: visc_option
    logical, intent(in)    :: debug
    ! local variables
    character*128          :: dummy_char1, dummy_char2, dummy_char3
    real(kind=dp)                 :: dummy
    integer                :: icol
    integer                :: nchar
    integer                :: funit
    real(kind=dp)                 :: ptot_psi
    real(kind=dp)                 :: dissipation_rate
    real(kind=dp)                 :: kolmogorov_length_scale
    real(kind=dp)                 :: kolmogorov_time_scale
    integer                :: ierr
    !
    ! assign file unit
    if(action.eq.action_wr_screen) then
       !
       funit = 6
       !
    elseif(action.eq.action_wr_file) then
       !
       funit = 200
       !
       ! open file
       open(unit=funit,file=trim(fname),status='unknown')
       !
    elseif(action.eq.action_rd_file) then
       !
       funit = 200
       !
       ! open file
       open(unit=funit,file=trim(fname),status='old')
       !
    end if
    !
    if(action.eq.action_wr_screen.or.action.eq.action_wr_file) then
       !
       !--------------------------------------------------
       ! write to screen or file
       !--------------------------------------------------
       !
       ! convert stagnation pressure to psi for comparison reasons
       call PAS2PSI_conversion(stag%pres,ptot_psi)
       !
       ! compute dissipation rate for kolmogorov scales (estiamte epsilon = U^3/L)
       dissipation_rate = stat%uvel**3.d0/lref
       !
       ! kolmogorov length scale
       kolmogorov_length_scale = ((stat%visc/stat%dens)**3.d0/dissipation_rate)**0.25d0
       !
       ! kolmogorov time scale
       kolmogorov_time_scale = ((stat%visc/stat%dens)/dissipation_rate)**0.5d0
       !       
       if(visc_option.eq.standard_visc) then
          write(funit,'(a)')               'Viscosity Option               : Standard Sutherland Law'
       elseif(visc_option.eq.low_temp_corr) then
          write(funit,'(a)')               'Viscosity Option               : Standard Sutherland Law with Low Temperature Correction'
       elseif(visc_option.eq.keyes) then
          write(funit,'(a)')               'Viscosity Option               : Keyes Law'
       end if
       !
       write(funit,'(a,f30.16,a)')         'Gas constant, Rgas             : ',Rgas       ,' [J/(kg K)]'
       write(funit,'(a,f30.16,a)')         'heat capacity (p = const.), cp : ',cp         ,' [J/(kg K)]'    
       write(funit,'(a,f30.16,a)')         'heat capacity (V = const.), cv : ',cv         ,' [J/(kg K)]'
       write(funit,'(a,f30.16,a)')         'heat capacity ratio, gamma     : ',gamma      ,' [-]'
       write(funit,'(a,f30.16,a)')         'Prandtl number, Pr             : ',Pr         ,' [-]'
       write(funit,'(a,f30.16,a)')         'freestream Mach number, M      : ',stat%mach  ,' [-]'
       write(funit,'(a,f30.16,a,f30.16,a)')'stagnation pressure, ptot      : ',stag%pres,' / ',ptot_psi,' [Pa]/[psi]'
       write(funit,'(a,f30.16,a)')         'freestream pressure, pfs       : ',stat%pres      ,' [Pa]'
       write(funit,'(a,f30.16,a)')         'stagnation temperature, Ttot   : ',stag%temp      ,' [K]'
       write(funit,'(a,f30.16,a)')         'freestream temperature, Tfs    : ',stat%temp      ,' [K]'
       write(funit,'(a,f30.16,a)')         'stagnation density, rhotot     : ',stag%dens      ,' [kg/m^3]'    
       write(funit,'(a,f30.16,a)')         'freestream density, rhofs      : ',stat%dens      ,' [kg/m^3]'    
       write(funit,'(a,f30.16,a)')         'viscosity, mu                  : ',stat%visc      ,' [kg/(m s)]'
       write(funit,'(a,f30.16,a)')         'unit Reynolds number, re1      : ',re1        ,' [1/m]'
       write(funit,'(a,f30.16,a)')         '1/sqrt(re1)                    : ',1/sqrt(re1),' [sqrt(m)]'
       write(funit,'(a,f30.16,a)')         'freestream velocity, Ufs       : ',stat%uvel      ,' [m/s]'
       write(funit,'(a,f30.16,a)')         'freestream speed of sound, cfs : ',stat%sos       ,' [m/s]'
       write(funit,'(a,f30.16,a)')         'reference length scale, lref   : ',lref       ,' [m]'
       write(funit,'(a,f30.16,a)')         'reference time scale, tref     : ',tref       ,' [s]'
       write(funit,'(a,f30.16,a)')         'stagnation enthalpy            : ',stag%ht/1e6,' [MJ/kg]'
       write(funit,'(a,e30.16,a)')         'kolmogorov length scale        : ',kolmogorov_length_scale,' [m]'
       write(funit,'(a,e30.16,a)')         'kolmogorov time scale          : ',kolmogorov_time_scale,' [s]'
       !
    elseif(action.eq.action_rd_file) then
       !
       !--------------------------------------------------
       ! read from file
       !--------------------------------------------------
       !
       read(funit,'(a)')dummy_char1
       !
       if(debug) then
          !
          write(*,'(a)')trim(dummy_char1)
          !
       end if
       !       
       icol = scan(dummy_char1,':')
       nchar = len(trim(dummy_char1))
       !
       if(debug) then
          !
          write(*,*)'icol = ',icol
          write(*,*)'nchar = ',nchar
          !
       end if
       !
       dummy_char2 = dummy_char1(icol+1:nchar)
       !
       ! delete leading spaces
       call delete_leading_spaces(dummy_char2)
       !
       if(debug) then
          !
          write(*,*)trim(dummy_char2)
          !
       end if
       !
       if(trim(dummy_char2).eq.'Standard Sutherland Law') then
          !
          visc_option = standard_visc
          !
       elseif(trim(dummy_char2).eq.'Standard Sutherland Law with Low Temperature Correction') then
          !
          visc_option = low_temp_corr
          !
       elseif(trim(dummy_char2).eq.'Keyes Law') then
          !
          visc_option = keyes
          !
       end if
       !
       if(debug) then
          !
          write(*,*)'visc_option = ',visc_option
          !
       end if
       !
       read(funit,'(a33,f30.16)')dummy_char1,Rgas
       read(funit,'(a33,f30.16)')dummy_char1,cp
       read(funit,'(a33,f30.16)')dummy_char1,cv
       read(funit,'(a33,f30.16)')dummy_char1,gamma
       read(funit,'(a33,f30.16)')dummy_char1,Pr
       !
       if(Rgas.eq.287.15d0) then
          !
          gas_type = air
          !
       elseif(Rgas.eq.296.80) then
          !
          gas_type = nitrogen
          !
       else
          !
          gas_type = user_defined
          !
       end if
       !
       if(debug) then
          !
          write(*,*)'Rgas = ',Rgas
          write(*,*)'cp = ',cp
          write(*,*)'cv = ',cv
          write(*,*)'gamma = ',gamma
          write(*,*)'Pr = ',Pr
          !
       end if
       !
       read(funit,'(a33,f30.16)')dummy_char1,stat%mach
       read(funit,'(a33,f30.16)')dummy_char1,stag%pres
       read(funit,'(a33,f30.16)')dummy_char1,stat%pres
       read(funit,'(a33,f30.16)')dummy_char1,stag%temp
       read(funit,'(a33,f30.16)')dummy_char1,stat%temp
       read(funit,'(a33,f30.16)')dummy_char1,stag%dens
       read(funit,'(a33,f30.16)')dummy_char1,stat%dens
       read(funit,'(a33,f30.16)')dummy_char1,stat%visc
       !
       if(debug) then
          !
          write(*,*)'M = ',stat%mach
          write(*,*)'stagnation pressure = ',stag%pres
          write(*,*)'static pressure = ',stat%pres
          write(*,*)'stagnation temperature = ',stag%temp
          write(*,*)'static temperature = ',stat%temp
          write(*,*)'stagnation density = ',stag%dens
          write(*,*)'static density = ',stat%dens
          write(*,*)'viscosity = ',stat%visc
          !
       end if
       !
       read(funit,'(a33,f30.16)')dummy_char1,re1
       read(funit,'(a33,f30.16)')dummy_char1,dummy
       read(funit,'(a33,f30.16)')dummy_char1,stat%uvel
       read(funit,'(a33,f30.16)')dummy_char1,stat%sos
       read(funit,'(a33,f30.16)')dummy_char1,lref
       read(funit,'(a33,f30.16)')dummy_char1,tref
       !
       if(debug) then
          !
          write(*,*)'Unit Reynolds number = ',re1
          write(*,*)'Freestream velocity = ',stat%uvel
          write(*,*)'Freestream speed of sound = ',stat%sos
          write(*,*)'Reference length scale = ',lref
          write(*,*)'Reference time scale = ',tref
          !
       end if
       !
       read(funit,'(a33,f30.16)',iostat=ierr)dummy_char1,stag%ht
       !
       if(ierr.ne.0) then
          !
          ! legacy file, no kolmogorov scales yet
          write(*,'(a)')'WARNING: Legacy flow_conditions.dat file => consider updating it by running flow conditions tool'
          !
          stag%ht=0
          !
       end if
       !
       ! reascale stagnation enthalpy to J (from MJ)
       stag%ht = stag%ht*1e6
       !
       read(funit,'(a33,e30.16)',iostat=ierr)dummy_char1,kolmogorov_length_scale
       !
       if(ierr.ne.0) then
          !
          ! legacy file, no kolmogorov scales yet
          write(*,'(a)')'WARNING: Legacy flow_conditions.dat file => consider updating it by running flow conditions tool'
          !
          kolmogorov_length_scale = 0.d0
          kolmogorov_time_scale = 0.d0
          !
       else
          !
          read(funit,'(a33,e30.16)')dummy_char1,kolmogorov_time_scale
          !
       end if
       !      
       if(debug) then
          !
          write(*,*)'Kolmogorov length scale = ',kolmogorov_length_scale
          write(*,*)'Kolmogorov time scale = ',kolmogorov_time_scale
          !
       end if
       !
    end if
    !
  end subroutine flow_conditions_rdwr
  !====================================================================================================
  ! get temperature value
  !====================================================================================================
  subroutine get_temp_val(temp_val_k,temp_type)
    !
    implicit none
    !
    ! input variables
    integer, intent(in) :: temp_type
    ! output variables
    real(kind=dp), intent(out) :: temp_val_k
    ! local variables
    real(kind=dp)              :: temp_val
    character*128       :: temp_dim
    character*512       :: message
    logical             :: input_correct
    !
    type_name(freestream) = 'freestream'
    type_name(stagnation) = 'stagnation'
    type_name(wall_value)  = 'wall'
    !
    input_correct = .false.
    !
    do while (.not.input_correct) 
       !
       write(*,'(A,A)')trim(type_name(temp_type)),' Temperature Value, Temperature Dimension (K,C,R,F)'
       read(*,*)temp_val,temp_dim
       !
       call low_cap(temp_dim,cap2low)
       !
       ! convert temperature to K
       if(trim(temp_dim).eq.'c'.or.trim(temp_dim).eq.'celsius') then
          !
          ! celsius to kelvin
          write(message,'(a)')'Convert degree Celsius to Kelvin'
          call print_message(message,GREEN)
          call CELS2KELV_conversion(temp_val,temp_val_k)
          !
          input_correct = .true.
          !
       elseif(trim(temp_dim).eq.'r'.or.trim(temp_dim).eq.'rankine') then
          !
          ! rankine to kelvin
          write(message,'(a)')'Convert Rankine to Kelvin'
          call print_message(message,GREEN)
          call RANK2KELV_conversion(temp_val,temp_val_k)
          !
          input_correct = .true.
          !
       elseif(trim(temp_dim).eq.'f'.or.trim(temp_dim).eq.'fahrenheit') then
          !
          ! fahrenheit to kelvin
          write(message,'(a)')'Convert Fahrenheit to Kelvin'
          call print_message(message,GREEN)
          call FAHR2KELV_conversion(temp_val,temp_val_k)
          !
          input_correct = .true.
          !
       elseif(trim(temp_dim).eq.'k'.or.trim(temp_dim).eq.'kelvin') then
          !
          ! no conversion needed
          write(message,'(a)')'No temperature conversion needed'
          call print_message(message,GREEN)
          temp_val_k = temp_val
          !
          input_correct = .true.
          !
       else
          !
          write(message,'(A,A,A)')'Temperature dimension ',trim(temp_dim),' not supported'
          call print_message(message,RED)
          write(message,'(A,A,A)')'Choose from available dimensions: K (Kelvin), C (Celsius), R (Rankine), F (Fahrenheit)'
          call print_message(message,RED)
          !
       end if
       !
    end do
    !
  end subroutine get_temp_val
  !====================================================================================================
  ! get pressure value
  !====================================================================================================
  subroutine get_pres_val(pres_val_pa,pres_type)
    !
    implicit none
    !
    ! input variables
    integer, intent(in) :: pres_type
    ! output variables
    real(kind=dp), intent(out) :: pres_val_pa
    ! local variables
    real(kind=dp)              :: pres_val
    character*128       :: pres_dim
    character*512       :: message
    logical             :: input_correct
    !
    type_name(freestream) = 'freestream'
    type_name(stagnation) = 'stagnation'
    !
    input_correct = .false.
    !
    do while(.not.input_correct)
       !
       write(*,'(A,A)')trim(type_name(pres_type)),' Pressure Value, Pressure Dimension (psi,var,pa)'
       read(*,*)pres_val,pres_dim
       !
       call low_cap(pres_dim,cap2low)
       !
       ! convert pressure to Pascal
       if(trim(pres_dim).eq.'psi') then
          !
          ! psi to pascal
          write(message,'(a)')'Convert psi to Pascal'
          call print_message(message,GREEN)
          call PSI2PAS_conversion(pres_val,pres_val_pa)
          !
          input_correct = .true.
          !
       elseif(trim(pres_dim).eq.'bar') then
          !
          ! bar to pascal
          write(message,'(a)')'Convert bar to Pascal'
          call print_message(message,GREEN)
          call BAR2PAS_conversion(pres_val,pres_val_pa)
          !
          input_correct = .true.
          !
       elseif(trim(pres_dim).eq.'pa'.or.trim(pres_dim).eq.'pascal') then
          !
          ! no conversion needed
          write(message,'(a)')'No conversion needed'
          call print_message(message,GREEN)
          pres_val_pa=pres_val
          !
          input_correct = .true.
          !
       else
          !
          write(message,'(A,A,A)')'pressure dimension ',trim(pres_dim),' not supported'
          call print_message(message,RED)
          write(message,'(A,A,A)')'Choose from available dimensions: psi, bar, pascal'
          call print_message(message,RED)
          !
       end if
    end do
    !
  end subroutine get_pres_val
  !====================================================================================================
  ! get density value
  !====================================================================================================
  subroutine get_dens_val(dens_val,dens_type)
    !
    implicit none
    !
    ! input variables
    integer, intent(in) :: dens_type
    ! output variables
    real(kind=dp), intent(out) :: dens_val
    ! local variables
    character*128       :: dens_dim
    character*512       :: message
    logical             :: input_correct
    !
    type_name(freestream) = 'freestream'
    type_name(stagnation) = 'stagnation'
    !
    input_correct = .false.
    !
    do while(.not.input_correct)
       !
       write(*,'(a,a)')trim(type_name(dens_type)),' density value, densitye dimension (kgm3)'
       read(*,*)dens_val,dens_dim
       !
       call low_cap(dens_dim,cap2low)
       !
       ! convert denssure to kg/m^3
       if(trim(dens_dim).eq.'kgm3') then
          !
          ! no conversion needed
          write(message,'(a)')'no conversion needed'
          call print_message(message,GREEN)
          !
          input_correct = .true.
          !
       else
          !
          write(message,'(a,a,a)')'density dimension ',trim(dens_dim),' currently not supported'
          call print_message(message,RED)
          write(message,'(a)')'extend get_dens_val routine in flow_conditions_mod.F90'
          call print_message(message,RED)
          !
          stop
          !
       end if
       !
    end do
    !
  end subroutine get_dens_val
  !====================================================================================================
  ! get mach number
  !====================================================================================================
  subroutine get_mach_number(mach)
    !
    implicit none
    !
    ! output variable
    real(kind=dp), intent(out) :: mach
    !
    write(*,'(a)')'Mach Number:'
    read(*,*)mach
    !
  end subroutine get_mach_number
  !====================================================================================================
  ! get flow conditions
  !====================================================================================================
  subroutine get_reference_conditions(ref)
    !
    implicit none
    !
    ! input/output variables
    type(t_state), intent(out) :: ref
    ! local variables 
    integer                    :: pres_type
    integer                    :: temp_type
    type(t_state)              :: stag
    character*512              :: message
    logical                    :: debug
    !
    ! initialize debug flag
    debug = .false.
    !
    ! initialize state
    ref%uvel = 0.d0
    ref%vvel = 0.d0
    ref%wvel = 0.d0
    ref%temp = 0.d0
    ref%pres = 0.d0
    ref%dens = 0.d0
    ref%visc = 0.d0
    ref%mach = 0.d0
    ref%sos  = 0.d0
    !
    !==================================================
    ! get Mach number
    !==================================================
    !
    call get_mach_number(ref%mach)
    !
    !==================================================
    ! get pressure
    !==================================================
    !
    write(*,'(a)')'Select input pressure type:'
    write(*,'(A,I1,A)')'(',stagnation,') stagnation pressure'
    write(*,'(A,I1,A)')'(',freestream,') freestream pressure'
    read(*,*)pres_type
    !
    if(pres_type.eq.stagnation) then
       !
       ! stagnation pressure supplied
       call get_pres_val(stag%pres,pres_type)
       ! compute freestream pressure
       call stag_stat_conversion(ref%mach,gamma,stag%pres,ref%pres,pres_id,stag2stat,.false.)
       !
    elseif(pres_type.eq.freestream) then
       !
       ! reference/freestream pressure supplied
       call get_pres_val(ref%pres,pres_type)
       ! compute stagnation pressure
       call stag_stat_conversion(ref%mach,gamma,ref%pres,stag%pres,pres_id,stat2stag,.false.)
       !
    else
       !
       write(message,'(A,I1,A)')'Pressure input type: ',pres_type,' not supported'
       call print_message(message,RED)
       stop
       !
    end if
    !
    !==================================================
    ! get temperature
    !==================================================
    !
    write(*,'(a)')'Select input temperature type:'
    write(*,'(A,I1,A)')'(',stagnation,') stagnation temperature'
    write(*,'(A,I1,A)')'(',freestream,') freestream temperature'
    read(*,*)temp_type
    !
    if(temp_type.eq.stagnation) then
       !
       ! stagnation temperature supplied
       call get_temp_val(stag%temp,temp_type)
       ! compute freestream temperature
       call stag_stat_conversion(ref%mach,gamma,stag%temp,ref%temp,temp_id,stag2stat,.false.)
       !
    elseif(temp_type.eq.freestream) then
       !
       ! reference/freestream temperature supplied
       call get_temp_val(ref%temp,temp_type)
       ! compute stagnation temperature
       call stag_stat_conversion(ref%mach,gamma,ref%temp,stag%temp,temp_id,stat2stag,.false.)
       !
    else
       !
       write(message,'(A,I1,A)')'Temperature input type: ',temp_type,' not supported'
       call print_message(message,RED)
       stop
       !
    end if
    !
    ! safe reference conditions
    ref%sos  = sqrt(gamma*ref%temp*Rgas)
    ref%uvel = ref%mach*ref%sos
    ref%dens = ref%pres/(Rgas*ref%temp)
    ref%ht   = cp*ref%temp+0.5d0*ref%uvel*ref%uvel
    !
    if(debug) then
       !
       write(*,'(a,e30.16)')'u-velocity     : ',ref%uvel
       write(*,'(a,e30.16)')'mach number    : ',ref%mach
       write(*,'(a,e30.16)')'speed of sound : ',ref%sos
       write(*,'(a,e30.16)')'density        : ',ref%dens
       write(*,'(a,e30.16)')'pressure       : ',ref%pres
       write(*,'(a,e30.16)')'temperature    : ',ref%temp
       !
    end if
    !
  end subroutine get_reference_conditions
  !====================================================================================================
  ! equation of state
  !====================================================================================================
  subroutine equation_of_state(state,Rgas,type)
    !
    implicit none
    !
    ! input/output variables
    type(t_state), intent(inout) :: state
    real(kind=dp), intent(in)           :: Rgas
    integer, intent(in)          :: type
    !
    if(type.eq.calc_pres) then
       !
       state%pres = state%dens*Rgas*state%temp
       !
    elseif(type.eq.calc_dens) then
       !
       state%dens = state%pres/(Rgas*state%temp)
       !
    elseif(type.eq.calc_temp) then
       !
       state%temp = state%pres/(Rgas*state%dens)
       !
    end if
    !
  end subroutine equation_of_state
  !
#if(PAR==1)
  !====================================================================================================
  ! broadcast a state
  !====================================================================================================
  subroutine broadcast_state(state)
    !
    ! input/output variable
    type(t_state), intent(inout) :: state
    ! local variables
    integer                      :: ierr
    !
    call mpi_bcast(state%mach,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(state%uvel,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(state%vvel,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(state%wvel,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(state%temp,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(state%pres,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(state%dens,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(state%visc,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(state%h   ,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(state%ht  ,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(state%sos ,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    !
  end subroutine broadcast_state
#endif
  !
  !====================================================================================================
  ! detect flow conditions
  !====================================================================================================
  subroutine flow_conditions_detect(stat,stag,re1,lref,tref,visc_option,debug)
    !
    implicit none
    !
    ! input/output variables
    type(t_state), intent(out) :: stat
    type(t_state), intent(out) :: stag
    real(kind=dp), intent(out)        :: re1
    real(kind=dp), intent(out)        :: lref
    real(kind=dp), intent(out)        :: tref
    integer, intent(out)       :: visc_option
    logical, intent(in)        :: debug
    ! local variables
    character*128              :: fname_root
    character*128              :: fname_tmp
    character*128              :: fname
    character*128              :: answer
    logical                    :: flow_conditions_found
    logical                    :: do_supply_fname
    integer                    :: lvl
    integer                    :: lvl_max
    integer                    :: idir
    !
    write(*,*)
    write(*,'(a)')trim(hrule)
    write(*,'(a)')'Try to automatically detect flow conditions ...'
    write(*,'(a)')trim(hrule)
    write(*,*)
    write(*,'(a)')'search for default flow_conditions.dat file in work and parent directories...'
    !
    ! initialie lvl counter
    ! lvl = 0: working directory
    ! lvl = 1: parent directory
    ! lvl = 2: grand-parent directory (one above parent directory)
    ! lvl = 3: great-grand-parent directory (one above grand-parent directory, 2 above parent directory, ...)
    ! lvl = 4: ...
    lvl = 0
    !
    ! set maximum at which level we will look for flow conditions
    lvl_max = 5
    !
    ! initialize flow_conditions_found with .false. to enter loop
    flow_conditions_found = .false.
    !
    ! set file name for flow conditions file
    fname_root = 'flow_conditions.dat'
    !
    do while(.not.flow_conditions_found)
       !
       ! initialize file name
       fname = fname_root
       !
       ! loop over parent levels => add directories to file name
       do idir = 1,lvl
          !
          write(fname_tmp,'(a,a)')'../',trim(fname)
          !
          fname = fname_tmp
          !
       end do
       !
       ! check if maximum level to sreach was reached => if yes break out of loop
       if(lvl.eq.lvl_max) then
          !
          write(*,*)
          write(*,'(a)')trim(hrule_err)
          write(*,'(a)')'error: maximum level of parent directory search reached without detecing flow conditions file'
          write(*,'(a)')trim(hrule_err)
          write(*,*)
          !
          exit
          !
       end if
       !
       ! check if flow conditions were found
       call check_file_status(fname,flow_conditions_found)
       !
       if(flow_conditions_found) then
          !
          write(*,*)
          write(*,'(a,a)')'flow conditions file detected in: ',trim(fname)
          !
       end if
       !
       ! increase lvl counter by 1
       lvl = lvl + 1
       !
    end do
    !
    if(flow_conditions_found) then
       !
       ! flow conditions file found => read and then write file
       !
       write(*,*)
       write(*,'(a)')'reading flow conditions...'
       write(*,*)
       !
       call flow_conditions_rdwr(action_rd_file,fname,stat,stag,re1,lref,tref,visc_option,.false.)
       !
       call flow_conditions_rdwr(action_wr_screen,fname,stat,stag,re1,lref,tref,visc_option,.false.)
       !
    else
       !
       ! no flow conditions file found => get user input
       write(*,'(a)')''
       write(*,'(a)')'supply user file name for flow conditions?'
       write(*,'(a)')'[Y/y]es'
       write(*,'(a)')'[N/n]o'
       read(*,*)answer
       !
       call util_interpret_answer(answer,do_supply_fname)
       !
       if(do_supply_fname) then
          !
          write(*,*)
          write(*,'(a)')'flow conditions file name:'
          call util_get_input_file(fname)
          !
          call flow_conditions_rdwr(action_rd_file,fname,stat,stag,re1,lref,tref,visc_option,.false.)
          !
          call flow_conditions_rdwr(action_wr_screen,fname,stat,stag,re1,lref,tref,visc_option,.false.)
          !
          flow_conditions_found = .true.
          !
       end if
       !
    end if
    !
    if(flow_conditions_found) then
       !
       ! if flow conditions were found check if user wants to accept the conditions
       !
       write(*,*)
       write(*,'(a)')'accept detected flow conditions (y/n)?'
       read(*,*)answer
       !
       call util_interpret_answer(answer,flow_conditions_found)
       !
    end if
    !
    if(.not.flow_conditions_found) then
       !
       write(*,'(a)')'No flow conditions file in work directory detected -> manual user input required'
       !
       ! get user input for viscosity option
       call select_visc_option(visc_option)
       !
       ! get user input for gas properties
       call get_gas_properties
       !
       ! get user input for reference conditions
       call get_reference_conditions(stat)
       !
       ! calculate freestream viscosity
       call viscosity(stat%temp,stat%visc,visc_option)
       !
       ! calculate stagnation quantities
       !
       ! temperature
       call stag_stat_conversion(stat%mach,gamma,stat%temp,stag%temp,temp_id,stat2stag,.false.)
       ! pressure
       call stag_stat_conversion(stat%mach,gamma,stat%pres,stag%pres,pres_id,stat2stag,.false.)
       ! density
       call equation_of_state(stag,Rgas,calc_dens)
       !
       ! calculate unit reynolds number
       re1 = stat%pres*stat%mach*sqrt(gamma)/(stat%visc*sqrt(Rgas*stat%temp))
       !
       ! set refernce length scale
       lref = 1.d0
       !
       ! calculate reference time scale
       tref = lref/stat%uvel
       !
       ! calculate stagnation enthalpy
       stag%ht = cp*stag%temp
       !
       ! display flow conditions on screen
       call flow_conditions_rdwr(action_wr_screen,fname,stat,stag,re1,lref,tref,visc_option,.false.)
       !
    end if    
    !
  end subroutine flow_conditions_detect
  !
end module flow_conditions_mod
