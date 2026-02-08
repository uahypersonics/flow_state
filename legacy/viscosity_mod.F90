!====================================================================================================
! compute viscosity with sutherland's law or with sutherland's law with low temperature correction
! date: 02/18/2015
!====================================================================================================
module viscosity_mod
  !
  use util_mod
  use gas_properties_mod
  !
  implicit none
  !
  integer, parameter :: standard_visc = 1
  integer, parameter :: low_temp_corr = 2
  integer, parameter :: low_temp_blend = 3
  !
  ! references for keyes law:
  !
  ! Priebe, S., & Martin, P. (2011). Direct numerical simulation of a hypersonic turbulent boundary layer on a large domain. In 41st AIAA Fluid Dynamics Conference and Exhibit (AIAA 2011-3432).
  ! Roy, C. J., & Blottner, F. G. (2006). Review and assessment of turbulence models for hypersonic flows. Progress in Aerospace Sciences, 42(7-8), 469-530.
  integer, parameter :: keyes = 4
  integer, parameter :: sutherland_user = 5
  integer, parameter :: power_law = 6
  !
  real(kind=dp), private:: tref, visc_ref, sutherland_constant, visc_m
  !
  ! contains statement -> include subroutines after contains statement
contains
  !====================================================================================================
  ! select a viscosity option
  !====================================================================================================
  subroutine select_visc_option(visc_option)
    !
    implicit none
    !
    ! output variables
    integer, intent(out) :: visc_option
    ! local variables
    logical              :: selection_valid
    character*512        :: message
    !
    selection_valid = .false.
    visc_option=-99
    !
    do while (.not.selection_valid)
       !
       ! set option on how to compute the viscosity (either Standard or with low temperature correciton)
       write(*,'(a)')"Choose opion for sutherland's law"
       write(*,'(a,i2,a)')'(',standard_visc,') Standard law'
       write(*,'(a,i2,a)')'(',low_temp_corr,') Standard law with low temperature correction'
       write(*,'(a,i2,a)')'(',low_temp_blend,') Standard law with low temperature correction (blended functions as in profcom)'
       write(*,'(a,i2,a)')'(',keyes,') Keyes law'
       write(*,'(a,i2,a)')'(',sutherland_user,') Sutherland with user defined constants'
       write(*,'(a,i2,a)')'(',power_law,') Power law'
       read(*,*)visc_option
       !
       if((visc_option.eq.standard_visc).or.(visc_option.eq.low_temp_corr).or.(visc_option.eq.low_temp_blend).or.(visc_option.eq.keyes)) then
          !
          selection_valid = .true.
          !
       elseif(visc_option.eq.sutherland_user) then
          !
          write(*,'(a)')'reference temperature (Tref):'
          read(*,*)tref
          !
          write(*,'(a)')'reference viscosity (mu_ref):'
          read(*,*)visc_ref
          !
          write(*,'(a)')'constant S:'
          read(*,*)sutherland_constant
          !
          selection_valid = .true.
          !
       elseif(visc_option.eq.power_law) then
          !
          write(*,'(a)')'reference temperature (Tref):'
          read(*,*)tref
          !
          write(*,'(a)')'reference viscosity (mu_ref):'
          read(*,*)visc_ref
          !
          write(*,'(a)')'Power:'
          read(*,*)visc_m
          !
          selection_valid = .true.
          !
       else
          !
          write(message,'(a,i3,a)')'Selected scheme for viscosity law: ',visc_option,' not supported'
          call print_message(message,RED)
          !
          selection_valid = .false.
          !
       end if
       !
    end do
    !
  end subroutine select_visc_option
  !====================================================================================================
  ! sutherland's law with or without low temperature correction
  !====================================================================================================
  subroutine viscosity(temp,visc,visc_option)
    !
    implicit none
    !
    ! input variables
    real(kind=dp), intent(in)  :: temp
    integer, intent(in) :: visc_option
    ! output variables
    real(kind=dp), intent(out) :: visc
    ! local variables
    real(kind=dp)              :: s
    real(kind=dp)              :: c1
    real(kind=dp)              :: c2
    real(kind=dp)              :: t1, t2
    real(kind=dp)              :: a(8),visc0
    real(kind=dp)              :: temp_norm
    real(kind=dp)              :: num, den
    real(kind=dp)              :: a0,a1,a2
    integer             :: i
    !
    ! set parameters
    !
    ! S in [K]
    s = 110.4
    !
    ! C1 in [kg/(m*s*sqrt(K))]
    c1 = 1.458e-6
    ! 
    c2 = 6.93873e-8
    !
    ! T1 in [K]
    t1 = 40.d0
    !
    ! T2 in [K]
    t2 = 110.4
    !
    ! compute viscosity depending on input option
    !
    if(visc_option.eq.standard_visc) then
       !
       ! standard sutherland's law
       visc = c1*temp**1.5d0/(temp+s)
       !
    elseif(visc_option.eq.low_temp_corr) then
       !
       ! low temperature correction for sutherlands law
       if(temp.lt.t1) then
          !
          visc = c2*t1
          !
       elseif(temp.ge.t1.and.temp.le.t2) then
          !
          visc = c2*temp
          !
       else
          !
          visc = c1*temp**1.5d0/(temp+s)          
          !
       end if
       !
    elseif(visc_option.eq.low_temp_blend) then
       !
       ! set coefficients for blending function (from profcom)
       visc0 = 7.659704848E-6
       a(1)  = -4.479148053679334e1
       a(2)  =  3.195188079744342e2
       a(3)  = -9.716235566382709e2
       a(4)  =  1.632645086771892e3
       a(5)  = -1.637375578884298e3
       a(6)  =  9.802775658900685e2
       a(7)  = -3.234667180557399e2
       a(8)  =  4.58157988617632e1
       !
       if(temp.gt.130.0) then
          !
          visc = c1*temp**1.5d0/(temp+s)
          !
       elseif(temp.gt.100.0) then
          !
          visc = 0.d0
          !
          do i = 1,8
             !
             visc = visc+(a(i)*temp**(8.d0-dble(i))/s**(8.d0-dble(i)))
             !
          end do
          !
          ! multiply with leading constant
          visc = visc*visc0
          !
       else
          !
          visc = c2*temp
          !
       end if
       !
    elseif(visc_option.eq.keyes) then
       !
       if(gas_type.eq.air) then
          !
          a0 = 1.488e-6
          a1 = 122.1
          a2 = 5.0
          !
       elseif(gas_type.eq.nitrogen) then
          !
          a0 = 1.418e-6
          a1 = 116.4
          a2 = 5.0
          !
       else
          !
          write(*,'(a)')'Gas type: ',gas_type,' unknown'
          write(*,'(a)')'Abort'
          stop
          !
       end if
       !
       t1 = 10.d0**(-a2/temp)
       !
       num = a0*sqrt(temp)
       !
       den = 1+a1*t1/temp
       !
       visc = num/den
       !
    elseif(visc_option.eq.sutherland_user) then
       !
       ! sutherlands law, user defined constants
       visc = visc_ref*(temp/tref)**1.5d0*(tref+sutherland_constant)/(temp+s)
       !
    elseif(visc_option.eq.power_law) then
       !
       visc = visc_ref*(temp/tref)**visc_m
       !
    end if
    !
  end subroutine viscosity
  !====================================================================================================
  ! viscosity derivative with respect to temperature
  !====================================================================================================
  subroutine viscosity_derivative(temp,dviscdt,visc_option)
    !
    implicit none
    !
    ! input variables
    real(kind=dp), intent(in)  :: temp
    integer, intent(in) :: visc_option
    ! output variables
    real(kind=dp), intent(out) :: dviscdt
    ! local variables
    real(kind=dp)              :: visc
    real(kind=dp)              :: s
    real(kind=dp)              :: c1
    real(kind=dp)              :: c2
    real(kind=dp)              :: t1, t2
    real(kind=dp)              :: temp_norm
    real(kind=dp)              :: num, den
    real(kind=dp)              :: a(8),visc0
    real(kind=dp)              :: a0,a1,a2
    integer             :: i
    !
    ! set parameters
    !
    ! S in [K]
    s = 110.4
    !
    ! C1 in [kg/(m*s*sqrt(K))]
    c1 = 1.458e-6
    ! 
    c2 = 6.93873e-8
    !
    ! T1 in [K]
    t1 = 40.d0
    !
    ! T2 in [K]
    t2 = 110.4
    !
    ! compute viscosity depending on input option
    !
    if(visc_option.eq.standard_visc) then
       !
       ! standard sutherland's law
       visc = c1*temp**1.5d0/(temp+s)
       !
       ! viscosity derviative with respect to temperature
       dviscdt = visc*(temp+3.d0*s)/(2*temp*(temp+s))
       !
    elseif(visc_option.eq.low_temp_corr) then
       !
       ! low temperature correction for sutherlands law
       if(temp.lt.t1) then
          !
          visc = c2*t1
          dviscdt = 0.d0
          !
       elseif(temp.ge.t1.and.temp.le.t2) then
          !
          visc = c2*temp
          dviscdt = c2
          !
       else
          !
          visc = c1*temp**1.5d0/(temp+s)
          dviscdt = visc*(temp+3.d0*s)/(2*temp*(temp+s))
          !
       end if
       !
    elseif(visc_option.eq.low_temp_blend) then
       !
       ! set coefficients for blending function (from profcom)
       visc0 = 7.659704848E-6
       a(1)  = -4.479148053679334e1
       a(2)  =  3.195188079744342e2
       a(3)  = -9.716235566382709e2
       a(4)  =  1.632645086771892e3
       a(5)  = -1.637375578884298e3
       a(6)  =  9.802775658900685e2
       a(7)  = -3.234667180557399e2
       a(8)  =  4.58157988617632e1
       !
       if(temp.gt.130.0) then
          !
          visc = c1*temp**1.5d0/(temp+s)
          dviscdt = visc*(temp+3.d0*s)/(2*temp*(temp+s))
          !
       elseif(temp.gt.100.0) then
          !
          visc = 0.d0
          !
          do i = 1,8
             !
             visc = visc+(a(i)*temp**(8.d0-dble(i))/s**(8.d0-dble(i)))
             !
          end do
          !
          ! multiply with leading constant
          visc = visc*visc0
          !
          dviscdt = 0.d0
          !
          do i = 1,7
             !
             dviscdt = dviscdt+(a(i)*(8.d0-dble(i))*temp**(7.d0-dble(i))/s**(8.d0-dble(i)))
             !
          end do
          !
          dviscdt = dviscdt*visc0
          !
       else
          !
          visc = c2*temp
          dviscdt = c2
          !
       end if
       !
    elseif(visc_option.eq.keyes) then
       !
       if(gas_type.eq.air) then
          !
          a0 = 1.488e-6
          a1 = 122.1
          a2 = 5.0
          !
       elseif(gas_type.eq.nitrogen) then
          !
          a0 = 1.418e-6
          a1 = 116.4
          a2 = 5.0
          !
       else
          !
          write(*,'(a)')'Gas type: ',gas_type,' unknown'
          write(*,'(a)')'Abort'
          stop
          !
       end if
       !
       t1 = 10.d0**(-a2/temp)
       !
       num = a0*sqrt(temp)
       !
       den = 1+a1*t1/temp
       !
       visc = num/den
       !
       num = a0*(0.5d0*temp*sqrt(temp)+a1*t1*(1.5d0*sqrt(temp)-a2/sqrt(temp)))
       !
       den = (temp+a1*t1)**2.d0
       !
       dviscdt = num/den
       !
    elseif(visc_option.eq.sutherland_user) then
       !
       ! sutherlands law, user defined constants
       visc = visc_ref*(temp/tref)**1.5d0*(tref+sutherland_constant)/(temp+sutherland_constant)
       !
       dviscdt = visc*(temp+3.d0*sutherland_constant)/(2.d0*temp*(temp+sutherland_constant))
       !
    elseif(visc_option.eq.power_law) then
       !
       visc = visc_ref*(temp/tref)**visc_m
       !
       dviscdt = visc_m*visc_ref*(temp/tref)**(visc_m-1.d0)
       !
    end if
    !
  end subroutine viscosity_derivative
  !====================================================================================================
  ! compute viscosity for a temperature range
  !====================================================================================================
  subroutine calc_visc
    !
    implicit none
    !
    integer       :: i
    real(kind=dp)        :: temp_lo
    real(kind=dp)        :: temp_hi
    integer       :: nopts
    real(kind=dp)        :: dtemp
    real(kind=dp)        :: temp_loc
    real(kind=dp)        :: visc_loc
    real(kind=dp)        :: dviscdt_loc
    character*128 :: fname
    integer       :: visc_option
    character*128 :: visc_info
    character*512 :: message
    integer       :: funit
    !
    call get_gas_properties
    !
    write(*,'(a)')'Give temperature range'
    write(*,'(a)')'start temperature: '
    read(*,*)temp_lo
    write(*,'(a)')'end temperature: '
    read(*,*)temp_hi
    write(*,'(a)')'number of points'
    read(*,*)nopts
    !
    ! compute temperature increment
    dtemp = (temp_hi-temp_lo)/(nopts-1)
    !
    ! select option for viscosity law
    call select_visc_option(visc_option)
    !
    if(visc_option.eq.standard_visc) then
       !
       visc_info = 'sutherland_standard'
       !
    elseif(visc_option.eq.low_temp_corr) then
       !
       visc_info = 'sutherland_low_temp_corr'
       !
    elseif(visc_option.eq.low_temp_blend) then
       !
       visc_info = 'sutherland_low_temp_corr_blended'
       !
    elseif(visc_option.eq.keyes) then
       !
       visc_info = 'keyes'
       !
    elseif(visc_option.eq.power_law) then
       !
       visc_info = 'power_law'
       !
    else
       !
       write(message,'(A,I2,A)')'Viscosity law option ',visc_option,' not available'
       !
       call print_message(message,RED)
       stop
       !
    end if
    !
    write(*,'(a)')'Output file name:'
    read(*,*)fname
    !
    funit = 300
    !
    ! open file
    open(unit=funit,file=trim(fname),status='unknown')
    !
    ! write file header
    write(funit,'(a,a,a)')'TITLE = "',trim(visc_info),'"'
    write(funit,'(a)')'VARIABLES = "temp","visc","dviscdt"'
    !
    write(funit,'(a,a,a,i6)')'ZONE T = "',trim(visc_info),'", I = ',nopts
    !
    do i = 1,nopts
       !
       temp_loc = temp_lo + (i-1) * dtemp
       !
       call viscosity(temp_loc,visc_loc,visc_option)
       call viscosity_derivative(temp_loc,dviscdt_loc,visc_option)
       !
       write(funit,'(2e30.16)')temp_loc,visc_loc,dviscdt_loc
       !
    end do
    !
    ! close output file
    close(funit)
    !
  end subroutine calc_visc
  !
end module viscosity_mod
