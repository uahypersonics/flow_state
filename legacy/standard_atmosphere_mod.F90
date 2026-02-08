!----------------------------------------------------------------------------------------------------
! module to calculate temperature and pressure of standard earth asmosphere
! Source: https://www.grc.nasa.gov/www/k-12/airplane/atmosmet.html
!----------------------------------------------------------------------------------------------------
module standard_atmosphere_mod
  !
  ! load modules
  use dimension_conversion_mod
  use params
  !
  implicit none
  !
  ! global variables (if needed)  
  !
  ! include subroutines after contains statment
contains
  !----------------------------------------------------------------------------------------------------
  ! calculate standard atmosphere distributions
  !----------------------------------------------------------------------------------------------------
  subroutine standard_atmosphere(debug)
    !
    implicit none
    !
    ! input/output variables
    logical, intent(in) :: debug
    ! local variables
    real(kind=dp) :: hs,he,dh,h
    integer :: nh, i
    character*128 :: fname
    integer :: funit
    type(t_state) :: atmo
    !
    fname = 'standard_atmosphere.dat'
    funit = 200
    !
    open(unit=funit,file=trim(fname),status='unknown')
    !
    write(funit,'(a)')'TITLE = "standard_atmosphere"'
    write(funit,'(a)')'VARIABLES = "h" "temp" "pres" "dens"'
    write(funit,'(a)')'ZONE T = "standard_atmosphere"'
    !
    write(*,'(a)')'Altitude start and end value (hs,he):'
    read(*,*)hs,he
    !
    write(*,'(a)')'Number of points (nh):'
    read(*,*)nh
    !
    ! calculate distance
    dh = (he-hs)/(nh-1)
    !
    do i = 1,nh
       !
       h = hs+(i-1)*dh
       !
       call standard_atmosphere_state(h,atmo,debug)
       !
       write(funit,*)h,atmo%temp,atmo%pres,atmo%dens
       !
    end do
    !
    close(funit)
    !
  end subroutine standard_atmosphere
  !----------------------------------------------------------------------------------------------------
  ! calculate atmoshperic temperature
  !----------------------------------------------------------------------------------------------------
  subroutine standard_atmosphere_state(h,atmo,debug)
    !
    implicit none
    !
    ! input variables
    real(kind=dp), intent(in)           :: h
    logical, intent(in)          :: debug
    ! input/output variables
    type(t_state), intent(inout) :: atmo
    ! local variables
    real(kind=dp)                       :: temp_celsius
    !
    if(h.le.11000) then
       !
       !--------------------------------------------------
       ! troposphere
       !
       ! h < 11000 m
       !--------------------------------------------------
       !
       ! calculate temperature (in degrees celsius)
       temp_celsius = 15.04 - 0.00649 * h
       !
       ! convert to kelvin
       call CELS2KELV_conversion(temp_celsius,atmo%temp)
       !
       ! calculate pressure (in pascal)
       atmo%pres = 101.29*1000*(atmo%temp/288.08)**5.256
       !
    elseif((h.gt.11000).and.(h.le.25000)) then
       !
       !--------------------------------------------------
       ! lower stratosphere
       !
       ! 11000 m < h < 25000 m
       !--------------------------------------------------
       !
       ! calculate temperature (in degrees celsius)
       temp_celsius = -56.46
       !
       ! convert to kelvin
       call CELS2KELV_conversion(temp_celsius,atmo%temp)
       !
       ! calculate pressure (in pascal)
       atmo%pres = 22.65*1000*exp(1.73-0.000157*h)
       !
    elseif(h.gt.25000) then
       !
       !--------------------------------------------------
       ! upper stratosphere
       !
       ! h > 25000 m
       !--------------------------------------------------
       !
       ! calculate temperature (in degrees celsius)
       temp_celsius = -131.21 + 0.00299 * h
       !
       ! convert kelvin
       call CELS2KELV_conversion(temp_celsius,atmo%temp)
       !
       ! calculate pressre (in pascal)
       atmo%pres = 2.488 * (atmo%temp/216.6)**(-11.388)*1000
       !
    end if
    !
    if(debug) then
       !
       write(*,'(a,f20.10,a)')'altitude    : ',h,' m'
       write(*,'(a,f20.10,a)')'temperature : ',atmo%temp,' K'
       write(*,'(a,f20.10,a)')'pressure    : ',atmo%pres,' Pa'
       !
    end if
    !
  end subroutine standard_atmosphere_state
  !
end module standard_atmosphere_mod
