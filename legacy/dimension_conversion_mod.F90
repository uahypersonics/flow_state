!====================================================================================================
! convert commonly known dimensions
! date: 02/18/2015
!====================================================================================================
module dimension_conversion_mod
  !
  ! load modules
  use params
  use util_mod
  !
  implicit none
  !
  ! set everything to private in this module
  private
  !
  ! build mappping to have unique conversion rules
  !
  !--------------------------------------------------
  ! pressure conversion: 3x3 matrix
  !--------------------------------------------------
  !
  ! assign integers to different pressure dimensions
  integer, parameter :: PAS = 1
  integer, parameter :: BAR = 2
  integer, parameter :: PSI = 3
  !
  ! pres_conv(PAS,PAS) = 1 => convert pascal to pascal
  ! pres_conv(PAS,BAR) = 2 => convert pascal to bar
  ! pres_conv(PAS,PSI) = 3 => convert pascal to psi
  integer, parameter :: PAS2PAS = 1
  integer, parameter :: PAS2BAR = 2
  integer, parameter :: PAS2PSI = 3
  !
  ! pres_conv(BAR,PAS) = 1 => convert bar to pascal
  ! pres_conv(BAR,BAR) = 2 => convert bar to bar
  ! pres_conv(BAR,PSI) = 3 => convert bar to psi
  integer, parameter :: BAR2PAS = 4
  integer, parameter :: BAR2BAR = 5
  integer, parameter :: BAR2PSI = 6  
  !
  ! pres_conv(PSI,PAS) = 1 => convert psi to pascal
  ! pres_conv(PSI,BAR) = 2 => convert psi to bar
  ! pres_conv(PSI,PSI) = 3 => convert psi to psi
  integer, parameter :: PSI2PAS = 7
  integer, parameter :: PSI2BAR = 8
  integer, parameter :: PSI2PSI = 9  
  !
  !--------------------------------------------------
  ! temperature conversion: 4x4 matrix
  !--------------------------------------------------
  !
  ! assign integers to different temperature dimensions
  !
  integer, parameter :: CELS = 1
  integer, parameter :: RANK = 2
  integer, parameter :: FAHR = 3
  integer, parameter :: KELV = 4
  !
  ! temp_conv(CELS,CELS) = 1 => convert celsius to celsius
  ! temp_conv(CELS,RANK) = 2 => convert celsius to rankine
  ! temp_conv(CELS,FAHR) = 3 => convert celsius to fahrenheit
  ! temp_conv(CELS,KELV) = 3 => convert celsius to kelvin
  integer, parameter :: CELS2CELS = 1
  integer, parameter :: CELS2RANK = 2
  integer, parameter :: CELS2FAHR = 3
  integer, parameter :: CELS2KELV = 4
  !
  ! temp_conv(RANK,CELS) = 1 => convert rankine to celsius
  ! temp_conv(RANK,RANK) = 2 => convert rankine to rankine
  ! temp_conv(RANK,FAHR) = 3 => convert rankine to fahrenheit
  ! temp_conv(RANK,KELV) = 3 => convert rankine to kelvin
  integer, parameter :: RANK2CELS = 5
  integer, parameter :: RANK2RANK = 6
  integer, parameter :: RANK2FAHR = 7
  integer, parameter :: RANK2KELV = 8
  !
  ! temp_conv(FAHR,CELS) = 1 => convert fahrenheit to celsius
  ! temp_conv(FAHR,RANK) = 2 => convert fahrenheit to rankine
  ! temp_conv(FAHR,FAHR) = 3 => convert fahrenheit to fahrenheit
  ! temp_conv(FAHR,KELV) = 3 => convert fahrenheit to kelvin
  integer, parameter :: FAHR2CELS = 9
  integer, parameter :: FAHR2RANK = 10
  integer, parameter :: FAHR2FAHR = 11
  integer, parameter :: FAHR2KELV = 12
  !
  ! temp_conv(KELV,CELS) = 1 => convert kelvin to celsius
  ! temp_conv(KELV,RANK) = 2 => convert kelvin to rankine
  ! temp_conv(KELV,FAHR) = 3 => convert kelvin to fahrenheit
  ! temp_conv(KELV,KELV) = 3 => convert kelvin to kelvin
  integer, parameter :: KELV2CELS = 13
  integer, parameter :: KELV2RANK = 14
  integer, parameter :: KELV2FAHR = 15
  integer, parameter :: KELV2KELV = 16
  !
  ! make some routines publically available
  public :: dimension_conversion
  public :: BAR2PAS_conversion, BAR2PSI_conversion
  public :: PAS2BAR_conversion, PAS2PSI_conversion
  public :: PSI2BAR_conversion, PSI2PAS_conversion
  public :: FAHR2KELV_conversion, FAHR2CELS_conversion, FAHR2RANK_conversion
  public :: CELS2KELV_conversion, CELS2RANK_conversion, CELS2FAHR_conversion
  public :: RANK2KELV_conversion, RANK2CELS_conversion, RANK2FAHR_conversion
  public :: KELV2FAHR_conversion, KELV2CELS_conversion, KELV2RANK_conversion

  !
  ! contains statement -> include subroutines after contains statement
CONTAINS
  !====================================================================================================
  ! convert dimensions of input values to regularly used dimensions
  !====================================================================================================
  subroutine dimension_conversion(dim_in,val_in,dim_out,val_out,var_id,debug)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in) :: dim_in
    character(*), intent(in) :: dim_out
    real(kind=dp), intent(in)       :: val_in
    integer, intent(in)      :: var_id
    logical, intent(in)      :: debug
    ! output variables
    real(kind=dp), intent(out)      :: val_out
    ! local variables
    character*512            :: message
    character*128            :: dimensions(2)
    integer                  :: idim
    integer                  :: pres_conv(3,3)
    integer                  :: temp_conv(4,4)
    integer                  :: key(2)
    integer                  :: conversion_id
    character*128            :: pres_dim_name(3)
    character*128            :: temp_dim_name(4)
    !
    if(debug) then
       !
       write(*,'(a,a)')     'dim_in  = ',trim(dim_in)
       write(*,'(a,a)')     'dim_out = ',trim(dim_out)
       write(*,'(a,e20.10)')'val_in  = ',val_in
       write(*,'(a,e20.10)')'val_out = ',val_out
       write(*,'(a,i4)')    'var_id  = ',var_id
       !
    end if
    !
    ! assign dimension names
    pres_dim_name(PAS) = 'pascal'
    pres_dim_name(BAR) = 'bar'
    pres_dim_name(PSI) = 'psi'
    !
    temp_dim_name(CELS) = 'celsius'
    temp_dim_name(RANK) = 'rankine'
    temp_dim_name(FAHR) = 'fahrenheit'
    temp_dim_name(KELV) = 'kelvin'
    !
    ! assign conversion table for pressure
    pres_conv(PAS,PAS) = PAS2PAS
    pres_conv(PAS,BAR) = PAS2BAR
    pres_conv(PAS,PSI) = PAS2PSI
    !
    pres_conv(BAR,PAS) = BAR2PAS
    pres_conv(BAR,BAR) = BAR2BAR
    pres_conv(BAR,PSI) = BAR2PSI
    !
    pres_conv(PSI,PAS) = PSI2PAS
    pres_conv(PSI,BAR) = PSI2BAR
    pres_conv(PSI,PSI) = PSI2PSI
    !
    ! assign conversion table for temperature
    temp_conv(CELS,CELS) = CELS2CELS
    temp_conv(CELS,RANK) = CELS2RANK
    temp_conv(CELS,FAHR) = CELS2FAHR
    temp_conv(CELS,KELV) = CELS2KELV
    !
    temp_conv(RANK,CELS) = RANK2CELS
    temp_conv(RANK,RANK) = RANK2RANK
    temp_conv(RANK,FAHR) = RANK2FAHR
    temp_conv(RANK,KELV) = RANK2KELV
    !
    temp_conv(FAHR,CELS) = FAHR2CELS
    temp_conv(FAHR,RANK) = FAHR2RANK
    temp_conv(FAHR,FAHR) = FAHR2FAHR
    temp_conv(FAHR,KELV) = FAHR2KELV
    !
    temp_conv(KELV,CELS) = KELV2CELS
    temp_conv(KELV,RANK) = KELV2RANK
    temp_conv(KELV,FAHR) = KELV2FAHR
    temp_conv(KELV,KELV) = KELV2KELV    
    !
    dimensions(1) = trim(dim_in)
    dimensions(2) = trim(dim_out)
    !
    do idim = 1,2
       !
       ! convert dimension to all lowercase letters
       call low_cap(dimensions(idim),cap2low)
       !
       if(debug) then
          !
          write(*,*)'idim = ',idim
          write(*,*)'dimensions(idim) = |',trim(dimensions(idim)),'|'
          !
       end if
       !
       ! assign a dimension key for input and output dimension
       !
       if(var_id.eq.pres_id) then
          !
          if(trim(dimensions(idim)).eq.'psi') then
             !
             key(idim) = PSI
             !
          elseif(trim(dimensions(idim)).eq.'bar') then
             !
             key(idim) = BAR
             !
          elseif(trim(dimensions(idim)).eq.'pa'.or.trim(dimensions(idim)).eq.'pascal') then
             !
             key(idim) = PAS
             !
          else
             !
             write(message,'(A,A,A)')'pressure dimension ',trim(dimensions(idim)),' not supported'
             call print_message(message,RED)
             stop
             !
          end if
          !
       elseif(var_id.eq.temp_id) then
          !
          ! assign dimension key for input and output dimension
          if(trim(dimensions(idim)).eq.'c'.or.trim(dimensions(idim)).eq.'celsius') then
             !
             key(idim) = CELS
             !
          elseif(trim(dimensions(idim)).eq.'r'.or.trim(dimensions(idim)).eq.'rankine') then
             !
             key(idim) = RANK
             !
          elseif(trim(dimensions(idim)).eq.'f'.or.trim(dimensions(idim)).eq.'fahrenheit') then
             !
             key(idim) = FAHR
             !
          elseif(trim(dimensions(idim)).eq.'k'.or.trim(dimensions(idim)).eq.'kelvin') then
             !
             key(idim) = KELV
             !
          else
             !
             write(message,'(A,A,A)')'Temperature dimension ',trim(dimensions(idim)),' not supported'
             call print_message(message,RED) 
             stop
             !
          end if
          !
       else
          !
          ! dimension conversion of this variable not supported
          write(message,'(A,I6,A)')'Dimension conversion of variable id: ',var_id,' not supported'
          call print_message(message,RED)
          stop
          !
       end if
       !
       if(debug) then
          !
          write(*,'(a,i4,a,i4)')'key(',idim,') = ',key(idim)
          !
       end if
       !
    end do
    !
    !--------------------------------------------------
    ! pressure conversion
    !
    ! determine the conversion id from table
    !--------------------------------------------------
    !
    if(var_id.eq.pres_id) then
       !
       ! look up from pressure conversion table
       !
       write(*,'(A,A,A,A)')'Convert pressure from: ',trim(pres_dim_name(key(1))),' -> ',trim(pres_dim_name(key(2)))
       !
       conversion_id = pres_conv(key(1),key(2))
       !
       if(conversion_id.eq.PAS2PAS) then
          !
          !--------------------------------------------------
          ! pascal -> pascal
          !--------------------------------------------------
          val_out = val_in
          !
       elseif(conversion_id.eq.PAS2BAR) then
          !
          !--------------------------------------------------
          ! pascal -> bar
          !--------------------------------------------------
          call BAR2PAS_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.PAS2PSI) then
          !
          !--------------------------------------------------
          ! pascal -> psi
          !--------------------------------------------------
          call PAS2PSI_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.BAR2PAS) then
          !
          !--------------------------------------------------
          ! bar -> pascal
          !--------------------------------------------------
          call BAR2PAS_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.BAR2BAR) then
          !
          !--------------------------------------------------
          ! bar -> bar
          !--------------------------------------------------
          val_out = val_in
          !
       elseif(conversion_id.eq.BAR2PSI) then
          !
          !--------------------------------------------------
          ! bar -> psi
          !--------------------------------------------------
          call BAR2PSI_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.PSI2PAS) then
          !
          !--------------------------------------------------
          ! psi -> pascal
          !--------------------------------------------------
          call PSI2PAS_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.PSI2BAR) then
          !
          !--------------------------------------------------
          ! psi -> bar
          !--------------------------------------------------
          call PSI2BAR_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.PSI2PSI) then
          !
          !--------------------------------------------------
          ! psi -> psi
          !--------------------------------------------------
          val_out = val_in
          !
       end if
       !
    end if
    !
    !--------------------------------------------------
    ! temperature conversion
    !
    ! determine the conversion id from table
    !--------------------------------------------------
    !
    if(var_id.eq.temp_id) then
       !
       ! look up from pressure conversion table
       !
       write(*,'(A,A,A,A)')'Convert temperautre from: ',trim(temp_dim_name(key(1))),' -> ',trim(temp_dim_name(key(2)))
       !
       conversion_id = temp_conv(key(1),key(2))
       !
       if(conversion_id.eq.CELS2CELS) then
          !
          !--------------------------------------------------
          ! celsius -> celsius
          !--------------------------------------------------
          val_out = val_in
          !
       elseif(conversion_id.eq.CELS2RANK) then
          !
          !--------------------------------------------------
          ! celsius -> rankine
          !--------------------------------------------------
          call CELS2RANK_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.CELS2FAHR) then
          !
          !--------------------------------------------------
          ! celsius -> fahrenheit
          !--------------------------------------------------
          call CELS2FAHR_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.CELS2KELV) then
          !
          !--------------------------------------------------
          ! celsius -> kelvin
          !--------------------------------------------------
          call CELS2KELV_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.RANK2CELS) then
          !
          !--------------------------------------------------
          ! rankine -> celsius
          !--------------------------------------------------
          call RANK2CELS_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.RANK2RANK) then
          !
          !--------------------------------------------------
          ! rankine -> rankine
          !--------------------------------------------------
          val_out = val_in
          !
       elseif(conversion_id.eq.RANK2FAHR) then
          !
          !--------------------------------------------------
          ! rankine -> fahrenheit
          !--------------------------------------------------
          call RANK2FAHR_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.RANK2KELV) then
          !
          !--------------------------------------------------
          ! rankine -> kelvin
          !--------------------------------------------------
          call RANK2KELV_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.FAHR2CELS) then
          !
          !--------------------------------------------------
          ! fahrenheit -> celsius
          !--------------------------------------------------
          call FAHR2CELS_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.FAHR2RANK) then
          !
          !--------------------------------------------------
          ! fahrenheit -> rankine
          !--------------------------------------------------
          call FAHR2RANK_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.FAHR2FAHR) then
          !
          !--------------------------------------------------
          ! fahrenheit -> fahrenheit
          !--------------------------------------------------
          val_out = val_in
          !
       elseif(conversion_id.eq.FAHR2KELV) then
          !
          !--------------------------------------------------
          ! fahrenheit -> kelvin
          !--------------------------------------------------
          call FAHR2KELV_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.KELV2CELS) then
          !
          !--------------------------------------------------
          ! kelvin -> celsius
          !--------------------------------------------------
          call KELV2CELS_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.KELV2RANK) then
          !
          !--------------------------------------------------
          ! kelvin -> rankine
          !--------------------------------------------------
          call KELV2RANK_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.KELV2FAHR) then
          !
          !--------------------------------------------------
          ! kelvin -> fahrenheit
          !--------------------------------------------------
          call KELV2FAHR_conversion(val_in,val_out)
          !
       elseif(conversion_id.eq.KELV2KELV) then
          !
          !--------------------------------------------------
          ! kelvin -> kelvin
          !--------------------------------------------------
          val_out = val_in
          !
       end if
       !
    end if
    !
  end subroutine dimension_conversion
  !====================================================================================================
  ! celsius -> kelvin
  !====================================================================================================
  subroutine CELS2KELV_conversion(tempCELS,tempKELV)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempCELS
    real(kind=dp), intent(out) :: tempKELV
    !
    tempKELV = tempCELS + 273.15d0
    !
  end subroutine CELS2KELV_conversion
  !====================================================================================================
  ! celsius -> rankine
  !====================================================================================================
  subroutine CELS2RANK_conversion(tempCELS,tempRANK)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempCELS
    real(kind=dp), intent(out) :: tempRANK
    !
    tempRANK = 0.d0
    !
    write(*,*)'not implemented yet'
    stop
    !
  end subroutine CELS2RANK_conversion
  !====================================================================================================
  ! celsius -> fahrenheit
  !====================================================================================================
  subroutine CELS2FAHR_conversion(tempCELS,tempFAHR)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempCELS
    real(kind=dp), intent(out) :: tempFAHR
    !
    tempFAHR = 0.d0
    !
    write(*,*)'not implemented yet'
    stop
    !
  end subroutine CELS2FAHR_conversion
  !====================================================================================================
  ! rankine -> kelvin
  !====================================================================================================
  subroutine RANK2KELV_conversion(tempRANK,tempKELV)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempRANK
    real(kind=dp), intent(out) :: tempKELV
    !
    tempKELV = tempRANK * 5.d0/9.d0
    !
  end subroutine RANK2KELV_conversion
  !====================================================================================================
  ! rankine -> celsius
  !====================================================================================================
  subroutine RANK2CELS_conversion(tempRANK,tempCELS)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempRANK
    real(kind=dp), intent(out) :: tempCELS
    !
    tempCELS = 0.d0
    !
    write(*,*)'not implemented yet'
    stop
    !
  end subroutine RANK2CELS_conversion  
  !====================================================================================================
  ! rankine -> fahrenheit
  !====================================================================================================
  subroutine RANK2FAHR_conversion(tempRANK,tempFAHR)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempRANK
    real(kind=dp), intent(out) :: tempFAHR
    !
    tempFAHR = 0.d0
    !
    write(*,*)'not implemented yet'
    stop
    !
  end subroutine RANK2FAHR_conversion
  !====================================================================================================
  ! fahrenheit -> kelvin
  !====================================================================================================
  subroutine FAHR2KELV_conversion(tempFAHR,tempKELV)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempFAHR
    real(kind=dp), intent(out) :: tempKELV
    !
    tempKELV = (tempFAHR + 459.67d0)*5.d0/9.d0
    !
  end subroutine FAHR2KELV_conversion
  !====================================================================================================
  ! fahrenheit -> rankine
  !====================================================================================================
  subroutine FAHR2RANK_conversion(tempFAHR,tempRANK)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempFAHR
    real(kind=dp), intent(out) :: tempRANK
    !
    tempRANK = 0.d0
    !
    write(*,*)'not implemented yet'
    stop
    !
  end subroutine FAHR2RANK_conversion
  !====================================================================================================
  ! fahrenheit -> celsius
  !====================================================================================================
  subroutine FAHR2CELS_conversion(tempFAHR,tempCELS)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempFAHR
    real(kind=dp), intent(out) :: tempCELS
    !
    tempCELS = 0.d0
    !
    write(*,*)'not implemented yet'
    stop
    !
  end subroutine FAHR2CELS_conversion
  !====================================================================================================
  ! kelvin -> celsius
  !====================================================================================================
  subroutine KELV2CELS_conversion(tempKELV,tempCELS)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempKELV
    real(kind=dp), intent(out) :: tempCELS
    !
    tempCELS = 0.d0
    !
    write(*,*)'not implemented yet'
    stop
    !
  end subroutine KELV2CELS_conversion
  !====================================================================================================
  ! kelvin -> rankine
  !====================================================================================================
  subroutine KELV2RANK_conversion(tempKELV,tempRANK)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempKELV
    real(kind=dp), intent(out) :: tempRANK
    !
    tempRANK = 0.d0
    !
    write(*,*)'not implemented yet'
    stop
    !
  end subroutine KELV2RANK_conversion
  !====================================================================================================
  ! kelvin -> fahrenheit
  !====================================================================================================
  subroutine KELV2FAHR_conversion(tempKELV,tempFAHR)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: tempKELV
    real(kind=dp), intent(out) :: tempFAHR
    !
    tempFAHR = 0.d0
    !
    write(*,*)'not implemented yet'
    stop
    !
  end subroutine KELV2FAHR_conversion
  !====================================================================================================
  ! psi -> pascal
  !====================================================================================================
  subroutine PSI2PAS_conversion(presPSI,presPAS)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: presPSI
    real(kind=dp), intent(out) :: presPAS
    !
    presPAS = presPSI * 6894.75729
    !
  end subroutine PSI2PAS_conversion
  !====================================================================================================
  ! pascal -> psi
  !====================================================================================================
  subroutine PAS2PSI_conversion(presPAS,presPSI)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: presPAS
    real(kind=dp), intent(out) :: presPSI
    !
    presPSI = presPAS/6894.75729
    !
  end subroutine PAS2PSI_conversion
  !====================================================================================================
  ! bar -> pascal
  !====================================================================================================
  subroutine BAR2PAS_conversion(presBAR,presPAS)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: presBAR
    real(kind=dp), intent(out) :: presPAS
    !
    presPAS = presBAR * 100000.d0
    !
  end subroutine BAR2PAS_conversion
  !====================================================================================================
  ! pascal -> bar
  !====================================================================================================
  subroutine PAS2BAR_conversion(presPAS,presBAR)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: presPAS
    real(kind=dp), intent(out) :: presBAR
    !
    presBAR = presPAS/(100000.d0)
    !
  end subroutine PAS2BAR_conversion
  !====================================================================================================
  ! psi -> bar
  !====================================================================================================
  subroutine PSI2BAR_conversion(presPSI,presBAR)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: presPSI
    real(kind=dp), intent(out) :: presBAR
    !
    presBAR = 0.d0
    !
    write(*,*)'not implemented yet'
    stop
    !
  end subroutine PSI2BAR_conversion
  !====================================================================================================
  ! psi -> pascal
  !====================================================================================================
  subroutine BAR2PSI_conversion(presBAR,presPSI)
    !
    implicit none
    !
    real(kind=dp), intent(in)  :: presBAR
    real(kind=dp), intent(out) :: presPSI
    !
    presPSI = 0.d0
    !
    write(*,*)'not implemented yet'
    stop
    !
  end subroutine BAR2PSI_conversion
  !
end module dimension_conversion_mod
