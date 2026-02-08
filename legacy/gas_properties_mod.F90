!====================================================================================================
! save gas properties
! date: 04/23/2015
!====================================================================================================
module gas_properties_mod
  !
  ! load modules
  use util_mod
#if(PAR==1)
  use parallel_mod
#endif
  !
  implicit none
  !
  ! global variables
  real(kind=dp)             :: gamma
  real(kind=dp)             :: Rgas
  real(kind=dp)             :: cp
  real(kind=dp)             :: cv
  real(kind=dp)             :: Pr
  integer, parameter :: air = 1
  integer, parameter :: nitrogen = 2
  integer, parameter :: user_defined = 3
  integer            :: gas_type
  !
  ! contains statement -> include subroutines after contains statement
CONTAINS
  !====================================================================================================
  ! get input for gas properties
  !====================================================================================================
  subroutine get_gas_properties
    !
    implicit none
    !
    ! local variables
    integer             :: key
    character*512       :: message
    real(kind=dp)              :: GasPropertyVal
    character*128       :: GasProperty
    !
    write(*,'(a)')'Select method to determine gas properties'
    write(*,'(a)')'(1) Standard air'
    write(*,'(a)')'(2) Standard nitrogen'
    write(*,'(a)')'(3) User input'
    read(*,*)key
    !
    if(key.eq.1) then
       !
       !--------------------------------------------------
       ! Standard air
       !--------------------------------------------------
       !
       ! ratio of heat capacities
       gamma = 1.4d0
       ! gas constant
       Rgas = 287.15d0
       ! specific heat at constant pressure
       cp = Rgas*(gamma)/(gamma-1.d0)
       ! specific heat at constant volume
       cv = cp-Rgas
       ! Prandtl number
       Pr = 0.71
       ! gas type
       gas_type = air
       !
    elseif(key.eq.2) then
       !--------------------------------------------------
       ! Standard nitrogen
       !--------------------------------------------------
       !
       ! ratio of heat capacities
       gamma = 1.4d0
       ! gas constant
       Rgas = 296.80
       ! specific heat at constant pressure
       cp = Rgas*(gamma)/(gamma-1.d0)
       ! specific heat at constant volume
       cv = cp-Rgas
       ! Prandtl number
       Pr = 0.72
       ! gas type
       gas_type = nitrogen
       !
    elseif(key.eq.3) then
       !
       !--------------------------------------------------
       ! get user input
       !--------------------------------------------------
       !
       write(*,'(a)')'gamma'
       read(*,*)gamma
       write(*,'(a)')'Which gas property (R/Rgas or cp), value'
       read(*,*)GasProperty,GasPropertyVal    
       !
       if(trim(GasProperty).eq.'Rgas'.or.trim(GasProperty).eq.'rgas'.or.trim(GasProperty).eq.'RGAS'.or.trim(GasProperty).eq.'R'.or.trim(GasProperty).eq.'r') then
          !
          Rgas = GasPropertyVal
          cp = Rgas*gamma/(gamma-1.d0)
          !
       elseif(trim(GasProperty).eq.'cp'.or.trim(GasProperty).eq.'CP'.or.trim(GasProperty).eq.'Cp') then
          !
          cp = GasPropertyVal
          Rgas = cp*(gamma-1.d0)/gamma
          !
       else
          !
          write(message,'(A,A,A)')'Gas property ',trim(GasProperty),' not known, provide different input gas constant'
          call print_message(message,RED)
          stop
          !
       end if
       !
       cv = cp-Rgas
       !
       write(*,'(a)')'Prandtl number'
       read(*,*)Pr
       !
       ! set gas type variable
       gas_type = user_defined
       !
    else
       !
       write(message,'(A,I3,A)')'Option :',key,' to compute gas properties not supported'
       call print_message(message,RED)
       stop
       !
    end if
    !
    ! write info for user
    write(*,'(a)')'Gas Properties:'
    write(*,'(A,F24.16,A)')'gamma = ',gamma,' [-]'
    write(*,'(A,F24.16,A)')'Rgas  = ',Rgas,' [J/(kgK)]'
    write(*,'(A,F24.16,A)')'cp    = ',cp,' [J/(kgK)]'
    write(*,'(A,F24.16,A)')'cv    = ',cv,' [J/(kgK)]'
    write(*,'(A,F24.16,A)')'Pr    = ',Pr,' [-]'
    !
  end subroutine get_gas_properties
  !
#if(PAR==1)
  !====================================================================================================
  ! broadcast gas properties
  !====================================================================================================
  subroutine broadcast_gas_properties
    !
    integer :: ierr
    !
    call mpi_bcast(Rgas,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(gamma,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(cp,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(cv,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    call mpi_bcast(Pr,1,mpi_double_precision,0,MPI_COMM_WORLD,ierr)
    !
  end subroutine broadcast_gas_properties
  !
#endif
  !
end module gas_properties_mod
