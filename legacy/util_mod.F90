!====================================================================================================
! This module contains various subroutines that are frequently needed by other modules
!====================================================================================================
module util_mod
  !
  ! use modules
  use params
  use ios
  !
  implicit none
  !
  ! global variables
  integer, parameter :: cap2low = 1
  integer, parameter :: low2cap = 2
  !
  ! contains statement -> include subroutines after contains statement
contains
  !----------------------------------------------------------------------------------------------------
  ! count lines in ascii input file
  !----------------------------------------------------------------------------------------------------
  subroutine util_count_lines(fname,nlines,debug)
    !
    implicit none
    !
    ! input/output variables
    character(*), intent(in) :: fname
    integer, intent(out)     :: nlines
    logical, intent(in)      :: debug
    ! local variables
    integer                  :: ierr
    integer                  :: funit
    character*128            :: dummy
    !
    ! find free unit number
    call util_find_free_funit(funit,debug)
    !
    ! open file
    open(unit=funit,file=trim(fname),status='unknown')
    !
    ! initialize ierr flag with 0 => no error
    ierr = 0
    !
    ! initialize nlines with 0
    nlines = 0
    !
    do while(ierr.eq.0)
       !
       ! increase nlines count
       nlines = nlines + 1
       !       
       ! read line in input file => grep iostat => ierr = 0 - no problem, ierr != 0 - could not read data
       read(funit,*,iostat=ierr)dummy
       !
    end do
    !
    ! subtract one line
    nlines = nlines - 1
    !
    ! close file
    close(funit)
    !
    ! return to calling routine
    return
    ! 
  end subroutine util_count_lines
  !----------------------------------------------------------------------------------------------------
  ! count number of lines of an ascii file
  !----------------------------------------------------------------------------------------------------
  subroutine count_lines_ascii(fname,nlines,nsets,nheader)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in) :: fname
    ! output variables
    integer, intent(out)     :: nlines
    integer, intent(out)     :: nsets
    integer, intent(out)     :: nheader
    ! local variables
    integer                  :: ierr
    character*128            :: dummy
    integer                  :: nlines_tmp(50000)
    logical                  :: set_separator
    integer                  :: ichar
    integer                  :: iset
    integer                  :: funit
    integer                  :: dymmy_int
    integer                  :: ierr_int
    real(kind=dp)                   :: dummy_real
    integer                  :: ierr_real
    character*4              :: substring
    logical                  :: is_header
    !
    ! set file unit
    call util_find_free_funit(funit,.false.)
    !
    ! initialize nlines 
    nlines = 0
    !
    ! initialize nsets
    nsets = 1
    !
    ! open file
    open(unit=funit,file=trim(fname),status='old',iostat=ierr) 
    !
    ! check if file was opened properly
    if(ierr.ne.0) then
       !
       write(*,'(A,A,A)')'Input file: ',trim(fname),' could not be opened'
       stop
       !
    end if
    !
    ! check if there is a header in the file
    nheader = 0
    !
    do 
       !
       ! try to read integer value
       read(funit,*,iostat=ierr_int)dymmy_int
       backspace(funit)
       ! try to read real value
       read(funit,*,iostat=ierr_real)dummy_real
       !
       if(ierr_int.ne.0.and.ierr_real.ne.0) then
          !
          ! assume this line belongs to the global file header
          is_header = .true.
          !
          ! read in header line as character
          backspace(funit)
          read(funit,*)dummy
          !
          do ichar = 1,125
             !
             substring=dummy(ichar:ichar+3)
             !
             call low_cap(substring,cap2low)
             !
             if(trim(substring).eq.'zone') then
                !
                ! this is not part of the global header it's the set information
                is_header = .true.
                !
             end if
             !
          end do
          !
          if(is_header) then
             nheader = nheader + 1
          end if
          !
       else
          !
          ! input is either a real or an integer => header end
          !
          exit
          !
       end if
       !
    end do
    !
    ! go back one line
    backspace(funit)
    !
    ! initialize line count
    nlines_tmp = 0
    !
    ! count lines
    do
       !
       set_separator = .false.
       !
       read(funit,*,iostat=ierr)dummy
       !
       if(ierr.eq.0) then
          !
          ! parse for zone separators
          do ichar = 1,128
             if(dummy(ichar:ichar).eq.'&') then
                set_separator = .true.
                exit
             end if
          end do
          !
          do ichar = 1,125
             !
             substring=dummy(ichar:ichar+3)
             !
             call low_cap(substring,cap2low)
             !
             if(trim(substring).eq.'zone') then
                !
                set_separator = .true.
                exit
                !
             end if
             !
          end do
          !
          if(set_separator) then
             nsets = nsets + 1
             nlines_tmp(nsets) = 0
          else
             nlines_tmp(nsets) = nlines_tmp(nsets) + 1
          end if
          !
       else
          !
          exit
          !
       end if
       !
    end do
    !
    ! close file
    close(funit)
    !
    ! if the line count of the last set is zero it is not a set and the separation token & was superfluous -> ignore last set
    if(nlines_tmp(nsets).eq.0) then
       nsets = nsets - 1
    end if
    !
    nlines = nlines_tmp(1)
    !
    ! if there are multiple sets then the line count for each set should be equal
    if(nsets.gt.1) then
       !
       do iset = 2,nsets
          !
          if(nlines_tmp(iset).ne.nlines) then
             !
             write(*,'(A,I15,A,I4,A,A,A,I15,A)')'Warning: nlines = ',nlines_tmp(iset),' of set ',iset,' of input file ',trim(fname),' /= nlines = ',nlines,' of set 1'
             !
          end if
          !
       end do
       !
    end if
    !
    ! print info on screen
    write(*,'(A,I6,A,A,A)')'number of lines        = ',nlines,' (',trim(fname),')'
    write(*,'(A,I6,A,A,A)')'number of sets         = ',nsets,' (',trim(fname),')'
    write(*,'(A,I6,A,A,A)')'number of header lines = ',nheader,' (',trim(fname),')'
    !
  end subroutine count_lines_ascii
  !----------------------------------------------------------------------------------------------------
  ! count columns in a block data ascii file
  !----------------------------------------------------------------------------------------------------
  subroutine count_columns_ascii(fname,nskip,ncols)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in)           :: fname
    integer, intent(in)                :: nskip
    ! output variables
    integer, intent(out)               :: ncols
    ! local variables
    integer                            :: ierr
    integer :: i
    integer                            :: funit
    !
    ! set file unit
    call util_find_free_funit(funit,.false.)
    !
    ! initialize nlines 
    ncols = 0
    !
    ! open file
    open(unit=funit,file=trim(fname),status='old',iostat=ierr) 
    !
    ! read past header
    do i = 1,nskip
       !
       read(funit,*)dummy
       !
    end do
    !
    ! read in one line as character string
    read(funit,'(a)')dummy
    !
    call util_split_str2col(dummy,ncols)
    !
    close(funit)
    !
  end subroutine count_columns_ascii
  !----------------------------------------------------------------------------------------------------
  ! subroutine to split string into columns
  !----------------------------------------------------------------------------------------------------
  subroutine util_split_str2col(dummy,ncols)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in)           :: dummy
    ! output variables
    integer, intent(out)               :: ncols
    ! local variables
    integer                            :: ierr
    integer                            :: iword
    integer                            :: nchar_max
    integer                            :: i_word_s(100)
    integer                            :: i_word_e(100)
    integer                            :: iparse_start
    integer                            :: i,j
    integer                            :: ichar
    character*128                      :: word
    integer, parameter                 :: type_integer = 0
    integer, parameter                 :: type_real = 1
    integer, dimension(:), allocatable :: number_type
    character*600                      :: info_line
    integer                            :: funit
    logical                            :: debug
    !
    ! initialize nlines 
    ncols = 0
    !
    ! count length of character string
    nchar_max = len(trim(dummy))
    !
    if(debug) then
       !
       write(*,'(A,A)')  'Input line           = ',trim(dummy)
       write(*,'(A,I10)')'number of characters = ',nchar_max
       !
    end if
    !
    ncols = 0
    iparse_start=1
    i = 0
    !
    ! parse character string and count columns
    do while (iparse_start.le.nchar_max)
       !
       ichar = iparse_start+i
       !
       if(dummy(ichar:ichar).ne.' ') then
          !
          i_word_s(ncols+1)=ichar
          !
          do j = ichar,ichar+100
             !
             if(dummy(j:j).eq.' ') then
                !
                i_word_e(ncols+1) = j-1
                iparse_start = j
                i=0
                ncols = ncols + 1
                exit
                !
             end if
             !
          end do
          !
       else
          i = i+1
       end if
       !
    end do
    !
    allocate(number_type(ncols))
    !
    number_type = 0
    !
    do i=1,ncols
       !
       word = dummy(i_word_s(i):i_word_e(i))
       !
       ! check if input is integer or real
       do ichar = 1,128
          !
          if(word(ichar:ichar).eq.'.'.or.word(ichar:ichar).eq.'E'.or.word(ichar:ichar).eq.'+'.or.word(ichar:ichar).eq.'-'.or.word(ichar:ichar).eq.'e') then
             !
             number_type(i)=type_real
             !
          end if
          !
       end do
       !
    end do
    !
    write(info_line,'(a)')'Type: '
    !
    do i=1,ncols
       !
       if(number_type(i).eq.type_integer) then
          !
          if(i.lt.ncols) then
             write(info_line,'(A,A)')trim(info_line),' Integer,'
          else
             write(info_line,'(A,A)')trim(info_line),' Integer'
          end if
          !
       elseif(number_type(i).eq.type_real) then
          !
          if(i.lt.ncols) then
             write(info_line,'(A,A)')trim(info_line),' Real,'
          else
             write(info_line,'(A,A)')trim(info_line),' Real'
          end if
          !
       else
          !
          write(info_line,'(A,A)')trim(info_line),' unknown'          
          !
       end if
       !
    end do
    !
    ! print info on screen
    write(*,'(a,i6)')'number of columns = ',ncols
    write(*,'(a)')trim(info_line)
    !
    close(funit)
    !
  end subroutine util_split_str2col
  !----------------------------------------------------------------------------------------------------
  ! Read in a grid file (1D)
  ! The file should only consist of one column of coordinates
  !----------------------------------------------------------------------------------------------------
  subroutine ReadGridData1D(nopts,grid,fname)
    !
    implicit none
    !
    ! input variables
    integer, intent(in)       :: nopts
    character*128, intent(in) :: fname
    ! output variables
    real(kind=dp), intent(out)       :: grid(1:nopts)
    ! local variables
    integer                   :: ierr
    integer                   :: i
    !
    ! open grid file
    open(unit=200,file=trim(fname),status='old',iostat=ierr)
    !
    do i=1,nopts
       read(200,*,iostat=ierr)grid(i)
    end do
    !
    ! close grid file
    close(200)
    !
  end subroutine ReadGridData1D
  !----------------------------------------------------------------------------------------------------
  ! read ascii file with multiple columns
  !----------------------------------------------------------------------------------------------------
  subroutine read_ascii_data(nopts,ncols,nheader,set_in,fname,data_in)
    !
    implicit none
    !
    ! input variables
    integer, intent(in)      :: nopts
    integer, intent(in)      :: ncols
    integer, intent(in)      :: nheader
    integer, intent(in)      :: set_in
    character(*), intent(in) :: fname
    ! output variables
    real(kind=dp), intent(out)      :: data_in(1:nopts,1:ncols)
    ! local variables
    integer                  :: ierr
    integer                  :: i,j
    real(kind=dp)                   :: dummy
    character*128            :: dummyChar
    integer                  :: funit
    !
    call util_find_free_funit(funit,.false.)
    !
    ! open grid file
    open(unit=funit,file=trim(fname),status='old',iostat=ierr)
    !
    if(ierr.ne.0) then
       write(*,'(a)')'Error: Could not open input file: ',trim(fname)
       stop
    end if
    !
    ! read past header
    do j=1,nheader
       !
       read(funit,*)dummyChar
       !
    end do
    !
    ! read past sets to get to input set
    do j=1,set_in-1
       !
       do i=1,nopts
          !
          read(funit,*)dummy
          !
       end do
       !
       read(funit,*)dummyChar
       !
    end do
    !
    ! we arrived at data set that needs to be read
    !
    do i=1,nopts
       read(funit,*,iostat=ierr)(data_in(i,j),j=1,ncols)
    end do
    !
    ! close grid file
    close(funit)
    !
  end subroutine read_ascii_data
  !----------------------------------------------------------------------------------------------------
  ! read header lines
  !----------------------------------------------------------------------------------------------------
  subroutine read_ascii_file_header(fname,nheader,header_lines)
    !
    implicit none
    !
    ! input/output variables
    character*128, intent(in)  :: fname
    integer, intent(in)        :: nheader
    character(*), intent(out) :: header_lines(nheader)
    ! local variables
    integer                    :: funit
    integer                    :: i
    !
    call util_find_free_funit(funit,.false.)
    !
    open(unit=funit,file=trim(fname),status='unknown')
    !
    do i = 1,nheader
       !
       read(funit,'(a)')header_lines(i)
       !
    end do
    !
    close(funit)
    !
  end subroutine read_ascii_file_header
  !----------------------------------------------------------------------------------------------------
  ! write tecplot ascii header
  !----------------------------------------------------------------------------------------------------
  subroutine write_tecplot_header(funit,nvars,var_names,Title)
    !
    implicit none
    !
    ! input variables
    integer, intent(in)       :: nvars
    character*128, intent(in) :: var_names(nvars)
    character*128, intent(in) :: Title
    integer, intent(in)       :: funit
    ! local variables
    character*512             :: VarHeader
    integer                   :: i
    !
    ! write Title
    write(funit,'(A,A,A)')'TITLE="',trim(Title),'"'
    !
    ! assemble string for variable header
    VarHeader = 'Variables = '
    !
    do i=1,nvars
       !
       if(i.lt.nvars) then
          write(VarHeader,'(A,A,A,A)')trim(VarHeader),'"',trim(var_names(i)),'",'
       else
          write(VarHeader,'(A,A,A,A)')trim(VarHeader),'"',trim(var_names(i)),'"'
       end if
       !
    end do
    !
    write(funit,'(a)')trim(VarHeader)
    !
  end subroutine write_tecplot_header
  !----------------------------------------------------------------------------------------------------
  ! create a timestamp string for an integer variable
  !----------------------------------------------------------------------------------------------------
  subroutine create_time_stamp(nt,time_stamp,type)
    !
    implicit none
    !
    ! input variable
    integer, intent(in)        :: nt
    integer, intent(in)        :: type
    ! output variable
    character*128, intent(out) :: time_stamp
    ! local variables
    character*20               :: fmt1,fmt2,fmt3,fmt4
    integer                    :: dig1,dig2
    character*128              :: str_real
    !
    if(type.eq.type_int) then
       !
       if(nt.lt.10) then
          !
          write(time_stamp,'(A,I1)')'nt_0000000',nt
          !
       elseif(nt.lt.100) then
          !
          write(time_stamp,'(A,I2)')'nt_000000',nt
          !
       elseif(nt.lt.1000) then
          !
          write(time_stamp,'(A,I3)')'nt_00000',nt
          !
       elseif(nt.lt.10000) then
          !
          write(time_stamp,'(A,I4)')'nt_0000',nt
          !
       elseif(nt.lt.100000) then
          !
          write(time_stamp,'(A,I5)')'nt_000',nt
          !
       elseif(nt.lt.1000000) then
          !
          write(time_stamp,'(A,I6)')'nt_00',nt
          !
       elseif(nt.lt.10000000) then
          !
          write(time_stamp,'(A,I7)')'nt_0',nt       
          !
       elseif(nt.lt.100000000) then
          !
          write(time_stamp,'(A,I8)')'nt_',nt       
          !
       else
          !
          write(*,'(A,I10,A)')'Error: Timestamps for nt = ',nt,' cannot be created, nt is too large, adjust routine create_timesampInt in utilities.F90'
          stop
          !
       end if
       !
    else
       !
       ! create 4 digits after the decimal point
       dig1 = 4
       dig2 = dig1+6
       !
       if(dig1.lt.10) then
          write(fmt1,'(a)')'I1'
       elseif(dig1.lt.100) then
          write(fmt1,'(a)')'I2'
       elseif(dig1.lt.1000) then
          write(fmt1,'(a)')'I3'
       end if
       !
       if(dig2.lt.10) then
          write(fmt2,'(a)')'I1'
       elseif(dig2.lt.100) then
          write(fmt2,'(a)')'I2'
       elseif(dig2.lt.1000) then
          write(fmt2,'(a)')'I3'
       end if
       !
       write(fmt3,'(A,A,A,A,A)')'(A,',trim(fmt2),',A,',trim(fmt1),',A)'
       !
       ! assign fmt string
       write(fmt4,fmt3)'(E',dig2,'.',dig1,')'
       !
       write(str_real,fmt4)nt
       !
       ! replace decimal point with pt
       call dec2pt(str_real)
       !
       ! assemble final time stamp
       write(time_stamp,'(a,a)')'t_',trim(str_real)
       !
    end if
    !
  end subroutine create_time_stamp
  !----------------------------------------------------------------------------------------------------
  ! check if file exists
  !----------------------------------------------------------------------------------------------------
  subroutine check_file_status(fname,file_exists)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in)  :: fname
    ! local variables
    logical, intent(out)      :: file_exists
    character*512             :: message
    !
    file_exists = .false.
    !
    ! check if file exists
    inquire(file=fname,exist=file_exists)
    !
  end subroutine check_file_status
  !----------------------------------------------------------------------------------------------------
  ! identify flow variable
  !----------------------------------------------------------------------------------------------------
  subroutine find_var_id(nvars,var_names,variable,var_id)
    !
    use params
    !
    implicit none
    !
    ! input variables
    integer, intent(in)      :: nvars
    integer, intent(in)      :: variable
    character(*), intent(in) :: var_names(1:nvars)
    ! output variables
    integer, intent(out)     :: var_id
    ! local variables 
    integer                  :: ivar
    character*128            :: var_name_loc
    logical                  :: debug
    character*512            :: message
    logical                  :: switch
    logical                  :: is_ampl
    logical                  :: is_phas
    integer                  :: is,ie
    character*128            :: str_tmp
    !
    debug = .false.
    !
    var_id = -99
    !
    do ivar = 1,nvars
       !
       ! save variable name in local array
       var_name_loc = var_names(ivar)
       !
       ! convert all uppercase letters to lowercase letters
       call low_cap(var_name_loc,cap2low)
       !
       ! delete leading spaces
       call delete_leading_spaces(var_name_loc)
       !
       if(variable.eq.dens_id) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! density
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'rho'
          !
          if(trim(var_name_loc).eq.str_tmp) then
             !
             switch = .true.
             !
          else
             !
             switch = .false.
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'r'
             !
             if(trim(var_name_loc).eq.str_tmp) then
                !
                switch = .true.
                !
             else
                !
                switch = .false.
                !
             end if
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'n1v1'
             !
             if(trim(var_name_loc).eq.str_tmp) then
                !
                switch = .true.
                !
             else
                !
                switch = .false.
                !
             end if
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'dens'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'density'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          str_tmp = 'ampl'
          call util_find_substring(var_name_loc,str_tmp,is_ampl,is,ie,debug)
          str_tmp = 'phas'
          call util_find_substring(var_name_loc,str_tmp,is_phas,is,ie,debug)
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.uvel_id) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! u-velocity
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'uvel'
          call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
          !
          if(.not.switch) then
             !
             str_tmp = 'u-vel'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'n1v2'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'u-velocity'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          str_tmp = 'ampl'
          call util_find_substring(var_name_loc,str_tmp,is_ampl,is,ie,debug)
          str_tmp = 'phas'
          call util_find_substring(var_name_loc,str_tmp,is_phas,is,ie,debug)
          !
          if(.not.switch) then
             !
             str_tmp = 'u'
             if(trim(var_name_loc).eq.str_tmp) then
                !
                switch = .true.
                !
             end if
             !
          end if
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.vvel_id) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! v-velocity
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'vvel'
          call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
          !
          if(.not.switch) then
             !
             str_tmp = 'v-vel'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'n1v3'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'v-velocity'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          str_tmp = 'ampl'
          call util_find_substring(var_name_loc,str_tmp,is_ampl,is,ie,debug)
          str_tmp = 'phas'
          call util_find_substring(var_name_loc,str_tmp,is_phas,is,ie,debug)
          !
          if(.not.switch) then
             !
             str_tmp = 'v'
             if(trim(var_name_loc).eq.str_tmp) then
                !
                switch = .true.
                !
             end if
             !
          end if
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.wvel_id) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! w-velocity
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'wvel'
          call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
          !
          if(.not.switch) then
             !
             str_tmp = 'w-vel'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'w-velocity'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          str_tmp = 'ampl'
          call util_find_substring(var_name_loc,str_tmp,is_ampl,is,ie,debug)
          str_tmp = 'phas'
          call util_find_substring(var_name_loc,str_tmp,is_phas,is,ie,debug)
          !
          if(.not.switch) then
             !
             str_tmp = 'w'
             if(trim(var_name_loc).eq.str_tmp) then
                !
                switch = .true.
                !
             end if
             !
          end if
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.pres_id) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! pressure
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'p'
          if(trim(var_name_loc).eq.str_tmp) then
             !
             switch = .true.
             !
          else
             !
             switch = .false.
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'pres'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'pressure'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          str_tmp = 'ampl'
          call util_find_substring(var_name_loc,str_tmp,is_ampl,is,ie,debug)
          str_tmp = 'phas'
          call util_find_substring(var_name_loc,str_tmp,is_phas,is,ie,debug)
          !          
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.temp_id) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! temperature
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 't'
          if(trim(var_name_loc).eq.str_tmp) then
             !
             switch = .true.
             !
          else
             !
             switch = .false.
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'temp'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'n1v4'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if          
          !
          if(.not.switch) then
             !
             str_tmp = 'temperature'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          str_tmp = 'ampl'
          call util_find_substring(var_name_loc,str_tmp,is_ampl,is,ie,debug)
          str_tmp = 'phas'
          call util_find_substring(var_name_loc,str_tmp,is_phas,is,ie,debug)
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.mach_id) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! mach number
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'm'
          if(trim(var_name_loc).eq.str_tmp) then
             !
             switch = .true.
             !
          else
             !
             switch = .false.
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'mach'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'mach-number'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          str_tmp = 'ampl'
          call util_find_substring(var_name_loc,str_tmp,is_ampl,is,ie,debug)
          str_tmp = 'phas'
          call util_find_substring(var_name_loc,str_tmp,is_phas,is,ie,debug)
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.xmomID) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! x-momentum mass flux
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'rhou'
          if(trim(var_name_loc).eq.str_tmp) then
             !
             switch = .true.
             !
          else
             !
             switch = .false.
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'xmom'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'x-mom'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          str_tmp = 'ampl'
          call util_find_substring(var_name_loc,str_tmp,is_ampl,is,ie,debug)
          str_tmp = 'phas'
          call util_find_substring(var_name_loc,str_tmp,is_phas,is,ie,debug)
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.etot_id) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! total energy
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'etot'
          if(trim(var_name_loc).eq.str_tmp) then
             !
             switch = .true.
             !
          else
             !
             switch = .false.
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'tot_ener'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          str_tmp = 'ampl'
          call util_find_substring(var_name_loc,str_tmp,is_ampl,is,ie,debug)
          str_tmp = 'phas'
          call util_find_substring(var_name_loc,str_tmp,is_phas,is,ie,debug)
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.x_id) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! x-coordinate
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'x'
          if(trim(var_name_loc).eq.str_tmp) then
             !
             switch = .true.
             !
          else
             !
             switch = .false.
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'x-grid'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.yID) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! y-coordinate
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'y'
          if(trim(var_name_loc).eq.str_tmp) then
             !
             switch = .true.
             !
          else
             !
             switch = .false.
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'y-grid'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.zID) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! z-coordinate
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'z'
          if(trim(var_name_loc).eq.str_tmp) then
             !
             switch = .true.
             !
          else
             !
             switch = .false.
             !
          end if
          !
          if(.not.switch) then
             !
             str_tmp = 'z-grid'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.dudyID) then
          !
          if(&
               &trim(var_name_loc).eq.'dudh'.or.&
               &trim(var_name_loc).eq.'uderiv'.or.&
               &trim(var_name_loc).eq.'u-deriv'.or.&
               &trim(var_name_loc).eq.'dudy'&
               &) then
             !
             var_id = ivar
             !
             exit
             !
          end if          
          !
       elseif(variable.eq.d2udy2ID) then
          !
          if(&
               &trim(var_name_loc).eq.'d2udh'.or.&
               &trim(var_name_loc).eq.'d2udh2'.or.&
               &trim(var_name_loc).eq.'d2udy2'&
               &) then
             !
             var_id = ivar
             !
             exit
             !
          end if          
          !
       elseif(variable.eq.dTdyID) then
          !
          if(&
               &trim(var_name_loc).eq.'dtdh'.or.&
               &trim(var_name_loc).eq.'tderiv'.or.&
               &trim(var_name_loc).eq.'t-deriv'.or.&
               &trim(var_name_loc).eq.'dtdy'&
               &) then
             !
             var_id = ivar
             !
             exit
             !
          end if                    
          !
       elseif(variable.eq.d2Tdy2ID) then
          !
          if(&
               &trim(var_name_loc).eq.'d2tdh'.or.&
               &trim(var_name_loc).eq.'d2tdh2'.or.&
               &trim(var_name_loc).eq.'d2tdy2'&
               &) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.omgx_id) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! omega x
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          str_tmp = 'omgx'
          call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
          !
          if(.not.switch) then
             !
             str_tmp = 'omega_x'
             call util_find_substring(var_name_loc,str_tmp,switch,is,ie,debug)
             !
          end if
          !
          str_tmp = 'ampl'
          call util_find_substring(var_name_loc,str_tmp,is_ampl,is,ie,debug)
          str_tmp = 'phas'
          call util_find_substring(var_name_loc,str_tmp,is_phas,is,ie,debug)
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.fvID) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! scaled v-velocity from similarity solver
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !          
          if(trim(var_name_loc).eq.'fv') then
             !
             switch = .true.
             !
          else
             !
             switch = .false.
             !
          end if
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       elseif(variable.eq.cp_id) then
          !
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          ! specific heat at constant pressure
          !++++++++++++++++++++++++++++++++++++++++++++++++++
          !
          if(trim(var_name_loc).eq.'cp') then
             !
             switch = .true.
             !
          else
             !
             switch = .false.
             !
          end if
          !
          if(switch) then
             !
             var_id = ivar
             !
             exit
             !
          end if
          !
       else
          !
          write(message,'(A,I6,A)')'WARNING: Variable identifier ',variable,' not known (subroutine find_var_id::utilities.F90)'
          call print_message(message,RED)
          write(message,'(a)')'WARNING: This might result in postprocessing error'
          call print_message(message,RED)
          !
       end if
       !
    end do
    !
    if(debug) then
       !
       if(variable.eq.pres_id) then
          write(*,'(A,I4)')'pressure is variable    :',var_id
       elseif(variable.eq.uvel_id) then
          write(*,'(A,I4)')'u-velocity is variable  :',var_id
       elseif(variable.eq.vvel_id) then
          write(*,'(A,I4)')'v-velocity is variable  :',var_id
       elseif(variable.eq.wvel_id) then
          write(*,'(A,I4)')'w-velocity is variable  :',var_id
       elseif(variable.eq.dens_id) then
          write(*,'(A,I4)')'density is variable     :',var_id
       elseif(variable.eq.temp_id) then
          write(*,'(A,I4)')'temperature is variable :',var_id
       elseif(variable.eq.mach_id) then
          write(*,'(A,I4)')'Mach number is variable :',var_id
       end if
    end if
    !
  end subroutine find_var_id
  !----------------------------------------------------------------------------------------------------
  ! translate variable name to variable id
  !----------------------------------------------------------------------------------------------------
  subroutine util_var_name_2_var_id(var_name,var_id,debug)
    !
    implicit none
    !
    ! input/output variables
    character(*), intent(in) :: var_name
    integer, intent(out)     :: var_id
    logical, intent(in)      :: debug
    ! local variables
    character*128            :: var_name_loc
    logical                  :: var_found
    integer                  :: is,ie
    !
    ! initialize var_id
    var_id = -99
    !
    ! save input variable var_name in var_name_loc to modify without changing input variable
    var_name_loc = trim(var_name)
    !
    ! convert all uppercase letters to lowercase letters
    call low_cap(var_name_loc,cap2low)
    !
    ! delete leading spaces
    call delete_leading_spaces(var_name_loc)
    !
    ! check if u-velocity
    call util_find_substring(var_name_loc,'uvel',var_found,is,ie,.false.)
    !
    if(var_found) then
       !
       var_id = uvel_id
       return
       !
    end if
    !
    ! check if v-velocity
    call util_find_substring(var_name_loc,'vvel',var_found,is,ie,.false.)
    !
    if(var_found) then
       !
       var_id = vvel_id
       return
       !
    end if
    !
    ! check if w-velocity
    call util_find_substring(var_name_loc,'wvel',var_found,is,ie,.false.)
    !
    if(var_found) then
       !
       var_id = wvel_id
       return
       !
    end if
    !
    ! check if temperature
    call util_find_substring(var_name_loc,'temp',var_found,is,ie,.false.)
    !
    if(var_found) then
       !
       var_id = temp_id
       return
       !
    end if
    !
    ! check if pressure
    call util_find_substring(var_name_loc,'pres',var_found,is,ie,.false.)
    !
    if(var_found) then
       !
       var_id = pres_id
       return
       !
    end if
    !
    ! check if density
    call util_find_substring(var_name_loc,'dens',var_found,is,ie,.false.)
    !
    if(var_found) then
       !
       var_id = dens_id
       return
       !
    end if    
    !
  end subroutine util_var_name_2_var_id
  !----------------------------------------------------------------------------------------------------
  ! get file extension
  !----------------------------------------------------------------------------------------------------
  subroutine util_get_fext(fname_in,fname_ext)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in)   :: fname_in
    ! output variables
    character(*), intent(out)  :: fname_ext
    ! local variables
    integer                    :: i_ext_start
    integer                    :: i_ext_end
    logical                    :: has_extension
    character*512              :: message
    logical                    :: debug
    !
    debug = .false.
    !
    has_extension = .false.
    fname_ext = 'na'
    !
    i_ext_end = len(trim(fname_in))
    !
    if(debug) then
       !
       write(*,'(a,a)')'Input file name: ',trim(fname_in)
       write(*,'(a,i4)')'length of string: ',i_ext_end
       !
    end if
    !
    do i_ext_start = i_ext_end,1,-1
       !
       if(fname_in(i_ext_start:i_ext_start).eq.'.') then
          !
          fname_ext = fname_in(i_ext_start+1:i_ext_end)
          has_extension = .true.
          !
          exit
          !
       end if
    end do
    !
    if(has_extension) then
       !
       write(*,*)
       write(*,'(a,a,a)')'file type : ',trim(fname_ext),' file'
       !
    else
       !
       write(*,*)
       write(*,'(a)')trim(hrule_war)
       write(*,'(a)')'warning: no file extension found'
       write(*,'(a)')trim(hrule_war)
       write(*,*)
       !
    end if
    !
  end subroutine util_get_fext
  !----------------------------------------------------------------------------------------------------
  ! get file base name (everything before the .)
  !----------------------------------------------------------------------------------------------------
  subroutine util_get_fbase(fname,fbase)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in)  :: fname
    ! output variables
    character(*), intent(out) :: fbase
    ! local variables
    integer                   :: idot
    !
    idot = scan(trim(fname),'.')
    !
    if(idot.lt.1) then
       !
       fbase = trim(fname)
       !
    else
       !
       fbase = fname(1:idot-1)
       !
    end if
    !
  end subroutine util_get_fbase
  !----------------------------------------------------------------------------------------------------
  ! convert real number to string
  !----------------------------------------------------------------------------------------------------
  subroutine real2string(num,String,digits)
    !
    implicit none
    !
    ! input variables
    real(kind=dp), intent(in)         :: num
    integer, intent(in)        :: digits
    ! output variables
    character*128, intent(out) :: String
    ! local varaibles
    integer                    :: iStart
    integer                    :: iEnd
    integer                    :: ichar
    character*128              :: fmt
    !
    ! assign format
    if(digits+3.lt.10) then
       !
       if(digits.lt.10) then
          !
          write(fmt,'(a,i1,a,i1,a)')'(f',digits+3,'.',digits,')'
          !
       else
          !
          write(fmt,'(a,i1,a,i2,a)')'(f',digits+3,'.',digits,')'
          !
       end if
       !
    elseif(digits+3.lt.100) then
       !
       if(digits.lt.10) then
          !
          write(fmt,'(a,i2,a,i1,a)')'(f',digits+3,'.',digits,')'
          !
       else
          !
          write(fmt,'(a,i2,a,i2,a)')'(f',digits+3,'.',digits,')'
          !
       end if
       !
    end if
    !
    write(String,fmt)num
    !
    ! last index of string
    iEnd = len(trim(String))
    !
    ! delete leading spaces
    do ichar=1,128
       !
       if(String(ichar:ichar).eq.' ') then
          !
       else
          !
          iStart = ichar
          !
          exit
          !
       end if
       !
    end do
    !
    String = String(iStart:iEnd)
    !
  end subroutine real2string
  !----------------------------------------------------------------------------------------------------
  ! interpet an ansewr and set a logical switch to true or false
  !----------------------------------------------------------------------------------------------------
  subroutine util_interpret_answer(answer,switch)
    !
    implicit none
    !
    character*128, intent(inout) :: answer
    logical, intent(out)         :: switch
    character*512                :: message
    !
    ! initialize switch with false
    switch = .false.
    !
    ! convert answer to all lowercase letters
    call low_cap(answer,cap2low)
    !
    if(trim(answer).eq.'y'.or.trim(answer).eq.'Y'.or.trim(answer).eq.'yes'.or.trim(answer).eq.'Yes'.or.trim(answer).eq.'YES') then
       !
       ! answer is yes => set switch to true
       switch = .true.
       !
    elseif(trim(answer).eq.'n'.or.trim(answer).eq.'no') then
       !
       switch = .false.
       !
    else
       !
       write(message,'(A,A,A)')'Answer : ',trim(answer),' not known => cannot be used to properly set switch'
       call print_message(message,RED)
       stop
       !
    end if
    !
  end subroutine util_interpret_answer
  !----------------------------------------------------------------------------------------------------
  ! swap bytes
  !----------------------------------------------------------------------------------------------------
  SUBROUTINE swap_f8(FLOAT8)
    !
    IMPLICIT NONE
    !
    REAL(KIND=8), INTENT(IN OUT) :: FLOAT8
    !
    INTEGER(KIND=1), DIMENSION(8) :: BYTE_ARR, BYTE_ARR_TMP
    INTEGER :: I
    !
    BYTE_ARR = TRANSFER (FLOAT8, BYTE_ARR)
    !
    BYTE_ARR_TMP = BYTE_ARR
    !
    DO I = 1, 8
       BYTE_ARR(I) = BYTE_ARR_TMP(9-I)
    END DO
    !
    FLOAT8 = TRANSFER (BYTE_ARR, FLOAT8)
    !
    RETURN
    !
  END SUBROUTINE swap_f8
  !----------------------------------------------------------------------------------------------------
  ! swap bytes different option
  !----------------------------------------------------------------------------------------------------
  subroutine swapios(var,id,iword)
    !
    implicit none
    !
    integer          :: id,iword,i,ii,iw
    character(len=1) :: vars(8)
    character(len=1) :: var(1:id*iword)
    !
    do i=1,id
       !
       ii=1+(i-1)*iword
       !
       do iw=1,iword
          vars(iw)=var(ii+iword-iw)
       enddo
       !
       do iw=1,iword
          var(ii+iw-1)=vars(iw)
       enddo
       !
    enddo
    !
  end subroutine swapios
  !----------------------------------------------------------------------------------------------------
  ! print ios file info
  !----------------------------------------------------------------------------------------------------
  subroutine print_ios_info(fname,nx,ny,nz,nt,np,inf)
    !
    implicit none
    !
    character*128, intent(in)         :: fname
    integer, intent(in)               :: nx,ny,nz,nt,np
    character(len=72), dimension(128) :: inf
    integer                           :: i
    !
    write(*,'(A,A)')'File Info: ',trim(fname)
    write(*,'(A,I8)')'nx = ',nx
    write(*,'(A,I8)')'ny = ',ny
    write(*,'(A,I8)')'nz = ',nz
    write(*,'(A,I8)')'nt = ',nt
    write(*,'(A,I8)')'np = ',np
    !
    do i=1,np
       !
       write(*,'(A,I2,A,A)')'Variable(',i,') = ',trim(inf(i))
       !
    end do
    !
  end subroutine print_ios_info
  !----------------------------------------------------------------------------------------------------
  ! check if byteswapping is needed
  !----------------------------------------------------------------------------------------------------
  subroutine check_byteswap(fname,byteswap)
    !
    use ios
    !
    implicit none
    ! 
    ! input variables
    character(*), intent(in)              :: fname
    ! output variables 
    logical, intent(out)                   :: byteswap
    ! local variables
    character(len=124)                     :: fbase
    integer(kind=4)                        :: imach
    integer(kind=4), parameter             :: funit = 24
    integer(kind=4), parameter             :: ftape = 24
    integer(kind=4), dimension(nt_ios_max) :: itimes
    character(len=72), dimension(128)      :: inf
    real(kind=dp),dimension(:,:), allocatable     :: dummy_real
    character*128                          :: dummy_char
    integer(kind=4)                        :: ierr
    integer                                :: n_swapped
    integer                                :: i,j,k
    character*512                          :: message
    integer(kind=4)                        :: itloc,kloc,ivar
    integer(kind=4)                        :: mt,mz,my,mx,mp,minf
    integer                                :: exp_val
    integer                                :: its,ite
    character*128                          :: exp_char
    logical                                :: debug
    integer                                :: is,ie
    logical                                :: is_positive
    integer                                :: exponent_value
    integer                                :: ndig
    character*128                          :: str_fmt
    !
    debug = .false.
    !
    write(*,'(a,a)')'Check if file is byteswapped: ',trim(fname)
    !
    ! initialize flag
    byteswap = .false.
    !
    ! open input file
    call mkfname(fname,fbase,imach,ierr)
    !
    ! open for 1D line input
    call readcd2d(ftape,funit,fbase,imach,mt,mz,my,mx,mp,itimes,inf,minf,ierr)
    !
    its = 1
    !
    if(mt.gt.1) then
       ite = 2
    else
       ite = 1
    end if
    !
    ! set irregular vaule count to zero
    n_swapped = 0
    !
    ! set k and j indices of input line
    kloc = 1
    !
    allocate(dummy_real(mx,my))
    !
    do itloc = its,ite
       !
       ! loop over variables
       do ivar = 1,1 !mp
          !
          ! read first line in input file
          call readd2d(funit, dummy_real(1:mx,1:my),itloc,ivar,kloc,ierr)
          !
          ! loop over j
          do j = 1,my/10
             ! loop over i
             do i = 1,mx/10
                !
                if(dummy_real(i,j).eq.0.d0) then
                   !
                   ! do nothing
                   !
                else
                   !
                   ! get exponent
                   write(exp_char,'(e32.10)')abs(dummy_real(i,j))
                   !
                   if(adjustl(exp_char).eq.'NaN') then
                      !
                      n_swapped=n_swapped+1
                      !
                   else
                      !
                      is = scan(trim(exp_char),'-')+1
                      is_positive=.false.
                      !
                      if(is.eq.1) then
                         !
                         is = scan(trim(exp_char),'+')+1
                         is_positive=.true.
                         !
                      end if
                      !
                      if(is_positive) then
                         !
                         ie = len(trim(exp_char))
                         !
                         ndig = len(trim(exp_char(is:ie)))
                         !
                         if(ndig.lt.10) then
                            write(str_fmt,'(a,i1,a)')'(i',ndig,')'
                         elseif(ndig.lt.100) then
                            write(str_fmt,'(a,i2,a)')'(i',ndig,')'
                         elseif(ndig.lt.1000) then
                            write(str_fmt,'(a,i3,a)')'(i',ndig,')'
                         end if
                         !
                         read(exp_char(is:ie),str_fmt)exponent_value
                         !
                         if(exponent_value.gt.200) then
                            !                       
                            n_swapped = n_swapped+1
                            !
                            ! if a swapped value was found with two rigorous checks then exit
                            exit
                            !
                         end if
                         !
                      end if
                      !
                   end if
                   !
                end if
                !
                ! end i-loop
             end do
             !
             ! end j-loop
          end do
          !
          ! ! check if any irregular values are found in 2d slice
          ! ! 
          ! ! loop over j-index
          ! do j = 1,my
          !    !
          !    ! loop over x-index
          !    do i=1,mx
          !       !
          !       exp_val = exponent(dummy_real(i,j))
          !       !
          !       if(abs(exp_val).gt.200) then
          !          !
          !          n_swapped = n_swapped + 1
          !          !
          !       end if
          !       !
          !       ! end i-loop
          !    end do
          !    !
          !    ! end j-loop
          ! end do
          !
          ! end variable loop
       end do
       !
       ! end timestep loop
    end do
    !
    ! close file
    close(funit)
    !
    write(message,'(A,I10,A,I10)')'number of values that appear to be swapped: ',n_swapped,'/',mx*my*mp
    !
    if(n_swapped.gt.0) then
       !
       call print_message(message,RED)
       write(message,'(a)')'=> Byteswapping needed'
       call print_message(message,RED)
       byteswap = .true.
       !
    else
       !
       call print_message(message,GREEN)
       write(message,'(a)')'=> No Byteswapping needed'
       call print_message(message,GREEN)
       !
    end if
    !
  end subroutine check_byteswap
  !----------------------------------------------------------------------------------------------------
  ! get exponent of number
  !----------------------------------------------------------------------------------------------------
  subroutine get_exponent(numString,Exp)
    !
    implicit none
    !
    character*128, intent(in) :: numString
    integer, intent(out) :: Exp
    integer:: iExpS,iExpE
    character*20:: ExpString
    integer:: nchars
    integer :: i
    integer :: ndigits
    integer :: Dig1, Dig2, Dig3, Dig4
    !
    nchars = len(trim(numString))
    iExpE = nchars
    !
    do i=1,128
       !
       if(numString(i:i).eq.'E') then
          !
          iExpS = i+2
          !
       end if
       !
    end do
    !
    ExpString = numString(iExpS:iExpE)
    !
    ndigits = iExpE-iExpS+1
    !
    read(ExpString,'(I10)')Exp
    !
  end subroutine get_exponent
  !----------------------------------------------------------------------------------------------------
  ! convert integer to string
  !----------------------------------------------------------------------------------------------------
  subroutine num2str_integer(num,string_int,len)
    !
    implicit none
    !
    ! input variables
    integer, intent(in) :: num
    integer, intent(in) :: len
    ! output variables
    character*128       :: string_int
    integer             :: i
    integer             :: len_int
    character*10        :: fmt
    logical             :: is_negative
    !
    is_negative=.false.
    !
    if(num<0) then
       !
       is_negative = .true.
       !
    end if
    !
    if(abs(num).lt.10) then
       !
       len_int = 1
       !
    elseif(abs(num).lt.100) then
       !
       len_int = 2
       !
    elseif(abs(num).lt.1000) then
       !
       len_int = 3
       !
    elseif(abs(num).lt.10000) then
       !
       len_int = 4
       !
    elseif(abs(num).lt.100000) then
       !
       len_int = 5
       !
    elseif(abs(num).lt.1000000) then
       !
       len_int = 6
       !
    elseif(abs(num).lt.10000000) then
       !
       len_int = 7
       !
    elseif(abs(num).lt.100000000)then
       !
       len_int = 8
       !
    end if
    !
    ! initialize integer string
    if(is_negative) then
       !
       string_int = '-'
       !
    else
       !
       string_int = ''
       !
    end if
    !
    do i=1,len-len_int
       write(string_int,'(A,A)')trim(string_int),'0'
    end do
    !
    ! assign fmt string
    write(fmt,'(A,I1,A)')'(A,I',len_int,')'
    !
    write(string_int,fmt)trim(string_int),abs(num)
    !
  end subroutine num2str_integer
  !----------------------------------------------------------------------------------------------------
  ! convert integer to string (no leading zeros)
  !----------------------------------------------------------------------------------------------------
  subroutine util_num2str_int(num,string,debug)
    !
    implicit none
    !
    ! input variables
    integer, intent(in) :: num
    logical, intent(in) :: debug
    character(*), intent(out) :: string
    ! output variables
    character*128 :: fmt
    integer :: len_int
    !
    if(abs(num).lt.10) then
       !
       len_int = 1
       !
    elseif(abs(num).lt.100) then
       !
       len_int = 2
       !
    elseif(abs(num).lt.1000) then
       !
       len_int = 3
       !
    elseif(abs(num).lt.10000) then
       !
       len_int = 4
       !
    elseif(abs(num).lt.100000) then
       !
       len_int = 5
       !
    elseif(abs(num).lt.1000000) then
       !
       len_int = 6
       !
    elseif(abs(num).lt.10000000) then
       !
       len_int = 7
       !
    elseif(abs(num).lt.100000000)then
       !
       len_int = 8
       !
    end if
    !
    ! assign fmt string
    write(fmt,'(a,i1,a)')'(I',len_int,')'
    !
    write(string,fmt)abs(num)
    !
  end subroutine util_num2str_int
  !----------------------------------------------------------------------------------------------------
  ! convert real number to string
  !----------------------------------------------------------------------------------------------------
  subroutine num2str_real(num,string_real,DIG1)
    !
    implicit none
    !
    ! input variables
    real(kind=dp), intent(in)  :: num
    integer, intent(in) :: DIG1
    ! output variables
    character*128       :: string_real
    integer             :: i
    integer             :: lenBase
    integer             :: LenBeforeDec
    character*20        :: fmt1
    character*20        :: fmt2
    character*20        :: fmt3
    character*20        :: fmt4
    integer             :: DIG2
    !
    DIG2 = DIG1+6
    !
    if(DIG1.lt.10) then
       write(fmt1,'(a)')'I1'
    elseif(DIG1.lt.100) then
       write(fmt1,'(a)')'I2'
    elseif(DIG1.lt.1000) then
       write(fmt1,'(a)')'I3'
    end if
    !
    if(DIG2.lt.10) then
       write(fmt2,'(a)')'I1'
    elseif(DIG2.lt.100) then
       write(fmt2,'(a)')'I2'
    elseif(DIG2.lt.1000) then
       write(fmt2,'(a)')'I3'
    end if
    !
    write(fmt3,'(A,A,A,A,A)')'(A,',trim(fmt2),',A,',trim(fmt1),',A)'
    !
    ! assign fmt string
    write(fmt4,fmt3)'(E',DIG2,'.',DIG1,')'
    !
    write(string_real,fmt4)num
    !
  end subroutine num2str_real
  !----------------------------------------------------------------------------------------------------
  ! convert real number to string 
  !----------------------------------------------------------------------------------------------------
  subroutine num2str(num,str,dig2)
    !
    implicit none
    !
    ! input variables
    real(kind=dp), intent(in)        :: num
    character(*), intent(out) :: str
    integer, intent(in)       :: dig2
    !
    ! output variables
    real(kind=dp)                    :: denominator
    real(kind=dp)                    :: frac
    integer                   :: dig1
    integer                   :: dig_tot
    character*20              :: fmt1, fmt2, fmt3, fmt4
    !
    ! check how big the number is 
    denominator = 1
    !
    ! determine the number of digits required for conversion before the decimal point
    dig1 = 0
    !
    frac = num/denominator
    !
    do while (frac>=1)
       !
       dig1 = dig1+1
       !
       denominator = denominator*10
       !
       frac = num/denominator
       !
    end do
    !
    ! use at least one leading zero
    if(dig1.eq.0) then
       !
       dig1 = 1
       !
    end if
    !
    dig_tot = dig2+dig1+1
    !
    if(dig2.lt.10) then
       write(fmt2,'(a)')'i1'
    elseif(dig2.lt.100) then
       write(fmt2,'(a)')'i2'
    elseif(dig2.lt.1000) then
       write(fmt2,'(a)')'i3'
    end if
    !
    if(dig_tot.lt.10) then
       write(fmt1,'(a)')'i1'
    elseif(dig_tot.lt.100) then
       write(fmt1,'(a)')'i2'
    elseif(dig_tot.lt.1000) then
       write(fmt1,'(a)')'i3'
    end if
    !
    write(fmt3,'(a,a,a,a,a)')'(a,',trim(fmt1),',a,',trim(fmt2),',a)'
    !
    write(fmt4,fmt3)'(f',dig_tot,'.',dig2,')'
    !
    write(str,fmt4)num
    !
  end subroutine num2str
  !----------------------------------------------------------------------------------------------------
  ! find closest point
  !----------------------------------------------------------------------------------------------------
  subroutine find_closest_point(nopts,f,fsearch,iclosest)
    !
    implicit none
    !
    ! input variables
    integer, intent(in)  :: nopts
    real(kind=dp), intent(in)   :: f(nopts)
    real(kind=dp), intent(in)   :: fsearch
    ! output variables
    integer, intent(out) :: iclosest
    ! local variables
    integer              :: i
    real(kind=dp)               :: tol
    real(kind=dp)               :: dflo, dfhi
    !
    ! check if point is outside of the domain
    if(fsearch.lt.f(1)) then
       !
       iclosest = 1
       !
    elseif(fsearch.gt.f(nopts)) then
       !
       iclosest = nopts
       !
    else
       !
       ! set tolerance for exact match
       tol = 1e-12
       !
       ! initialize index for closes point
       iclosest = -99
       !
       do i = 2,nopts
          !
          dflo = f(i-1)-fsearch
          dfhi = f(i)-fsearch
          !
          if(abs(dflo).le.tol) then
             !
             ! lower point is exact match
             iclosest = i-1
             exit
             !
          elseif(abs(dfhi).le.tol) then
             !
             ! upper point is exact match
             iclosest = i
             exit
             !
          elseif(dflo.lt.0.d0.and.dfhi.gt.0.d0) then
             !
             ! search point is bracketed by i-1 and i, check which point is closer
             if(abs(dflo).lt.abs(dfhi)) then
                !
                ! lower point i-1 is closer
                iclosest = i-1
                exit
                !
             else
                !
                ! point i is closer
                iclosest = i
                exit
                !
             end if
             !
          end if
          !
       end do
       !
    end if
    !
  end subroutine find_closest_point
  !----------------------------------------------------------------------------------------------------
  ! write a message on the screen choose a color if wanted
  !----------------------------------------------------------------------------------------------------
  subroutine print_message(message,color)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in) :: message
    integer, intent(in)      :: color
    ! local variables
    character*128            :: start_code
    character*128            :: end_code
    !
    ! generate start and end codes to generate correct colors
    if(color.lt.10) then
       write(start_code,'(A,A,I1,A)')achar(27),'[',color,'m'
    elseif(color.lt.100) then
       write(start_code,'(A,A,I2,A)')achar(27),'[',color,'m'       
    elseif(color.lt.1000) then
       write(start_code,'(A,A,I3,A)')achar(27),'[',color,'m'
    end if
    !
    end_code = achar(27)//'[0m'
    !
    ! print message on screen
    write(*,'(A,A,A)')trim(start_code),trim(message),trim(end_code)
    !
  end subroutine print_message
  !----------------------------------------------------------------------------------------------------
  ! Convert stagnation value to static value or static value to stagnation value
  ! ConversionVal = 1: Convert pressure
  ! ConversionVal = 2: Convert temperature
  ! ConversionDir = 1: Convert stagnation value to static value
  ! ConversionDir = 2: Convert static value to stagnation value
  !----------------------------------------------------------------------------------------------------
  subroutine stag_stat_conversion(M,gamma,ValIn,ValOut,ConversionVal,ConversionDir,DumpInfo)
    !
    implicit none
    !
    ! input variables
    real(kind=dp), intent(in)  :: M
    real(kind=dp), intent(in)  :: gamma
    real(kind=dp), intent(in)  :: ValIn
    integer, intent(in) :: ConversionVal
    integer, intent(in) :: ConversionDir
    logical, intent(in) :: DumpInfo
    ! output variables 
    real(kind=dp), intent(out) :: ValOut
    ! local variables
    real(kind=dp)              :: C1
    real(kind=dp)              :: ConversionFactor
    character*128       :: ValNameBase
    character*128       :: ValInName
    character*128       :: ValOutName
    character*128       :: DimName
    character*128       :: EndString
    character*512       :: message
    !
    if(ConversionVal.eq.pres_id) then
       !
       ValNameBase = 'pressure'
       EndString = '   :'
       DimName = 'Pa'
       !
       C1 = (1.d0+0.5*(gamma-1.d0)*M*M)**(gamma/(gamma-1.d0))
       !
    elseif(ConversionVal.eq.temp_id) then
       !
       ValNameBase = 'temperature'
       EndString = ':'
       DimName = 'K'
       !
       C1 = (1.d0+0.5*(gamma-1.d0)*M*M)
       !
    else
       !
       write(message,'(a)')'Error in stag_stat_conversion::utilities.F90'
       call print_message(message,RED)
       write(message,'(A,I6,A)')'Conversion value identifier ',ConversionVal,' not supported'
       call print_message(message,RED)
       stop
       !
    end if
    !
    if(ConversionDir.eq.stag2stat) then
       !
       write(ValInName,'(A,A,A)')'stagnation ',trim(ValNameBase),trim(EndString)
       write(ValOutName,'(A,A,4x,A)')'static ',trim(ValNameBase),trim(EndString)
       !
       ConversionFactor = 1.d0/C1
       !
    elseif(ConversionDir.eq.stat2stag) then
       !
       write(ValInName,'(A,A,4x,A)')'static ',trim(ValNameBase),trim(EndString)
       write(ValOutName,'(A,A,A)')'stagnation ',trim(ValNameBase),trim(EndString)
       !
       ConversionFactor = C1
       !
    end if
    !
    ValOut = ConversionFactor * ValIn
    !
    if(DumpInfo) then
       ! write user info
       write(*,'(A,1x,F16.8,1x,A)')trim(ValInName),ValIn,trim(DimName)
       write(*,'(A,1x,F16.8,1x,A)')trim(ValOutName),ValOut,trim(DimName)
    end if
    !
  end subroutine stag_stat_conversion
  !----------------------------------------------------------------------------------------------------
  ! try and read tecplto variable names in file header
  !----------------------------------------------------------------------------------------------------
  subroutine GetTecplotvar_names(fname,nheader,nvars,var_names,FileFMT)
    !
    implicit none
    !
    ! input variables
    character*128, intent(in)  :: fname
    integer, intent(in)        :: nheader
    integer, intent(in)        :: nvars
    ! output variables
    character*128, intent(out) :: var_names(1:nvars)
    integer, intent(out)       :: FileFMT
    ! local variables
    character*512              :: dummy
    character*512              :: message
    integer                    :: i
    integer                    :: ivar
    integer                    :: is, ie
    character*9                :: substring
    logical                    :: Foundvar_names
    !
    ! initialize varnames
    do ivar = 1,nvars
       !
       var_names(ivar) = 'NA'
       !
    end do
    !
    ! open input file and search for Variables in header
    open(unit=2208,file=trim(fname),status='unknown')
    !
    Foundvar_names = .false.
    !
    ! read header lines
    do i=1,nheader
       !
       read(2208,'(a)')dummy
       !
       substring=dummy(1:9)
       !
       call low_cap(substring,cap2low)
       !
       if(trim(substring).eq.'variables') then
          !
          ! variable description detected -> get names
          !
          Foundvar_names = .true.
          !
          exit
          !
       end if
       !
    end do
    !
    ! if variable names were detected then analyze string and find names
    if(Foundvar_names) then
       !
       ! print message for users
       message = 'Variable names detected in input file => Input file is tecplot ASCII file'
       call print_message(message,GREEN)
       !
       ! set FileFMT to TECFMT (defined in params.F90)
       FileFMT = TECFMT
       !
       ! parse string for variable names
       is = 10
       !
       do ivar = 1,nvars
          !
          do i=is,512
             !
             if(dummy(i:i).eq.'"') then
                is = i
                exit
             end if
             !
          end do
          !
          do i=is+1,512
             if(dummy(i:i).eq.'"') then
                !
                ie = i
                exit
                !
             end if
          end do
          !
          var_names(ivar) = dummy(is+1:ie-1)
          !
          ! reset start for variable name search
          is = ie+1
          !
       end do
       !
       ! write info on screen
       do ivar = 1,nvars
          !
          write(*,'(A,I4,A,A)')'Variable Name (',ivar,') = ',trim(var_names(ivar))
          !
       end do
       !
    else
       !
       ! no variable names detected
       message = 'No variable names detected in input file => Input File is generic data file (ASCII)'
       call print_message(message,RED)
       !
       FileFMT = ASCIIFMT
       !
    end if
    !
    ! close input file
    close(2208)
    !
  end subroutine GetTecplotvar_names
  !----------------------------------------------------------------------------------------------------
  ! Create directory
  !----------------------------------------------------------------------------------------------------
  subroutine create_dir(dir_name)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in) :: dir_name
    ! local variables
    character*512            :: message
    logical                  :: dir_exists
    character*128            :: cmd
    !
    ! check if directory exsts
    inquire(file=trim(dir_name),EXIST=dir_exists)
    !
    if(dir_exists) then
       ! nothing to be done
    else
       !
       write(cmd,'(A,A)')'mkdir -p ',trim(dir_name)
       call system(trim(cmd))
       !
    end if
    !
  end subroutine create_dir
  !----------------------------------------------------------------------------------------------------
  ! check if dimensions are consistent
  !----------------------------------------------------------------------------------------------------
  subroutine check_dim_consistency(noptsFlow,noptsGrid,dir,DimConsistent)
    !
    implicit none
    !
    ! input variables
    integer, intent(in)  :: noptsFlow
    integer, intent(in)  :: noptsGrid
    integer, intent(in)  :: dir
    logical, intent(out) :: DimConsistent 
    ! local variables
    character*128        :: DimName
    character*512        :: message
    !
    ! initialize logical variable to check dimension consistentcy with false
    DimConsistent = .false.
    !
    ! assign dimension name
    if(dir.eq.xdir) then
       DimName = 'x'
    elseif(dir.eq.ydir) then
       DimName = 'y'
    elseif(dir.eq.zdir) then
       DimName = 'z'
    end if
    !
    ! compare point counts of flow and grid
    if(noptsFlow.eq.noptsGrid) then
       !
       DimConsistent=.true.
       !
    end if
    !
    if(DimConsistent) then
       !
       write(message,'(A,A,A,I10)')'=> Dimensions in ',trim(DimName),'-direction are consistent, noptsGrid = noptsFlow = ',noptsFlow
       call print_message(message,GREEN)
       !
    else
       !
       write(message,'(A,A,A,I10,A,I10)')'=> Dimensions in ',trim(DimName),'-direction are inconsistent, noptsGrid = ',noptsGrid,', noptsFlow = ',noptsFlow
       call print_message(message,RED)
       !
    end if
    !
  end subroutine check_dim_consistency
  !----------------------------------------------------------------------------------------------------
  ! replace decimal point with pt
  !----------------------------------------------------------------------------------------------------
  subroutine dec2pt(string)
    !
    implicit none
    ! 
    ! input/output variables
    character*128, intent(inout) :: string
    ! local variables
    integer                      :: idec
    integer                      :: ichar
    !
    do ichar = 1,128
       !
       if(string(ichar:ichar).eq.'.') then
          !
          idec = ichar
          exit
          !
       end if
       !
    end do
    !
    ! shift all characters one to the right after the decimal point
    do ichar = 127,idec+1,-1
       !
       string(ichar+1:ichar+1) = string(ichar:ichar)
       !
    end do
    !
    ! replace decimal point with pt
    string(idec:idec+1)='pt'
    !
  end subroutine dec2pt
  !----------------------------------------------------------------------------------------------------
  ! switch lower case to upper case or vice versa
  !----------------------------------------------------------------------------------------------------
  subroutine low_cap(string,option)
    !
    ! input/output variables
    character(*), intent(inout) :: string
    integer, intent(in)         :: option
    ! local variables
    integer                     :: ic
    integer                     :: i
    character(26), parameter    :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(26), parameter    :: low = 'abcdefghijklmnopqrstuvwxyz'
    character(len(string))      :: dummy
    !
    ! set work string
    dummy = string
    !
    do i = 1,LEN_TRIM(string)
       !
       if(option.eq.low2cap) then
          !
          ic = INDEX(low,string(i:i))
          if(ic > 0) dummy(i:i) = cap(ic:ic)
          !
       elseif(option.eq.cap2low) then
          !
          ic = INDEX(cap,string(i:i))
          if(ic > 0) dummy(i:i) = low(ic:ic)
          !
       end if
       !
    end do
    !
    string = trim(dummy)
    !
  end subroutine low_cap
  !----------------------------------------------------------------------------------------------------
  ! compute local speed of sound profile
  !----------------------------------------------------------------------------------------------------
  subroutine calc_sos_profile(nopts,temp,M,sos)
    !
    implicit none
    !
    ! input/output variables
    integer, intent(in) :: nopts
    real(kind=dp), intent(in)  :: temp(1:nopts)
    real(kind=dp), intent(in)  :: M
    real(kind=dp), intent(out) :: sos(1:nopts)
    ! local variables
    integer             :: i
    !
    ! Note: the speed of sound is computed in non-dimensional variables
    ! 
    ! dimensional speed of sound:
    ! c_dim = sqrt(gamma*R_dim*T_dim)
    !
    ! nondimensional speed of sound:
    ! c = c_dim/U_inf
    ! nondimensional temperature (as written by the code):
    ! T = T_dim/T_inf
    !
    ! freestream speed of sound:
    ! c_inf = sqrt(gamma*R_dim*T_inf)
    !
    ! Mach number:
    ! M = U_inf/c_inf
    !
    ! => c = c_dim/U_inf = sqrt(gamma*R_dim*T*T_inf)/U_inf
    !    c = sqrt(T) * sqrt(gamma*R_dim*T_inf)/U_inf
    !    c = sqrt(T) * c_inf/U_inf
    !    c = sqrt(T)/M
    !
    do i=1,nopts
       !
       sos(i) = sqrt(temp(i))/M
       !
    end do
    ! 
  end subroutine calc_sos_profile
  !----------------------------------------------------------------------------------------------------
  ! get input file name
  !----------------------------------------------------------------------------------------------------
  subroutine util_get_input_file(fname_in)
    !
    implicit none
    !
    ! output variables
    character(*), intent(out) :: fname_in
    ! local variables
    logical                   :: file_exists
    character*512             :: message
    character*128             :: string_tmp
    integer                   :: i,is,ie,nchar
    logical                   :: debug
    !
    debug = .false.
    !
    ! set file_exists flag to false
    file_exists = .false.
    !
    ! enter do while loop to get a file that exists or let user abort
    do while (.not. file_exists)
       !
       ! read input file string
       read(*,'(a)')string_tmp
       !
       ! check if there are empty spaces in the input file name -> most likely explanations => cut before first space
       nchar = len(string_tmp)
       !
       ! convert all tab characters to spaces
       do i = 1,nchar
          !
          if(ichar(string_tmp(i:i)).eq.9) then
             !
             string_tmp(i:i)=' '
             !
          end if
          !
       end do
       !
       if(debug) then
          !
          write(*,'(a)')''
          write(*,'(a)')'Input string: ',trim(string_tmp)
          write(*,'(a,i4)')'Input characters: ',nchar
          !
       end if
       !
       is = 1
       !
       ! remove leading spaces
       do i = 1,nchar
          !
          if(string_tmp(i:i).eq.' ') then
             !
             is = i
             !
          else
             !
             exit
             !
          end if
          !
       end do
       !
       if(debug) then
          !
          write(*,'(a,i4)')'start index is = ',is
          !
       end if
       !
       ! remove leading spaces
       ie = nchar
       !
       do i = is,nchar
          !
          if(string_tmp(i:i).eq.' ') then
             !
             ie = i-1
             exit
             !
          end if
          !
       end do
       !
       if(debug) then
          !
          write(*,'(a,i4)')'end index ie = ',ie
          !
       end if
       !
       fname_in = trim(string_tmp(is:ie))
       !
       call low_cap(string_tmp,cap2low)
       !
       if(trim(string_tmp).eq.'exit') then
          !
          write(*,'(a)')
          write(*,'(a)')'abort program'
          stop
          !
       end if
       !
       ! check if File exists
       call check_file_status(fname_in,file_exists)
       !
       if(file_exists) then
          !
          write(*,*)
          write(*,'(a,a,a)')'file ',trim(fname_in),' exists'
          !
       else
          !
          write(*,'(a)')''
          write(*,'(a)')trim(hrule_err)
          write(*,'(a,a,a)')'error: file ',trim(fname_in),' does not exist'
          write(*,'(a)')'error: supply valid input file or type exit to abort'
          write(*,'(a)')trim(hrule_err)
          write(*,*)
          !
       end if
       !
    end do
    !
  end subroutine util_get_input_file
  !----------------------------------------------------------------------------------------------------
  ! get date and time string
  !----------------------------------------------------------------------------------------------------
  subroutine get_date_time(DateTimeString)
    !
    implicit none
    !
    ! output variables
    character*128, intent(out) :: DateTimeString
    ! local variables
    integer                    :: date_time(8)
    character*10               :: b(3)
    character*128              :: MonthString,DayString,YearString
    character*128              :: HourString,MinuteString,SecondString
    !
    call date_and_time(b(1),b(2),b(3),date_time)
    !
    ! convert year to string
    call num2str_integer(date_time(1),YearString,4)
    ! convert month to string
    call num2str_integer(date_time(2),MonthString,2)
    ! convert Day to string
    call num2str_integer(date_time(3),DayString,2)
    !
    ! convert year to string
    call num2str_integer(date_time(5),HourString,2)
    ! convert month to string
    call num2str_integer(date_time(6),MinuteString,2)
    ! convert Day to string
    call num2str_integer(date_time(7),SecondString,2)
    !
    ! assemble DateTimeString
    write(DateTimeString,'(A,A,A,A,A,A,A,A,A,A,A,A)')'Date (MM/DD/YYYY): ',&
                                                    &trim(MonthString),'/',&
                                                    &trim(DayString),'/',&
                                                    &trim(YearString),&
                                                    &'; Time (HH:MM:SS): ',&
                                                    &trim(HourString),':',&
                                                    &trim(MinuteString),':',&
                                                    &trim(SecondString)
    !
  end subroutine get_date_time
  !----------------------------------------------------------------------------------------------------
  ! get user name
  !----------------------------------------------------------------------------------------------------
  subroutine get_user_name(UserString)
    !
    implicit none
    !
    ! output variables
    character*128, intent(out) :: UserString
    ! local variables
    character(32)              :: login
    !
    call getenv("USER",UserString)
    !
  end subroutine get_user_name
  !----------------------------------------------------------------------------------------------------
  ! delete leading spaces in a character string
  !----------------------------------------------------------------------------------------------------
  subroutine delete_leading_spaces(String)
    !
    implicit none
    !
    character(*), intent(inout) :: String
    character*128               :: StringTMP
    integer                     :: is
    integer                     :: ichar
    integer                     :: nchar
    !
    ! save input string in temporary string
    StringTMP = trim(String)
    !
    nchar = len(trim(StringTMP))
    !
    is = 1
    !
    do ichar = 1,nchar
       !
       if(StringTMP(ichar:ichar).eq.' ') then
          is = ichar+1
       else
          exit
       end if
       !
    end do
    !
    String = StringTMP(is:nchar)
    !
  end subroutine delete_leading_spaces
  !----------------------------------------------------------------------------------------------------
  ! compute kfetch index based on number of points that are available in z and the actual k index
  !----------------------------------------------------------------------------------------------------
  subroutine get_kfetch(k,nz,kfetch,pm)
    !
    implicit none
    !
    ! input variables
    integer, intent(in)    :: k
    integer, intent(in)    :: nz
    ! output variables 
    integer, intent(out)   :: kfetch
    ! input, output variables
    integer, intent(inout) :: pm
    !
    if(k.gt.nz) then
       !
       ! k-index out of range
       if(mod(k-nz,nz-1).eq.0) then
          !
          pm = pm * (-1)
          !
          if(pm.eq.1) then
             kfetch = 0
          else
             kfetch = nz+1
          end if
          !
       end if
       !
       kfetch = kfetch + pm
       !
    else
       !
       ! k-index within actual range
       pm     = 1
       kfetch = k
       !
       if(k.eq.nz) then
          kfetch = nz
          pm     = -1
       end if
       !
    end if
    !
  end subroutine get_kfetch
  !----------------------------------------------------------------------------------------------------
  ! compute factor for wvelocity
  !----------------------------------------------------------------------------------------------------
  subroutine get_wvelpm(k,nz,wvelpm)
    !
    implicit none
    !
    ! input variables
    integer, intent(in)  :: k
    integer, intent(in)  :: nz
    ! output variables 
    integer, intent(out) :: wvelpm
    !
    if(k.gt.nz) then
       !
       wvelpm = (-1)**(k/nz)
       !
    else
       !
       wvelpm = 1
       !
    end if
    !
  end subroutine get_wvelpm
  !----------------------------------------------------------------------------------------------------
  ! compute factorial
  !----------------------------------------------------------------------------------------------------
  subroutine factorial(n,res)
    !
    integer, intent(in) :: n
    real(kind=dp), intent(out) :: res
    integer             :: i
    !
    res=1.d0
    !
    do i=2,n
       !
       res=res*i
       !
    end do
    !
  end subroutine factorial
  !----------------------------------------------------------------------------------------------------
  ! check ios info string length
  !----------------------------------------------------------------------------------------------------
  subroutine check_ios_info_string_length(InfoString)
    !
    implicit none
    !
    ! input/output variables
    character(*), intent(inout) :: InfoString
    ! local variables
    integer                     :: nchars
    character*512               :: message
    character(len=72)                :: TmpString
    integer                     :: ichar
    !
    nchars = len(trim(InfoString))
    !
    if(nchars.gt.72) then
       !
       ! info string too long
       write(message,'(A,A)')'WARNING: Info string ',trim(InfoString)
       call print_message(message,RED)
       write(message,'(A,I4,A,i4,a)')'WARNING: number of characters =  ',nchars,' > ',72,' allowed characters'
       call print_message(message,RED)
       write(message,'(a,i4,a)')'WARNING Truncate info string to ',72,' characters'
       call print_message(message,RED)
       !
       TmpString=trim(InfoString)
       InfoString=trim(TmpString)
       !
       write(message,'(A,A)')'Truncated string: ',trim(InfoString)
       call print_message(message,RED)
       !
    end if
    !
  end subroutine check_ios_info_string_length
  !----------------------------------------------------------------------------------------------------
  ! Get reference values for N-factor computation
  !----------------------------------------------------------------------------------------------------
  subroutine get_reference_values(dummy_char,iref,xref,nx,x)
    !
    ! input variables
    character*30, intent(in)          :: dummy_char
    integer, intent(in)               :: nx
    real(kind=dp), intent(in)                :: x(nx)
    ! output variables
    integer, intent(out)              :: iref
    real(kind=dp), intent(out)               :: xref
    ! local variables
    integer                           :: ne
    integer                           :: i
    integer                           :: var_type
    integer, parameter                :: type_real = 1
    integer, parameter                :: type_int = 2
    character*128                     :: fmt
    !
    ! initialize values
    iref = -99
    xref = 1e20
    !
    do i=1,30
       !
       if(dummy_char(i:i).eq.' ') then
          ne = i-1
          exit
       end if
       !
    end do
    !
    var_type = type_int
    !
    ! check if input is real or integer
    do i=1,ne
       !
       if(dummy_char(i:i).eq.'.'.or.dummy_char(i:i).eq.'d'.or.dummy_char(i:i).eq.'e') then
          !
          var_type = type_real
          !
          exit
          !
       end if
       !
    end do    
    !
    if(var_type.eq.type_int) then
       !
       write(*,'(a)')'Integer input'
       !
       if(ne.lt.10) then
          !
          write(fmt,'(A,I1,A)')'(I',ne,')'
          !
       elseif(ne.le.100) then
          !
          write(fmt,'(A,I2,A)')'(I',ne,')'          
          !
       end if
       !
       read(dummy_char,fmt)iref
       !
       xref = x(iref)
       !
    elseif(var_type.eq.type_real) then
       !
       write(*,'(a)')'Real input'
       !
       read(dummy_char,*)xref
       !
       call find_closest_point(nx,x(1:nx),xref,iref)
       !
    end if
    !
    write(*,'(A,I20)')   'Reference index: ',iref
    write(*,'(A,F20.10)')'Reference x-pos: ',xref
    !
  end subroutine get_reference_values
  !----------------------------------------------------------------------------------------------------
  ! search for specific substring
  !----------------------------------------------------------------------------------------------------
  subroutine util_find_substring(string,substring,switch,is,ie,debug)
    !
    ! input variables
    character(*), intent(inout) :: string
    character(*), intent(in)    :: substring
    logical, intent(in)         :: debug
    ! output variables
    logical, intent(out)        :: switch
    integer, intent(out)        :: is,ie
    ! local variables
    integer                     :: nchar_full
    integer                     :: nchar_sub
    integer                     :: nfit
    integer                     :: i
    character*128               :: comparison_string
    character*128               :: substring_loc
    !
    ! copy input argument substring into local substring which can be modified
    substring_loc = trim(substring)
    !
    ! first set all strings to lowercase
    call low_cap(string,cap2low)
    !
    call low_cap(substring_loc,cap2low)
    !
    ! initialize switch
    switch = .false.
    !
    ! determine lenght of full string
    nchar_full = len(trim(string))
    !
    ! determine lenght of substring_loc
    nchar_sub = len(trim(substring_loc))
    !
    ! check how many times the substring_loc could fit into full string
    nfit = int(nchar_full/nchar_sub)
    !
    if(debug) then
       !
       write(*,'(A,A)') 'full string                 =  ',trim(string)
       write(*,'(A,I8)')'legnth of full string nchar = ',nchar_full
       !
       write(*,'(A,A)') 'substring_loc                  =  ',trim(substring_loc)
       write(*,'(A,I8)')'legnth of substring nchar  = ',nchar_sub
       !
       write(*,'(A,I4,A)')'substring could fit ',nfit,' times in full string'
       stop
       !
    end if
    !
    is = 1
    !
    do i = 1,nchar_full-nchar_sub+1
       !
       ie = is+nchar_sub-1
       !
       comparison_string = string(is:ie)
       !
       if(debug) then
          !
          write(*,*)i,is,ie,'compare: ',trim(comparison_string),' to ',trim(substring_loc)
          !
       end if
       !
       if(trim(comparison_string).eq.trim(substring_loc)) then
          !
          switch = .true.
          !
          exit
          !
       end if
       !
       ! increase substring start by 1
       is = is + 1
       !
    end do
    !
    ! if the substring was not found then set is and ie to -99
    if(.not.switch) then
       !
       is = -99
       ie = -99
       !
    end if
    !
    ! return to calling routine
    return
    !
  end subroutine util_find_substring
  !----------------------------------------------------------------------------------------------------
  ! replace substring
  !----------------------------------------------------------------------------------------------------
  subroutine replace_substring(string,substring_old,substring_new,debug)
    !
    ! input variables
    character(*), intent(inout) :: string
    character(*), intent(in)    :: substring_old
    character(*), intent(in)    :: substring_new
    logical, intent(in)         :: debug
    ! local variables
    integer                     :: nchar_full
    integer                     :: nchar_sub    
    integer                     :: is,ie
    integer                     :: i
    character*128               :: comparison_string
    character*128               :: str_tmp
    !
    ! determine lenght of full string
    nchar_full = len(trim(string))
    ! determine lenght of substring
    nchar_sub = len(trim(substring_old))
    !
    is = 1
    !
    do i = 1,nchar_full-nchar_sub+1
       !
       ie = is+nchar_sub-1
       !
       comparison_string = string(is:ie)
       !
       if(debug) then
          !
          write(*,*)i,is,ie,'compare: ',trim(comparison_string),' to ',trim(substring_old)
          !
       end if
       !
       if(trim(comparison_string).eq.trim(substring_old)) then
          !
          exit
          !
       end if
       !
       ! increase substring start by 1
       is = is + 1
       !
    end do
    !
    write(str_tmp,'(a,a,a)')trim(string(1:is-1)),trim(substring_new),trim(string(ie+1:nchar_full))
    !
    string=str_tmp
    !
  end subroutine replace_substring
  !----------------------------------------------------------------------------------------------------
  ! stop the run with error message
  !----------------------------------------------------------------------------------------------------
  subroutine stop_run(message)
    !
#if(PAR==1)
    use mpi
#endif
    !
    implicit none
    !
    ! input variables
    character(*), intent(in) :: message
    ! local variables
    integer                  :: rank
    integer                  :: ierr
    !
#if(PAR==1)
    ! get processor rank
    call mpi_comm_rank(mpi_comm_world,rank,ierr)
#endif
    !
    if(rank.eq.0) then
       call print_message(message,RED)
    end if
    !
#if(PAR==0)
    stop
#elif(PAR==1)
    call mpi_abort(mpi_comm_world,ierr,ierr)
#endif
    !
  end subroutine stop_run
  !----------------------------------------------------------------------------------------------------
  ! routine to output error message
  ! use as:
  ! call set_error(__FILE__,__LINE__,'message')
  !----------------------------------------------------------------------------------------------------
  subroutine set_error(fname,line,message)
    !
    implicit none
    !
    character(*), intent(in) :: fname
    integer, intent(in)      :: line
    character(*), intent(in) :: message
    character*128            :: start_code
    character*128            :: end_code    
    integer                  :: color
    !
    color = RED
    !
    if(color.lt.10) then
       write(start_code,'(A,A,I1,A)')achar(27),'[',color,'m'
    elseif(color.lt.100) then
       write(start_code,'(A,A,I2,A)')achar(27),'[',color,'m'       
    elseif(color.lt.1000) then
       write(start_code,'(A,A,I3,A)')achar(27),'[',color,'m'
    end if
    !
    end_code = achar(27)//'[0m'
    !
    write(*,'(a,a,a,a,i10,a)')trim(start_code),'Error in : file = ',trim(fname),', line = ',line,trim(end_code)
    write(*,'(a,a,a)')trim(start_code),trim(message),trim(end_code)
    stop
    !
  end subroutine set_error
  !----------------------------------------------------------------------------------------------------
  ! check exponent
  !----------------------------------------------------------------------------------------------------
  subroutine check_exponent(dummy,exponent,debug)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in) :: dummy
    logical, intent(in)      :: debug
    ! output variables
    integer, intent(out)     :: exponent
    ! local variables
    integer                  :: i
    integer                  :: is,ie
    integer                  :: ndigits
    !
    ie = len(trim(dummy))
    !
    do is=1,ie
       !
       if(dummy(is:is).eq.'E') then
          exit
       end if
       !
    end do
    !
    is = is + 2
    !
    read(dummy(is:ie),'(i10)')exponent
    !
    if(debug) then
       !
       write(*,'(a)')    'number   : ',trim(dummy)
       write(*,'(a,i10)')'Exponent : ',exponent
       !
    end if
    !
  end subroutine check_exponent
  !----------------------------------------------------------------------------------------------------
  ! get geometry type
  !----------------------------------------------------------------------------------------------------
  subroutine get_geometry_type(geometry_type)
    !
    implicit none
    !
    ! output variables
    integer, intent(out) :: geometry_type
    ! local variables
    character*512        :: message
    !
    ! initialize output variable
    geometry_type = -99
    !
    write(*,'(a)')'Select geometry type: '
    write(*,'(a)')'NOTE: for a straight cone with a non-body fitted grid (for FV code) select generalized geometry'
    !
    write(*,'(a,i1,a)')'(',flat_plate,') flat plate'
    write(*,'(a,i1,a)')'(',straight_cone,') straight cone (body-fitted)'
    write(*,'(a,i1,a)')'(',flared_cone_purdue,') flared cone (purdue)'
    write(*,'(a,i1,a)')'(',flared_cone_nasa,') flared cone (nasa)'
    write(*,'(a,i1,a)')'(',flared_cone_aedc,') flared cone (aedc)'
    write(*,'(a,i1,a)')'(',generalized,') generalized geometry (for example von Karman ogive or straight cone for FV code)'
    read(*,*)geometry_type
    !
    if(geometry_type.eq.flat_plate) then
       !
       ! valid geometry selection
       !
    elseif(geometry_type.eq.straight_cone) then
       !
       ! valid geometry selection
       !
    elseif(geometry_type.eq.flared_cone_purdue.or.geometry_type.eq.flared_cone_nasa.or.geometry_type.eq.flared_cone_aedc) then
       !
       ! valid geometry selection
       !
    elseif(geometry_type.eq.generalized) then
       !
       ! valid geometry selection
       !
    else
       !
       write(message,'(a,i4,a)')'Geometry type: ',geometry_type,' unknown'
       call print_message(message,RED)
       stop
       !
    end if
    !
  end subroutine get_geometry_type
  !----------------------------------------------------------------------------------------------------
  ! linear interpolation between two values
  !----------------------------------------------------------------------------------------------------
  subroutine linear_interpolation(y0,y1,f0,f1,y_int,f_int,debug)
    !
    implicit none
    !
    ! input variables
    real(kind=dp), intent(in)  :: y0, y1, f0, f1, f_int
    logical, intent(in) :: debug
    ! output variables
    real(kind=dp), intent(out) :: y_int
    ! local variables
    real(kind=dp)              :: dy, df
    !
    df = f1-f0
    dy = y1-y0
    !
    y_int = y0 + (f_int-f0)/df*dy
    !
  end subroutine linear_interpolation
  !----------------------------------------------------------------------------------------------------
  ! search for a string enclosed by a specific character
  !----------------------------------------------------------------------------------------------------
  subroutine get_enclosed_string(str_tmp,delimeter,is,ie,enclosed_string,debug)
    !
    implicit none
    !
    ! input output variables
    character(*), intent(inout) :: str_tmp
    character(*), intent(in)    :: delimeter
    integer, intent(out)        :: is,ie
    character(*), intent(out)   :: enclosed_string
    logical, intent(in)         :: debug
    ! local variables
    integer                     :: i
    integer                     :: nchar
    integer                     :: delimeter_count
    !
    ! initialize values
    is = -99
    ie = -99
    !
    ! get total length of character string
    nchar = len(trim(str_tmp))
    !
    ! initialize delimeter count
    delimeter_count = 0
    !
    do i = 1,nchar
       !
       if(debug) then
          !
          write(*,'(a,i4,a,a)')'i = ',i,': ',str_tmp(i:i)
          !
       end if
       !
       if(str_tmp(i:i).eq.trim(delimeter)) then
          !
          if(delimeter_count.eq.0) then
             !
             if(debug) then
                !
                write(*,'(a,i4)')'Start of enclosed string found = ',is
                !
             end if
             !
             is = i
             !
          else
             !
             ie = i
             !
             if(debug) then
                !
                write(*,'(a,i4)')'End of enclosed string found = ',ie
                !
             end if
             !
             ! check if enclosed string is just an empty space
             if(str_tmp(is:ie).eq.' ') then
                !
                ! continue with search
             else
                ! exit out
                exit
                !
             end if
             !
          end if
          !
          delimeter_count = delimeter_count+1
          !
       end if
       !
    end do
    !
    if(is>0.and.ie>0) then
       !
       enclosed_string=str_tmp(is+1:ie-1)
       !
    else
       !
       enclosed_string='NA'
       !
    end if
    !
    if(debug) then
       !
       write(*,'(a,a)')'Enclosed string: ',trim(enclosed_string)
       !
    end if
    !
  end subroutine get_enclosed_string
  !----------------------------------------------------------------------------------------------------
  ! count number of delimeters in one line
  !----------------------------------------------------------------------------------------------------
  subroutine count_delimeters(str_tmp,delimeter,ndelimeter)
    !
    ! input output variables
    character(*), intent(inout) :: str_tmp
    character(*), intent(in)    :: delimeter
    integer, intent(out)        :: ndelimeter
    ! local variables
    integer                     :: nchar
    integer                     :: i
    !
    ! get total length of character string
    nchar = len(trim(str_tmp))
    !
    ! initialize delimeter count
    ndelimeter = 0
    !
    do i = 1,nchar
       !
       if(str_tmp(i:i).eq.trim(delimeter)) then
          !
          ndelimeter = ndelimeter + 1
          !
       end if
       !
    end do
    !
  end subroutine count_delimeters
  !----------------------------------------------------------------------------------------------------  
  ! convert a string to an integer
  !----------------------------------------------------------------------------------------------------
  subroutine str2int(str_tmp,int_tmp,debug)
    !
    implicit none
    !
    ! input/output variables
    character(*), intent(in) :: str_tmp
    integer, intent(out)     :: int_tmp
    logical, intent(in)      :: debug
    ! local variables
    integer                  :: ndig
    integer                  :: nchar
    character*128            :: str_fmt
    logical                  :: fmt_assigned
    integer                  :: mag
    integer                  :: exp
    !
    ! initialize magnitude and exponent
    mag = 10
    exp = 1
    !
    ! initialize format assigned value
    fmt_assigned = .false.
    !
    ndig = len(trim(str_tmp))
    !
    if(debug) then
       !
       write(*,'(a,i10)')'number of digits: ',ndig
       !
    end if
    !
    if(ndig.lt.10) then
       !
       write(str_fmt,'(a,i1,a)')'(i',ndig,')'
       !
    elseif(ndig.le.100) then
       !
       write(str_fmt,'(a,i2,a)')'(i',ndig,')'
       !
    elseif(ndig.le.1000) then
       !
       write(str_fmt,'(a,i3,a)')'(i',ndig,')'
       !
    elseif(ndig.le.10000) then
       !
       write(str_fmt,'(a,i4,a)')'(i',ndig,')'
       !
    elseif(ndig.le.100000) then
       !
       write(str_fmt,'(a,i5,a)')'(i',ndig,')'
       !
    elseif(ndig.le.1000000) then
       !
       write(str_fmt,'(a,i6,a)')'(i',ndig,')'
       !
    elseif(ndig.le.10000000) then
       !
       write(str_fmt,'(a,i7,a)')'(i',ndig,')'
       !
    elseif(ndig.le.100000000) then
       !
       write(str_fmt,'(a,i8,a)')'(i',ndig,')'
       !
    elseif(ndig.le.1000000000) then
       !
       write(str_fmt,'(a,i9,a)')'(i',ndig,')'
       !
    end if
    !
    if(debug) then
       !
       write(*,*)'fmt = ',trim(str_fmt),', string = ',trim(str_tmp)
       !
    end if
    !
    !read(str_tmp,str_fmt)int_tmp
    read(str_tmp,'(i8)')int_tmp
    !
  end subroutine str2int
  !----------------------------------------------------------------------------------------------------
  ! subroutine to try and get the computational time step 
  !----------------------------------------------------------------------------------------------------  
  subroutine util_get_dt_sim(dt_sim,debug)
    !
    implicit none
    !
    ! input/output variables
    real(kind=dp), intent(out) :: dt_sim
    logical, intent(in) :: debug
    ! local variables
    integer             :: idir,ndirs,ifile
    integer             :: funit
    integer             :: nskip
    character*128       :: dir_name, dir_name_tmp
    character*128       :: fname, fname_full
    logical             :: file_exists
    integer             :: i
    character*128       :: dummy
    logical             :: dt_sim_found
    character*128       :: answer
    integer             :: ierr
    integer :: nlines
    character*512, dimension(:), allocatable :: lines
    integer :: is,ie
    integer :: iline_dt
    character*512 :: dt_str
    !
    ! initialize dt_sim
    dt_sim = -1e20
    !
    ! output for user
    write(*,'(a)')''
    write(*,'(a)')'attempt to determine computational timestep'
    write(*,'(a)')''
    !
    ! set line number to be skipped
    nskip = 52
    !
    ! set counter how many directories should be searched
    ! ndirs = 1 => search local directory (./)
    ! ndirs = 2 =. search local directory (./) and parent directory (../)
    ! ndirs = 3 => search local directory (./), first parent directory (../) and second parent directory (../../)
    !
    ndirs = 2
    !
    dir_name = ''
    !
    do_idir: do idir = 1,ndirs
       !
       do ifile = 1,40
          !
          if(ifile<10) then
             !
             write(fname,'(a,i1,a)')'dat_c0',ifile-1,'.in'
             !
          else
             !
             write(fname,'(a,i2,a)')'dat_c',ifile-1,'.in'
             !
          end if
          !
          ! assemble file name
          write(fname_full,'(a,a)')trim(dir_name),trim(fname)
          !
          if(debug) then
             !
             write(*,*)idir,ifile,trim(fname_full)
             !
          end if
          !
          ! check if file name exists
          inquire(file=fname_full,exist=file_exists)
          !
          if(debug) then
             !
             write(*,*)'file exists = ',file_exists
             !
          end if
          !
          if(file_exists) then
             !
             exit do_idir
             !
          end if
          !
       end do
       !
       write(dir_name_tmp,'(a,a)')trim(dir_name),'../'
       !
       dir_name = dir_name_tmp
       !
       ! end idir loop
    end do do_idir
    !
    ! if file was found => try to read simulation time step
    if(file_exists) then
       !
       ! get free funit number
       call util_find_free_funit(funit,debug)
       !
       ! open file that exists
       open(unit=funit,file=trim(fname_full),status='unknown')
       !
       nlines = 0
       !
       ierr = 0
       !
       ! count lines
       do while(ierr.eq.0)
          !
          read(funit,'(a)',iostat=ierr)dummy
          !
          if(ierr.eq.0) nlines = nlines + 1
          !
       end do
       !
       rewind(funit)
       !
       allocate(lines(nlines))
       !
       ! read all lines
       do i = 1,nlines
          !
          read(funit,'(a)')lines(i)
          !
          if(debug) then
             !
             write(*,'(a,i5,a,a)')'line (',i,'): ',trim(lines(i))
             !
          end if
          !
       end do
       !
       ! loop over lines and find string "time integration" or "TIME DATA"
       !
       do i = 1,nlines
          !
          call util_find_substring(lines(i),'TIME DATA',dt_sim_found,is,ie,.false.)
          !
          if(dt_sim_found.and.is.eq.1) then
             !
             iline_dt = i+4
             !
             exit
             !
          end if
          !
          call util_find_substring(lines(i),'time integration',dt_sim_found,is,ie,.false.)
          !
          if(dt_sim_found.and.is.eq.1) then
             !
             iline_dt = i+4
             !
             exit
             !
          end if          
          !
       end do
       !
       if(dt_sim_found) then
          !
          dt_str = trim(lines(iline_dt))
          !
          ! initialize is and ie
          is = 1
          ie = len(trim(dt_str))
          !          
          if(debug) then
             !
             write(*,'(a)')''
             write(*,'(a)')'time data found...'
             write(*,'(a)')''
             write(*,'(a,i5)')'line: ',iline_dt
             write(*,'(a,a)')'string: ',trim(dt_str)
             !
          end if
          !
          ! remove all leading spaces
          do i = 1,len(trim(dt_str))
             !
             if(dt_str(i:i).eq.' ') then
                !
                is = i
                !
             else
                !
                exit
                !
             end if
             !
          end do
          !
          ! remove all trailing spaces
          do i = is+1,len(trim(dt_str))
             !
             if(dt_str(i:i).eq.' ' .or. dt_str(i:i).eq.char(9)) then
                !
                ie = i-1
                !
                exit
                !
             end if
             !
          end do
          !
          read(dt_str(is:ie),'(e30.16)')dt_sim
          !
          write(*,*)
          write(*,'(a,a,a,e20.10)')'computational timestep found in file ',trim(fname_full),': ',dt_sim
          write(*,'(a)')'Time step correct?'
          write(*,'(a)')'[Y/y]es'
          write(*,'(a)')'[N/n]o'
          read(*,*)answer
          !
          call util_interpret_answer(answer,dt_sim_found)
          !          
       else
          !
          write(*,'(a)')''
          write(*,'(a)')trim(hrule_war)
          write(*,'(a)')'warning: automatic time step detection failed => possible cause: legacy input file'
          write(*,'(a)')trim(hrule_war)
          write(*,'(a)')''
          !
       end if
       !
    else
       !
       dt_sim_found = .false.
       !
    end if
    !
    if(.not.dt_sim_found) then
       !
       write(*,'(a)')'Provide value for computational timestep:'
       read(*,*)dt_sim
       !
    end if
    !
    ! close input file
    close(funit)    
    !
  end subroutine util_get_dt_sim
  !----------------------------------------------------------------------------------------------------
  ! subroutine to try and get the computational extent in z
  !----------------------------------------------------------------------------------------------------  
  subroutine util_get_zsze(zs,ze,debug)
    !
    implicit none
    !
    ! input/output variables
    real(kind=dp), intent(out) :: zs,ze
    logical, intent(in) :: debug
    ! local variables
    integer             :: idir,ndirs,ifile
    integer             :: funit
    integer             :: nskip
    character*128       :: dir_name, dir_name_tmp
    character*128       :: fname,fname_full
    logical             :: file_exists
    integer             :: i
    character*128       :: dummy
    logical             :: zvals_found
    character*128       :: answer
    integer             :: ierr
    !
    ! initialize zs
    zs = 0
    !
    ! initialize ze
    ze = 0
    !
    ! output for user
    write(*,'(a)')''
    write(*,'(a)')'determine spanwise/azimuthal extent of the computational domain...'
    write(*,'(a)')''
    !
    ! set line number to be skipped
    nskip = 43
    !
    ! set counter how many directories should be searched
    ! ndirs = 1 => search local directory (./)
    ! ndirs = 2 =. search local directory (./) and parent directory (../)
    ! ndirs = 3 => search local directory (./), first parent directory (../) and second parent directory (../../)
    !
    ndirs = 2
    !
    dir_name = ''
    !
    do idir = 1,ndirs
       !
       do ifile = 1,40
          !
          if(ifile<10) then
             !
             write(fname,'(a,i1,a)')'dat_c0',ifile-1,'.in'
             !
          else
             !
             write(fname,'(a,i2,a)')'dat_c',ifile-1,'.in'
             !
          end if
          !
          ! assemble file name
          write(fname_full,'(a,a)')trim(dir_name),trim(fname)
          !
          if(debug) then
             !
             write(*,*)idir,ifile,trim(fname_full)
             !
          end if
          !
          ! check if file name exists
          inquire(file=fname_full,exist=file_exists)
          !
          if(debug) then
             !
             write(*,*)'file exists = ',file_exists
             !
          end if
          !
          if(file_exists) then
             !
             exit
             !
          end if
          !
       end do
       !
       if(file_exists) then
          !
          exit
          !
       end if
       !
       write(dir_name_tmp,'(a,a)')trim(dir_name),'../'
       !
       dir_name = dir_name_tmp
       !
       ! end idir loop
    end do
    !
    ! if file was found => try to read simulation time step
    if(file_exists) then
       !
       ! get free funit number
       call util_find_free_funit(funit,debug)
       !
       ! open file that exists
       open(unit=funit,file=trim(fname_full),status='unknown')
       !
       do i = 1,nskip
          !
          read(funit,'(a)')dummy
          !
          if(debug) then
             !
             write(*,*)i,trim(dummy)
             !
          end if
          !
       end do
       !    
       ! read time step
       read(funit,*,iostat=ierr)zs
       read(funit,*,iostat=ierr)ze       
       !
       if(ierr.ne.0) then
          !
          nskip = 42
          !
          rewind(funit)
          !
          do i = 1,nskip
             !
             read(funit,'(a)')dummy
             !
             if(debug) then
                !
                write(*,*)i,trim(dummy)
                !
             end if
             !
          end do
          !    
          ! read zs and ze
          read(funit,*,iostat=ierr)zs
          read(funit,*,iostat=ierr)ze       
          !
       end if
       !
       if(ierr.ne.0) then
          !
          write(*,'(a)')''
          write(*,'(a)')trim(hrule_err)
          write(*,'(a)')'Warning: automatic detection of computational extent failed'
          write(*,'(a)')trim(hrule_err)
          write(*,'(a)')''
          !
          ! set zvals_found to false
          zvals_found = .false.
          !
       else
          !       
          write(*,'(a,a)')'Extent of computational fomdain found in:  ',trim(fname_full)
          write(*,'(a,e20.10)')'zs = ',zs
          write(*,'(a,e20.10)')'ze = ',ze
          write(*,*)
          write(*,'(a)')'spanwise/azimuthal extent of computational domain correct?'
          write(*,'(a)')'[Y/y]es'
          write(*,'(a)')'[N/n]o'
          read(*,*)answer
          !
          call util_interpret_answer(answer,zvals_found)
          !
       end if
       !
    else
       !
       zvals_found = .false.
       !
    end if
    !
    if(.not.zvals_found) then
       !
       write(*,'(a)')'Provide value for computational extent in the spanwsie/azimuthal direction (zs,ze):'
       read(*,*)zs,ze
       !
    end if
    !
    ! close input file
    close(funit)    
    !
  end subroutine util_get_zsze
  !----------------------------------------------------------------------------------------------------
  ! subroutine to try and get the number of points in z used in the simulation
  !----------------------------------------------------------------------------------------------------  
  subroutine util_get_nz(nz,debug)
    !
    implicit none
    !
    ! input/output variables
    integer, intent(out) :: nz
    logical, intent(in) :: debug
    ! local variables
    integer             :: idir,ndirs,ifile
    integer             :: funit
    integer             :: nskip
    character*128       :: dir_name, dir_name_tmp
    character*128       :: fname,fname_full
    logical             :: file_exists
    integer             :: i
    character*128       :: dummy
    logical             :: val_found
    character*128       :: answer
    integer             :: ierr
    integer :: dummy_int
    !
    ! initialize nz
    nz = 1
    !
    ! output for user
    write(*,'(a)')''
    write(*,'(a)')'determine number of points used in computational domain...'
    write(*,'(a)')''
    !
    ! set line number to be skipped
    nskip = 22
    !
    ! set counter how many directories should be searched
    ! ndirs = 1 => search local directory (./)
    ! ndirs = 2 =. search local directory (./) and parent directory (../)
    ! ndirs = 3 => search local directory (./), first parent directory (../) and second parent directory (../../)
    !
    ndirs = 2
    !
    dir_name = ''
    !
    do idir = 1,ndirs
       !
       do ifile = 1,40
          !
          if(ifile<10) then
             !
             write(fname,'(a,i1,a)')'dat_c0',ifile-1,'.in'
             !
          else
             !
             write(fname,'(a,i2,a)')'dat_c',ifile-1,'.in'
             !
          end if
          !
          ! assemble file name
          write(fname_full,'(a,a)')trim(dir_name),trim(fname)
          !
          if(debug) then
             !
             write(*,*)idir,ifile,trim(fname_full)
             !
          end if
          !
          ! check if file name exists
          inquire(file=fname_full,exist=file_exists)
          !
          if(debug) then
             !
             write(*,*)'file exists = ',file_exists
             !
          end if
          !
          if(file_exists) then
             !
             exit
             !
          end if
          !
       end do
       !
       if(file_exists) then
          !
          exit
          !
       end if
       !
       write(dir_name_tmp,'(a,a)')trim(dir_name),'../'
       !
       dir_name = dir_name_tmp
       !
       ! end idir loop
    end do
    !
    ! if file was found => try to read simulation time step
    if(file_exists) then
       !
       ! get free funit number
       call util_find_free_funit(funit,debug)
       !
       ! open file that exists
       open(unit=funit,file=trim(fname_full),status='unknown')
       !
       do i = 1,nskip
          !
          read(funit,'(a)')dummy
          !
          if(debug) then
             !
             write(*,*)i,trim(dummy)
             !
          end if
          !
       end do
       !    
       ! read time step
       read(funit,*,iostat=ierr)dummy_int,dummy_int,nz
       !
       if(ierr.ne.0) then
          !
          write(*,'(a)')''
          write(*,'(a)')trim(hrule_err)
          write(*,'(a)')'Warning: automatic detection of nz failed'
          write(*,'(a)')trim(hrule_err)
          write(*,'(a)')''
          !
          ! set zvals_found to false
          val_found = .false.
          !
       else
          !       
          write(*,'(a,a)')'nz found in:  ',trim(fname_full)
          write(*,'(a,i6)')'nz = ',nz
          write(*,*)
          write(*,'(a)')'nz correct?'
          write(*,'(a)')'[Y/y]es'
          write(*,'(a)')'[N/n]o'
          read(*,*)answer
          !
          call util_interpret_answer(answer,val_found)
          !
       end if
       !
    else
       !
       val_found = .false.
       !
    end if
    !
    if(.not.val_found) then
       !
       write(*,'(a)')'Provide value for nz:'
       read(*,*)nz
       !
    end if
    !
    ! close input file
    close(funit)    
    !
  end subroutine util_get_nz
  !----------------------------------------------------------------------------------------------------
  ! try to determined if symmetric discritization was used
  !----------------------------------------------------------------------------------------------------  
  subroutine util_check_symmetric(is_symmetric,debug)
    !
    implicit none
    !
    ! input/output variables
    logical, intent(out) :: is_symmetric
    logical, intent(in)  :: debug
    ! local variables
    integer             :: idir,ndirs,ifile
    integer             :: funit
    integer             :: nlines
    character*128       :: dir_name, dir_name_tmp
    character*128       :: fname(9), fname_full
    logical             :: file_exists
    integer             :: iline
    character*128       :: dummy
    logical             :: discretization_found
    character*128       :: answer
    integer             :: ierr
    integer             :: nchars
    integer             :: is,ie
    integer             :: ichar
    !
    ! initialize is_symmetric flag
    is_symmetric = .false.
    !
    ! output for user
    write(*,'(a)')''
    write(*,'(a)')'check if symmetry condition was used...'
    write(*,'(a)')''
    !
    ! file list to try and open
    fname(1) = 'dat.in'
    fname(2) = 'dat_c00.in'
    fname(3) = 'dat_c01.in'
    fname(4) = 'dat_c02.in'
    fname(5) = 'dat_c03.in'
    fname(6) = 'dat_c04.in'
    fname(7) = 'dat_c05.in'
    fname(8) = 'dat_c06.in'
    fname(9) = 'dat_c07.in'
    !
    ! set counter how many directories should be searched
    ! ndirs = 1 => search local directory (./)
    ! ndirs = 2 =. search local directory (./) and parent directory (../)
    ! ndirs = 3 => search local directory (./), first parent directory (../) and second parent directory (../../)
    !
    ndirs = 2
    !
    dir_name = ''
    !
    do idir = 1,ndirs
       !
       do ifile = 1,9
          !
          ! assemble file name
          write(fname_full,'(a,a)')trim(dir_name),trim(fname(ifile))
          !
          if(debug) then
             !
             write(*,*)idir,ifile,trim(fname_full)
             !
          end if
          !
          ! check if file name exists
          inquire(file=fname_full,exist=file_exists)
          !
          if(debug) then
             !
             write(*,*)'file exists = ',file_exists
             !
          end if
          !
          if(file_exists) then
             !
             exit
             !
          end if
          !
       end do
       !
       if(file_exists) then
          !
          exit
          !
       end if
       !
       write(dir_name_tmp,'(a,a)')trim(dir_name),'../'
       !
       dir_name = dir_name_tmp
       !
       ! end idir loop
    end do
    !
    ! if file was found => try to read simulation time step
    if(file_exists) then
       !
       ! get free funit number
       call util_find_free_funit(funit,debug)
       !
       ! open file that exists
       open(unit=funit,file=trim(fname_full),status='unknown')
       !
       ierr = 0
       !
       nlines = 1
       !
       do while (ierr==0)
          !
          read(funit,'(a)',iostat=ierr)dummy
          !
          nlines = nlines + 1
          !
       end do
       !
       rewind(funit)
       !
       do iline = 1,nlines
          !
          read(funit,'(a)')dummy
          !
          if(trim(dummy)=='spatial discretization') then
             !
             read(funit,'(a)')dummy
             read(funit,'(a)',iostat=ierr)dummy
             !
             exit
             !
          end if
          !
       end do
       !
       nchars = len(trim(dummy))
       !
       do ichar = 1,nchars
          !
          if(dummy(ichar:ichar).ne.' ') then
             !
             is = ichar
             !
             exit
             !
          end if          
          !
       end do
       !
       do ichar = is+1,nchars
          !
          if(dummy(ichar:ichar).eq.' ' .or. dummy(ichar:ichar) .eq. char(9)) then
             !
             ie = ichar-1
             !
             exit
             !
          end if
          !
       end do
       !
       if(dummy(is:ie).eq.'F' .or. dummy(is:ie).eq.'f') then
          !
          is_symmetric = .false.
          !
       else
          !
          is_symmetric = .true.
          !
       end if
       !
       if(ierr.ne.0) then
          !
          write(*,'(a)')''
          write(*,'(a)')trim(hrule_war)
          write(*,'(a)')'warning: could not determine if symmetry condition was used'
          write(*,'(a)')trim(hrule_war)
          write(*,'(a)')''
          !
       else
          !
          write(*,'(a,a)')'diescretization information found in ',trim(fname_full)
          write(*,*)
          !
          if(is_symmetric) then
             !
             write(*,'(a)')'symmetry condition was used'
             !
          else
             !
             write(*,'(a)')'no symmetry condition applied => periodic calculation'
             !
          end if
          !
          write(*,*)
          write(*,'(a)')'spanwise/azimuthal discretization correct?'
          write(*,'(a)')'[Y/y]es'
          write(*,'(a)')'[N/n]o'
          read(*,*)answer
          !
          call util_interpret_answer(answer,discretization_found)
          !
       end if
       !
    end if
    !
    if(.not.discretization_found) then
       !
       write(*,*)
       write(*,'(a)')'symmetry condition in spanwise/azimuthal direction?'
       write(*,'(a)')'[Y/y]es'
       write(*,'(a)')'[N/n]o'
       read(*,*)answer
       !
       call util_interpret_answer(answer,is_symmetric)
       !
    end if
    !
    ! close input file
    close(funit)
    !
  end subroutine util_check_symmetric
  !----------------------------------------------------------------------------------------------------
  ! subroutine to try and get the reference time scale
  !----------------------------------------------------------------------------------------------------  
  subroutine util_get_tref(tref,debug)
    !
    implicit none
    !
    ! input/output variables
    real(kind=dp), intent(out) :: tref
    logical, intent(in) :: debug
    ! local variables
    integer             :: idir,ndirs
    integer             :: funit
    integer             :: nskip
    character*128       :: dir_name, dir_name_tmp
    character*128       :: fname, fname_full
    logical             :: file_exists
    integer             :: i
    character*128       :: dummy
    logical             :: tref_found
    character*128       :: answer
    !
    ! initialize tref
    tref = -1e20
    !
    ! output for user
    write(*,'(a)')''
    write(*,'(a)')'attempt to determine reference time scale'
    write(*,'(a)')''
    !
    ! file list to try and open
    fname = 'flow_conditions.dat'
    !
    ! set line number to be skipped
    nskip = 19
    !
    ! set counter how many directories should be searched
    ! ndirs = 1 => search local directory (./)
    ! ndirs = 2 =. search local directory (./) and parent directory (../)
    ! ndirs = 3 => search local directory (./), first parent directory (../) and second parent directory (../../)
    !
    ndirs = 3
    !
    dir_name = './'
    !
    do idir = 1,ndirs
       !
       ! assemble file name
       write(fname_full,'(a,a)')trim(dir_name),trim(fname)
       !
       if(debug) then
          !
          write(*,*)idir,trim(fname_full)
          !
       end if
       !
       ! check if file name exists
       inquire(file=fname_full,exist=file_exists)
       !
       if(file_exists) then
          !
          exit
          !
       end if
       !
       write(dir_name_tmp,'(a,a)')trim(dir_name),'../'
       !
       dir_name = dir_name_tmp
       !
       ! end idir
    end do
    !
    if(file_exists) then
       !
       ! get free funit number
       call util_find_free_funit(funit,debug)
       !
       ! open file that exists
       open(unit=funit,file=trim(fname_full),status='unknown')
       !
       do i = 1,nskip
          !
          read(funit,'(a)')dummy
          !
          if(debug) then
             !
             write(*,*)i,trim(dummy)
             !
          end if
          !
       end do
       !
       ! read time step
       read(funit,'(a33,f30.16)')dummy,tref
       !
       write(*,'(a,a,a,f20.10)')'Reference time scale found in file ',trim(fname_full),': ',tref
       write(*,'(a)')'Reference time scale correct?'
       write(*,'(a)')'[Y/y]es'
       write(*,'(a)')'[N/n]o'
       read(*,*)answer
       !
       call util_interpret_answer(answer,tref_found)
       !
    else
       !
       tref_found = .false.
       !
    end if
    !
    if(.not.tref_found) then
       !
       write(*,'(a)')'Provide value for the reference time scale:'
       read(*,*)tref
       !
    end if
    !
    ! close input file
    close(funit)    
    !
  end subroutine util_get_tref
  !----------------------------------------------------------------------------------------------------
  ! subroutine to get frequency quantum
  !----------------------------------------------------------------------------------------------------  
  subroutine util_get_df(df,debug)
    !
    implicit none
    !
    ! input/output variables
    real(kind=dp), intent(out) :: df
    logical, intent(in) :: debug
    ! local variables
    integer             :: idir,ndirs
    integer             :: funit
    integer             :: nskip
    character*128       :: dir_name, dir_name_tmp
    character*128       :: fname, fname_full
    logical             :: file_exists
    integer             :: i
    character*128       :: dummy,dummy2
    logical             :: df_found
    character*128       :: answer
    !
    ! output for user
    write(*,'(a)')''
    write(*,'(a)')'attempt to determine frequency quantum'
    write(*,'(a)')''
    !
    ! file list to try and open
    fname = 'fft.frequency_map'
    !
    ! set line number to be skipped
    nskip = 4
    !
    ! set counter how many directories should be searched
    ! ndirs = 1 => search local directory (./)
    ! ndirs = 2 =. search local directory (./) and parent directory (../)
    ! ndirs = 3 => search local directory (./), first parent directory (../) and second parent directory (../../)
    !
    ndirs = 3
    !
    dir_name = './'
    !
    do idir = 1,ndirs
       !
       ! assemble file name
       write(fname_full,'(a,a)')trim(dir_name),trim(fname)
       !
       if(debug) then
          !
          write(*,*)idir,trim(fname_full)
          !
       end if
       !
       ! check if file name exists
       inquire(file=fname_full,exist=file_exists)
       !
       if(file_exists) then
          !
          exit
          !
       end if
       !
       write(dir_name_tmp,'(a,a)')trim(dir_name),'../'
       !
       dir_name = dir_name_tmp
       !
       ! end idir
    end do
    !
    if(file_exists) then
       !
       ! get free funit number
       call util_find_free_funit(funit,debug)
       !
       ! open file that exists
       open(unit=funit,file=trim(fname_full),status='unknown')
       !
       do i = 1,nskip
          !
          read(funit,'(a)')dummy
          !
          if(debug) then
             !
             write(*,*)i,trim(dummy)
             !
          end if
          !
       end do
       !
       ! read time step
       read(funit,'(a28,e36.16,a)')dummy,df,dummy2
       !
       write(*,'(a,a,a,f20.10)')'frequency quantum found in file ',trim(fname_full),': ',df
       write(*,'(a)')'frequency quantum correct?'
       write(*,'(a)')'[Y/y]es'
       write(*,'(a)')'[N/n]o'
       read(*,*)answer
       !
       call util_interpret_answer(answer,df_found)
       !
    else
       !
       df_found = .false.
       !
    end if
    !
    if(.not.df_found) then
       !
       write(*,'(a)')'Provide value for frequency quantum:'
       read(*,*)df
       !
    end if
    !
    ! close input file
    close(funit)    
    !
  end subroutine util_get_df
  !----------------------------------------------------------------------------------------------------
  ! subroutine to get kc_domain
  !----------------------------------------------------------------------------------------------------  
  subroutine util_get_kc_domain(kc_domain,debug)
    !
    implicit none
    !
    ! input/output variables
    real(kind=dp), intent(out) :: kc_domain
    logical, intent(in) :: debug
    ! local variables
    integer             :: idir,ndirs
    integer             :: funit
    integer             :: nskip
    character*128       :: dir_name, dir_name_tmp
    character*128       :: fname, fname_full
    logical             :: file_exists
    integer             :: i
    character*128       :: dummy,dummy2
    logical             :: kc_domain_found
    character*128       :: answer
    logical :: str_found
    integer :: is,ie
    integer :: ierr
    character*128 :: substring
    logical :: is_symmetric
    real(kind=dp) :: ze
    !
    ! initialize kc_domain_found flag to .false.
    kc_domain_found = .false.
    !
    ! output for user
    write(*,'(a)')''
    write(*,'(a)')'trying to determine kc_domain...'
    write(*,'(a)')''
    !
    ! file list to try and open
    fname = 'dat_c00.in'
    !
    ! set line number to be skipped
    nskip = 4
    !
    ! set counter how many directories should be searched
    ! ndirs = 1 => search local directory (./)
    ! ndirs = 2 =. search local directory (./) and parent directory (../)
    ! ndirs = 3 => search local directory (./), first parent directory (../) and second parent directory (../../)
    !
    ndirs = 3
    !
    dir_name = './'
    !
    do idir = 1,ndirs
       !
       ! assemble file name
       write(fname_full,'(a,a)')trim(dir_name),trim(fname)
       !
       if(debug) then
          !
          write(*,*)idir,trim(fname_full)
          !
       end if
       !
       ! check if file name exists
       inquire(file=fname_full,exist=file_exists)
       !
       if(file_exists) then
          !
          exit
          !
       end if
       !
       write(dir_name_tmp,'(a,a)')trim(dir_name),'../'
       !
       dir_name = dir_name_tmp
       !
       ! end idir
    end do
    !
    if(file_exists) then
       !
       if(debug) then
          !
          write(*,*)
          write(*,'(a,a)')'found input file: ',trim(fname_full)
          !
       end if
       !
       ! get free funit number
       call util_find_free_funit(funit,debug)
       !
       ! open file that exists
       open(unit=funit,file=trim(fname_full),status='unknown')
       !
       ! search for spatial discretization substring
       !
       substring = 'spatial discretization'
       !
       str_found = .false.
       !       
       do while(.not. str_found)
          !
          read(funit,'(a)',iostat=ierr)dummy
          !
          if(ierr.ne.0) exit
          !
          call util_find_substring(dummy,substring,str_found,is,ie,.false.)
          !
       end do
       !
       if(str_found.and.is.eq.1) then
          !
          if(debug) then
             !
             write(*,*)
             write(*,'(a,a,a)')'substring "',trim(substring),'" found'
             write(*,'(a,i5)')'is = ',is
             write(*,'(a,i5)')'ie = ',ie
             !
          end if
          !
          read(funit,'(a)')dummy
          read(funit,*)is_symmetric
          !
          ! search for spatial discretization substring
          !
          substring = 'geometry'
          !
          str_found = .false.
          !       
          do while(.not. str_found)
             !
             read(funit,'(a)',iostat=ierr)dummy
             !
             if(ierr.ne.0) exit
             !
             call util_find_substring(dummy,substring,str_found,is,ie,.false.)
             !
          end do
          !
          if(str_found.and.is.eq.1) then
             !
             if(debug) then
                !
                write(*,*)
                write(*,'(a,a,a)')'substring "',trim(substring),'" found'
                write(*,'(a,i5)')'is = ',is
                write(*,'(a,i5)')'ie = ',ie
                !
             end if
             !
             read(funit,'(a)')dummy
             read(funit,'(a)')dummy
             read(funit,*)ze
             !
             kc_domain_found = .true.
             !
             if(is_symmetric) then
                !
                kc_domain = dble(pi)/dble(ze)
                !
             else
                !
                kc_domain = 2.d0*dble(pi)/dble(ze)
                !
             end if
             !
          end if
          !
       else
          !
          if(debug) then
             !
             write(*,*)
             write(*,'(a)')'end of file reached before substring was found'
             !
          end if
          !             
       end if
       !
       write(*,'(a,a,a,f20.4)')'kc_domain found in file ',trim(fname_full),': ',kc_domain
       write(*,'(a)')'kc_domain correct?'
       write(*,'(a)')'[Y/y]es'
       write(*,'(a)')'[N/n]o'
       read(*,*)answer
       !
       call util_interpret_answer(answer,kc_domain_found)
       !
    end if
    !
    if(.not.kc_domain_found) then
       !
       write(*,'(a)')'kc_domain for calculation:'
       read(*,*)kc_domain
       !
    end if
    !
    ! close input file
    close(funit)    
    !
  end subroutine util_get_kc_domain
  !----------------------------------------------------------------------------------------------------
  ! subroutine to determine an unused unit number
  !----------------------------------------------------------------------------------------------------  
  subroutine util_find_free_funit(funit,debug)
    !
    implicit none
    !
    ! input/output variables
    integer, intent(out) :: funit
    logical, intent(in)  :: debug
    ! local variables
    logical              :: invalid_funit
    !
    ! set unit number of input file and check if it was already assigned to another file
    invalid_funit = .true.
    !
    ! initialize funit
    funit = 199
    !
    do while(invalid_funit)
       !
       ! increase funit until a free unit number is found
       funit=funit+1
       !
       ! check if unit number was already opened
       inquire(unit=funit,opened=invalid_funit)
       !
       if(debug) then
          !
          write(*,*)funit,invalid_funit
          !
       end if
       !
    end do    
    !
  end subroutine util_find_free_funit
  !----------------------------------------------------------------------------------------------------
  ! remove all non-number characters from string
  !----------------------------------------------------------------------------------------------------
  subroutine remove_signs(string,debug)
    !
    implicit none
    ! 
    ! input/output variables
    character(128), intent(inout) :: string
    logical, intent(in)         :: debug
    ! local variables
    integer                     :: ichar
    integer                     :: nchars
    character(128)              :: string_tmp
    integer                     :: iloc
    !
    string_tmp = string
    !
    string = ' '
    !
    nchars = len(trim(string_tmp))
    !
    if(debug) then
       !
       write(*,*)'nchars = ',nchars
       !
    end if
    !
    iloc = 1
    !
    do ichar = 1,nchars
       !
       select case(string_tmp(ichar:ichar))
          !
       case('0')
          !
          string(iloc:iloc) = string_tmp(ichar:ichar)
          iloc = iloc + 1
          !
       case('1')
          !
          string(iloc:iloc) = string_tmp(ichar:ichar)
          iloc = iloc + 1
          !
       case('2') 
          !
          string(iloc:iloc) = string_tmp(ichar:ichar)
          iloc = iloc + 1
          !
       case('3')
          !
          string(iloc:iloc) = string_tmp(ichar:ichar)
          iloc = iloc + 1
          !
       case('4')
          !
          string(iloc:iloc) = string_tmp(ichar:ichar)
          iloc = iloc + 1
          !
       case('5')
          !
          string(iloc:iloc) = string_tmp(ichar:ichar)
          iloc = iloc + 1
          !
       case('6')
          !
          string(iloc:iloc) = string_tmp(ichar:ichar)
          iloc = iloc + 1
          !
       case('7')
          !
          string(iloc:iloc) = string_tmp(ichar:ichar)
          iloc = iloc + 1
          !
       case('8')
          !
          string(iloc:iloc) = string_tmp(ichar:ichar)
          iloc = iloc + 1
          !
       case('9')
          !
          string(iloc:iloc) = string_tmp(ichar:ichar)
          iloc = iloc + 1
          !
       case default
          !
       end select
       !
    end do
    !
    if(debug) then
       !
       write(*,*)trim(string_tmp)
       write(*,*)trim(string)
       !
    end if
    !
  end subroutine remove_signs
  !----------------------------------------------------------------------------------------------------
  ! strip path from file name
  !----------------------------------------------------------------------------------------------------
  subroutine strip_path(string,debug)
    !
    implicit none
    !
    ! input/output variables
    character(128), intent(inout) :: string
    logical, intent(in)           :: debug
    ! local variables
    ! local variables
    integer                     :: ichar
    integer                     :: nchars
    character(128)              :: string_tmp
    integer                     :: iloc
    !
    string_tmp = string
    !
    string = ' '
    !
    nchars = len(trim(string_tmp))
    !
    do iloc = nchars,1,-1
       !
       if(string_tmp(iloc:iloc)=='/') then
          !
          exit
          !
       end if
       !
    end do
    !
    string = string_tmp(iloc+1:nchars)
    !
  end subroutine strip_path
  !----------------------------------------------------------------------------------------------------
  ! routine to find files in the current working directory with a specific file ending
  !----------------------------------------------------------------------------------------------------
  subroutine util_list_files(fext,debug)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in) :: fext
    logical, intent(in)      :: debug
    ! local variables
    character*128            :: cmd
    !
    ! generate command to list files with specified file ending
    write(cmd,'(a,a,a)')'ls -1 *.',trim(fext)
    !
    ! carry out system command
    call system(cmd)
    !
  end subroutine util_list_files
  !----------------------------------------------------------------------------------------------------
  ! routine to find files in the current working directory with a specific file ending and return as
  ! an array list to the calling routine
  !----------------------------------------------------------------------------------------------------
  subroutine util_get_file_list(fext,nfiles,file_list,debug)
    !
    implicit none
    !
    ! input variables
    character(*), intent(in) :: fext
    logical, intent(in)      :: debug
    integer, intent(out)     :: nfiles
    character(128), pointer  :: file_list(:)
    ! local variables
    character*128            :: cmd
    integer                  :: ierr
    character*128            :: dummy_char
    integer                  :: ifile
    character(128), dimension(:), allocatable :: file_list_old
    integer :: nfiles_old
    !
    ! initialize nfiles_old
    nfiles_old = 0
    !
    if(associated(file_list)) then
       !
       nfiles_old = size(file_list)
       !
       allocate(file_list_old(nfiles_old))
       !
       do ifile = 1,nfiles_old
          !
          file_list_old(ifile) = file_list(ifile)
          !
       end do
       !
    end if
    !
    ! generate command to list files with specified file ending
    ! generate command to list files with specified file ending
    write(cmd,'(a,a,a,a)')'ls -1 *.',trim(fext),' > ls.tmp'
    !
    ! carry out system command
    call system(cmd)
    !
    ! initialize number of files
    nfiles = 0
    !
    ! open ls.tmp file and count number of files
    open(unit=333,file='ls.tmp',status='old',iostat=ierr)
    !
    if(ierr.ne.0) then
       !
       write(*,'(a)')trim(hrule_err)
       write(*,'(a,a,a)')'error: failure in util_get_file_list => cannot get list of ',trim(fext),' files'
       write(*,'(a)')trim(hrule_err)
       write(*,'(a)')''
       return
       !
    end if
    !
    ierr = 0
    !
    do while(ierr.eq.0)
       !
       read(333,'(a)',iostat=ierr)dummy_char
       !
       if(ierr.eq.0) nfiles = nfiles + 1
       !
    end do
    !
    ! rewind file
    rewind(333)
    !
    allocate(file_list(nfiles+nfiles_old))
    !
    do ifile = 1,nfiles_old
       !
       file_list(ifile) = file_list_old(ifile)
       !
    end do
    !
    do ifile = 1,nfiles
       !
       read(333,'(a)')file_list(ifile+nfiles_old)
       !
    end do
    !
    close(333)
    !
    ! update nfiles
    nfiles = nfiles+nfiles_old
    !
    ! remove temporary file
    call system('rm -f ls.tmp')
    !
    if(debug) then
       !
       write(*,'(a,a,a,i3)')'number of ',trim(fext),' files detected: ',nfiles
       write(*,*)
       !
       do ifile = 1,nfiles
          !
          write(*,'(a,i3,a,a)')'file (',ifile,'): ',trim(file_list(ifile))
          !
       end do
       !
    end if
    !
  end subroutine util_get_file_list
  !----------------------------------------------------------------------------------------------------
  ! routine to get file sequence
  !----------------------------------------------------------------------------------------------------
  subroutine util_get_file_sequence(nfiles,file_sequence,debug)
    !
    implicit none
    !
    ! input variables
    logical, intent(in)      :: debug
    integer, intent(out)     :: nfiles
    character(128), pointer  :: file_sequence(:)
    ! local variables
    character*128            :: cmd
    integer                  :: ierr
    character*128            :: dummy_char
    integer                  :: ifile
    character*128            :: fname_structure
    integer                  :: inums,inume
    integer                  :: nchar,nchar_dummy
    integer                  :: num_len
    integer                  :: i
    integer                  :: nfiles_tot
    integer                  :: ifile_loc
    !
    write(*,'(a)')''
    write(*,'(a)')'auto-detect file sequence'
    write(*,'(a)')''
    !
    write(*,'(a)')'file name structure'
    write(*,*)
    !
    write(*,'(a)')'examples: '
    write(*,'(a)')'file_name_kc_001_smoothed.dat => file structure: file_name_kc_###_smoothed.dat'
    write(*,'(a)')'file_name_00001.dat => file structure: file_name_#####.dat'
    !
    write(*,*)
    write(*,'(a)')'provide file name structure:'
    !
    read(*,*)fname_structure
    !
    num_len = 0
    !
    inums = 100
    inume = 0
    !
    nchar = len(trim(fname_structure))
    !
    ! check length of number
    do i = 1,len(trim(fname_structure))
       !
       if(fname_structure(i:i).eq.'#') then
          !
          if(i.lt.inums) then
             inums = i
          end if
          !
          if(i.gt.inume) then
             !
             inume = i
             !
          end if
          !
          num_len = num_len + 1
          !
       end if
       !
    end do
    !
    ! debug output
    if(debug) then
       !
       write(*,*)
       write(*,'(a,i10)')'Length of number sequencing: ',num_len
       write(*,'(a,i10)')'Start of number string: ',inums
       write(*,'(a,i10)')'End of number string: ',inume
       !
    end if
    !
    ! generate command to list files with specified file ending
    !write(cmd,'(a,a,a,a,a)')'ls -1 ',trim(fname_structure(1:inums-1)),'*',trim(fname_structure(inume+1:nchar)),' 1> ls.tmp 2> /dev/null'
    write(cmd,'(a,a,a,a,a)')'ls -1 ',trim(fname_structure(1:inums-1)),'*',trim(fname_structure(inume+1:nchar)),' > ls.tmp'
    !
    ! carry out system command
    call system(trim(cmd),ierr)
    !
    if(ierr.ne.0) then
       !
       write(*,*)
       write(*,'(a)')trim(hrule_err)
       write(*,'(a,a)')'error: failure in util_get_file_sequence => cannot get file sequence with structure ',trim(fname_structure)
       write(*,'(a)')'error: abort'
       write(*,'(a)')trim(hrule_err)
       write(*,'(a)')''
       !
       stop
       !
    end if
    !    
    ! initialize number of files
    nfiles = 0
    nfiles_tot = 0
    !
    ! open ls.tmp file and count number of files
    open(unit=333,file='ls.tmp',status='old',iostat=ierr)
    !
    ierr = 0
    !
    do while(ierr.eq.0)
       !
       read(333,'(a)',iostat=ierr)dummy_char
       !
       if(ierr.eq.0) then
          !
          nfiles_tot = nfiles_tot + 1
          !
          ! check if file fits the pattern
          !
          ! compare dummy character string and original string length
          nchar_dummy = len(trim(dummy_char))
          !
          if(nchar_dummy.eq.nchar) then
             !
             nfiles = nfiles + 1
             !
          end if
          !
       end if
       !
    end do
    !
    ! rewind file
    rewind(333)
    !
    allocate(file_sequence(nfiles))
    !
    ifile_loc = 1
    !
    do ifile = 1,nfiles_tot
       !
       read(333,'(a)')dummy_char
       !
       ! compare dummy character string and original string length
       nchar_dummy = len(trim(dummy_char))
       !
       if(nchar_dummy.eq.nchar) then
          !
          file_sequence(ifile_loc)=trim(dummy_char)
          ifile_loc = ifile_loc + 1
          !
       end if
       !
    end do
    !
    close(333)
    !
    ! remove temporary file
    call system('rm -f ls.tmp')
    !
    if(debug) then
       !
       write(*,*)
       write(*,'(a,a,a,i3)')'sequence of ',trim(fname_structure),' files detected: ',nfiles
       write(*,*)
       !
       do ifile = 1,nfiles
          !
          write(*,'(a,i3,a,a)')'file (',ifile,'): ',trim(file_sequence(ifile))
          !
       end do
       !
    end if
    !
  end subroutine util_get_file_sequence
  !----------------------------------------------------------------------------------------------------
  ! subroutine to get directory tree
  !----------------------------------------------------------------------------------------------------
  subroutine util_get_dir_tree(dir_tree,debug)
    !
    implicit none
    !
    ! input variables
    logical, intent(in) :: debug
    character(len=128), pointer :: dir_tree(:)
    ! local variables
    character(len=1024) :: cwd
    integer :: iostat
    integer :: ndirs
    integer :: nchar
    integer :: ichar
    integer, dimension(:), allocatable :: islash
    integer :: idir
    !
    call getcwd(cwd)
    !
    nchar = len(trim(cwd))
    !
    ndirs = 0
    !
    do ichar = 1,nchar
       !
       if(cwd(ichar:ichar)=='/') then
          !
          ndirs = ndirs + 1
          !
       end if
       !
    end do
    !
    allocate(islash(ndirs))
    !
    idir = 1
    !
    do ichar = 1,nchar
       !
       if(cwd(ichar:ichar)=='/') then
          !
          islash(idir) = ichar
          !
          idir = idir + 1
          !
       end if
       !
    end do
    !
    allocate(dir_tree(ndirs))
    !
    do idir = 1,ndirs-1
       !
       dir_tree(idir) = cwd(islash(idir)+1:islash(idir+1)-1)       
       !
    end do
    !
    dir_tree(ndirs) = cwd(islash(ndirs)+1:nchar)
    !
  end subroutine util_get_dir_tree
  !
end module util_mod
