module filehandler
    implicit none

    contains 
        subroutine read_binary_int2(filename, array, arraysize, ElementeimFile)
            character(len=24), intent(in) :: filename
            integer :: arraysize
            integer ::ElementeimFile
            integer*2::sample
            double precision, dimension(arraysize) :: array
            integer :: i=0,cnt
            integer:: ioerror=0
            open(1, file=filename, FORM='UNFORMATTED',access='direct', recl=2, iostat=ioerror)
            i=1
            print *, "iostat, Filename:",ioerror, filename
            do
                read(1,rec=i,err=100) sample
                array(i)=dble(sample)
                
                ! write(*,*)i, array(i)
                i=i+1
                ElementeimFile=i
            end do
        100 return
        end subroutine

        subroutine print_to_screen(array, arraysize, toelementN)
            integer:: arraysize, toelementN, i=1
            double precision, dimension(arraysize) :: array
            
            do while (i<=toelementN)
                write(*,*) i, array(i)
                i=i+1 
                if (i==arraysize) return 

            end do 
            return 
        end subroutine


    end module filehandler

    ! Compile with: gfortran -o fft.exe fft.f95 -I/usr/include -lfftw3
program main
    use filehandler
    implicit none
    include "fftw3.f"
    !Real-input (r2c) DFT plans should use use dfftw_execute_dft_r2c,
    character(len=250)::filename=""
    integer, parameter :: arraysize=450000
    integer :: i=0, ElementeimFile=1
    double precision, dimension(arraysize) :: array=0d0
    integer*2, parameter :: sample_rate = 22050 !1s, entspricht Hz
    Double precision :: Freq_Min=1d0/dble(sample_rate), Freq_max=1, Dauer_file=1
    double complex, dimension(arraysize/2+1) :: out =0d0

    integer*8 :: fourier_plan=0
    call get_command_argument(1, filename)
    if (filename == "") then
       write(*,*)"Specify a filename!"
       read(*,*) filename
       !stop
    endif
    write(*,*)"# ",filename


    call read_binary_int2(filename, array, arraysize, ElementeimFile)

   ! call print_to_screen(array, arraysize, 10)
    call dfftw_plan_dft_r2c_1d(fourier_plan,arraysize,array,out,FFTW_ESTIMATE)
    call dfftw_execute_dft_r2c(fourier_plan, array, out)
    call dfftw_destroy_plan(fourier_plan)
    !print *, ElementeimFile !Funktioniert.arraysize/2+1


    !i durch frequenz ersetzen..  Nyquist is 22050 / 2 also 11025, min is freq_min + 11025/ElementeimFile
    do i=1,arraysize/2+1
         write(2,*) i, freq_min+dble(i)/4d0*(dble(sample_rate)/dble(ElementeimFile)), ZABS(out(i))*ZABS(out(i))
    end do
    Dauer_file= dble(ElementeimFile)/dble(sample_rate)
    Freq_max=1d0/Dauer_file

    print *, ElementeimFile, Dauer_file,"s", Freq_min, Freq_max
end program
!
!To transform a one-dimensional real array in Fortran, you might do:
!
!        double precision in
!        dimension in(N)
!        double complex out
!        dimension out(N/2 + 1)
!        integer*8 plan
!
!        call dfftw_plan_dft_r2c_1d(plan,N,in,out,FFTW_ESTIMATE)
!        call dfftw_execute_dft_r2c(plan, in, out)
!        call dfftw_destroy_plan(plan)
