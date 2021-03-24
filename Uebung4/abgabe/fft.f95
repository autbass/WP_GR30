
! Compile with: gfortran -o fft.exe fft.f95 -I/usr/include -lfftw3
module filehandler
    implicit none

    contains 
        subroutine read_binary_int2(filename, array, arraysize, ElementeimFile)
            character(len=24), intent(in) :: filename
            integer :: arraysize
            integer ::ElementeimFile
            integer*2::sample
            double precision, dimension(arraysize) :: array
            double precision :: dB_maxvalue=32768
            integer :: i=0,cnt
            integer:: ioerror=0
            open(1, file=filename, FORM='UNFORMATTED',access='direct', recl=2, iostat=ioerror)
            i=1
            if (ioerror /= 0) print *, "Error opening input file:", filename
            do
                read(1,rec=i,err=100) sample
                array(i)=dble(sample)/dB_maxvalue
                
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
        subroutine print_fourier_to_file(out, arraysize, filename,freq_interval)
            integer:: arraysize, toelementN, i=1
            integer:: ioerror=0
            double precision :: dB_maxvalue=32768
            double complex, dimension(arraysize/2+1) :: out
            character(len=250), intent(inout) :: filename
            double precision :: freq_interval, fact=1d0/(2d0*acos(-1.0d0))
            
            !Renormierung, factor 1/2pi drauf.
            out=out*fact 

            open(2, file=filename, iostat=ioerror)
            if (ioerror /= 0) print *, "Error opening output file"
            do i=1,arraysize/2+1
                write(2,*) freq_interval*i, 20d0*log10((ABS(out(i)))/dB_maxvalue) !20d0*log10((ABS(out(i)))/(dB_maxvalue))
            end do
        end subroutine

    end module filehandler

    ! Compile with: gfortran -o fft.exe fft.f95 -I/usr/include -lfftw3
program main
    use filehandler
    implicit none
    include "fftw3.f"

    character(len=250)::filename=""
    character(len=250):: outputfile="fourier_out.dat"
    integer, parameter :: arraysize=450000
    integer :: i=0, ElementeimFile=1
    double precision, dimension(arraysize) :: array=0d0
    integer*2, parameter :: sample_rate = 22050 !1s, entspricht Hz
    Double precision :: Freq_Min=1d0/dble(sample_rate), Freq_max=11025d0, Dauer_file=1,freqIntervall=0
    double complex, dimension(arraysize/2+1) :: out =0d0
    

    integer*8 :: fourier_plan=0
    call get_command_argument(1, filename)
    call get_command_argument(2, outputfile)
    if (filename == "") then
        write(*,*)"Specify a filename! .q for exiting, if you forgot to ls first.."

        read(*,*) filename
        if(filename ==".q") stop
        !stop
    endif

    !reads binary data from filename into array, also returns number of Elements in the file.
    call read_binary_int2(filename, array, arraysize, ElementeimFile)
    
    ! prints the first N (10 in this case) Elements of the binary file. Used to check if it worked.
    ! call print_to_screen(array, arraysize, 10)
    
    ! Fourier subroutines from FFTW3
    call dfftw_plan_dft_r2c_1d(fourier_plan,arraysize,array,out,FFTW_ESTIMATE)
    call dfftw_execute_dft_r2c(fourier_plan, array, out)
    call dfftw_destroy_plan(fourier_plan)
    !print *, ElementeimFile !Funktioniert.arraysize/2+1

    ! calculate the Frequency intervals then used to output my data.
    freqIntervall=(Freq_max-freq_min)/(arraysize/2+1)    
    Dauer_file= dble(ElementeimFile)/dble(sample_rate)

    !Output subroutine
    call print_fourier_to_file(out, arraysize, outputfile,freqIntervall)

    write(*,1010) filename, outputfile, ElementeimFile, Dauer_file, sample_rate, freqIntervall*(arraysize/2+1)

1010 FORMAT ('Ein paar Infos zum durchgefuehrten:',&
        /,'Filename Input:', A15,&
        /,'Filename Output:', A25 ,&
        /,'Vorhandene Elemente im File', I12,&
        /,'Dauer des Audios in Sekunden', F12.3,&
        /,'Sample_rate', I12,&
        /,'Nyquist Frequenz', F12.0, &
        /,'Outputfile ausgegeben in dBFS')
end program
