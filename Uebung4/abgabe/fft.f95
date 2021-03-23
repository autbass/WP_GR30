module filehandler
    implicit none

    contains 
        subroutine read_binary_int2(filename, array, arraysize, ElementeimFile)
            character(len=24), intent(in) :: filename
            integer :: arraysize
            integer, intent(out)::ElementeimFile
            integer*2, dimension(arraysize) :: array
            integer :: i,cnt
            open(1, file=filename, FORM='UNFORMATTED',access='direct', recl=2)
            i=1
            ElementeimFile=1
            do
                read(1,rec=i,err=100) array(i)
                ElementeimFile=ElementeimFile+1
                ! write(*,*)i, array(i)
            i=i+1
            end do
        100 return
        end subroutine

        subroutine print_to_screen(array, arraysize, toelementN)
            integer:: arraysize, toelementN, i=1
            integer*2, dimension(arraysize) :: array
            
            do while (i<=toelementN)
                write(*,*) i, array(i)
                i=i+1 
                if (i==arraysize) return 

            end do 
            return 
        end subroutine


    end module filehandler

program main
    use filehandler
    implicit none
    character(len=24)::filename="stimmgabel1.raw"
    integer     :: arraysize=450000,i=0, ElementeimFile=0
    integer*2, dimension(450000) :: array
    integer*2 :: sample_rate = 22050 !1s, entspricht Hz


    call read_binary_int2(filename, array, arraysize, ElementeimFile)
    call print_to_screen(array, arraysize, 10)
    !print *, ElementeimFile !Funktioniert.

    
end program

