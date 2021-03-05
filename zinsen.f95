program zinsen
    implicit none
    double precision, dimension (1:10) :: kapital=0, dev=1
    double precision :: n=1
    integer :: i=0, readerror=0
    character(20) :: file_out='zinsen.dat'

    open(unit=100, file=file_out, status='replace', iostat=readerror)
    if(readerror .NE. 0) print *, 'error opening file'
    !Alles in Einheiten von Startkapital 1
    do i=1,10
        kapital(i)=(1+(1d0/(n)))**(n)
        dev(i)=abs(exp(1.)-kapital(i))
        write(100,*) n, kapital(i), dev(i)
        n=n*10
    end do

    close(unit=100,iostat=readerror)
    if(readerror .NE. 0) print *, 'error closing file'
end program zinsen