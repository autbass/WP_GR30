module fitroutines
    implicit none 

    contains 
    subroutine least_squares_fit(file_name)

    !Data dictionary
        character(len=24), intent(in) :: file_name
        integer :: ierror=0             ! Status Flag from I/O Messages
        integer :: n = 0                ! Number of input data pairs (x,y)
        REAL :: slope                   ! Slope of the line
        REAL :: sum_x = 0.              ! Sum of all input X Values
        REAL :: sum_x2 = 0.             ! Sum of all input X Values squared
        REAL :: sum_y = 0.              ! Sum of all input Y Values
        REAL :: sum_xy = 0.             ! Sum of all input X*Y Values
        REAL :: x                       ! an input x value
        REAL :: x_bar                   ! Average x value
        REAL :: y                       ! an input y value
        REAL :: y_bar                   ! average y value
        REAL :: y_int                   ! y-axis intercept of the line

        OPEN(UNIT=100, FILE=file_name, STATUS='old', IOSTAT=ierror)
        if (ierror /= 0) print *, "error opening file:", file_name

        DO  
            READ (100, *,IOSTAT=ierror) x,y
            if (ierror /= 0) EXIT 
            n = n + 1 
            sum_x = sum_x + x 
            sum_y = sum_y + y 
            sum_x2 = sum_x2 + x**2
            sum_xy = sum_xy + x*y 
        END DO 

        x_bar = sum_x / real(n)
        y_bar = sum_y / real(n)
        slope = ((sum_xy) - sum_x*y_bar) / (sum_x2 - sum_x * x_bar)
        y_int = y_bar - slope * x_bar 
        !WRITE(*, 1020) slope, y_int, N, 1d0/slope, abs(slope/resistance*1000)
        WRITE(*, 1010) slope, y_int, N

    1010 FORMAT ('Regression coefficients for the least-squares line:',&
        /,' k, = ', F12.3,&
        /,'(d) = ', F12.3,&
        /,'No of points = ', I12)
    end subroutine least_squares_fit
    
end module fitroutines


program brownian_motion
    use fitroutines
    implicit none

    !Data dictionary
    real :: x = 0.      ! x value
    real :: y = 0.      ! y value
    real :: x_sum=0.    ! sum of x values
    real :: y_sum=0.    ! sum of y values
    real :: length      ! norm of (x,y)
    real :: sum_sq      ! quadrate
    real :: distance=0. ! covered distance
    real :: avg_dist=0. ! sum over distances / nr experiments
    real :: varianz=0.
    real :: standardabweichung =0.
    real :: Erwartungswert =0.
    real :: temp = 0.
    integer :: Anz_experimente=100
    integer :: n=1      ! number of 'experiments'
    integer :: steps=50 ! number of steps
    integer :: i,j=0, m=0
    real, dimension(1000) :: distance_record=0.
    character(len=24) :: file_name = "outputbsp2.dat"
    character(len=24) :: file_name2 ="sampletrajectory.dat"
    call RANDOM_SEED()
    OPEN(110, file = file_name)
    OPEN(120, file = file_name2)
    write(*,*) "#      steps ", "Erwartungswert ", "Standardabw."
    do steps=50,1000,50
        do n=1, Anz_experimente !Anz_experimente, 1
            do i=1,steps
                call RANDOM_NUMBER(x)
                call RANDOM_NUMBER(y)
                x=x*2.-1.
                y=y*2.-1.
            !normierung auf Schrittlänge 1
                length=sqrt(x*x+y*y)
                x=x/length
                y=y/length
                x_sum=x_sum+x
                y_sum=y_sum+y
                if(j==0) write (120,*) i, sqrt(x_sum*x_sum+y_sum*y_sum)
            end do
            if(j==0) j=j+1
            !mittelwert + std abweichung.
            distance=sqrt(x_sum*x_sum+y_sum*y_sum)
            distance_record(n)=distance
            distance=0
            x_sum=0
            y_sum=0
        end do 
        sum_sq=dot_product(distance_record, distance_record)!/(real(Anz_experimente))
        Erwartungswert=sum(distance_record)/(real(Anz_experimente))
        varianz=(sum_sq- Erwartungswert**2)/real(Anz_experimente**2)
        standardabweichung=sqrt(varianz)
    print *, steps, Erwartungswert, standardabweichung
    write(110,*) steps, Erwartungswert*Erwartungswert
    distance_record=0.
    end do 

    CLOSE (120)
    CLOSE (110)
    call least_squares_fit(file_name)

end program brownian_motion