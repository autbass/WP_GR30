program least_squares_fit
    implicit none

!Data dictionary
    character(len=24) :: file_name='fort.1'
    integer :: ierror               ! Status Flag from I/O Messages
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
    REAL :: resistance = 0.47       !Wiederstand in 1000k Ohm -> nF
    OPEN(UNIT=100, FILE=file_name, STATUS='old', IOSTAT=ierror)
    if (ierror /= 0) print *, "error opening file:", file_name

    DO
        READ (100, *,IOSTAT=ierror) x,y
        if (ierror /= 0) EXIT 
        n = n + 1 
        y=log(y)
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
    WRITE(*, 1010) slope, y_int, N!, 1d0/slope, abs(slope/resistance*1000)

1010 FORMAT ('Regression coefficients for the least-squares line:',&
    /,' k, = ', F12.3,&
    /,'(d) = ', F12.3,&
    /,'No of points = ', I12)

1020 FORMAT ('Regression coefficients for the least-squares line:',&
    /,' k, Entl. pro µs, slope (1/tau) = ', F12.3,&
    /,' Anfangsspannung V_0, (d) = ', F12.3,&
    /,' No of points = ', I12,&
    /,' tau = ', F12.3,& 
    /,' Capacity C in nF = ', G12.3)
    
end program least_squares_fit
