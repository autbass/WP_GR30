program brownian_motion
    implicit none

    !Data dictionary
    real :: x = 0.       ! x value
    real :: y = 0.      ! y value
    real :: x_sum=0.       ! sum of x values
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
    integer :: n=1        ! number of 'experiments'
    integer :: steps=50   ! number of steps
    integer :: i,j=0, m=0
    real, dimension(1000) :: distance_record=0.
    call RANDOM_SEED()
    
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
                if(j==0) write (3,*) i, sqrt(x_sum*x_sum+y_sum*y_sum)
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
        !print *, Erwartungswert, varianz, standardabweichung
        !print *, distance_record
    !length=avg_dist/real(Anz_experimente)
    print *, steps, Erwartungswert, standardabweichung
    write(1,*) steps, Erwartungswert*Erwartungswert, standardabweichung
    distance_record=0.
    end do 
end program brownian_motion