module precision_module
    implicit none
    integer, parameter :: sp & !single precision
                = selected_real_kind(6,37)
    integer, parameter :: dp & !double precision
                =selected_real_kind(15,307)
    integer, parameter :: qp & !quad precision
                =selected_real_kind(30,291)
end module precision_module
    
module maths_module
    use precision_module, wp => qp !rename-list, working precision -> dp
    !wp then used in all type declarations.
    implicit none
    real (wp), parameter :: pi =  &
                3.141592653589793238462643383279502884_wp
end module maths_module

module sub1_area_module
    implicit none

    contains
        subroutine kreisrechner(radius, area, circumference)

            use precision_module, wp => qp
            use maths_module
            implicit none
            real (wp), intent(in) :: radius
            real (wp), intent(out):: area, circumference

            area=pi*radius*radius
            circumference = 2.0_wp*pi*radius
        end subroutine kreisrechner

        subroutine kreisapprox(n)
            use precision_module, wp => qp
            use maths_module
            implicit none
            real (wp), intent(inout):: n
            !print *, n
            n=sqrt(.5_wp*(1_wp-sqrt(1_wp-n*n)))
           
        end subroutine kreisapprox

        end module sub1_area_module


program main

    use precision_module, wp => qp
    use maths_module
    use sub1_area_module

    implicit none

    real(wp) :: r=.5,area=0,circumference=0
    integer :: n=0, nmax=1000 !laufindex
    integer :: ecken = 4, eckmin=0 !Anzahl Ecken
    real(wp) :: s=0, smin=0, delta_a=0
    !beginn mit eingeschriebenen quadrat, kreis mit r=1/2 = halbe diagonale
    !eine Seitenlänge sqrt((1/2)^2+(1/2)^2)
    s=sqrt(r)
    delta_a=pi-4*s
    !print *, 'Umfang anfangs eingeschriebens Quadrat', s*ecken
    call kreisrechner(r,area,circumference)
    
    do n=1,nmax
        call kreisapprox(s)
        ecken=ecken*2
        if (abs(pi-s*ecken)<delta_a) then
            delta_a=pi-s*ecken
            smin=s
            eckmin=ecken
        end if
        !print *, s, ecken, s*ecken, abs(pi-s*ecken), 'working precision:', wp, 'byte'
    end do
    print *,'kleinste Abweichung von', delta_a, 'bei ', eckmin, '-eck', 'Saitenlänge: ', smin, 'Ergebnis: ', smin*eckmin 
end program main

