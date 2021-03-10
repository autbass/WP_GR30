!Legendre polynome vom grad n berechnen. 
!
!Aufgabe 1
module polinomials_module
    implicit none
    contains
        double precision function legendre(n,x)
        integer :: n, k=0
        double precision :: x
        double precision, dimension (0:n) :: pln

        pln(0)=1
        pln(1)=x

        if (n<=1) then 
            legendre=pln(n)
        else
            do k=2,n
                pln(k)=((2.d0*dble(k)-1.d0)*x*pln(k-1)-(dble(k)-1.d0)*pln(k-2))/dble(k)   
            end do
            legendre=pln(n)
        end if
        end function legendre

        double precision function legendre_ableitung(n,x)
            integer :: n, m=1, k=0
            double precision :: x 
            double precision, dimension (0:n) :: pln
            pln(0)=0
            pln(1)=1
            
            if (n<=1) then 
                legendre_ableitung=pln(n)
            else 
                do k=2, n 
                    pln(k)=((2.0*dble(k))*x*pln(k-1)-(dble(n)-2)*(pln(k-2)))/(dble(n)-dble(m))
                end do
                legendre_ableitung=pln(n)
            end if
            
        end function legendre_ableitung
end module polinomials_module

module calc
    use polinomials_module
    implicit none
    !Aufgabe 2
    contains
    subroutine legnewton(m,n,k,e,x0)
        integer :: m,n, k
        double precision :: e, x0
        n=0
        !abs(legendre(k,x0))>e .AND.
        do while (n<m .AND. abs(legendre(k,x0))>e)
            ! print*, n, x0, legendre(k,x0)
            x0=x0-legendre(k,x0)/(legendre_ableitung(k,x0))
            n=n+1
        end do
    end subroutine legnewton

!Aufgabe 3
    subroutine legintegrate(n,m,ergebnis, xmin, xmax, step, steps)
        double precision, intent(out) :: ergebnis
        double precision, intent(in) :: xmin, xmax, step
        integer, intent(in)          :: n,m,steps
        integer                      :: k=1
        double precision             :: x=0
        double precision, dimension(1:steps) :: poly
        x=xmin
        ergebnis=0
        do k=1,steps-1
            poly(k) = legendre(n,xmin+(k-1)*step)*legendre(m,xmin+(k-1)*step)
        end do

        ergebnis=ergebnis+poly(1)+poly(steps-1)
        !integral=(step/2)*(f(n=1)+f(n=steps)+2*f(2<n<steps))
        do k=2,(steps-3)
            ergebnis=ergebnis+2*poly(k)
            !write(*,*) k, ergebnis
        end do
        ergebnis=ergebnis*(step/(2.d0))
        !write(*,*) "ergebnis subroutine, n, m", ergebnis,n,m
    end subroutine legintegrate


end module calc



program legendre_main
    use polinomials_module
    use calc
    implicit none
    integer :: n = 0, m=0, i=1, total = 0                            !Laufinteger. total benutzt um arrays anzup.f. unterschiedliche step
    integer, parameter :: nmax = 10                             !Legendre Polynome bis grad N berechnen
    double precision, allocatable, dimension(:,:) :: pn         !2dim array für alle Legendre werte
    double precision, allocatable, dimension(:)   :: pn_Abl     !1dim array für Ableitung
    double precision, parameter :: xmin=-1.d0, xmax=1.d0              !Grenzen
    double precision            :: x=0
    double precision, parameter :: step=.01                     !Schrittweite
    !newton Parameter
    double precision, parameter :: abbruchbed = .5d-3            !abbruch bei:
    integer, parameter          :: maxiterationen = 50          !schritte newtonverfahren
    integer, parameter          :: runs = 100000                !anzahl guesses startwerte
    double precision            :: guess

    !integral result
    double precision            :: integral
    
    x=xmin
    
    !Schrittzähler für array allocation
    do while (x<xmax+step)
        total=total+1
        x=x+step
    end do
    allocate(pn(0:nmax,0:total))
    allocate(pn_Abl(1:total))

    !damit legendre 0 und 1 auch leicht ansprechbar im ausgabe array stehen.
        do while (x<xmax+step)
            pn(n-1,i)=1
            pn(n,i)=x
            i=i+1
            x=x+step
        end do
    x=xmin
    i=0
    
    !Berechnung legendre polynome 
    do n=0,nmax
        do while (x<xmax+step)
            pn(n,i)=legendre(n,x)
            
            !write(1,*) x, pn
            x=x+step
            i=i+1
        end do
        x=xmin
        i=0
    end do

    !Write out legendre polinomials format: x, 0th, 1st, 2nd, 3rd ... with implicit do loop
    do while (x<xmax+step)
        write(1,*) x,(pn(n,i), n=0, nmax)
        i=i+1
        x=x+step
    end do
    x=xmin
    i=0

    !calculate derivative of legendre polinomial 9 to be used with newton raphson. 
    do while (x<xmax+step)
        pn_Abl=legendre_ableitung(9,x)
        !write(2,*) x, pn_Abl(i), pn(9,i)
        x=x+step
        i=i+1
    end do
    i=0
    
    n=0
    !guess random no. 0-1, shift to -1<x<1, call newton, find roots. 
    do i=1,runs
        CALL RANDOM_NUMBER(guess)
        guess=(guess*2.d0)-1.d0
        call legnewton(maxiterationen,n,9,abbruchbed,guess)
        if (n .NE. maxiterationen .AND. (guess>xmin) .AND. guess<xmax) write(2,*) guess, legendre(9,guess), "run/i", i, n
    end do

    x=xmin
    write(*,*) "Grad der legendrepolynome welche miteinand multipliziert+zw. -1,1 integriert werden sollen"
    write(*,*) "2 ganze Zahlen eingeben"
    read(*,*) n
    read(*,*) m
    call legintegrate(n,m, integral, xmin, xmax, step, total)
    print*, "Produktintegral über legendre funktion grad n,m:"
    if (abs(integral) <.01d0) then
        write(*,*) "Ergebnis kleiner 10^-2, kompatibel mit 0"
    else 
        write(*,*) integral
    end if
    

    !Prüfen ob routine funktioniert. Ja!
    !do n=1,nmax
    !    do m = 1, nmax
    !    call legintegrate(n,m, integral, xmin, xmax, step, total)
    !    if(integral >.01d0)write(*,*) n,m, integral
    !    end do
    !end do

    deallocate (pn)
    deallocate (pn_Abl)
end program legendre_main

