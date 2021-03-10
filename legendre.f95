!Legendre polynome vom grad n berechnen. 
!
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

module newton_calc
    use polinomials_module
    implicit none
    
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
end module newton_calc



program legendre_main
    use polinomials_module
    use newton_calc
    implicit none
    integer :: n = 0, i=1, total = 0                            !Laufinteger. total benutzt um arrays anzup.f. unterschiedliche step
    integer, parameter :: nmax = 10                             !Legendre Polynome bis grad N berechnen
    double precision, allocatable, dimension(:,:) :: pn         !2dim array für alle Legendre werte
    double precision, allocatable, dimension(:)   :: pn_Abl     !1dim array für Ableitung
    double precision :: xmin=-1.d0, xmax=1.d0, x=0              !Grenzen, Laufindex
    double precision, parameter :: step=.01                     !Schrittweite
    !newton Parameter
    double precision, parameter :: abbruchbed = 1d-4            !abbruch bei:
    integer, parameter          :: maxiterationen = 50          !schritte newtonverfahren
    integer, parameter          :: runs = 100000                !anzahl guesses startwerte
    double precision            :: guess
    
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
        !write(1,*) x,(pn(n,i), n=0, nmax)
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
    x=xmin
    n=0
    !guess random no. 0-1, shift to -1<x<1, call newton, find roots. 
    do i=1,runs
        CALL RANDOM_NUMBER(guess)
        guess=(guess*2.d0)-1.d0
        call legnewton(maxiterationen,n,9,abbruchbed,guess)
        if (n .NE. maxiterationen .AND. (guess>xmin) .AND. guess<xmax) write(2,*) guess, legendre(9,guess), "run/i", i, n
    end do


    deallocate (pn)
    deallocate (pn_Abl)
end program legendre_main

