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
end module polinomials_module


program legendre_main
    use polinomials_module
    implicit none
    integer :: n = 0, i=0, total = 0
    integer, parameter :: nmax = 10
    double precision, allocatable, dimension(:,:) :: pn
    double precision :: xmin=-1.d0, xmax=1.d0, x=0
    double precision, parameter :: step=.01
    x=xmin
    
    do while (x<xmax+step)
        total=total+1
        x=x+step
    end do
    allocate(pn(-1:nmax,0:total))
    
    !x=xmin
    !do while (x<xmax+step)
    !    pn(-1,i)=legendre(0,x)
    !    pn(0,i)=legendre(1,x)
        !print *, x,pn(-1,i), pn(0,i)
    !    i=i+1
    !    x=x+step
        
    !end do
    !x=xmin
    !i=0
    
        do while (x<xmax+step)
            pn(n,i)=x
            i=i+1
            x=x+step
        end do
    x=xmin
    
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

    do while (x<xmax+step)
        write(1,*) x,(pn(n,i), n=0, nmax)
        i=i+1
        x=x+step
    end do
    

    deallocate (pn)
end program legendre_main

