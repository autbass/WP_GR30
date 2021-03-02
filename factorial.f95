!Plan:
!Zuerst recursive function schreiben welche im weiteren Programm aufgerufen werden kann
!diese soll faktorielle berechnen, da fortran Berechnung nativ nicht unterstützt. Als Modul & dann einbinden ins hauptprogramm
!Hauptprogramm selbst lediglich simples abfragen nach Input, funktionsaufruf, output.

module faktorielle_modul
    implicit none

contains
recursive integer(kind=16) function faktorielle(i) & !function als recursive deklariert
result (answer)
implicit none
!intent(in) damit für übergebene Variablen keine typumwandlung dr funktion erfolgt
integer(kind=16), intent(in) :: i 
if(i==0) then
    answer = 1
else
    answer=i*faktorielle(i-1)
end if
end function faktorielle
end module faktorielle_modul


program main
    use faktorielle_modul
    implicit none
    integer(kind=16)     :: j=0, i=1, m=0
    do while (faktorielle(i)<faktorielle(i+1))
        i=i+1
        j=faktorielle(i)
    end do

    write(*,*) 'Faktorielle Berechnung, max. sinnvoller Wert N_max:', i
    write(*,*) 'benutzte Genauigkeit', kind(i), 'byte Integer'
    print *, ' positive ganze Zahl eingeben (integer)'
    read *, m
    do while (m<0 .OR. m>i)
        print *, 'Zahl außerhalb des zulässigen Wertebereichs, bitte Zahl eingeben:'
        read *, m
    end do
    j=0
    j = faktorielle(m)
    print *, m, '! =', j



end program main



