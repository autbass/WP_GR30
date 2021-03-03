!Plan:
!Zuerst recursive function schreiben welche im weiteren Programm aufgerufen werden kann
!diese soll faktorielle berechnen, da Fortran Berechnung nativ nicht unterstützt. Als Modul & dann einbinden ins hauptprogramm
!Hauptprogramm selbst lediglich simples abfragen nach Input, funktionsaufruf, output. ebenfalls N_max berechnung.
!
!Integer arten in eigenem Modul bestimmen
!faktorielle Modul mit Inteface ausstatten welches je nach Genauigkeit der übergebenen INT variable aussucht
!Berechnung benutzt.
!Sicher nicht most straightforward option, 
!wollte module nutzen versuchen, und interface struktur für unterschiedliche Integer typen.

!unterschiedliche Integer-Arten in externem Modul siehe Chivers-Sleightholme 2015
!Introd. to Fortran

module integer_kind_module
    implicit none
    integer, parameter :: i8 &
                = selected_int_kind( 2)     !(1 byte)
    integer, parameter :: i16 &
                = selected_int_kind( 4)     !(2 bytes)
    integer, parameter :: i32 &
                = selected_int_kind( 9)     !(4 bytes)
    integer, parameter :: i64 &             !(kind 8)
                = selected_int_kind(18)  
    integer, parameter :: i128 &            !(kind 16)
                = selected_int_kind(32)
end module integer_kind_module

module faktorielle_modul
    use integer_kind_module
    implicit none
    !interface über faktorielle. faktorielle aufrufe über faktorielle(i), je nach i wird andere procedure verwendet.
    interface faktorielle
        module procedure faktorielle_integer_128
        module procedure faktorielle_integer_64
        module procedure faktorielle_integer_32
        module procedure faktorielle_integer_16
        module procedure faktorielle_integer_8
    end interface faktorielle

!   Je nachdem welche Genauigkeit das übergebene 'i' hat wird entsprechende Funktion berechnet.
!   Hier werden die ganzen oben genannten module spezifiziert. entscheiden für die Auswahl des Moduls,
!   ist die deklaration des i dummies. 
    contains
        recursive integer(i128) function faktorielle_integer_128(i) & !function als recursive deklariert
        result (answer)
        implicit none
        !intent(in) damit für übergebene Variablen keine typumwandlung dr funktion erfolgt
        integer(i128), intent(in) :: i 
        if(i==0) then
            answer = 1
        else
            answer=i*faktorielle(i-1)
        end if
        !print *, 'Berechnung der faktoriellen mit', kind(i)
    end function faktorielle_integer_128

    recursive integer(i64) function faktorielle_integer_64(i) & !function als recursive deklariert
        result (answer)
        implicit none
        !intent(in) damit für übergebene Variablen keine typumwandlung dr funktion erfolgt
        integer(i64), intent(in) :: i 
        if(i==0) then
            answer = 1
        else
            answer=i*faktorielle(i-1)
        end if
        !print *, 'Berechnung der faktoriellen mit', kind(i) 
        !testzeile ob richtige routine verwendet wird
    end function faktorielle_integer_64

    recursive integer(i32) function faktorielle_integer_32(i) & !function als recursive deklariert
        result (answer)
        implicit none
        !intent(in) damit für übergebene Variablen keine typumwandlung dr funktion erfolgt
        integer(i32), intent(in) :: i 
        if(i==0) then
            answer = 1
        else
            answer=i*faktorielle(i-1)
        end if
    end function faktorielle_integer_32

    recursive integer(i16) function faktorielle_integer_16(i) & !function als recursive deklariert
        result (answer)
        implicit none
        !intent(in) damit für übergebene Variablen keine typumwandlung dr funktion erfolgt
        integer(i16), intent(in) :: i 
        if(i==0) then
            answer = 1
        else
            answer=i*faktorielle(i-1)
        end if
    end function faktorielle_integer_16

    recursive integer(i8) function faktorielle_integer_8(i) & !function als recursive deklariert
    result (answer)
    implicit none
    !intent(in) damit für übergebene Variablen keine typumwandlung dr funktion erfolgt
    integer(i8), intent(in) :: i 
    if(i==0) then
        answer = 1
    else
        answer=i*faktorielle(i-1)
    end if
    end function faktorielle_integer_8
end module faktorielle_modul


program main
    use integer_kind_module
    use faktorielle_modul
    implicit none
    integer(i128)     :: j=0, i=1, m=0
    
    integer(i128)    :: int128=1, jnt128=0
    integer(i64)     :: int64=1, jnt64=0
    integer(i32)     :: int32=1, jnt32=0
    integer(i16)     :: int16=1, jnt16=0
    integer(i8)      :: int8=1, jnt8=0

    !Berechnung maximal erlaubter Werte für unterschiedliche Integer-arten.
    !Wenn Wertebereich überschritten wird folgt negativer Wert für i+1 -> > i, Maximal darstellbar: i
    !faktorielle ruft Modul faktorielle_modul auf, interface beinhaltet Routinen für alle
    !unterschiedlichen integer-arten welche im integer_kind_module bestimmt wurden
    !je nachdem welche integer-art das übergebene Argument i in faktorielle(i) hat,
    !wird die entsprechende rekursive funktion zur Berechnung gewählt.

    do while (faktorielle(i)<faktorielle(i+1))
        i=i+1
        j=faktorielle(i)
    end do

    do while (faktorielle(int128)<faktorielle(int128+1))
        int128=int128+1
        jnt128=faktorielle(int128)
        !print *, int128, jnt128
    end do

    do while (faktorielle(int64)<faktorielle(int64+1))
        int64=int64+1
        jnt64=faktorielle(int64)
        !print *, int64, jnt64
    end do

    do while (faktorielle(int32)<faktorielle(int32+1))
        int32=int32+1
        jnt32=faktorielle(int32)
        !print *, int32, jnt32
    end do

    do while (faktorielle(int16)<faktorielle(int16+1) .AND. faktorielle(int16)>0)
        int16=int16+1
        jnt16=faktorielle(int16)
        !print *, int16, jnt16
    end do
        int16=int16-1
    !N_max Ergebnisse, Eingabeaufforderung, einlesen, Ergebnis ausgeben.
    write(*,100) 'Faktorielle Berechnung, maximal darstellbare Zahl N_max! für unterschiedliche Genauigkeiten:'
    write(*,100) 'Bei benutzter Genauigkeit', kind(int16), 'byte Integer, N_max=', int16
    write(*,100) 'Bei benutzter Genauigkeit', kind(int32), 'byte Integer, N_max=', int32
    write(*,100) 'Bei benutzter Genauigkeit', kind(int64), 'byte Integer, N_max=', int64
    write(*,100) 'Bei benutzter Genauigkeit', kind(int128), 'byte Integer, N_max=', int128
    
    write(*,200) ' positive ganze Zahl eingeben (integer) zwischen 0-', i
    read *, m

    ! Abfangen von unzulässigen ganzzahligen Werten. Könnte man auf non-int ausweiten. Gibt aktuell Fehler.
    do while (m<0 .OR. m>i)
        print *, 'Zahl außerhalb des zulässigen Wertebereichs, bitte Zahl eingeben:'
        read *, m
    end do
    j=0
    j = faktorielle(m)
    write(*,300) m, '! =', j

100 format (a,' ', i4, a,' ', i2) !allg. Formatierung.
200 format (a, i2)         !Eingabeaufforderung
300 format (i2, a, i38)    !output, i128 ziemlich lang, daher mal i38 gewählt.

end program main