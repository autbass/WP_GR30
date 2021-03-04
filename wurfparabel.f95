! Einlesen von File
! Differenzenquotienten berechnen für Geschwindigkeit
! alles doubles
! überlegen ob 1 array zum abspeichern oder gesonderte für (x, y, t_i), (v_x, v_y)

!Plan:
!fürs Einlesen: nach Chivers & Sleight, Introduction to Programming with Fortran Abschnitt 22.3
!nicht von vornherein Array festlegen (weiß zwar 197 entries, tun wir mal so als wüssten wir's nicht)
!Formatierung der Zahlen nehm ich aber als gegeben an. 
!Könnte es einfach statisch anlegen, netter wäre aber
!linked-list -> schauen wie viele Einträge im File sind
!Array alloziieren mit entsprechender Größe
!Daten ins Array kopieren.
!Einlesen der Datei. 

!Modul für linked-liste.
!link beinhaltet 2 Komponenten: Zahl(in dem Fall 2 Zahlen x,y) und pointer zu nächster
module link_module
    type link
        double precision :: n,m
        type (link), pointer :: next => null() !intrinsic null() to provide initial value for next
    end type link
end module link_module


program wurfparabel
    use link_module
    implicit none
    type (link), pointer :: root, current   !Variablen welche nur auf link types pointen darf
    integer :: io_stat_number = 0          !return von iostat, ob korrekt geöffnet wurde
    integer :: i = 0, m                     !Laufvariablen
    double precision, allocatable, dimension(:) :: &
            x,y,vx,vy,t
    integer, parameter :: n=1000            ! Anzahl einzulesender Zeilen
    integer :: n_max=0                      ! de-facto Anzahl an Zeilen im File
    
    character (len=80) :: file_name = &           !filename/directory im header änderbar
            'flug.dat'
    character (len=80) :: file_out = &             !outputfilename/directory änderbar. falls pfad länger als 20 Zeichen erweitern!
            'result.dat'
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!    Programmabschnitt: Bestimmung wieviele Zeilen File hat. !!!!
!!!!    Array alloziieren, Elemente aus Linked-list in Array    !!!!
!!!!    kopieren                                                !!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    open(unit=100, file=file_name, status='old', action='read', iostat=io_stat_number)
    
    !if-Schleife prüft ob File richtig geöffnet wurde Format anweisung für write später einfügen.
    if (io_stat_number == 0) then
        write (*,*) file_name, 'opened without any errors'
    else
        write(*,*) 'Could not open file:', file_name, 'please check if given path is correct.'
    end if

    ! Speicher für root alloziieren
    allocate(root)
    ! Erste Zahl einlesen, wenn nicht letzte, nächste Zahl einlesen
    read (unit=100,fmt=*, iostat=io_stat_number) root%n, root%m
    if (io_stat_number==0) then !Nicht am File-Ende
        i=i+1
        allocate (root%next)
    end if
    current => root !Zurück zum Listenanfang.
    !restliche Zahlen einlesen.

    do while (associated(current%next))
        current => current%next
        read (unit=100, fmt=*, iostat=io_stat_number) &
            current%n, current %m
        if (io_stat_number==0) then
            i=i+1
            allocate (current%next)
        end if
    end do
    !I sollte jetzt hoffentlich die richtige Anzahl an Zeilen ausgeben.
    m=i
    write(*,*) 'Anzahl an Zeilen:', m
    ! Jetzt kann ich meine ganzen Arrays mit der richtigen Anzahl an Zeilen alloziieren.
    allocate (x(1:m), y(1:m),vx(1:m),vy(1:m),t(1:m))
    !laufindex resetten
    i=1
    !Nun habe ich meine Arrays, muss Elemente welche ich in die linked-List eingelesen habe 
    !frisch alloziierte Array kopieren, daher pointer wieder an anfang der Linked list setzen
    current => root
    do while(associated(current%next))
        t(i)=i*10d-2
        x(i)=current%n 
        y(i)=current%m
        i=i+1
        current=current%next
    end do
    !Zum testen ob read funktioniert hat. Ja!
    !do i=1,m 
    !    write(*,*) t(i), x(i),y(i)
    !end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    
    


end program wurfparabel