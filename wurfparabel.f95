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
    integer :: io_stat_number = 0           !return von iostat, 0.. Alles ok, negativ für End of file/end of record, probleme aller art.
    integer :: i = 0, m                     !i... Laufvariable, m... Anzahl der Zeilen/Einträge
    double precision, allocatable, dimension(:) :: &
            x,y,vx,vy,t                     ! meine ganzen 1er-Arrays, wobei Größe erst im Programm festgelegt wird, dr. allocate(x(1:m))
    double precision :: deltat = 0.001d1, &       !einzelner Zeitschritt
                        x_max=0, y_max=0, &  
                        bahnlaenge=0           
    double precision, dimension(1) :: i_max=0      !Für Zeile mit Maximum. i_max=minloc(array,mask array>0) gibt mir zeile wo maximum ist.

    character (len=80) :: file_name = &           !filename/directory hier änderbar
            'flug.dat'
    character (len=80) :: file_out = &             !outputfilename/directory hier änderbar.
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
    !für den Umgang mit meinen oben bestimmten type link hat Elemente root%n, root%m, root%next
    !n, m für Zahlen, next als pointer aufs nächste Element.
    ! Speicher für root alloziieren
    allocate(root)
    ! Erste Zahl einlesen, wenn nicht letzte, nächste Zahl einlesen
    read (unit=100,fmt=*, iostat=io_stat_number) root%n, root%m
    if (io_stat_number==0) then !Nicht am File-Ende
        i=i+1
        allocate (root%next)
    end if
    current => root !pointer zurück zum Listenanfang.
    !restliche Zahlen einlesen.
    !associated(pointer): associated(pointer) is true if pointer is associated with a target; otherwise, it returns false.
    !ideal für die do while Bedingung.
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
    !m ab nun Anzahl der Zeilen, funktioniert.
    m=i
    !write(*,*) 'Anzahl an Zeilen:', m
    ! Jetzt kann ich meine ganzen Arrays mit der richtigen Anzahl an Zeilen alloziieren.
    allocate (x(1:m), y(1:m),vx(1:m),vy(1:m),t(1:m))
    !laufindex resetten
    i=1
    !Nun habe ich meine Arrays, muss Elemente welche ich in die linked-List eingelesen habe 
    !frisch alloziierte Array kopieren, daher pointer wieder an anfang der Linked list setzen
    current => root
    
    !i als integer, ist halt dann in Einheiten von 10^-3 Sekunden
    do while(associated(current%next))
        t(i)=0d1
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

    !Differenzenquotient kommt als nächstes zur Berechnung der Geschwindikeit.
    !Vorwärtsdifferenzenquotient 
    ! Vx = (f(x+deltax)-f(x))/deltax, analog für y.
    
    ! Nur bis m-1, verliere einen Eintrag wg. ersten Wert.
    do i=1,m-1
        t(i)=t(i)+deltat*i
        vy(i)=((y(i+1)-y(i))/deltat)
        vx(i)=((x(i+1)-x(i))/deltat)
        
        !write(*,*) t(i), x(i),y(i),vx(i),vy(i)
    end do


    !Ort des Maximums des Arrays suchen. Maximum bei y'=f'(x)=0. in diesem simplen fall.
    !Bahnlaenge größter X-wert, steht nichts von Abwurf auf Bodenhöhe,
    !sonst müsst ich mir den y=0 Schnittpunkt suchen
        i_max=minloc(vy,mask=(vy>0))
        i=int(i_max(1)) !integer conversion.
        !da numerisch kann Maximum entweder nächste gräßerer oder nächstkleinerer wert an 0 sein. 
        !Nächste Zeile checkt ob nächster Funktionswert größer ist, falls ja ist dort das Maximum,
        !ansosnten nichts tun, weil maximum gefunden.
        if(y(i)<y(i+1)) i=i+1
        x_max=x(i)
        y_max=y(i)
        bahnlaenge=maxval(x)
        write(*,*) '# Bahnlaenge=', bahnlaenge, 'Maximum in Zeile', i,'(x_max,y_max)=', x_max, y_max 
    
    
    
        close (UNIT=100)
end program wurfparabel