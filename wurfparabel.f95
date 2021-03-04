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



program wurfparabel
    implicit none
    integer :: io_stat_number = -1 !return von iostat, ob korrekt geöffnet wurde
    integer, parameter :: n=1000 ! Anzahl einzulesender Zeilen
    integer :: n_max=0           ! de-facto Anzahl an Zeilen im File
    double precision, dimension (1:n) :: x,y,vx,vy,t !Variablen als einzelarrays
    integer :: i !Laufnummer
    character*20 :: file_name = &       !filename/directory im header änderbar
            'flug.dat'
    character*20 :: file_out = &        !outputfilename/directory änderbar. falls pfad länger als 20 Zeichen erweitern!
            'result.dat'
    

    open(unit=100, file=file_name, status='old', action='read', iostat=io_stat_number)
    if (io_stat_number == 0) then
        write (*,*) file_name, 'opened without any errors'
    else
        write(*,*) 'Could not open file:', file_name, 'please check if given path is correct.'
    end if
    
        do i=1,n
                read(100, *, iostat=io_stat_number) x(i), y(i)
                print *, i, x(i), y(i)  
            if (io_stat_number==-1) exit      
        end do
    close(100)

    !do i=1,10
    !   write(*,*) x(i), y(i)
    !end do

    


end program wurfparabel