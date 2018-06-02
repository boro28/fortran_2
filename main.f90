subroutine start_clock(Icount)
    integer(kind = 4), intent(inout) :: Icount
    integer(kind = 4) :: ir, im

    call system_clock(Icount, ir, im)

end subroutine start_clock


subroutine stop_clock(Dtime, Icount)
    integer(kind = 4), intent(in) :: Icount
    real(kind = 8), intent(out) :: Dtime
    integer(kind = 4) :: ic, ir, im

    call system_clock(ic, ir, im)
    ! write(*, *) 'start time ', Icount, ' stop time', ic

    if (ic > Icount) then
        Dtime = dble(ic - Icount) / dble(ir)
    elseif (ic < Icount) then
        Dtime = dble(im + ic - Icount) / dble(ir)
    else
        Dtime = 0.d0
    endif

end subroutine stop_clock

program fortra_2project! pierwsza macierz
    use matrix
    implicit none
    real (kind = 8), allocatable :: first(:, :) ! pierwsza macierz
    real (kind = 8), allocatable :: second(:, :) ! druga macierz
    real (kind = 8), allocatable :: multiply(:, :) ! macierz wynikowa
    integer (kind = 4) :: status ! kod błędu, 0 gdy OK
    integer(kind = 4) :: iclock, i,j,k
    integer(kind = 4), dimension(3) :: AllocateStatus, DeAllocateStatus
    real (kind = 8), dimension(5) :: dtime

    open(unit = 2, file = "results.txt")

    do i = 10, 1000, 10
        allocate(first(i, i), STAT = AllocateStatus(1))
        allocate(second(i, i), STAT = AllocateStatus(2))
        allocate(multiply(i, i), STAT = AllocateStatus(3))
        if(MAXVAL(AllocateStatus) .EQ. 1) then
            write(2, *)"Error allocating memory"
            STOP
        end if

        do k = 1, i
            do j = 1, i
                first(k, j) = k + j
                second(k, j) = k - j
            end do
        end do

        call start_clock(iclock) !starts the clock
        call mm(first, second, multiply, status)
        call stop_clock(dtime(1), iclock) !end the clock

        call start_clock(iclock) !starts the clock
        call mm_dot(first, second, multiply, status)
        call stop_clock(dtime(2), iclock) !end the clock

        call start_clock(iclock) !starts the clock
        call mm_chunk(first, second, multiply, status)
        call stop_clock(dtime(3), iclock) !end the clock

        call start_clock(iclock) !starts the clock
        call mm_dot_and_chunk(first, second, multiply, status)
        call stop_clock(dtime(4), iclock) !end the clock

        call start_clock(iclock) !starts the clock
        multiply = matmul(first, second)
        call stop_clock(dtime(5), iclock) !end the clock

        write(2, *) "mm:", dtime(1), "dot:", dtime(2), "chunk:", dtime(3), "dot_and_chunk:", dtime(4), "matmul:", dtime(5)

        deallocate(first, STAT = DeAllocateStatus(1))
        deallocate(second, STAT = DeAllocateStatus(2))
        deallocate(multiply, STAT = DeAllocateStatus(3))

        if(MAXVAL(AllocateStatus) .EQ. 1) then
            write(2, *)"Error deallocating memory"
            STOP
        end if

    end do

end program