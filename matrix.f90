module matrix
    implicit none
contains
    subroutine mm(first, second, multiply, status)
        implicit none
        real (kind = 8), intent(in) :: first(:,:) ! pierwsza macierz
        real (kind = 8), intent(in) :: second(:,:) ! druga macierz
        real (kind = 8), intent(out) :: multiply(:,:) ! macierz wynikowa
        integer (kind = 4), intent(out) :: status ! kod błędu, 0 gdy OK

        integer (kind = 4), dimension(2) :: f_s, s_s,m_s
        integer (kind = 4) :: i,j,k
        real (kind = 8) :: tmp

        status = 1  !if multipling loop doesn't end status will be 1 for failed multiplication

        !gets sizes of given matrixes
        f_s=shape(first)
        s_s=shape(second)
        m_s=shape(multiply)

        if ( (f_s(2) .EQ. s_s(1)) .AND. (f_s(1) .EQ. m_s(1)) .AND. (s_s(2) .EQ. m_s(2)) ) then
            do i=1,m_s(1)           !columns
                do j=1,m_s(2)       !rows
                    tmp=0.d0
                    do k=1,f_s(2)
                        tmp = tmp + first(i,k)*second(k,j)
                    end do
                    multiply(i,j)=tmp
                end do
            end do
            status = 0               !if all goes as planned status is set to 0
        end if

    end subroutine mm

    subroutine mm_dot(first, second, multiply, status)
        implicit none
        real (kind = 8), intent(in) :: first(:,:) ! pierwsza macierz
        real (kind = 8), intent(in) :: second(:,:) ! druga macierz
        real (kind = 8), intent(out) :: multiply(:,:) ! macierz wynikowa
        integer (kind = 4), intent(out) :: status ! kod błędu, 0 gdy OK

        integer (kind = 4), dimension(2) :: f_s, s_s,m_s
        integer (kind = 4) :: i,j

        status = 1  !if multipling loop doesn't end status will be 1 for failed multiplication

        !gets sizes of given matrixes
        f_s=shape(first)
        s_s=shape(second)
        m_s=shape(multiply)

        if ( (f_s(2) .EQ. s_s(1)) .AND. (f_s(1) .EQ. m_s(1)) .AND. (s_s(2) .EQ. m_s(2)) ) then
            do i=1,m_s(1) !columns
                do j=1,m_s(2) !rows
                    multiply(i, j) = dot_product(first(i,:),second(:,j))
                end do
            end do
            status = 0  !if all goes as planned status is set to 0
        end if

    end subroutine mm_dot

    subroutine mm_chunk(first, second, multiply, status)
        implicit none
        real (kind = 8), intent(in) :: first(:,:) ! pierwsza macierz
        real (kind = 8), intent(in) :: second(:,:) ! druga macierz
        real (kind = 8), intent(out) :: multiply(:,:) ! macierz wynikowa
        integer (kind = 4), intent(out) :: status ! kod błędu, 0 gdy OK

        integer (kind = 4), dimension(2) :: f_s, s_s,m_s
        integer (kind = 4) :: i,j,k,jj,kk,ichunk
        real (kind = 8) :: tmp

        status = 1  !if multipling loop doesn't end status will be 1 for failed multiplication

        !gets sizes of given matrixes
        f_s=shape(first)
        s_s=shape(second)
        m_s=shape(multiply)

        if ( (f_s(2) .EQ. s_s(1)) .AND. (f_s(1) .EQ. m_s(1)) .AND. (s_s(2) .EQ. m_s(2)) ) then
            ichunk = 512 ! I have a 3MB cache size (real*4)
            do jj = 1, m_s(1), ichunk
                do kk = 1, m_s(2), ichunk
                    do j = jj, min(jj + ichunk - 1, m_s(1))
                        do k = kk, min(kk + ichunk - 1, m_s(2))
                            tmp = 0.d0
                            do i = 1, f_s(2)
                                tmp = tmp + first(j, i) * second(i, k)
                            end do
                            multiply(j, k) = tmp
                        end do
                    end do

                end do
            end do
            status = 0  !if all goes as planned status is set to 0
        end if

    end subroutine mm_chunk

    subroutine mm_dot_and_chunk(first, second, multiply, status)
        implicit none
        real (kind = 8), intent(in) :: first(:,:) ! pierwsza macierz
        real (kind = 8), intent(in) :: second(:,:) ! druga macierz
        real (kind = 8), intent(out) :: multiply(:,:) ! macierz wynikowa
        integer (kind = 4), intent(out) :: status ! kod błędu, 0 gdy OK

        integer (kind = 4), dimension(2) :: f_s, s_s,m_s
        integer (kind = 4) :: j,k,jj,kk,ichunk

        status = 1  !if multipling loop doesn't end status will be 1 for failed multiplication

        !gets sizes of given matrixes
        f_s=shape(first)
        s_s=shape(second)
        m_s=shape(multiply)

        if ( (f_s(2) .EQ. s_s(1)) .AND. (f_s(1) .EQ. m_s(1)) .AND. (s_s(2) .EQ. m_s(2)) ) then
            ichunk = 512 ! I have a 3MB cache size (real*4)
            do jj = 1, m_s(1), ichunk
                do kk = 1, m_s(2), ichunk
                    do j = jj, min(jj + ichunk - 1, m_s(1))
                        do k = kk, min(kk + ichunk - 1, m_s(2))
                            multiply(j, k) = dot_product(first(j,:),second(:,k))
                        end do
                    end do

                end do
            end do
            status = 0  !if all goes as planned status is set to 0
        end if

    end subroutine mm_dot_and_chunk

end module matrix