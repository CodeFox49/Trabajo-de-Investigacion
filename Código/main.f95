program saint_venant_1d
    implicit none

    integer :: i, n, nx, nt
    real :: dx, dt, g

    real, allocatable :: A(:), Q(:)
    real, allocatable :: Anew(:), Qnew(:)

    print *, "Ingrese dx:"
    read *, dx

    print *, "Ingrese dt:"
    read *, dt

    print *, "Ingrese nx:"
    read *, nx

    print *, "Ingrese nt:"
    read *, nt

    !g = 9.81
    !nx = 200
    !nt = 300
    !dx = 10.0
    !dt = 0.2

    allocate(A(nx), Q(nx), Anew(nx), Qnew(nx))

    call condiciones_iniciales(A, Q, nx)

    do n = 1, nt
        call resolver_saint_venant(A, Q, Anew, Qnew, nx, dx, dt, g)
        A = Anew
        Q = Qnew
    end do
    
    !Impresión para exportar a Python
    do i = 1, nx
        print *, (i-1)*dx, A(i), Q(i)
    end do

end program !Fin del main

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine condiciones_iniciales(A, Q, nx)
    implicit none
    integer, intent(in) :: nx
    real, intent(out) :: A(nx), Q(nx)

    integer :: i

    do i = 1, nx
        if (i < nx/2) then
            A(i) = 1.2
        else
            A(i) = 1.0
        end if
        Q(i) = 0.0
    end do
    
    ! do i = 1, nx
    !     A(i) = 1.0
    !     Q(i) = 0.0
    ! end do
end subroutine condiciones_iniciales

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine resolver_saint_venant(A, Q, Anew, Qnew, nx, dx, dt, g)
    implicit none
    integer, intent(in) :: nx
    real, intent(in) :: dx, dt, g
    real, intent(in) :: A(nx), Q(nx)
    real, intent(out) :: Anew(nx), Qnew(nx)

    real :: Ap(nx), Qp(nx)
    real :: F1(nx), F2(nx)
    integer :: i

    ! Flujos
    do i = 1, nx
        F1(i) = Q(i)
        F2(i) = Q(i)*Q(i)/A(i) + 0.5*g*A(i)*A(i)
    end do

    ! Predictor
    do i = 2, nx-1
        Ap(i) = A(i) - dt/dx*(F1(i+1)-F1(i))
        Qp(i) = Q(i) - dt/dx*(F2(i+1)-F2(i))
    end do

    ! Fronteras
    Ap(1)=A(1); Ap(nx)=A(nx)
    Qp(1)=Q(1); Qp(nx)=Q(nx)

    ! Nuevos flujos
    do i = 1, nx
        F1(i) = Qp(i)
        F2(i) = Qp(i)*Qp(i)/Ap(i) + 0.5*g*Ap(i)*Ap(i)
    end do

    ! Corrector
    do i = 2, nx-1
        Anew(i) = 0.5*(A(i)+Ap(i) - dt/dx*(F1(i)-F1(i-1)))
        Qnew(i) = 0.5*(Q(i)+Qp(i) - dt/dx*(F2(i)-F2(i-1)))
    end do

    Anew(1)=A(1); Anew(nx)=A(nx)
    Qnew(1)=Q(1); Qnew(nx)=Q(nx)

end subroutine resolver_saint_venant

!Program Hello
!implicit none
!REAL, parameter :: x
!INTEGER :: num = 4

!Print *, "Hello World"
!num = 4!
!Print *, num

!End Program Hello