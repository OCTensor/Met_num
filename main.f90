!
! LEITOR do modulo - by_octensor
!

program main
    
    use sistemas_lineares
    use derivadas
    use integrais
        
        !  ---------------    sistemas lineares ----------------------
        ! declaracao para sistemas lineares / modo geral !

    !real, allocatable :: matrix(:,:)
    !real, allocatable :: d(:,:)
    !real, allocatable :: x(:), b(:), y(:)
    !integer :: n, m

            !obs: sempre olhar o argumento para saber quem deve importar, matrix sem
            ! matrizes no formato: 3x3:b logo n=3, m=4
    !n = 3
    !m = 4

    !call ler_matrix_basic('matriz.dat', matrix, n, m)
   
    !call escalonamento_3x3(matrix, n, m, d, b)
    
    !call elim_gauss_3x3(matrix, x)
    !call fat_lu_3x3(matrix, n, m, d, b)
    !call gauss_jacobi_3x3(matrix)
    !call gauss_seidel_3x3(matrix)

        ! --------------- fim sistemas lineares -----------------------

        ! ---------------       DERIVADAS        ----------------------
        ! declaracao para DERIVADAS / modo geral !
    
    !real :: h, x, y
    !real :: a1, b1, a2, b2
    
    !h = 1e-2

    ! CASO SEJA NECESSÁRIO USAR INTERVALOS (SEMPRE VAI SER!)

    !a1 = 0
    !a2 = 0

    !b1 = 1
    !b2 = 5

    ! SE O Y = 0 USAR DERIVADA DE PRIMEIRA ORDERM !
    ! ponto para calcular o valor !!!!!
    !x = b1
    !y = b2

    ! calcular derivada de primeira ordem f(x) e f(x,y) (diferencas finitas atrasada de primeira ordem)
    !call set_calc_derivada(derivada_fa_1, f, h, x, y, a1, a2, b1, b2)

    ! calcular derivada de primeira ordem f(x) e f(x,y) (diferencas finitas atrasada de segunda ordem)
    !call set_calc_derivada(derivada_fa_2, f, h, x, y, a1, a2, b1, b2)

    !calcular derivada de segunda ordem f(x)'' e f(x,y)'' (diferencas finitas atrasada de primeira ordem)
    !call set_calc_derivada_no_array(derivada_fa_1_2, f, h, x, y, a1, a2, b1, b2)

    !calcular derivada de segunda ordem f(x)'' e f(x,y)'' (diferencas finitas atrasada de segunda ordem)
    !call set_calc_derivada_no_array(derivada_fa_2_2, f, h, x, y, a1, a2, b1, b2)

    !***    8a questão

    ! call set_calc_derivada_no_array(derivada_fa_2, f1, h, x, y, a1, a2, b1, b2)
    
    ! h = 1e-2
    ! a1 = 0
    ! a2 = 0
    ! b1 = 1
    ! b2 = 5
    ! x = b1
    ! y = b2
    
    ! call set_calc_derivada_no_array(derivada_fa_2, f2, h, x, y, a1, a2, b1, b2)


    !***


        ! *********************** CALCULAR DERIVADA COM ARRAY ***************************
    ! real, allocatable :: matrix(:,:) 
    ! real :: h, x, y
    ! real :: a1, b1, a2, b2
    ! integer :: n, m
    ! h = 1e-2

    ! n=10
    ! m=2

    ! x=1 ! apenas para dizer que existe
    ! y=0 ! apenas para dizer que existe
    ! call ler_matrix_basic('seno.dat', matrix, n, m)

    ! print *, 'começar calculo seno' 

    ! call set_calc_derivada_with_array(matrix ,derivada_fa_2_2 ,f , h, x, y, n, m)
        
        ! *********************** FIM -  DERIVADA COM ARRAY ***************************

        ! ---------------       FIM DERIVADAS        ----------------------


        ! ---------------         INTEGRAIS        ----------------------
        ! declaracao para INTEGRAIS / modo geral !

    !real ::  a, b
    !integer :: n
    !a=0
    !b=300

    !n=12

    !para resolver f(x,y) só separar em dx e dy e depois multiplicar
    !call trap_1(f, a, b, n)
    !call simpson_1(f, a, b, n)
        ! ---------------        FIM INTEGRAIS        ----------------------

contains

! USAR FUNÇÕES AQUI PARA DERIVADAS E INTEGRAIS
! ALGUMAS IRÃO REQUERIR USAR DENTRO DE LOOP ETC

!
! função 1 dimensão 

real function f(x,y)
 f = sin(x)
end function f


!
! real function f1(x, y)
!  f1 = x + y -3
! end function f1

! real function f2(x, y)
!  f2 = x**2 + y**2 - 9
! end function f2

!
! função 2 dimensões
!
!real function f(x,y)
! f = 2
!end function

end program main
