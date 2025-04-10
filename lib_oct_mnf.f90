! cody_by>>> OCTENSOR

module sistemas_lineares
contains
    
    ! SUBROTINA PARA LER DADOS DE UM ARQUIVO .DAT (EM FORMA MATRICIAL NXM:B)
    ! --- SUBROTINA COMPLETA ! COM SEPARAÇÕES
    
    subroutine ler_matrix_full(arquivo, matrix, a, b, n, m)
        character(len=*), intent(in) :: arquivo
        integer, intent(in) :: n, m
        real, allocatable, intent(out) :: matrix(:,:), a(:,:), b(:)
        integer :: i, j, unit_num

        unit_num = 10
        open(unit=unit_num, file=arquivo, status='old', action='read')
        
        allocate(matrix(n, m))
        allocate(a(n, m-1))
        allocate(b(n))

        do i = 1, n
            read(unit_num, *) (matrix(i, j), j = 1, m)
        end do
        
        ! extrair a matriz de coeficientes e o vetor de termos independentes
        do i = 1, n
            do j = 1, m-1
                a(i, j) = matrix(i, j)
            end do
            b(i) = matrix(i, m)
        end do


        print *, '==========================================='
        print *, '::','abaixo valores da matriz completa','::'
        print *, '==========================================='

        do i=1, n
            print *, (matrix(i,j),j=1,m)
        end do

        print *, '==========================================='
        print *, '::','abaixo valores da matriz a(:)    ','::'
        print *, '==========================================='

        do i=1, n
            print *, (a(i,j),j=1, m-1)
        end do

        print *, '==========================================='
        print *, '::','abaixo valores da matriz b(:)    ','::'
        print *, '==========================================='

        do i=1, n
            print *, (b(i))
        end do

        close(unit_num)

        print *, '==========================================='

    end subroutine ler_matrix_full

    !
    ! SUBROTINA PARA LER DADOS DE UM ARQUIVO .DAT (EM FORMA MATRICIAL NXM:B)
    ! --- SUBROTINA basica ! sem SEPARAÇÕES
    
    subroutine ler_matrix_basic(arquivo, matrix, n, m)
        character(len=*), intent(in) :: arquivo
        integer, intent(in) :: n, m
        real, allocatable, intent(out) :: matrix(:,:)
        integer :: i, j, unit_num

        unit_num = 10
        open(unit=unit_num, file=arquivo, status='old', action='read')

        allocate(matrix(n, m))

        do i = 1, n
            read(unit_num, *) (matrix(i, j), j = 1, m)
        end do

        print *, '==========================================='
        print *, '::','abaixo valores da matriz completa','::'
        print *, '==========================================='

        do i=1, n
            print *, (matrix(i,j),j=1,m)
        end do

        close(unit_num)

    end subroutine ler_matrix_basic

    !ETAPA DE ESCALOMENTO 3X3
    ! PRECISA DA SUBROTINA ler_arquivo_basic (preferencial)

    subroutine escalonamento_3x3(matrix, n, m, d, b)
        real, intent(inout) :: matrix(:,:)
        real, allocatable, intent(out) :: d(:,:), b(:)
        integer, intent(inout) :: n, m
        integer :: i, j

        allocate(b(n))
        allocate(d(n,m))

        do i=1, n
          b(i) = matrix(i,4)
        end do

        d(2,1) = matrix(2,1)/matrix(1,1)
        d(3,1) = matrix(3,1)/matrix(1,1)

        do j=1, m
            matrix(2,j) = matrix(2,j) - d(2,1)*matrix(1,j)
        end do

        do j=1, m
            matrix(3,j) = matrix(3,j) - d(3,1)*matrix(1,j)
        end do

        d(3,2) = matrix(3,2)/matrix(2,2)

        do j=1, m
            matrix(3,j) = matrix(3,j) - d(3,2)*matrix(2,j)
        end do


        print *, '==========================================='
        print *, '::','  abaixo valores da escalonada   ','::'
        print *, '==========================================='

        do i=1,n
          print *, (matrix(i,j), j=1,m)
        end do

    end subroutine escalonamento_3x3

    !ETAPA DE ESCALOMENTO 2X2
    ! PRECISA DA SUBROTINA ler_arquivo_basic (preferencial)

    subroutine escalonamento_2x2(matrix, n, m, d, b)
        real, intent(inout) :: matrix(:,:)
        real, allocatable, intent(out) :: d(:,:), b(:)
        integer, intent(inout) :: n, m
        integer :: i, j

        allocate(d(n,m))
        allocate(b(n))

        do i=1, n
          b(i) = matrix(i,3)
        end do

        d(2,1) = matrix(2,1)/matrix(1,1)

        do j=1, m
            matrix(2,j) = matrix(2,j) - d(2,1)*matrix(1,j)
        end do

        print *, '==========================================='
        print *, '::','  abaixo valores da escalonada   ','::'
        print *, '==========================================='

        do i=1,n
          print *, (matrix(i,j), j=1,m)
        end do

    end subroutine escalonamento_2x2


    ! SUBROTINA PARA ELIMINAÇÃO GAUSSIANA 3x3 !!!
    ! -- UTILIZA a subrotina ler_matrix_basic (preferencial)
    ! -- UTILIZA a subrotina escalonamento_3x3
    ! -- SALVANDO A MATRIX NO CORPO PRINCIPAL DO CÓDIGO

    subroutine elim_gauss_3x3(matrix, x)
        real, intent(in) :: matrix(:,:)
        real, allocatable, intent(out) :: x(:)

        allocate(x(3))

        x(3) = matrix(3,4) / matrix(3,3)
        x(2) = (matrix(2,4) - x(3)*matrix(2,3))/matrix(2,2)
        x(1) = (matrix(1,4) - x(2)*matrix(1,2) - x(3)*matrix(1,3))/matrix(1,1)

        print *, '==========================================='
        print *, '::','            gauss 3x3            ','::'
        print *, '::','  abaixo valores de x            ','::'
        print *, '==========================================='

        do i=1, 3
          print *, 'valor de x = ', i, ' :::: ', x(i)
        end do

    end subroutine elim_gauss_3x3



    ! SUBROTINA PARA ELIMINAÇÃO GAUSSIANA 2x2 !!!
    ! -- UTILIZA a subrotina ler_matrix_basic (preferencial)
    ! -- UTILIZA a subrotina escalonamento_2x2
    ! -- SALVANDO A MATRIX NO CORPO PRINCIPAL DO CÓDIGO

    subroutine elim_gauss_2x2(matrix, x)
        real, intent(in) :: matrix(:,:)
        real, allocatable, intent(out) :: x(:)

        allocate(x(2))

        x(2) = matrix(2,3)/matrix(2,2)
        x(1) = (matrix(1,3) - x(2)*matrix(1,2))/matrix(1,1)

        print *, '==========================================='
        print *, '::','            gauss 2x2            ','::'
        print *, '::','  abaixo valores de x            ','::'
        print *, '==========================================='

        do i=1, 2
          print *, 'valor de x = ', i, ' :::: ', x(i)
        end do

    end subroutine elim_gauss_2x2


    ! SUBROTINA PARA FATORAÇÃO LU 3x3 !!!
    ! -- UTILIZA a subrotina ler_matrix_basic (preferencial)
    ! -- REQUER A UTILIZAÇÃO DE escalonamento_3x3
    ! -- SALVANDO A MATRIX NO CORPO PRINCIPAL DO CÓDIGO

    subroutine fat_lu_3x3(matrix, n ,m, d, b)
      real, intent(in) :: matrix(:,:), d(:,:), b(:)
      real, allocatable :: l(:,:), u(:,:)
      real, allocatable :: x(:), y(:)
      integer, intent(in) :: n, m
      integer :: i,j

      allocate(l(n,m-1))
      allocate(u(n,m-1))
      allocate(x(n))
      allocate(y(n))

      do i=1, n
        do j=1, m-1
            u(i,j) = matrix(i,j)
        end do
      end do

      do i=1, n
        do j=1, m-1
          l(i,j) = 0
        end do
      end do

      do i=1,n
        do j=1, m-1
          if (i == j) then
            l(i,j) = 1
          end if
        end do
      end do

      do i=2, 3
        do j=1, 2
          if (i==j) then
            print*, 'pulando ij =', i, j
          else
            l(i,j) = d(i,j)
          end if
        end do
      end do

      print *, '==========================================='
      print *, '::','            matrix   L           ','::'
      print *, '==========================================='


      do i=1,n
        print *, (l(i,j),j=1,m-1)
      end do

      print *, '==========================================='
      print *, '::','            matrix   U           ','::'
      print *, '==========================================='


      do i=1,n
        print *, (u(i,j),j=1,m-1)
      end do

      y(1) = b(1)
      y(2) = b(2) - l(2,1)*y(1)
      y(3) = b(3) - y(1)*l(3,1) - y(2)*l(3,2)

      print *, '==========================================='
      print *, '::','            valores de y         ','::'
      print *, '==========================================='

      do i=1,n
        print *,'para y=',i, ':::::', y(i)
      end do

      print *, '==========================================='
      print *, '::','            valores de x         ','::'
      print *, '==========================================='

      x(3) = y(3)/u(3,3)
      x(2) = (y(2) - x(3)*u(2,3))/u(2,2)
      x(1) = (y(1) - x(2)*u(1,2) - x(3)*u(1,3))/u(1,1)

      do i=1,n
        print *,'para x=',i, ':::::', x(i)
      end do

    end subroutine fat_lu_3x3

    subroutine fat_lu_2x2(matrix, n ,m, d, b)
      real, intent(in) :: matrix(:,:), d(:,:), b(:)
      real, allocatable :: l(:,:), u(:,:)
      real, allocatable :: x(:), y(:)
      integer, intent(in) :: n, m
      integer :: i,j

      allocate(l(n,m-1))
      allocate(u(n,m-1))
      allocate(x(n))
      allocate(y(n))

      do i=1, n
        do j=1, m-1
            u(i,j) = matrix(i,j)
        end do
      end do

      do i=1, n
        do j=1, m-1
          l(i,j) = 0
        end do
      end do

      do i=1,n
        do j=1, m-1
          if (i == j) then
            l(i,j) = 1
          end if
        end do
      end do

      do i=2, 2
        do j=1, 1
          if (i==j) then
            print*, 'pulando ij =', i, j
          else
            l(i,j) = d(i,j)
          end if
        end do
      end do

      print *, '==========================================='
      print *, '::','            matrix   L           ','::'
      print *, '==========================================='

      do i=1,n
        print *, (l(i,j),j=1,m-1)
      end do

      print *, '==========================================='
      print *, '::','            matrix   U           ','::'
      print *, '==========================================='

      do i=1,n
        print *, (u(i,j),j=1,m-1)
      end do

      y(1) = b(1)
      y(2) = b(2) - l(2,1)*y(1)

      print *, '==========================================='
      print *, '::','            valores de y         ','::'
      print *, '==========================================='

      do i=1,n
        print *,'para y=',i, ':::::', y(i)
      end do

      print *, '==========================================='
      print *, '::','            valores de x         ','::'
      print *, '==========================================='

      x(2) = y(2)/u(2,2)
      x(1) = (y(1) - x(2)*u(1,2))/u(1,1)

      do i=1,n
        print *,'para x=',i, ':::::', x(i)
      end do

    end subroutine fat_lu_2x2

  subroutine gauss_jacobi_3x3(matrix)
    real, intent(in) :: matrix(:, :)
    real :: tol, erro, x1, x2, x3, x1_1, x2_1, x3_1
    integer :: i, max_iter

    tol = 1.0e-6
    max_iter = 100  

    ! Chute inicial
    x1 = 0.0
    x2 = 0.0
    x3 = 0.0

    do i = 1, max_iter
      ! Atualiza os valores de x
      x1_1 = (matrix(1,4) - matrix(1,2)*x2 - matrix(1,3)*x3) / matrix(1,1)
      x2_1 = (matrix(2,4) - matrix(2,1)*x1 - matrix(2,3)*x3) / matrix(2,2)
      x3_1 = (matrix(3,4) - matrix(3,1)*x1 - matrix(3,2)*x2) / matrix(3,3)

      !ref funcão: https://www.cita.utoronto.ca/~merz/intel_f10b/main_for/mergedProjects/lref_for/source_files/rfmax.htm
      erro = max(abs(x1 - x1_1), abs(x2 - x2_1), abs(x3 - x3_1))

      print*, 'Iteração', i
      print*, 'Erro x1:', abs(x1 - x1_1)
      print*, 'Erro x2:', abs(x2 - x2_1)
      print*, 'Erro x3:', abs(x3 - x3_1)
      print*, 'Erro máximo:', erro
      print*, '------------------------'

      x1 = x1_1
      x2 = x2_1
      x3 = x3_1

      if (erro < tol) then
        print*, 'convergiu em ... ', i, ' ...iterações'
        exit
      end if
    end do

    print*, 'valor de x1 = ', x1
    print*, 'valor de x2 = ', x2
    print*, 'valor de x3 = ', x3

  end subroutine gauss_jacobi_3x3

  subroutine gauss_jacobi_2x2(matrix)
    real, intent(in) :: matrix(:, :)
    real :: tol, erro, x1, x2, x1_1, x2_1
    integer :: i, max_iter

    tol = 1.0e-6
    max_iter = 100  

    ! Chute inicial
    x1 = 0.0
    x2 = 0.0

    do i = 1, max_iter

      x1_1 = (matrix(1,3) - matrix(1,2)*x2) / matrix(1,1)
      x2_1 = (matrix(2,3) - matrix(2,1)*x1) / matrix(2,2)

      erro = max(abs(x1 - x1_1), abs(x2 - x2_1))

      print*, 'Iteração', i
      print*, 'Erro x1:', abs(x1 - x1_1)
      print*, 'Erro x2:', abs(x2 - x2_1)
      print*, 'Erro máximo:', erro
      print*, '------------------------'

      x1 = x1_1
      x2 = x2_1

      if (erro < tol) then
        print*, 'Convergiu em ... ', i, ' ...iterações'
        exit
      end if
    end do

    print*, 'valor de x1 = ', x1
    print*, 'valor de x2 = ', x2

  end subroutine gauss_jacobi_2x2

  subroutine gauss_seidel_3x3(matrix)
      real, intent(in) :: matrix(:, :)
      real :: tol, erro, x1, x2, x3, x1_1, x2_1, x3_1
      integer :: i, max_iter

      tol = 1.0e-6
      max_iter = 100  

      x1 = 0.0
      x2 = 0.0
      x3 = 0.0

      do i = 1, max_iter
          ! atualiza x1
          x1_1 = (matrix(1,4) - matrix(1,2)*x2 - matrix(1,3)*x3) / matrix(1,1)

          ! atualiza x2 com o valor mais recente de x1
          x2_1 = (matrix(2,4) - matrix(2,1)*x1_1 - matrix(2,3)*x3) / matrix(2,2)

          ! atualiza x3 com os valores mais recentes de x1 e x2
          x3_1 = (matrix(3,4) - matrix(3,1)*x1_1 - matrix(3,2)*x2_1) / matrix(3,3)

          ! calcula o erro máximo entre os valores anteriores e os novos
          erro = max(abs(x1 - x1_1), abs(x2 - x2_1), abs(x3 - x3_1))

          print*, 'Iteração', i
          print*, 'Erro x1:', abs(x1 - x1_1)
          print*, 'Erro x2:', abs(x2 - x2_1)
          print*, 'Erro x3:', abs(x3 - x3_1)
          print*, 'Erro máximo:', erro
          print*, '------------------------'

          x1 = x1_1
          x2 = x2_1
          x3 = x3_1

          if (erro < tol) then
              print*, 'convergiu em ... ', i, ' ...iterações'
              exit
          end if
      end do

      print*, 'valor final de x1:', x1
      print*, 'valor final de x2:', x2
      print*, 'valor final de x3:', x3
  end subroutine gauss_seidel_3x3

  subroutine gauss_seidel_2x2(matrix)
      real, intent(in) :: matrix(:, :)
      real :: tol, erro, x1, x2, x1_1, x2_1
      integer :: i, max_iter

      tol = 1.0e-6
      max_iter = 100  

      ! Chute inicial
      x1 = 0.0
      x2 = 0.0

      do i = 1, max_iter
          ! atualiza x1
          x1_1 = (matrix(1,3) - matrix(1,2)*x2) / matrix(1,1)

          ! atualiza x2 com o valor mais recente de x1
          x2_1 = (matrix(2,3) - matrix(2,1)*x1_1) / matrix(2,2)

          ! calcula o erro máximo entre os valores anteriores e os novos
          erro = max(abs(x1 - x1_1), abs(x2 - x2_1))

          print*, 'Iteração', i
          print*, 'Erro x1:', abs(x1 - x1_1)
          print*, 'Erro x2:', abs(x2 - x2_1)
          print*, 'Erro máximo:', erro
          print*, '------------------------'

          x1 = x1_1
          x2 = x2_1

          if (erro < tol) then
              print*, 'convergiu em ... ', i, ' ...iterações'
              exit
          end if
      end do

      print*, 'valor final de x1:', x1
      print*, 'valor final de x2:', x2
  end subroutine gauss_seidel_2x2

end module sistemas_lineares

!!!!
module derivadas
contains
    
    
    ! uma regra foi posto, a cada aumento de ordem tem adição
    ! de um d, exemplo:: df, ddf, dddf, dddf

    !
    ! FINITAS ATRASADA DE PRIMEIRA ORDEM!

  subroutine derivada_fa_1(f, h, x, y, df_x, df_y)
    real, external :: f
    real, intent(in) :: h, x, y
    real, intent(out) :: df_x, df_y

    if (y == 0) then
      df_x = (f(x + h) - f(x)) / h
    else
    ! Derivada em relação a x (mantendo y fixo)
      df_x = (f(x + h, y) - f(x, y)) / h
    ! Derivada em relação a y (mantendo x fixo)
      df_y = (f(x, y + h) - f(x, y)) / h
    end if
  end subroutine derivada_fa_1

    ! ainda f'()
    ! FINITAS ATRASADA DE segunda ORDEM!
    ! a diferença é a precisão

  subroutine derivada_fa_2(f, h, x, y, df_x, df_y)
    real, external :: f
    real, intent(in) :: h, x, y
    real, intent(out) :: df_x, df_y

    if (y == 0) then
      df_x = (3*f(x) - 4*f(x - h) + f(x - 2*h)) / (2*h)
    else
    ! Derivada em relação a x (mantendo y fixo)
      df_x = (3*f(x, y) - 4*f(x - h, y) + f(x - 2*h, y)) / (2*h)
    ! Derivada em relação a y (mantendo x fixo)
      df_y = (3*f(x, y) - 4*f(x, y - h) + f(x, y - 2*h)) / (2*h)
    end if
  end subroutine derivada_fa_2

    ! derivada segunda f''()
    ! FINITAS ATRASADA DE primeira ORDEM!

  subroutine derivada_fa_1_2(f, h, x, y, df_x, df_y)
    real, external :: f
    real, intent(in) :: h, x, y
    real, intent(out) :: df_x, df_y

    if (y == 0) then
      df_x = (f(x) - 2*f(x - h) + f(x - 2*h)) / (h**2)
    else
    ! Derivada em relação a x (mantendo y fixo)
      df_x = (f(x,y) - 2*f(x - h, y) + f(x - 2*h, y)) / (h**2)
    ! Derivada em relação a y (mantendo x fixo)
      df_y = (f(x,y) - 2*f(x, y - h) + f(x, y - 2*h)) / (h**2)
    end if
  end subroutine derivada_fa_1_2
   
    ! derivada segunda f''()
    ! FINITAS ATRASADA DE segunda ORDEM!

  subroutine derivada_fa_2_2(f, h, x, y, df_x, df_y)
    real, external :: f
    real, intent(in) :: h, x, y
    real, intent(out) :: df_x, df_y

    if (y == 0) then
      df_x = (2*f(x) - 5*f(x - h) + 4*f(x - 2*h) - f(x - 3*h)) / (h**2)
    else
    ! Derivada em relação a x (mantendo y fixo)
      df_x = (2*f(x,y) - 5*f(x - h,y) + 4*f(x - 2*h,y) - f(x - 3*h,y)) / (h**2)
    ! Derivada em relação a y (mantendo x fixo)
      df_y = (2*f(x,y) - 5*f(x,y - h) + 4*f(x,y - 2*h) - f(x,y - 3*h)) / (h**2)
    end if
  end subroutine derivada_fa_2_2

    ! SUBROTINA PARA OTIMIZAR CALCULOS DE DERIVADAS!
    ! SOMENTE F = 1 DIMENSÃO ou 2 DIMENSÕES
    ! sem interação com .dat de pontos

  subroutine set_calc_derivada_no_array(deriv, f, h, x, y, a1, a2, b1, b2)
    real, external :: f
    real, intent(inout) :: h, x, y, a1, a2, b1, b2
    external :: deriv
    ! Declaração das variáveis para armazenar os resultados das derivadas
    real :: df_x, df_y

    if (y == 0) then
      do while (x <= b1)
        call deriv(f, h, x, y, df_x, df_y)
        print *, 'Derivada em x =', x, ' ... é ...', df_x
        x = x + h
      end do
    else
      do while (x <= b1 .and. y <= b2)
        call deriv(f, h, x, y, df_x, df_y)
        print *, 'Derivada em x =', x, ' e y =', y, ' ... são ...', df_x, df_y
        x = x + h
        y = y + h
      end do
    end if

  end subroutine set_calc_derivada_no_array

    ! INCOMPLETOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
    ! SUBROTINA PARA OTIMIZAR CALCULOS DE DERIVADAS!
    ! SOMENTE F = 1 DIMENSÃO ou 2 DIMENSÕES
    ! com interação com .dat de pontos

  subroutine set_calc_derivada_with_array(matrix, deriv, f, h, x, y, n, m)
    real, intent(in) :: matrix(:,:)
    real, external :: f
    real, intent(inout) :: h, x, y
    real :: a1,a2,b1,b2
    external :: deriv
    ! Declaração das variáveis para armazenar os resultados das derivadas
    real :: df_x, df_y

    k = n

    a1 = 0
    a2 = 0 
    
    do i=1, k
      print*, 'PARA TODO K=',i

      if (y == 0) then
        b1 = matrix(i,1)
        x = b1
        do while (x <= b1)
          call deriv(f, h, x, y, df_x, df_y)
          print *, 'para matrix ::', 'i=',i,'j=',1
          print *, 'Derivada em x =', x, ' ... é ...', df_x
          x = x + h
        end do
      else
        b1 = matrix(i,1)
        b2 = matrix(i,2)
        x = b1
        y = b2
        do while (x <= b1 .and. y <= b2)
          call deriv(f, h, x, y, df_x, df_y)
          print *, 'para matrix ::', 'i=',i,'j=',1, ' ...em x'
          print *, 'para matrix ::', 'i=',i,'j=',2, ' ...em y'
          print *, 'Derivada em x =', x, ' e y =', y, ' ... são ...', df_x, df_y
          x = x + h
          y = y + h
        end do
      end if
    end do

  end subroutine set_calc_derivada_with_array

end module derivadas

module integrais
contains

  ! REGRA DO TRAPEZIO com n intervalos  (composto)
  ! ref:: https://www.youtube.com/watch?v=smNmTmeP-KA
  ! ref:: https://cn.ect.ufrn.br/index.php?r=conteudo%2Finteg-trapezio
  subroutine trap_1(f, a, b, n)
    real, external :: f
    real, intent(in) :: a, b
    integer, intent(in) :: n
    real :: integral
    real :: h, x
    integer :: i

    ! dividir subintervalo

    h = (b-a)/n

    ! somando extremidades
    integral = f(a) + f(b)

    ! somando valores intermediário
    do i=1, n-1
      x = a + (i * h)
      integral = integral + 2*f(x)
    end do

    ! multiplica por h/2 que nem na apostilha
    integral = integral*(h/2)

    print *, 'METODO DO TRAPEZIN' 

    print *, 'Valor da integral de', a, 'até', b, 'é:', integral

  end subroutine trap_1

  ! regra 1/3 de simpson !!!!!
  subroutine simpson_1(f, a, b, n)
    implicit none
    real, external :: f
    real, intent(in) :: a, b
    integer, intent(in) :: n
    real :: integral, h, x
    integer :: i

    ! regra de Simpson requer um número par de subintervalos.
    if (mod(n, 2) /= 0) then
      print *, "Erro: n deve ser par para a regra de Simpson."
      stop
    end if

    ! calcula o tamanho do subintervalo.
    h = (b - a) / n

    ! nicia a soma com as extremidades.
    integral = f(a) + f(b)

    ! somando os pontos intermediários com os pesos adequados.
    do i = 1, n-1
      x = a + i * h
      if (mod(i, 2) == 1) then ! resto
        integral = integral + 4*f(x)
      else
        integral = integral + 2*f(x)
      end if
    end do

    ! multiplica pelo fator h/3.
    integral = integral * (h/3)

    print*, 'METODO DE SIMPSON 1/3'

    print *, 'Valor da integral de', a, 'até', b, 'é:', integral

  end subroutine simpson_1

  ! uma ideia de f(x,y) automatizada
  subroutine trap_2(f, a_x, b_x, a_y, b_y, n_x, n_y)
      real, external :: f
      real, intent(in) :: a_x, b_x, a_y, b_y
      integer, intent(in) :: n_x, n_y
      real :: integral, h_x, h_y, x, y
      integer :: i, j

      h_x = (b_x - a_x) / n_x
      h_y = (b_y - a_y) / n_y

      integral = f(a_x, a_y) + f(b_x, a_y) + f(a_x, b_y) + f(b_x, b_y)

      do i = 1, n_x - 1
          x = a_x + i * h_x
          integral = integral + 2 * (f(x, a_y) + f(x, b_y))
      end do

      do j = 1, n_y - 1
          y = a_y + j * h_y
          integral = integral + 2 * (f(a_x, y) + f(b_x, y))
      end do

      do i = 1, n_x - 1
          do j = 1, n_y - 1
              x = a_x + i * h_x
              y = a_y + j * h_y
              integral = integral + 4 * f(x, y)
          end do
      end do

      integral = integral * (h_x * h_y) / 4

      print *, 'valor da integral (Trapézio) em 2D: ', integral
  end subroutine trap_2

end module integrais

module equacoes_dif
contains
  subroutine teste
 !!!!1
  end subroutine teste
end module equacoes_dif