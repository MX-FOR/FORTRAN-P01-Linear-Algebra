program P01_1P
implicit none
!Programador:: Diego Alejandro Tellez Martinez
!Fecha :: 06/03/20
!Descripcion de Programa:
!Dadas dos matrices realiza operaciones elementales con ambas 

! ================================== Variables =========================================
integer::fm1,cm1,fm2,cm2 !Valores de las filas y columnas para matriz 1 y 2 
integer:: k,p,m,P01,n !Condadores y potencia n
real,allocatable,dimension(:,:) :: M1,M2,MR,Mm,Mp2,Mp1,Mt1,Mt2 !Creacion de matrices, valores e impresion
!M1 (Matriz 1) 
!M2 (Matriz 2) 
!MR (Resultado suma o resta de matriz 1 y matriz 2) 
!Mm (Resultado de la multiplicacion de matrices) 
!Mp2 y Mp1 (Potencia de las matrices) 
!Mt1 y Mt2 (Transpuesta de las matrices) 
! ======================================================================================



write(*,*) " ============================================================================" 
write(*,*) "=                          Algebra Lineal  (01.1)                            ="
write(*,*) " ============================================================================" 
write(*,*) " =                      Calculadora para 2 matrices                          "

!Creacion de matrices  
print*, "Dame las dimensiones de la matriz 1 por filas y columnas "
   read(*,*) fm1
   read(*,*) cm1
   print*, "Dame las dimensiones de la matriz 2 por filas y columnas "
   read(*,*) fm2
   read(*,*) cm2
    
   allocate(M1(fm1,cm1))
   allocate(M2(fm2,cm2))
   allocate(MR(fm2,cm2))
   allocate(Mm(fm1,cm2))
   allocate(Mp1(fm1,fm1))
   allocate(Mp2(fm2,fm2))
   allocate(Mt1(cm1,fm1))
   allocate(Mt2(cm2,fm2))


   print*, "Ingresar valores a las matrices"   

   print*, "Matriz 1"

  do k=1,fm1
  do p=1,cm1
  print*, "Fila",k,"Columna",p
  read(*,*) M1(p,k)
  end do 
  end do 

  print*, "Matriz 2"

  do k=1,fm2
  do p=1,cm2
  print*, "Fila",k,"Columna",p
  read(*,*) M2(p,k)
  end do 
 end do 

 print*, "Matriz 1" 

                  do k=1,fm1 !fila
                  write(*,*)(M1(k,p),P=1,cm1)
          
                  end do
 Print*, "Matriz 2"

                  do k=1,fm2 !fila
                  write(*,*)(M2(k,p),P=1,cm2)
  
end do     


!Creacion de Index 

 write(*,*) "=================   Contenido de operaciones disponibles   ================"

 write(*,*) " 01. Suma de Matrices " 
                         
 write(*,*) " ============================================================================" 


 write(*,*) " 02. Resta de Matrices "  

 write(*,*) " ============================================================================" 


 write(*,*) " 03. Multiplicacion de Matrices "

 write(*,*) " ============================================================================" 


 write(*,*) " 04. Matriz a la n potencia " 
 
 write(*,*) " ============================================================================" 


 write(*,*) " 05. Transpuesta" 

 write(*,*) " ============================================================================" 



 write(*,*) " Escribe el numero del procedimineto  a realizar ( Del 01 al 05 )"
 read(*,*) P01  !Index 

Select Case (P01) 
   Case(01) !        ======================================= 1 =================================
 write(*,*) " 01. Suma de Matrices " 
   if (fm1.eq.fm2.and.cm1.eq.cm2) then 
   do k=1,fm1
   do p =1,cm1 
   Mr(k,p) = Mr(k,p) + M1(k,p) + M2(k,p)
   end do 
   end do 

    Print*, "Matriz Resultante"

                  do k=1,fm1!fila
                  write(*,*)(MR(k,p),p=1,cm1)!columna
                  end do
  else 
  write(*,*) "Las matrices no se pueden sumar"
end if                                                                 

Case(02)!       ======================================= 2 =================================
    write(*,*) " 02. Resta de Matrices "  
    if (fm1.eq.fm2.and.cm1.eq.cm2) then 
   do k=1,fm1
   do p =1,cm1 
   Mr(k,p) = MR(k,p) + M1(k,p) - M2(k,p)
   end do 
   end do 

    Print*, "Matriz Resultante"

                  do k=1,fm1!fila
                  write(*,*)(MR(k,p),p=1,cm1)!columna
                  end do
  else 
  write(*,*) "Las matrices no se pueden restar"
end if 


Case(03) !      ======================================= 3 =================================    
     write(*,*) " 03. Multiplicacion de Matrices "
     if (cm1.eq.fm2) then 
     do k=1,fm1
     do p=1,cm2
     do m=1,cm1

     Mm(k,p) = Mm(k,p) + M1(k,m)*M2(m,p)
     end do 
     end do 
     end do

      Print*, "Matriz Resultante"

                  do k=1,fm1!fila
                  write(*,*)(Mm(k,p),p=1,cm2)!columna
                  end do

                  else 

                  print*, "Las matrices no se pueden multiplicar"

end if 

Case(04)  !    ======================================= 4 =================================  
      write(*,*) " 04. Matriz a la n potencia " 
      if (fm1.eq.cm1.and.fm2.eq.cm2) then 
       print*, "Dame la potencia a elevear la matriz" 
         read(*,*) n

                 Mp1 = M1
                    do k = 1,n-1                 
                     Mp1 = matmul(Mp1,M1)
                      end do

                    !Impresion de matriz
                   PRINT*, "Matriz Resultante "
                  do k=1,fm1 !fila
                  write(*,*)(Mp1(p,k),P=1,fm1)!columna
                  end do     


               Mp2 = M2
                 do k = 1,n-1                 
                     Mp2 = matmul(Mp2,M2)
                 end do

                    !Impresion de matriz
                   PRINT*, "Matriz Resultante "
                  do k=1,fm2 !fila
                  write(*,*)(Mp2(p,k),P=1,fm2 )!columna
                  end do 
                  else 
                  print*, "Las matrices no se pueden potenciar"
end if 
   
Case(05) !    ======================================= 5 =================================   
       write(*,*) " 05. Transpuesta"  

       do k =1,fm1
       do p=1,cm1
       Mt1(k,p) = M1(p,k)
       end do 
       end do 

       do k=1,fm2 
       do p=1,cm2
       Mt2(k,p) = M2(p,k)
       end do 
       end do 

           PRINT*, "Transpuesta Matriz 1 "
                  do k=1,cm1 !fila
                  write(*,*)(Mt1(p,k),P=1,fm1)!columna
                  end do 

           PRINT*, "Transpuesta Matriz 2 "
                  do k=1,cm2 !fila
                  write(*,*)(Mt2(p,k),P=1,fm2)!columna
                  end do               
  Case Default 

  write(*,*) "Numero Incorrecto (Selecciona un numero del 01 al 05 ) "
  
  End Select  !Casos 





end program P01_1P
