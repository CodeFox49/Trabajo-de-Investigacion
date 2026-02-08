Algoritmo saint_venant_1d
	
	//Programa Principal
	
    Definir i, n, nx, nt Como Entero
    Definir dx, dt, g Como Real
    Definir A, Q, Anew, Qnew Como Real
	
    Escribir "Ingrese dx:"
    Leer dx
    Escribir "Ingrese dt:"
    Leer dt
    Escribir "Ingrese nx:"
    Leer nx
    Escribir "Ingrese nt:"
    Leer nt
	
    g <- 9.81
	
    Dimension A[nx], Q[nx], Anew[nx], Qnew[nx]
	
    condiciones_iniciales(A, Q, nx)
	
    Para n <- 1 Hasta nt Hacer
		
        resolver_saint_venant(A, Q, Anew, Qnew, nx, dx, dt, g)
		
        // COPIA CORRECTA DE ARREGLOS
        Para i <- 1 Hasta nx Hacer
            A[i] <- Anew[i]
            Q[i] <- Qnew[i]
        FinPara
		
    FinPara
	
    Para i <- 1 Hasta nx Hacer
        Escribir (i-1)*dx, A[i], Q[i]
    FinPara
	
FinAlgoritmo

SubProceso condiciones_iniciales(A Por Referencia, Q Por Referencia, nx)
	
    Definir i Como Entero
	
    Para i <- 1 Hasta nx Hacer
        Si i < nx/2 Entonces
            A[i] <- 1.2
        Sino
            A[i] <- 1.0
        FinSi
        Q[i] <- 0.0
    FinPara
	
FinSubProceso

SubProceso resolver_saint_venant(A Por Referencia, Q Por Referencia, Anew Por Referencia, Qnew Por Referencia, nx, dx, dt, g)
	
	//resolver_saint_venant
	
    Definir i Como Entero
	Definir Ap, Qp, F1, F2 Como Real
	Dimension Ap[nx], Qp[nx], F1[nx], F2[nx]
	
    Para i <- 1 Hasta nx Hacer
        F1[i] <- Q[i]
        F2[i] <- (Q[i]*Q[i])/A[i] + 0.5*g*A[i]*A[i]
    FinPara
	
    Para i <- 2 Hasta nx-1 Hacer
        Ap[i] <- A[i] - dt/dx*(F1[i+1] - F1[i])
        Qp[i] <- Q[i] - dt/dx*(F2[i+1] - F2[i])
    FinPara
	
    Ap[1] <- A[1]
    Ap[nx] <- A[nx]
    Qp[1] <- Q[1]
    Qp[nx] <- Q[nx]
	
    Para i <- 1 Hasta nx Hacer
        F1[i] <- Qp[i]
        F2[i] <- (Qp[i]*Qp[i])/Ap[i] + 0.5*g*Ap[i]*Ap[i]
    FinPara
	
    Para i <- 2 Hasta nx-1 Hacer
        Anew[i] <- 0.5*(A[i] + Ap[i] - dt/dx*(F1[i] - F1[i-1]))
        Qnew[i] <- 0.5*(Q[i] + Qp[i] - dt/dx*(F2[i] - F2[i-1]))
    FinPara
	
    Anew[1] <- A[1]
    Anew[nx] <- A[nx]
    Qnew[1] <- Q[1]
    Qnew[nx] <- Q[nx]
	
FinSubProceso



