      subroutine extracc(E, hf1, cc1, hf2, cc2, LL)
        implicit none
        ! output: E
        ! input : hf1 cc1: energy of hartree-fock and couple cluster
        ! method with small basis set v(LL-1)z
        ! input : hf2 cc2: energy of hartree-fock and couple cluster
        ! method with large basis set v(LL)z
        ! input : LL: the `cardinal number' of large basis set

        real*8 :: hf1,hf2,cc1, cc2
        real*8 :: corr1, corr2
        real*8 :: hfe, corre
        real*8 :: E
        integer :: LL
        real*8 :: L
        L=LL*1.0d0

        corr1=cc1-hf1
        corr2=cc2-hf2


        ! extrapolation of SCF: following 
        ! A. Karton and J.M.L. Martin, 
        ! Theoretical Chemistry Accounts 115, 330 (2005).
        hfe= hf2 +
     &  (hf2-hf1)
     &  /( (L*dexp(9*(dsqrt(L)-dsqrt(L-1))))/(L+1)-1.0d0)
        !write(*,*) hfe

        ! extrapolation of electronic correlation energy: following 
        ! T. Helgaker, W. Klopper, H. Koch, and J. Noga, 
        ! The Journal of Chemical Physics 106, 9639 (1997).
        corre=(corr2*L**3-corr1*(L-1)**3)
     &       /(      L**3-      (L-1)**3)

        !write(*,*) corre
        E=hfe+corre
        return
      end subroutine extracc



