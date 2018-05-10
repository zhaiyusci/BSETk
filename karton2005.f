      real*8 function karton2005_5(hf1,hf2,LL)
        implicit none
        ! extrapolation of SCF: following 
        ! A. Karton and J.M.L. Martin, 
        ! Theoretical Chemistry Accounts 115, 330 (2005).
        ! Equation (5)
        ! input : hf1: the hartree-fock energy of small basis set
        ! input : hf2: the hartree-fock energy of large basis set
        ! input : LL: the `cardinal number' of large basis set
        real*8 :: hf1,hf2
        integer :: LL

        real*8 :: L
        L=LL
        karton2005_5= hf2 +
     &  (hf2-hf1)
     &  /( (L*dexp(9*(dsqrt(L)-dsqrt(L-1))))/(L+1)-1.0d0)
        return 
      end function karton2005_5
