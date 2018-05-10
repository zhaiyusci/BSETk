      program main 
        implicit none 
        real*8 :: hf1=-20.d0, hf2=-21.d0
        real*8 :: cc1=-3.d0, cc2=-2.d0
        real*8, external:: corr2017
        write(*,*) corr2017(hf1,cc1,hf2,cc2,4)
        stop
        end

      real*8 function corr2017(hf1, cc1, hf2, cc2, LL)
        implicit none
        ! input : hf1 cc1: energy of hartree-fock and couple cluster
        ! method with small basis set v(LL-1)z
        ! input : hf2 cc2: energy of hartree-fock and couple cluster
        ! method with large basis set v(LL)z
        ! input : LL: the `cardinal number' of large basis set

        real*8 :: hf1,hf2,cc1, cc2
        real*8 :: corr1, corr2
        integer :: LL
        real*8, external :: karton2005_5, helgaker1997_7

        corr1=cc1-hf1
        corr2=cc2-hf2
        corr2017=karton2005_5(hf1,hf2,LL)+helgaker1997_7(corr1,corr2,LL)


        return
      end function corr2017



